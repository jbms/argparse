import argparse
import sys
import textwrap
import os

class ArgumentParserError(Exception):

    def __init__(self, message, stdout=None, stderr=None, error_code=None):
        Exception.__init__(self, message, stdout, stderr)
        self.message = message
        self.stdout = stdout
        self.stderr = stderr
        self.error_code = error_code

class Sig(object):

    def __init__(self, *args, **kwargs):
        self.args = args
        self.kwargs = kwargs

        
NS = dict

class Optional(object):
    def __init__(self, value):
        self.value = value

class Typed(object):
    def __init__(self, type_str, value):
        self.type_str = type_str
        self.value = value

class TypedList(list):
    def __init__(self, type, values = []):
        super(TypedList, self).__init__(values)
        self.type = type

def fix_value(x):
    if x is object or type(x) is object:
        return 'object'
    if x is Exception:
        return 'Exception'
    return x

def type_repr(x):
    if isinstance(x, Typed):
        return x.type_str
    if isinstance(x, TypedList):
        return 'vector<%s>' % type_repr(x.type())
    if x is bool or isinstance(x, bool):
        return 'bool'
    if x is int or isinstance(x, int):
        return 'int'
    if x is str or isinstance(x, str):
        return 'string'
    if x is float or isinstance(x, float):
        return 'double'
    if isinstance(x, Optional):
        return 'optional<%s>' % type_repr(x.value)
    raise RuntimeError('unknown type: %r' % x)

import json

def my_repr(x):
    x = fix_value(x)
    if isinstance(x, Typed):
        return '%s(%s)' % (type_repr(x), my_repr(x.value))
    if isinstance(x, TypedList):
        return type_repr(x) + my_repr(list(x))
    if isinstance(x, Optional):
        return '%s(%s)' % (type_repr(x), my_repr(x.value))
    if isinstance(x, list) or isinstance(x, tuple):
        return '{' + ', '.join(map(my_repr,x)) + '}'
    if isinstance(x, dict):
        return '{' + ', '.join(my_repr((k,v)) for k, v in x.items()) + '}'
    if isinstance(x, str):
        if len(x) == 0:
            return '""s'
        lines = x.splitlines(True)
        s = '\n'.join(json.dumps(line) for line in lines) + 's'
        if len(lines) > 1:
            return '\n' + s + '\n'
        else:
            return s
    return json.dumps(x)

def get_options_spec(sig):
    x = []
    if 'dest' in sig.kwargs:
        x.append(sig.kwargs['dest'])
    x += sig.args
    if len(x) == 1:
        return my_repr(x[0])
    return my_repr(x)

def make_typed(x, modifier):
    if modifier is not None:
        if isinstance(modifier, str):
            return Typed(modifier, make_typed(x, None))
        return modifier(make_typed(x, None))
    if isinstance(x, TypedList):
        return x
    if isinstance(x, list):
        if len(x) == 0:
            return TypedList(str)
        return TypedList(type(x[0]), x)
    return x

def convert_ns(ns, type_modifiers):
    if isinstance(ns, argparse.Namespace):
        ns = ns.__dict__
    return {k: make_typed(v, type_modifiers.get(k, None)) for k, v in ns.items() if v is not None}

def get_nargs(nargs):
    if nargs == argparse.OPTIONAL:
        return 'OPTIONAL'
    if nargs == argparse.ZERO_OR_MORE:
        return 'ZERO_OR_MORE'
    if nargs == argparse.ONE_OR_MORE:
        return 'ONE_OR_MORE'
    if nargs == argparse.PARSER:
        return 'PARSER'
    if nargs == argparse.REMAINDER:
        return 'REMAINDER'
    return 'Nargs(%d)' % nargs

def convert_arg_str(s):
    if isinstance(s, list):
        return s
    return s.split()


def get_argument_code(parser, arg, type_modifiers, result = None):

    if 'choices' in arg.kwargs:
        raise RuntimeError('choices not supported')

    if result is None:
        result = parser.add_argument(*arg.args, **arg.kwargs)
    t = result.type or str

    suffix = ''

    if 'required' in arg.kwargs and arg.kwargs['required']:
        suffix += '.required(true)'

    if 'metavar' in arg.kwargs:
        suffix += '.metavar(%s)' % my_repr(arg.kwargs['metavar'])
        
    if 'help' in arg.kwargs:
        val = arg.kwargs['help']
        if val is argparse.SUPPRESS:
            suffix += '.hide()'
        else:
            suffix += '.help(%s)' % my_repr(val)

    if 'dest' not in arg.kwargs:
        dest = result.dest
        if '_' in dest:
            dest = dest.replace('_', '-')
            result.dest = dest

    default_value_str = ''
    if 'default' in arg.kwargs and arg.kwargs['default'] is not argparse.SUPPRESS:
        default_value = arg.kwargs['default']
        if type(default_value) != t:
            default_value = t(default_value)
            result.default = default_value
        default_value_str = '.default_value(%s)' % my_repr(default_value)


    if isinstance(result, argparse._StoreConstAction):
        code = 'add_flag(' + get_options_spec(arg) + ', ' + my_repr(result.const) + ')'
        if 'default' not in arg.kwargs:
            result.default = None
    elif isinstance(result, argparse._VersionAction):
        code = 'add_version_option(%s, %s)' % (get_options_spec(arg), my_repr(result.version))

    elif isinstance(result, argparse._StoreAction):
        if result.nargs == argparse.OPTIONAL:
            const_value = None
            if 'const' in arg.kwargs:
                const_value = arg.kwargs['const']
                if type(const_value) != t:
                    const_value = t(const_value)
                    result.const = const_value
                code = 'add_optional(%s, %s)%s' % (get_options_spec(arg), my_repr(const_value), default_value_str)
            else:
                if not result.option_strings and 'default' in arg.kwargs and arg.kwargs['default'] is not argparse.SUPPRESS:
                    # use default as missing value instead
                    code = 'add_optional(%s, %s)' % (get_options_spec(arg), my_repr(default_value))
                else:
                    type_modifiers[result.dest] = Optional
                    code = 'add_optional(%s)%s' % (get_options_spec(arg), default_value_str)

        elif 'nargs' in arg.kwargs:
            default_value_str = ''
            if 'default' in arg.kwargs and arg.kwargs['default'] is not argparse.SUPPRESS:
                default_value = arg.kwargs['default']
                if not isinstance(default_value, list):
                    default_value = [default_value]
                    result.default = default_value
                default_value_str = '.default_value(%s)' % my_repr(default_value)

            code = 'add_multi(%s, %s)%s' % (get_options_spec(arg), get_nargs(arg.kwargs['nargs']), default_value_str)
            #result.default = TypedList(str)
            #result.default = None
        else:
            t = result.type or str
            if t is not str:
                template_prefix = 'template add<%s>' % type_repr(t)
            else:
                template_prefix = 'add'

            code = '%s(%s)%s' % (template_prefix, get_options_spec(arg), default_value_str)
    elif isinstance(result, argparse._AppendConstAction):
        result.default = TypedList(type(fix_value(result.const)))
        code = 'add_append_flag(' + get_options_spec(arg) + ', ' + my_repr(result.const) + ')'
    elif isinstance(result, argparse._AppendAction):
        result.default = TypedList(str)
        if 'nargs' in arg.kwargs:
            raise RuntimeError('nargs not supported for add_append')
        code = 'add_append(' + get_options_spec(arg) + ')'
    elif isinstance(result, argparse._CountAction):
        result.default = 0
        type_modifiers[result.dest] = 'size_t'
        code = 'add_count(' + get_options_spec(arg) + ')'
    else:
        raise RuntimeError('unsupported action type: %r' % type(result))
    return code + suffix

class TempDirMixin(object):
    pass

class ParserTestCaseMeta(type):
    def __init__(cls, name, bases, dct):

        if name == 'ParserTestCase':
            return

        if TempDirMixin in bases:
            sys.stderr.write('Skipping %s\n' % name)
            return

        lines = []
        def print(x):
            lines.append(x)

        try:

            print('TEST_F(ParserTestCase, %s) {\n' % name)
            if cls.__doc__:
                for line in cls.__doc__.splitlines():
                    print('  // %s' % line)
            print('')

            parser_sig = Sig()
            if hasattr(cls, 'parser_signature'):
                parser_sig = cls.parser_signature

            parser = argparse.ArgumentParser(*parser_sig.args, **parser_sig.kwargs)

            if 'argument_default' in parser_sig.kwargs:
                raise RuntimeError('argument_default not supported')

            init_lines = []
            if 'prefix_chars' in parser_sig.kwargs:
                init_lines.append('p.prefix_chars(%s);' % my_repr(parser_sig.kwargs['prefix_chars']))
            add_help = True
            if 'add_help' in parser_sig.kwargs:
                add_help = parser_sig.kwargs['add_help']
            if add_help:
                init_lines.append('p.add_help_option();')

            if init_lines:
                print('  init([&](auto &p) {')
                for line in init_lines:
                    print('    %s' % line)
                print('  });')

            type_modifiers = dict()

            for arg in cls.argument_signatures:
                code = get_argument_code(parser, arg, type_modifiers)
                print('  args([&](auto &p) { p.%s; });' % (code,))
                
            #print('%s: args = %r' % (name, cls.argument_signatures))

            print('')

            print('  failures(' + my_repr([convert_arg_str(x) for x in cls.failures]) + ');')

            for arg_str, ns_result_orig in cls.successes:
                ns_result = parser.parse_args(convert_arg_str(arg_str)).__dict__

                print('  success(' + my_repr(convert_arg_str(arg_str)) + ', ' + my_repr(convert_ns(ns_result, type_modifiers)) + ');')

            print('}')

            sys.stdout.write('\n'.join(lines) + '\n')
        except RuntimeError as e:
            sys.stderr.write('Skipping %s: %r\n' % (name, e))

class ParserTestCase(object, metaclass = ParserTestCaseMeta):
    pass

class HelpTestCaseMeta(type):
    def __init__(cls, name, bases, dct):
        if name == 'HelpTestCase':
            return

        lines = []
        
        def print(x):
            lines.append(x)

        width = 78
        if name == 'TestShortColumns':
            width = 13

        try:

            print('TEST_F(ParserTestCase, %s) {\n' % name)
            if cls.__doc__:
                for line in cls.__doc__.splitlines():
                    print('  // %s' % line)
            print('')

            print('  ArgumentParser p;')

            parser_sig = Sig()
            if hasattr(cls, 'parser_signature'):
                parser_sig = cls.parser_signature

            if 'formatter_class' in parser_sig.kwargs:
                raise RuntimeError('help formatter not supported: %r' % parser_sig.kwargs['formatter_class'])

            parser = argparse.ArgumentParser(*parser_sig.args, **parser_sig.kwargs)

            if 'prefix_chars' in parser_sig.kwargs:
                print('  p.prefix_chars(%s);' % my_repr(parser_sig.kwargs['prefix_chars']))
            add_help = True
            if 'add_help' in parser_sig.kwargs:
                add_help = parser_sig.kwargs['add_help']
            if add_help:
                print('  p.add_help_option();')

            for x in ['prog', 'epilog', 'description', 'usage']:
                if x in parser_sig.kwargs:
                    val = parser_sig.kwargs[x]
                    if val is argparse.SUPPRESS:
                        val = ''
                    print('  p.%s(%s);' % (x, my_repr(val)))

            for arg in cls.argument_signatures:
                code = get_argument_code(parser, arg, {})
                print('  p.%s;' % code)

            for group, arg_sigs in getattr(cls, 'argument_group_signatures', []):
                py_group = parser.add_argument_group(*group.args, **group.kwargs)
                group_suffix = ''
                if py_group.description:
                    group_suffix += ', %s' % my_repr(py_group.description)
                print('  {')
                print('    auto group = p.add_group(%s%s);' % (my_repr(py_group.title), group_suffix))
                for arg in arg_sigs:
                    code = get_argument_code(py_group, arg, {})
                    print('    group.%s;' % code)
                print('  }')

            subparsers_sigs = getattr(cls, 'subparsers_signatures', [])
            if subparsers_sigs:
                py_subparsers = parser.add_subparsers()
                print('  auto subparsers = p.add_subparsers();')
                for subparser_sig in subparsers_sigs:
                    print('  {')
                    def my_add_parser(name, **kwargs):
                        suffix = ''
                        if 'help' in kwargs:
                            suffix += ', %s' % my_repr(kwargs['help'])
                        names = [name]
                        if 'aliases' in kwargs:
                            names += kwargs['aliases']
                        print('    auto subparser = subparsers.add_parser(%s%s);' % (my_repr(names), suffix))
                        
                    my_add_parser(*subparser_sig.args, **subparser_sig.kwargs)
                    py_subparser = py_subparsers.add_parser(*subparser_sig.args, **subparser_sig.kwargs)
                    print('  }')

            for help_type in ['usage', 'help']:
                import textwrap
                print('  std::ostringstream %s;' % help_type)
                print('  ASSERT_NO_THROW(p.print_%s(%s, %d));' % (help_type,help_type,width))
                print('  std::string %s_expected = %s;' % (help_type, my_repr(textwrap.dedent(getattr(cls,help_type)))))
                print('  ASSERT_HELP_EQ(%s_expected, %s.str());' % (help_type, help_type))

            print('}')

            sys.stdout.write('\n'.join(lines) + '\n')
        except RuntimeError as e:
            sys.stderr.write('Skipping %s: %r\n' % (name, e))
        
class HelpTestCase(object, metaclass = HelpTestCaseMeta):
    pass

# Captures a trace of actions performed by a test
class TraceState(object):
    def __init__(self):
        self.lines = []
        self.next_name = {}
    def get_name(self, key):
        self.next_name.setdefault(key, 0)
        val = self.next_name[key]
        self.next_name[key] += 1
        if val == 0:
            return key
        return key + '%d' % val
    def print(self, line):
        self.lines.append(line)



trace_state = None

class TraceValue(object):
    def __init__(self, name, py_obj, compare_str):
        self.name = name
        self.py_obj = py_obj
        self.compare_str = compare_str

def fix_parser_init_args(**kwargs):
    parents = kwargs.get('parents', None)
    new_kwargs = kwargs.copy()
    if parents is not None:
        new_kwargs['parents'] = [p.py_obj for p in parents]
    return new_kwargs
        

class Action(object):
    def __init__(self, parent, *args, **kwargs):
        self.parent = parent
        self.py_obj = parent.py_obj.add_argument(*args, **kwargs)
        code = get_argument_code(parent.py_obj, Sig(*args, **kwargs), parent.type_modifiers, self.py_obj)
        trace_state.print('%s.%s;' % (parent.name, code))

class Subparsers(object):
    def __init__(self, parent, **kwargs):
        self.parent = parent
        self.py_obj = parent.py_obj.add_subparsers(**kwargs)
        self.name = trace_state.get_name('subs')
        self.type_modifiers = parent.type_modifiers

        suffix = ''
        if 'metavar' in kwargs:
            suffix += '.metavar(%s)' % my_repr(kwargs['metavar'])
        if 'help' in kwargs:
            if kwargs['help'] is argparse.SUPPRESS:
                suffix += '.hide()'
            else:
                suffix += '.help(%s)' % my_repr(kwargs['help'])
        if 'title' in kwargs or 'description' in kwargs:
            title = kwargs.get('title', 'subcommands')
            description = kwargs.get('description', '')
            parent = parent.add_argument_group(title, description)
            self.parent = parent
        arg_code = ''
        if 'dest' in kwargs and 'dest' is not argparse.SUPPRESS:
            arg_code = my_repr(kwargs['dest'])
        trace_state.print('auto %s = %s.add_subparsers(%s)%s;' % (self.name, self.parent.name, arg_code, suffix))

    def add_parser(self, *args, **kwargs):
        return Subparser(self, *args, **kwargs)


class Group(object):
    def __init__(self, parent, title, description = None):
        self.parent = parent
        self.type_modifiers = parent.type_modifiers
        self.py_obj = parent.py_obj.add_argument_group(title, description = description)
        self.name = trace_state.get_name('g')
        suffix = ''
        if description is not None:
            suffix += ', %s' % my_repr(description)
        trace_state.print('auto %s = %s.add_group(%s%s);' % (self.name, parent.name, my_repr(title), suffix))


    def add_mutually_exclusive_group(self, *args, **kwargs):
        return MutexGroup(self, *args, **kwargs)

    def add_argument(self, *args, **kwargs):
        return Action(self, *args, **kwargs)

def print_parser_options(name, **kwargs):
    add_help = True
    if 'add_help' in kwargs:
        add_help = kwargs['add_help']
    if add_help:
        trace_state.print('%s.add_help_option();' % name)
    for x in ['prog', 'epilog', 'description', 'usage', 'prefix_chars']:
        if x in kwargs:
            val = kwargs[x]
            if val is argparse.SUPPRESS:
                val = ''
            trace_state.print('%s.%s(%s);' % (name, x, my_repr(val)))
    if 'parents' in kwargs:
        for parent in kwargs['parents']:
            trace_state.print('%s.parent(%s);' % (name, parent.name))

from io import StringIO
class StdIOBuffer(StringIO):
    pass

def stderr_to_parser_error(parse_args, *args, **kwargs):
    # if this is being called recursively and stderr or stdout is already being
    # redirected, simply call the function and let the enclosing function
    # catch the exception
    if isinstance(sys.stderr, StdIOBuffer) or isinstance(sys.stdout, StdIOBuffer):
        return parse_args(*args, **kwargs)

    # if this is not being called recursively, redirect stderr and
    # use it as the ArgumentParserError message
    old_stdout = sys.stdout
    old_stderr = sys.stderr
    sys.stdout = StdIOBuffer()
    sys.stderr = StdIOBuffer()
    try:
        try:
            result = parse_args(*args, **kwargs)
            for key in list(vars(result)):
                if getattr(result, key) is sys.stdout:
                    setattr(result, key, old_stdout)
                if getattr(result, key) is sys.stderr:
                    setattr(result, key, old_stderr)
            return result
        except SystemExit:
            code = sys.exc_info()[1].code
            stdout = sys.stdout.getvalue()
            stderr = sys.stderr.getvalue()
            raise RuntimeError("SystemExit", stdout, stderr, code)
    finally:
        sys.stdout = old_stdout
        sys.stderr = old_stderr


class ErrorRaisingArgumentParser_Impl(argparse.ArgumentParser):

    def parse_args(self, *args, **kwargs):
        parse_args = super(ErrorRaisingArgumentParser_Impl, self).parse_args
        return stderr_to_parser_error(parse_args, *args, **kwargs)

    def exit(self, *args, **kwargs):
        exit = super(ErrorRaisingArgumentParser_Impl, self).exit
        return stderr_to_parser_error(exit, *args, **kwargs)

    def error(self, *args, **kwargs):
        error = super(ErrorRaisingArgumentParser_Impl, self).error
        return stderr_to_parser_error(error, *args, **kwargs)



class ErrorRaisingArgumentParser(object):
    def __init__(self, *args, **kwargs):
        if 'argument_default' in kwargs:
            raise RuntimeError('argument_default not supported')
        self.type_modifiers = {}
        kwargs.setdefault('prog', 'PROG')
        self.py_obj = ErrorRaisingArgumentParser_Impl(*args, **fix_parser_init_args(**kwargs))
        self.name = trace_state.get_name('p')
        trace_state.print('ArgumentParser %s;' % self.name)
        print_parser_options(self.name, **kwargs)
    def add_argument(self, *args, **kwargs):
        return Action(self, *args, **kwargs)

    def add_argument_group(self, *args, **kwargs):
        return Group(self, *args, **kwargs)

    def add_mutually_exclusive_group(self, *args, **kwargs):
        return MutexGroup(self, *args, **kwargs)

    def add_subparsers(self, *args, **kwargs):
        return Subparsers(self, *args, **kwargs)

    def parse_args(self, args):
        args_var = trace_state.get_name('args')
        trace_state.print('vector<string> %s = %s;' % (args_var, my_repr(args)))
        result_var = trace_state.get_name('r')
        trace_state.print('Result %s = %s.try_parse(get_string_view_vec(%s));' % (result_var, self.name, args_var))
        py_obj = self.py_obj.parse_args(args)
        compare_str = convert_ns(py_obj, self.type_modifiers)
        return TraceValue(result_var, py_obj, 'ASSERT_TRUE(results_equal(%s, %s)) << ::testing::PrintToString(%s);' % (my_repr(compare_str), result_var, args_var))

    def format_help(self):
        result_var = '%s.help_string(78)' % self.name
        py_obj = self.py_obj.format_help()
        return TraceValue(result_var, py_obj, 'ASSERT_HELP_EQ(%s, %s);' % (my_repr(py_obj), result_var))
    def format_usage(self):
        result_var = '%s.usage_string(78)' % self.name
        py_obj = self.py_obj.format_usage()
        return TraceValue(result_var, py_obj, 'ASSERT_HELP_EQ(%s, %s);' % (my_repr(py_obj), result_var))

class Subparser(ErrorRaisingArgumentParser):
    def __init__(self, parent, name, **kwargs):
        self.parent = parent
        self.type_modifiers = parent.type_modifiers
        self.py_obj = parent.py_obj.add_parser(name, **fix_parser_init_args(**kwargs))
        self.name = trace_state.get_name('sub')
        names = [name]
        suffix = ''
        if 'help' in kwargs:
            suffix += ', %s' % my_repr(kwargs['help'])
        
        if 'aliases' in kwargs:
            names += kwargs['aliases']

        trace_state.print('auto %s = %s.add_parser(%s%s);' % (self.name, self.parent.name, my_repr(names), suffix))
        print_parser_options(self.name, **kwargs)

class MutexGroup(object):
    def __init__(self, parent, required = False):
        self.parent = parent
        self.py_obj = parent.py_obj.add_mutually_exclusive_group(required = required)
        self.name = trace_state.get_name('m')
        self.type_modifiers = parent.type_modifiers
        trace_state.print('auto %s = %s.add_mutually_exclusive_group(%s);' % (self.name, self.parent.name, my_repr(required)))
        
    def add_argument(self, *args, **kwargs):
        return Action(self, *args, **kwargs)

pending_tests = []

class TestCaseMeta(type):
    def __init__(cls, name, bases, dct):
        if name == 'TestCase':
            return

        def run():
            lines = []
            def print(line):
                lines.append(line)

            try:
                print('')
                if cls.__doc__:
                    for line in cls.__doc__.splitlines():
                        print('// %s' % line)
                print('')
                
                global trace_state

                all_members = list(cls.__dict__.keys())
                for base in bases:
                    all_members += list(base.__dict__.keys())

                for method_name in all_members:
                    if method_name.startswith('test_'):
                        obj = cls()
                        trace_state = TraceState()
                        try:
                            obj.setUp()
                            obj.main_program = 'PROG'
                            getattr(obj, method_name)()
                            #print('  // %s' % method_name)

                            #print('  {')
                            print('TEST(%s, %s) {' % (name, method_name))
                            for x in trace_state.lines:
                                print('  ' + x)
                            #print('  }')
                            print('}')                            
                        except RuntimeError as e:
                            import traceback
                            traceback.print_exc()
                            sys.stderr.write('Skipping %s/%s: %r\n' % (name, method_name, e))



                sys.stdout.write('\n'.join(lines) + '\n')
            except RuntimeError as e:
                sys.stderr.write('Skipping %s: %r\n' % (name, e))
        pending_tests.append(run)

class TestCase(object, metaclass = TestCaseMeta):
    def setUp(self):
        pass

    def assertEqual(self, a, b):
        if isinstance(a, TraceValue):
            trace_state.print(a.compare_str)
        elif isinstance(b, TraceValue):
            trace_state.print(b.compare_str)
        else:
            raise RuntimeError('unsupported assertion')
    def assertRaises(self, exc, func, *args, **kwargs):
        if exc is ArgumentParserError and func.__name__ == 'parse_args':
            args_var = trace_state.get_name('args')
            trace_state.print('vector<string> %s = %s;' % (args_var, my_repr(args[0])))
            trace_state.print('ASSERT_THROW(%s.try_parse(get_string_view_vec(%s)), ParseError);' %
                              (func.__self__.name, args_var))
        else:
            raise RuntimeError('unsupported assertRaises: %r %r' % (exc, func))

# ===============
# Optionals tests
# ===============

class TestOptionalsSingleDash(ParserTestCase):
    """Test an Optional with a single-dash option string"""

    argument_signatures = [Sig('-x')]
    failures = ['-x', 'a', '--foo', '-x --foo', '-x -y']
    successes = [
        ('', NS(x=None)),
        ('-x a', NS(x='a')),
        ('-xa', NS(x='a')),
        ('-x -1', NS(x='-1')),
        ('-x-1', NS(x='-1')),
    ]


class TestOptionalsSingleDashCombined(ParserTestCase):
    """Test an Optional with a single-dash option string"""

    argument_signatures = [
        Sig('-x', action='store_true'),
        Sig('-yyy', action='store_const', const=42),
        Sig('-z'),
    ]
    failures = ['a', '--foo', '-xa', '-x --foo', '-x -z', '-z -x',
                '-yx', '-yz a', '-yyyx', '-yyyza', '-xyza']
    successes = [
        ('', NS(x=False, yyy=None, z=None)),
        ('-x', NS(x=True, yyy=None, z=None)),
        ('-za', NS(x=False, yyy=None, z='a')),
        ('-z a', NS(x=False, yyy=None, z='a')),
        ('-xza', NS(x=True, yyy=None, z='a')),
        ('-xz a', NS(x=True, yyy=None, z='a')),
        ('-x -za', NS(x=True, yyy=None, z='a')),
        ('-x -z a', NS(x=True, yyy=None, z='a')),
        ('-y', NS(x=False, yyy=42, z=None)),
        ('-yyy', NS(x=False, yyy=42, z=None)),
        ('-x -yyy -za', NS(x=True, yyy=42, z='a')),
        ('-x -yyy -z a', NS(x=True, yyy=42, z='a')),
    ]


class TestOptionalsSingleDashLong(ParserTestCase):
    """Test an Optional with a multi-character single-dash option string"""

    argument_signatures = [Sig('-foo')]
    failures = ['-foo', 'a', '--foo', '-foo --foo', '-foo -y', '-fooa']
    successes = [
        ('', NS(foo=None)),
        ('-foo a', NS(foo='a')),
        ('-foo -1', NS(foo='-1')),
        ('-fo a', NS(foo='a')),
        ('-f a', NS(foo='a')),
    ]


class TestOptionalsSingleDashSubsetAmbiguous(ParserTestCase):
    """Test Optionals where option strings are subsets of each other"""

    argument_signatures = [Sig('-f'), Sig('-foobar'), Sig('-foorab')]
    failures = ['-f', '-foo', '-fo', '-foo b', '-foob', '-fooba', '-foora']
    successes = [
        ('', NS(f=None, foobar=None, foorab=None)),
        ('-f a', NS(f='a', foobar=None, foorab=None)),
        ('-fa', NS(f='a', foobar=None, foorab=None)),
        ('-foa', NS(f='oa', foobar=None, foorab=None)),
        ('-fooa', NS(f='ooa', foobar=None, foorab=None)),
        ('-foobar a', NS(f=None, foobar='a', foorab=None)),
        ('-foorab a', NS(f=None, foobar=None, foorab='a')),
    ]


class TestOptionalsSingleDashAmbiguous(ParserTestCase):
    """Test Optionals that partially match but are not subsets"""

    argument_signatures = [Sig('-foobar'), Sig('-foorab')]
    failures = ['-f', '-f a', '-fa', '-foa', '-foo', '-fo', '-foo b']
    successes = [
        ('', NS(foobar=None, foorab=None)),
        ('-foob a', NS(foobar='a', foorab=None)),
        ('-foor a', NS(foobar=None, foorab='a')),
        ('-fooba a', NS(foobar='a', foorab=None)),
        ('-foora a', NS(foobar=None, foorab='a')),
        ('-foobar a', NS(foobar='a', foorab=None)),
        ('-foorab a', NS(foobar=None, foorab='a')),
    ]


class TestOptionalsNumeric(ParserTestCase):
    """Test an Optional with a short opt string"""

    argument_signatures = [Sig('-1', dest='one')]
    failures = ['-1', 'a', '-1 --foo', '-1 -y', '-1 -1', '-1 -2']
    successes = [
        ('', NS(one=None)),
        ('-1 a', NS(one='a')),
        ('-1a', NS(one='a')),
        ('-1-2', NS(one='-2')),
    ]


class TestOptionalsDoubleDash(ParserTestCase):
    """Test an Optional with a double-dash option string"""

    argument_signatures = [Sig('--foo')]
    failures = ['--foo', '-f', '-f a', 'a', '--foo -x', '--foo --bar']
    successes = [
        ('', NS(foo=None)),
        ('--foo a', NS(foo='a')),
        ('--foo=a', NS(foo='a')),
        ('--foo -2.5', NS(foo='-2.5')),
        ('--foo=-2.5', NS(foo='-2.5')),
    ]


class TestOptionalsDoubleDashPartialMatch(ParserTestCase):
    """Tests partial matching with a double-dash option string"""

    argument_signatures = [
        Sig('--badger', action='store_true'),
        Sig('--bat'),
    ]
    failures = ['--bar', '--b', '--ba', '--b=2', '--ba=4', '--badge 5']
    successes = [
        ('', NS(badger=False, bat=None)),
        ('--bat X', NS(badger=False, bat='X')),
        ('--bad', NS(badger=True, bat=None)),
        ('--badg', NS(badger=True, bat=None)),
        ('--badge', NS(badger=True, bat=None)),
        ('--badger', NS(badger=True, bat=None)),
    ]


class TestOptionalsDoubleDashPrefixMatch(ParserTestCase):
    """Tests when one double-dash option string is a prefix of another"""

    argument_signatures = [
        Sig('--badger', action='store_true'),
        Sig('--ba'),
    ]
    failures = ['--bar', '--b', '--ba', '--b=2', '--badge 5']
    successes = [
        ('', NS(badger=False, ba=None)),
        ('--ba X', NS(badger=False, ba='X')),
        ('--ba=X', NS(badger=False, ba='X')),
        ('--bad', NS(badger=True, ba=None)),
        ('--badg', NS(badger=True, ba=None)),
        ('--badge', NS(badger=True, ba=None)),
        ('--badger', NS(badger=True, ba=None)),
    ]


class TestOptionalsSingleDoubleDash(ParserTestCase):
    """Test an Optional with single- and double-dash option strings"""

    argument_signatures = [
        Sig('-f', action='store_true'),
        Sig('--bar'),
        Sig('-baz', action='store_const', const=42),
    ]
    failures = ['--bar', '-fbar', '-fbaz', '-bazf', '-b B', 'B']
    successes = [
        ('', NS(f=False, bar=None, baz=None)),
        ('-f', NS(f=True, bar=None, baz=None)),
        ('--ba B', NS(f=False, bar='B', baz=None)),
        ('-f --bar B', NS(f=True, bar='B', baz=None)),
        ('-f -b', NS(f=True, bar=None, baz=42)),
        ('-ba -f', NS(f=True, bar=None, baz=42)),
    ]


class TestOptionalsAlternatePrefixChars(ParserTestCase):
    """Test an Optional with option strings with custom prefixes"""

    parser_signature = Sig(prefix_chars='+:/', add_help=False)
    argument_signatures = [
        Sig('+f', action='store_true'),
        Sig('::bar'),
        Sig('/baz', action='store_const', const=42),
    ]
    failures = ['--bar', '-fbar', '-b B', 'B', '-f', '--bar B', '-baz', '-h', '--help', '+h', '::help', '/help']
    successes = [
        ('', NS(f=False, bar=None, baz=None)),
        ('+f', NS(f=True, bar=None, baz=None)),
        ('::ba B', NS(f=False, bar='B', baz=None)),
        ('+f ::bar B', NS(f=True, bar='B', baz=None)),
        ('+f /b', NS(f=True, bar=None, baz=42)),
        ('/ba +f', NS(f=True, bar=None, baz=42)),
    ]


class TestOptionalsAlternatePrefixCharsAddedHelp(ParserTestCase):
    """When ``-`` not in prefix_chars, default operators created for help
       should use the prefix_chars in use rather than - or --
       http://bugs.python.org/issue9444"""

    parser_signature = Sig(prefix_chars='+:/', add_help=True)
    argument_signatures = [
        Sig('+f', action='store_true'),
        Sig('::bar'),
        Sig('/baz', action='store_const', const=42),
    ]
    failures = ['--bar', '-fbar', '-b B', 'B', '-f', '--bar B', '-baz']
    successes = [
        ('', NS(f=False, bar=None, baz=None)),
        ('+f', NS(f=True, bar=None, baz=None)),
        ('::ba B', NS(f=False, bar='B', baz=None)),
        ('+f ::bar B', NS(f=True, bar='B', baz=None)),
        ('+f /b', NS(f=True, bar=None, baz=42)),
        ('/ba +f', NS(f=True, bar=None, baz=42))
    ]


class TestOptionalsAlternatePrefixCharsMultipleShortArgs(ParserTestCase):
    """Verify that Optionals must be called with their defined prefixes"""

    parser_signature = Sig(prefix_chars='+-', add_help=False)
    argument_signatures = [
        Sig('-x', action='store_true'),
        Sig('+y', action='store_true'),
        Sig('+z', action='store_true'),
    ]
    failures = ['-w',
                '-xyz',
                '+x',
                '-y',
                '+xyz',
    ]
    successes = [
        ('', NS(x=False, y=False, z=False)),
        ('-x', NS(x=True, y=False, z=False)),
        ('+y -x', NS(x=True, y=True, z=False)),
        ('+yz -x', NS(x=True, y=True, z=True)),
    ]


class TestOptionalsShortLong(ParserTestCase):
    """Test a combination of single- and double-dash option strings"""

    argument_signatures = [
        Sig('-v', '--verbose', '-n', '--noisy', action='store_true'),
    ]
    failures = ['--x --verbose', '-N', 'a', '-v x']
    successes = [
        ('', NS(verbose=False)),
        ('-v', NS(verbose=True)),
        ('--verbose', NS(verbose=True)),
        ('-n', NS(verbose=True)),
        ('--noisy', NS(verbose=True)),
    ]


class TestOptionalsDest(ParserTestCase):
    """Tests various means of setting destination"""

    argument_signatures = [Sig('--foo-bar'), Sig('--baz', dest='zabbaz')]
    failures = ['a']
    successes = [
        ('--foo-bar f', NS(foo_bar='f', zabbaz=None)),
        ('--baz g', NS(foo_bar=None, zabbaz='g')),
        ('--foo-bar h --baz i', NS(foo_bar='h', zabbaz='i')),
        ('--baz j --foo-bar k', NS(foo_bar='k', zabbaz='j')),
    ]


class TestOptionalsDefault(ParserTestCase):
    """Tests specifying a default for an Optional"""

    argument_signatures = [Sig('-x'), Sig('-y', default=42)]
    failures = ['a']
    successes = [
        ('', NS(x=None, y=42)),
        ('-xx', NS(x='x', y=42)),
        ('-yy', NS(x=None, y='y')),
    ]


class TestOptionalsNargsDefault(ParserTestCase):
    """Tests not specifying the number of args for an Optional"""

    argument_signatures = [Sig('-x')]
    failures = ['a', '-x']
    successes = [
        ('', NS(x=None)),
        ('-x a', NS(x='a')),
    ]


class TestOptionalsNargs1(ParserTestCase):
    """Tests specifying the 1 arg for an Optional"""

    argument_signatures = [Sig('-x', nargs=1)]
    failures = ['a', '-x']
    successes = [
        ('', NS(x=None)),
        ('-x a', NS(x=['a'])),
    ]


class TestOptionalsNargs3(ParserTestCase):
    """Tests specifying the 3 args for an Optional"""

    argument_signatures = [Sig('-x', nargs=3)]
    failures = ['a', '-x', '-x a', '-x a b', 'a -x', 'a -x b']
    successes = [
        ('', NS(x=None)),
        ('-x a b c', NS(x=['a', 'b', 'c'])),
    ]


class TestOptionalsNargsOptional(ParserTestCase):
    """Tests specifying an Optional arg for an Optional"""

    argument_signatures = [
        Sig('-w', nargs='?'),
        Sig('-x', nargs='?', const=42),
        Sig('-y', nargs='?', default='spam'),
        Sig('-z', nargs='?', type=int, const='42', default='84'),
    ]
    failures = ['2']
    successes = [
        ('', NS(w=None, x=None, y='spam', z=84)),
        ('-w', NS(w=None, x=None, y='spam', z=84)),
        ('-w 2', NS(w='2', x=None, y='spam', z=84)),
        ('-x', NS(w=None, x=42, y='spam', z=84)),
        ('-x 2', NS(w=None, x='2', y='spam', z=84)),
        ('-y', NS(w=None, x=None, y=None, z=84)),
        ('-y 2', NS(w=None, x=None, y='2', z=84)),
        ('-z', NS(w=None, x=None, y='spam', z=42)),
        ('-z 2', NS(w=None, x=None, y='spam', z=2)),
    ]


class TestOptionalsNargsZeroOrMore(ParserTestCase):
    """Tests specifying an args for an Optional that accepts zero or more"""

    argument_signatures = [
        Sig('-x', nargs='*'),
        Sig('-y', nargs='*', default='spam'),
    ]
    failures = ['a']
    successes = [
        ('', NS(x=None, y='spam')),
        ('-x', NS(x=[], y='spam')),
        ('-x a', NS(x=['a'], y='spam')),
        ('-x a b', NS(x=['a', 'b'], y='spam')),
        ('-y', NS(x=None, y=[])),
        ('-y a', NS(x=None, y=['a'])),
        ('-y a b', NS(x=None, y=['a', 'b'])),
    ]


class TestOptionalsNargsOneOrMore(ParserTestCase):
    """Tests specifying an args for an Optional that accepts one or more"""

    argument_signatures = [
        Sig('-x', nargs='+'),
        Sig('-y', nargs='+', default='spam'),
    ]
    failures = ['a', '-x', '-y', 'a -x', 'a -y b']
    successes = [
        ('', NS(x=None, y='spam')),
        ('-x a', NS(x=['a'], y='spam')),
        ('-x a b', NS(x=['a', 'b'], y='spam')),
        ('-y a', NS(x=None, y=['a'])),
        ('-y a b', NS(x=None, y=['a', 'b'])),
    ]


class TestOptionalsChoices(ParserTestCase):
    """Tests specifying the choices for an Optional"""

    argument_signatures = [
        Sig('-f', choices='abc'),
        Sig('-g', type=int, choices=range(5))]
    failures = ['a', '-f d', '-fad', '-ga', '-g 6']
    successes = [
        ('', NS(f=None, g=None)),
        ('-f a', NS(f='a', g=None)),
        ('-f c', NS(f='c', g=None)),
        ('-g 0', NS(f=None, g=0)),
        ('-g 03', NS(f=None, g=3)),
        ('-fb -g4', NS(f='b', g=4)),
    ]


class TestOptionalsRequired(ParserTestCase):
    """Tests the an optional action that is required"""

    argument_signatures = [
        Sig('-x', type=int, required=True),
    ]
    failures = ['a', '']
    successes = [
        ('-x 1', NS(x=1)),
        ('-x42', NS(x=42)),
    ]


class TestOptionalsActionStore(ParserTestCase):
    """Tests the store action for an Optional"""

    argument_signatures = [Sig('-x', action='store')]
    failures = ['a', 'a -x']
    successes = [
        ('', NS(x=None)),
        ('-xfoo', NS(x='foo')),
    ]


class TestOptionalsActionStoreConst(ParserTestCase):
    """Tests the store_const action for an Optional"""

    argument_signatures = [Sig('-y', action='store_const', const=object)]
    failures = ['a']
    successes = [
        ('', NS(y=None)),
        ('-y', NS(y=object)),
    ]


class TestOptionalsActionStoreFalse(ParserTestCase):
    """Tests the store_false action for an Optional"""

    argument_signatures = [Sig('-z', action='store_false')]
    failures = ['a', '-za', '-z a']
    successes = [
        ('', NS(z=True)),
        ('-z', NS(z=False)),
    ]


class TestOptionalsActionStoreTrue(ParserTestCase):
    """Tests the store_true action for an Optional"""

    argument_signatures = [Sig('--apple', action='store_true')]
    failures = ['a', '--apple=b', '--apple b']
    successes = [
        ('', NS(apple=False)),
        ('--apple', NS(apple=True)),
    ]


class TestOptionalsActionAppend(ParserTestCase):
    """Tests the append action for an Optional"""

    argument_signatures = [Sig('--baz', action='append')]
    failures = ['a', '--baz', 'a --baz', '--baz a b']
    successes = [
        ('', NS(baz=None)),
        ('--baz a', NS(baz=['a'])),
        ('--baz a --baz b', NS(baz=['a', 'b'])),
    ]


class TestOptionalsActionAppendWithDefault(ParserTestCase):
    """Tests the append action for an Optional"""

    argument_signatures = [Sig('--baz', action='append', default=['X'])]
    failures = ['a', '--baz', 'a --baz', '--baz a b']
    successes = [
        ('', NS(baz=['X'])),
        ('--baz a', NS(baz=['X', 'a'])),
        ('--baz a --baz b', NS(baz=['X', 'a', 'b'])),
    ]


class TestOptionalsActionAppendConst(ParserTestCase):
    """Tests the append_const action for an Optional"""

    argument_signatures = [
        Sig('-b', action='append_const', const=Exception),
        Sig('-c', action='append', dest='b'),
    ]
    failures = ['a', '-c', 'a -c', '-bx', '-b x']
    successes = [
        ('', NS(b=None)),
        ('-b', NS(b=[Exception])),
        ('-b -cx -b -cyz', NS(b=[Exception, 'x', Exception, 'yz'])),
    ]


class TestOptionalsActionAppendConstWithDefault(ParserTestCase):
    """Tests the append_const action for an Optional"""

    argument_signatures = [
        Sig('-b', action='append_const', const=Exception, default=['X']),
        Sig('-c', action='append', dest='b'),
    ]
    failures = ['a', '-c', 'a -c', '-bx', '-b x']
    successes = [
        ('', NS(b=['X'])),
        ('-b', NS(b=['X', Exception])),
        ('-b -cx -b -cyz', NS(b=['X', Exception, 'x', Exception, 'yz'])),
    ]


class TestOptionalsActionCount(ParserTestCase):
    """Tests the count action for an Optional"""

    argument_signatures = [Sig('-x', action='count')]
    failures = ['a', '-x a', '-x b', '-x a -x b']
    successes = [
        ('', NS(x=None)),
        ('-x', NS(x=1)),
    ]


# ================
# Positional tests
# ================

class TestPositionalsNargsNone(ParserTestCase):
    """Test a Positional that doesn't specify nargs"""

    argument_signatures = [Sig('foo')]
    failures = ['', '-x', 'a b']
    successes = [
        ('a', NS(foo='a')),
    ]


class TestPositionalsNargs1(ParserTestCase):
    """Test a Positional that specifies an nargs of 1"""

    argument_signatures = [Sig('foo', nargs=1)]
    failures = ['', '-x', 'a b']
    successes = [
        ('a', NS(foo=['a'])),
    ]


class TestPositionalsNargs2(ParserTestCase):
    """Test a Positional that specifies an nargs of 2"""

    argument_signatures = [Sig('foo', nargs=2)]
    failures = ['', 'a', '-x', 'a b c']
    successes = [
        ('a b', NS(foo=['a', 'b'])),
    ]


class TestPositionalsNargsZeroOrMore(ParserTestCase):
    """Test a Positional that specifies unlimited nargs"""

    argument_signatures = [Sig('foo', nargs='*')]
    failures = ['-x']
    successes = [
        ('', NS(foo=[])),
        ('a', NS(foo=['a'])),
        ('a b', NS(foo=['a', 'b'])),
    ]


class TestPositionalsNargsZeroOrMoreDefault(ParserTestCase):
    """Test a Positional that specifies unlimited nargs and a default"""

    argument_signatures = [Sig('foo', nargs='*', default='bar')]
    failures = ['-x']
    successes = [
        ('', NS(foo='bar')),
        ('a', NS(foo=['a'])),
        ('a b', NS(foo=['a', 'b'])),
    ]


class TestPositionalsNargsOneOrMore(ParserTestCase):
    """Test a Positional that specifies one or more nargs"""

    argument_signatures = [Sig('foo', nargs='+')]
    failures = ['', '-x']
    successes = [
        ('a', NS(foo=['a'])),
        ('a b', NS(foo=['a', 'b'])),
    ]


class TestPositionalsNargsOptional(ParserTestCase):
    """Tests an Optional Positional"""

    argument_signatures = [Sig('foo', nargs='?')]
    failures = ['-x', 'a b']
    successes = [
        ('', NS(foo=None)),
        ('a', NS(foo='a')),
    ]


class TestPositionalsNargsOptionalDefault(ParserTestCase):
    """Tests an Optional Positional with a default value"""

    argument_signatures = [Sig('foo', nargs='?', default=42)]
    failures = ['-x', 'a b']
    successes = [
        ('', NS(foo=42)),
        ('a', NS(foo='a')),
    ]


class TestPositionalsNargsOptionalConvertedDefault(ParserTestCase):
    """Tests an Optional Positional with a default value
    that needs to be converted to the appropriate type.
    """

    argument_signatures = [
        Sig('foo', nargs='?', type=int, default='42'),
    ]
    failures = ['-x', 'a b', '1 2']
    successes = [
        ('', NS(foo=42)),
        ('1', NS(foo=1)),
    ]


class TestPositionalsNargsNoneNone(ParserTestCase):
    """Test two Positionals that don't specify nargs"""

    argument_signatures = [Sig('foo'), Sig('bar')]
    failures = ['', '-x', 'a', 'a b c']
    successes = [
        ('a b', NS(foo='a', bar='b')),
    ]


class TestPositionalsNargsNone1(ParserTestCase):
    """Test a Positional with no nargs followed by one with 1"""

    argument_signatures = [Sig('foo'), Sig('bar', nargs=1)]
    failures = ['', '--foo', 'a', 'a b c']
    successes = [
        ('a b', NS(foo='a', bar=['b'])),
    ]


class TestPositionalsNargs2None(ParserTestCase):
    """Test a Positional with 2 nargs followed by one with none"""

    argument_signatures = [Sig('foo', nargs=2), Sig('bar')]
    failures = ['', '--foo', 'a', 'a b', 'a b c d']
    successes = [
        ('a b c', NS(foo=['a', 'b'], bar='c')),
    ]


class TestPositionalsNargsNoneZeroOrMore(ParserTestCase):
    """Test a Positional with no nargs followed by one with unlimited"""

    argument_signatures = [Sig('foo'), Sig('bar', nargs='*')]
    failures = ['', '--foo']
    successes = [
        ('a', NS(foo='a', bar=[])),
        ('a b', NS(foo='a', bar=['b'])),
        ('a b c', NS(foo='a', bar=['b', 'c'])),
    ]


class TestPositionalsNargsNoneOneOrMore(ParserTestCase):
    """Test a Positional with no nargs followed by one with one or more"""

    argument_signatures = [Sig('foo'), Sig('bar', nargs='+')]
    failures = ['', '--foo', 'a']
    successes = [
        ('a b', NS(foo='a', bar=['b'])),
        ('a b c', NS(foo='a', bar=['b', 'c'])),
    ]


class TestPositionalsNargsNoneOptional(ParserTestCase):
    """Test a Positional with no nargs followed by one with an Optional"""

    argument_signatures = [Sig('foo'), Sig('bar', nargs='?')]
    failures = ['', '--foo', 'a b c']
    successes = [
        ('a', NS(foo='a', bar=None)),
        ('a b', NS(foo='a', bar='b')),
    ]


class TestPositionalsNargsZeroOrMoreNone(ParserTestCase):
    """Test a Positional with unlimited nargs followed by one with none"""

    argument_signatures = [Sig('foo', nargs='*'), Sig('bar')]
    failures = ['', '--foo']
    successes = [
        ('a', NS(foo=[], bar='a')),
        ('a b', NS(foo=['a'], bar='b')),
        ('a b c', NS(foo=['a', 'b'], bar='c')),
    ]


class TestPositionalsNargsOneOrMoreNone(ParserTestCase):
    """Test a Positional with one or more nargs followed by one with none"""

    argument_signatures = [Sig('foo', nargs='+'), Sig('bar')]
    failures = ['', '--foo', 'a']
    successes = [
        ('a b', NS(foo=['a'], bar='b')),
        ('a b c', NS(foo=['a', 'b'], bar='c')),
    ]


class TestPositionalsNargsOptionalNone(ParserTestCase):
    """Test a Positional with an Optional nargs followed by one with none"""

    argument_signatures = [Sig('foo', nargs='?', default=42), Sig('bar')]
    failures = ['', '--foo', 'a b c']
    successes = [
        ('a', NS(foo=42, bar='a')),
        ('a b', NS(foo='a', bar='b')),
    ]


class TestPositionalsNargs2ZeroOrMore(ParserTestCase):
    """Test a Positional with 2 nargs followed by one with unlimited"""

    argument_signatures = [Sig('foo', nargs=2), Sig('bar', nargs='*')]
    failures = ['', '--foo', 'a']
    successes = [
        ('a b', NS(foo=['a', 'b'], bar=[])),
        ('a b c', NS(foo=['a', 'b'], bar=['c'])),
    ]


class TestPositionalsNargs2OneOrMore(ParserTestCase):
    """Test a Positional with 2 nargs followed by one with one or more"""

    argument_signatures = [Sig('foo', nargs=2), Sig('bar', nargs='+')]
    failures = ['', '--foo', 'a', 'a b']
    successes = [
        ('a b c', NS(foo=['a', 'b'], bar=['c'])),
    ]


class TestPositionalsNargs2Optional(ParserTestCase):
    """Test a Positional with 2 nargs followed by one optional"""

    argument_signatures = [Sig('foo', nargs=2), Sig('bar', nargs='?')]
    failures = ['', '--foo', 'a', 'a b c d']
    successes = [
        ('a b', NS(foo=['a', 'b'], bar=None)),
        ('a b c', NS(foo=['a', 'b'], bar='c')),
    ]


class TestPositionalsNargsZeroOrMore1(ParserTestCase):
    """Test a Positional with unlimited nargs followed by one with 1"""

    argument_signatures = [Sig('foo', nargs='*'), Sig('bar', nargs=1)]
    failures = ['', '--foo', ]
    successes = [
        ('a', NS(foo=[], bar=['a'])),
        ('a b', NS(foo=['a'], bar=['b'])),
        ('a b c', NS(foo=['a', 'b'], bar=['c'])),
    ]


class TestPositionalsNargsOneOrMore1(ParserTestCase):
    """Test a Positional with one or more nargs followed by one with 1"""

    argument_signatures = [Sig('foo', nargs='+'), Sig('bar', nargs=1)]
    failures = ['', '--foo', 'a']
    successes = [
        ('a b', NS(foo=['a'], bar=['b'])),
        ('a b c', NS(foo=['a', 'b'], bar=['c'])),
    ]


class TestPositionalsNargsOptional1(ParserTestCase):
    """Test a Positional with an Optional nargs followed by one with 1"""

    argument_signatures = [Sig('foo', nargs='?'), Sig('bar', nargs=1)]
    failures = ['', '--foo', 'a b c']
    successes = [
        ('a', NS(foo=None, bar=['a'])),
        ('a b', NS(foo='a', bar=['b'])),
    ]


class TestPositionalsNargsNoneZeroOrMore1(ParserTestCase):
    """Test three Positionals: no nargs, unlimited nargs and 1 nargs"""

    argument_signatures = [
        Sig('foo'),
        Sig('bar', nargs='*'),
        Sig('baz', nargs=1),
    ]
    failures = ['', '--foo', 'a']
    successes = [
        ('a b', NS(foo='a', bar=[], baz=['b'])),
        ('a b c', NS(foo='a', bar=['b'], baz=['c'])),
    ]


class TestPositionalsNargsNoneOneOrMore1(ParserTestCase):
    """Test three Positionals: no nargs, one or more nargs and 1 nargs"""

    argument_signatures = [
        Sig('foo'),
        Sig('bar', nargs='+'),
        Sig('baz', nargs=1),
    ]
    failures = ['', '--foo', 'a', 'b']
    successes = [
        ('a b c', NS(foo='a', bar=['b'], baz=['c'])),
        ('a b c d', NS(foo='a', bar=['b', 'c'], baz=['d'])),
    ]


class TestPositionalsNargsNoneOptional1(ParserTestCase):
    """Test three Positionals: no nargs, optional narg and 1 nargs"""

    argument_signatures = [
        Sig('foo'),
        Sig('bar', nargs='?', default=0.625),
        Sig('baz', nargs=1),
    ]
    failures = ['', '--foo', 'a']
    successes = [
        ('a b', NS(foo='a', bar=0.625, baz=['b'])),
        ('a b c', NS(foo='a', bar='b', baz=['c'])),
    ]


class TestPositionalsNargsOptionalOptional(ParserTestCase):
    """Test two optional nargs"""

    argument_signatures = [
        Sig('foo', nargs='?'),
        Sig('bar', nargs='?', default=42),
    ]
    failures = ['--foo', 'a b c']
    successes = [
        ('', NS(foo=None, bar=42)),
        ('a', NS(foo='a', bar=42)),
        ('a b', NS(foo='a', bar='b')),
    ]


class TestPositionalsNargsOptionalZeroOrMore(ParserTestCase):
    """Test an Optional narg followed by unlimited nargs"""

    argument_signatures = [Sig('foo', nargs='?'), Sig('bar', nargs='*')]
    failures = ['--foo']
    successes = [
        ('', NS(foo=None, bar=[])),
        ('a', NS(foo='a', bar=[])),
        ('a b', NS(foo='a', bar=['b'])),
        ('a b c', NS(foo='a', bar=['b', 'c'])),
    ]


class TestPositionalsNargsOptionalOneOrMore(ParserTestCase):
    """Test an Optional narg followed by one or more nargs"""

    argument_signatures = [Sig('foo', nargs='?'), Sig('bar', nargs='+')]
    failures = ['', '--foo']
    successes = [
        ('a', NS(foo=None, bar=['a'])),
        ('a b', NS(foo='a', bar=['b'])),
        ('a b c', NS(foo='a', bar=['b', 'c'])),
    ]


class TestPositionalsChoicesString(ParserTestCase):
    """Test a set of single-character choices"""

    argument_signatures = [Sig('spam', choices=set('abcdefg'))]
    failures = ['', '--foo', 'h', '42', 'ef']
    successes = [
        ('a', NS(spam='a')),
        ('g', NS(spam='g')),
    ]


class TestPositionalsChoicesInt(ParserTestCase):
    """Test a set of integer choices"""

    argument_signatures = [Sig('spam', type=int, choices=range(20))]
    failures = ['', '--foo', 'h', '42', 'ef']
    successes = [
        ('4', NS(spam=4)),
        ('15', NS(spam=15)),
    ]


class TestPositionalsActionAppend(ParserTestCase):
    """Test the 'append' action"""

    argument_signatures = [
        Sig('spam', action='append'),
        Sig('spam', action='append', nargs=2),
    ]
    failures = ['', '--foo', 'a', 'a b', 'a b c d']
    successes = [
        ('a b c', NS(spam=['a', ['b', 'c']])),
    ]

# ========================================
# Combined optionals and positionals tests
# ========================================

class TestOptionalsNumericAndPositionals(ParserTestCase):
    """Tests negative number args when numeric options are present"""

    argument_signatures = [
        Sig('x', nargs='?'),
        Sig('-4', dest='y', action='store_true'),
    ]
    failures = ['-2', '-315']
    successes = [
        ('', NS(x=None, y=False)),
        ('a', NS(x='a', y=False)),
        ('-4', NS(x=None, y=True)),
        ('-4 a', NS(x='a', y=True)),
    ]


class TestOptionalsAlmostNumericAndPositionals(ParserTestCase):
    """Tests negative number args when almost numeric options are present"""

    argument_signatures = [
        Sig('x', nargs='?'),
        Sig('-k4', dest='y', action='store_true'),
    ]
    failures = ['-k3']
    successes = [
        ('', NS(x=None, y=False)),
        ('-2', NS(x='-2', y=False)),
        ('a', NS(x='a', y=False)),
        ('-k4', NS(x=None, y=True)),
        ('-k4 a', NS(x='a', y=True)),
    ]


class TestEmptyAndSpaceContainingArguments(ParserTestCase):

    argument_signatures = [
        Sig('x', nargs='?'),
        Sig('-y', '--yyy', dest='y'),
    ]
    failures = ['-y']
    successes = [
        ([''], NS(x='', y=None)),
        (['a badger'], NS(x='a badger', y=None)),
        (['-a badger'], NS(x='-a badger', y=None)),
        (['-y', ''], NS(x=None, y='')),
        (['-y', 'a badger'], NS(x=None, y='a badger')),
        (['-y', '-a badger'], NS(x=None, y='-a badger')),
        (['--yyy=a badger'], NS(x=None, y='a badger')),
        (['--yyy=-a badger'], NS(x=None, y='-a badger')),
    ]


class TestPrefixCharacterOnlyArguments(ParserTestCase):

    parser_signature = Sig(prefix_chars='-+')
    argument_signatures = [
        Sig('-', dest='x', nargs='?', const='badger'),
        Sig('+', dest='y', type=int, default=42),
        Sig('-+-', dest='z', action='store_true'),
    ]
    failures = ['-y', '+ -']
    successes = [
        ('', NS(x=None, y=42, z=False)),
        ('-', NS(x='badger', y=42, z=False)),
        ('- X', NS(x='X', y=42, z=False)),
        ('+ -3', NS(x=None, y=-3, z=False)),
        ('-+-', NS(x=None, y=42, z=True)),
        ('- ===', NS(x='===', y=42, z=False)),
    ]


class TestNargsZeroOrMore(ParserTestCase):
    """Tests specifying an args for an Optional that accepts zero or more"""

    argument_signatures = [Sig('-x', nargs='*'), Sig('y', nargs='*')]
    failures = []
    successes = [
        ('', NS(x=None, y=[])),
        ('-x', NS(x=[], y=[])),
        ('-x a', NS(x=['a'], y=[])),
        ('-x a -- b', NS(x=['a'], y=['b'])),
        ('a', NS(x=None, y=['a'])),
        ('a -x', NS(x=[], y=['a'])),
        ('a -x b', NS(x=['b'], y=['a'])),
    ]


class TestNargsRemainder(ParserTestCase):
    """Tests specifying a positional with nargs=REMAINDER"""

    argument_signatures = [Sig('x'), Sig('y', nargs='...'), Sig('-z')]
    failures = ['', '-z', '-z Z']
    successes = [
        ('X', NS(x='X', y=[], z=None)),
        ('-z Z X', NS(x='X', y=[], z='Z')),
        ('X A B -z Z', NS(x='X', y=['A', 'B', '-z', 'Z'], z=None)),
        ('X Y --foo', NS(x='X', y=['Y', '--foo'], z=None)),
    ]


class TestOptionLike(ParserTestCase):
    """Tests options that may or may not be arguments"""

    argument_signatures = [
        Sig('-x', type=float),
        Sig('-3', type=float, dest='y'),
        Sig('z', nargs='*'),
    ]
    failures = ['-x', '-y2.5', '-xa', '-x -a',
                '-x -3', '-x -3.5', '-3 -3.5',
                '-x -2.5', '-x -2.5 a', '-3 -.5',
                'a x -1', '-x -1 a', '-3 -1 a']
    successes = [
        ('', NS(x=None, y=None, z=[])),
        ('-x 2.5', NS(x=2.5, y=None, z=[])),
        ('-x 2.5 a', NS(x=2.5, y=None, z=['a'])),
        ('-3.5', NS(x=None, y=0.5, z=[])),
        ('-3-.5', NS(x=None, y=-0.5, z=[])),
        ('-3 .5', NS(x=None, y=0.5, z=[])),
        ('a -3.5', NS(x=None, y=0.5, z=['a'])),
        ('a', NS(x=None, y=None, z=['a'])),
        ('a -x 1', NS(x=1.0, y=None, z=['a'])),
        ('-x 1 a', NS(x=1.0, y=None, z=['a'])),
        ('-3 1 a', NS(x=None, y=1.0, z=['a'])),
    ]


class TestDefaultSuppress(ParserTestCase):
    """Test actions with suppressed defaults"""

    argument_signatures = [
        Sig('foo', nargs='?', default=argparse.SUPPRESS),
        Sig('bar', nargs='*', default=argparse.SUPPRESS),
        Sig('--baz', action='store_true', default=argparse.SUPPRESS),
    ]
    failures = ['-x']
    successes = [
        ('', NS()),
        ('a', NS(foo='a')),
        ('a b', NS(foo='a', bar=['b'])),
        ('--baz', NS(baz=True)),
        ('a --baz', NS(foo='a', baz=True)),
        ('--baz a b', NS(foo='a', bar=['b'], baz=True)),
    ]


class TestParserDefaultSuppress(ParserTestCase):
    """Test actions with a parser-level default of SUPPRESS"""

    parser_signature = Sig(argument_default=argparse.SUPPRESS)
    argument_signatures = [
        Sig('foo', nargs='?'),
        Sig('bar', nargs='*'),
        Sig('--baz', action='store_true'),
    ]
    failures = ['-x']
    successes = [
        ('', NS()),
        ('a', NS(foo='a')),
        ('a b', NS(foo='a', bar=['b'])),
        ('--baz', NS(baz=True)),
        ('a --baz', NS(foo='a', baz=True)),
        ('--baz a b', NS(foo='a', bar=['b'], baz=True)),
    ]


class TestParserDefault42(ParserTestCase):
    """Test actions with a parser-level default of 42"""

    parser_signature = Sig(argument_default=42)
    argument_signatures = [
        Sig('--version', action='version', version='1.0'),
        Sig('foo', nargs='?'),
        Sig('bar', nargs='*'),
        Sig('--baz', action='store_true'),
    ]
    failures = ['-x']
    successes = [
        ('', NS(foo=42, bar=42, baz=42, version=42)),
        ('a', NS(foo='a', bar=42, baz=42, version=42)),
        ('a b', NS(foo='a', bar=['b'], baz=42, version=42)),
        ('--baz', NS(foo=42, bar=42, baz=True, version=42)),
        ('a --baz', NS(foo='a', bar=42, baz=True, version=42)),
        ('--baz a b', NS(foo='a', bar=['b'], baz=True, version=42)),
    ]


class TestArgumentsFromFile(TempDirMixin, ParserTestCase):
    """Test reading arguments from a file"""

    def setUp(self):
        super(TestArgumentsFromFile, self).setUp()
        file_texts = [
            ('hello', 'hello world!\n'),
            ('recursive', '-a\n'
                          'A\n'
                          '@hello'),
            ('invalid', '@no-such-path\n'),
        ]
        for path, text in file_texts:
            file = open(path, 'w')
            file.write(text)
            file.close()

    parser_signature = Sig(fromfile_prefix_chars='@')
    argument_signatures = [
        Sig('-a'),
        Sig('x'),
        Sig('y', nargs='+'),
    ]
    failures = ['', '-b', 'X', '@invalid', '@missing']
    successes = [
        ('X Y', NS(a=None, x='X', y=['Y'])),
        ('X -a A Y Z', NS(a='A', x='X', y=['Y', 'Z'])),
        ('@hello X', NS(a=None, x='hello world!', y=['X'])),
        ('X @hello', NS(a=None, x='X', y=['hello world!'])),
        ('-a B @recursive Y Z', NS(a='A', x='hello world!', y=['Y', 'Z'])),
        ('X @recursive Z -a B', NS(a='B', x='X', y=['hello world!', 'Z'])),
        (["-a", "", "X", "Y"], NS(a='', x='X', y=['Y'])),
    ]


class TestArgumentsFromFileConverter(TempDirMixin, ParserTestCase):
    """Test reading arguments from a file"""

    def setUp(self):
        super(TestArgumentsFromFileConverter, self).setUp()
        file_texts = [
            ('hello', 'hello world!\n'),
        ]
        for path, text in file_texts:
            file = open(path, 'w')
            file.write(text)
            file.close()

    class FromFileConverterArgumentParser(ErrorRaisingArgumentParser):

        def convert_arg_line_to_args(self, arg_line):
            for arg in arg_line.split():
                if not arg.strip():
                    continue
                yield arg
    parser_class = FromFileConverterArgumentParser
    parser_signature = Sig(fromfile_prefix_chars='@')
    argument_signatures = [
        Sig('y', nargs='+'),
    ]
    failures = []
    successes = [
        ('@hello X', NS(y=['hello', 'world!', 'X'])),
    ]



# =====================
# Help formatting tests
# =====================


class TestHelpBiggerOptionals(HelpTestCase):
    """Make sure that argument help aligns when options are longer"""

    parser_signature = Sig(prog='PROG', description='DESCRIPTION',
                           epilog='EPILOG')
    argument_signatures = [
        Sig('-v', '--version', action='version', version='0.1'),
        Sig('-x', action='store_true', help='X HELP'),
        Sig('--y', help='Y HELP'),
        Sig('foo', help='FOO HELP'),
        Sig('bar', help='BAR HELP'),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PROG [-h] [-v] [-x] [--y Y] foo bar
        '''
    help = usage + '''\

        DESCRIPTION

        positional arguments:
          foo            FOO HELP
          bar            BAR HELP

        optional arguments:
          -h, --help     show this help message and exit
          -v, --version  show program's version number and exit
          -x             X HELP
          --y Y          Y HELP

        EPILOG
    '''
    version = '''\
        0.1
        '''

class TestShortColumns(HelpTestCase):
    '''Test extremely small number of columns.

    TestCase prevents "COLUMNS" from being too small in the tests themselves,
    but we don't want any exceptions thrown in such case. Only ugly representation.
    '''
    def setUp(self):
        env = support.EnvironmentVarGuard()
        env.set("COLUMNS", '15')
        self.addCleanup(env.__exit__)

    parser_signature            = TestHelpBiggerOptionals.parser_signature
    argument_signatures         = TestHelpBiggerOptionals.argument_signatures
    argument_group_signatures   = TestHelpBiggerOptionals.argument_group_signatures
    usage = '''\
        usage: PROG
               [-h]
               [-v]
               [-x]
               [--y Y]
               foo
               bar
        '''
    help = usage + '''\

        DESCRIPTION

        positional arguments:
          foo
            FOO HELP
          bar
            BAR HELP

        optional arguments:
          -h, --help
            show this
            help
            message and
            exit
          -v, --version
            show
            program's
            version
            number and
            exit
          -x
            X HELP
          --y Y
            Y HELP

        EPILOG
    '''
    version                     = TestHelpBiggerOptionals.version


class TestHelpBiggerOptionalGroups(HelpTestCase):
    """Make sure that argument help aligns when options are longer"""

    parser_signature = Sig(prog='PROG', description='DESCRIPTION',
                           epilog='EPILOG')
    argument_signatures = [
        Sig('-v', '--version', action='version', version='0.1'),
        Sig('-x', action='store_true', help='X HELP'),
        Sig('--y', help='Y HELP'),
        Sig('foo', help='FOO HELP'),
        Sig('bar', help='BAR HELP'),
    ]
    argument_group_signatures = [
        (Sig('GROUP TITLE', description='GROUP DESCRIPTION'), [
            Sig('baz', help='BAZ HELP'),
            Sig('-z', nargs='+', help='Z HELP')]),
    ]
    usage = '''\
        usage: PROG [-h] [-v] [-x] [--y Y] [-z Z [Z ...]] foo bar baz
        '''
    help = usage + '''\

        DESCRIPTION

        positional arguments:
          foo            FOO HELP
          bar            BAR HELP

        optional arguments:
          -h, --help     show this help message and exit
          -v, --version  show program's version number and exit
          -x             X HELP
          --y Y          Y HELP

        GROUP TITLE:
          GROUP DESCRIPTION

          baz            BAZ HELP
          -z Z [Z ...]   Z HELP

        EPILOG
    '''
    version = '''\
        0.1
        '''


class TestHelpBiggerPositionals(HelpTestCase):
    """Make sure that help aligns when arguments are longer"""

    parser_signature = Sig(usage='USAGE', description='DESCRIPTION')
    argument_signatures = [
        Sig('-x', action='store_true', help='X HELP'),
        Sig('--y', help='Y HELP'),
        Sig('ekiekiekifekang', help='EKI HELP'),
        Sig('bar', help='BAR HELP'),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: USAGE
        '''
    help = usage + '''\

        DESCRIPTION

        positional arguments:
          ekiekiekifekang  EKI HELP
          bar              BAR HELP

        optional arguments:
          -h, --help       show this help message and exit
          -x               X HELP
          --y Y            Y HELP
        '''

    version = ''


class TestHelpReformatting(HelpTestCase):
    """Make sure that text after short names starts on the first line"""

    parser_signature = Sig(
        prog='PROG',
        description='   oddly    formatted\n'
                    'description\n'
                    '\n'
                    'that is so long that it should go onto multiple '
                    'lines when wrapped')
    argument_signatures = [
        Sig('-x', metavar='XX', help='oddly\n'
                                     '    formatted -x help'),
        Sig('y', metavar='yyy', help='normal y help'),
    ]
    argument_group_signatures = [
        (Sig('title', description='\n'
                                  '    oddly formatted group\n'
                                  '\n'
                                  'description'),
         [Sig('-a', action='store_true',
              help=' oddly \n'
                   'formatted    -a  help  \n'
                   '    again, so long that it should be wrapped over '
                   'multiple lines')]),
    ]
    usage = '''\
        usage: PROG [-h] [-x XX] [-a] yyy
        '''
    help = usage + '''\

        oddly formatted description that is so long that it should go onto \
multiple
        lines when wrapped

        positional arguments:
          yyy         normal y help

        optional arguments:
          -h, --help  show this help message and exit
          -x XX       oddly formatted -x help

        title:
          oddly formatted group description

          -a          oddly formatted -a help again, so long that it should \
be wrapped
                      over multiple lines
        '''
    version = ''


class TestHelpWrappingShortNames(HelpTestCase):
    """Make sure that text after short names starts on the first line"""

    parser_signature = Sig(prog='PROG', description= 'D\nD' * 30)
    argument_signatures = [
        Sig('-x', metavar='XX', help='XHH HX' * 20),
        Sig('y', metavar='yyy', help='YH YH' * 20),
    ]
    argument_group_signatures = [
        (Sig('ALPHAS'), [
            Sig('-a', action='store_true', help='AHHH HHA' * 10)]),
    ]
    usage = '''\
        usage: PROG [-h] [-x XX] [-a] yyy
        '''
    help = usage + '''\

        D DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD \
DD DD DD
        DD DD DD DD D

        positional arguments:
          yyy         YH YHYH YHYH YHYH YHYH YHYH YHYH YHYH YHYH YHYH YHYH \
YHYH YHYH
                      YHYH YHYH YHYH YHYH YHYH YHYH YHYH YH

        optional arguments:
          -h, --help  show this help message and exit
          -x XX       XHH HXXHH HXXHH HXXHH HXXHH HXXHH HXXHH HXXHH HXXHH \
HXXHH HXXHH
                      HXXHH HXXHH HXXHH HXXHH HXXHH HXXHH HXXHH HXXHH HXXHH HX

        ALPHAS:
          -a          AHHH HHAAHHH HHAAHHH HHAAHHH HHAAHHH HHAAHHH HHAAHHH \
HHAAHHH
                      HHAAHHH HHAAHHH HHA
        '''
    version = ''


class TestHelpWrappingLongNames(HelpTestCase):
    """Make sure that text after long names starts on the next line"""

    parser_signature = Sig(usage='USAGE', description= 'D D' * 30)
    argument_signatures = [
        Sig('-v', '--version', action='version', version='V V' * 30),
        Sig('-x', metavar='X' * 25, help='XH XH' * 20),
        Sig('y', metavar='y' * 25, help='YH YH' * 20),
    ]
    argument_group_signatures = [
        (Sig('ALPHAS'), [
            Sig('-a', metavar='A' * 25, help='AH AH' * 20),
            Sig('z', metavar='z' * 25, help='ZH ZH' * 20)]),
    ]
    usage = '''\
        usage: USAGE
        '''
    help = usage + '''\

        D DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD \
DD DD DD
        DD DD DD DD D

        positional arguments:
          yyyyyyyyyyyyyyyyyyyyyyyyy
                                YH YHYH YHYH YHYH YHYH YHYH YHYH YHYH YHYH \
YHYH YHYH
                                YHYH YHYH YHYH YHYH YHYH YHYH YHYH YHYH YHYH YH

        optional arguments:
          -h, --help            show this help message and exit
          -v, --version         show program's version number and exit
          -x XXXXXXXXXXXXXXXXXXXXXXXXX
                                XH XHXH XHXH XHXH XHXH XHXH XHXH XHXH XHXH \
XHXH XHXH
                                XHXH XHXH XHXH XHXH XHXH XHXH XHXH XHXH XHXH XH

        ALPHAS:
          -a AAAAAAAAAAAAAAAAAAAAAAAAA
                                AH AHAH AHAH AHAH AHAH AHAH AHAH AHAH AHAH \
AHAH AHAH
                                AHAH AHAH AHAH AHAH AHAH AHAH AHAH AHAH AHAH AH
          zzzzzzzzzzzzzzzzzzzzzzzzz
                                ZH ZHZH ZHZH ZHZH ZHZH ZHZH ZHZH ZHZH ZHZH \
ZHZH ZHZH
                                ZHZH ZHZH ZHZH ZHZH ZHZH ZHZH ZHZH ZHZH ZHZH ZH
        '''
    version = '''\
        V VV VV VV VV VV VV VV VV VV VV VV VV VV VV VV VV VV VV VV VV VV VV \
VV VV VV
        VV VV VV VV V
        '''


class TestHelpUsage(HelpTestCase):
    """Test basic usage messages"""

    parser_signature = Sig(prog='PROG')
    argument_signatures = [
        Sig('-w', nargs='+', help='w'),
        Sig('-x', nargs='*', help='x'),
        Sig('a', help='a'),
        Sig('b', help='b', nargs=2),
        Sig('c', help='c', nargs='?'),
    ]
    argument_group_signatures = [
        (Sig('group'), [
            Sig('-y', nargs='?', help='y'),
            Sig('-z', nargs=3, help='z'),
            Sig('d', help='d', nargs='*'),
            Sig('e', help='e', nargs='+'),
        ])
    ]
    usage = '''\
        usage: PROG [-h] [-w W [W ...]] [-x [X [X ...]]] [-y [Y]] [-z Z Z Z]
                    a b b [c] [d [d ...]] e [e ...]
        '''
    help = usage + '''\

        positional arguments:
          a               a
          b               b
          c               c

        optional arguments:
          -h, --help      show this help message and exit
          -w W [W ...]    w
          -x [X [X ...]]  x

        group:
          -y [Y]          y
          -z Z Z Z        z
          d               d
          e               e
        '''
    version = ''


class TestHelpOnlyUserGroups(HelpTestCase):
    """Test basic usage messages"""

    parser_signature = Sig(prog='PROG', add_help=False)
    argument_signatures = []
    argument_group_signatures = [
        (Sig('xxxx'), [
            Sig('-x', help='x'),
            Sig('a', help='a'),
        ]),
        (Sig('yyyy'), [
            Sig('b', help='b'),
            Sig('-y', help='y'),
        ]),
    ]
    usage = '''\
        usage: PROG [-x X] [-y Y] a b
        '''
    help = usage + '''\

        xxxx:
          -x X  x
          a     a

        yyyy:
          b     b
          -y Y  y
        '''
    version = ''


class TestHelpUsageLongProg(HelpTestCase):
    """Test usage messages where the prog is long"""

    parser_signature = Sig(prog='P' * 60)
    argument_signatures = [
        Sig('-w', metavar='W'),
        Sig('-x', metavar='X'),
        Sig('a'),
        Sig('b'),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
               [-h] [-w W] [-x X] a b
        '''
    help = usage + '''\

        positional arguments:
          a
          b

        optional arguments:
          -h, --help  show this help message and exit
          -w W
          -x X
        '''
    version = ''


class TestHelpUsageLongProgOptionsWrap(HelpTestCase):
    """Test usage messages where the prog is long and the optionals wrap"""

    parser_signature = Sig(prog='P' * 60)
    argument_signatures = [
        Sig('-w', metavar='W' * 25),
        Sig('-x', metavar='X' * 25),
        Sig('-y', metavar='Y' * 25),
        Sig('-z', metavar='Z' * 25),
        Sig('a'),
        Sig('b'),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
               [-h] [-w WWWWWWWWWWWWWWWWWWWWWWWWW] \
[-x XXXXXXXXXXXXXXXXXXXXXXXXX]
               [-y YYYYYYYYYYYYYYYYYYYYYYYYY] [-z ZZZZZZZZZZZZZZZZZZZZZZZZZ]
               a b
        '''
    help = usage + '''\

        positional arguments:
          a
          b

        optional arguments:
          -h, --help            show this help message and exit
          -w WWWWWWWWWWWWWWWWWWWWWWWWW
          -x XXXXXXXXXXXXXXXXXXXXXXXXX
          -y YYYYYYYYYYYYYYYYYYYYYYYYY
          -z ZZZZZZZZZZZZZZZZZZZZZZZZZ
        '''
    version = ''


class TestHelpUsageLongProgPositionalsWrap(HelpTestCase):
    """Test usage messages where the prog is long and the positionals wrap"""

    parser_signature = Sig(prog='P' * 60, add_help=False)
    argument_signatures = [
        Sig('a' * 25),
        Sig('b' * 25),
        Sig('c' * 25),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
               aaaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb
               ccccccccccccccccccccccccc
        '''
    help = usage + '''\

        positional arguments:
          aaaaaaaaaaaaaaaaaaaaaaaaa
          bbbbbbbbbbbbbbbbbbbbbbbbb
          ccccccccccccccccccccccccc
        '''
    version = ''


class TestHelpUsageOptionalsWrap(HelpTestCase):
    """Test usage messages where the optionals wrap"""

    parser_signature = Sig(prog='PROG')
    argument_signatures = [
        Sig('-w', metavar='W' * 25),
        Sig('-x', metavar='X' * 25),
        Sig('-y', metavar='Y' * 25),
        Sig('-z', metavar='Z' * 25),
        Sig('a'),
        Sig('b'),
        Sig('c'),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PROG [-h] [-w WWWWWWWWWWWWWWWWWWWWWWWWW] \
[-x XXXXXXXXXXXXXXXXXXXXXXXXX]
                    [-y YYYYYYYYYYYYYYYYYYYYYYYYY] \
[-z ZZZZZZZZZZZZZZZZZZZZZZZZZ]
                    a b c
        '''
    help = usage + '''\

        positional arguments:
          a
          b
          c

        optional arguments:
          -h, --help            show this help message and exit
          -w WWWWWWWWWWWWWWWWWWWWWWWWW
          -x XXXXXXXXXXXXXXXXXXXXXXXXX
          -y YYYYYYYYYYYYYYYYYYYYYYYYY
          -z ZZZZZZZZZZZZZZZZZZZZZZZZZ
        '''
    version = ''


class TestHelpUsagePositionalsWrap(HelpTestCase):
    """Test usage messages where the positionals wrap"""

    parser_signature = Sig(prog='PROG')
    argument_signatures = [
        Sig('-x'),
        Sig('-y'),
        Sig('-z'),
        Sig('a' * 25),
        Sig('b' * 25),
        Sig('c' * 25),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PROG [-h] [-x X] [-y Y] [-z Z]
                    aaaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb
                    ccccccccccccccccccccccccc
        '''
    help = usage + '''\

        positional arguments:
          aaaaaaaaaaaaaaaaaaaaaaaaa
          bbbbbbbbbbbbbbbbbbbbbbbbb
          ccccccccccccccccccccccccc

        optional arguments:
          -h, --help            show this help message and exit
          -x X
          -y Y
          -z Z
        '''
    version = ''


class TestHelpUsageOptionalsPositionalsWrap(HelpTestCase):
    """Test usage messages where the optionals and positionals wrap"""

    parser_signature = Sig(prog='PROG')
    argument_signatures = [
        Sig('-x', metavar='X' * 25),
        Sig('-y', metavar='Y' * 25),
        Sig('-z', metavar='Z' * 25),
        Sig('a' * 25),
        Sig('b' * 25),
        Sig('c' * 25),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PROG [-h] [-x XXXXXXXXXXXXXXXXXXXXXXXXX] \
[-y YYYYYYYYYYYYYYYYYYYYYYYYY]
                    [-z ZZZZZZZZZZZZZZZZZZZZZZZZZ]
                    aaaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb
                    ccccccccccccccccccccccccc
        '''
    help = usage + '''\

        positional arguments:
          aaaaaaaaaaaaaaaaaaaaaaaaa
          bbbbbbbbbbbbbbbbbbbbbbbbb
          ccccccccccccccccccccccccc

        optional arguments:
          -h, --help            show this help message and exit
          -x XXXXXXXXXXXXXXXXXXXXXXXXX
          -y YYYYYYYYYYYYYYYYYYYYYYYYY
          -z ZZZZZZZZZZZZZZZZZZZZZZZZZ
        '''
    version = ''


class TestHelpUsageOptionalsOnlyWrap(HelpTestCase):
    """Test usage messages where there are only optionals and they wrap"""

    parser_signature = Sig(prog='PROG')
    argument_signatures = [
        Sig('-x', metavar='X' * 25),
        Sig('-y', metavar='Y' * 25),
        Sig('-z', metavar='Z' * 25),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PROG [-h] [-x XXXXXXXXXXXXXXXXXXXXXXXXX] \
[-y YYYYYYYYYYYYYYYYYYYYYYYYY]
                    [-z ZZZZZZZZZZZZZZZZZZZZZZZZZ]
        '''
    help = usage + '''\

        optional arguments:
          -h, --help            show this help message and exit
          -x XXXXXXXXXXXXXXXXXXXXXXXXX
          -y YYYYYYYYYYYYYYYYYYYYYYYYY
          -z ZZZZZZZZZZZZZZZZZZZZZZZZZ
        '''
    version = ''


class TestHelpUsagePositionalsOnlyWrap(HelpTestCase):
    """Test usage messages where there are only positionals and they wrap"""

    parser_signature = Sig(prog='PROG', add_help=False)
    argument_signatures = [
        Sig('a' * 25),
        Sig('b' * 25),
        Sig('c' * 25),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PROG aaaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb
                    ccccccccccccccccccccccccc
        '''
    help = usage + '''\

        positional arguments:
          aaaaaaaaaaaaaaaaaaaaaaaaa
          bbbbbbbbbbbbbbbbbbbbbbbbb
          ccccccccccccccccccccccccc
        '''
    version = ''


class TestHelpVariableExpansion(HelpTestCase):
    """Test that variables are expanded properly in help messages"""

    parser_signature = Sig(prog='PROG')
    argument_signatures = [
        Sig('-x', type=int,
            help='x %(prog)s %(default)s %(type)s %%'),
        Sig('-y', action='store_const', default=42, const='XXX',
            help='y %(prog)s %(default)s %(const)s'),
        Sig('--foo', choices='abc',
            help='foo %(prog)s %(default)s %(choices)s'),
        Sig('--bar', default='baz', choices=[1, 2], metavar='BBB',
            help='bar %(prog)s %(default)s %(dest)s'),
        Sig('spam', help='spam %(prog)s %(default)s'),
        Sig('badger', default=0.5, help='badger %(prog)s %(default)s'),
    ]
    argument_group_signatures = [
        (Sig('group'), [
            Sig('-a', help='a %(prog)s %(default)s'),
            Sig('-b', default=-1, help='b %(prog)s %(default)s'),
        ])
    ]
    usage = ('''\
        usage: PROG [-h] [-x X] [-y] [--foo {a,b,c}] [--bar BBB] [-a A] [-b B]
                    spam badger
        ''')
    help = usage + '''\

        positional arguments:
          spam           spam PROG None
          badger         badger PROG 0.5

        optional arguments:
          -h, --help     show this help message and exit
          -x X           x PROG None int %
          -y             y PROG 42 XXX
          --foo {a,b,c}  foo PROG None a, b, c
          --bar BBB      bar PROG baz bar

        group:
          -a A           a PROG None
          -b B           b PROG -1
        '''
    version = ''


class TestHelpVariableExpansionUsageSupplied(HelpTestCase):
    """Test that variables are expanded properly when usage= is present"""

    parser_signature = Sig(prog='PROG', usage='%(prog)s FOO')
    argument_signatures = []
    argument_group_signatures = []
    usage = ('''\
        usage: PROG FOO
        ''')
    help = usage + '''\

        optional arguments:
          -h, --help  show this help message and exit
        '''
    version = ''


class TestHelpVariableExpansionNoArguments(HelpTestCase):
    """Test that variables are expanded properly with no arguments"""

    parser_signature = Sig(prog='PROG', add_help=False)
    argument_signatures = []
    argument_group_signatures = []
    usage = ('''\
        usage: PROG
        ''')
    help = usage
    version = ''


class TestHelpSuppressUsage(HelpTestCase):
    """Test that items can be suppressed in usage messages"""

    parser_signature = Sig(prog='PROG', usage=argparse.SUPPRESS)
    argument_signatures = [
        Sig('--foo', help='foo help'),
        Sig('spam', help='spam help'),
    ]
    argument_group_signatures = []
    help = '''\
        positional arguments:
          spam        spam help

        optional arguments:
          -h, --help  show this help message and exit
          --foo FOO   foo help
        '''
    usage = ''
    version = ''


class TestHelpSuppressOptional(HelpTestCase):
    """Test that optional arguments can be suppressed in help messages"""

    parser_signature = Sig(prog='PROG', add_help=False)
    argument_signatures = [
        Sig('--foo', help=argparse.SUPPRESS),
        Sig('spam', help='spam help'),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PROG spam
        '''
    help = usage + '''\

        positional arguments:
          spam  spam help
        '''
    version = ''


class TestHelpSuppressOptionalGroup(HelpTestCase):
    """Test that optional groups can be suppressed in help messages"""

    parser_signature = Sig(prog='PROG')
    argument_signatures = [
        Sig('--foo', help='foo help'),
        Sig('spam', help='spam help'),
    ]
    argument_group_signatures = [
        (Sig('group'), [Sig('--bar', help=argparse.SUPPRESS)]),
    ]
    usage = '''\
        usage: PROG [-h] [--foo FOO] spam
        '''
    help = usage + '''\

        positional arguments:
          spam        spam help

        optional arguments:
          -h, --help  show this help message and exit
          --foo FOO   foo help
        '''
    version = ''


class TestHelpSuppressPositional(HelpTestCase):
    """Test that positional arguments can be suppressed in help messages"""

    parser_signature = Sig(prog='PROG')
    argument_signatures = [
        Sig('--foo', help='foo help'),
        Sig('spam', help=argparse.SUPPRESS),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PROG [-h] [--foo FOO]
        '''
    help = usage + '''\

        optional arguments:
          -h, --help  show this help message and exit
          --foo FOO   foo help
        '''
    version = ''


class TestHelpRequiredOptional(HelpTestCase):
    """Test that required options don't look optional"""

    parser_signature = Sig(prog='PROG')
    argument_signatures = [
        Sig('--foo', required=True, help='foo help'),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PROG [-h] --foo FOO
        '''
    help = usage + '''\

        optional arguments:
          -h, --help  show this help message and exit
          --foo FOO   foo help
        '''
    version = ''


class TestHelpAlternatePrefixChars(HelpTestCase):
    """Test that options display with different prefix characters"""

    parser_signature = Sig(prog='PROG', prefix_chars='^;', add_help=False)
    argument_signatures = [
        Sig('^^foo', action='store_true', help='foo help'),
        Sig(';b', ';;bar', help='bar help'),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PROG [^^foo] [;b BAR]
        '''
    help = usage + '''\

        optional arguments:
          ^^foo              foo help
          ;b BAR, ;;bar BAR  bar help
        '''
    version = ''


class TestHelpNoHelpOptional(HelpTestCase):
    """Test that the --help argument can be suppressed help messages"""

    parser_signature = Sig(prog='PROG', add_help=False)
    argument_signatures = [
        Sig('--foo', help='foo help'),
        Sig('spam', help='spam help'),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PROG [--foo FOO] spam
        '''
    help = usage + '''\

        positional arguments:
          spam       spam help

        optional arguments:
          --foo FOO  foo help
        '''
    version = ''


class TestHelpVersionOptional(HelpTestCase):
    """Test that the --version argument can be suppressed help messages"""

    parser_signature = Sig(prog='PROG')
    argument_signatures = [
        Sig('-v', '--version', action='version', version='1.0'),
        Sig('--foo', help='foo help'),
        Sig('spam', help='spam help'),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PROG [-h] [-v] [--foo FOO] spam
        '''
    help = usage + '''\

        positional arguments:
          spam           spam help

        optional arguments:
          -h, --help     show this help message and exit
          -v, --version  show program's version number and exit
          --foo FOO      foo help
        '''
    version = '''\
        1.0
        '''


class TestHelpNone(HelpTestCase):
    """Test that no errors occur if no help is specified"""

    parser_signature = Sig(prog='PROG')
    argument_signatures = [
        Sig('--foo'),
        Sig('spam'),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PROG [-h] [--foo FOO] spam
        '''
    help = usage + '''\

        positional arguments:
          spam

        optional arguments:
          -h, --help  show this help message and exit
          --foo FOO
        '''
    version = ''


class TestHelpTupleMetavar(HelpTestCase):
    """Test specifying metavar as a tuple"""

    parser_signature = Sig(prog='PROG')
    argument_signatures = [
        Sig('-w', help='w', nargs='+', metavar=('W1', 'W2')),
        Sig('-x', help='x', nargs='*', metavar=('X1', 'X2')),
        Sig('-y', help='y', nargs=3, metavar=('Y1', 'Y2', 'Y3')),
        Sig('-z', help='z', nargs='?', metavar=('Z1', )),
    ]
    argument_group_signatures = []
    usage = '''\
        usage: PROG [-h] [-w W1 [W2 ...]] [-x [X1 [X2 ...]]] [-y Y1 Y2 Y3] \
[-z [Z1]]
        '''
    help = usage + '''\

        optional arguments:
          -h, --help        show this help message and exit
          -w W1 [W2 ...]    w
          -x [X1 [X2 ...]]  x
          -y Y1 Y2 Y3       y
          -z [Z1]           z
        '''
    version = ''


class TestHelpRawText(HelpTestCase):
    """Test the RawTextHelpFormatter"""

    parser_signature = Sig(
        prog='PROG', formatter_class=argparse.RawTextHelpFormatter,
        description='Keep the formatting\n'
                    '    exactly as it is written\n'
                    '\n'
                    'here\n')

    argument_signatures = [
        Sig('--foo', help='    foo help should also\n'
                          'appear as given here'),
        Sig('spam', help='spam help'),
    ]
    argument_group_signatures = [
        (Sig('title', description='    This text\n'
                                  '  should be indented\n'
                                  '    exactly like it is here\n'),
         [Sig('--bar', help='bar help')]),
    ]
    usage = '''\
        usage: PROG [-h] [--foo FOO] [--bar BAR] spam
        '''
    help = usage + '''\

        Keep the formatting
            exactly as it is written

        here

        positional arguments:
          spam        spam help

        optional arguments:
          -h, --help  show this help message and exit
          --foo FOO       foo help should also
                      appear as given here

        title:
              This text
            should be indented
              exactly like it is here

          --bar BAR   bar help
        '''
    version = ''


class TestHelpRawDescription(HelpTestCase):
    """Test the RawTextHelpFormatter"""

    parser_signature = Sig(
        prog='PROG', formatter_class=argparse.RawDescriptionHelpFormatter,
        description='Keep the formatting\n'
                    '    exactly as it is written\n'
                    '\n'
                    'here\n')

    argument_signatures = [
        Sig('--foo', help='  foo help should not\n'
                          '    retain this odd formatting'),
        Sig('spam', help='spam help'),
    ]
    argument_group_signatures = [
        (Sig('title', description='    This text\n'
                                  '  should be indented\n'
                                  '    exactly like it is here\n'),
         [Sig('--bar', help='bar help')]),
    ]
    usage = '''\
        usage: PROG [-h] [--foo FOO] [--bar BAR] spam
        '''
    help = usage + '''\

        Keep the formatting
            exactly as it is written

        here

        positional arguments:
          spam        spam help

        optional arguments:
          -h, --help  show this help message and exit
          --foo FOO   foo help should not retain this odd formatting

        title:
              This text
            should be indented
              exactly like it is here

          --bar BAR   bar help
        '''
    version = ''


class TestHelpArgumentDefaults(HelpTestCase):
    """Test the ArgumentDefaultsHelpFormatter"""

    parser_signature = Sig(
        prog='PROG', formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        description='description')

    argument_signatures = [
        Sig('--foo', help='foo help - oh and by the way, %(default)s'),
        Sig('--bar', action='store_true', help='bar help'),
        Sig('spam', help='spam help'),
        Sig('badger', nargs='?', default='wooden', help='badger help'),
    ]
    argument_group_signatures = [
        (Sig('title', description='description'),
         [Sig('--baz', type=int, default=42, help='baz help')]),
    ]
    usage = '''\
        usage: PROG [-h] [--foo FOO] [--bar] [--baz BAZ] spam [badger]
        '''
    help = usage + '''\

        description

        positional arguments:
          spam        spam help
          badger      badger help (default: wooden)

        optional arguments:
          -h, --help  show this help message and exit
          --foo FOO   foo help - oh and by the way, None
          --bar       bar help (default: False)

        title:
          description

          --baz BAZ   baz help (default: 42)
        '''
    version = ''

class TestHelpVersionAction(HelpTestCase):
    """Test the default help for the version action"""

    parser_signature = Sig(prog='PROG', description='description')
    argument_signatures = [Sig('-V', '--version', action='version', version='3.6')]
    argument_group_signatures = []
    usage = '''\
        usage: PROG [-h] [-V]
        '''
    help = usage + '''\

        description

        optional arguments:
          -h, --help     show this help message and exit
          -V, --version  show program's version number and exit
        '''
    version = ''

class TestHelpSubparsersOrdering(HelpTestCase):
    """Test ordering of subcommands in help matches the code"""
    parser_signature = Sig(prog='PROG',
                           description='display some subcommands')
    argument_signatures = [Sig('-v', '--version', action='version', version='0.1')]

    subparsers_signatures = [Sig(name=name)
                             for name in ('a', 'b', 'c', 'd', 'e')]

    usage = '''\
        usage: PROG [-h] [-v] {a,b,c,d,e} ...
        '''

    help = usage + '''\

        display some subcommands

        positional arguments:
          {a,b,c,d,e}

        optional arguments:
          -h, --help     show this help message and exit
          -v, --version  show program's version number and exit
        '''

    version = '''\
        0.1
        '''

class TestHelpSubparsersWithHelpOrdering(HelpTestCase):
    """Test ordering of subcommands in help matches the code"""
    parser_signature = Sig(prog='PROG',
                           description='display some subcommands')
    argument_signatures = [Sig('-v', '--version', action='version', version='0.1')]

    subcommand_data = (('a', 'a subcommand help'),
                       ('b', 'b subcommand help'),
                       ('c', 'c subcommand help'),
                       ('d', 'd subcommand help'),
                       ('e', 'e subcommand help'),
                       )

    subparsers_signatures = [Sig(name=name, help=help)
                             for name, help in subcommand_data]

    usage = '''\
        usage: PROG [-h] [-v] {a,b,c,d,e} ...
        '''

    help = usage + '''\

        display some subcommands

        positional arguments:
          {a,b,c,d,e}
            a            a subcommand help
            b            b subcommand help
            c            c subcommand help
            d            d subcommand help
            e            e subcommand help

        optional arguments:
          -h, --help     show this help message and exit
          -v, --version  show program's version number and exit
        '''

    version = '''\
        0.1
        '''



class TestHelpMetavarTypeFormatter(HelpTestCase):
    """"""

    def custom_type(string):
        return string

    parser_signature = Sig(prog='PROG', description='description',
                           formatter_class=argparse.MetavarTypeHelpFormatter)
    argument_signatures = [Sig('a', type=int),
                           Sig('-b', type=custom_type),
                           Sig('-c', type=float, metavar='SOME FLOAT')]
    argument_group_signatures = []
    usage = '''\
        usage: PROG [-h] [-b custom_type] [-c SOME FLOAT] int
        '''
    help = usage + '''\

        description

        positional arguments:
          int

        optional arguments:
          -h, --help      show this help message and exit
          -b custom_type
          -c SOME FLOAT
        '''
    version = ''

# ================
# Subparsers tests
# ================

class TestAddSubparsers(TestCase):
    """Test the add_subparsers method"""

    def assertArgumentParserError(self, *args, **kwargs):
        self.assertRaises(ArgumentParserError, *args, **kwargs)

    def _get_parser(self, subparser_help=False, prefix_chars=None,
                    aliases=False):
        # create a parser with a subparsers argument
        if prefix_chars:
            parser = ErrorRaisingArgumentParser(
                prog='PROG', description='main description', prefix_chars=prefix_chars)
            parser.add_argument(
                prefix_chars[0] * 2 + 'foo', action='store_true', help='foo help')
        else:
            parser = ErrorRaisingArgumentParser(
                prog='PROG', description='main description')
            parser.add_argument(
                '--foo', action='store_true', help='foo help')
        parser.add_argument(
            'bar', type=float, help='bar help')

        # check that only one subparsers argument can be added
        subparsers_kwargs = {}
        if aliases:
            subparsers_kwargs['metavar'] = 'COMMAND'
            subparsers_kwargs['title'] = 'commands'
        else:
            subparsers_kwargs['help'] = 'command help'
        subparsers = parser.add_subparsers(**subparsers_kwargs)
        self.assertArgumentParserError(parser.add_subparsers)

        # add first sub-parser
        parser1_kwargs = dict(description='1 description')
        if subparser_help:
            parser1_kwargs['help'] = '1 help'
        if aliases:
            parser1_kwargs['aliases'] = ['1alias1', '1alias2']
        parser1 = subparsers.add_parser('1', **parser1_kwargs)
        parser1.add_argument('-w', type=int, help='w help')
        parser1.add_argument('x', choices='abc', help='x help')

        # add second sub-parser
        parser2_kwargs = dict(description='2 description')
        if subparser_help:
            parser2_kwargs['help'] = '2 help'
        parser2 = subparsers.add_parser('2', **parser2_kwargs)
        parser2.add_argument('-y', choices='123', help='y help')
        parser2.add_argument('z', type=complex, nargs='*', help='z help')

        # add third sub-parser
        parser3_kwargs = dict(description='3 description')
        if subparser_help:
            parser3_kwargs['help'] = '3 help'
        parser3 = subparsers.add_parser('3', **parser3_kwargs)
        parser3.add_argument('t', type=int, help='t help')
        parser3.add_argument('u', nargs='...', help='u help')

        # return the main parser
        return parser

    def setUp(self):
        super().setUp()
        self.parser = self._get_parser()
        self.command_help_parser = self._get_parser(subparser_help=True)

    def test_parse_args_failures(self):
        # check some failure cases:
        for args_str in ['', 'a', 'a a', '0.5 a', '0.5 1',
                         '0.5 1 -y', '0.5 2 -w']:
            args = args_str.split()
            self.assertArgumentParserError(self.parser.parse_args, args)

    def test_parse_args(self):
        # check some non-failure cases:
        self.assertEqual(
            self.parser.parse_args('0.5 1 b -w 7'.split()),
            NS(foo=False, bar=0.5, w=7, x='b'),
        )
        self.assertEqual(
            self.parser.parse_args('0.25 --foo 2 -y 2 3j -- -1j'.split()),
            NS(foo=True, bar=0.25, y='2', z=[3j, -1j]),
        )
        self.assertEqual(
            self.parser.parse_args('--foo 0.125 1 c'.split()),
            NS(foo=True, bar=0.125, w=None, x='c'),
        )
        self.assertEqual(
            self.parser.parse_args('-1.5 3 11 -- a --foo 7 -- b'.split()),
            NS(foo=False, bar=-1.5, t=11, u=['a', '--foo', '7', '--', 'b']),
        )

    def test_parse_known_args(self):
        self.assertEqual(
            self.parser.parse_known_args('0.5 1 b -w 7'.split()),
            (NS(foo=False, bar=0.5, w=7, x='b'), []),
        )
        self.assertEqual(
            self.parser.parse_known_args('0.5 -p 1 b -w 7'.split()),
            (NS(foo=False, bar=0.5, w=7, x='b'), ['-p']),
        )
        self.assertEqual(
            self.parser.parse_known_args('0.5 1 b -w 7 -p'.split()),
            (NS(foo=False, bar=0.5, w=7, x='b'), ['-p']),
        )
        self.assertEqual(
            self.parser.parse_known_args('0.5 1 b -q -rs -w 7'.split()),
            (NS(foo=False, bar=0.5, w=7, x='b'), ['-q', '-rs']),
        )
        self.assertEqual(
            self.parser.parse_known_args('0.5 -W 1 b -X Y -w 7 Z'.split()),
            (NS(foo=False, bar=0.5, w=7, x='b'), ['-W', '-X', 'Y', 'Z']),
        )

    def test_dest(self):
        parser = ErrorRaisingArgumentParser()
        parser.add_argument('--foo', action='store_true')
        subparsers = parser.add_subparsers(dest='bar')
        parser1 = subparsers.add_parser('1')
        parser1.add_argument('baz')
        self.assertEqual(NS(foo=False, bar='1', baz='2'),
                         parser.parse_args('1 2'.split()))

    def test_help(self):
        self.assertEqual(self.parser.format_usage(),
                         'usage: PROG [-h] [--foo] bar {1,2,3} ...\n')
        self.assertEqual(self.parser.format_help(), textwrap.dedent('''\
            usage: PROG [-h] [--foo] bar {1,2,3} ...

            main description

            positional arguments:
              bar         bar help
              {1,2,3}     command help

            optional arguments:
              -h, --help  show this help message and exit
              --foo       foo help
            '''))

    def test_help_extra_prefix_chars(self):
        # Make sure - is still used for help if it is a non-first prefix char
        parser = self._get_parser(prefix_chars='+:-')
        self.assertEqual(parser.format_usage(),
                         'usage: PROG [-h] [++foo] bar {1,2,3} ...\n')
        self.assertEqual(parser.format_help(), textwrap.dedent('''\
            usage: PROG [-h] [++foo] bar {1,2,3} ...

            main description

            positional arguments:
              bar         bar help
              {1,2,3}     command help

            optional arguments:
              -h, --help  show this help message and exit
              ++foo       foo help
            '''))


    def test_help_alternate_prefix_chars(self):
        parser = self._get_parser(prefix_chars='+:/')
        self.assertEqual(parser.format_usage(),
                         'usage: PROG [+h] [++foo] bar {1,2,3} ...\n')
        self.assertEqual(parser.format_help(), textwrap.dedent('''\
            usage: PROG [+h] [++foo] bar {1,2,3} ...

            main description

            positional arguments:
              bar         bar help
              {1,2,3}     command help

            optional arguments:
              +h, ++help  show this help message and exit
              ++foo       foo help
            '''))

    def test_parser_command_help(self):
        self.assertEqual(self.command_help_parser.format_usage(),
                         'usage: PROG [-h] [--foo] bar {1,2,3} ...\n')
        self.assertEqual(self.command_help_parser.format_help(),
                         textwrap.dedent('''\
            usage: PROG [-h] [--foo] bar {1,2,3} ...

            main description

            positional arguments:
              bar         bar help
              {1,2,3}     command help
                1         1 help
                2         2 help
                3         3 help

            optional arguments:
              -h, --help  show this help message and exit
              --foo       foo help
            '''))

    def test_subparser_title_help(self):
        parser = ErrorRaisingArgumentParser(prog='PROG',
                                            description='main description')
        parser.add_argument('--foo', action='store_true', help='foo help')
        parser.add_argument('bar', help='bar help')
        subparsers = parser.add_subparsers(title='subcommands',
                                           description='command help',
                                           help='additional text')
        parser1 = subparsers.add_parser('1')
        parser2 = subparsers.add_parser('2')
        self.assertEqual(parser.format_usage(),
                         'usage: PROG [-h] [--foo] bar {1,2} ...\n')
        self.assertEqual(parser.format_help(), textwrap.dedent('''\
            usage: PROG [-h] [--foo] bar {1,2} ...

            main description

            positional arguments:
              bar         bar help

            optional arguments:
              -h, --help  show this help message and exit
              --foo       foo help

            subcommands:
              command help

              {1,2}       additional text
            '''))

    def _test_subparser_help(self, args_str, expected_help):
        try:
            self.parser.parse_args(args_str.split())
        except ArgumentParserError:
            err = sys.exc_info()[1]
            if err.stdout != expected_help:
                print(repr(expected_help))
                print(repr(err.stdout))
            self.assertEqual(err.stdout, expected_help)

    def test_subparser1_help(self):
        self._test_subparser_help('5.0 1 -h', textwrap.dedent('''\
            usage: PROG bar 1 [-h] [-w W] {a,b,c}

            1 description

            positional arguments:
              {a,b,c}     x help

            optional arguments:
              -h, --help  show this help message and exit
              -w W        w help
            '''))

    def test_subparser2_help(self):
        self._test_subparser_help('5.0 2 -h', textwrap.dedent('''\
            usage: PROG bar 2 [-h] [-y {1,2,3}] [z [z ...]]

            2 description

            positional arguments:
              z           z help

            optional arguments:
              -h, --help  show this help message and exit
              -y {1,2,3}  y help
            '''))

    def test_alias_invocation(self):
        parser = self._get_parser(aliases=True)
        self.assertEqual(
            parser.parse_known_args('0.5 1alias1 b'.split()),
            (NS(foo=False, bar=0.5, w=None, x='b'), []),
        )
        self.assertEqual(
            parser.parse_known_args('0.5 1alias2 b'.split()),
            (NS(foo=False, bar=0.5, w=None, x='b'), []),
        )

    def test_error_alias_invocation(self):
        parser = self._get_parser(aliases=True)
        self.assertArgumentParserError(parser.parse_args,
                                       '0.5 1alias3 b'.split())

    def test_alias_help(self):
        parser = self._get_parser(aliases=True, subparser_help=True)
        self.maxDiff = None
        self.assertEqual(parser.format_help(), textwrap.dedent("""\
            usage: PROG [-h] [--foo] bar COMMAND ...

            main description

            positional arguments:
              bar                   bar help

            optional arguments:
              -h, --help            show this help message and exit
              --foo                 foo help

            commands:
              COMMAND
                1 (1alias1, 1alias2)
                                    1 help
                2                   2 help
                3                   3 help
            """))


# ============
# Groups tests
# ============

class TestPositionalsGroups(TestCase):
    """Tests that order of group positionals matches construction order"""

    def test_nongroup_first(self):
        parser = ErrorRaisingArgumentParser()
        parser.add_argument('foo')
        group = parser.add_argument_group('g')
        group.add_argument('bar')
        parser.add_argument('baz')
        expected = NS(foo='1', bar='2', baz='3')
        result = parser.parse_args('1 2 3'.split())
        self.assertEqual(expected, result)

    def test_group_first(self):
        parser = ErrorRaisingArgumentParser()
        group = parser.add_argument_group('xxx')
        group.add_argument('foo')
        parser.add_argument('bar')
        parser.add_argument('baz')
        expected = NS(foo='1', bar='2', baz='3')
        result = parser.parse_args('1 2 3'.split())
        self.assertEqual(expected, result)

    def test_interleaved_groups(self):
        parser = ErrorRaisingArgumentParser()
        group = parser.add_argument_group('xxx')
        parser.add_argument('foo')
        group.add_argument('bar')
        parser.add_argument('baz')
        group = parser.add_argument_group('yyy')
        group.add_argument('frell')
        expected = NS(foo='1', bar='2', baz='3', frell='4')
        result = parser.parse_args('1 2 3 4'.split())
        self.assertEqual(expected, result)

# ===================
# Parent parser tests
# ===================

class TestParentParsers(TestCase):
    """Tests that parsers can be created with parent parsers"""

    def assertArgumentParserError(self, *args, **kwargs):
        self.assertRaises(ArgumentParserError, *args, **kwargs)

    def setUp(self):
        super().setUp()
        self.wxyz_parent = ErrorRaisingArgumentParser(add_help=False)
        self.wxyz_parent.add_argument('--w')
        x_group = self.wxyz_parent.add_argument_group('x')
        x_group.add_argument('-y')
        self.wxyz_parent.add_argument('z')

        self.abcd_parent = ErrorRaisingArgumentParser(add_help=False)
        self.abcd_parent.add_argument('a')
        self.abcd_parent.add_argument('-b')
        c_group = self.abcd_parent.add_argument_group('c')
        c_group.add_argument('--d')

        self.w_parent = ErrorRaisingArgumentParser(add_help=False)
        self.w_parent.add_argument('--w')

        self.z_parent = ErrorRaisingArgumentParser(add_help=False)
        self.z_parent.add_argument('z')

        # parents with mutually exclusive groups
        self.ab_mutex_parent = ErrorRaisingArgumentParser(add_help=False)
        group = self.ab_mutex_parent.add_mutually_exclusive_group()
        group.add_argument('-a', action='store_true')
        group.add_argument('-b', action='store_true')

        self.main_program = os.path.basename(sys.argv[0])

    def test_single_parent(self):
        parser = ErrorRaisingArgumentParser(parents=[self.wxyz_parent])
        self.assertEqual(parser.parse_args('-y 1 2 --w 3'.split()),
                         NS(w='3', y='1', z='2'))

    def test_single_parent_mutex(self):
        self._test_mutex_ab(self.ab_mutex_parent.parse_args)
        parser = ErrorRaisingArgumentParser(parents=[self.ab_mutex_parent])
        self._test_mutex_ab(parser.parse_args)

    def test_single_granparent_mutex(self):
        parents = [self.ab_mutex_parent]
        parser = ErrorRaisingArgumentParser(add_help=False, parents=parents)
        parser = ErrorRaisingArgumentParser(parents=[parser])
        self._test_mutex_ab(parser.parse_args)

    def _test_mutex_ab(self, parse_args):
        self.assertEqual(parse_args([]), NS(a=False, b=False))
        self.assertEqual(parse_args(['-a']), NS(a=True, b=False))
        self.assertEqual(parse_args(['-b']), NS(a=False, b=True))
        self.assertArgumentParserError(parse_args, ['-a', '-b'])
        self.assertArgumentParserError(parse_args, ['-b', '-a'])
        self.assertArgumentParserError(parse_args, ['-c'])
        self.assertArgumentParserError(parse_args, ['-a', '-c'])
        self.assertArgumentParserError(parse_args, ['-b', '-c'])

    def test_multiple_parents(self):
        parents = [self.abcd_parent, self.wxyz_parent]
        parser = ErrorRaisingArgumentParser(parents=parents)
        self.assertEqual(parser.parse_args('--d 1 --w 2 3 4'.split()),
                         NS(a='3', b=None, d='1', w='2', y=None, z='4'))

    def test_multiple_parents_mutex(self):
        parents = [self.ab_mutex_parent, self.wxyz_parent]
        parser = ErrorRaisingArgumentParser(parents=parents)
        self.assertEqual(parser.parse_args('-a --w 2 3'.split()),
                         NS(a=True, b=False, w='2', y=None, z='3'))
        self.assertArgumentParserError(
            parser.parse_args, '-a --w 2 3 -b'.split())
        self.assertArgumentParserError(
            parser.parse_args, '-a -b --w 2 3'.split())

    def test_conflicting_parents(self):
        self.assertRaises(
            argparse.ArgumentError,
            argparse.ArgumentParser,
            parents=[self.w_parent, self.wxyz_parent])

    def test_conflicting_parents_mutex(self):
        self.assertRaises(
            argparse.ArgumentError,
            argparse.ArgumentParser,
            parents=[self.abcd_parent, self.ab_mutex_parent])

    def test_same_argument_name_parents(self):
        parents = [self.wxyz_parent, self.z_parent]
        parser = ErrorRaisingArgumentParser(parents=parents)
        self.assertEqual(parser.parse_args('1 2'.split()),
                         NS(w=None, y=None, z='2'))

    def test_subparser_parents(self):
        parser = ErrorRaisingArgumentParser()
        subparsers = parser.add_subparsers()
        abcde_parser = subparsers.add_parser('bar', parents=[self.abcd_parent])
        abcde_parser.add_argument('e')
        self.assertEqual(parser.parse_args('bar -b 1 --d 2 3 4'.split()),
                         NS(a='3', b='1', d='2', e='4'))

    def test_subparser_parents_mutex(self):
        parser = ErrorRaisingArgumentParser()
        subparsers = parser.add_subparsers()
        parents = [self.ab_mutex_parent]
        abc_parser = subparsers.add_parser('foo', parents=parents)
        c_group = abc_parser.add_argument_group('c_group')
        c_group.add_argument('c')
        parents = [self.wxyz_parent, self.ab_mutex_parent]
        wxyzabe_parser = subparsers.add_parser('bar', parents=parents)
        wxyzabe_parser.add_argument('e')
        self.assertEqual(parser.parse_args('foo -a 4'.split()),
                         NS(a=True, b=False, c='4'))
        self.assertEqual(parser.parse_args('bar -b  --w 2 3 4'.split()),
                         NS(a=False, b=True, w='2', y=None, z='3', e='4'))
        self.assertArgumentParserError(
            parser.parse_args, 'foo -a -b 4'.split())
        self.assertArgumentParserError(
            parser.parse_args, 'bar -b -a 4'.split())

    def test_parent_help(self):
        parents = [self.abcd_parent, self.wxyz_parent]
        parser = ErrorRaisingArgumentParser(parents=parents)
        parser_help = parser.format_help()
        progname = self.main_program
        self.assertEqual(parser_help, textwrap.dedent('''\
            usage: {}{}[-h] [-b B] [--d D] [--w W] [-y Y] a z

            positional arguments:
              a
              z

            optional arguments:
              -h, --help  show this help message and exit
              -b B
              --w W

            c:
              --d D

            x:
              -y Y
        '''.format(progname, ' ' if progname else '' )))

    def test_groups_parents(self):
        parent = ErrorRaisingArgumentParser(add_help=False)
        g = parent.add_argument_group(title='g', description='gd')
        g.add_argument('-w')
        g.add_argument('-x')
        m = parent.add_mutually_exclusive_group()
        m.add_argument('-y')
        m.add_argument('-z')
        parser = ErrorRaisingArgumentParser(parents=[parent])

        self.assertRaises(ArgumentParserError, parser.parse_args,
            ['-y', 'Y', '-z', 'Z'])

        parser_help = parser.format_help()
        progname = self.main_program
        self.assertEqual(parser_help, textwrap.dedent('''\
            usage: {}{}[-h] [-w W] [-x X] [-y Y | -z Z]

            optional arguments:
              -h, --help  show this help message and exit
              -y Y
              -z Z

            g:
              gd

              -w W
              -x X
        '''.format(progname, ' ' if progname else '' )))


# ==============================
# Mutually exclusive group tests
# ==============================

class TestMutuallyExclusiveGroupErrors(TestCase):

    def test_invalid_add_argument_group(self):
        parser = ErrorRaisingArgumentParser()
        raises = self.assertRaises
        raises(TypeError, parser.add_mutually_exclusive_group, title='foo')

    def test_invalid_add_argument(self):
        parser = ErrorRaisingArgumentParser()
        group = parser.add_mutually_exclusive_group()
        add_argument = group.add_argument
        raises = self.assertRaises
        raises(ValueError, add_argument, '--foo', required=True)
        raises(ValueError, add_argument, 'bar')
        raises(ValueError, add_argument, 'bar', nargs='+')
        raises(ValueError, add_argument, 'bar', nargs=1)
        raises(ValueError, add_argument, 'bar', nargs=argparse.PARSER)

    def test_help(self):
        parser = ErrorRaisingArgumentParser(prog='PROG')
        group1 = parser.add_mutually_exclusive_group()
        group1.add_argument('--foo', action='store_true')
        group1.add_argument('--bar', action='store_false')
        group2 = parser.add_mutually_exclusive_group()
        group2.add_argument('--soup', action='store_true')
        group2.add_argument('--nuts', action='store_false')
        expected = '''\
            usage: PROG [-h] [--foo | --bar] [--soup | --nuts]

            optional arguments:
              -h, --help  show this help message and exit
              --foo
              --bar
              --soup
              --nuts
              '''
        self.assertEqual(parser.format_help(), textwrap.dedent(expected))

class MEMixin(object):

    def test_failures_when_not_required(self):
        parse_args = self.get_parser(required=False).parse_args
        error = ArgumentParserError
        for args_string in self.failures:
            self.assertRaises(error, parse_args, args_string.split())

    def test_failures_when_required(self):
        parse_args = self.get_parser(required=True).parse_args
        error = ArgumentParserError
        for args_string in self.failures + ['']:
            self.assertRaises(error, parse_args, args_string.split())

    def test_successes_when_not_required(self):
        parse_args = self.get_parser(required=False).parse_args
        successes = self.successes + self.successes_when_not_required
        for args_string, expected_ns in successes:
            actual_ns = parse_args(args_string.split())
            self.assertEqual(actual_ns, expected_ns)

    def test_successes_when_required(self):
        parse_args = self.get_parser(required=True).parse_args
        for args_string, expected_ns in self.successes:
            actual_ns = parse_args(args_string.split())
            self.assertEqual(actual_ns, expected_ns)

    def test_usage_when_not_required(self):
        format_usage = self.get_parser(required=False).format_usage
        expected_usage = self.usage_when_not_required
        self.assertEqual(format_usage(), textwrap.dedent(expected_usage))

    def test_usage_when_required(self):
        format_usage = self.get_parser(required=True).format_usage
        expected_usage = self.usage_when_required
        self.assertEqual(format_usage(), textwrap.dedent(expected_usage))

    def test_help_when_not_required(self):
        format_help = self.get_parser(required=False).format_help
        help = self.usage_when_not_required + self.help
        self.assertEqual(format_help(), textwrap.dedent(help))

    def test_help_when_required(self):
        format_help = self.get_parser(required=True).format_help
        help = self.usage_when_required + self.help
        self.assertEqual(format_help(), textwrap.dedent(help))


class TestMutuallyExclusiveSimple(MEMixin, TestCase):

    def get_parser(self, required=None):
        parser = ErrorRaisingArgumentParser(prog='PROG')
        group = parser.add_mutually_exclusive_group(required=required)
        group.add_argument('--bar', help='bar help')
        group.add_argument('--baz', nargs='?', const='Z', help='baz help')
        return parser

    failures = ['--bar X --baz Y', '--bar X --baz']
    successes = [
        ('--bar X', NS(bar='X', baz=None)),
        ('--bar X --bar Z', NS(bar='Z', baz=None)),
        ('--baz Y', NS(bar=None, baz='Y')),
        ('--baz', NS(bar=None, baz='Z')),
    ]
    successes_when_not_required = [
        ('', NS(bar=None, baz=None)),
    ]

    usage_when_not_required = '''\
        usage: PROG [-h] [--bar BAR | --baz [BAZ]]
        '''
    usage_when_required = '''\
        usage: PROG [-h] (--bar BAR | --baz [BAZ])
        '''
    help = '''\

        optional arguments:
          -h, --help   show this help message and exit
          --bar BAR    bar help
          --baz [BAZ]  baz help
        '''


class TestMutuallyExclusiveLong(MEMixin, TestCase):

    def get_parser(self, required=None):
        parser = ErrorRaisingArgumentParser(prog='PROG')
        parser.add_argument('--abcde', help='abcde help')
        parser.add_argument('--fghij', help='fghij help')
        group = parser.add_mutually_exclusive_group(required=required)
        group.add_argument('--klmno', help='klmno help')
        group.add_argument('--pqrst', help='pqrst help')
        return parser

    failures = ['--klmno X --pqrst Y']
    successes = [
        ('--klmno X', NS(abcde=None, fghij=None, klmno='X', pqrst=None)),
        ('--abcde Y --klmno X',
            NS(abcde='Y', fghij=None, klmno='X', pqrst=None)),
        ('--pqrst X', NS(abcde=None, fghij=None, klmno=None, pqrst='X')),
        ('--pqrst X --fghij Y',
            NS(abcde=None, fghij='Y', klmno=None, pqrst='X')),
    ]
    successes_when_not_required = [
        ('', NS(abcde=None, fghij=None, klmno=None, pqrst=None)),
    ]

    usage_when_not_required = '''\
    usage: PROG [-h] [--abcde ABCDE] [--fghij FGHIJ]
                [--klmno KLMNO | --pqrst PQRST]
    '''
    usage_when_required = '''\
    usage: PROG [-h] [--abcde ABCDE] [--fghij FGHIJ]
                (--klmno KLMNO | --pqrst PQRST)
    '''
    help = '''\

    optional arguments:
      -h, --help     show this help message and exit
      --abcde ABCDE  abcde help
      --fghij FGHIJ  fghij help
      --klmno KLMNO  klmno help
      --pqrst PQRST  pqrst help
    '''


class TestMutuallyExclusiveFirstSuppressed(MEMixin, TestCase):

    def get_parser(self, required):
        parser = ErrorRaisingArgumentParser(prog='PROG')
        group = parser.add_mutually_exclusive_group(required=required)
        group.add_argument('-x', help=argparse.SUPPRESS)
        group.add_argument('-y', action='store_false', help='y help')
        return parser

    failures = ['-x X -y']
    successes = [
        ('-x X', NS(x='X', y=True)),
        ('-x X -x Y', NS(x='Y', y=True)),
        ('-y', NS(x=None, y=False)),
    ]
    successes_when_not_required = [
        ('', NS(x=None, y=True)),
    ]

    usage_when_not_required = '''\
        usage: PROG [-h] [-y]
        '''
    usage_when_required = '''\
        usage: PROG [-h] -y
        '''
    help = '''\

        optional arguments:
          -h, --help  show this help message and exit
          -y          y help
        '''


class TestMutuallyExclusiveManySuppressed(MEMixin, TestCase):

    def get_parser(self, required):
        parser = ErrorRaisingArgumentParser(prog='PROG')
        group = parser.add_mutually_exclusive_group(required=required)
        add = group.add_argument
        add('--spam', action='store_true', help=argparse.SUPPRESS)
        add('--badger', action='store_false', help=argparse.SUPPRESS)
        add('--bladder', help=argparse.SUPPRESS)
        return parser

    failures = [
        '--spam --badger',
        '--badger --bladder B',
        '--bladder B --spam',
    ]
    successes = [
        ('--spam', NS(spam=True, badger=True, bladder=None)),
        ('--badger', NS(spam=False, badger=False, bladder=None)),
        ('--bladder B', NS(spam=False, badger=True, bladder='B')),
        ('--spam --spam', NS(spam=True, badger=True, bladder=None)),
    ]
    successes_when_not_required = [
        ('', NS(spam=False, badger=True, bladder=None)),
    ]

    usage_when_required = usage_when_not_required = '''\
        usage: PROG [-h]
        '''
    help = '''\

        optional arguments:
          -h, --help  show this help message and exit
        '''


class TestMutuallyExclusiveOptionalAndPositional(MEMixin, TestCase):

    def get_parser(self, required):
        parser = ErrorRaisingArgumentParser(prog='PROG')
        group = parser.add_mutually_exclusive_group(required=required)
        group.add_argument('--foo', action='store_true', help='FOO')
        group.add_argument('--spam', help='SPAM')
        group.add_argument('badger', nargs='*', default='X', help='BADGER')
        return parser

    failures = [
        '--foo --spam S',
        '--spam S X',
        'X --foo',
        'X Y Z --spam S',
        '--foo X Y',
    ]
    successes = [
        ('--foo', NS(foo=True, spam=None, badger='X')),
        ('--spam S', NS(foo=False, spam='S', badger='X')),
        ('X', NS(foo=False, spam=None, badger=['X'])),
        ('X Y Z', NS(foo=False, spam=None, badger=['X', 'Y', 'Z'])),
    ]
    successes_when_not_required = [
        ('', NS(foo=False, spam=None, badger='X')),
    ]

    usage_when_not_required = '''\
        usage: PROG [-h] [--foo | --spam SPAM | badger [badger ...]]
        '''
    usage_when_required = '''\
        usage: PROG [-h] (--foo | --spam SPAM | badger [badger ...])
        '''
    help = '''\

        positional arguments:
          badger       BADGER

        optional arguments:
          -h, --help   show this help message and exit
          --foo        FOO
          --spam SPAM  SPAM
        '''


class TestMutuallyExclusiveOptionalsMixed(MEMixin, TestCase):

    def get_parser(self, required):
        parser = ErrorRaisingArgumentParser(prog='PROG')
        parser.add_argument('-x', action='store_true', help='x help')
        group = parser.add_mutually_exclusive_group(required=required)
        group.add_argument('-a', action='store_true', help='a help')
        group.add_argument('-b', action='store_true', help='b help')
        parser.add_argument('-y', action='store_true', help='y help')
        group.add_argument('-c', action='store_true', help='c help')
        return parser

    failures = ['-a -b', '-b -c', '-a -c', '-a -b -c']
    successes = [
        ('-a', NS(a=True, b=False, c=False, x=False, y=False)),
        ('-b', NS(a=False, b=True, c=False, x=False, y=False)),
        ('-c', NS(a=False, b=False, c=True, x=False, y=False)),
        ('-a -x', NS(a=True, b=False, c=False, x=True, y=False)),
        ('-y -b', NS(a=False, b=True, c=False, x=False, y=True)),
        ('-x -y -c', NS(a=False, b=False, c=True, x=True, y=True)),
    ]
    successes_when_not_required = [
        ('', NS(a=False, b=False, c=False, x=False, y=False)),
        ('-x', NS(a=False, b=False, c=False, x=True, y=False)),
        ('-y', NS(a=False, b=False, c=False, x=False, y=True)),
    ]

    usage_when_required = usage_when_not_required = '''\
        usage: PROG [-h] [-x] [-a] [-b] [-y] [-c]
        '''
    help = '''\

        optional arguments:
          -h, --help  show this help message and exit
          -x          x help
          -a          a help
          -b          b help
          -y          y help
          -c          c help
        '''


class TestMutuallyExclusiveInGroup(MEMixin, TestCase):

    def get_parser(self, required=None):
        parser = ErrorRaisingArgumentParser(prog='PROG')
        titled_group = parser.add_argument_group(
            title='Titled group', description='Group description')
        mutex_group = \
            titled_group.add_mutually_exclusive_group(required=required)
        mutex_group.add_argument('--bar', help='bar help')
        mutex_group.add_argument('--baz', help='baz help')
        return parser

    failures = ['--bar X --baz Y', '--baz X --bar Y']
    successes = [
        ('--bar X', NS(bar='X', baz=None)),
        ('--baz Y', NS(bar=None, baz='Y')),
    ]
    successes_when_not_required = [
        ('', NS(bar=None, baz=None)),
    ]

    usage_when_not_required = '''\
        usage: PROG [-h] [--bar BAR | --baz BAZ]
        '''
    usage_when_required = '''\
        usage: PROG [-h] (--bar BAR | --baz BAZ)
        '''
    help = '''\

        optional arguments:
          -h, --help  show this help message and exit

        Titled group:
          Group description

          --bar BAR   bar help
          --baz BAZ   baz help
        '''


class TestMutuallyExclusiveOptionalsAndPositionalsMixed(MEMixin, TestCase):

    def get_parser(self, required):
        parser = ErrorRaisingArgumentParser(prog='PROG')
        parser.add_argument('x', help='x help')
        parser.add_argument('-y', action='store_true', help='y help')
        group = parser.add_mutually_exclusive_group(required=required)
        group.add_argument('a', nargs='?', help='a help')
        group.add_argument('-b', action='store_true', help='b help')
        group.add_argument('-c', action='store_true', help='c help')
        return parser

    failures = ['X A -b', '-b -c', '-c X A']
    successes = [
        ('X A', NS(a='A', b=False, c=False, x='X', y=False)),
        ('X -b', NS(a=None, b=True, c=False, x='X', y=False)),
        ('X -c', NS(a=None, b=False, c=True, x='X', y=False)),
        ('X A -y', NS(a='A', b=False, c=False, x='X', y=True)),
        ('X -y -b', NS(a=None, b=True, c=False, x='X', y=True)),
    ]
    successes_when_not_required = [
        ('X', NS(a=None, b=False, c=False, x='X', y=False)),
        ('X -y', NS(a=None, b=False, c=False, x='X', y=True)),
    ]

    usage_when_required = usage_when_not_required = '''\
        usage: PROG [-h] [-y] [-b] [-c] x [a]
        '''
    help = '''\

        positional arguments:
          x           x help
          a           a help

        optional arguments:
          -h, --help  show this help message and exit
          -y          y help
          -b          b help
          -c          c help
        '''


for x in pending_tests:
    x()
