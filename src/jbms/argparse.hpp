#ifndef HEADER_GUARD_35f42b765c27537810b765eedc22958e
#define HEADER_GUARD_35f42b765c27537810b765eedc22958e

#include <string>
#include <experimental/string_view>
#include <experimental/optional>
#include <type_traits>

#include <memory>

#ifdef USE_STD_EXPERIMENTAL_ANY
#include <experimental/any>
#else
#include <boost/any.hpp>
#endif

#include <boost/lexical_cast.hpp>

#include <vector>
#include <unordered_map>
#include <functional>

#include <iosfwd>

namespace jbms {
namespace argparse {

using std::experimental::optional;
using std::experimental::nullopt;
using std::string;
using std::experimental::string_view;
using std::vector;

#ifdef USE_STD_EXPERIMENTAL_ANY
using std::experimental::any;
using std::experimental::any_cast;
#else
using boost::any;
using boost::any_cast;
#endif

using std::shared_ptr;

/**
 * \brief Specifies a constraint on the arguments that may be consumed by an optional or positional rule
 **/
enum NargsValue : int {
  /**
   * \brief Matches zero or one argument.
   **/
  OPTIONAL = -1,
    /**
     * \brief Matches any number of arguments, including zero.
     **/
  ZERO_OR_MORE = -2,
    /**
     * \brief Matches one or more argument.
     **/
  ONE_OR_MORE = -3,
    /**
     * \brief Matches a subcommand invocation
     * All subsequent arguments are consumed, and at least one argument is required.  The first argument must look like an argument rather than the option.  Literal ignore-options arguments (i.e. "--") are processed for the first argument (for positional rules only) but are not given special treatment if they occur after the first position.
     **/
  PARSER = -4,
    /**
     * \brief Matches all remaining arguments
     * All subsequent arguments are matched, irrespective of whether they look like normal arguments or syntactically appear to be options.  Literal ignore-options arguments (i.e. "--") are not given special treatment.
     **/
  REMAINDER = -5,

#ifndef DOXYGEN
  NARGS_INVALID = -6,
#endif
};

/**
 * \brief Specifies a constraint on argument matching.
 * \details Constraints may be specified either symbolically using a \ref NargsValue constant, or as an exact number of arguments that must be specified.
 *
 **/
struct Nargs {
  NargsValue value;
  operator NargsValue () const { return value; }

  /**
   * \brief Specify a symbolic constraint using a \ref NargsValue constant
   **/
  Nargs(NargsValue value) : value(value) {}

  /**
   * \brief Specify that exactly \p count arguments must be matched
   *
   * For optional (rather than positional) rules, \p count may be zero.
   **/
  Nargs(int count) : value(NargsValue(count)) {
    if (count < 0)
      throw std::invalid_argument("Explicit number of arguments must be positive");
  }
};


/**
 * Returns a representation of a string in string literal syntax
 **/
string repr(string_view s);

/**
 * \class jbms::argparse::ArgumentParser
 **/
class ArgumentParser;
/**
 * \class jbms::argparse::ArgumentGroup
 **/
class ArgumentGroup;
/**
 * \class jbms::argparse::MutuallyExclusiveGroup
 **/
class MutuallyExclusiveGroup;

/**
 * \class jbms::argparse::Subparsers
 **/
class Subparsers;

/**
 * \class jbms::argparse::ArgumentContainer
**/
class ArgumentContainer;

/**
 * \brief Opaque type representing a generic argument parsing rule
 * \ingroup ActionDefinition
 * \class jbms::argparse::GenericArgument
 **/
class GenericArgument;

template <class Value>
class Argument;

/**
 * \brief Stores the command-line parsing result
 * This is map from the key to the dynamically-typed value.
 **/
class Result : public std::unordered_map<string,any> {
public:

  using std::unordered_map<string,any>::unordered_map;

  /**
   * \brief Retrieves the result with the specified key \p name and type \p T
   * \tparam T The type of the stored result
   **/
  template <class T>
  T const *get(std::string const &name) const {
    auto it = find(name);
    if (it == end() || it->second.empty())
      return nullptr;
    return &any_cast<T const &>(it->second);
  }

  using std::unordered_map<string,any>::operator[];

  template <class Value>
  Value const *operator[](Argument<Value> const &a) const {
    return get<Value>(a.key());
  }
};

/**
 * \brief Convenience interface for specifying an optional string value
 **/
class OptionalString : public optional<string> {
public:
  using optional<string>::optional;
  OptionalString() = default;
  OptionalString(string_view s)
    : optional<string>(string(s))
  {}
  OptionalString(const char *s)
    : optional<string>(string(s))
  {}
};


/**
 * \brief Specifies a list of strings.
 *
 * This can be implicitly constructed from either a single string or a list of strings.
 **/
class OptionsSpec : public vector<string> {
public:
  OptionsSpec() = default;
  OptionsSpec(const char *x) : vector<string>{ std::string(x) } {}
  OptionsSpec(std::string x) : vector<string>{ std::move(x) } {}
  OptionsSpec(string_view x) : vector<string>{ std::string(x) } {}
  OptionsSpec(std::initializer_list<std::string> x) : vector<string>(x) {}
  OptionsSpec(std::vector<std::string> x) : vector<string>(x) {}
  OptionsSpec(OptionalString x) {
    if (x)
      push_back(*x);
  }
};

/**
 * \defgroup ActionDefinition Defining custom processing actions
 * \brief Interface for defining custom processing behavior.
 * \details The methods of \ref ArgumentContainer provide access to the builtin set of argument processing actions, which are defined in \ref jbms::argparse::action.  These processing actions receive the list of parsed argument strings as input (consistent with the \ref Nargs value), and are responsible for performing an appropriate action and (optionally) updating the \ref Result map.
 * @{
 **/


/**
 * \param help If equal to nullopt, then the argument is suppressed from the help and usage messages.  In contrast, an empty string will result in the argument being displayed without a help message.
 * \relatesalso GenericArgument
 **/
void set_help(GenericArgument &arg, OptionalString help);

/**
 * \relatesalso GenericArgument
 **/
void set_nargs(GenericArgument &arg, Nargs nargs);
/**
 * \relatesalso GenericArgument
 **/
void set_required(GenericArgument &arg, bool value);
/**
 * \relatesalso GenericArgument
 **/
void set_default_value(GenericArgument &arg, any value);

/**
 * Handlers receive the entire result map as a parameter for greater generality.
 * In most cases a handler will only update result[key].  Subparsers will update multiple fields, however.
 * \relatesalso GenericArgument
 **/
using Handler = std::function<void (ArgumentParser const &parser,
                                    Result &result,
                                    string const &key,
                                    vector<string_view> values)>;

/**
 * \relatesalso GenericArgument
 **/
using CopyHandler = std::function<Handler (GenericArgument &arg, Handler const &handler)>;

/**
 * Specifies the handler, along with a function to copy it if the argument is cloned (rather than just using the copy constructor).
 *
 * This is useful if the handler contains a reference to the underlying ArgumentParser.
 * If no copy_handler is specified, the handler will just be copy-constructed
 * \relatesalso GenericArgument
 **/
void set_handler(GenericArgument &arg, Handler handler, CopyHandler copy_handler = {});

/**
 * \relatesalso GenericArgument
 **/
Handler &get_handler(GenericArgument &arg);

/**
 * \relatesalso GenericArgument
 **/
void set_metavar(GenericArgument &arg, OptionsSpec value);

/**
 * \brief Specifies a logical sub-command to be shown in the help message.
 * \relatesalso GenericArgument
**/
void add_choice_help(GenericArgument &arg, string name, string invocation, OptionalString help = {});

/**
 * \relatesalso GenericArgument
 **/
string const &get_key(GenericArgument &arg);

/**
 * \relatesalso GenericArgument
 **/
ArgumentParser get_parser(GenericArgument &arg);
/**
 * \relatesalso GenericArgument
 **/
ArgumentGroup get_group(GenericArgument &arg);

/**@}*/


/**
 * \brief Interface for accessing properties of a positional/optional parsing rule
 * \tparam Value Specifies the result type, or \p void if the parsing rule does not produce a result
 **/
template <class Value = void>
class Argument {
  shared_ptr<GenericArgument> generic;
public:
  Argument(shared_ptr<GenericArgument> generic) : generic(generic)
  {}

  /**
   * \returns The key for accessing the result
   **/
  string const &key() const{ return get_key(*generic); }

  /**
   * \brief Specify the help text for this optional/positional rule
   **/
  Argument help(std::string help) const {
    set_help(*generic, std::move(help));
    return *this;
  }

  /**
   * \brief Hide this rule from the help and usage messages
   **/
  Argument hide() const {
    set_help(*generic, {});
    return *this;
  }

  /**
   * \brief Specify the default result to use if the rule does not match
   * For more complicated actions such as \p action::append or \p action::count, this specifies the initial value and can affect the result even if the rule does match.
   **/
#ifndef DOXYGEN
  template <class ValueT = Value>
#endif
  Argument default_value(
#ifdef DOXYGEN
                         Value value
#else
                         std::common_type_t<ValueT> value
#endif
                         ) const {
    set_default_value(*generic, std::move(value));
    return *this;
  }

  /**
   * \brief Specifies that the option must be specified
   * \pre This is only valid for optional (rather than positional) rules.  Positional rules are always required to "match", though they may be permitted to match zero arguments depending on how they are defined.
   **/
  Argument required(bool value) const {
    set_required(*generic, value);
    return *this;
  }

  /**
   * \brief Specifies the placeholder string for the argument in help and usage messages
   * Either a single string or multiple strings may be specified.
   **/
  Argument metavar(OptionsSpec value) const {
    set_metavar(*generic, std::move(value));
    return *this;
  }
};

namespace detail {

template <class T>
struct remove_optional {
  using type = T;
};

template <class T>
struct remove_optional<optional<T>> {
  using type = T;
};

template <class T>
using remove_optional_t = typename remove_optional<T>::type;

[[noreturn]] void handle_type_conversion_failure(const char *name, string_view s);

template <class T>
T lexical_cast(string_view s) {
  try {
    return boost::lexical_cast<T>(s);
  } catch (boost::bad_lexical_cast &) {
    handle_type_conversion_failure(typeid(T).name(), s);
  }
}

}

/**
 * \defgroup Conversion Type conversion
 * \brief Specifying type conversion for argument parsing
 *
 * Actions that take a single argument depend on a \ref SingleConverter that converts a single string to a result of some type \p T.  Actions that take multiple arguments depend on a \ref MultiConverter, that converts a list of strings to a result of some type \p T.
 *
 * A \ref SingleConverter with result type \p T can be converted automatically to a \ref MultiConverter that simply converts each argument independently (via \ref convert_all_arguments).
 * @{
 **/

/**
 * \brief Converts a string to an arbitrary type using Boost.LexicalCast
 *
 * This serves as the default converter if one is not specified.
 **/
template <class T>
T default_single_converter(string_view s) {
  return detail::lexical_cast<detail::remove_optional_t<T>>(s);
}

/**
 * \brief Applies \ref default_single_converter to each vector element
 **/
template <class T>
vector<T> default_multi_converter(vector<string_view> const &s) {
  vector<T> result;
  for (auto x : s)
    result.push_back(default_single_converter<T>(x));
  return result;
}

/**
 * \brief Converts a single string to a value
 * \tparam T Result value type
 **/
template <class T>
class SingleConverter {
public:
  using value_type = T;
  using Convert = std::function<T (string_view s)>;
  Convert convert;
  OptionalString metavar;

  /**
   * Default converter
   **/
  SingleConverter() : convert(&default_single_converter<T>) {}

  /**
   * Construct from a conversion function
   **/
  template <class ConvertT, std::enable_if_t<std::is_constructible<Convert, ConvertT &&>::value> * = nullptr>
  SingleConverter(ConvertT &&convert, OptionalString metavar = {})
      : convert(std::forward<ConvertT>(convert)), metavar(std::move(metavar)) {}

  /**
   * Construct from a list of choices
   **/
  SingleConverter(vector<T> choices,
                  SingleConverter<T> convert = {}) {
    std::ostringstream ostr;
    ostr << "{";
    bool is_first = true;
    for (auto const &x : choices) {
      if (!is_first)
        ostr << ",";
      is_first = false;
      ostr << x;
    }
    ostr << "}";
    this->metavar = ostr.str();

    this->convert = [choices = std::move(choices), convert = std::move(convert)](string_view s) {
      auto value = convert.convert(s);
      for (auto const &x : choices) {
        if (x == value)
          return value;
      }
      std::ostringstream ostr;
      ostr << "invalid choice: " << repr(s) << " (choose from ";
      bool is_first = true;
      for (auto const &x : choices) {
        if (!is_first)
          ostr << ", ";
        is_first = false;
        ostr << x;
      }
      ostr << ")";
      throw std::invalid_argument(ostr.str());
    };
  }

  /**
   * Construct from a list of choices (specified as initializer_list).
   *
   * This overload is needed so that an argument of type SingleConstructor can be implicitly constructed from an initializer list.
   **/
  SingleConverter(std::initializer_list<T> choices,
                  SingleConverter<T> convert = {})
    : SingleConverter(vector<T>(choices.begin(), choices.end()), std::move(convert))
  {}
};


/**
 * \brief Maps a single-argument conversion function to a multi-argument conversion function
 *
 * The returned function simply maps each argument using \p conv.
 * \returns A function that converts \p vector<string_view> to \p T
 * \param conv A function from \p string_view to \p T
 **/
template <class F>
auto convert_all_arguments(F conv) {
  using T = std::decay_t<decltype(conv(std::declval<string_view>()))>;
  return [conv = std::move(conv)](vector<string_view> const & s) -> vector<T> {
    vector<T> result;
    for (auto x : s)
      result.push_back(conv(x));
    return result;
  };
}

/**
 * \brief Generic MultiConverter for non-vector result types
 *
 * There is no default conversion function for this case.
 **/
template <class T>
class MultiConverter {
public:
  using value_type = T;
  using Convert = std::function<value_type (vector<string_view> const &)>;
  Convert convert;
  OptionsSpec metavar;

  /**
   * Construct from a conversion function
   **/
  template <class ConvertT, std::enable_if_t<std::is_constructible<Convert, ConvertT &&>::value> * = nullptr>
  MultiConverter(ConvertT &&convert,
                 OptionsSpec metavar = {})
    : convert(std::forward<ConvertT>(convert)), metavar(std::move(metavar))
  {}
};

/**
 * \brief MultiConverter specialization for vector<Element> case
 *
 * We can automatically generate a conversion using a SingleConverter<Element>, by just converting each element.
 **/
template <class Element>
class MultiConverter<vector<Element>> {
public:
  using value_type = vector<Element>;
  using element_type = Element;
  using Convert = std::function<value_type (vector<string_view> const &)>;
  using SingleConvert = std::function<element_type (string_view)>;
  Convert convert;
  OptionsSpec metavar;

  /**
   * \brief Default converter using boost::lexical_cast for each element
   **/
  MultiConverter() : convert(&default_multi_converter<element_type>) {}

  /**
   * Construct from a conversion function
   **/
  template <class ConvertT, std::enable_if_t<std::is_constructible<Convert, ConvertT &&>::value> * = nullptr>
  MultiConverter(ConvertT &&convert,
                 OptionsSpec metavar = {})
    : convert(std::forward<ConvertT>(convert)), metavar(std::move(metavar))
  {}

  /**
   * Construct from a SingleConverter
   **/
  MultiConverter(SingleConverter<element_type> convert)
    : convert(convert_all_arguments(std::move(convert.convert))),
      metavar(std::move(convert.metavar))
  {}

  /**
   * Construct from a single conversion function
   **/
  template <class ConvertT, std::enable_if_t<std::is_constructible<SingleConvert, ConvertT &&>::value> * = nullptr>
  MultiConverter(ConvertT &&convert,
                 OptionalString metavar = {})
    : convert(convert_all_arguments(std::forward<ConvertT>(convert))), metavar(std::move(metavar))
  {}

  /**
   * Construct from element choices
   **/
  MultiConverter(vector<element_type> choices,
                 SingleConverter<element_type> convert = {})
    : MultiConverter(SingleConverter<element_type>(std::move(choices), std::move(convert)))
  {}

  /**
   * Construct from element choices (initializer list)
   **/
  MultiConverter(std::initializer_list<element_type> choices,
                 SingleConverter<element_type> convert = {})
    : MultiConverter(SingleConverter<element_type>(choices, std::move(convert)))
  {}

};

/**@}*/

namespace action {

template <class T = string>
Argument<T> store_single(shared_ptr<GenericArgument> generic, SingleConverter<T> convert = {}) {
  set_nargs(*generic, 1);
  set_metavar(*generic, std::move(convert.metavar));
  set_handler(*generic,
              [conv = std::move(convert.convert)](ArgumentParser const &, Result &result, string const &key, vector<string_view> const & values) {
    assert(values.size() == 1);
    result[key] = conv(values.at(0));
  });
  return Argument<T>(std::move(generic));
}


template <class Value = optional<string>>
Argument<Value> store_optional(shared_ptr<GenericArgument> generic, Value missing_value = {}, SingleConverter<Value> convert = {}) {
  set_nargs(*generic, OPTIONAL);
  set_metavar(*generic, std::move(convert.metavar));
  set_handler(
      *generic,
      [ conv = std::move(convert.convert), missing_value = std::move(missing_value) ](ArgumentParser const &,
                                                                                      Result &result,
                                                                                      string const &key,
                                                                                      vector<string_view> const & values) {
        auto &a = result[key];
        if (values.empty())
          a = missing_value;
        else {
          assert(values.size() == 1);
          a = conv(values.at(0));
        }
      });
  return Argument<Value>(std::move(generic));
}

template <class T = vector<string>>
Argument<T> store_multi(shared_ptr<GenericArgument> generic, Nargs nargs, MultiConverter<T> convert = {}) {
  set_nargs(*generic, nargs);
  set_metavar(*generic, std::move(convert.metavar));
  set_handler(*generic,
              [conv = std::move(convert.convert)](
                  ArgumentParser const &, Result & result, string const & key, vector<string_view> const & values) {
    result[key] = conv(values);
  });
  return Argument<T>(std::move(generic));
}

template <class T = bool>
auto store_flag(shared_ptr<GenericArgument> generic, T value) {
  set_nargs(*generic, 0);
  set_handler(*generic,
              [value = std::move(value)](
                  ArgumentParser const &, Result & result, string const & key, vector<string_view> const & values) {
    assert(values.size() == 0);
    (void)values;
    result[key] = value;
  });
  return Argument<T>(std::move(generic));
}

template <class T = string>
Argument<vector<T>> append(shared_ptr<GenericArgument> generic, SingleConverter<T> convert = {}) {
  using Value = vector<T>;

  set_nargs(*generic, 1);
  set_default_value(*generic, Value());
  set_metavar(*generic, std::move(convert.metavar));
  set_handler(*generic,
              [conv = std::move(convert.convert)](
                  ArgumentParser const &, Result & result, string const & key, vector<string_view> const & values) {
    assert(values.size() == 1);
    any_cast<Value &>(result[key]).emplace_back(conv(values.at(0)));
  });
  return Argument<Value>(std::move(generic));
}

template <class T>
Argument<vector<T>> append_flag(shared_ptr<GenericArgument> generic, T value) {
  using Value = std::vector<T>;

  set_nargs(*generic, 0);
  set_default_value(*generic, Value());
  set_handler(*generic, [value = std::move(value)](ArgumentParser const &, Result &result, string const &key, vector<string_view> const & values) {
    assert(values.size() == 0);
    (void)values;
    any_cast<Value &>(result[key]).emplace_back(value);
  });
  return Argument<Value>(std::move(generic));
}

template <class Value = size_t>
Argument<Value> count(shared_ptr<GenericArgument> generic) {
  set_nargs(*generic, 0);
  set_default_value(*generic, Value());
  set_handler(*generic, [](ArgumentParser const &, Result &result, string const &key, vector<string_view> const &values) {
    assert(values.size() == 0);
    (void)values;
    any_cast<Value &>(result[key]) += Value(1);
  });
  return Argument<Value>(std::move(generic));
}

}

/**
 * \brief Specifies formatting parameters for help text
 **/
struct HelpFormatterParameters {
  /**
   * \brief Use default values for all parameters.
   **/
  HelpFormatterParameters() = default;

  /**
   * \brief Specify just the line length, using default values of all other parameters.
   **/
  HelpFormatterParameters(int width) : width(width) {}

  /**
   * \brief Program name for usage information.
   *
   * If empty, defaults to the program name set on the \ref ArgumentParser.
   **/
  string prog;

  /**
   * \brief Amount to indent help sections
   **/
  int indent_increment = 2;

  /**
   * \brief Maximum amount to indent help text
   * If action invocations are longer than this amount, the invocation will be listed on a separate line.
   **/
  int max_help_position = 24;

  /**
   * \brief Maximum line length for word wrapping
   *
   * If not specified (i.e. set to 0), and STDIN is a terminal, defaults to the terminal width.  Otherwise, defaults to 80.
   **/
  int width = 0;

  /**
   * \brief Lower bound on the line length for word wrapping help text
   * This lower bound may apply if there is a large amount of indentation and/or \ref width is small.
   **/
  int min_text_width = 11;
};

/**
 * \defgroup Subparser Subparser support
 * \brief Subparsers are useful for implementing git/svn-style sub-command interfaces
 *
 * \see ArgumentContainer::add_subparsers
 * @{
 **/

/**
 * \brief Dispatch to a set of secondary parsers based on a command name
 **/
class Subparsers {
public:

  Subparsers() = default;

  /**
   * \brief Add a mapping from one or more command names to a subparser
   *
   * \returns The new ArgumentParser for defining the parsing rules for the arguments to the sub-command
   *
   * \param[in] names One or more command names (at least one must be specified)
   * \param[in] short_desc  Short description text to show in help message.  The parser will not be listed if the default value of \p nullopt is specified.
   **/
  ArgumentParser add_parser(OptionsSpec names, OptionalString short_desc = {}) const;

  /**
   * \brief Specify the help text for this subparser dispatch rule
   *
   * \note Help for individual subparsers/commands is specified as an argument to \ref add_parser.
   **/
  Subparsers help(string help) const;

  /**
   * \brief Hide this parsing rule from the usage and help messages
   **/
  Subparsers hide() const;

  /**
   * \brief Specify the placeholder for the command name in the help message
   **/
  Subparsers metavar(OptionsSpec value) const;

private:
  shared_ptr<GenericArgument> generic;
  Subparsers(shared_ptr<GenericArgument> generic) : generic(std::move(generic)) {}
  friend class ArgumentContainer;
};

/**@}*/


/**
 * \defgroup RuleDefinition Defining parsing rules
 * @{
 **/

/**
 * \brief Interface for defining argument parsing rules
 **/
class ArgumentContainer {
public:
  virtual shared_ptr<GenericArgument> add_generic(OptionsSpec spec) const = 0;

  /**
   * \brief Returns the parser that this container is a part of.
   **/
  virtual ArgumentParser get_parser() const = 0;

  /**
   * \brief Define a rule that matches exactly one argument
   *
   * The result is equal to the value of the matched argument.
   *
   * \note If the rule matches multiple times (which can happen only if it is an optional, rather than positional, rule), the result is determined only from the last match.
   *
   * \param conv The converter to use and/or set of valid choices
   * \tparam Value The result type, which defaults to \p string
   **/
  template <class Value = string>
  Argument<Value> add(OptionsSpec spec, SingleConverter<Value> conv = {}) const {
    return action::store_single<Value>(add_generic(std::move(spec)), std::move(conv));
  }

  /**
   * \brief Define a rule that allows an optional argument.
   *
   * Either zero or one argument is consumed.
   * The result is equal to the value of the matched argument if there is one, or the specified \p missing value otherwise.
   * \note If an optional rather than positional rule is specified, the \p missing value is used only if the option itself is specified, but there is no non-option argument following it.  Otherwise the result will equal the default value, or be left undefined if no default value is specified.
   * \param missing the result to use if the argument is omitted
   * \tparam Value The result type, which defaults to \p optional<string>
   **/
  template <class Value = optional<string>>
  Argument<Value> add_optional(OptionsSpec spec, Value missing = {}, SingleConverter<Value> conv = {}) const {
    return action::store_optional<Value>(add_generic(std::move(spec)), std::move(missing), std::move(conv));
  }

  /**
   * \brief Define an optional/positional rule that may match multiple arguments
   * \param nargs Constrains the matching of arguments, see \ref Nargs.
   **/
  template <class Value = vector<string>>
  Argument<Value> add_multi(OptionsSpec spec, Nargs nargs, MultiConverter<Value> conv = {}) const {
    return action::store_multi<Value>(add_generic(std::move(spec)), nargs, std::move(conv));
  }

  /**
   * \brief Defines a rule for matching an optional flag/switch without any arguments.
   *
   * If the option is specified, the result is equal to the specified \p value.  Otherwise, it is equal to the default value, if one is defined, or is left undefined.
   *
   * \param spec Specifies the result key and/or the option strings
   * \tparam Value The result type, \p bool by default.
   **/
  template <class Value = bool>
  Argument<Value> add_flag(OptionsSpec spec, Value value = true) const {
    return action::store_flag<Value>(add_generic(std::move(spec)), std::move(value));
  }

  /**
   * \brief Match a flag/switch without arguments, and aggregate results
   *
   * The specified value is appended to the currently stored vector of values.  The initial/default value is an empty vector.  This rule is particularly useful if multiple rules share the same result key.
   *
   * \param spec Specifies the result key and/or the option strings
   * \tparam Value The \p value_type of the result vector, \p bool by default
   **/
  template <class Value = bool>
  Argument<vector<Value>> add_append_flag(OptionsSpec spec, Value value = true) const {
    return action::append_flag<Value>(add_generic(std::move(spec)), std::move(value));
  }

  /**
   * \brief Match a single argument, and aggregate the results from multiple matches
   *
   * The matched argument is appended to the currently stored vector of values.
   *
   * The initial/default value is an empty vector.
   *
   * \param spec Specifies the result key and/or the option strings
   * \tparam Value The \p value_type of the result vector, \p std::string by default.
   **/
  template <class Value = string>
  Argument<vector<Value>> add_append(OptionsSpec spec, SingleConverter<Value> conv = {}) const {
    return action::append(add_generic(std::move(spec)), std::move(conv));
  }

  /**
   * \brief Match a flag/switch without arguments, and count the number of occurrences
   *
   * The rule does not allow any arguments to be specified.
   *
   * \param spec Specifies the result key and/or the option strings
   * \tparam Value The counter type, \p size_t by default.
   **/
  template <class Value = size_t>
  Argument<Value> add_count(OptionsSpec spec) const {
    return action::count(add_generic(std::move(spec)));
  }

  /**
   * \brief Adds an option for printing the help message.
   *
   * \param spec If not specified, defaults to defining a short 'h' and long 'help' option, using the default prefix character.
   *
   * When this message is parsed, the help message will be printed to standard output, and then exit(0) will be called.
   **/
  Argument<void> add_help_option(OptionsSpec spec = {}, HelpFormatterParameters const &params = {}) const;
  Argument<void> add_help_option(HelpFormatterParameters const &params) const {
    return add_help_option({}, params);
  }

  /**
   * \brief Adds an option for printing a simple message and exiting, e.g. for version information.
   *
   * The specified message will be printed to standard output, and then exit(0) will be called.
   **/
  Argument<void> add_print_option(OptionsSpec spec, string message) const;

  /**
   * \brief Adds a default option for printing version information.
   *
   * Defines a short 'h' option and a long 'help' option using the default prefix character.
   **/
  Argument<void> add_version_option(string message) const;

  /**
   * \brief Adds a default option for printing version information.
   **/
  Argument<void> add_version_option(OptionsSpec spec, string message) const;

  /**
   * \brief Define an argument that dispatches to a set of sub-parsers
   * The argument consumes all remaining arguments; the first argument selects the sub-parser to use; the remaining arguments are parsed by the sub-parser.
   * \see Subparser
   **/
  Subparsers add_subparsers(OptionsSpec spec = {}) const;
};
/**@}*/

/**
 * \brief Simple grouping construct for arguments
 *
 * Groups primarily serve to group arguments for the purpose of printing help information.  Adding an argument to a group just adds it to the underlying \ref ArgumentParser.
 *
 * Groups cannot be nested.
 **/
class ArgumentGroup : public ArgumentContainer {
public:

  virtual shared_ptr<GenericArgument> add_generic(OptionsSpec spec) const override;
  virtual ArgumentParser get_parser() const override;

  /**
   * \brief Change the group's title text
   **/
  ArgumentGroup title(string title) const;

  /**
   * \brief Change the group's description text
   **/
  ArgumentGroup description(string description) const;

  /**
   * \brief Add a mutual-exclusion group associated with this argument group
   *
   * Arguments added to the mutual exclusion group will be members of this group by default.
   * \param required Specifies if exactly one member of the group is required.
   * \see MutuallyExclusive
   **/
  MutuallyExclusiveGroup add_mutually_exclusive_group(bool required = false) const;



  /**
   * \brief Opaque implementation data
   **/
  struct Impl;

  ArgumentGroup() = default;
  explicit operator bool () const { return bool(impl); }

  ArgumentGroup(shared_ptr<Impl> impl)
    : impl(impl)
  {}

  shared_ptr<Impl> impl;
};

/**
 * \defgroup MutuallyExclusive Mutually-exclusive groups
 * \brief Mutually-exclusive groups Defining mutually-exclusive parsing rules
 *
 * Mutually-exclusive groups can be either \b optional or \b required.  In either case, it is an error for the command line to match \a more than one parsing rule in a mutually exclusive group.  For a \b required group, it is an error for no rules to match (exactly one rule must match).
 *
 * \remark Defining a rule as part of a mutually-exclusive group does not affect how it is parsed; the constraint imposed by the mutually-exclusive group is simply checked after parsing.
 *
 * Individual optional rules defined in a mutually-exclusive group must not be marked as required, nor may positional rules require arguments.
 *
 * \see ArgumentGroup::add_mutually_exclusive_group
 * \see ArgumentParser::add_mutually_exclusive_group
 * @{
 **/
/**
 * \brief Interface for defining mutually-exclusive parsing rules
 **/
class MutuallyExclusiveGroup : public ArgumentContainer {
public:

  virtual shared_ptr<GenericArgument> add_generic(OptionsSpec spec) const override;
  virtual ArgumentParser get_parser() const override;

  /**
   * \brief Opaque implementation data
   **/
  struct Impl;

  MutuallyExclusiveGroup() = default;
  explicit operator bool () const { return bool(impl); }

  MutuallyExclusiveGroup(shared_ptr<Impl> impl, ArgumentGroup group = {})
    : impl(impl), group(group)
  {}

  /**
   * \brief Associate this mutual-exclusion group with an \ref ArgumentGroup
   * \returns a mutually exclusive group that shares mutual exclusion with this group but places arguments into the specified \ref ArgumentGroup.
   **/
  MutuallyExclusiveGroup operator[](ArgumentGroup group) const { return MutuallyExclusiveGroup(this->impl, group); }

  shared_ptr<Impl> impl;
  ArgumentGroup group;
};

/**@}*/

/**
 * \brief Defines arguments and parses command lines
 **/
class ArgumentParser : public ArgumentContainer {
public:

  /**
   * \brief Create a group to which arguments can be added.
   *
   * Groups are only relevant to the help text; they do not affect parsing.  There are two implicit groups, titled "positional arguments" and "optional arguments", that contain the arguments not added to any explicit group.
   *
   * \param title The group heading
   * \param description Description paragraph that follows the group heading starting on a new line.
   **/
  ArgumentGroup add_group(string title = {}, string description = {});

  /**
   * \brief Parse a command line
   *
   * The first argument (\p argv[0]) is used to set the program name.  The remaining arguments are parsed according to the defined parsing rules.
   *
   * \returns The parsing results
   * \note If there is an error parsing, prints the usage message to standard error and exits with status 1.
   **/
  Result parse(int argc, char **argv) const;

  /**
   * \brief Parse a list of arguments.
   *
   * \returns The parsing results
   * \note If there is an error parsing, prints the usage message to standard error and exits with status 1.
   **/
  Result parse(vector<string_view> args) const;

  /**
   * \brief Parses a list of arguments
   *
   * \param[in,out] result The supplied Result map will be updated according to the parsing rules
   *
   * \note If there is an error parsing, prints the usage message to standard error and exits with status 1.
   **/
  void parse(Result &result, vector<string_view> args) const;

  /**
   * Extra arguments will remain in \p args and are not considered an error.
   * \throws ParseError on failure
   **/
  void try_parse_known_args(Result &result, vector<string_view> &args);

  /**
   * Extra arguments will remain in \p args and are not considered an error.
   * Exits with status 1 on failure.
   **/
  void parse_known_args(Result &result, vector<string_view> &args);

  /**
   * \throws ParseError on failure
   **/
  Result try_parse(int argc, char **argv) const;

  /**
   * \throws ParseError on failure
   **/
  Result try_parse(vector<string_view> args) const;

  /**
   * \throws ParseError on failure
   **/
  void try_parse(Result &result, vector<string_view> args) const;

  /**
   * \brief Prints the usage message to the specified stream
   *
   * \param os Output stream to use
   * \param params Parameters for customizing help formatting
   **/
  void print_usage(std::ostream &os, HelpFormatterParameters const &params = {}) const;

  /**
   * \returns The usage message
   * \param params Parameters for customizing help formatting
   **/
  string usage_string(HelpFormatterParameters const &params = {}) const;

  /**
   * \brief Prints the help message to the specified stream
   * \param params Parameters for customizing help formatting
   **/
  void print_help(std::ostream &os, HelpFormatterParameters const &params = {}) const;

  /**
   * \returns The help message
   * \param params Parameters for customizing help formatting
   **/
  string help_string(HelpFormatterParameters const &params = {}) const;

  /**
   * \brief Set the program name to use in the automatically-generated usage message
   **/
  ArgumentParser prog(string prog_name) const;

  /**
   * \brief Sets the program name to equal super_parser's prog name + " " + \p prog_name
   *
   * This relationship will hold dynamically even if the super parser's prog is set at a later time.
   * A weak reference to the super parser will be held.
   **/
  ArgumentParser prog_as_subparser(ArgumentParser const &super_parser, string prog_name) const ;

  /**
   * \brief Specify description text for the command.
   * This message will be follow the usage information.
   **/
  ArgumentParser description(string s) const;

  /**
   * \brief Specify additional description text.
   * This message will follow the other help information.
   **/
  ArgumentParser epilog(string s) const;

  /**
   * \brief Specify an explicit usage string to use in place of the automatically-generated one.
   **/
  ArgumentParser usage(string s) const;

  /**
   * \brief Specify alternative option prefix characters.
   *
   * By default there is only one prefix character, \p '-'.  Short options must start with one prefix character, and long options must start with two (possibly different) prefix characters.
   **/
  ArgumentParser prefix_chars(string value) const;

  /**
   * \brief Define a mutually exclusive group of arguments
   * \param required Specifies if exactly one member of the group is required.
   * \see MutuallyExclusive
   **/
  MutuallyExclusiveGroup add_mutually_exclusive_group(bool required = false) const;

  /**
   * \brief Copies the arguments and groups defined in another parser
   *
   * All of the argument parsing rule definitions, as well as the ArgumentGroup and MutuallyExclusiveGroup definitions, are copied to this parser.  They are added in the same order they were added to \p parent.  Any sub-parsers are also copied.
   **/
  ArgumentParser parent(ArgumentParser const &parent) const;


  virtual shared_ptr<GenericArgument> add_generic(OptionsSpec spec) const override;
  virtual ArgumentParser get_parser() const override;

  /**
   * \brief Create a deep copy
   **/
  ArgumentParser clone() const;

  /**
   * \brief Opaque implementation data.
   **/
  struct Impl;

  ArgumentParser() = default;
  ArgumentParser(shared_ptr<Impl> impl)
    : impl(impl)
  {}
  shared_ptr<Impl> impl;
};

/**
 * \brief Create a new ArgumentParser
 * \related ArgumentParser
 **/
ArgumentParser make_parser();

/**
 * \brief Exception representing a command-line parsing error
 **/
class ParseError : public std::invalid_argument {
  ArgumentParser parser_;
public:
  ParseError(ArgumentParser parser_, std::string const &message)
    : std::invalid_argument(message), parser_(parser_)
  {}

  /**
   * \return The parser associated with the error
   **/
  ArgumentParser parser() const { return parser_; }

  /**
   * \brief Print an error message and exit
   * This prints the parser usage method, followed by the error message, and then exits the program with code 1.
   **/
  [[noreturn]] void handle() const;
};


} // namespace jbms::argparse
} // namespace jbms


#endif /* HEADER GUARD */
