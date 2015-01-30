#include "./argparse.hpp"
#include <unordered_set>
#include <regex>
#include <stdlib.h>
#include <cctype>
#include <iostream>

// Needed to determine terminal width
// TODO: add support for Windows
#include <sys/ioctl.h>
#include <stdio.h>
#include <unistd.h>

#include <boost/core/demangle.hpp>

namespace jbms {
namespace argparse {

using std::weak_ptr;

string repr(string_view s) {
  // FIXME: make this escape bad characters
  string x;
  x += '\'';
  x += string(s);
  x += '\'';
  return x;
}


/**
 * Generic utilities
 **/
inline namespace util {

template <class Assoc, class Key>
static auto find_ptr(Assoc const &assoc, Key const &key) {
  auto it = assoc.find(key);
  if (it != assoc.end())
    return &it->second;
  return decltype(&it->second)(nullptr);
}

template <class Assoc, class Key>
static auto find_pair_ptr(Assoc const &assoc, Key const &key) {
  auto it = assoc.find(key);
  if (it != assoc.end())
    return &*it;
  return decltype(&*it)(nullptr);
}

static bool starts_with(string_view s, string_view prefix) {
  return s.size() >= prefix.size() && memcmp(&s[0], &prefix[0], prefix.size() * sizeof(string_view::value_type)) == 0;
}

// This converts an ASCII string to uppercase.  This is broken for UTF-8 since in unicode and especially UTF-8 upper case is not a code-point-wise operation.
static string ascii_to_upper(string_view s) {
  string result;
  result.reserve(s.size());
  for (auto x : s)
    result.push_back((char)std::toupper(x));
  return result;
}

static string replace_all(string_view s, string_view find_str, string_view replace_str) {
  string result;
  size_t start_pos = 0;
  for (size_t pos; (pos = s.find(find_str, start_pos)) != string_view::npos; start_pos = pos + find_str.size()) {
    result.append(&s[start_pos], &s[pos]);
    result.append(replace_str.begin(), replace_str.end());
  }
  result.append(&s[start_pos], &s[s.size()]);
  return result;
}

template <class T>
struct simple_range {
  T begin_, end_;
  simple_range(T begin_, T end_) : begin_(begin_), end_(end_) {}
  T begin() const { return begin_; }
  T end() const { return end_; }
};


struct always_true_pred {
  template <class... T>
  bool operator()(T &&...) {
    return true;
  }
};

struct identity {
  template <class T>
  T &&operator()(T &&x) { return std::forward<T>(x); }
};

template <class Range, class Transform = identity, class Predicate = always_true_pred>
void join(std::ostream &os, string_view sep, Range const &args, Transform &&t = {}, Predicate &&p = {}) {
  bool first = true;
  for (auto const &x : args) {
    if (!p(x))
      continue;
    if (!first)
      os << sep;
    first = false;
    os << t(x);
  }
}

template <class Range, class Transform = identity, class Predicate = always_true_pred>
static string join(string_view sep, Range const &args, Transform &&t = {}, Predicate &&p = {}) {
  std::ostringstream ostr;
  join(ostr, sep, args, std::forward<Transform>(t), std::forward<Predicate>(p));
  return ostr.str();
}

} // end namespace jbms::argparse::util


namespace detail {

[[noreturn]] void handle_type_conversion_failure(const char *name, string_view s) {
  throw std::invalid_argument("invalid " + boost::core::demangle(name) + " value: " + repr(s));
}

}


static void get_nargs_range(NargsValue nargs, size_t &min, size_t &max) {
  switch (nargs) {
  case OPTIONAL:
    min = 0; max = 1;
    return;
  case ZERO_OR_MORE:
  case REMAINDER:
    min = 0; max = std::numeric_limits<size_t>::max();
    return;
  case ONE_OR_MORE:
  case PARSER:
    min = 1; max = std::numeric_limits<size_t>::max();
    return;
  case NARGS_INVALID:
    // let this fall through to throw statement below
    break;
  }
  if (nargs >= 0) {
    min = max = static_cast<size_t>(nargs);
    return;
  }

  throw std::logic_error("Invalid nargs value encountered");
}


class GenericArgument {
public:

  string key;
  vector<string> option_strings;
  optional<string> help = string{};
  vector<string> metavar;
  bool required = false;
  NargsValue nargs = NARGS_INVALID;

  Handler handler;
  CopyHandler copy_handler;

  any default_value;

  struct ChoiceInfo {
    ChoiceInfo(string name, string invocation, OptionalString help)
      : name(name), invocation(invocation), help(help)
    {}
    string name;
    string invocation;
    optional<string> help;
  };

  vector<ChoiceInfo> choice_help;

  MutuallyExclusiveGroup::Impl *mutex_group = nullptr; // this is only set if the argument is part of a mutually-exclusive group
  weak_ptr<ArgumentGroup::Impl> group; // note: This is always set

  string name() const {
    if (!option_strings.empty())
      return join("/", option_strings);
    if (!metavar.empty())
      return metavar.front();
    // FIXME: this might not be correct
    return key;
  }

  bool is_positional() const { return option_strings.empty(); }

  bool is_help_suppressed() const { return !help; }

  virtual shared_ptr<GenericArgument> clone_for(ArgumentParser const &parser) {
    shared_ptr<GenericArgument> x(new GenericArgument);
    x->key = this->key;
    x->option_strings = this->option_strings;
    x->help = this->help;
    x->metavar = this->metavar;
    x->required = this->required;
    x->nargs = this->nargs;
    x->copy_handler = this->copy_handler;
    if (x->copy_handler)
      x->handler = x->copy_handler(*x, this->handler);
    else
      x->handler = this->handler;
    x->default_value = this->default_value;
    x->choice_help = this->choice_help;
    return x;
  }

  string format_metavar(size_t i) const {
    if (!metavar.empty())
      return metavar[std::min(metavar.size() - 1, i)];
    if (!choice_help.empty()) {
      std::ostringstream ostr;
      ostr << '{';
      join(ostr, ",", choice_help, [](auto const &x) { return x.name; });
      ostr << '}';
      return ostr.str();
    }
    if (is_positional())
      return key;
    return ascii_to_upper(key);
  }
};


void set_help(GenericArgument &arg, OptionalString help) {
  arg.help = std::move(help);
}

void set_nargs(GenericArgument &impl, Nargs nargs) {
  if (impl.is_positional() && nargs.value == NargsValue(0))
    throw std::logic_error("nargs cannot be 0 for positional arguments");
  impl.nargs = nargs;

  if (impl.is_positional()) {
    // set required value based on nargs
    switch (nargs) {
    case OPTIONAL:
    case ZERO_OR_MORE:
    case REMAINDER:
      impl.required = false;
      break;
    default:
      impl.required = true;
      break;
    }
  }
}

void set_required(GenericArgument &impl, bool value) {
  if (impl.is_positional())
    throw std::logic_error("required is not a valid option for positional arguments");
  impl.required = value;
}

void set_default_value(GenericArgument &impl, any value) {
  impl.default_value = value;
}


void set_metavar(GenericArgument &impl_, OptionsSpec value) {
  impl_.metavar = std::move(value);
}

void set_handler(GenericArgument &arg, Handler handler, CopyHandler copy_handler) {
  arg.handler = std::move(handler);
  arg.copy_handler = std::move(copy_handler);
}

Handler &get_handler(GenericArgument &impl) { return impl.handler; }

void add_choice_help(GenericArgument &arg, string name, string invocation, OptionalString help) {
  arg.choice_help.emplace_back(std::move(name), std::move(invocation), std::move(help));
}

string const &get_key(GenericArgument &arg) {
  return arg.key;
}

ArgumentParser get_parser(GenericArgument &arg) {
  return ArgumentGroup(arg.group.lock()).get_parser();
}

struct ArgumentGroup::Impl {
  vector<GenericArgument const *> actions;
  string title, description;
  weak_ptr<ArgumentParser::Impl> parser;

  shared_ptr<Impl> clone() {
    shared_ptr<Impl> x(new Impl);
    x->title = title;
    x->description = description;
    return x;
  }
};


ArgumentGroup ArgumentGroup::title(string title) const {
  impl->title = std::move(title);
  return *this;
}

ArgumentGroup ArgumentGroup::description(string description) const {
  impl->description = std::move(description);
  return *this;
}

struct MutuallyExclusiveGroup::Impl {
  vector<GenericArgument const *> actions;
  weak_ptr<ArgumentParser::Impl> parser;
  bool required;

  shared_ptr<Impl> clone() {
    shared_ptr<Impl> x(new Impl);
    x->required = required;
    return x;
  }
};

struct ArgumentParser::Impl {
  string prog;
  weak_ptr<ArgumentParser::Impl> prog_super_parser;

  string get_prog() const {
    if (auto super_parser = prog_super_parser.lock()) {
      return super_parser->get_prog() + " " + prog;
    }
    return prog;
  }

  char default_prefix = '-';
  string description;
  string epilog;
  optional<string> usage;

  // Default grouping of arguments
  ArgumentGroup positional_group, optional_group;

  vector<ArgumentGroup> groups;
  vector<shared_ptr<MutuallyExclusiveGroup::Impl>> mutex_groups;

  vector<shared_ptr<GenericArgument>> actions;

  std::unordered_map<string_view,GenericArgument const *> option_string_actions;

  string prefix_chars = "-";

  string ignore_options_string = "--";

  bool is_prefix_char(char c) const {
    return prefix_chars.find(c) != string::npos;
  }

  std::regex negative_number_re { R"RE(^-\d+$|^-\d*\.\d+$)RE" };

  bool has_negative_number_options = false;

  bool is_negative_number(string_view s) const {
    return regex_search(s.begin(), s.end(), negative_number_re);
  }
};

ArgumentParser ArgumentParser::get_parser() const { return *this; }
ArgumentParser ArgumentGroup::get_parser() const { return ArgumentParser(impl->parser.lock()); }
ArgumentParser MutuallyExclusiveGroup::get_parser() const { return ArgumentParser(impl->parser.lock()); }


void copy_actions(ArgumentParser const &source, ArgumentParser const &key);

ArgumentParser ArgumentParser::clone() const {
  auto p = make_parser();
  p.impl->prog = impl->prog;
  p.impl->default_prefix = impl->default_prefix;
  p.impl->description = impl->description;
  p.impl->epilog = impl->epilog;
  p.impl->usage = impl->usage;
  p.impl->prefix_chars = impl->prefix_chars;
  p.impl->ignore_options_string = impl->ignore_options_string;
  p.impl->has_negative_number_options = impl->has_negative_number_options;
  p.parent(*this);
  return p;
}

ArgumentParser make_parser() {
  ArgumentParser p;
  p.impl.reset(new ArgumentParser::Impl);
  p.impl->positional_group = p.add_group("positional arguments");
  p.impl->optional_group = p.add_group("optional arguments");
  return p;
}


static void register_argument(GenericArgument &arg, shared_ptr<ArgumentGroup::Impl> const &group) {
  arg.group = group; // create weak reference
  group->actions.push_back(&arg);
}

static void register_argument(GenericArgument &arg, shared_ptr<ArgumentParser::Impl> const &parser) {
  register_argument(arg, arg.is_positional() ? parser->positional_group.impl : parser->optional_group.impl);
}

static void register_argument(GenericArgument &arg, shared_ptr<MutuallyExclusiveGroup::Impl> const &mutex_group,
                              shared_ptr<ArgumentGroup::Impl> const &group = {}) {
  arg.mutex_group = mutex_group.get();
  mutex_group->actions.push_back(&arg);
  if (group)
    register_argument(arg, group);
  else
    register_argument(arg, mutex_group->parser.lock());
}

MutuallyExclusiveGroup ArgumentGroup::add_mutually_exclusive_group(bool required) const {
  return get_parser().add_mutually_exclusive_group(required)[*this];
}

MutuallyExclusiveGroup ArgumentParser::add_mutually_exclusive_group(bool required) const {
  shared_ptr<MutuallyExclusiveGroup::Impl> g_impl(new MutuallyExclusiveGroup::Impl);
  g_impl->parser = impl;
  g_impl->required = required;
  impl->mutex_groups.push_back(g_impl);
  return MutuallyExclusiveGroup(g_impl);
}

namespace detail {
struct ParserState {
  ArgumentParser parser;
  Result &parse_result;
  auto &impl() { return *parser.impl; }


  /**
   * \brief Input arguments to parse.
   **/
  vector<string_view> input;

  /**
   * \brief Next position in \ref input to use.
   **/
  size_t arg_index = 0;


  /**
   * \brief Next positional action to consider.
   **/
  size_t positional_action_index = 0;

  /**
   * \brief Indicates if all remaining arguments are treated as positional.
   *
   * This is set once we see "--".
   **/
  bool ignore_options = false;


  vector<GenericArgument const *> positional_actions;

  /**
   * Normally we try exhaustive matching.  If there are PARSER or REMAINDER positional arguments, though, exhaustive matching wouldn't be well-defined.
   **/
  bool greedy_parsing = false;

  std::unordered_map<MutuallyExclusiveGroup::Impl *,GenericArgument const *> mutex_group_to_seen_arg;

  ParserState(ArgumentParser const &parser, Result &parse_result, vector<string_view> input)
    : parser(parser), parse_result(parse_result), input(std::move(input))
  {

    // Check that mutually exclusive groups don't have required arguments
    for (auto const &g : impl().mutex_groups) {
      if (g->actions.size() <= 1)
        continue;

      for (auto a : g->actions) {
        if (a->required) {
          throw std::logic_error(a->name() + ": required arguments are not allowed in mutually exclusive groups");
        }
      }
    }

    // Determine list of positional actions
    for (auto const &action : impl().actions) {
      if (action->is_positional()) {
        positional_actions.push_back(action.get());
        if (action->nargs == PARSER || action->nargs == REMAINDER)
          greedy_parsing = true;
      }
    }
  }


  struct ParsedOption {
    /**
     * True if we determined that the next argument is syntactically an option.
     *
     * This may be true even if \ref action is false, because we failed to match to a known option.
     **/
    bool is_option = false;

    /**
     * Action matched.
     **/
    GenericArgument const *action = nullptr;

    /**
     * If true, indicates that \ref argument can only be an argument.  Otherwise, \ref argument may be either an argument or additional single-character options.
     **/
    bool arg_is_explicit = false;

    /**
     * Prefix of the argument that matched
     **/
    string_view option_string;

    /**
     * Possible option argument or additional single-character options.
     **/
    string_view argument;
  };

  optional<ParsedOption> parsed_option;

  /**
   * Try to parse the next argument as an option.
   *
   * If
   **/
  void parse_option() {
    if (parsed_option)
      return;

    // Construct the ParsedOption result.  It defaults to is_option == false.
    parsed_option.emplace();

    if (ignore_options) {
      return;
    }

    string_view arg = input[arg_index];

    // If it is an empty string, it is a positional
    if (arg.empty()) {
      return;
    }

    // If it doesn't start with a prefix character, it is positional
    if (impl().prefix_chars.find(arg[0]) == string::npos)
      return;

    // If the option is an exact match, use it
    if (auto action = find_ptr(impl().option_string_actions, arg)) {
      parsed_option->is_option = true;
      parsed_option->action = *action;
      parsed_option->option_string = arg;
      return;
    }

    // If it is just a single character and didn't already match, it is positional
    if (arg.size() == 1) {
      return;
    }

    // If it is ignore_options_string, or consists only of prefix characters, it is not an option
    if (impl().ignore_options_string == arg || arg.find_first_not_of(impl().prefix_chars) == string_view::npos)
      return;

    // Try to match the option string before the '='
    auto eq_pos = arg.find('=');
    if (eq_pos != string_view::npos) {
      auto option_string = arg.substr(0, eq_pos);
      auto explicit_arg = arg.substr(eq_pos+1);
      if (auto action = find_ptr(impl().option_string_actions, option_string)) {
        parsed_option->is_option = true;
        parsed_option->action = *action;
        parsed_option->argument = explicit_arg;
        parsed_option->option_string = option_string;
        parsed_option->arg_is_explicit = true;
        return;
      }
    }

    // Search through all possible prefixes of the option string
    std::vector<ParsedOption> candidates;
    auto add_candidate = [&](auto const &match, string_view argument, bool is_explicit) {
      candidates.emplace_back();
      auto &c = candidates.back();
      c.is_option = true;
      c.action = match.second;
      c.argument = argument;
      c.option_string = match.first;
      c.arg_is_explicit = is_explicit;
    };

    if (arg.size() >= 2 &&
        impl().prefix_chars.find(arg[1]) != string::npos) {

      // options starting with two prefix characters (i.e. "--option") are only split at the '='

      string_view option_prefix, explicit_arg;
      if (eq_pos != string_view::npos) {
        option_prefix = arg.substr(0, eq_pos);
        explicit_arg = arg.substr(eq_pos+1);
      } else {
        option_prefix = arg;
      }

      for (auto const &p : impl().option_string_actions) {
        if (starts_with(p.first, option_prefix)) {
          add_candidate(p, explicit_arg, eq_pos != string_view::npos);
        }
      }
    } else {
      // Single character options can be concatenated with their arguments but multiple character options always have to have their argument separate.

      string_view short_option_prefix = arg.substr(0,2);

      for (auto const &p : impl().option_string_actions) {
        if (p.first == short_option_prefix) {
          string_view short_explicit_arg = arg.substr(2);
          add_candidate(p, short_explicit_arg, false);
        } else if (starts_with(p.first, arg)) {
          add_candidate(p, {}, false);
        }
      }
    }

    // If multiple actions match, the option string was ambiguous
    if (candidates.size() > 1) {

      string msg = "ambiguous option: ";
      msg += string(arg);
      msg += " could match ";
      bool is_first = true;
      for (auto const &c : candidates) {
        if (!is_first)
          msg += ", ";
        is_first = false;
        msg += string(c.option_string);
      }

      throw std::invalid_argument(msg);
    }

    // If exactly one action matched, return it
    if (candidates.size() == 1) {
      parsed_option = candidates.front();
      return;
    }

    // If it was not found as an option but looks like a negative number, treat it as positional unless we have negative number options
    if (!impl().has_negative_number_options && impl().is_negative_number(arg)) {
      return;
    }

    // As a heuristic, if it contains a space, assume it is positional
    if (arg.find(' ') != string_view::npos) {
      return;
    }

    // It syntactically appears to be an option, but did not match any of the options
    parsed_option->is_option = true;
  }

  void advance() {
    parsed_option = nullopt;
    ++arg_index;
  }

  void check_argument_count(size_t count, size_t min, size_t max, NargsValue nargs) {
    if (count < min) {
      if (nargs == PARSER)
        throw std::invalid_argument("expected A... arguments");
      if (min == max)
        throw std::invalid_argument(min == 1 ? std::string("expected one argument")
                                    : ("expected " + std::to_string(min) + " arguments"));
      else if (max == 1)
        throw std::invalid_argument("expected at most one argument");
      else
        throw std::invalid_argument("expected at least one argument");
    }
  }

  vector<string_view> consume_arguments(NargsValue nargs, bool is_positional) {
    vector<string_view> result;
    size_t min, max;
    get_nargs_range(nargs, min, max);

    bool allow_ignore_options = is_positional;

    // FIXME: maybe make this customizable
    bool stop_at_option = true;

    if (nargs == REMAINDER) {
      stop_at_option = false;
      allow_ignore_options = false;
    }

    while (result.size() < max && arg_index < input.size()) {
      auto arg = input[arg_index];

      if (!ignore_options && (arg == impl().ignore_options_string)) {

        if (allow_ignore_options) {
          ignore_options = true;
          advance();
          continue;
        } else if (stop_at_option) {
          break;
        }
      }

      if (stop_at_option) {
        parse_option();
        if (parsed_option->is_option)
          break;
      }

      result.push_back(arg);

      if (nargs == PARSER)
        stop_at_option = false;

      advance();
    }

    check_argument_count(result.size(), min, max, nargs);
    return result;
  }

  vector<string_view> extras;

  struct PendingAction {
    GenericArgument const *action;
    string_view option_string; // reference to data in an GenericArgument
    vector<string_view> args;
    PendingAction(GenericArgument const *action, string_view option_string,
                  vector<string_view> args)
      : action(action), option_string(option_string), args(std::move(args))
    {}
  };

  std::unordered_set<GenericArgument const *> seen_actions;

  void take_action(GenericArgument const *action, string_view option_string, vector<string_view> args) {
    seen_actions.insert(action);
    (void)option_string;

    // See if the argument actually matched something for the purposes of mutual exclusion
    bool actually_matched = (!args.empty() || !action->is_positional());

    if (actually_matched && action->mutex_group) {
      auto it = mutex_group_to_seen_arg.find(action->mutex_group);

      // We allow the same command to be given multiple times
      if (it != mutex_group_to_seen_arg.end() && it->second != action) {
        throw std::invalid_argument("not allowed with argument " + it->second->name());
      }
      mutex_group_to_seen_arg[action->mutex_group] = action;
    }

    if (!actually_matched && !action->default_value.empty()) {
      // For positional arguments that match zero arguments, just use the default value
    } else {
      action->handler(parser, parse_result, action->key, args);
    }
  }

  void take_action_or_format_error(GenericArgument const *action, string_view option_string, vector<string_view> args) {
    try {
      take_action(action, option_string, std::move(args));
    } catch (std::invalid_argument &e) {
      auto name = action->name();
      if (name.empty())
        throw;
      throw std::invalid_argument(name + ": " + e.what());
    }
  }

  void take_action(PendingAction &a) {
    take_action(a.action, a.option_string, std::move(a.args));
  }

  void set_defaults() {
    for (auto const &a : impl().actions) {
      if (!a->default_value.empty() && !parse_result.count(a->key))
        parse_result[a->key] = a->default_value;
    }
  }

  // pending actions list used by \ref consume_option
  vector<PendingAction> pending_actions;

  /**
   * \pre parsed_option && parsed_option->is_option
   **/
  void consume_option() {
    auto action = parsed_option->action;

    // If no action was found, then it is an extra argument
    if (!action) {
      extras.push_back(input[arg_index]);
      advance();
      return;
    }

    string_view argument = parsed_option->argument;
    string_view option_string = parsed_option->option_string;

    try {

      // Loop because argument may contain extra options, and we will use multiple passes
      while (true) {

        // If there is an inline argument, try to match it
        if (!argument.empty() || parsed_option->arg_is_explicit) {

          // Check if the action wants <= 1 arguments
          size_t min, max;
          get_nargs_range(action->nargs, min, max);
          size_t count = std::min(max, size_t(1));
          check_argument_count(count, min, max, action->nargs);

          if (count == 0 && !parsed_option->arg_is_explicit) {
            // The current option was matched as a short option with extra text at the end.
            // Try to match the extra text to another short option.
            pending_actions.emplace_back(action, option_string, vector<string_view>{});

            assert(option_string.size() == 2);

            string new_short_option;
            new_short_option += option_string[0];
            new_short_option += argument[0];

            if (auto match = find_pair_ptr(impl().option_string_actions, new_short_option)) {
              // We found another option, so repeat the loop
              option_string = match->first;
              action = match->second;
              argument = argument.substr(1);
              continue;
            }

          } else if (count == 1) {
            // We used the inline argument
            // No more processing needed at this position
            pending_actions.emplace_back(action, option_string, vector<string_view>{ argument });
            advance();
            break;
          }
          throw std::invalid_argument("ignored explicit argument " + repr(argument));
        } else {
          // No inline argument, so try to match against the next arguments on the command line
          advance();

          auto cur_args = consume_arguments(action->nargs, false /* is option */);
          pending_actions.emplace_back(action, option_string, std::move(cur_args));
          break;
        }
      }

      for (auto &a : pending_actions)
        take_action(a);
      pending_actions.clear();
    }
    catch (std::invalid_argument &e) {
      throw std::invalid_argument("argument " + string(option_string) + ": " + e.what());
    }
  }

  void parse_exhaustive() {
    // String that describes the non-option arguments found
    // 'A' indicates an argument
    // 'O' indicates one or more option at that position, which is only used if interleaving is prohibited
    string positional_desc;
    vector<string_view> available_args;

    bool last_argument_was_option = false;
    while (arg_index < input.size()) {
      parse_option();

      if (parsed_option->is_option) {
        // Process option
        consume_option();
        last_argument_was_option = true;

      } else {
        auto arg = input[arg_index];

        if (!ignore_options && arg == impl().ignore_options_string) {
          ignore_options = true;
          advance();
          continue;
        }

        available_args.push_back(input[arg_index]);

        // FIXME: add interleave check
        if (last_argument_was_option) {
          positional_desc += 'O';
          last_argument_was_option = false;
        }

        positional_desc += 'A';
        advance();
      }
    }

    vector<size_t> pattern_offsets;
    string positional_pattern = "^";
    for (auto action : positional_actions) {
      positional_pattern += "O?(";
      switch (action->nargs) {
      case OPTIONAL:
        positional_pattern += "A?";
        break;
      case ZERO_OR_MORE:
        positional_pattern += "A*";
        break;
      case ONE_OR_MORE:
        positional_pattern += "A+";
        break;
      default:
        if (action <= 0)
          throw std::logic_error("unexpected nargs value for exhaustive positional parsing: " + std::to_string(int(action->nargs)));
        positional_pattern.append(size_t(action->nargs), 'A');
        break;
      }
      positional_pattern += ')';
      pattern_offsets.push_back(positional_pattern.size());
    }

    // Initially try to match all positional arguments (required for a valid parse)
    // Then to produce better error messages, try to match successively shorter prefixes of the positional arguments
    std::smatch match;
    size_t arg_i = 0;
    for (size_t num_actions = pattern_offsets.size(); num_actions > 0; --num_actions) {
      std::regex positional_re(positional_pattern.substr(0,pattern_offsets[num_actions-1]));
      if (std::regex_search(positional_desc, match, positional_re)) {
        vector<string_view> cur_args;
        for (size_t action_i = 0; action_i < num_actions; ++action_i) {
          size_t end_arg_i = arg_i + match[action_i+1].length();
          cur_args.assign(available_args.begin() + arg_i, available_args.begin() + end_arg_i);
          arg_i = end_arg_i;
          auto action = positional_actions[action_i];
          take_action_or_format_error(action, {} /* optional string*/, std::move(cur_args));
        }
        break;
      }
    }
    extras.insert(extras.end(), available_args.begin() + arg_i, available_args.end());
  }


  void parse_greedy() {

    while (arg_index < input.size()) {

      parse_option();

      if (parsed_option->is_option) {
        // Process option
        consume_option();
      } else if (positional_action_index < positional_actions.size()) {
        // process position argument
        auto action = positional_actions[positional_action_index];
        auto cur_args = consume_arguments(action->nargs, true /* is positional */);
        ++positional_action_index;
        take_action_or_format_error(action, {}/* option string*/, std::move(cur_args));
      } else {
        auto arg = input[arg_index];

        // The current argument is not an option but we have consumed all positional arguments already.
        // Therefore it is an extra argument.
        extras.push_back(arg);
        advance();
      }
    }

    // There are no arguments left, but we may still be able to process remaining positional arguments that can match zero arguments
    while (positional_action_index < positional_actions.size()) {
      auto action = positional_actions[positional_action_index];
      size_t min, max;
      get_nargs_range(action->nargs, min, max);
      if (min != 0)
        break;

      ++positional_action_index;
      take_action_or_format_error(action, {}/* option string*/, {} /* no args */);
    }
  }

  void check_required() {
    vector<string> missing_required_actions;
    for (auto const &action : impl().actions) {
      if (action->required && !seen_actions.count(action.get()))
        missing_required_actions.push_back(action->name());
    }

    if (!missing_required_actions.empty())
      throw std::invalid_argument("the following arguments are required: " + join(", ", missing_required_actions));

    for (auto const &group : impl().mutex_groups) {
      if (!group->required)
        continue;

      if (mutex_group_to_seen_arg.count(group.get()))
        continue;
      throw std::invalid_argument(
          "one of the arguments " +
          join(" ", group->actions, [](auto a) { return a->name(); }, [](auto a) { return !a->is_help_suppressed(); }) +
          " is required");
    }
  }

};

}

ArgumentGroup ArgumentParser::add_group(string title, string description) {
  ArgumentGroup group(std::shared_ptr<ArgumentGroup::Impl>(new ArgumentGroup::Impl));
  group.impl->parser = impl;
  group.impl->title = title;
  group.impl->description = description;
  impl->groups.push_back(group);
  return group;
}

static void register_option_strings(shared_ptr<ArgumentParser::Impl> const &impl, GenericArgument &arg) {
  for (auto const &option_string : arg.option_strings) {
    if (!impl->option_string_actions.emplace(option_string, &arg).second) {
      throw std::logic_error("conflicting option string: " + option_string);
    }
  }
}

/**
 * Constructs a new GenericArgument based on \p spec and registers it with the argument parser.
 *
 * This does not add the argument to any argument group.
 **/
static shared_ptr<GenericArgument> make_argument(shared_ptr<ArgumentParser::Impl> const &impl, OptionsSpec names) {
  shared_ptr<GenericArgument> arg_impl(new GenericArgument);

  if (names.empty())
    throw std::logic_error("Invalid argument specification");

  if (names.size() == 1 && (names.front().empty() || !impl->is_prefix_char(names[0][0]))) {
    // positional argument
    arg_impl->key = std::move(names[0]);
  } else {
    string_view long_option;
    string_view key;
    size_t name_i = 0;

    if (names[0].empty() ||
        !impl->is_prefix_char(names[0][0])) {
      // first name specifies the key
      key = names[0];
      name_i = 1;
    }

    for (; name_i < names.size(); ++name_i) {
      auto const &name = names[name_i];
      if (name.empty() || !impl->is_prefix_char(name[0]))
        throw std::logic_error("invalid option string " + repr(name) + ": must start with a character " +
                               repr(impl->prefix_chars));
      if (impl->is_negative_number(name))
        impl->has_negative_number_options = true;

      arg_impl->option_strings.emplace_back(name);
      if (long_option.empty() && name.size() >= 2 && impl->is_prefix_char(name[1]))
        long_option = name;
    }
    if (long_option.empty())
      long_option = names[0];
    if (key.empty()) {
      // trim off the prefix characters
      size_t j = 0;
      while (j < long_option.size() && impl->is_prefix_char(long_option[j]))
        ++j;
      key = long_option.substr(j);
    }
    arg_impl->key = string(key);

    register_option_strings(impl, *arg_impl);
  }

  impl->actions.push_back(arg_impl);
  return arg_impl;
}

shared_ptr<GenericArgument> ArgumentParser::add_generic(OptionsSpec spec) const {
  auto arg = make_argument(impl, std::move(spec));
  register_argument(*arg, impl);
  return arg;
}

shared_ptr<GenericArgument> ArgumentGroup::add_generic(OptionsSpec spec) const {
  auto arg = make_argument(impl->parser.lock(), std::move(spec));
  register_argument(*arg, impl);
  return arg;
}

shared_ptr<GenericArgument> MutuallyExclusiveGroup::add_generic(OptionsSpec spec) const {
  auto arg = make_argument(impl->parser.lock(), std::move(spec));
  register_argument(*arg, impl, group.impl);
  return arg;
}

/* input should contain only spaces as whitespace */
static vector<string> wordwrap_paragraph(string_view input, size_t width) {
  vector<string> output;

  while (!input.empty()) {
    if (input[0] == ' ') {
      input.remove_prefix(1);
      continue;
    }

    if (input.size() < width) {
      output.push_back(string(input));
      break;
    }

#if 0
    // Find last space in second half of line
    auto line = input.substr(0, width);
    auto second_half = line.substr(width / 2);
    size_t space_pos = second_half.rfind(' ');
    if (space_pos != string_view::npos) {
      line = line.substr(0, space_pos + width / 2);
    }
#else
    // See if there is a space in the first width+1 characters
    auto line = input.substr(0, width+1);
    size_t space_pos = line.rfind(' ');
    if (space_pos == string_view::npos) {
      // no space in first width+1 characters, just break at the first space
      space_pos = input.find(' ', width+1);
    }
    line = input.substr(0, space_pos);
#endif

    output.push_back(string(line));
    input.remove_prefix(line.size());
  }

  return output;
}

void ParseError::handle() const {
  parser().print_usage(std::cerr);
  std::cerr << parser().impl->get_prog() << ": error: " << what() << std::endl;
  std::exit(1);
}

/**
 * Exits with status 1 on failure.
 **/
Result ArgumentParser::parse(int argc, char **argv) const {
  try {
    return try_parse(argc, argv);
  }
  catch (ParseError &e) {
    e.handle();
  }
}

/**
 * Exits with status 1 on failure.
 **/
Result ArgumentParser::parse(vector<string_view> args) const {
  try {
    return try_parse(args);
  }
  catch (ParseError &e) {
    e.handle();
  }
}

/**
 * Exits with status 1 on failure.
 **/
void ArgumentParser::parse(Result &result, vector<string_view> args) const {
  try {
    return try_parse(result, args);
  }
  catch (ParseError &e) {
    e.handle();
  }
}

Result ArgumentParser::try_parse(int argc, char **argv) const {
  if (argc > 0 && impl->prog.empty()) {
    impl->prog = argv[0];
  }

  Result result;
  vector<string_view> args;
  for (int i = 1; i < argc; ++i)
    args.push_back(argv[i]);

  try_parse(result, std::move(args));

  return result;
}

Result ArgumentParser::try_parse(vector<string_view> args) const {
  Result result;
  try_parse(result, std::move(args));
  return result;
}


void ArgumentParser::try_parse(Result &result, vector<string_view> args) const {
  try {
    detail::ParserState state(*this, result, args);
    state.set_defaults();

    if (state.greedy_parsing)
      state.parse_greedy();
    else
      state.parse_exhaustive();

    state.check_required();

    if (!state.extras.empty()) {
      throw std::invalid_argument("unrecognized arguments: " + join(" ", state.extras));
    }
  }
  catch (std::invalid_argument &e) {
    throw ParseError(*this, e.what());
  }
}


/**
 * Help and version commands
 **/


Argument<void> ArgumentContainer::add_help_option(OptionsSpec spec, HelpFormatterParameters const &params) const {
  if (spec.empty()) {
    auto p = get_parser();
    spec = { string(1, p.impl->default_prefix) + "h", string(2, p.impl->default_prefix) + "help" };
  }
  auto generic = add_generic(std::move(spec));
  set_nargs(*generic, 0);

  set_handler(*generic, [params](ArgumentParser const &parser, Result &, string const &, vector<string_view> const &) {
      parser.print_help(std::cout, params);
      std::exit(0);
  });
  set_help(*generic, "show this help message and exit");
  return Argument<void>(std::move(generic));
}

Argument<void> ArgumentContainer::add_print_option(OptionsSpec spec, string message) const {
  auto generic = add_generic(std::move(spec));
  set_nargs(*generic, 0);

  set_handler(*generic,
              [ message = std::move(message) ](ArgumentParser const &, Result &, string const &, vector<string_view> const &) {
    std::cout << message;
    if (!message.empty() && message.back() != '\n')
      std::cout << '\n';
    std::cout << std::flush;
    std::exit(0);
  });
  return Argument<void>(std::move(generic));
}

Argument<void> ArgumentContainer::add_version_option(OptionsSpec spec, string message) const {
  return add_print_option(std::move(spec), std::move(message)).help("show program's version number and exit");
}

Argument<void> ArgumentContainer::add_version_option(string message) const {
  auto p = get_parser();
  return add_version_option({ string(1, p.impl->default_prefix) + "v", string(2, p.impl->default_prefix) + "version" }, message);
}




ArgumentParser ArgumentParser::prog(string prog_name) const {
  impl->prog = prog_name;
  impl->prog_super_parser = {};
  return *this;
}

ArgumentParser ArgumentParser::prog_as_subparser(ArgumentParser const &super_parser, string prog_name) const {
  impl->prog = prog_name;
  impl->prog_super_parser = super_parser.impl;
  return *this;
}

ArgumentParser ArgumentParser::description(string s) const {
  impl->description = std::move(s);
  return *this;
}

ArgumentParser ArgumentParser::epilog(string s) const {
  impl->epilog = std::move(s);
  return *this;
}

ArgumentParser ArgumentParser::usage(string s) const {
  impl->usage = std::move(s);
  return *this;
}

ArgumentParser ArgumentParser::prefix_chars(string value) const {
  if (value.empty())
    throw std::logic_error("prefix chars must be non-empty");
  impl->prefix_chars = std::move(value);
  if (impl->prefix_chars.find('-') != string::npos)
    impl->default_prefix = '-';
  else
    impl->default_prefix = impl->prefix_chars[0];
  return *this;
}



/**
 * Help
 **/



class HelpFormatter : public HelpFormatterParameters {
public:

  std::regex whitespace_matcher { R"RE(\s+)RE" };
  vector<string> wordwrap(string const &input, size_t width) {
    string text = std::regex_replace(input, whitespace_matcher, " ");
    string_view text_view = text;

    // strip whitespace at beginning and end
    while (!text_view.empty() && text_view[0] == ' ')
      text_view.remove_prefix(1);
    while (!text_view.empty() && text_view[text_view.size() - 1] == ' ')
      text_view.remove_suffix(1);
    return wordwrap_paragraph(text_view, width);
  }


  int current_indent = 0;
  int level = 0;
  int action_max_length = 0;
  std::ostringstream os;

  string trim() const {
    std::regex long_break_matcher("\n\n\n+");
    auto s = std::regex_replace(os.str(), long_break_matcher, "\n\n");
    string_view sv = s;
    auto pos = sv.find_first_not_of("\n");
    if (pos == string_view::npos)
      return {};
    sv = sv.substr(pos);
    sv = sv.substr(0, sv.find_last_not_of("\n") + 1);
    string result(sv);
    if (!result.empty())
      result += '\n';
    return result;
  }

  struct Section : public std::ostringstream {
    HelpFormatter &formatter;
    // Output stream to write output to in destructor
    std::ostream &os;

    string heading;


    Section(HelpFormatter &formatter, std::ostream &os, string heading)
      : formatter(formatter), os(os), heading(std::move(heading))
    {
      formatter.indent();
    }

    ~Section() {
      formatter.dedent();
      string contents = str();
      if (contents.empty())
        return;
      os << '\n';
      if (!heading.empty()) {
        formatter.spaces(os);
        os << heading << ":\n";
      }
      os << contents << '\n';
    }
  };


  HelpFormatter(HelpFormatterParameters p)
    : HelpFormatterParameters(std::move(p))
  {
    if (width == 0) {


      // TODO: add support for Windows
      struct ::winsize w;
      if (::ioctl(0, TIOCGWINSZ, &w) != -1) {
        width = int(w.ws_col);
      } else

        width = 80;

      width -= 2;
    }
    max_help_position = std::min(max_help_position, std::max(width - 20, indent_increment * 2));
  }

  void indent() {
    current_indent += indent_increment;
    level += 1;
  }

  void dedent() {
    current_indent -= indent_increment;
    assert(current_indent >= 0);
    level -= 1;
  }

  static void spaces(std::ostream &os, int n) {
    for (int i = 0; i < n; ++i)
      os << ' ';
  }

  void spaces(std::ostream &os) {
    spaces(os, current_indent);
  }

  size_t get_text_width() const {
    return size_t(std::max(width - current_indent, min_text_width));
  }


  void print_text(std::ostream &os, optional<string> const &text) {
    if (text)
      print_text(os, *text);
  }

  void print_text(std::ostream &os, string const &text_in) {
    if (text_in.empty())
      return;
    auto text = replace_all(text_in, "%(prog)s", prog);
    auto text_width = get_text_width();
    auto lines = wordwrap(text, text_width);
    for (auto const &line : lines) {
      spaces(os);
      os << line << '\n';
    }
    os << '\n';
  }

  void print_usage(std::ostream &os,
                   optional<string> const &usage_str,
                   vector<GenericArgument const *> const &actions,
                   optional<string> prefix) {

    // suppressed
    if (usage_str && usage_str->empty())
      return;

    if (!prefix)
      prefix = string("usage: ");

    string usage;

    if (usage_str) {
      usage = *prefix + replace_all(*usage_str, "%(prog)s", prog);
    } else if (!usage_str && actions.empty()) {
      usage = *prefix + prog;
    } else {
      vector<GenericArgument const *> optionals, positionals;
      for (auto a : actions) {
        if (a->is_positional())
          positionals.push_back(a);
        else
          optionals.push_back(a);
      }

      vector<GenericArgument const *> all_actions = optionals;
      all_actions.insert(all_actions.end(), positionals.begin(), positionals.end());

      // Sort all option arguments before positional arguments

      // First try to format as a single line

      auto all_usage = _format_actions_usage(all_actions);

      // see if it all fits on one line
      usage = *prefix + prog;

      // single-line usage for optionals and positionals
      string action_usage =
        join(" ", all_usage, identity{}, [](auto const &x) { return !x.empty(); });

      if (!usage.empty() && !action_usage.empty()) {
        usage += ' ';
      }
      usage += action_usage;

      auto text_width = get_text_width();

      if (usage.size() > text_width) {
        // Too long to fit on one line

        usage = *prefix;

        bool short_prog = false;

        // If prog is short, follow it with optionals and positionals, and indent to end of prog.
        if (double(usage.size() + prog.size()) <= 0.75 * double(text_width)) {
          usage += prog;
          if (!prog.empty())
            usage += ' ';
          short_prog = true;
        }
        // Otherwise, put prog on its own line and indent up to prefix

        string indent_str(usage.size(), ' ');
        size_t last_break = 0;
        bool break_next = false;

        auto write_part = [&](string const &part) {
          size_t line_len = usage.size() - last_break;
          bool first_part = (line_len == indent_str.size());

          if (break_next || (!first_part && part.size() + 1 + line_len > text_width)) {
            usage += '\n';
            last_break = usage.size();
            usage += indent_str;
            break_next = false;
            first_part = true;
          }
          if (!first_part)
            usage += ' ';
          usage += part;
        };

        auto write_parts = [&](vector<string> const &parts) {
          for (auto const &p : parts)
            write_part(p);
        };

        if (!short_prog) {
          write_part(prog);
          break_next = true;
        }

        // See if remaining usage fits on one line
        if (!short_prog && (action_usage.size() + indent_str.size() < size_t(text_width))) {
          write_part(action_usage);
        } else {
          auto opt_usage = _format_actions_usage(optionals);
          auto pos_usage = _format_actions_usage(positionals);

          if (!opt_usage.empty()) {
            write_parts(opt_usage);
            break_next = true;
          }
          write_parts(pos_usage);
        }

      }
    }
    usage += '\n';
    os << usage << '\n';
  }

  string format_metavar(GenericArgument const *action, size_t i) {
    return action->format_metavar(i);
  }

  string _format_args(GenericArgument const *action, bool force_required = false) {

    auto nargs = action->nargs;
    if (force_required) {
      switch (nargs) {
      case OPTIONAL:
        nargs = NargsValue(1);
        break;
      case ZERO_OR_MORE:
        nargs = ONE_OR_MORE;
        break;
      default:
        break;
      }
    }

    auto get_metavar = [&](size_t i) { return format_metavar(action, i); };
    switch (nargs) {
    case OPTIONAL: {
      string result;
      result += '[';
      result += get_metavar(0);
      result += ']';
      return result;
    }
    case ZERO_OR_MORE: {
      string result;
      result += '[';
      result += get_metavar(0);
      result += " [";
      result += get_metavar(1);
      result += " ...]]";
      return result;
    }
    case ONE_OR_MORE: {
      string result;
      result += get_metavar(0);
      result += " [";
      result += get_metavar(1);
      result += " ...]";
      return result;
    }
    case REMAINDER:
      return "...";
    case PARSER: {
      string result = get_metavar(0);
      result += " ...";
      return result;
    }
    default:
      break;
    }
    assert(action->nargs > 0);
    string result = get_metavar(0);
    for (int i = 1; i < action->nargs; ++i) {
      result += ' ';
      result += get_metavar(i);
    }
    return result;
  }

  string format_action_invocation(GenericArgument const *action) {
    // For positionals, just print the first metavar
    if (action->is_positional())
      return format_metavar(action, 0);

    // For optionals, format is:
    //   [if no argument]:   -s, --long
    //   [if argument]:      -s ARGS, --long ARGS

    if (action->nargs == 0)
      return join(", ", action->option_strings);
    string f = _format_args(action);
    return join(", ", action->option_strings, [&](string const &s) { return s + " " + f; });
  }

  // Update action max length to take into account \p action
  void update_action_max_length(GenericArgument const *action) {
    if (action->is_help_suppressed())
      return;

    // arguments are always indented by 1 level
    action_max_length = std::max(action_max_length,
                                 int(format_action_invocation(action).size()) + indent_increment);

    for (auto const &choice : action->choice_help) {
      if (choice.help) {
        auto const &s = choice.invocation;
        size_t len = s.size() + indent_increment * 2;
        action_max_length = std::max(action_max_length, int(len));
      }
    }
  }

  vector<string> _format_actions_usage(vector<GenericArgument const *> const &actions) {
    vector<string> result;

    // map from mutex group to number of non-suppressed actions
    // only groups with at least one non-suppressed action will be present, so 0 is never valid
    std::unordered_map<MutuallyExclusiveGroup::Impl *,size_t> mutex_group_shown_count;

    auto get_part = [&](GenericArgument const *action, bool in_mutex_group) {
      if (action->is_positional()) {
        return _format_args(action,in_mutex_group /* shown as "required" if in mutex group */);
      } else {
        string x = string(action->option_strings[0]);
        if (action->nargs != 0) {
          x += ' ';
          x += _format_args(action);
        }

        if (!action->required && !in_mutex_group)
          x = '[' + x + ']';
        return x;
      }
    };

    for (size_t action_i = 0; action_i < actions.size(); ++action_i) {
      auto action = actions[action_i];

      if (action->is_help_suppressed())
        continue;


      // Contiguous mutually-exclusive groups are formatted as a single part

      if (action->mutex_group) {
        auto mutex_group = action->mutex_group;

        // See if the whole group is contiguous starting from this point
        size_t end_action_i = action_i + 1;
        size_t num_shown = 1;

        while (end_action_i < actions.size() && actions[end_action_i]->mutex_group == mutex_group) {
          if (!actions[end_action_i]->is_help_suppressed())
            ++num_shown;
          ++end_action_i;
        }

        // count how many actions are shown in the mutex group, so that we can determine if we have all of them here
        auto &total_count = mutex_group_shown_count[mutex_group];
        if (total_count == 0) {
          for (auto other_act : mutex_group->actions) {
            if (!other_act->is_help_suppressed())
              ++total_count;
          }
        }

        if (total_count == num_shown) {
          // We have the whole group

          bool force_required = (total_count > 1 || mutex_group->required);

          string s = join(" | ",
                          simple_range<GenericArgument const * const *>(&actions[action_i], &actions[end_action_i]),
                          [&](auto a) { return get_part(a, force_required); },
                          [&](auto a) { return !a->is_help_suppressed(); });
          if (total_count > 1) {
            if (mutex_group->required)
              s = "(" + s + ")";
            else
              s = "[" + s + "]";
          }
          result.push_back(std::move(s));

          action_i = end_action_i - 1;
          continue;
        }
      }
      result.push_back(get_part(action, false /* not in group */));
    }
    return result;
  }

  void print_action_str(std::ostream &os, string const &action_header, string const &help) {
    // +2 spaces of padding at end of invocation before help text
    int help_position = std::min(action_max_length + 2, max_help_position);

    int help_width = std::max(width - help_position, min_text_width);
    int action_width = help_position - current_indent - 2;

    spaces(os);
    os << action_header;

    if (!help.empty()) {
      bool is_first = true;
      if (action_header.size() > size_t(action_width)) {
        // long action header, start help on the next line
        os << '\n';
        is_first = false;
      } else {
        // short action header, just line up to help position
        spaces(os, help_position - current_indent - int(action_header.size()));
      }
      for (auto const &line : wordwrap(help, size_t(help_width))) {
        if (!is_first)
          spaces(os, help_position);
        is_first = false;
        os << line;
        os << '\n';
      }
    } else {
      os << '\n';
    }
  }

  void print_action(std::ostream &os, GenericArgument const *action) {

    if (action->is_help_suppressed())
      return;

    auto action_header = format_action_invocation(action);
    print_action_str(os, action_header, *action->help);
    // FIXME: expand help

    for (auto const &choice : action->choice_help) {
      if (choice.help) {
        string extra_indent(indent_increment, ' ');
        print_action_str(os, extra_indent + choice.invocation, *choice.help);
      }
    }
  }
};

void ArgumentParser::print_usage(std::ostream &os, HelpFormatterParameters const &params) const {
  os << usage_string(params);
}


void ArgumentParser::print_help(std::ostream &os, HelpFormatterParameters const &params) const {
  os << help_string(params);
}

string ArgumentParser::help_string(HelpFormatterParameters const &params_in) const {
  auto params = params_in;
  if (params.prog.empty())
    params.prog = impl->get_prog();

  HelpFormatter formatter(params);

  vector<GenericArgument const *> actions;
  for (auto const &a : impl->actions) {
    formatter.update_action_max_length(a.get());
    actions.push_back(a.get());
  }

  formatter.print_usage(formatter.os, impl->usage, actions, {});

  formatter.print_text(formatter.os, impl->description);

  for (auto const &group : impl->groups) {
    HelpFormatter::Section section(formatter, formatter.os, group.impl->title);
    formatter.print_text(section, group.impl->description);
    for (auto action : group.impl->actions)
      formatter.print_action(section, action);
  }

  formatter.print_text(formatter.os, impl->epilog);

  return formatter.trim();
}

string ArgumentParser::usage_string(HelpFormatterParameters const &params_in) const {
  auto params = params_in;
  if (params.prog.empty())
    params.prog = impl->get_prog();
  HelpFormatter formatter(params);

  vector<GenericArgument const *> actions;
  for (auto const &a : impl->actions)
    actions.push_back(a.get());

  formatter.print_usage(formatter.os, impl->usage, actions, {});
  return formatter.trim();
}

ArgumentParser ArgumentParser::parent(ArgumentParser const &parent) const {
  if (parent.impl->has_negative_number_options)
    impl->has_negative_number_options = true;

  std::unordered_map<ArgumentGroup::Impl const *,shared_ptr<ArgumentGroup::Impl>> group_map;
  std::unordered_map<MutuallyExclusiveGroup::Impl const *,shared_ptr<MutuallyExclusiveGroup::Impl>> mutex_group_map;


  group_map[parent.impl->positional_group.impl.get()] = impl->positional_group.impl;
  group_map[parent.impl->optional_group.impl.get()] = impl->optional_group.impl;

  for (auto const &group : parent.impl->groups) {
    if (group_map.count(group.impl.get()))
      continue;
    auto x = group.impl->clone();
    x->parser = impl;
    group_map[group.impl.get()] = x;
    impl->groups.push_back(std::move(x));
  }

  for (auto const &mutex_group : parent.impl->mutex_groups) {
    auto x = mutex_group->clone();
    x->parser = impl;
    mutex_group_map[mutex_group.get()] = x;
    impl->mutex_groups.push_back(std::move(x));
  }

  for (auto const &action : parent.impl->actions) {
    auto x = action->clone_for(*this);

    shared_ptr<ArgumentGroup::Impl> group;
    if (auto g = action->group.lock())
      group = group_map.at(g.get());

    if (action->mutex_group)
      register_argument(*x, mutex_group_map.at(action->mutex_group), group);
    else if (group)
      register_argument(*x, group);
    else
      register_argument(*x, impl);


    register_option_strings(impl, *x);
    impl->actions.push_back(std::move(x));
  }
  return *this;
}




/**
 * Subparser support
 **/

namespace detail {
struct SubparserEntry {
  ArgumentParser parser;
  vector<string> names;

  string format_invocation() const {
    string s = names.front();
    if (names.size() > 1) {
      s += " (";
      for (size_t i = 1; i < names.size(); ++i) {
        if (i > 1)
          s += ", ";
        s += names[i];
      }
      s += ')';
    }
    return s;
  }
};

struct SubparsersHandler {
  GenericArgument &arg;
  SubparsersHandler(GenericArgument &arg)
    : arg(arg)
  {}

  vector<SubparserEntry> parsers;
  std::unordered_map<string_view,size_t> parser_map;

  void operator()(ArgumentParser const &, Result &result, string const &key, vector<string_view> values) const {
    auto command = values.at(0);

    if (auto entry_num = find_ptr(parser_map, command)) {
      auto const &e = parsers[*entry_num];
      values.erase(values.begin());
      e.parser.try_parse(result, values);
    } else {
      string command_metavar = arg.format_metavar(0);
      if (command_metavar.empty())
        command_metavar = "command";
      throw std::invalid_argument("unknown " + command_metavar + " " + repr(command));
    }

    if (!key.empty())
      result[key] = string(command);
  }
};

Handler copy_subparsers_handler(GenericArgument &arg, Handler const &handler_func) {
  auto handler = handler_func.target<detail::SubparsersHandler>();
  if (!handler)
    throw std::logic_error("Internal error: corrupted subparsers");

  SubparsersHandler new_handler(arg);
  new_handler.parser_map = handler->parser_map;
  for (auto const &parser : handler->parsers) {
    new_handler.parsers.emplace_back();
    auto &new_parser = new_handler.parsers.back();
    new_parser.names = parser.names;
    new_parser.parser = parser.parser.clone();
    if (auto super_parser = parser.parser.impl->prog_super_parser.lock())
      new_parser.parser.impl->prog_super_parser = get_parser(arg).impl;
  }
  return std::move(new_handler);
}
}

Subparsers ArgumentContainer::add_subparsers(OptionsSpec spec) const {
  if (spec.empty())
    spec.push_back(string() /* empty key */);
  auto generic = add_generic(std::move(spec));
  set_nargs(*generic, PARSER);
  set_handler(*generic, detail::SubparsersHandler(*generic), &detail::copy_subparsers_handler);
  return Subparsers(std::move(generic));
}

Subparsers Subparsers::help(string help) const {
  set_help(*generic, std::move(help));
  return *this;
}

Subparsers Subparsers::hide() const {
  set_help(*generic, {});
  return *this;
}

Subparsers Subparsers::metavar(OptionsSpec value) const {
  set_metavar(*generic, std::move(value));
  return *this;
}

ArgumentParser Subparsers::add_parser(OptionsSpec names, OptionalString short_desc) const {

  auto handler = get_handler(*generic).target<detail::SubparsersHandler>();
  if (!handler)
    throw std::logic_error("Internal error: corrupted subparsers");

  size_t index = handler->parsers.size();
  handler->parsers.emplace_back();
  auto &entry = handler->parsers.back();
  entry.parser = make_parser();

  entry.names = std::move(names);
  if (entry.names.empty())
    throw std::logic_error("At least one name must be specified");

  entry.parser.prog_as_subparser(get_parser(*generic), entry.names.front());

  for (auto const &name : entry.names) {
    if (!handler->parser_map.emplace(name, index).second)
      throw std::logic_error("Duplicate parser name: " + name);
  }

  add_choice_help(*generic, entry.names.front(), entry.format_invocation(), short_desc);

  return entry.parser;
}


} // namespace jbms::argparse
} // namespace jbms
