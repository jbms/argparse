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

static void propagate_prog_name(detail::ArgumentParserImpl *impl_);

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

static string repr(string_view s) {
  // FIXME: make this escape bad characters
  string x;
  x += '\'';
  x += string(s);
  x += '\'';
  return x;
}

template <class T>
struct simple_range {
  T begin_, end_;
  simple_range(T begin_, T end_) : begin_(begin_), end_(end_) {}
  T begin() const { return begin_; }
  T end() const { return end_; }
};

template <class Range, class Transform>
static string join(string_view sep, Range const &args, Transform &&t) {
  string result;
  bool first = true;
  for (auto const &x : args) {
    if (!first) {
      result += string(sep);
    }
    first = false;
    result += string(t(x));
  }
  return result;
}

template <class Range, class Transform, class Predicate>
static string join_if(string_view sep, Range const &args, Transform &&t, Predicate &&p) {
  string result;
  bool first = true;
  for (auto const &x : args) {
    if (!p(x))
      continue;

    if (!first) {
      result += string(sep);
    }
    first = false;
    result += string(t(x));
  }
  return result;
}

template <class Range>
static string join(string_view sep, Range const &args) {
  return join(sep, args, [](auto const &x) -> decltype(x) { return x; });
}


static void get_nargs_range(Nargs nargs, size_t &min, size_t &max) {
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

namespace detail {

[[noreturn]] void handle_type_conversion_failure(const char *name, string_view s) {
  throw std::invalid_argument("invalid " + boost::core::demangle(name) + " value: " + repr(s));
}

class ArgumentImpl {
public:

  MutuallyExclusiveGroup::Impl *mutex_group = nullptr;
  ArgumentGroup::Impl *group = nullptr;

  string dest;
  vector<string> option_strings;

  string name() const {
    if (!option_strings.empty())
      return join("/", option_strings);
    if (!metavar.empty())
      return metavar.front();
    // FIXME: this might not be correct
    return dest;
  }

  bool is_positional() const { return option_strings.empty(); }

  bool is_help_suppressed() const { return !help; }

  optional<string> help = string{};
  vector<string> metavar;
  bool required = false;
  Nargs nargs = NARGS_INVALID;
  Handler handler;
  any default_value;
  virtual ~ArgumentImpl() {}

  virtual std::unique_ptr<ArgumentImpl> clone() {
    return std::make_unique<ArgumentImpl>(*this);
  }

  string format_metavar(size_t i) const {
    if (!metavar.empty())
      return metavar[std::min(metavar.size() - 1, i)];
    if (is_positional())
      return dest;
    return ascii_to_upper(dest);
  }
};



void set_help(ArgumentImpl &impl, optional<string> help) {
  impl.help = std::move(help);
}

void set_nargs(ArgumentImpl &impl, Nargs nargs) {
  if (impl.is_positional() && nargs == Nargs(0))
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
void set_required(ArgumentImpl &impl, bool value) {
  if (impl.is_positional())
    throw std::logic_error("required is not a valid option for positional arguments");
  impl.required = value;
}

void set_default_value(ArgumentImpl &impl, any value) {
  impl.default_value = value;
}

void set_handler(ArgumentImpl &impl, Handler handler) {
  impl.handler = handler;
}

void set_metavar(ArgumentImpl &impl_, OptionsSpec value) {
  impl_.metavar = std::move(value.spec);
}


}

struct ArgumentGroup::Impl {
  vector<detail::ArgumentImpl const *> actions;
  string title, description;
  detail::ArgumentParserImpl *parent;

  std::unique_ptr<Impl> clone() {
    auto x = std::make_unique<Impl>();
    x->title = title;
    x->description = description;
    return x;
  }

  void register_argument_impl(detail::ArgumentImpl *arg) {
    arg->group = this;
    actions.push_back(arg);
  }
};

ArgumentGroup &ArgumentGroup::title(string title) {
  impl_->title = std::move(title);
  return *this;
}

ArgumentGroup &ArgumentGroup::description(string description) {
  impl_->description = std::move(description);
  return *this;
}


struct MutuallyExclusiveGroup::Impl {
  vector<detail::ArgumentImpl const *> actions;
  ArgumentGroup::Impl *parent_group = nullptr;
  detail::ArgumentParserImpl *parent = nullptr;
  bool required;

  std::unique_ptr<Impl> clone() {
    auto x = std::make_unique<Impl>();
    x->required = required;
    return x;
  }


  void register_argument_impl(detail::ArgumentImpl *arg_impl);
};

namespace detail {
struct ArgumentParserImpl {
  string prog;
  bool prog_set_explicitly = false;
  char default_prefix = '-';
  string description;
  string epilog;
  optional<string> usage;

  // Default grouping of arguments
  ArgumentGroup::Impl positional_group, optional_group;

  void register_argument_impl(detail::ArgumentImpl *arg) {
    if (arg->is_positional())
      positional_group.actions.push_back(arg);
    else
      optional_group.actions.push_back(arg);
  }

  vector<std::unique_ptr<ArgumentGroup::Impl>> groups;
  vector<std::unique_ptr<MutuallyExclusiveGroup::Impl>> mutex_groups;

  vector<std::unique_ptr<detail::ArgumentImpl>> actions;

  std::unordered_map<string_view,detail::ArgumentImpl const *> option_string_actions;

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

  ArgumentParserImpl() {
    positional_group.title = "positional arguments";
    optional_group.title = "optional arguments";
  }

  void copy_from(ArgumentParserImpl *parent);

  std::unique_ptr<ArgumentParserImpl> clone() {
    auto result = std::make_unique<ArgumentParserImpl>();
    result->prog = prog;
    result->prog_set_explicitly = prog_set_explicitly;
    result->default_prefix = default_prefix;
    result->description = description;
    result->epilog = epilog;
    result->usage = usage;
    result->prefix_chars = prefix_chars;
    result->ignore_options_string = ignore_options_string;
    result->has_negative_number_options = has_negative_number_options;
    result->copy_from(this);
    return result;
  }
};

}
detail::ArgumentParserImpl *ArgumentParser::get_parser_impl() { return impl_.get(); }
detail::ArgumentParserImpl *ArgumentGroup::get_parser_impl() { return impl_->parent; }

void ArgumentParser::register_argument_impl(detail::ArgumentImpl *arg) { impl_->register_argument_impl(arg); }
void ArgumentGroup::register_argument_impl(detail::ArgumentImpl *arg) { impl_->register_argument_impl(arg); }

void MutuallyExclusiveGroup::Impl::register_argument_impl(detail::ArgumentImpl *arg_impl) {
  arg_impl->mutex_group = this;
  actions.push_back(arg_impl);
  if (parent_group)
    parent_group->register_argument_impl(arg_impl);
  else
    parent->register_argument_impl(arg_impl);
}

void MutuallyExclusiveGroup::register_argument_impl(detail::ArgumentImpl *arg_impl) { impl_->register_argument_impl(arg_impl); }
detail::ArgumentParserImpl *MutuallyExclusiveGroup::get_parser_impl() { return impl_->parent; }



MutuallyExclusiveGroup ArgumentGroup::add_mutually_exclusive_group(bool required) {
  impl_->parent->mutex_groups.push_back(std::make_unique<MutuallyExclusiveGroup::Impl>());
  auto ptr = impl_->parent->mutex_groups.back().get();
  ptr->required = required;
  ptr->parent = impl_->parent;
  ptr->parent_group = impl_;
  MutuallyExclusiveGroup g;
  g.impl_ = ptr;
  return g;
}

MutuallyExclusiveGroup ArgumentParser::add_mutually_exclusive_group(bool required) {
  impl_->mutex_groups.push_back(std::make_unique<MutuallyExclusiveGroup::Impl>());
  auto ptr = impl_->mutex_groups.back().get();
  ptr->required = required;
  ptr->parent = impl_.get();
  MutuallyExclusiveGroup g;
  g.impl_ = ptr;
  return g;
}

namespace detail {
struct ParserState {
  ArgumentParser parser;
  Result &parse_result;
  detail::ArgumentParserImpl &impl() { return *parser.impl_; }


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


  vector<ArgumentImpl const *> positional_actions;

  /**
   * Normally we try exhaustive matching.  If there are PARSER or REMAINDER positional arguments, though, exhaustive matching wouldn't be well-defined.
   **/
  bool greedy_parsing = false;

  std::unordered_map<MutuallyExclusiveGroup::Impl *,ArgumentImpl const *> mutex_group_to_seen_arg;

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
    ArgumentImpl const *action = nullptr;

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

  void check_argument_count(size_t count, size_t min, size_t max, Nargs nargs) {
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

  vector<string_view> consume_arguments(Nargs nargs, bool is_positional) {
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
    ArgumentImpl const *action;
    string_view option_string; // reference to data in an ArgumentImpl
    vector<string_view> args;
    PendingAction(ArgumentImpl const *action, string_view option_string,
                  vector<string_view> args)
      : action(action), option_string(option_string), args(std::move(args))
    {}
  };

  std::unordered_set<ArgumentImpl const *> seen_actions;

  void take_action(ArgumentImpl const *action, string_view option_string, vector<string_view> args) {
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
      action->handler(parser, parse_result, action->dest, args);
    }
  }

  void take_action(PendingAction &a) {
    take_action(a.action, a.option_string, std::move(a.args));
  }

  void set_defaults() {
    for (auto const &a : impl().actions) {
      if (!a->default_value.empty() && !parse_result.count(a->dest))
        parse_result[a->dest] = a->default_value;
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
    string positional_pattern;
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
      if (std::regex_match(positional_desc, match, positional_re)) {
        vector<string_view> cur_args;
        for (size_t action_i = 0; action_i < num_actions; ++action_i) {
          size_t end_arg_i = arg_i + match[action_i+1].length();
          cur_args.assign(available_args.begin() + arg_i, available_args.begin() + end_arg_i);
          arg_i = end_arg_i;
          take_action(positional_actions[action_i], {} /* optional string*/, std::move(cur_args));
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
        // FIXME: need to insert argument name for error handling
        take_action(action, {}/* option string*/, std::move(cur_args));
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
      take_action(action, {}/* option string*/, {} /* no args */);
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
          join_if(" ", group->actions, [](auto a) { return a->name(); }, [](auto a) { return !a->is_help_suppressed(); }) +
          " is required");
    }
  }

};

}

ArgumentParser::ArgumentParser()
  : impl_(std::make_shared<detail::ArgumentParserImpl>()) {
}

ArgumentParser::~ArgumentParser()
{}

ArgumentGroup ArgumentParser::add_group(string title, string description) {
  impl_->groups.emplace_back(new ArgumentGroup::Impl);
  auto group_impl = impl_->groups.back().get();
  group_impl->parent = impl_.get();
  group_impl->title = std::move(title);
  group_impl->description = std::move(description);
  return ArgumentGroup(group_impl);
}

static void register_option_strings(detail::ArgumentParserImpl *impl_, detail::ArgumentImpl *arg_impl) {
  for (auto const &option_string : arg_impl->option_strings) {
    if (!impl_->option_string_actions.emplace(option_string, arg_impl).second) {
      throw std::logic_error("conflicting option string: " + option_string);
    }
  }
}

/**
 * Constructs an ArgumentImpl based on \p spec and registers it with the argument parser.
 *
 * This does not add the argument to any argument group.
 *
 * \param arg_impl If non-null, this object (which may be an instance of a subclass of ArgumentImpl) is used instead of constructing a new ArgumentImpl.
 **/
static detail::ArgumentImpl &
make_argument_impl_helper(detail::ArgumentParserImpl *impl_, OptionsSpec spec, detail::ArgumentImpl *arg_impl) {
  impl_->actions.emplace_back(arg_impl ? arg_impl : new detail::ArgumentImpl);
  arg_impl = impl_->actions.back().get();
  //arg_impl->parent = impl_;

  auto const &names = spec.spec;

  if (names.empty())
    throw std::logic_error("Invalid argument specification");

  if (names.size() == 1 && (names.front().empty() || !impl_->is_prefix_char(names[0][0]))) {
    // positional argument
    arg_impl->dest = std::move(names[0]);
  } else {
    string_view long_option;
    string_view dest;
    size_t name_i = 0;

    if (names[0].empty() ||
        !impl_->is_prefix_char(names[0][0])) {
      // first name specifies the dest
      dest = names[0];
      name_i = 1;
    }

    for (; name_i < names.size(); ++name_i) {
      auto const &name = names[name_i];
      if (name.empty() || !impl_->is_prefix_char(name[0]))
        throw std::logic_error("invalid option string " + repr(name) + ": must start with a character " +
                               repr(impl_->prefix_chars));
      if (impl_->is_negative_number(name))
        impl_->has_negative_number_options = true;

      arg_impl->option_strings.emplace_back(name);
      if (long_option.empty() && name.size() >= 2 && impl_->is_prefix_char(name[1]))
        long_option = name;
    }
    if (long_option.empty())
      long_option = names[0];
    if (dest.empty()) {
      // trim off the prefix characters
      size_t j = 0;
      while (j < long_option.size() && impl_->is_prefix_char(long_option[j]))
        ++j;
      dest = long_option.substr(j);
    }
    arg_impl->dest = string(dest);

    register_option_strings(impl_, arg_impl);
  }

  return *arg_impl;
}

namespace detail {

detail::ArgumentImpl &make_argument_impl(ArgumentContainer *container, OptionsSpec spec, detail::ArgumentImpl *arg_impl = nullptr) {
  auto &arg = make_argument_impl_helper(container->get_parser_impl(), std::move(spec), arg_impl);
  container->register_argument_impl(&arg);
  return arg;
}

detail::ArgumentImpl &ArgumentContainer::make_argument_impl(OptionsSpec spec) {
  return detail::make_argument_impl(this, std::move(spec));
}
}


namespace detail {
ArgumentBase::ArgumentBase(ArgumentImpl *impl_)
  : impl_(impl_), dest_(impl_->dest) {
}
}

struct HelpFormatterParameters {
  string prog;
  int indent_increment = 2;
  int max_help_position = 24;
  int width = 0;
  int min_text_width = 11;
};


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
  std::cerr << parser().impl_->prog << ": error: " << what() << std::endl;
  std::exit(1);
}

static void parse_helper(ArgumentParser const &parser, Result &result, vector<string_view> args) {
  auto impl_ = parser.impl_.get();

  // We will set a prog name automatically for all subparsers if they don't already have a prog name explicitly specified
  propagate_prog_name(impl_);

  try {
    detail::ParserState state(parser, result, args);
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
    throw ParseError(parser, e.what());
  }
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
  if (argc > 0 && impl_->prog.empty()) {
    impl_->prog = argv[0];
  }

  Result result;
  vector<string_view> args;
  for (int i = 1; i < argc; ++i)
    args.push_back(argv[i]);

  parse_helper(*this, result, std::move(args));

  return result;
}

Result ArgumentParser::try_parse(vector<string_view> args) const {
  Result result;
  parse_helper(*this, result, std::move(args));
  return result;
}


void ArgumentParser::try_parse(Result &result, vector<string_view> args) const {
  parse_helper(*this, result, std::move(args));
}


namespace detail {

struct SubparserEntry {
  ArgumentParser parser;
  vector<string> names;
  optional<string> short_desc;

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

class SubparsersArgumentImpl : public ArgumentImpl {
public:

  void initialize() {
    set_nargs(*this, PARSER);
    handler = [this](ArgumentParser const &, Result &result, string_view, vector<string_view> values) {
      auto command = values.front();
      if (!dest.empty())
        result[dest] = string(command);

      auto it = parser_map.find(command);
      if (it == parser_map.end()) {
        string command_metavar = format_metavar(0);
        if (command_metavar.empty())
          command_metavar = "command";
        throw std::invalid_argument("unknown " + command_metavar + " " + repr(command));
      }

      auto const &e = parsers[it->second];
      values.erase(values.begin());
      parse_helper(e.parser, result, values);
    };
  }

  SubparsersArgumentImpl() = default;
  SubparsersArgumentImpl(ArgumentImpl const &x) : ArgumentImpl(x) {}

  std::unordered_map<string_view,size_t> parser_map;

  std::vector<SubparserEntry> parsers;

  virtual std::unique_ptr<ArgumentImpl> clone() override {
    auto x = std::make_unique<SubparsersArgumentImpl>(*this);
    x->parser_map = parser_map;
    for (auto const &parser : x->parsers) {
      x->parsers.emplace_back();
      auto &new_parser = x->parsers.back();
      new_parser = parser;
      new_parser.parser.impl_ = parser.parser.impl_->clone();
    }
    return std::move(x);
  }
};

Argument<void> ArgumentContainer::add_help_option(OptionsSpec spec) {
  auto &arg_impl = this->make_argument_impl(std::move(spec));
  set_nargs(arg_impl, Nargs(0));

  set_handler(arg_impl, [](ArgumentParser const &parser, Result &, string const &, vector<string_view>) {
    parser.print_help(std::cout);
    std::exit(0);
  });
  set_help(arg_impl, string("show this help message and exit"));
  return Argument<void>(&arg_impl);
}

Argument<void> ArgumentContainer::add_help_option() {
  auto impl_ = get_parser_impl();
  return add_help_option({ string(1, impl_->default_prefix) + "h", string(2, impl_->default_prefix) + "help" });
}


Argument<void> ArgumentContainer::add_print_option(OptionsSpec spec, string message) {
  auto &arg_impl = this->make_argument_impl(std::move(spec));
  set_nargs(arg_impl, Nargs(0));

  set_handler(arg_impl,
              [ message = std::move(message) ](ArgumentParser const &, Result &, string const &, vector<string_view>) {
    std::cout << message;
    if (!message.empty() && message.back() != '\n')
      std::cout << '\n';
    std::cout << std::flush;
    std::exit(0);
  });
  return Argument<void>(&arg_impl);
}

Argument<void> ArgumentContainer::add_version_option(OptionsSpec spec, string message) {
  return add_print_option(std::move(spec), std::move(message)).help("show program's version number and exit");
}

Argument<void> ArgumentContainer::add_version_option(string message) {
  auto impl_ = get_parser_impl();
  return add_version_option({ string(1, impl_->default_prefix) + "v", string(2, impl_->default_prefix) + "version" }, message);
}

Subparsers ArgumentContainer::add_subparsers(OptionsSpec spec) {
  auto arg_impl = new detail::SubparsersArgumentImpl;
  detail::make_argument_impl(this, std::move(spec), arg_impl);
  arg_impl->initialize();
  return Subparsers(arg_impl);
}

Subparsers ArgumentContainer::add_subparsers() {
  return add_subparsers(""/* empty dest */);
}


}

static void propagate_prog_name(detail::ArgumentParserImpl *impl_) {
  if (impl_->prog.empty())
    return;
  // set prog name for all subparser parsers
  for (auto const &action : impl_->actions) {
    if (auto subparsers_impl = dynamic_cast<detail::SubparsersArgumentImpl *>(action.get())) {
      for (auto const &e : subparsers_impl->parsers) {
        auto p_impl = e.parser.impl_.get();
        if (!p_impl->prog_set_explicitly) {
          p_impl->prog = impl_->prog + " " + e.names.front();
        }
      }
    }
  }
}

ArgumentParser &ArgumentParser::prog(string prog_name) {
  impl_->prog = prog_name;
  impl_->prog_set_explicitly = true;
  propagate_prog_name(impl_.get());
  return *this;
}

ArgumentParser &ArgumentParser::description(string s) {
  impl_->description = std::move(s);
  return *this;
}

ArgumentParser &ArgumentParser::epilog(string s) {
  impl_->epilog = std::move(s);
  return *this;
}

ArgumentParser &ArgumentParser::usage(string s) {
  impl_->usage = std::move(s);
  return *this;
}

ArgumentParser &ArgumentParser::prefix_chars(string value) {
  if (value.empty())
    throw std::logic_error("prefix chars must be non-empty");
  impl_->prefix_chars = std::move(value);
  if (impl_->prefix_chars.find('-') != string::npos)
    impl_->default_prefix = '-';
  else
    impl_->default_prefix = impl_->prefix_chars[0];
  return *this;
}



Subparsers &Subparsers::help(string help) {
  set_help(*impl_, std::move(help));
  return *this;
}

Subparsers &Subparsers::hide() {
  set_help(*impl_, {});
  return *this;
}

Subparsers &Subparsers::metavar(OptionsSpec value) {
  set_metavar(*impl_, std::move(value));
  return *this;
}
ArgumentParser Subparsers::add_parser(OptionsSpec names, string short_desc) {
  return add_parser(std::move(names), optional<string>{std::move(short_desc)});
}


ArgumentParser Subparsers::add_parser(OptionsSpec names, optional<string> short_desc) {
  ArgumentParser parser;

  size_t index = impl_->parsers.size();
  impl_->parsers.emplace_back();
  auto &entry = impl_->parsers.back();
  entry.parser = parser;
  entry.names = std::move(names.spec);
  entry.short_desc = std::move(short_desc);
  if (entry.names.empty())
    throw std::logic_error("At least one name must be specified");

  for (auto const &name : entry.names) {
    if (!impl_->parser_map.emplace(name, index).second)
      throw std::logic_error("Duplicate parser name: " + name);
  }

  return parser;
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
                   vector<detail::ArgumentImpl const *> const &actions,
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
      vector<detail::ArgumentImpl const *> optionals, positionals;
      for (auto a : actions) {
        if (a->is_positional())
          positionals.push_back(a);
        else
          optionals.push_back(a);
      }

      vector<detail::ArgumentImpl const *> all_actions = optionals;
      all_actions.insert(all_actions.end(), positionals.begin(), positionals.end());

      // Sort all option arguments before positional arguments

      // First try to format as a single line

      auto all_usage = _format_actions_usage(all_actions);

      // see if it all fits on one line
      usage = *prefix + prog;

      // single-line usage for optionals and positionals
      string action_usage =
          join_if(" ", all_usage, [](auto const & x)->decltype(auto) { return x; }, [](auto const &x) { return !x.empty(); });

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

  string format_metavar(detail::ArgumentImpl const *action, size_t i) {
    if (auto subimpl = dynamic_cast<detail::SubparsersArgumentImpl const *>(action)) {
      if (!subimpl->metavar.empty())
        return subimpl->metavar[0];
      return '{' + join(",", subimpl->parsers, [](auto const &p) { return p.names[0]; }) + '}';
    }
    return action->format_metavar(i);
  }

  string _format_args(detail::ArgumentImpl const *action, bool force_required = false) {

    auto nargs = action->nargs;
    if (force_required) {
      switch (nargs) {
      case OPTIONAL:
        nargs = Nargs(1);
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

  string format_action_invocation(detail::ArgumentImpl const *action) {
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
  void update_action_max_length(detail::ArgumentImpl const *action) {
    if (action->is_help_suppressed())
      return;

    // arguments are always indented by 1 level
    action_max_length = std::max(action_max_length,
                                 int(format_action_invocation(action).size()) + indent_increment);

    if (auto sub_act = dynamic_cast<detail::SubparsersArgumentImpl const *>(action)) {
      for (auto const &p : sub_act->parsers) {
        if (p.short_desc) {
          auto s = p.format_invocation();
          size_t len = s.size() + indent_increment * 2;
          action_max_length = std::max(action_max_length, int(len));
        }
      }
    }
  }

  vector<string> _format_actions_usage(vector<detail::ArgumentImpl const *> const &actions) {
    vector<string> result;

    // map from mutex group to number of non-suppressed actions
    // only groups with at least one non-suppressed action will be present, so 0 is never valid
    std::unordered_map<MutuallyExclusiveGroup::Impl *,size_t> mutex_group_shown_count;

    auto get_part = [&](detail::ArgumentImpl const *action, bool in_mutex_group) {
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

          string s = join_if(" | ",
                             simple_range<detail::ArgumentImpl const * const *>(&actions[action_i], &actions[end_action_i]),
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

  void print_action(std::ostream &os, detail::ArgumentImpl const *action) {

    if (action->is_help_suppressed())
      return;

    auto action_header = format_action_invocation(action);
    print_action_str(os, action_header, *action->help);

    if (auto sub_act = dynamic_cast<detail::SubparsersArgumentImpl const *>(action)) {
      string extra_indent(indent_increment, ' ');
      for (auto const &p : sub_act->parsers) {
        if (p.short_desc) {
          print_action_str(os, extra_indent + p.format_invocation(), *p.short_desc);
        }
      }
    }

    // FIXME: expand help
  }
};

void ArgumentParser::print_usage(std::ostream &os, int width) const {

  HelpFormatterParameters params;
  params.width = width;
  params.prog = impl_->prog;
  HelpFormatter formatter(params);

  vector<detail::ArgumentImpl const *> actions;
  for (auto const &a : impl_->actions)
    actions.push_back(a.get());

  formatter.print_usage(formatter.os, impl_->usage, actions, {});
  os << formatter.trim();
}


void ArgumentParser::print_help(std::ostream &os, int width) const {
  propagate_prog_name(impl_.get());

  HelpFormatterParameters params;
  params.prog = impl_->prog;
  params.width = width;
  HelpFormatter formatter(params);

  vector<detail::ArgumentImpl const *> actions;
  for (auto const &a : impl_->actions) {
    formatter.update_action_max_length(a.get());
    actions.push_back(a.get());
  }

  formatter.print_usage(formatter.os, impl_->usage, actions, {});

  formatter.print_text(formatter.os, impl_->description);

  auto do_group = [&] (ArgumentGroup::Impl const *group) {
    HelpFormatter::Section section(formatter, formatter.os, group->title);
    formatter.print_text(section, group->description);
    for (auto action : group->actions)
      formatter.print_action(section, action);
  };

  do_group(&impl_->positional_group);
  do_group(&impl_->optional_group);
  for (auto const &group : impl_->groups) {
    do_group(group.get());
  }

  formatter.print_text(formatter.os, impl_->epilog);

  os << formatter.trim();
}

string ArgumentParser::help_string(int width) const {
  std::ostringstream ostr;
  print_help(ostr, width);
  return ostr.str();
}

string ArgumentParser::usage_string(int width) const {
  std::ostringstream ostr;
  print_usage(ostr, width);
  return ostr.str();
}

namespace detail {
void ArgumentParserImpl::copy_from(ArgumentParserImpl *parent) {
  if (parent->has_negative_number_options)
    has_negative_number_options = true;

  std::unordered_map<ArgumentGroup::Impl *,ArgumentGroup::Impl *> group_map;
  std::unordered_map<MutuallyExclusiveGroup::Impl *,MutuallyExclusiveGroup::Impl *> mutex_group_map;

  group_map[&parent->positional_group] = &this->positional_group;
  group_map[&parent->optional_group] = &this->optional_group;
  group_map[nullptr] = nullptr;

  for (auto const &group : parent->groups) {
    auto x = group->clone();
    x->parent = this;
    group_map[group.get()] = x.get();
    this->groups.push_back(std::move(x));
  }

  for (auto const &mutex_group : parent->mutex_groups) {
    auto x = mutex_group->clone();
    x->parent = this;
    x->parent_group = group_map[mutex_group->parent_group];
    mutex_group_map[mutex_group.get()] = x.get();
    this->mutex_groups.push_back(std::move(x));
  }

  for (auto const &action : parent->actions) {
    auto x = action->clone();
    register_option_strings(this, x.get());
    if (action->mutex_group)
      mutex_group_map.at(action->mutex_group)->register_argument_impl(x.get());
    else if (action->group)
      group_map.at(action->group)->register_argument_impl(x.get());
    else
      register_argument_impl(x.get());
    actions.push_back(std::move(x));
  }
}
}


ArgumentParser &ArgumentParser::parent(ArgumentParser const &parent) {
  impl_->copy_from(parent.impl_.get());
  return *this;
}


}
}
