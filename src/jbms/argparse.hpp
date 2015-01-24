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

enum NargsValue : int {
  OPTIONAL = -1,
  ZERO_OR_MORE = -2,
  ONE_OR_MORE = -3,
  PARSER = -4,
  REMAINDER = -5,
  NARGS_INVALID = -6,
};

struct Nargs {
  NargsValue value;
  operator NargsValue () const { return value; }

  Nargs(NargsValue value) : value(value) {}
  Nargs(int count) : value(NargsValue(count)) {
    if (count < 0)
      throw std::invalid_argument("Explicit number of arguments must be positive");
  }
};


/**
 * Returns a representation of a string in string literal syntax
 **/
string repr(string_view s);

class ArgumentParser;
class ArgumentGroup;
class MutuallyExclusiveGroup;
class Subparsers;
class ArgumentContainer;

class GenericArgument;

template <class Value>
class Argument;

class Result : public std::unordered_map<string,any> {
public:

  using std::unordered_map<string,any>::unordered_map;

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
    return get<Value>(a.dest());
  }
};


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
 * \param help If equal to nullopt, then the argument is suppressed from the help and usage messages.  In contrast, an empty string will result in the argument being displayed without a help message.
 **/
void set_help(GenericArgument &arg, OptionalString help);
void set_nargs(GenericArgument &arg, Nargs nargs);
void set_required(GenericArgument &arg, bool value);
void set_default_value(GenericArgument &arg, any value);

/**
 * Handlers receive the entire result map as a parameter for greater generality.
 * In most cases a handler will only update result[dest].  Subparsers will update multiple fields, however.
 **/
using Handler = std::function<void (ArgumentParser const &parser,
                                    Result &result,
                                    string const &dest,
                                    vector<string_view> values)>;

using CopyHandler = std::function<Handler (GenericArgument &arg, Handler const &handler)>;

/**
 * Specifies the handler, along with a function to copy it if the argument is cloned (rather than just using the copy constructor).
 *
 * This is useful if the handler contains a reference to the underlying ArgumentParser.
 **/
// If no copy_handler is specified, the handler will just be copy-constructed
void set_handler(GenericArgument &arg, Handler handler, CopyHandler copy_handler = {});
Handler &get_handler(GenericArgument &arg);

void set_metavar(GenericArgument &arg, OptionsSpec value);

/**
 * \brief Specifies a logical sub-command to be shown in the help message.
**/
void add_choice_help(GenericArgument &arg, string name, string invocation, OptionalString help = {});

string const &get_dest(GenericArgument &arg);

ArgumentParser get_parser(GenericArgument &arg);
ArgumentGroup get_group(GenericArgument &arg);


template <class Value = void>
class Argument {
  shared_ptr<GenericArgument> generic;
public:
  Argument(shared_ptr<GenericArgument> generic) : generic(generic)
  {}

  string const &dest() const{ return get_dest(*generic); }

  Argument help(std::string help) const {
    set_help(*generic, std::move(help));
    return *this;
  }

  Argument hide() const {
    set_help(*generic, {});
    return *this;
  }

  // Only instantiate when called.
  // This is invalid for Value == void
  template <class ValueT = Value>
  Argument default_value(std::common_type_t<ValueT> value) const {
    set_default_value(*generic, std::move(value));
    return *this;
  }

  Argument required(bool value) const {
    set_required(*generic, value);
    return *this;
  }

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

template <class T>
T default_single_converter(string_view s) {
  return detail::lexical_cast<detail::remove_optional_t<T>>(s);
}

template <class T>
vector<T> default_multi_converter(vector<string_view> const &s) {
  vector<T> result;
  for (auto x : s)
    result.push_back(default_single_converter<T>(x));
  return result;
}

template <class T>
class SingleConverter {
public:
  using value_type = T;
  std::function<T (string_view s)> convert;
  OptionalString metavar;

  /**
   * Default converter
   **/
  SingleConverter() : convert(&default_single_converter<T>) {}

  /**
   * Construct from a conversion function
   **/
  SingleConverter(std::function<T (string_view)> convert,
                  OptionalString metavar = {})
    : convert(std::move(convert)), metavar(std::move(metavar))
  {}

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
  MultiConverter(Convert convert,
                 OptionsSpec metavar = {})
    : convert(std::move(convert)), metavar(std::move(metavar))
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
  Convert convert;
  OptionsSpec metavar;

  /**
   * \brief Default converter using boost::lexical_cast for each element
   **/
  MultiConverter() : convert(&default_multi_converter<element_type>) {}

  /**
   * Construct from a conversion function
   **/
  MultiConverter(Convert convert,
                 OptionsSpec metavar = {})
    : convert(std::move(convert)), metavar(std::move(metavar))
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
  MultiConverter(std::function<element_type (string_view)> convert,
                 OptionalString metavar = {})
    : MultiConverter(SingleConverter<element_type>(std::move(convert), std::move(metavar)))
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

namespace action {

template <class T = string>
Argument<T> store_single(shared_ptr<GenericArgument> generic, SingleConverter<T> convert = {}) {
  set_nargs(*generic, 1);
  set_metavar(*generic, std::move(convert.metavar));
  set_handler(*generic,
              [conv = std::move(convert.convert)](ArgumentParser const &, Result &result, string const &dest, vector<string_view> const & values) {
    assert(values.size() == 1);
    result[dest] = conv(values.at(0));
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
                                                                                      string const &dest,
                                                                                      vector<string_view> const & values) {
        auto &a = result[dest];
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
                  ArgumentParser const &, Result & result, string const & dest, vector<string_view> const & values) {
    result[dest] = conv(values);
  });
  return Argument<T>(std::move(generic));
}

template <class T = bool>
auto store_flag(shared_ptr<GenericArgument> generic, T value) {
  set_nargs(*generic, 0);
  set_handler(*generic,
              [value = std::move(value)](
                  ArgumentParser const &, Result & result, string const & dest, vector<string_view> const & values) {
    assert(values.size() == 0);
    (void)values;
    result[dest] = value;
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
                  ArgumentParser const &, Result & result, string const & dest, vector<string_view> const & values) {
    assert(values.size() == 1);
    any_cast<Value &>(result[dest]).emplace_back(conv(values.at(0)));
  });
  return Argument<Value>(std::move(generic));
}

template <class T>
Argument<vector<T>> append_flag(shared_ptr<GenericArgument> generic, T value) {
  using Value = std::vector<T>;

  set_nargs(*generic, 0);
  set_default_value(*generic, Value());
  set_handler(*generic, [value = std::move(value)](ArgumentParser const &, Result &result, string const &dest, vector<string_view> const & values) {
    assert(values.size() == 0);
    (void)values;
    any_cast<Value &>(result[dest]).emplace_back(value);
  });
  return Argument<Value>(std::move(generic));
}

template <class Value = size_t>
Argument<Value> count(shared_ptr<GenericArgument> generic) {
  set_nargs(*generic, 0);
  set_default_value(*generic, Value());
  set_handler(*generic, [](ArgumentParser const &, Result &result, string const &dest, vector<string_view> const &values) {
    assert(values.size() == 0);
    (void)values;
    any_cast<Value &>(result[dest]) += Value(1);
  });
  return Argument<Value>(std::move(generic));
}

}

struct HelpFormatterParameters {
  HelpFormatterParameters() = default;
  HelpFormatterParameters(int width) : width(width) {}
  string prog;
  int indent_increment = 2;
  int max_help_position = 24;
  int width = 0;
  int min_text_width = 11;
};



class Subparsers {
public:

  Subparsers() = default;

  /**
   * \param short_desc  Short description text to show in help message.  The parser will not be listed if nullopt is specified.
   **/
  ArgumentParser add_parser(OptionsSpec names, OptionalString short_desc = {}) const;

  Subparsers help(string help) const;
  Subparsers hide() const;
  Subparsers metavar(OptionsSpec value) const;

private:
  shared_ptr<GenericArgument> generic;
  Subparsers(shared_ptr<GenericArgument> generic) : generic(std::move(generic)) {}
  friend class ArgumentContainer;
};


class ArgumentContainer {
public:
  virtual shared_ptr<GenericArgument> add_generic(OptionsSpec spec) const = 0;

  /**
   * Returns the parser that this container is a part of.
   **/
  virtual ArgumentParser get_parser() const = 0;


  template <class Value = string>
  Argument<Value> add(OptionsSpec spec, SingleConverter<Value> conv = {}) const {
    return action::store_single<Value>(add_generic(std::move(spec)), std::move(conv));
  }

  template <class Value = optional<string>>
  Argument<Value> add_optional(OptionsSpec spec, Value missing = {}, SingleConverter<Value> conv = {}) const {
    return action::store_optional<Value>(add_generic(std::move(spec)), std::move(missing), std::move(conv));
  }

  template <class Value = vector<string>>
  Argument<Value> add_multi(OptionsSpec spec, Nargs nargs, MultiConverter<Value> conv = {}) const {
    return action::store_multi<Value>(add_generic(std::move(spec)), nargs, std::move(conv));
  }

  /**
   * \brief Stores the specified constant.
   **/
  template <class Value = bool>
  Argument<Value> add_flag(OptionsSpec spec, Value value = true) const {
    return action::store_flag<Value>(add_generic(std::move(spec)), std::move(value));
  }

  template <class Value = bool>
  Argument<vector<Value>> add_append_flag(OptionsSpec spec, Value value = true) const {
    return action::append_flag<Value>(add_generic(std::move(spec)), std::move(value));
  }

  template <class Value = string>
  Argument<vector<Value>> add_append(OptionsSpec spec, SingleConverter<Value> conv = {}) const {
    return action::append(add_generic(std::move(spec)), std::move(conv));
  }

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

  Subparsers add_subparsers(OptionsSpec spec = {}) const;
};


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


  ArgumentGroup title(string title) const;
  ArgumentGroup description(string description) const;


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
   * Returns a mutually exclusive group that shares mutual exclusion with this group but places arguments into the specified ArgumentGroup.
   **/
  MutuallyExclusiveGroup operator[](ArgumentGroup group) const { return MutuallyExclusiveGroup(this->impl, group); }

  shared_ptr<Impl> impl;
  ArgumentGroup group;
};

class ArgumentParser : public ArgumentContainer {
public:
  ArgumentGroup add_group(string title = {}, string description = {});

  /**
   * Exits with status 1 on failure.
   **/
  Result parse(int argc, char **argv) const;

  /**
   * Exits with status 1 on failure.
   **/
  Result parse(vector<string_view> args) const;

  /**
   * Exits with status 1 on failure.
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
   * Exits with status 1 on failure.
   **/
  Result try_parse(vector<string_view> args) const;

  /**
   * \throws ParseError on failure
   **/
  void try_parse(Result &result, vector<string_view> args) const;


  void print_usage(std::ostream &, HelpFormatterParameters const &params = {}) const;
  string usage_string(HelpFormatterParameters const &params = {}) const;

  void print_help(std::ostream &, HelpFormatterParameters const &params = {}) const;
  string help_string(HelpFormatterParameters const &params = {}) const;

  ArgumentParser prog(string prog_name) const;

  /**
   * \brief Sets the prog name to equal super_parser's prog name + " " + prog_name
   *
   * This relationship will hold dynamically even if the super parser's prog is set at a later time.
   * A weak reference to the super parser will be held.
   **/
  ArgumentParser prog_as_subparser(ArgumentParser const &super_parser, string prog_name) const ;

  ArgumentParser description(string s) const;
  ArgumentParser epilog(string s) const;
  ArgumentParser usage(string s) const;

  ArgumentParser prefix_chars(string value) const;

  MutuallyExclusiveGroup add_mutually_exclusive_group(bool required = false) const;

  /**
   * \brief Copies the arguments and groups defined in another parser
   **/
  ArgumentParser parent(ArgumentParser const &parent) const;


  virtual shared_ptr<GenericArgument> add_generic(OptionsSpec spec) const override;
  virtual ArgumentParser get_parser() const override;


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

ArgumentParser make_parser();

class ParseError : public std::invalid_argument {
  ArgumentParser parser_;
public:
  ParseError(ArgumentParser parser_, std::string const &message)
    : std::invalid_argument(message), parser_(parser_)
  {}
  ArgumentParser parser() const { return parser_; }

  /**
   * Prints the usage method, the error message, and then exits the program with code 1.
   **/
  [[noreturn]] void handle() const;
};


} // namespace jbms::argparse
} // namespace jbms


#endif /* HEADER GUARD */
