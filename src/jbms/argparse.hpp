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

enum Nargs : int {
  OPTIONAL = -1,
  ZERO_OR_MORE = -2,
  ONE_OR_MORE = -3,
  PARSER = -4,
  REMAINDER = -5,
  NARGS_INVALID = -6,
};

class ArgumentParser;
class ArgumentGroup;
class MutuallyExclusiveGroup;

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


class OptionsSpec {
public:
  std::vector<std::string> spec;

  OptionsSpec(const char *x) : spec{ std::string(x) } {}
  OptionsSpec(std::string x) : spec{ std::move(x) } {}
  OptionsSpec(string_view x) : spec{ std::string(x) } {}
  OptionsSpec(std::initializer_list<std::string> x) : spec(x) {}
  OptionsSpec(std::vector<std::string> x) : spec(x) {}
};


namespace detail {

class ArgumentImpl;
class SubparsersArgumentImpl;

using Handler = std::function<void (ArgumentParser const &parser,
                                    Result &result,
                                    string const &dest,
                                    vector<string_view> values)>;


void set_help(ArgumentImpl &impl, optional<std::string> help);
void set_nargs(ArgumentImpl &impl, Nargs nargs);
void set_required(ArgumentImpl &impl, bool value);
void set_default_value(ArgumentImpl &impl, any value);

void set_handler(ArgumentImpl &impl, Handler handler);

void set_metavar(ArgumentImpl &impl_, OptionsSpec value);

// void set_metavar(ArgumentImpl &impl_, std::vector<std::string> value);


class ArgumentBase {
protected:
  ArgumentImpl *impl_ = nullptr;
  std::string dest_;

  template <class Value>
  friend class Argument;

public:
  string const &dest() const { return dest_; }

  ArgumentBase() = default;
  ArgumentBase(ArgumentImpl *impl_);
};

}

template <class Value>
class Argument : public detail::ArgumentBase {
public:
  Argument() = default;
  Argument(detail::ArgumentImpl *impl) : detail::ArgumentBase(impl) {}


  Argument &help(std::string help) {
    set_help(*impl_, std::move(help));
    return *this;
  }

  Argument &hide() {
    set_help(*impl_, {});
    return *this;
  }

  // Only instantiate when called.
  // This is invalid for Value == void
  template <class ValueT = Value>
  Argument &default_value(std::common_type_t<ValueT> const &value) {
    set_default_value(*impl_, value);
    return *this;
  }

  Argument &required(bool value) {
    set_required(*impl_, value);
    return *this;
  }

  Argument &metavar(OptionsSpec value) {
    set_metavar(*impl_, std::move(value));
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

namespace action {


template <class T>
struct store_single {
  using value_type = T;

  void initialize(detail::ArgumentImpl &impl) const {
    set_nargs(impl, Nargs(1));
  }

  void operator()(ArgumentParser const &, Result &result, string const &dest, vector<string_view> values) const {
    result[dest] = detail::lexical_cast<T>(std::move(values[0]));
  }
};


template <class T>
struct store_optional {
  using value_type = T;

  value_type missing_value;

  store_optional(value_type missing_value)
    : missing_value(std::move(missing_value)) {}

  void initialize(detail::ArgumentImpl &impl) const {
    set_nargs(impl, OPTIONAL);
  }

  void operator()(ArgumentParser const &, Result &result, std::string const &dest, std::vector<string_view> values) const {
    if (values.empty())
      result[dest] = missing_value;
    else
      result[dest] = value_type(detail::lexical_cast<detail::remove_optional_t<T>>(std::move(values[0])));
  }
};


template <class T>
struct store_multi {
  using value_type = std::vector<T>;
  Nargs nargs;
  store_multi(Nargs nargs)
    : nargs(nargs) {}

  void initialize(detail::ArgumentImpl &impl) const {
    set_nargs(impl, nargs);
  }

  void operator()(ArgumentParser const &, Result &result, std::string const &dest, vector<string_view> values) const {
    std::vector<T> x;
    for (auto &v : values) {
      x.push_back(detail::lexical_cast<T>(std::move(v)));
    }
    result[dest] = x;
  }
};

template <class T>
struct store_flag {
  using value_type = T;
  value_type value;

  store_flag(value_type const &value) : value(value) {}

  void initialize(detail::ArgumentImpl &impl) const {
    set_nargs(impl, Nargs(0));
  }

  void operator()(ArgumentParser const &, Result &result, std::string const &dest, vector<string_view> values) const {
    (void)values;
    result[dest] = value;
  }
};

template <class T>
struct append {
  using value_type = std::vector<T>;

  void initialize(detail::ArgumentImpl &impl) const {
    set_nargs(impl, Nargs(1));
    set_default_value(impl, value_type{});
  }

  void operator()(ArgumentParser const &, Result &result, string const &dest, vector<string_view> values) const {
    any_cast<value_type &>(result[dest]).push_back(detail::lexical_cast<T>(values[0]));
  }
};

template <class T>
struct append_flag {
  using value_type = std::vector<T>;
  T value;

  append_flag(T const &value) : value(value) {}

  void initialize(detail::ArgumentImpl &impl) const {
    set_nargs(impl, Nargs(0));
    set_default_value(impl, value_type{});
  }

  void operator()(ArgumentParser const &, Result &result, string const &dest, vector<string_view> values) const {
    (void)values;
    any_cast<value_type &>(result[dest]).push_back(value);
  }
};


template <class T>
struct count_action {
  using value_type = T;

  void initialize(detail::ArgumentImpl &impl) const {
    set_nargs(impl, Nargs(0));
    set_default_value(impl, value_type{});
  }

  void operator()(ArgumentParser const &, Result &result, std::string const &dest, vector<string_view> values) const {
    (void)values;
    auto &x = result[dest];
    any_cast<value_type &>(x) += value_type(1);
  }
};

}

namespace detail {
class ArgumentContainer;
class ArgumentParserImpl;
}

class Subparsers {
  friend class detail::ArgumentContainer;
public:

  Subparsers() = default;

  /**
   * \param short_desc  Short description text to show in help message.  The parser will not be listed if nullopt is specified.
   **/
  ArgumentParser add_parser(OptionsSpec names, optional<string> short_desc = {});
  ArgumentParser add_parser(OptionsSpec names, string short_desc);

  Subparsers &help(string help);
  Subparsers &hide();
  Subparsers &metavar(OptionsSpec value);

private:
  Subparsers(detail::SubparsersArgumentImpl *impl) : impl_(impl) {}
  detail::SubparsersArgumentImpl *impl_ = nullptr;
};



namespace detail {

class ArgumentContainer {
private:
  detail::ArgumentImpl &make_argument_impl(OptionsSpec spec);
public:
  // for internal use only
  virtual void register_argument_impl(detail::ArgumentImpl *arg_impl) = 0;
  virtual detail::ArgumentParserImpl *get_parser_impl() = 0;

public:
  template <class Action>
  Argument<typename std::decay_t<Action>::value_type> add_generic(OptionsSpec spec, Action &&action) {
    auto &impl = make_argument_impl(std::move(spec));
    action.initialize(impl);
    set_handler(impl, detail::Handler(std::move(action)));
    return { &impl };
  }

  template <class Value = string>
  Argument<Value> add(OptionsSpec spec) {
    return Argument<Value>(add_generic(std::move(spec), action::store_single<Value>{}));
  }

  template <class Value = optional<string>>
  Argument<Value> add_optional(OptionsSpec spec, Value missing = {}) {
    return Argument<Value>(add_generic(std::move(spec), action::store_optional<Value>(missing)));
  }


  template <class Value = std::string>
  Argument<std::vector<Value>> add_multi(OptionsSpec spec, Nargs nargs) {
    return Argument<std::vector<Value>>(add_generic(std::move(spec), action::store_multi<Value>(nargs)));
  }

  /**
   * Stores the specified constant.
   **/
  template <class Value = bool>
  Argument<Value> add_flag(OptionsSpec spec, Value const &value = true) {
    return Argument<Value>(add_generic(std::move(spec), action::store_flag<Value>(value)));
  }

  template <class Value = bool>
  Argument<vector<Value>> add_append_flag(OptionsSpec spec, Value const &value = true) {
    return Argument<vector<Value>>(add_generic(std::move(spec), action::append_flag<Value>(value)));
  }

  template <class Value = string>
  Argument<vector<Value>> add_append(OptionsSpec spec) {
    return Argument<vector<Value>>(add_generic(std::move(spec), action::append<Value>{}));
  }

  template <class Value = size_t>
  Argument<Value> add_count(OptionsSpec spec) {
    return Argument<Value>(add_generic(std::move(spec), action::count_action<Value>{}));
  }

  /**
   * \brief Adds a default option for printing the help message.
   *
   * A short 'h' and long 'help' option will be defined, using the default prefix character.
   *
   * When this option is parsed, the help message will be printed to standard output, and then exit(0) will be called.
   **/
  Argument<void> add_help_option();

  /**
   * \brief Adds an option for printing the help message.
   *
   * When this message is parsed, the help message will be printed to standard output, and then exit(0) will be called.
   **/
  Argument<void> add_help_option(OptionsSpec spec);

  /**
   * \brief Adds an option for printing a simple message and exiting, e.g. for version information.
   *
   * The specified message will be printed to standard output, and then exit(0) will be called.
   **/
  Argument<void> add_print_option(OptionsSpec spec, string message);

  /**
   * \brief Adds a default option for printing version information.
   **/
  Argument<void> add_version_option(string message);

  /**
   * \brief Adds a default option for printing version information.
   **/
  Argument<void> add_version_option(OptionsSpec spec, string message);

  Subparsers add_subparsers(OptionsSpec spec);
  Subparsers add_subparsers();
};

}

/**
 * \brief Simple grouping construct for arguments
 *
 * Groups primarily serve to group arguments for the purpose of printing help information.  Adding an argument to a group just adds it to the underlying \ref ArgumentParser.
 *
 * Groups cannot be nested.
 **/
class ArgumentGroup : public detail::ArgumentContainer {
public:

  ArgumentGroup &title(string title);
  ArgumentGroup &description(string description);

  ArgumentGroup() = default;

  MutuallyExclusiveGroup add_mutually_exclusive_group(bool required = false);

public:
  // for internal use only
  virtual void register_argument_impl(detail::ArgumentImpl *arg_impl) override;
  virtual detail::ArgumentParserImpl *get_parser_impl() override;

public:
  struct Impl;
  ArgumentGroup(Impl *impl)
    : impl_(impl)
  {}

private:
  Impl *impl_ = nullptr;
};

class MutuallyExclusiveGroup : public detail::ArgumentContainer {
public:
  MutuallyExclusiveGroup() = default;

public:
  // for internal use only
  virtual void register_argument_impl(detail::ArgumentImpl *arg_impl) override;
  virtual detail::ArgumentParserImpl *get_parser_impl() override;

public:
  struct Impl;
  MutuallyExclusiveGroup(Impl *impl)
    : impl_(impl)
  {}

public:
  Impl *impl_ = nullptr;
};

class ArgumentParser : public detail::ArgumentContainer {
public:
  ArgumentParser();
  ArgumentParser(ArgumentParser &&other) = default;
  ArgumentParser(ArgumentParser const &other) = default;
  ~ArgumentParser();

  ArgumentParser &operator=(ArgumentParser &&other) = default;
  ArgumentParser &operator=(ArgumentParser const &other) = default;

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


  void print_usage(std::ostream &, int width = 0) const;
  string usage_string(int width = 0) const;

  void print_help(std::ostream &, int width = 0) const;
  string help_string(int width = 0) const;

  ArgumentParser &prog(string prog_name);
  ArgumentParser &description(string s);
  ArgumentParser &epilog(string s);
  ArgumentParser &usage(string s);

  ArgumentParser &prefix_chars(string value);

  MutuallyExclusiveGroup add_mutually_exclusive_group(bool required = false);

  /**
   * \brief Copies the arguments and groups defined in another parser
   **/
  ArgumentParser &parent(ArgumentParser const &parent);

public:
  // for internal use only
  virtual void register_argument_impl(detail::ArgumentImpl *arg_impl) override;
  virtual detail::ArgumentParserImpl *get_parser_impl() override;

public:
  // These members are for internal use only.
  std::shared_ptr<detail::ArgumentParserImpl> impl_;
};

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
