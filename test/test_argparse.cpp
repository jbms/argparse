#include "jbms/argparse.hpp"
#include <typeinfo>
#include <typeindex>
#include <sstream>

#include <gtest/gtest.h>
#include <boost/core/demangle.hpp>


namespace std {
namespace experimental {

template <class T>
void PrintTo(optional<T> const &x, ::std::ostream *os) {
  if (!x)
    *os << "<disengaged-optional>";
  else
    *os << ::testing::PrintToString(*x);
}
}
}

namespace jbms {
namespace argparse {

namespace {

::testing::AssertionResult check_help_eq(string const &expected, string const &actual) {
  if (expected == actual)
    return ::testing::AssertionSuccess();
  size_t mismatch_pos = std::mismatch(expected.begin(), expected.end(), actual.begin(), actual.end()).first - expected.begin();
  ::testing::AssertionResult msg = ::testing::AssertionFailure();
  msg << "Mismatch starts at position " << mismatch_pos << '\n';
  msg << "expected: " << ::testing::PrintToString(expected.substr(mismatch_pos)) << '\n';
  msg << "actual:   " << ::testing::PrintToString(actual.substr(mismatch_pos)) << '\n';
  return msg;
}

#define ASSERT_HELP_EQ(expected, actual) \
  ASSERT_TRUE(check_help_eq(expected, actual)) \
  /**/


using namespace std::literals::string_literals;

struct AnyMethods {
  struct Methods {
    bool (*equal)(any const &, any const &);
    string (*repr)(any const &);
    any (*get_canonical)(any const &);
  };
  std::unordered_map<std::type_index,Methods> methods;

  template <class T>
  void add() {
    Methods methods;
    methods.equal = [](any const &a, any const &b) {
      return any_cast<T const &>(a) == any_cast<T const &>(b);
    };
    methods.repr = [](any const &a) {
      return ::testing::PrintToString(any_cast<T const &>(a));
    };
    methods.get_canonical = [](any const &a) {
      return a;
    };

    this->methods[typeid(T)] = methods;
  }

  AnyMethods() {
    add<bool>();
    add<long>();
    add<short>();
    add<int>();
    add<optional<int>>();
    add<string>();
    add<optional<string>>();
    add<string_view>();
    add<double>();
    add<size_t>();
    add<float>();
    add<const char *>();
    add<vector<string>>();

    methods[typeid(string_view)].get_canonical = [](any const &a) -> any {
      return string(any_cast<string_view>(a));
    };

    methods[typeid(const char *)].get_canonical = [](any const &a) -> any {
      return string(any_cast<const char *>(a));
    };
  }

  Methods const &get_methods(any const &a) const {
    auto it = methods.find(a.type());
    if (it == methods.end())
      throw std::logic_error(string("unsupported type: ") + boost::core::demangle(a.type().name()));

    return it->second;
  }

  bool equal(any const &a, any const &b) const {
    if (a.type() != b.type())
      return false;

    return get_methods(a).equal(a, b);
  }

  string repr(any const &a) const {
    return get_methods(a).repr(a);
  }

  any get_canonical(any const &a) const {
    return get_methods(a).get_canonical(a);
  }
};

static AnyMethods any_methods;

Result get_canonical(Result const &orig) {
  Result result;
  for (auto const &p : orig) {
    result[p.first] = any_methods.get_canonical(p.second);
  }
  return result;
}

std::vector<string_view> get_string_view_vec(std::vector<std::string> const &a) {
  std::vector<string_view> result;
  for (auto const &x : a)
    result.push_back(x);
  return result;
}

::testing::AssertionResult results_equal(Result const &a, Result const &b) {
  ::testing::AssertionResult msg = ::testing::AssertionFailure();
  msg << "parse result mismatch\n";
  bool has_failed = false;
  for (auto const &p : a) {
    auto it = b.find(p.first);
    if (it == b.end()) {
      msg << p.first << ": " << any_methods.repr(p.second) << " vs <missing>\n";
      has_failed = true;
    } else if (!any_methods.equal(p.second, it->second)) {
      msg << p.first << ": " << any_methods.repr(p.second) << " vs " << any_methods.repr(it->second) << '\n';
      has_failed = true;
    }
  }

  for (auto const &p : b) {
    auto it = b.find(p.first);
    if (it == b.end()) {
      msg << p.first << ": <missing> vs " << any_methods.repr(p.second) << "\n";
      has_failed = true;
    }
  }

  if (has_failed)
    return msg;
  return ::testing::AssertionSuccess();
}

struct ParserTestCase : public ::testing::Test {

  using InitFunction = std::function<void (ArgumentParser &)>;
  InitFunction init_function;

  ParserTestCase() {
    init_function = [](ArgumentParser &) {};
  }

  void init(InitFunction x) {
    init_function = x;
  }

  using ArgsFunction = std::function<void (detail::ArgumentContainer &)>;
  std::vector<ArgsFunction> args_functions;
  void args(ArgsFunction x) {
    args_functions.push_back(x);
  }

  template <class F>
  void for_each_arg_method(F f) {

    // no groups
    {
      ArgumentParser parser;
      init_function(parser);

      for (auto const &a : args_functions) {
        a(parser);
      }

      f(parser);
    }

    // one group
    {
      ArgumentParser parser;
      init_function(parser);

      auto group = parser.add_group("foo");

      for (auto const &a : args_functions) {
        a(group);
      }

      f(parser);
    }

    // many groups
    {
      ArgumentParser parser;
      init_function(parser);

      size_t i = 0;
      for (auto const &a : args_functions) {
        auto group = parser.add_group("foo:" + std::to_string(i++));
        a(group);
      }

      f(parser);
    }
  }

  void failures(std::vector<std::vector<std::string>> examples) {
    for_each_arg_method([&](auto &p) {
        for (auto const &example : examples) {
          SCOPED_TRACE("Testing example: " + ::testing::PrintToString(example));
          ASSERT_THROW(p.try_parse(get_string_view_vec(example)), ParseError);
        }
    });
  }

  void success(std::vector<std::string> args, Result const &expected_orig) {
    SCOPED_TRACE("Testing example: " + ::testing::PrintToString(args));
    Result expected = get_canonical(expected_orig);
    for_each_arg_method([&](auto &p) {
        Result actual;
        ASSERT_NO_THROW((actual = p.try_parse(get_string_view_vec(args))));
        ASSERT_TRUE(results_equal(expected, actual)) << ::testing::PrintToString(args);
      });
  }

};

#include "generated_test_cases.hpp"


} // anon namespace


} // namespace jbms::argparse
} // namespace jbms


int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);

  return RUN_ALL_TESTS();
}
