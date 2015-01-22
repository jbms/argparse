Powerful command-line argument parsing library for C++, inspired by Python's excellent [argparse](https://docs.python.org/3/library/argparse.html) library

This library provides almost all of the functionality of the Python argparse package, and also provides a similar API, albeit with a modern C++ flavor.

The code is licensed under the highly-permissive [Boost Software License](http://www.boost.org/LICENSE_1_0.txt).

# Features:
- Lightweight (single header + single source file, minimal dependencies, limited template use for fast compilation)
- Supports almost all of the functionality of the Python argparse package
- Automatically generates highly-informative help and usage messages
- Handles both optional and positional arguments
- Subparser support, which provides builtin support for git/svn-style sub-commands
- Supports flexible option syntax, including both Unix-style short options (`-o`) and GNU-style long options (`--option`) as accepted by `getopt` and `getopt_long`, as well as other option styles.
  - Multiple single-character options that don't take an argument can be specified together, e.g. `-a -r -p` can be specified as `-arp`
  - Unique abbreviations of multiple-character options can be used
  - A single argument to an option can be specified either as `--long=value` or as `--long value`.

# Dependencies:
- Makes use of C++14/C++1y features, and therefore requires GCC >= 4.9 or Clang >= 3.5.
  - `std::experimental::optional`
  - `std::experimental::string_view`
- [Boost](http://www.boost.org/) >= 1.57
  - Boost.Any for storage of parsing results
  - Boost.LexicalCast for argument type conversion
  - Boost.Core for type printing in error messages
