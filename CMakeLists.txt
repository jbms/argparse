cmake_minimum_required(VERSION 2.8)
project(jbms-argparse CXX)

# This has to happen after the project command, otherwise CMAKE_CXX_FLAGS_* won't be set properly.
if (NOT CMAKE_BUILD_TYPE)
  message(STATUS "No build type selected, default to Release")
  set(CMAKE_BUILD_TYPE "Release")
endif()


list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

## Required for tests
#find_package(Boost COMPONENTS unit_test_framework REQUIRED)

# Add gtest
include(ExternalProject)
ExternalProject_Add(
  googletest-${PROJECT_NAME}
  PREFIX ${CMAKE_CURRENT_BINARY_DIR}/third_party
  SVN_REPOSITORY https://googletest.googlecode.com/svn/tags/release-1.7.0
  SVN_REVISION -r 704
  #TIMEOUT 10
  #PATCH_COMMAND svn patch ${CMAKE_SOURCE_DIR}/gtest.patch ${CMAKE_BINARY_DIR}/ThirdParty/src/googletest
  UPDATE_COMMAND ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
  TEST_COMMAND "")




set (${PROJECT_NAME}_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src")
set (${PROJECT_NAME}_INCLUDE_DIRS
  "${${PROJECT_NAME}_INCLUDE_DIR}"
  )

add_library(jbms-argparse src/jbms/argparse.cpp)
target_include_directories(jbms-argparse PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/src)

#### Build tests
enable_testing()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=gnu++1y -ggdb -Wall -Werror -Wconversion")

set(gtest_dir ${CMAKE_CURRENT_BINARY_DIR}/third_party/src/googletest-${PROJECT_NAME})

include_directories(${${PROJECT_NAME}_INCLUDE_DIRS}
  )



set(generated_tests "${CMAKE_CURRENT_BINARY_DIR}/generated_test_cases.hpp")

add_custom_command(OUTPUT "${generated_tests}"
  COMMAND python3 "${CMAKE_CURRENT_SOURCE_DIR}/test/convert_testcase.py" > "${generated_tests}"
  DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/test/convert_testcase.py"
  )

set(test_name test_argparse)
add_executable("${test_name}"
  test/test_argparse.cpp
  ${gtest_dir}/src/gtest-all.cc
  ${generated_tests}
  )

set_property(
  SOURCE ${gtest_dir}/src/gtest-all.cc
  APPEND_STRING
  PROPERTY COMPILE_FLAGS
  " -Wno-conversion -Wno-missing-field-initializers -pthread")


target_include_directories(${test_name} PRIVATE
  ${gtest_dir}/include
  ${gtest_dir}
  ${CMAKE_CURRENT_BINARY_DIR}
  )

add_custom_command(OUTPUT ${gtest_dir}/src/gtest-all.cc
  DEPENDS googletest-${PROJECT_NAME})

target_link_libraries("${test_name}" jbms-argparse
  #${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
  pthread
  )
#set_property(TARGET "${test_name}" PROPERTY COMPILE_DEFINITIONS BOOST_TEST_MAIN BOOST_TEST_DYN_LINK)

add_test("${test_name}" "${test_name}")

#add_executable("example" "example/example.cpp")
#target_link_libraries("example" jbms-argparse)
  
#### Install headers
set(header_install_dir "include/${PROJECT_NAME}")

install(DIRECTORY "${${PROJECT_NAME}_INCLUDE_DIR}" DESTINATION "${header_install_dir}")


set(config_PATH_VARS ${PROJECT_NAME}_INCLUDE_DIR)
set(config_REL_PATH "${header_install_dir}")

### Documentation
find_package(Doxygen)
if(DOXYGEN_FOUND)
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
  add_custom_target(jbms-argparse-doc ALL
    ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    SOURCES ${PROJECT_BINARY_DIR}/Doxyfile
    COMMENT "Generating API documentation with Doxygen" VERBATIM
    )
  set (${PROJECT_NAME}_HTML_DIR "${CMAKE_CURRENT_BINARY_DIR}/html")
  set (${PROJECT_NAME}_DOXYGEN_TAGFILE "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.doctag")
  set(html_install_dir "doc/${PROJECT_NAME}/html")
  set(tag_install_file "doc/${PROJECT_NAME}/${PROJECT_NAME}.doctag")
  install(DIRECTORY "${${PROJECT_NAME}_HTML_DIR}" DESTINATION "${html_install_dir}")
  install(FILES "${${PROJECT_NAME}_DOXYGEN_TAGFILE}" DESTINATION "${tag_install_file}")
  set(config_PATH_VARS ${config_PATH_VARS} ${PROJECT_NAME}_HTML_DIR ${PROJECT_NAME}_DOXYGEN_TAGFILE)
  set(config_REL_PATH ${config_REL_PATH} "${html_install_dir}" "${tag_install_file}")

endif(DOXYGEN_FOUND)

#### Generate CMake modules for importing this package
include(jbms-WriteConfig)
write_config_file(
  TEMPLATE "${CMAKE_CURRENT_SOURCE_DIR}/config.cmake.in"
  PATH_VARS ${config_PATH_VARS}
  REL_PATH ${config_REL_PATH})
