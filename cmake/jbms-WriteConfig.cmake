
function (write_config_file)
  include ( CMakeParseArguments )
  include ( CMakePackageConfigHelpers )

  cmake_parse_arguments (
    "_MAKE_CONFIG"
    ""
    "DESTINATION;TEMPLATE;NAME"
    "TARGETS;PATH_VARS;REL_PATH"
    ${ARGN} )

  if ("${_MAKE_CONFIG_NAME}" STREQUAL "")
    set(_MAKE_CONFIG_NAME "${PROJECT_NAME}")
  endif()

  if ("${_MAKE_CONFIG_DESTINATION}" STREQUAL "")
    set(_MAKE_CONFIG_DESTINATION "share/cmake/Modules")
  endif()

  set(NAME "${_MAKE_CONFIG_NAME}")

  # i) for local use. export() should never used to create the config
  # for the file shipped in a package since it's local.
  configure_package_config_file (
    "${_MAKE_CONFIG_TEMPLATE}"
    "${CMAKE_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
    INSTALL_DESTINATION "${_MAKE_CONFIG_DESTINATION}"
    PATH_VARS ${_MAKE_CONFIG_PATH_VARS} )

  if ("${_MAKE_CONFIG_TARGETS}")
  
    export ( TARGETS ${_MAKE_CONFIG_TARGETS}
      FILE "${CMAKE_BINARY_DIR}/${PROJECT_NAME}Targets.cmake" )

  endif("${_MAKE_CONFIG_TARGETS}")

  export ( PACKAGE ${PROJECT_NAME} )


  # ii) To ship in the package
  # TODO: back variable up
  # TODO: check PATH_VARS matches REL_PATH lenght

  list ( LENGTH _MAKE_CONFIG_PATH_VARS count )

  set ( index 0 )

  while ( index LESS count )
    list ( GET _MAKE_CONFIG_PATH_VARS ${index} _path_var )
    list ( GET _MAKE_CONFIG_REL_PATH ${index} _rel_path )
    set ( ${_path_var} ${_rel_path} )
    math ( EXPR index "${index}+1" )
  endwhile()

  configure_package_config_file (
    "${_MAKE_CONFIG_TEMPLATE}"
    "${CMAKE_BINARY_DIR}/export/Find${PROJECT_NAME}.cmake"
    INSTALL_DESTINATION "${_MAKE_CONFIG_DESTINATION}"
    PATH_VARS ${_MAKE_CONFIG_PATH_VARS} )
  
  if ("${_MAKE_CONFIG_TARGETS}")
    install ( EXPORT export
      DESTINATION ${_MAKE_CONFIG_DESTINATION}
      FILE "${PROJECT_NAME}Targets.cmake" )
  endif ("${_MAKE_CONFIG_TARGETS}")

  install(
    FILES "${CMAKE_BINARY_DIR}/export/Find${PROJECT_NAME}.cmake"
    DESTINATION "${_MAKE_CONFIG_DESTINATION}")

endfunction()
