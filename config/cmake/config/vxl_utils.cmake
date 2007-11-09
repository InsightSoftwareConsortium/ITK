# CMake 2.4 is not currently required, but when it is we can remove
#   CMakeConfigurableFile.in from config/cmake/Modules/NewCmake
#   (This is used in GENERATE_TEST_INCLUDE)
# CMAKE_MINIMUM_REQUIRED(VERSION 2.4)
IF(CMAKE_MINIMUM_REQUIRED_VERSION GREATER "2.3")
  MESSAGE(SEND_ERROR
    "The file CMakeConfigurableFile.in can be removed\n"
    "from config/cmake/Modules/NewCMake/ now!\n"
    "It should be available from CMake distribution. (miguelfv)")
ENDIF(CMAKE_MINIMUM_REQUIRED_VERSION GREATER "2.3")

#
# INSTALL_NOBASE_HEADER_FILES(prefix file file file ...)
# Will create install rules for those files of the list
# which are headers (.h or .txx).
# If .in files are given, the .in extension is removed.
#

MACRO(INSTALL_NOBASE_HEADER_FILES prefix)
FOREACH(file ${ARGN})
  IF(${file} MATCHES "\\.(h|txx)(\\.in)?$")
    STRING(REGEX REPLACE "\\.in$" "" install_file ${file})
    GET_FILENAME_COMPONENT(dir ${install_file} PATH)
    INSTALL_FILES(${prefix}/${dir} FILES ${install_file})
  ENDIF(${file} MATCHES "\\.(h|txx)(\\.in)?$")
ENDFOREACH(file ${filelist})
ENDMACRO(INSTALL_NOBASE_HEADER_FILES)

#---------------------------------------------------------------------
# GENERATE_TEST_DRIVER(<lib> <sources> [<lib1> <lib2> ...])
#
# - lib     : name of library being tested (e.g., vil, vul, etc.)
# - sources : variable containing the list of source files
# - libN    : libraries to link to
#
# If a test needs to be passed some arguments, you can provide them in
# a cmake variable named by the tests filename appended with '_args'
# (e.g., test_arg_args).
#
# Example usage:
#   SET(vil_test_sources
#     ...
#     test_stream.cxx
#     ...
#   )
#   SET(test_stream_args ${CMAKE_CURRENT_SOURCE_DIR}/file_read_data)
#   GENERATE_TEST_DRIVER(vil vil_test_sources vil vpl vul testlib vcl)
#---------------------------------------------------------------------
MACRO(GENERATE_TEST_DRIVER LIB SOURCES)
  CREATE_TEST_SOURCELIST(test_driver_sources ${LIB}_test_driver.cxx
    ${${SOURCES}}
  )

  ADD_EXECUTABLE(${LIB}_test_driver ${test_driver_sources})
  # ***** what if ARGN is empty?
  TARGET_LINK_LIBRARIES(${LIB}_test_driver ${ARGN})

  SET(tests_to_run ${test_driver_sources})
  REMOVE(tests_to_run ${LIB}_test_driver.cxx)

  FOREACH(test ${tests_to_run})
    GET_FILENAME_COMPONENT(test_name ${test} NAME_WE)
    ADD_TEST(${LIB}_${test_name}
      ${EXECUTABLE_OUTPUT_PATH}/${LIB}_test_driver
      ${test_name}
      ${${test_name}_args}
    )
  ENDFOREACH(test)
ENDMACRO(GENERATE_TEST_DRIVER)

#---------------------------------------------------------------------
# GENERATE_TEST_INCLUDE(<lib> <sources> <prefix>)
#
# - lib     : name of library (e.g., vil, vil_io, pbl, etc.)
# - sources : variable containing the list of library sources
# - prefix  : prefix used in the include statement
#
# Example usage:
#   GENERATE_TEST_INCLUDE(vil_io vil_io_sources "vil/io/")
#---------------------------------------------------------------------
MACRO(GENERATE_TEST_INCLUDE LIB SOURCES PREFIX)
  SET(CMAKE_CONFIGURABLE_FILE_CONTENT "/* */\n")
  FOREACH(FILE ${${SOURCES}})
    GET_FILENAME_COMPONENT(FILE_EXT ${FILE} EXT)
    IF(FILE_EXT STREQUAL ".h")
      SET(CMAKE_CONFIGURABLE_FILE_CONTENT
          "${CMAKE_CONFIGURABLE_FILE_CONTENT}#include <${PREFIX}${FILE}>\n#include <${PREFIX}${FILE}>\n")
    ENDIF(FILE_EXT STREQUAL ".h")
  ENDFOREACH(FILE)

  SET(CMAKE_CONFIGURABLE_FILE_CONTENT
      "${CMAKE_CONFIGURABLE_FILE_CONTENT}\n\nint main(){return 0;}\n")

  #CONFIGURE_FILE("${CMAKE_ROOT}/Modules/CMakeConfigurableFile.in"
  CONFIGURE_FILE("${MODULE_PATH}/NewCMake/CMakeConfigurableFile.in"
                 "${CMAKE_CURRENT_BINARY_DIR}/test_include.cxx"
                 @ONLY IMMEDIATE)

  ADD_EXECUTABLE(${LIB}_test_include ${CMAKE_CURRENT_BINARY_DIR}/test_include.cxx)
  TARGET_LINK_LIBRARIES(${LIB}_test_include ${LIB})
ENDMACRO(GENERATE_TEST_INCLUDE)
