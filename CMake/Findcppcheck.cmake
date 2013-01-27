# - try to find cppcheck tool
#
# Cache Variables:
# CPPCHECK_EXECUTABLE
#
# Non-cache variables you might use in your CMakeLists.txt:
# CPPCHECK_FOUND
# CPPCHECK_UNUSEDFUNC_ARG
# CPPCHECK_STYLE_ARG
# CPPCHECK_QUIET_ARG
# CPPCHECK_INCLUDEPATH_ARG
# CPPCHECK_FAIL_REGULAR_EXPRESSION
# CPPCHECK_WARN_REGULAR_EXPRESSION
# CPPCHECK_MARK_AS_ADVANCED - whether to mark our vars as advanced even
# if we don't find this program.
#
# Requires these CMake modules:
# FindPackageHandleStandardArgs (known included with CMake >=2.6.2)
#
# Original Author:
# 2009-2010 Ryan Pavlik <rpavlik@iastate.edu> <abiryan@ryand.net>
# http://academic.cleardefinition.com
# Iowa State University HCI Graduate Program/VRAC
#
# Copyright Iowa State University 2009-2010.
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE_1_0.txt or copy at
# http://www.boost.org/LICENSE_1_0.txt)

file(TO_CMAKE_PATH "${CPPCHECK_ROOT_DIR}" CPPCHECK_ROOT_DIR)
set(CPPCHECK_ROOT_DIR
  "${CPPCHECK_ROOT_DIR}"
  CACHE
  PATH
  "Path to search for cppcheck")
mark_as_advanced(CPPCHECK_ROOT_DIR)

# cppcheck app bundles on Mac OS X are GUI, we want command line only
set(_oldappbundlesetting ${CMAKE_FIND_APPBUNDLE})
set(CMAKE_FIND_APPBUNDLE NEVER)

# If we have a custom path, look there first.
if(CPPCHECK_ROOT_DIR)
  find_program(CPPCHECK_EXECUTABLE
    NAMES
      cppcheck
      cli
    PATHS
      "${CPPCHECK_ROOT_DIR}"
    PATH_SUFFIXES
      cli
    NO_DEFAULT_PATH)
endif()

find_program(CPPCHECK_EXECUTABLE NAMES cppcheck)

# Restore original setting for appbundle finding
set(CMAKE_FIND_APPBUNDLE ${_oldappbundlesetting})

if(CPPCHECK_EXECUTABLE)
  # Find out where our test file is
  get_filename_component(_cppcheckmoddir ${CMAKE_CURRENT_LIST_FILE} PATH)
  set(_cppcheckdummyfile "${_cppcheckmoddir}/Findcppcheck.cpp")

  # common to all version of cppcheck
  set(CPPCHECK_QUIET_ARG --quiet)
  set(CPPCHECK_FORCE_ARG --force)
  set(CPPCHECK_VERBOSE_ARG --verbose)
  set(CPPCHECK_INCLUDEPATH_ARG -I)
  set(CPPCHECK_DEFINITION_ARG -D)
  set( CPPCHECK_ALL_ARG --enable=all)

  # Check for the two types of command line arguments by just trying them
  execute_process(COMMAND
      ${CPPCHECK_EXECUTABLE}
      --enable=style
      ${CPPCHECK_QUIET_ARG}
      ${_cppcheckdummyfile}
    RESULT_VARIABLE
      _cppcheck_enable_style_result
    OUTPUT_QUIET
    ERROR_QUIET
    )

  if( "${_cppcheck_enable_style_result}" EQUAL 0 )

    set( CPPCHECK_STYLE_ARG --enable=style)

    # How to display errors and warnings:
    if(MSVC)
      set(CPPCHECK_TEMPLATE_ARG --template vs)
      set(CPPCHECK_FAIL_REGULAR_EXPRESSION "[(]error[)]")
      set(CPPCHECK_WARN_REGULAR_EXPRESSION "[(]style[)]")
    else()
      ## This is about the IDE, not the compiler for formatting support.  Many IDE's
      ## support the gcc style error messages.
      set(CPPCHECK_TEMPLATE_ARG --template gcc)
      set(CPPCHECK_FAIL_REGULAR_EXPRESSION " error: ")
      set(CPPCHECK_WARN_REGULAR_EXPRESSION " style: ")
    endif()

  else()
    message( "This file supports only version of cppcheck is newer than 1.43!" )

  endif()

  execute_process(COMMAND
      ${CPPCHECK_EXECUTABLE}
      --enable=unusedFunctions
      ${CPPCHECK_QUIET_ARG}
      ${_cppcheckdummyfile}
    RESULT_VARIABLE
      _cppcheck_enable_unused_functions_results
    OUTPUT_QUIET
    ERROR_QUIET
    )

  if("${_cppcheck_enable_unused_functions_results}" EQUAL 0)
    set(CPPCHECK_UNUSEDFUNC_ARG --enable=unusedFunctions)
  else()
    execute_process(COMMAND
      ${CPPCHECK_EXECUTABLE}
      --enable=unusedFunction
      ${CPPCHECK_QUIET_ARG}
      ${_cppcheckdummyfile}
    RESULT_VARIABLE
      _cppcheck_enable_unused_function_results
    OUTPUT_QUIET
    ERROR_QUIET
    )

    if( "${_cppcheck_enable_unused_function_results}" EQUAL 0 )
      set(CPPCHECK_UNUSEDFUNC_ARG --enable=unusedFunction)
    else()
      set(CPPCHECK_UNUSEDFUNC_ARG )
    endif()

  endif()

  execute_process(COMMAND
      ${CPPCHECK_EXECUTABLE}
      --enable=information
      ${CPPCHECK_QUIET_ARG}
      ${_cppcheckdummyfile}
    RESULT_VARIABLE
      _cppcheck_enable_information_results
    OUTPUT_QUIET
    ERROR_QUIET
    )

  if("${_cppcheck_enable_information_results}" EQUAL 0)
    # supported since
    set(CPPCHECK_INFORMATION_ARG --enable=information)
  else()
    set(CPPCHECK_INFORMATION_ARG )
  endif()

  execute_process(COMMAND
      ${CPPCHECK_EXECUTABLE}
      --enable=missingInclude
      ${CPPCHECK_QUIET_ARG}
      ${_cppcheckdummyfile}
    RESULT_VARIABLE
      _cppcheck_missingInclude_results
    OUTPUT_QUIET
    ERROR_QUIET
    )

  if("${_cppcheck_missingInclude_results}" EQUAL 0)
    # supported since
    set(CPPCHECK_MISSING_INCLUDE_ARG --enable=missingInclude)
  else()
    set(CPPCHECK_MISSING_INCLUDE_ARG )
  endif()

endif()

set(CPPCHECK_ALL
"${CPPCHECK_EXECUTABLE} ${CPPCHECK_POSSIBLEERROR_ARG} ${CPPCHECK_UNUSEDFUNC_ARG} ${CPPCHECK_STYLE_ARG} ${CPPCHECK_QUIET_ARG} ${CPPCHECK_INCLUDEPATH_ARG} some/include/path")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(cppcheck
  DEFAULT_MSG
  CPPCHECK_ALL
  CPPCHECK_EXECUTABLE
  CPPCHECK_UNUSEDFUNC_ARG
  CPPCHECK_STYLE_ARG
  CPPCHECK_INFORMATION_ARG
  CPPCHECK_MISSING_INCLUDE_ARG
  CPPCHECK_ALL_ARG
  CPPCHECK_INCLUDEPATH_ARG
  CPPCHECK_DEFINITION_ARG
  CPPCHECK_QUIET_ARG
  CPPCHECK_FORCE_ARG
  CPPCHECK_VERBOSE_ARG
)

if(CPPCHECK_FOUND OR CPPCHECK_MARK_AS_ADVANCED)
  mark_as_advanced(CPPCHECK_ROOT_DIR)
endif()

mark_as_advanced(CPPCHECK_EXECUTABLE)
