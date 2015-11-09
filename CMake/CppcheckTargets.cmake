# - Run cppcheck on c++ source files as a custom target and a test
#
# include(CppcheckTargets)
# add_cppcheck(<target-name> [UNUSED_FUNCTIONS] [STYLE] [POSSIBLE_ERROR] [FAIL_ON_WARNINGS]) -
# Create a target to check a target's sources with cppcheck and the indicated options
# add_cppcheck_sources(<target-name> [UNUSED_FUNCTIONS] [STYLE] [POSSIBLE_ERROR] [FAIL_ON_WARNINGS]) -
# Create a target to check standalone sources with cppcheck and the indicated options
#
# Requires these CMake modules:
# Findcppcheck
#
# Requires CMake 2.6 or newer (uses the 'function' command)
#
#
# Original Author:
# 2009-2010 Ryan Pavlik <rpavlik@iastate.edu> <abiryan@ryand.net>
# http://academic.cleardefinition.com
# Iowa State University HCI Graduate Program/VRAC
#
# Addition:
# 2011 Arnaud Gelas <arnaud_gelas@hms.harvard.edu>
# Harvard Medical School
#
# Copyright Iowa State University 2009-2010.
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE_1_0.txt or copy at
# http://www.boost.org/LICENSE_1_0.txt)

if(__add_cppcheck)
  return()
endif()
set(__add_cppcheck YES)

if(NOT CPPCHECK_FOUND)
  find_package(cppcheck QUIET)
endif()

if(CPPCHECK_FOUND)
  if(NOT TARGET all_cppcheck)
    add_custom_target(all_cppcheck)
    set_target_properties(all_cppcheck PROPERTIES EXCLUDE_FROM_ALL TRUE)
  endif()
endif()

# ------------------------------------------------------------------------------
macro( get_cppcheck_arg )

    list(FIND _input FORCE _force)
    if("${_force}" GREATER "-1")
      list(APPEND _cppcheck_args ${CPPCHECK_FORCE_ARG})
      list(REMOVE_AT _input ${_force})
    endif()

    list(FIND _input VERBOSE _verbose)
    if("${_verbose}" GREATER "-1")
      list(APPEND _cppcheck_args ${CPPCHECK_VERBOSE_ARG})
      list(REMOVE_AT _input ${_verbose})
    endif()

    list(FIND _input QUIET _quiet)
    if("${_quiet}" GREATER "-1")
      list(APPEND _cppcheck_args ${CPPCHECK_QUIET_ARG})
      list(REMOVE_AT _input ${_quiet})
    endif()

    list(FIND _input ALL _all)
    if("${_all}" GREATER "-1")
      list(APPEND _cppcheck_args ${CPPCHECK_ALL_ARG})
      list(REMOVE_AT _input ${_all})
    endif()

    list(FIND _input UNUSED_FUNCTIONS _unused_func)
    if("${_unused_func}" GREATER "-1")
      list(APPEND _cppcheck_args ${CPPCHECK_UNUSEDFUNC_ARG})
      list(REMOVE_AT _input ${_unused_func})
    endif()

    list(FIND _input STYLE _style)
    if("${_style}" GREATER "-1")
      list(APPEND _cppcheck_args ${CPPCHECK_STYLE_ARG})
      list(REMOVE_AT _input ${_style})
    endif()

    list(FIND _input INFORMATION _information)
    if("${_information}" GREATER "-1")
      list(APPEND _cppcheck_args ${CPPCHECK_INFORMATION_ARG})
      list(REMOVE_AT _input ${_information})
    endif()

    list(FIND _input MISSING_INCLUDE _missing_include)
    if("${_missing_include}" GREATER "-1")
      list(APPEND _cppcheck_args ${CPPCHECK_MISSING_INCLUDE_ARG})
      list(REMOVE_AT _input ${_missing_include})
    endif()

    list(FIND _input FAIL_ON_WARNINGS _fail_on_warn)
    if("${_fail_on_warn}" GREATER "-1")
      list(APPEND _cppcheck_args
        CPPCHECK_FAIL_REGULAR_EXPRESSION
        ${CPPCHECK_WARN_REGULAR_EXPRESSION})
      list(REMOVE_AT _input ${_fail_on_warn})
    endif()

endmacro()

# ------------------------------------------------------------------------------
# add_cppcheck_dir
function(add_cppcheck_dir _name _dir _include_dirs)
  if(CPPCHECK_FOUND)
    set(_cppcheck_args )
    set(_input ${ARGN})

    get_cppcheck_arg( ${_input} )

    # --------------------------------------------------------------
    foreach( _includeDirs ${_include_dirs} )
      set( _cppcheck_include ${_cppcheck_include} -I${_includeDirs} )
    endforeach()

    set( _cppcheck_compile_args ${_cppcheck_include} )

    itk_add_test(
      NAME
        ${_name}CPPCheckTest
      COMMAND
        "${CPPCHECK_EXECUTABLE}"
        ${CPPCHECK_TEMPLATE_ARG}
        ${_cppcheck_args}
        ${_cppcheck_compile_args}
        ${_dir}
        )

  set_tests_properties(${_name}CPPCheckTest
    PROPERTIES
    FAIL_REGULAR_EXPRESSION
      "${CPPCHECK_FAIL_REGULAR_EXPRESSION}")

  add_custom_command(TARGET
    all_cppcheck
    PRE_BUILD
    COMMAND
      ${CPPCHECK_EXECUTABLE}
      ${CPPCHECK_QUIET_ARG}
      ${CPPCHECK_TEMPLATE_ARG}
      ${_cppcheck_args}
      ${_cppcheck_compile_args}
      ${_dir}
    WORKING_DIRECTORY
      "${_dir}"
    COMMENT
      "${_name}_cppcheck: Running cppcheck on ${_dir}..."
    VERBATIM)
  endif()
endfunction()

# ------------------------------------------------------------------------------
# add_cppcheck_sources
function(add_cppcheck_sources _targetname)
  if(CPPCHECK_FOUND)
    # Normally --force should not be required, but since all compiler definitions
    # can't be detected, it is better to enforce testing all possibilities
    set(_cppcheck_args )
    set(_input ${ARGN})

    get_cppcheck_arg( "${_input}" )

    set(_files)
    foreach(_source ${_input})
      get_source_file_property(_cppcheck_loc "${_source}" LOCATION)
      if(_cppcheck_loc)
      # This file has a source file property, carry on.
        get_source_file_property(_cppcheck_lang "${_source}" LANGUAGE)
        if("${_cppcheck_lang}" MATCHES "CXX")
          list(APPEND _files "${_cppcheck_loc}")
        endif()
      else()
        # This file doesn't have source file properties - figure it out.
        get_filename_component(_cppcheck_loc "${_source}" ABSOLUTE)
        if(EXISTS "${_cppcheck_loc}")
          list(APPEND _files "${_cppcheck_loc}")
        else()
          message(FATAL_ERROR
            "Adding CPPCHECK for file target ${_targetname}: "
            "File ${_source} does not exist or needs a corrected path location "
            "since we think its absolute path is ${_cppcheck_loc}")
        endif()
      endif()
    endforeach()

    # let's take of include dirs here
    get_property( mytargINCLUDES DIRECTORY
      "${CMAKE_CURRENT_SOURCE_DIR}" PROPERTY INCLUDE_DIRECTORIES)

    set( _cppcheck_include )
    foreach( _includeDirs ${mytargINCLUDES} )
      set( _cppcheck_include ${_cppcheck_include} -I${_includeDirs} )
    endforeach()

    # --------------------------------------------------------------
    # let's take of compile definitions here
    # NOTE: it does not work, you need to get all definitions by
    # another way
    get_property(mytargDEFINITIONS DIRECTORY
      "${CMAKE_CURRENT_SOURCE_DIR}" PROPERTY COMPILE_DEFINITIONS)

    set( _cppcheck_def )
    foreach( _compiledef ${mytargDEFINITIONS} )
      set( _cppcheck_def ${_cppcheck_def} -D${_compiledef} )
    endforeach()

    # --------------------------------------------------------------
    set( _cppcheck_compile_args ${_cppcheck_include} ${_cppcheck_def} )

    itk_add_test(
      NAME
        ${_targetname}CPPCheckTest
      COMMAND
        "${CPPCHECK_EXECUTABLE}"
        ${CPPCHECK_TEMPLATE_ARG}
        ${_cppcheck_args}
        ${_cppcheck_compile_args}
        ${_files})

  set_tests_properties(${_targetname}CPPCheckTest
    PROPERTIES
    FAIL_REGULAR_EXPRESSION
      "${CPPCHECK_FAIL_REGULAR_EXPRESSION}")

  add_custom_command(TARGET
    all_cppcheck
    PRE_BUILD
    COMMAND
      ${CPPCHECK_EXECUTABLE}
      ${CPPCHECK_QUIET_ARG}
      ${CPPCHECK_TEMPLATE_ARG}
      ${_cppcheck_args}
      ${_cppcheck_compile_args}
      ${_files}
    WORKING_DIRECTORY
      "${CMAKE_CURRENT_SOURCE_DIR}"
    COMMENT
      "${_targetname}_cppcheck: Running cppcheck on target ${_targetname}..."
    VERBATIM)
  endif()
endfunction()

# ------------------------------------------------------------------------------
# add_cppcheck
function(add_cppcheck _name)
  if(NOT TARGET ${_name})
    message(FATAL_ERROR
      "add_cppcheck given a target name that does not exist: '${_name}' !")
  endif()
  if(CPPCHECK_FOUND)
    set(_cppcheck_args )
    set(_input ${ARGN})

    get_cppcheck_arg( ${_input} )

    get_target_property(_cppcheck_sources "${_name}" SOURCES)
    set(_files)
    foreach(_source ${_cppcheck_sources})
      get_source_file_property(_cppcheck_lang "${_source}" LANGUAGE)
      get_source_file_property(_cppcheck_loc "${_source}" LOCATION)
      if("${_cppcheck_lang}" MATCHES "CXX")
        list(APPEND _files "${_cppcheck_loc}")
      endif()
    endforeach()

    # let's take of include dirs here
    get_property( mytargINCLUDES DIRECTORY
      "${CMAKE_CURRENT_SOURCE_DIR}" PROPERTY INCLUDE_DIRECTORIES)

    set( _cppcheck_include )
    foreach( _includeDirs ${mytargINCLUDES} )
      set( _cppcheck_include "${_cppcheck_include} -I ${_includeDirs}" )
    endforeach()

    # --------------------------------------------------------------
    # let's take of compile definitions here
    get_property(mytargDEFINITIONS DIRECTORY
      "${CMAKE_CURRENT_SOURCE_DIR}" PROPERTY COMPILE_DEFINITIONS )

    set( _cppcheck_def )
    foreach( _compiledef ${mytargDEFINITIONS} )
      set( _cppcheck_def ${_cppcheck_def} -D${_compiledef} )
    endforeach()

    # --------------------------------------------------------------
    set( _cppcheck_compile_args --check-config ${_cppcheck_include} ${_cppcheck_def} )

     itk_add_test(
       NAME
         ${_name}CPPCheckTest
       COMMAND
         "${CPPCHECK_EXECUTABLE}"
         ${CPPCHECK_TEMPLATE_ARG}
         ${_cppcheck_args}
         ${_cppcheck_compile_args}
         ${_files})

      set_tests_properties(${_name}CPPCheckTest
        PROPERTIES
        FAIL_REGULAR_EXPRESSION
        "${CPPCHECK_FAIL_REGULAR_EXPRESSION}")

      add_custom_command(TARGET
        all_cppcheck
        PRE_BUILD
        COMMAND
          ${CPPCHECK_EXECUTABLE}
          ${CPPCHECK_QUIET_ARG}
          ${CPPCHECK_TEMPLATE_ARG}
          ${_cppcheck_args}
          ${_files}
        WORKING_DIRECTORY
          "${CMAKE_CURRENT_SOURCE_DIR}"
        COMMENT
          "${_name}_cppcheck: Running cppcheck on target ${_name}..."
       V ERBATIM)
   endif()

endfunction()
