macro(set_if_not_defined var defaultvalue)
# Macro allowing to set a variable to its default value if not already defined.
# The default value is set with:
#  (1) if set, the value environment variable <var>.
#  (2) if set, the value of local variable variable <var>.
#  (3) if none of the above, the value passed as a parameter.
# Setting the optional parameter 'OBFUSCATE' will display 'OBFUSCATED' instead of the real value.
  set(_obfuscate FALSE)
  foreach(arg ${ARGN})
    if(arg STREQUAL "OBFUSCATE")
      set(_obfuscate TRUE)
    endif()
  endforeach()
  if(DEFINED ENV{${var}} AND NOT DEFINED ${var})
    set(_value "$ENV{${var}}")
    if(_obfuscate)
      set(_value "OBFUSCATED")
    endif()
    message(STATUS "Setting '${var}' variable with environment variable value '${_value}'")
    set(${var} $ENV{${var}})
  endif()
  if(NOT DEFINED ${var})
    set(_value "${defaultvalue}")
    if(_obfuscate)
      set(_value "OBFUSCATED")
    endif()
    message(STATUS "Setting '${var}' variable with default value '${_value}'")
    set(${var} "${defaultvalue}")
  endif()
endmacro()

function(add_nifti_library target_in)
    add_library(${ARGV})
    add_library(NIFTI::${target_in} ALIAS ${target_in})
    get_property(tmp GLOBAL PROPERTY nifti_installed_targets)
    list(APPEND tmp "${target_in}")
    set_property(GLOBAL PROPERTY nifti_installed_targets "${tmp}")
endfunction()

function(add_nifti_executable target_in)
  add_executable(${ARGV})
  get_property(tmp GLOBAL PROPERTY nifti_installed_targets)
  list(APPEND tmp "${target_in}")
  set_property(GLOBAL PROPERTY nifti_installed_targets "${tmp}")
endfunction()

function(install_man_page target)
  # uses help2man to generate manpages for an executable sections can be added
  # to the man page by populating the OPTIONS_FOR_SECTIONS argument. For
  # example setting this to "-see_also" will include the output of the
  # "executable -see_also" in a "see_also" section of the final manpage.
  # Additional arguments to the help2man call can be provided in the OPTS
  # argument
  set(MAN_DEPENDS "")
  set(INCLUDE_STRING "")
  if(NIFTI_INSTALL_NO_DOCS)
    return()
  endif()

  # Parse args
  cmake_parse_arguments(ARG
      ""
      ""
      "OPTIONS_FOR_SECTIONS;OPTS"
      ${ARGN}
        )
    #message(FATAL_ERROR "${ARG_OPTS}:::::::: ${ARG_OPTIONS_FOR_SECTIONS}:::::: ${ARGN}")
  # generate additional sections for inclusion in manpage
  foreach(section ${ARG_OPTIONS_FOR_SECTIONS})
    string(REGEX REPLACE "-" "" SECTION_NAME ${section})
    set(SECTION_FILE  ${MAN_DIR}/${SECTION_NAME}.h2m)
    add_custom_command(
        OUTPUT ${SECTION_FILE}
        COMMAND   ${CMAKE_COMMAND} -E echo \"[ ${SECTION_NAME} ]\" > ${SECTION_FILE}
        COMMAND $<TARGET_FILE:${target}> ${section} >> ${SECTION_FILE}
        USES_TERMINAL
        COMMENT Generating ${SECTION_NAME} for ${target}
        )
    list(APPEND MAN_DEPENDS ${SECTION_FILE})
    list(APPEND INCLUDE_STRING "--include=${SECTION_FILE} ")
  endforeach()

  # Generate the man-page
  set(MAN_PAGE ${MAN_DIR}/${target}_manpage.1)
  add_custom_command(
      OUTPUT ${MAN_PAGE}
      DEPENDS ${MAN_DEPENDS}
      WORKING_DIRECTORY ${MAN_DIR}
      COMMAND ${HELP2MAN} $<TARGET_FILE:${target}> ${ARG_OPTS} ${INCLUDE_STRING} -o ${MAN_PAGE}
      COMMAND gzip -f ${MAN_PAGE}
      USES_TERMINAL
      COMMENT Generating man page for ${target}
      )
  add_custom_target(${target}_man
      ALL
      DEPENDS ${MAN_PAGE}
      )
endfunction()

function(install_nifti_target target_name)
  # Check if the current directory is the base directory of the project
  if("${PROJECT_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_LIST_DIR}")
    set(IS_PROJECT_DIR 1)
  else()
    set(IS_PROJECT_DIR 0)
  endif()

  if(NOT CMAKE_VER_AT_LEAST_3_13 AND IS_PROJECT_DIR)
    # Early version of CMake so installation must happen in the directory in
    # which the target is defined. No installation occurs from the project
    # directory
    return()
  elseif(CMAKE_VER_AT_LEAST_3_13 AND NOT IS_PROJECT_DIR)
    # CMake >=3.13 has support for referencing targets in parent scopes of the
    # one in which the target is defined. This enables a central management of
    # the installation process, along with installating an target export set.
    # No installation occurs from the directory in which the target is defined
    return()
  endif()

  # Install the targets now that the appropriate directory is confirmed.
  install(TARGETS ${target_name}
          EXPORT ${NIFTI_INSTALL_EXPORT_NAME}
          RUNTIME
            COMPONENT RuntimeBinaries
            DESTINATION ${NIFTI_INSTALL_RUNTIME_DIR}
          ARCHIVE
            DESTINATION ${NIFTI_INSTALL_LIBRARY_DIR}
            COMPONENT RuntimeLibraries
          LIBRARY
            DESTINATION ${NIFTI_INSTALL_LIBRARY_DIR}
            COMPONENT RuntimeLibraries
          PUBLIC_HEADER
            DESTINATION ${NIFTI_INSTALL_INCLUDE_DIR}
            COMPONENT Development
          INCLUDES
            DESTINATION ${NIFTI_INSTALL_INCLUDE_DIR}
          )
endfunction()

function(get_lib_version_var ver_header_text ver_type version_out)
  # Gets version for MAJOR/MINOR or PATCH from version header file
  string(REGEX MATCH "_${ver_type} [0-9]*" MATCHED "${ver_header_text}")
  string(REGEX REPLACE "_${ver_type} " "" OUTPUT "${MATCHED}")
  set(${version_out} ${OUTPUT} PARENT_SCOPE)
endfunction()

function(get_lib_version_vars version_header libver libver_major)
    # Function reads a file containing the lib version and sets the
    # approprioate variables in the parent scope
    file(READ ${version_header} VER_FILE)
    get_lib_version_var(${VER_FILE} "MAJOR" LIB_MAJOR_VERSION )
    get_lib_version_var(${VER_FILE} "MINOR"  LIB_MINOR_VERSION )
    get_lib_version_var(${VER_FILE} "PATCH"  LIB_PATCH_VERSION )
    set(LIB_VERSION "${LIB_MAJOR_VERSION}.${LIB_MINOR_VERSION}.${LIB_PATCH_VERSION}")

    # Check that a valid version has been specified (of the form XX.XX.XX)
    string(REGEX MATCH "^[0-9]*\.[0-9]*\.[0-9]*$" VER_MATCHED "${LIB_VERSION}" )
    if("" STREQUAL "${VER_MATCHED}")
        message(STATUS "matched ${VER_MATCHED}")
        message(FATAL_ERROR "Cannot find a valid version in the version header file ${version_header} (Found: '${LIB_VERSION}')")
    endif()
    # Set outputs in calling scope
    set(${libver} "${LIB_VERSION}" PARENT_SCOPE)
    set(${libver_major} "${LIB_MAJOR_VERSION}" PARENT_SCOPE)
endfunction()
