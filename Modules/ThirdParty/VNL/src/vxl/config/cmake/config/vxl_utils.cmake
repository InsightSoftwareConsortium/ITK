#
# INSTALL_NOBASE_HEADER_FILES(prefix file file file ...)
# Will create install rules for those files of the list
# which are headers (.h, .hxx or .hxx).
# If .in files are given, the .in extension is removed.
#

macro(INSTALL_NOBASE_HEADER_FILES prefix)
  foreach(file ${ARGN})
    if(${file} MATCHES "\\.(h|hxx|txx)?$")
      string(REGEX REPLACE "\\.in$" "" install_file ${file})
      get_filename_component(dir ${install_file} PATH)
      install(FILES ${install_file}
              DESTINATION ${prefix}/${dir}
              PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ
              COMPONENT Development )
    endif()
    if(${file} MATCHES "\\.in?$")
      string(REGEX REPLACE "\\.in$" "" install_file ${file})
      if(EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${install_file})
        install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${install_file}
                DESTINATION ${prefix}
                COMPONENT Development )
      else()
        message(WARNING "File not found: ${CMAKE_CURRENT_BINARY_DIR}/${install_file}")
      endif()
    endif()
  endforeach()
endmacro()

##
#
# A macro to setup configuration and installation of header files
#
macro(vxl_configure_file infile outfile installprefix)
  configure_file(${infile}  ${outfile}  ESCAPE_QUOTES @ONLY)
  install(FILES ${outfile}
      DESTINATION ${installprefix}
      PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ
      COMPONENT Development )
endmacro()

##
#
# A macro to setup configuration and installation of header files
#
macro(vxl_configure_file_copyonly infile outfile installprefix)
  configure_file(${infile}  ${outfile} COPYONLY)
  install(FILES ${outfile}
      DESTINATION ${installprefix}
      PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ
      COMPONENT Development )
endmacro()

#
# A function to configure where libraries are to be installed for
# vxl for adding a library, setting it's properties, and
# setting it's install location
#
#  LIBRARY_NAME        (required) is the name of the library to create
#  LIBRARY_SOURCES     (required) is a list of sources needed to create the
#                      library. It should also contain headers to install for
#                      building against the library.
#  HEADER_BUILD_DIR    (optional) directory to append to library target
#                      BUILD_INTERFACE. Useful to include a target build
#                      directory (CMAKE_CURRENT_BINARY_DIR) containing files
#                      generated via vxl_configure_file.
#  HEADER_INSTALL_DIR  (optional) directory to install headers relative to
#                      VXL_INSTALL_INCLUDE_DIR if VXL_INSTALL_INCLUDE_DIR is
#                      not its default value; otherwise, the relative path in
#                      the vxl source tree is used.
#  DISABLE_MSVC_MP     Disable automatic addition of the "/MP" flag in MSVC
#                      builds.  Avoids MSVC warning C5102: ignoring invalid
#                      command-line macro definition '/MP'
#
function( vxl_add_library )
  cmake_parse_arguments(vxl_add
     "DISABLE_MSVC_MP"  # options
     "LIBRARY_NAME;HEADER_BUILD_DIR;HEADER_INSTALL_DIR"  # oneValueArgs
     "LIBRARY_SOURCES"  # multiValueArgs
     ${ARGN} )

  ## If not source files, then no lib created
  list(LENGTH vxl_add_LIBRARY_SOURCES num_src_files)
  if( ${num_src_files} GREATER 0 )
    add_library(${vxl_add_LIBRARY_NAME} ${vxl_add_LIBRARY_SOURCES} )

    # This enables object-level build parallelism in VNL libraries for MSVC
    # - disabled for MSVC simulators such as clang-cl
    #     https://github.com/vxl/vxl/issues/863
    #     https://gitlab.kitware.com/cmake/cmake/-/issues/19724
    # - disabled for COMPILE_LANGUAGE!=CXX (via generator expression)
    # - disabled for DISABLE_MSVC_MP
    if(MSVC AND NOT "${CMAKE_CXX_SIMULATE_ID}" STREQUAL "MSVC" AND NOT vxl_add_DISABLE_MSVC_MP)
      target_compile_options(${vxl_add_LIBRARY_NAME} PRIVATE
          $<$<COMPILE_LANGUAGE:CXX>:/MP> )
    endif()

    set_property(GLOBAL APPEND PROPERTY VXLTargets_MODULES ${vxl_add_LIBRARY_NAME})
    if(VXL_LIBRARY_PROPERTIES)
       set_target_properties(${vxl_add_LIBRARY_NAME} PROPERTIES ${VXL_LIBRARY_PROPERTIES})
    endif()

    # Installation
    install(TARGETS ${vxl_add_LIBRARY_NAME}
      EXPORT ${VXL_INSTALL_EXPORT_NAME}
      RUNTIME DESTINATION ${VXL_INSTALL_RUNTIME_DIR} COMPONENT RuntimeLibraries
      LIBRARY DESTINATION ${VXL_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
      ARCHIVE DESTINATION ${VXL_INSTALL_ARCHIVE_DIR} COMPONENT Development)
  endif()

  # build interface
  set(build_interface "${CMAKE_CURRENT_SOURCE_DIR}")
  if (DEFINED vxl_add_HEADER_BUILD_DIR)
    list(APPEND build_interface "${vxl_add_HEADER_BUILD_DIR}")
  endif()

  target_include_directories(${vxl_add_LIBRARY_NAME} PUBLIC
      "$<BUILD_INTERFACE:${build_interface}>"
  )

  # install interface

  # If VXL_INSTALL_INCLUDE_DIR is the default value
  if("${VXL_INSTALL_INCLUDE_DIR}" STREQUAL "include/vxl")
    ## Identify the relative path for installing the header files and txx files
    string(REPLACE ${VXL_ROOT_SOURCE_DIR} "${VXL_INSTALL_INCLUDE_DIR}" relative_install_path ${CMAKE_CURRENT_SOURCE_DIR})
    target_include_directories(${vxl_add_LIBRARY_NAME} PUBLIC
        $<INSTALL_INTERFACE:${relative_install_path}>
    )
  else()
    set(relative_install_path "${VXL_INSTALL_INCLUDE_DIR}")
    if(DEFINED vxl_add_HEADER_INSTALL_DIR)
      set(relative_install_path "${relative_install_path}/${vxl_add_HEADER_INSTALL_DIR}")
    endif()
    target_include_directories(${vxl_add_LIBRARY_NAME} PUBLIC
        $<INSTALL_INTERFACE:${VXL_INSTALL_INCLUDE_DIR}>
    )
  endif()

  INSTALL_NOBASE_HEADER_FILES(${relative_install_path} ${vxl_add_LIBRARY_SOURCES})

endfunction()


# This macro sets a targets visibility to
# hidden, and configures the header that
# contains the proper export defines
# and configures the install location
#  SET_VXL_LIBRARY_PROPERTIES(
#     COMPILE_FLAGS "-fPIC -Wall"  # Extra compiler flags
#     TARGET_NAME ${_TARGET_NAME}  # Library target name to modify
#     BASE_NAME ${_BASE_NAME}      # Prefix for export macros ${BASE_NAME}_EXPORT used in headers
#                                  # #include "${BASE_NAME}_export.h"
#     EXPORT_HEADER_FILE ${_export_header_file}  # Where to generate the ${BASE_NAME}_export.h file
#     INSTALL_DIR ${_INSTALL_DIR}  # Where to install the ${EXPORT_HEADER_FILE}
#     USE_HIDDEN_VISIBILITY        # If set, then use hidden visibility for exports by default
#  )
#
macro(SET_VXL_LIBRARY_PROPERTIES)
  set(options USE_HIDDEN_VISIBILITY)
  set(oneValueArgs TARGET_NAME BASE_NAME INSTALL_DIR EXPORT_HEADER_FILE)
  set(multiValueArgs COMPILE_FLAGS)
  cmake_parse_arguments(LSLHVP
       "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  if(LSLHVP_UNPARSED_ARGUMENTS)
    message(FATAL_ERROR "Unknown keywords given to SET_LIBRARY_HIDDEN_VISIBILITY_PROPERTIES(): \"${LSLHVP_UNPARSED_ARGUMENTS}\"")
  endif()
  if(LSLHVP_COMPILE_FLAGS)
     set_target_properties(${LSLHVP_TARGET_NAME} PROPERTIES COMPILE_FLAGS "${LSLHVP_COMPILE_FLAGS}")
  endif()

  if(LSLHVP_USE_HIDDEN_VISIBILITY)
    if(NOT LSLHVP_BASE_NAME)
        message(FATAL_ERROR "BASE_NAME REQUIRED when using USE_HIDDEN_VISIBILITY")
    endif()
    if(NOT LSLHVP_EXPORT_HEADER_FILE)
        message(FATAL_ERROR "EXPORT_HEADER_FILE REQUIRED when using USE_HIDDEN_VISIBILITY")
    endif()
    if(NOT LSLHVP_INSTALL_DIR)
        message(FATAL_ERROR "INSTALL_DIR REQUIRED when using USE_HIDDEN_VISIBILITY")
    endif()

    if (BUILD_SHARED_LIBS OR (APPLE AND NOT BUILD_SHARED_LIBS) ) #APPLE Respects hidden visibility for static builds
      # export flags are only added when building shared libs, they cause
      # mismatched visibility warnings when building statically.
      if (USE_COMPILER_HIDDEN_VISIBILITY)
        # Prefer to use target properties supported by newer cmake
        set_target_properties(${LSLHVP_TARGET_NAME} PROPERTIES CXX_VISIBILITY_PRESET hidden)
        set_target_properties(${LSLHVP_TARGET_NAME} PROPERTIES C_VISIBILITY_PRESET hidden)
        set_target_properties(${LSLHVP_TARGET_NAME} PROPERTIES VISIBILITY_INLINES_HIDDEN 1)
      endif()
    endif()
  endif()

  generate_export_header(${LSLHVP_TARGET_NAME}
       BASE_NAME ${LSLHVP_BASE_NAME}
       EXPORT_FILE_NAME ${LSLHVP_EXPORT_HEADER_FILE}
  )
  install(FILES ${LSLHVP_EXPORT_HEADER_FILE}
      DESTINATION ${LSLHVP_INSTALL_DIR}
      PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ
      COMPONENT Development )
endmacro()

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
#   set(vil_test_sources
#     ...
#     test_stream.cxx
#     ...
#   )
#   set(test_stream_args ${CMAKE_CURRENT_SOURCE_DIR}/file_read_data)
#   GENERATE_TEST_DRIVER(vil vil_test_sources vil vpl vul testlib vcl)
#---------------------------------------------------------------------
macro(GENERATE_TEST_DRIVER LIB SOURCES)
  create_test_sourcelist(test_driver_sources ${LIB}_test_driver.cxx
    ${${SOURCES}}
  )

  add_executable(${LIB}_test_driver ${test_driver_sources})
  # ***** what if ARGN is empty?
  target_link_libraries(${LIB}_test_driver ${ARGN})

  set(tests_to_run ${test_driver_sources})
  list(REMOVE_ITEM tests_to_run ${LIB}_test_driver.cxx)

  foreach(test ${tests_to_run})
    get_filename_component(test_name ${test} NAME_WE)
    add_test( NAME ${LIB}_${test_name}
              COMMAND $<TARGET_FILE:${LIB}_test_driver> ${test_name} ${${test_name}_args}
            )
  endforeach()
endmacro()

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
macro(GENERATE_TEST_INCLUDE LIB SOURCES PREFIX)
  set(CMAKE_CONFIGURABLE_FILE_CONTENT "/* */\n")
  foreach(FILE ${${SOURCES}})
    get_filename_component(FILE_EXT ${FILE} EXT)
    if(FILE_EXT STREQUAL ".h")
      set(CMAKE_CONFIGURABLE_FILE_CONTENT
          "${CMAKE_CONFIGURABLE_FILE_CONTENT}#include <${PREFIX}${FILE}>\n#include <${PREFIX}${FILE}>\n")
    endif()
  endforeach()

  set(CMAKE_CONFIGURABLE_FILE_CONTENT
      "${CMAKE_CONFIGURABLE_FILE_CONTENT}\n\nint main(){return 0;}\n")

  configure_file("${CMAKE_ROOT}/Modules/CMakeConfigurableFile.in"
                 "${CMAKE_CURRENT_BINARY_DIR}/test_include.cxx"
                 @ONLY)

  add_executable(${LIB}_test_include ${CMAKE_CURRENT_BINARY_DIR}/test_include.cxx)
  target_link_libraries(${LIB}_test_include ${LIB})
endmacro()
