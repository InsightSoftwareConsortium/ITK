#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

include (FetchContent)

# Function to retrieve all of the CMake targets generated
# in a directory and all its subdirectories
function (get_generated_cmake_targets out_var dir)
  get_directory_property (dir_targets DIRECTORY "${dir}" BUILDSYSTEM_TARGETS)
  get_directory_property (dir_subdirs DIRECTORY "${dir}" SUBDIRECTORIES)

  foreach (subdir ${dir_subdirs})
    get_generated_cmake_targets(subdir_targets "${subdir}")
    list (APPEND dir_targets "${subdir_targets}")
  endforeach()

  set (${out_var} "${dir_targets}" PARENT_SCOPE)
endfunction ()

# Function to apply connector-specify workarounds to build
# code once a connector has been populated through FetchContent
function (apply_connector_workarounds connector_name source_dir)
  # For the cache VOL, remove the call to find_package(ASYNC).
  # Eventually, the FetchContent OVERRIDE_FIND_PACKAGE should be
  # able to fulfill this dependency when building the cache VOL,
  # but for now we have to hack around this until the async and
  # cache VOLs create CMake .config files
  if ("${connector_name}" MATCHES "vol-cache")
    # Remove find_package(ASYNC) call from connector's CMake code
    file (READ "${source_dir}/CMakeLists.txt" vol_cmake_contents)
    string (REGEX REPLACE "[ \t]*find_package[ \t]*\\([ \t]*ASYNC[^\r\n\\)]*\\)[ \t]*[\r\n]+" "" vol_cmake_contents "${vol_cmake_contents}")
    file (WRITE "${source_dir}/CMakeLists.txt" "${vol_cmake_contents}")

    # Remove setting of HDF5_VOL_CONNECTOR and HDF5_PLUGIN_PATH
    # in connector's external tests CMake code
    file (STRINGS "${source_dir}/tests/CMakeLists.txt" file_lines)
    file (WRITE "${source_dir}/tests/CMakeLists.txt" "")
    foreach (line IN LISTS file_lines)
      set (stripped_line "${line}")
      string (REGEX MATCH "^[ \t]*set_tests_properties\\([ \t]*[\r\n]?" match_string "${line}")
      if (NOT "${match_string}" STREQUAL "")
        string (REGEX REPLACE "^[ \t]*set_tests_properties\\([ \t]*[\r\n]?" "" stripped_line "${line}")
      endif ()
      string (REGEX MATCH "^[ \t]*.\\{test\\}[ \t]*[\r\n]?" match_string "${line}")
      if (NOT "${match_string}" STREQUAL "")
        string (REGEX REPLACE "^[ \t]*.\\{[A-Za-z]*\\}[ \t]*[\r\n]?" "" stripped_line "${line}")
      endif ()
      string (REGEX MATCH "^[ \t]*PROPERTIES[ \t]*[\r\n]?" match_string "${line}")
      if (NOT "${match_string}" STREQUAL "")
        string (REGEX REPLACE "^[ \t]*PROPERTIES[ \t]*[\r\n]?" "" stripped_line "${line}")
      endif ()
      string (REGEX MATCH "^[ \t]*ENVIRONMENT[ \t]*.*[\r\n]?" match_string "${line}")
      if (NOT "${match_string}" STREQUAL "")
        string (REGEX REPLACE "^[ \t]*ENVIRONMENT[ \t]*.*[\r\n]?" "" stripped_line "${line}")
      endif ()
      file (APPEND "${source_dir}/tests/CMakeLists.txt" "${stripped_line}\n")
    endforeach ()
  endif ()
endfunction ()

set (HDF5_VOL_ALLOW_EXTERNAL "NO" CACHE STRING "Allow building of external HDF5 VOL connectors with FetchContent")
set_property (CACHE HDF5_VOL_ALLOW_EXTERNAL PROPERTY STRINGS NO GIT LOCAL_DIR)
mark_as_advanced (HDF5_VOL_ALLOW_EXTERNAL)
if (HDF5_VOL_ALLOW_EXTERNAL MATCHES "GIT" OR HDF5_VOL_ALLOW_EXTERNAL MATCHES "LOCAL_DIR")
  # For compatibility, set some variables that projects would
  # typically look for after calling find_package(HDF5)
  set (HDF5_FOUND 1)
  set (HDF5_LIBRARIES "${HDF5_LIBSH_TARGET};${LINK_LIBS};${LINK_COMP_LIBS};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPI::MPI_C>")
  set (HDF5_INCLUDE_DIRS "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  set (HDF5_IS_PARALLEL ${H5_HAVE_PARALLEL})
  set (HDF5_VERSION ${HDF5_PACKAGE_VERSION})

  set (HDF5_C_LIBRARIES "${HDF5_LIBRARIES}")
  
  if (HDF5_BUILD_HL_LIB)
    set (HDF5_C_HL_LIBRARIES "${HDF5_HL_LIBSH_TARGET}")
  endif()

  set (HDF5_MAX_EXTERNAL_VOLS 10)
  set (HDF5_EXTERNAL_VOL_TARGETS "")

  foreach (vol_idx RANGE 1 ${HDF5_MAX_EXTERNAL_VOLS})
    # Generate fixed-width index number prepended with 0s
    # so VOL sources come in order from 1 - HDF5_MAX_EXTERNAL_VOLS
    set (vol_idx_num_digits 2) # Based on HDF5_MAX_EXTERNAL_VOLS
    set (vol_idx_fixed "${vol_idx}")
    string (LENGTH "${vol_idx_fixed}" vol_idx_len)
    while (vol_idx_len LESS vol_idx_num_digits)
      string (PREPEND vol_idx_fixed "0")
      math (EXPR vol_idx_len "${vol_idx_len}+1")
    endwhile ()

    if (HDF5_VOL_ALLOW_EXTERNAL MATCHES "GIT")
      set (HDF5_VOL_URL${vol_idx_fixed} "" CACHE STRING "Git repository URL of an external HDF5 VOL connector to build")
      mark_as_advanced (HDF5_VOL_URL${vol_idx_fixed})
      set (HDF5_VOL_SOURCE "${HDF5_VOL_URL${vol_idx_fixed}}")
    elseif(HDF5_VOL_ALLOW_EXTERNAL MATCHES "LOCAL_DIR")
      set (HDF5_VOL_PATH${vol_idx_fixed} "" CACHE STRING "Path to the source directory of an external HDF5 VOL connector to build")
      mark_as_advanced (HDF5_VOL_PATH${vol_idx_fixed})
      set (HDF5_VOL_SOURCE "${HDF5_VOL_PATH${vol_idx_fixed}}")
    endif()

    if (NOT "${HDF5_VOL_SOURCE}" STREQUAL "")
      # Deal with trailing slash in path for LOCAL_DIR case
      if (HDF5_VOL_ALLOW_EXTERNAL MATCHES "LOCAL_DIR")
        # Erase trailing slash
        string (REGEX REPLACE "/$" "" HDF5_VOL_SOURCE ${HDF5_VOL_SOURCE})
      endif()

      # Extract the name of the VOL connector
      string (FIND "${HDF5_VOL_SOURCE}" "/" hdf5_vol_name_pos REVERSE)
      if (hdf5_vol_name_pos EQUAL -1)
        if (HDF5_VOL_ALLOW_EXTERNAL MATCHES "GIT")
          message (SEND_ERROR "Invalid URL '${HDF5_VOL_SOURCE}' specified for HDF5_VOL_URL${vol_idx_fixed}")
        elseif (HDF5_VOL_ALLOW_EXTERNAL MATCHES "LOCAL_DIR")
          message (SEND_ERROR "Invalid source path '${HDF5_VOL_SOURCE}' specified for HDF5_VOL_PATH${vol_idx_fixed}")
        endif()
      endif ()

      math (EXPR hdf5_vol_name_pos "${hdf5_vol_name_pos}+1")

      string (SUBSTRING "${HDF5_VOL_SOURCE}" ${hdf5_vol_name_pos} -1 hdf5_vol_name)
      string (REPLACE ".git" "" hdf5_vol_name "${hdf5_vol_name}")
      string (STRIP "${hdf5_vol_name}" hdf5_vol_name)
      string (TOUPPER "${hdf5_vol_name}" hdf5_vol_name_upper)
      string (TOLOWER "${hdf5_vol_name}" hdf5_vol_name_lower)

      message (VERBOSE "Building VOL connector '${hdf5_vol_name}' with FetchContent from source ${HDF5_VOL_SOURCE}")

      # Set some cache variables that can be set by users when building
      if (HDF5_VOL_ALLOW_EXTERNAL MATCHES "GIT")
        set ("HDF5_VOL_${hdf5_vol_name_upper}_BRANCH" "main" CACHE STRING "Git branch (or tag) to use when building VOL connector '${hdf5_vol_name}'")
        mark_as_advanced ("HDF5_VOL_${hdf5_vol_name_upper}_BRANCH")
      endif()

      set ("HDF5_VOL_${hdf5_vol_name_upper}_CMAKE_PACKAGE_NAME"
        "${hdf5_vol_name_lower}"
        CACHE
        STRING
        "CMake package name used by find_package(...) calls for VOL connector '${hdf5_vol_name}'"
      )

      set ("HDF5_VOL_${hdf5_vol_name_upper}_NAME" "" CACHE STRING "Name of VOL connector to set for the HDF5_VOL_CONNECTOR environment variable")
      option ("HDF5_VOL_${hdf5_vol_name_upper}_TEST_PARALLEL" "Whether to test VOL connector '${hdf5_vol_name}' against the parallel API tests" OFF)

      mark_as_advanced ("HDF5_VOL_${hdf5_vol_name_upper}_NAME")
      mark_as_advanced ("HDF5_VOL_${hdf5_vol_name_upper}_TEST_PARALLEL")

      if (HDF5_TEST_API)
        if ("${HDF5_VOL_${hdf5_vol_name_upper}_NAME}" STREQUAL "")
          message (SEND_ERROR "HDF5_VOL_${hdf5_vol_name_upper}_NAME must be set to a valid connector name to use VOL connector '${hdf5_vol_name}' for testing")
        endif ()
      endif ()

      if ((HDF5_VOL_ALLOW_EXTERNAL MATCHES "GIT") AND ("${HDF5_VOL_${hdf5_vol_name_upper}_BRANCH}" STREQUAL ""))
        message (SEND_ERROR "HDF5_VOL_${hdf5_vol_name_upper}_BRANCH must be set to a valid git branch name (or git tag) to build VOL connector '${hdf5_vol_name}'")
      endif ()

      if ((HDF5_VOL_ALLOW_EXTERNAL MATCHES "LOCAL_DIR")
        AND NOT (EXISTS ${HDF5_VOL_SOURCE} AND IS_DIRECTORY ${HDF5_VOL_SOURCE}))
          message (FATAL_ERROR "HDF5_VOL_PATH${vol_idx_fixed} must be an absolute path to a valid directory")
      endif ()

      # Set internal convenience variables for FetchContent dependency name
      set (hdf5_vol_depname "${HDF5_VOL_${hdf5_vol_name_upper}_CMAKE_PACKAGE_NAME}")
      string (TOLOWER "${hdf5_vol_depname}" hdf5_vol_depname_lower)

      if (${CMAKE_VERSION} VERSION_GREATER_EQUAL "3.24")
        set("OVERRIDE_FIND_PACKAGE_OPT" "OVERRIDE_FIND_PACKAGE")
      endif()

      if (HDF5_VOL_ALLOW_EXTERNAL MATCHES "GIT")
        FetchContent_Declare (${hdf5_vol_depname}
            GIT_REPOSITORY "${HDF5_VOL_SOURCE}"
            GIT_TAG "${HDF5_VOL_${hdf5_vol_name_upper}_BRANCH}"
            "${OVERRIDE_FIND_PACKAGE_OPT}"
        )
      elseif(HDF5_VOL_ALLOW_EXTERNAL MATCHES "LOCAL_DIR")
        FetchContent_Declare (${hdf5_vol_depname}
          SOURCE_DIR "${HDF5_VOL_SOURCE}"
        )
      endif()

      FetchContent_GetProperties(${hdf5_vol_depname})
      if (NOT ${hdf5_vol_depname}_POPULATED)
        FetchContent_Populate(${hdf5_vol_depname})

        # Now that content has been populated, set other internal
        # convenience variables for FetchContent dependency
        set (hdf5_vol_depname_source_dir "${${hdf5_vol_depname_lower}_SOURCE_DIR}")
        set (hdf5_vol_depname_binary_dir "${${hdf5_vol_depname_lower}_BINARY_DIR}")

        if (NOT EXISTS "${hdf5_vol_depname_source_dir}/CMakeLists.txt")
          if (HDF5_VOL_ALLOW_EXTERNAL MATCHES "GIT")
            message (SEND_ERROR "The git repository branch '${HDF5_VOL_${hdf5_vol_name_upper}_BRANCH}' for VOL connector '${hdf5_vol_name}' does not appear to contain a CMakeLists.txt file")
          elseif (HDF5_VOL_ALLOW_EXTERNAL MATCHES "LOCAL_DIR")
            message(SEND_ERROR "The local directory '${HDF5_VOL_SOURCE}' for VOL connector '${hdf5_vol_name}' does not appear to contain a CMakeLists.txt file")
          endif()
        endif ()

        # If there are any calls to find_package(HDF5) in the connector's
        # CMakeLists.txt files, remove those since any found HDF5 targets
        # will conflict with targets being generated by this build of HDF5
        if (EXISTS "${hdf5_vol_depname_source_dir}/CMakeLists.txt")
          file (READ "${hdf5_vol_depname_source_dir}/CMakeLists.txt" vol_cmake_contents)
          string (REGEX REPLACE "[ \t]*find_package[ \t]*\\([ \t]*HDF5[^\r\n\\)]*\\)[ \t]*[\r\n]+" "" vol_cmake_contents "${vol_cmake_contents}")
          file (WRITE "${hdf5_vol_depname_source_dir}/CMakeLists.txt" "${vol_cmake_contents}")
        endif ()
        if (EXISTS "${hdf5_vol_depname_source_dir}/src/CMakeLists.txt")
          file (READ "${hdf5_vol_depname_source_dir}/src/CMakeLists.txt" vol_cmake_contents)
          string (REGEX REPLACE "[ \t]*find_package[ \t]*\\([ \t]*HDF5[^\r\n\\)]*\\)[ \t]*[\r\n]+" "" vol_cmake_contents "${vol_cmake_contents}")
          file (WRITE "${hdf5_vol_depname_source_dir}/src/CMakeLists.txt" "${vol_cmake_contents}")
        endif ()

        # Apply any connector-specific workarounds
        apply_connector_workarounds ("${hdf5_vol_name_lower}" "${hdf5_vol_depname_source_dir}")

        add_subdirectory (${hdf5_vol_depname_source_dir} ${hdf5_vol_depname_binary_dir})

        # Get list of targets generated by build of connector
        get_generated_cmake_targets (connector_targets ${hdf5_vol_depname_source_dir})

        # Create a custom target for the connector to encompass all its
        # targets and other custom properties set by us for later use
        add_custom_target ("HDF5_VOL_${hdf5_vol_name_lower}")

        # Define and set a custom property on the VOL connector target to
        # capture all of the connector's generated targets
        define_property (
          TARGET
          PROPERTY HDF5_VOL_TARGETS
          BRIEF_DOCS "Generated targets of this connector"
          FULL_DOCS "Generated targets of this connector"
        )

        set_target_properties (
          "HDF5_VOL_${hdf5_vol_name_lower}"
          PROPERTIES
            HDF5_VOL_TARGETS "${connector_targets}"
        )

        # Define and set a custom property on the VOL connector target to
        # capture the connector's name to set for the HDF5_VOL_CONNECTOR
        # environment variable for testing
        define_property (
          TARGET
          PROPERTY HDF5_VOL_NAME
          BRIEF_DOCS "VOL connector name to use for the HDF5_VOL_CONNECTOR environment variable when testing"
          FULL_DOCS "VOL connector name to use for the HDF5_VOL_CONNECTOR environment variable when testing"
        )

        set_target_properties (
          "HDF5_VOL_${hdf5_vol_name_lower}"
          PROPERTIES
            HDF5_VOL_NAME "${HDF5_VOL_${hdf5_vol_name_upper}_NAME}"
        )

        # Define and set a custom property on the VOL connector target to
        # capture whether the connector should be tested with the parallel
        # API tests
        define_property (
          TARGET
          PROPERTY HDF5_VOL_TEST_PARALLEL
          BRIEF_DOCS "Whether the VOL connector should be tested with the parallel API tests"
          FULL_DOCS "Whether the VOL connector should be tested with the parallel API tests"
        )

        set_target_properties (
          "HDF5_VOL_${hdf5_vol_name_lower}"
          PROPERTIES
            HDF5_VOL_TEST_PARALLEL ${HDF5_VOL_${hdf5_vol_name_upper}_TEST_PARALLEL}
        )

        # Add this VOL connector's target to the list of external connector targets
        list (APPEND HDF5_EXTERNAL_VOL_TARGETS "HDF5_VOL_${hdf5_vol_name_lower}")

        # Get the list of library targets from this VOL connector
        unset (connector_lib_targets)
        foreach (connector_target ${connector_targets})
          get_target_property (target_type ${connector_target} TYPE)
          if (target_type STREQUAL "SHARED_LIBRARY" OR target_type STREQUAL "STATIC_LIBRARY")
            list (APPEND connector_lib_targets "${connector_target}")
          endif ()
        endforeach ()

        # Add all of the previous VOL connector's library targets as
        # dependencies for the current VOL connector to ensure that
        # VOL connectors get built serially in case there are dependencies
        if (DEFINED last_vol_lib_targets)
          foreach (connector_target ${connector_targets})
            add_dependencies (${connector_target} ${last_vol_lib_targets})
          endforeach ()
        endif ()

        # Use this connector's library targets as dependencies
        # for the next connector that is built
        set (last_vol_lib_targets "${connector_lib_targets}")
      endif ()
    endif ()
  endforeach ()
endif ()
