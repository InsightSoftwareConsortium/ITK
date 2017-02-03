# vxl/config/cmake/Modules/UseVXL.cmake
# (also copied by CMake to the top-level of the vxl build tree)
#
# This CMake file may be included by projects outside VXL.  It
# configures them to make use of VXL headers and libraries.  The file
# is written to work in one of two ways.
#
# The preferred way to use VXL from an outside project with UseVXL.cmake:
#
#  find_package(VXL)
#  if(VXL_FOUND)
#    include(${VXL_CMAKE_DIR}/UseVXL.cmake)
#  else()
#    message("VXL_DIR should be set to the VXL build directory.")
#  endif()
#
# Read vxl/config/cmake/VXLConfig.cmake for the list of variables
# provided.  The names have changed to reduce namespace pollution.
# The old names can be made available by placing this line before
# including UseVXL.cmake:
#
# This UseVXL.cmake no longer adds options and testing features automatically
# to projects including it unless this line appears before including it:
#
#  set(VXL_PROVIDE_STANDARD_OPTIONS 1)
#
# For example, in order to enable full backward-compatibility while
# still using FIND_PACKAGE, use these lines:
#
#  find_package(VXL)
#  if(VXL_FOUND)
#    set(VXL_PROVIDE_STANDARD_OPTIONS 1)
#    include(${VXL_CMAKE_DIR}/UseVXL.cmake)
#  else()
#    message("VXL_DIR should be set to the VXL build directory.")
#  endif()
#
# The old way to use VXL from an outside project with UseVXL.cmake is
# also supported for backward-compatibility:
#
#  set(VXL_BINARY_PATH "" CACHE PATH "VXL build directory (location of UseVXL.cmake)")
#  if(VXL_BINARY_PATH)
#    include(${VXL_BINARY_PATH}/UseVXL.cmake)
#  else()
#    message("VXL_BINARY_PATH should be set to the VXL build directory (location of UseVXL.cmake)" )
#  endif()
#

# If this file has been included directly by a user project instead of
# through VXL_USE_FILE from VXLConfig.cmake, simulate old behavior.
if(NOT VXL_CONFIG_CMAKE)
  if(VXL_BINARY_PATH)

    # Let FIND_PACKAGE import the VXLConfig.cmake module.
    set(VXL_DIR ${VXL_BINARY_PATH})
    find_package(VXL)

    # Enable compatibility mode.
    set(VXL_PROVIDE_STANDARD_OPTIONS 1)

  endif()
endif()

# VXLConfig.cmake has now been included.  Use its settings.
if(VXL_CONFIG_CMAKE)
  # Load the compiler settings used for VXL.
  if(VXL_BUILD_SETTINGS_FILE)
    option( VXL_IMPORT_BUILD_SETTINGS "Import build settings (compiler flags, generator) from VXL?" YES )
    mark_as_advanced( VXL_IMPORT_BUILD_SETTINGS )
    if( VXL_IMPORT_BUILD_SETTINGS )
      include(${CMAKE_ROOT}/Modules/CMakeImportBuildSettings.cmake)
      CMAKE_IMPORT_BUILD_SETTINGS(${VXL_BUILD_SETTINGS_FILE})
    endif()
  endif()

  # Use the standard VXL include directories.
  include_directories(SYSTEM ${VXL_VCL_INCLUDE_DIRS} ${VXL_CORE_INCLUDE_DIRS})

  # Add link directories needed to use VXL.
  link_directories(${VXL_LIBRARY_DIR})

  if(VXL_CMAKE_DOXYGEN_DIR)
    # Allow use of VXL's cmake/doxygen framework
    include(${VXL_CMAKE_DOXYGEN_DIR}/doxygen.cmake)
  endif()

  if(VXL_PROVIDE_STANDARD_OPTIONS)
    # Provide the standard set of VXL CMake options to the project.
    include(${VXL_CMAKE_DIR}/VXLStandardOptions.cmake)
  endif()
endif()
