if(POLICY CMP0025)
  cmake_policy(SET CMP0025 NEW)
endif()
if(POLICY CMP0042)
  cmake_policy(SET CMP0042 NEW)
endif()
PROJECT (HDF5 C CXX)

#-----------------------------------------------------------------------------
# Instructions for use : Normal Build
#
# For standard build of HDF5 libraries,tests and tools.
# Run cmake using the HDF5 source tree to generate a build tree.
# Enable/Disable options according to requirements and
# set CMAKE_INSTALL_PREFIX to the required install path.
# Make install can be used to install all components for system-wide use.
#
if ("${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_BINARY_DIR}")
    MESSAGE(FATAL_ERROR "\nERROR! ${PROJECT_NAME} DOES NOT SUPPORT IN SOURCE BUILDS!\n"
      "CMAKE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
      " == CMAKE_CURRENT_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}\n"
      "NEXT STEPS:\n"
      "(1) Delete the CMakeCache.txt file and the CMakeFiles/ directory\n"
      "     under the source directory for ${PROJECT_NAME}, otherwise you\n"
      "     will not be able to configure ${PROJECT_NAME} correctly!\n"
      "      * For example, on linux machines do:\n"
      "        $ rm -r CMakeCache.txt CMakeFiles/\n"
      "(2) Create a different directory and configure ${PROJECT_NAME} in that directory.\n"
      "      * For example, on linux machines do:\n"
      "        $ mkdir MY_BUILD\n"
      "        $ cd MY_BUILD\n"
      "        $ cmake [OPTIONS] ..\n"
      )
endif ()

#-----------------------------------------------------------------------------
# Instructions for use : Sub-Project Build
#
# To include HDF5 as a sub-project within another project.
# Set HDF5_EXTERNALLY_CONFIGURED to 1 in the parent project and
# supply values for the following variables...
#
# HDF5_EXPORTED_TARGETS :
#   Set this to the name of the targets variable which controls exports
#   If unset (because parent project does not support/use the
#   install (EXPORT target...) syntax), then targets are not configured
#   for export during install.
#
# HDF5_LIB_DEPENDENCIES :
#   If the build of HDF5 libs is being customized, then rules for the
#   dependencies of the HDF5 libs may be 'incomplete', add additional
#   dependencies to this variable so that external projects pick them up
#
# HDF5_EXTERNAL_LIB_PREFIX :
#   If the parent project needs to install hdf libraries, but avoid
#   name conflicts with system versions, then a prefix may be added
#   to ensure that the correct versions configured are used.
#
# HDF5_INSTALL_BIN_DIR, HDF5_INSTALL_LIB_DIR, HDF5_INSTALL_INCLUDE_DIR, HDF5_INSTALL_DATA_DIR :
#   Customize the 'bin', 'lib', 'include', and 'share' installation directories.
#
# HDF5_INSTALL_NO_DEVELOPMENT :
#   Set to true to skip installation of headers and CMake package files.
#
# Consider this example from the ParaView project, it builds its own zlib
# library and tells HDF5 to add it as a dependency - this ensures that
# any project making use of this build of HDF5 will use the correct zlib
#
#   # Tell hdf5 that we are manually overriding certain settings
#   set (HDF5_EXTERNALLY_CONFIGURED 1)
#   # Avoid duplicating names of installed libraries
#   set (HDF5_EXTERNAL_LIB_PREFIX "vtk")
#   # Export configuration to this export variable
#   set (HDF5_EXPORTED_TARGETS "paraview-targets")
#
#   # Setup all necessary overrides for zlib so that HDF5 uses our
#   # internally compiled zlib rather than any other version
#   if (HDF5_ENABLE_Z_LIB_SUPPORT)
#     # We must tell the main HDF5 library that it depends on our zlib
#     set (HDF5_LIB_DEPENDENCIES vtkzlib)
#     # Override the zlib header file
#     if (VTK_USE_SYSTEM_ZLIB)
#       set (H5_ZLIB_HEADER "zlib.h")
#     else (VTK_USE_SYSTEM_ZLIB)
#       set (H5_ZLIB_HEADER "vtk_zlib.h")
#       # Set vars that FindZlib would have set if used in sub project
#       set (ZLIB_INCLUDE_DIRS "${VTK_ZLIB_INCLUDE_DIRS}")
#       set (ZLIB_LIBRARIES vtkzlib)
#     endif (VTK_USE_SYSTEM_ZLIB)
#   endif (HDF5_ENABLE_Z_LIB_SUPPORT)
#
#   # Add the sub project
#   add_subdirectory (Utilities/hdf5-1.8)
#-----------------------------------------------------------------------------
#string(TIMESTAMP CONFIG_DATE "%Y-%m-%d")

#-----------------------------------------------------------------------------
# Allow Visual Studio solution directories
#-----------------------------------------------------------------------------
# Provide a way for Visual Studio Express users to turn OFF the new FOLDER
# organization feature. Default to ON for non-Express users. Express users must
# explicitly turn off this option to build HDF5 in the Express IDE...
#
option (HDF5_USE_FOLDERS "Enable folder grouping of projects in IDEs." ON)
mark_as_advanced (HDF5_USE_FOLDERS)
if (HDF5_USE_FOLDERS)
  set_property (GLOBAL PROPERTY USE_FOLDERS ON)
endif (HDF5_USE_FOLDERS)
option (HDF5_NO_PACKAGES "CPACK - Disable packaging" OFF)
mark_as_advanced (HDF5_NO_PACKAGES)
option (ALLOW_UNSUPPORTED "Allow unsupported combinations of configure options" OFF)
mark_as_advanced (ALLOW_UNSUPPORTED)

#-----------------------------------------------------------------------------
# Set the core names of all the libraries
#-----------------------------------------------------------------------------
set (HDF5_LIB_CORENAME              "hdf5")
set (HDF5_TEST_LIB_CORENAME         "hdf5_test")
set (HDF5_CPP_LIB_CORENAME          "hdf5_cpp")
set (HDF5_HL_LIB_CORENAME           "hdf5_hl")
set (HDF5_HL_CPP_LIB_CORENAME       "hdf5_hl_cpp")
set (HDF5_TOOLS_LIB_CORENAME        "hdf5_tools")
set (HDF5_F90_LIB_CORENAME          "hdf5_fortran")
set (HDF5_F90_C_LIB_CORENAME        "hdf5_f90cstub")
set (HDF5_F90_TEST_LIB_CORENAME     "hdf5_test_fortran")
set (HDF5_F90_C_TEST_LIB_CORENAME   "hdf5_test_f90cstub")
set (HDF5_HL_F90_LIB_CORENAME       "hdf5_hl_fortran")
set (HDF5_HL_F90_C_LIB_CORENAME     "hdf5_hl_f90cstub")

#-----------------------------------------------------------------------------
# Set the true names of all the libraries if customized by external project
#-----------------------------------------------------------------------------
set (HDF5_LIB_NAME              "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_CORENAME}")
set (HDF5_TEST_LIB_NAME         "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TEST_LIB_CORENAME}")
set (HDF5_CPP_LIB_NAME          "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_CPP_LIB_CORENAME}")
set (HDF5_HL_LIB_NAME           "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_HL_LIB_CORENAME}")
set (HDF5_HL_CPP_LIB_NAME       "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_HL_CPP_LIB_CORENAME}")
set (HDF5_TOOLS_LIB_NAME        "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TOOLS_LIB_CORENAME}")
set (HDF5_F90_LIB_NAME          "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_F90_LIB_CORENAME}")
set (HDF5_F90_C_LIB_NAME        "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_F90_C_LIB_CORENAME}")
set (HDF5_F90_TEST_LIB_NAME     "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_F90_TEST_LIB_CORENAME}")
set (HDF5_F90_C_TEST_LIB_NAME   "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_F90_C_TEST_LIB_CORENAME}")
set (HDF5_HL_F90_LIB_NAME       "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_HL_F90_LIB_CORENAME}")
set (HDF5_HL_F90_C_LIB_NAME     "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_HL_F90_C_LIB_CORENAME}")

#-----------------------------------------------------------------------------
# Set the target names of all the libraries
#-----------------------------------------------------------------------------
set (HDF5_LIB_TARGET              "${HDF5_LIB_CORENAME}-static")
set (HDF5_TEST_LIB_TARGET         "${HDF5_TEST_LIB_CORENAME}-static")
set (HDF5_CPP_LIB_TARGET          "${HDF5_CPP_LIB_CORENAME}-static")
set (HDF5_HL_LIB_TARGET           "${HDF5_HL_LIB_CORENAME}-static")
set (HDF5_HL_CPP_LIB_TARGET       "${HDF5_HL_CPP_LIB_CORENAME}-static")
set (HDF5_TOOLS_LIB_TARGET        "${HDF5_TOOLS_LIB_CORENAME}-static")
set (HDF5_F90_LIB_TARGET          "${HDF5_F90_LIB_CORENAME}-static")
set (HDF5_F90_C_LIB_TARGET        "${HDF5_F90_C_LIB_CORENAME}-static")
set (HDF5_F90_TEST_LIB_TARGET     "${HDF5_F90_TEST_LIB_CORENAME}-static")
set (HDF5_F90_C_TEST_LIB_TARGET   "${HDF5_F90_C_TEST_LIB_CORENAME}-static")
set (HDF5_HL_F90_LIB_TARGET       "${HDF5_HL_F90_LIB_CORENAME}-static")
set (HDF5_HL_F90_C_LIB_TARGET     "${HDF5_HL_F90_C_LIB_CORENAME}-static")
set (HDF5_LIBSH_TARGET            "${HDF5_LIB_CORENAME}-shared")
set (HDF5_TEST_LIBSH_TARGET       "${HDF5_TEST_LIB_CORENAME}-shared")
set (HDF5_CPP_LIBSH_TARGET        "${HDF5_CPP_LIB_CORENAME}-shared")
set (HDF5_HL_LIBSH_TARGET         "${HDF5_HL_LIB_CORENAME}-shared")
set (HDF5_HL_CPP_LIBSH_TARGET     "${HDF5_HL_CPP_LIB_CORENAME}-shared")
set (HDF5_TOOLS_LIBSH_TARGET      "${HDF5_TOOLS_LIB_CORENAME}-shared")
set (HDF5_F90_LIBSH_TARGET        "${HDF5_F90_LIB_CORENAME}-shared")
set (HDF5_F90_C_LIBSH_TARGET      "${HDF5_F90_C_LIB_CORENAME}-shared")
set (HDF5_F90_TEST_LIBSH_TARGET   "${HDF5_F90_TEST_LIB_CORENAME}-shared")
set (HDF5_F90_C_TEST_LIBSH_TARGET "${HDF5_F90_C_TEST_LIB_CORENAME}-shared")
set (HDF5_HL_F90_LIBSH_TARGET     "${HDF5_HL_F90_LIB_CORENAME}-shared")
set (HDF5_HL_F90_C_LIBSH_TARGET   "${HDF5_HL_F90_C_LIB_CORENAME}-shared")

#-----------------------------------------------------------------------------
# Define some CMake variables for use later in the project
#-----------------------------------------------------------------------------
set (HDF_RESOURCES_DIR       ${HDF5_SOURCE_DIR}/config/cmake)
set (HDF_RESOURCES_EXT_DIR   ${HDF5_SOURCE_DIR}/config/cmake_ext_mod)
set (HDF5_SRC_DIR            ${HDF5_SOURCE_DIR}/src)
set (HDF5_TEST_SRC_DIR       ${HDF5_SOURCE_DIR}/test)
set (HDF5_CPP_SRC_DIR        ${HDF5_SOURCE_DIR}/c++)
set (HDF5_CPP_TST_DIR        ${HDF5_SOURCE_DIR}/c++/test)
set (HDF5_HL_SRC_DIR         ${HDF5_SOURCE_DIR}/hl)
set (HDF5_HL_CPP_SRC_DIR     ${HDF5_SOURCE_DIR}/hl/c++)
set (HDF5_TOOLS_SRC_DIR      ${HDF5_SOURCE_DIR}/tools)
set (HDF5_PERFORM_SRC_DIR    ${HDF5_SOURCE_DIR}/tools/perform)
set (HDF5_F90_SRC_DIR        ${HDF5_SOURCE_DIR}/fortran)

if (NOT HDF5_INSTALL_BIN_DIR)
  set (HDF5_INSTALL_BIN_DIR bin)
endif (NOT HDF5_INSTALL_BIN_DIR)
if (NOT HDF5_INSTALL_LIB_DIR)
  if (APPLE)
    set (HDF5_INSTALL_FMWK_DIR ${CMAKE_INSTALL_FRAMEWORK_PREFIX})
  endif (APPLE)
  set (HDF5_INSTALL_LIB_DIR lib)
endif (NOT HDF5_INSTALL_LIB_DIR)
if (NOT HDF5_INSTALL_INCLUDE_DIR)
  set (HDF5_INSTALL_INCLUDE_DIR include)
endif (NOT HDF5_INSTALL_INCLUDE_DIR)
option (HDF5_BUILD_FRAMEWORKS "TRUE to build as frameworks libraries, FALSE to build according to BUILD_SHARED_LIBS" FALSE)
if (NOT HDF5_INSTALL_DATA_DIR)
  if (NOT WIN32)
    if (APPLE)
      if (HDF5_BUILD_FRAMEWORKS)
        set (HDF5_INSTALL_EXTRA_DIR ../SharedSupport)
      else (HDF5_BUILD_FRAMEWORKS)
        set (HDF5_INSTALL_EXTRA_DIR share)
      endif (HDF5_BUILD_FRAMEWORKS)
      set (HDF5_INSTALL_FWRK_DIR ${CMAKE_INSTALL_FRAMEWORK_PREFIX})
    endif (APPLE)
    set (HDF5_INSTALL_DATA_DIR share)
    set (HDF5_INSTALL_CMAKE_DIR share/cmake)
  else (NOT WIN32)
    set (HDF5_INSTALL_DATA_DIR ".")
    set (HDF5_INSTALL_CMAKE_DIR cmake)
  endif (NOT WIN32)
endif (NOT HDF5_INSTALL_DATA_DIR)

#-----------------------------------------------------------------------------
# parse the full version number from H5public.h and include in H5_VERS_INFO
#-----------------------------------------------------------------------------
file (READ ${HDF5_SRC_DIR}/H5public.h _h5public_h_contents)
string (REGEX REPLACE ".*#define[ \t]+H5_VERS_MAJOR[ \t]+([0-9]*).*$"
    "\\1" H5_VERS_MAJOR ${_h5public_h_contents})
string (REGEX REPLACE ".*#define[ \t]+H5_VERS_MINOR[ \t]+([0-9]*).*$"
    "\\1" H5_VERS_MINOR ${_h5public_h_contents})
string (REGEX REPLACE ".*#define[ \t]+H5_VERS_RELEASE[ \t]+([0-9]*).*$"
    "\\1" H5_VERS_RELEASE ${_h5public_h_contents})
string (REGEX REPLACE ".*#define[ \t]+H5_VERS_SUBRELEASE[ \t]+\"([0-9A-Za-z._]*)\".*$"
    "\\1" H5_VERS_SUBRELEASE ${_h5public_h_contents})
#message (STATUS "VERSION: ${H5_VERS_MAJOR}.${H5_VERS_MINOR}.${H5_VERS_RELEASE}-${H5_VERS_SUBRELEASE}")

#-----------------------------------------------------------------------------
# parse the full soversion number from config/lt_vers.am and include in H5_SOVERS_INFO
#-----------------------------------------------------------------------------
file (READ ${HDF5_SOURCE_DIR}/config/lt_vers.am _lt_vers_am_contents)
string (REGEX REPLACE ".*LT_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_SOVERS_INTERFACE ${_lt_vers_am_contents})
string (REGEX REPLACE ".*LT_VERS_REVISION[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_SOVERS_MINOR ${_lt_vers_am_contents})
string (REGEX REPLACE ".*LT_VERS_AGE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_SOVERS_RELEASE ${_lt_vers_am_contents})
MATH (EXPR H5_SOVERS_MAJOR ${H5_SOVERS_INTERFACE}-${H5_SOVERS_RELEASE})
#message (STATUS "SOVERSION: ${H5_SOVERS_MAJOR}.${H5_SOVERS_RELEASE}.${H5_SOVERS_MINOR}")
string (REGEX REPLACE ".*LT_TOOLS_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_TOOLS_SOVERS_INTERFACE ${_lt_vers_am_contents})
string (REGEX REPLACE ".*LT_TOOLS_VERS_REVISION[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_TOOLS_SOVERS_MINOR ${_lt_vers_am_contents})
string (REGEX REPLACE ".*LT_TOOLS_VERS_AGE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_TOOLS_SOVERS_RELEASE ${_lt_vers_am_contents})
MATH (EXPR H5_TOOLS_SOVERS_MAJOR ${H5_TOOLS_SOVERS_INTERFACE}-${H5_TOOLS_SOVERS_RELEASE})
#message (STATUS "SOVERSION_TOOLS: ${H5_TOOLS_SOVERS_MAJOR}.${H5_TOOLS_SOVERS_RELEASE}.${H5_TOOLS_SOVERS_MINOR}")
string (REGEX REPLACE ".*LT_CXX_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_CXX_SOVERS_INTERFACE ${_lt_vers_am_contents})
string (REGEX REPLACE ".*LT_CXX_VERS_REVISION[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_CXX_SOVERS_MINOR ${_lt_vers_am_contents})
string (REGEX REPLACE ".*LT_CXX_VERS_AGE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_CXX_SOVERS_RELEASE ${_lt_vers_am_contents})
MATH (EXPR H5_CXX_SOVERS_MAJOR ${H5_CXX_SOVERS_INTERFACE}-${H5_CXX_SOVERS_RELEASE})
#message (STATUS "SOVERSION_CXX: ${H5_CXX_SOVERS_MAJOR}.${H5_CXX_SOVERS_RELEASE}.${H5_CXX_SOVERS_MINOR}")
string (REGEX REPLACE ".*LT_F_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_F_SOVERS_INTERFACE ${_lt_vers_am_contents})
string (REGEX REPLACE ".*LT_F_VERS_REVISION[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_F_SOVERS_MINOR ${_lt_vers_am_contents})
string (REGEX REPLACE ".*LT_F_VERS_AGE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_F_SOVERS_RELEASE ${_lt_vers_am_contents})
MATH (EXPR H5_F_SOVERS_MAJOR ${H5_F_SOVERS_INTERFACE}-${H5_F_SOVERS_RELEASE})
#message (STATUS "SOVERSION_F: ${H5_F_SOVERS_MAJOR}.${H5_F_SOVERS_RELEASE}.${H5_F_SOVERS_MINOR}")
string (REGEX REPLACE ".*LT_HL_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_HL_SOVERS_INTERFACE ${_lt_vers_am_contents})
string (REGEX REPLACE ".*LT_HL_VERS_REVISION[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_HL_SOVERS_MINOR ${_lt_vers_am_contents})
string (REGEX REPLACE ".*LT_HL_VERS_AGE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_HL_SOVERS_RELEASE ${_lt_vers_am_contents})
MATH (EXPR H5_HL_SOVERS_MAJOR ${H5_HL_SOVERS_INTERFACE}-${H5_HL_SOVERS_RELEASE})
#message (STATUS "SOVERSION_HL: ${H5_HL_SOVERS_MAJOR}.${H5_HL_SOVERS_RELEASE}.${H5_HL_SOVERS_MINOR}")
string (REGEX REPLACE ".*LT_HL_CXX_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_HL_CXX_SOVERS_INTERFACE ${_lt_vers_am_contents})
string (REGEX REPLACE ".*LT_HL_CXX_VERS_REVISION[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_HL_CXX_SOVERS_MINOR ${_lt_vers_am_contents})
string (REGEX REPLACE ".*LT_HL_CXX_VERS_AGE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_HL_CXX_SOVERS_RELEASE ${_lt_vers_am_contents})
MATH (EXPR H5_HL_CXX_SOVERS_MAJOR ${H5_HL_CXX_SOVERS_INTERFACE}-${H5_HL_CXX_SOVERS_RELEASE})
#message (STATUS "SOVERSION_HL_CXX: ${H5_HL_CXX_SOVERS_MAJOR}.${H5_HL_CXX_SOVERS_RELEASE}.${H5_HL_CXX_SOVERS_MINOR}")
string (REGEX REPLACE ".*LT_HL_F_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_HL_F_SOVERS_INTERFACE ${_lt_vers_am_contents})
string (REGEX REPLACE ".*LT_HL_F_VERS_REVISION[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_HL_F_SOVERS_MINOR ${_lt_vers_am_contents})
string (REGEX REPLACE ".*LT_HL_F_VERS_AGE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_HL_F_SOVERS_RELEASE ${_lt_vers_am_contents})
MATH (EXPR H5_HL_F_SOVERS_MAJOR ${H5_HL_F_SOVERS_INTERFACE}-${H5_HL_F_SOVERS_RELEASE})
#message (STATUS "SOVERSION_HL_F: ${H5_HL_F_SOVERS_MAJOR}.${H5_HL_F_SOVERS_RELEASE}.${H5_HL_F_SOVERS_MINOR}")

#-----------------------------------------------------------------------------
# Basic HDF5 stuff here
#-----------------------------------------------------------------------------
set (HDF5_PACKAGE "hdf5")
set (HDF5_PACKAGE_NAME "HDF5")
set (HDF5_PACKAGE_VERSION "${H5_VERS_MAJOR}.${H5_VERS_MINOR}.${H5_VERS_RELEASE}")
set (HDF5_PACKAGE_VERSION_MAJOR "${H5_VERS_MAJOR}.${H5_VERS_MINOR}")
set (HDF5_PACKAGE_VERSION_MINOR "${H5_VERS_RELEASE}")
if (NOT "${H5_VERS_SUBRELEASE}" STREQUAL "")
  set (HDF5_PACKAGE_VERSION_STRING "${HDF5_PACKAGE_VERSION}-${H5_VERS_SUBRELEASE}")
else (NOT "${H5_VERS_SUBRELEASE}" STREQUAL "")
  set (HDF5_PACKAGE_VERSION_STRING "${HDF5_PACKAGE_VERSION}")
endif (NOT "${H5_VERS_SUBRELEASE}" STREQUAL "")
set (HDF5_PACKAGE_SOVERSION "${H5_SOVERS_MAJOR}.${H5_SOVERS_RELEASE}.${H5_SOVERS_MINOR}")
set (HDF5_TOOLS_PACKAGE_SOVERSION "${H5_TOOLS_SOVERS_MAJOR}.${H5_TOOLS_SOVERS_RELEASE}.${H5_TOOLS_SOVERS_MINOR}")
set (HDF5_CXX_PACKAGE_SOVERSION "${H5_CXX_SOVERS_MAJOR}.${H5_CXX_SOVERS_RELEASE}.${H5_CXX_SOVERS_MINOR}")
set (HDF5_F_PACKAGE_SOVERSION "${H5_F_SOVERS_MAJOR}.${H5_F_SOVERS_RELEASE}.${H5_F_SOVERS_MINOR}")
set (HDF5_HL_PACKAGE_SOVERSION "${H5_HL_SOVERS_MAJOR}.${H5_HL_SOVERS_RELEASE}.${H5_HL_SOVERS_MINOR}")
set (HDF5_HL_CXX_PACKAGE_SOVERSION "${H5_HL_CXX_SOVERS_MAJOR}.${H5_HL_CXX_SOVERS_RELEASE}.${H5_HL_CXX_SOVERS_MINOR}")
set (HDF5_HL_F_PACKAGE_SOVERSION "${H5_HL_F_SOVERS_MAJOR}.${H5_HL_F_SOVERS_RELEASE}.${H5_HL_F_SOVERS_MINOR}")
set (HDF5_PACKAGE_STRING "${HDF5_PACKAGE_NAME} ${HDF5_PACKAGE_VERSION_STRING}")
set (HDF5_PACKAGE_TARNAME "${HDF5_PACKAGE}${HDF_PACKAGE_EXT}")
set (HDF5_PACKAGE_URL "http://www.hdfgroup.org")
set (HDF5_PACKAGE_BUGREPORT "help@hdfgroup.org")

#-----------------------------------------------------------------------------
# Include some macros for reusable code
#-----------------------------------------------------------------------------
include (${HDF_RESOURCES_EXT_DIR}/HDFMacros.cmake)
include (${HDF_RESOURCES_EXT_DIR}/HDFLibMacros.cmake)
include (${HDF_RESOURCES_DIR}/HDF5Macros.cmake)

#-----------------------------------------------------------------------------
# Setup output Directories
#-----------------------------------------------------------------------------
if (NOT HDF5_EXTERNALLY_CONFIGURED)
  set (CMAKE_RUNTIME_OUTPUT_DIRECTORY
      ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all Executables."
  )
  set (CMAKE_LIBRARY_OUTPUT_DIRECTORY
      ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all Libraries"
  )
  set (CMAKE_ARCHIVE_OUTPUT_DIRECTORY
      ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all static libraries."
  )
  set (CMAKE_Fortran_MODULE_DIRECTORY
      ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all fortran modules."
  )
else (NOT HDF5_EXTERNALLY_CONFIGURED)
  # if we are externally configured, but the project uses old cmake scripts
  # this may not be set and utilities like H5detect will fail
  if (NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)
    set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${EXECUTABLE_OUTPUT_PATH})
  endif (NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)
endif (NOT HDF5_EXTERNALLY_CONFIGURED)

#-----------------------------------------------------------------------------
# Targets built within this project are exported at Install time for use
# by other projects using FindHDF5.
#-----------------------------------------------------------------------------
if (NOT HDF5_EXPORTED_TARGETS)
  set (HDF5_EXPORTED_TARGETS "hdf5-targets")
endif (NOT HDF5_EXPORTED_TARGETS)

#-----------------------------------------------------------------------------
# To include a library in the list exported by the project AT BUILD TIME,
# add it to this variable. This is NOT used by Make Install, but for projects
# which include hdf5 as a sub-project within their build tree
#-----------------------------------------------------------------------------
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "")
set (EXTERNAL_HEADER_LIST "")
set (EXTERNAL_LIBRARY_LIST "")
set (EXTERNAL_LIBRARYDLL_LIST "")

#-----------------------------------------------------------------------------
# Run all the CMake configuration tests for our build environment
#-----------------------------------------------------------------------------
include (${HDF_RESOURCES_DIR}/ConfigureChecks.cmake)

set (CMAKE_INCLUDE_CURRENT_DIR_IN_INTERFACE ON)

#-----------------------------------------------------------------------------
# Mac OS X Options
#-----------------------------------------------------------------------------
if (HDF5_BUILD_FRAMEWORKS AND NOT BUILD_SHARED_LIBS)
  set (BUILD_SHARED_LIBS ON CACHE BOOL "Build Shared Libraries")
endif (HDF5_BUILD_FRAMEWORKS AND NOT BUILD_SHARED_LIBS)

#-----------------------------------------------------------------------------
# Option to Build Shared and Static libs, default is static
#-----------------------------------------------------------------------------
option (BUILD_SHARED_LIBS "Build Shared Libraries" ON)
set (H5_ENABLE_SHARED_LIB NO)
if (BUILD_SHARED_LIBS)
  set (H5_ENABLE_SHARED_LIB YES)
  set (LINK_SHARED_LIBS ${LINK_LIBS})
endif (BUILD_SHARED_LIBS)
set (H5_ENABLE_STATIC_LIB YES)
set (CMAKE_POSITION_INDEPENDENT_CODE ON)

#-----------------------------------------------------------------------------
# Option to Build Static executables
#-----------------------------------------------------------------------------
option (BUILD_STATIC_EXECS "Build Static Executabless" OFF)
if (BUILD_STATIC_EXECS)
  if (NOT WIN32)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -static")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -static")
  endif (NOT WIN32)
endif (BUILD_STATIC_EXECS)

#-----------------------------------------------------------------------------
# Option to use code coverage
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_COVERAGE "Enable code coverage for Libraries and Programs" OFF)
if (HDF5_ENABLE_COVERAGE)
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -O0 -fprofile-arcs -ftest-coverage")
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O0 -fprofile-arcs -ftest-coverage")
  set (LDFLAGS "${LDFLAGS} -fprofile-arcs -ftest-coverage")
endif (HDF5_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# Option to indicate using dmalloc
#-----------------------------------------------------------------------------
# option (HDF5_ENABLE_USING_DMALLOC "Indicate that dmalloc is used" OFF)
# if (HDF5_ENABLE_USING_DMALLOC)
#   find_package (DMALLOC)
#   set (H5_HAVE_DMALLOC DMALLOC_FOUND)
# endif (HDF5_ENABLE_USING_DMALLOC)

#-----------------------------------------------------------------------------
# Option to indicate using a memory checker
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_USING_MEMCHECKER)
  set (H5_USING_MEMCHECKER 1)
endif (HDF5_ENABLE_USING_MEMCHECKER)

#-----------------------------------------------------------------------------
# Option to use deprecated public API symbols
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_DEPRECATED_SYMBOLS "Enable deprecated public API symbols" ON)
if (HDF5_ENABLE_DEPRECATED_SYMBOLS)
  set (H5_NO_DEPRECATED_SYMBOLS 0)
else (HDF5_ENABLE_DEPRECATED_SYMBOLS)
  set (H5_NO_DEPRECATED_SYMBOLS 1)
endif (HDF5_ENABLE_DEPRECATED_SYMBOLS)

#-----------------------------------------------------------------------------
# When building utility executables that generate other (source) files :
# we make use of the following variables defined in the root CMakeLists.
# Certain systems may add /Debug or /Release to output paths
# and we need to call the executable from inside the CMake configuration
#-----------------------------------------------------------------------------
set (EXE_EXT "${CMAKE_EXECUTABLE_SUFFIX}")
if (WIN32)
  set (EXE_EXT ".exe")
  if (NOT CYGWIN)
    add_definitions (-D_BIND_TO_CURRENT_VCLIBS_VERSION=1)
    add_definitions (-D_CRT_SECURE_NO_WARNINGS)
    add_definitions (-D_CONSOLE)
  endif (NOT CYGWIN)
endif (WIN32)

if (MSVC)
  set (CMAKE_MFC_FLAG 0)
  set (WIN_COMPILE_FLAGS "")
  set (WIN_LINK_FLAGS "")
endif (MSVC)

set (MAKE_SYSTEM)
if (CMAKE_BUILD_TOOL MATCHES "make")
  set (MAKE_SYSTEM 1)
endif (CMAKE_BUILD_TOOL MATCHES "make")

set (CFG_INIT "/${CMAKE_CFG_INTDIR}")
if (MAKE_SYSTEM)
  set (CFG_INIT "")
endif (MAKE_SYSTEM)

#-----------------------------------------------------------------------------
# Add some definitions for Debug Builds
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_TRACE "Enable API tracing capability" OFF)
mark_as_advanced (HDF5_ENABLE_TRACE)
if (CMAKE_BUILD_TYPE MATCHES Debug)
  add_definitions (-DDEBUG)
  # Enable tracing of the API
  if (HDF5_ENABLE_TRACE)
    add_definitions (-DH5_DEBUG_API )
  endif (HDF5_ENABLE_TRACE)
  # Enable instrumenting of the library's internal operations
  option (HDF5_ENABLE_INSTRUMENT "Instrument The library" OFF)
  if (HDF5_ENABLE_INSTRUMENT)
    set (H5_HAVE_INSTRUMENTED_LIBRARY 1)
  endif (HDF5_ENABLE_INSTRUMENT)
  mark_as_advanced (HDF5_ENABLE_INSTRUMENT)
else (CMAKE_BUILD_TYPE MATCHES Debug)
  add_definitions (-DNDEBUG)
  if (HDF5_ENABLE_TRACE)
    add_definitions (-DH5_DEBUG_API )
  endif (HDF5_ENABLE_TRACE)
endif (CMAKE_BUILD_TYPE MATCHES Debug)

#-----------------------------------------------------------------------------
# Option to embed library info into executables
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_EMBEDDED_LIBINFO "embed library info into executables" ON)
if (HDF5_ENABLE_EMBEDDED_LIBINFO)
  set (H5_HAVE_EMBEDDED_LIBINFO 1)
endif (HDF5_ENABLE_EMBEDDED_LIBINFO)

include (${HDF_RESOURCES_DIR}/HDFCompilerFlags.cmake)

#-----------------------------------------------------------------------------
# All libs/tests/examples need the main include directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_BINARY_DIR} ${HDF5_SRC_DIR} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})

#-----------------------------------------------------------------------------
# Option to Enable MPI Parallel
#-----------------------------------------------------------------------------
set (CMAKE_MODULE_PATH ${HDF_RESOURCES_DIR} ${HDF_RESOURCES_EXT_DIR} ${CMAKE_MODULE_PATH})
option (HDF5_ENABLE_PARALLEL "Enable parallel build (requires MPI)" OFF)
if (HDF5_ENABLE_PARALLEL)
  include (FindMPI)
  INCLUDE_DIRECTORIES (${MPI_C_INCLUDE_PATH})
  if (MPI_C_FOUND)
    set (H5_HAVE_PARALLEL 1)
    # MPI checks, only do these if MPI_C_FOUND is true, otherwise they always fail
    # and once set, they are cached as false and not regenerated
    set (CMAKE_REQUIRED_LIBRARIES "${MPI_C_LIBRARIES}" )
    # Used by Fortran + MPI
    CHECK_SYMBOL_EXISTS (MPI_Comm_c2f "${MPI_C_INCLUDE_PATH}/mpi.h"  H5_HAVE_MPI_MULTI_LANG_Comm)
    CHECK_SYMBOL_EXISTS (MPI_Info_c2f "${MPI_C_INCLUDE_PATH}/mpi.h"  H5_HAVE_MPI_MULTI_LANG_Info)
  else (MPI_C_FOUND)
    message (STATUS "Parallel libraries not found")
  endif (MPI_C_FOUND)
endif (HDF5_ENABLE_PARALLEL)

# Parallel IO usage requires MPI to be Linked and Included
if (H5_HAVE_PARALLEL)
  set (LINK_LIBS ${LINK_LIBS} ${MPI_C_LIBRARIES})
  set (LINK_SHARED_LIBS ${LINK_SHARED_LIBS} ${MPI_C_LIBRARIES})
  if (MPI_C_LINK_FLAGS)
    set (CMAKE_EXE_LINKER_FLAGS "${MPI_C_LINK_FLAGS} ${CMAKE_EXE_LINKER_FLAGS}")
  endif (MPI_C_LINK_FLAGS)
endif (H5_HAVE_PARALLEL)

set (DEFAULT_API_VERSION "v18")
#-----------------------------------------------------------------------------
# Option to use 1.6.x API
#-----------------------------------------------------------------------------
option (HDF5_USE_16_API_DEFAULT "Use the HDF5 1.6.x API by default" OFF)
set (H5_USE_16_API_DEFAULT 0)
if (HDF5_USE_16_API_DEFAULT)
  set (H5_USE_16_API_DEFAULT 1)
  set (DEFAULT_API_VERSION "v16")
endif (HDF5_USE_16_API_DEFAULT)

#-----------------------------------------------------------------------------
# Include user macros
#-----------------------------------------------------------------------------
include (UserMacros.cmake)

#-----------------------------------------------------------------------------
# Include filter (zlib, szip, etc.) macros
#-----------------------------------------------------------------------------
include (CMakeFilters.cmake)

#-----------------------------------------------------------------------------
# Option for external libraries on windows
#-----------------------------------------------------------------------------
option (HDF5_PACKAGE_EXTLIBS "CPACK - include external libraries" OFF)
if (NOT HDF5_EXTERNALLY_CONFIGURED)
  if (HDF5_PACKAGE_EXTLIBS)
    set (HDF5_NO_PACKAGES OFF CACHE BOOL "CPACK - Disable packaging" FORCE)
    if (HDF5_ENABLE_Z_LIB_SUPPORT AND ZLIB_FOUND)
      PACKAGE_ZLIB_LIBRARY (${HDF5_ALLOW_EXTERNAL_SUPPORT})
    endif (HDF5_ENABLE_Z_LIB_SUPPORT AND ZLIB_FOUND)

    if (HDF5_ENABLE_SZIP_SUPPORT AND SZIP_FOUND)
      PACKAGE_SZIP_LIBRARY (${HDF5_ALLOW_EXTERNAL_SUPPORT})
    endif (HDF5_ENABLE_SZIP_SUPPORT AND SZIP_FOUND)
  endif (HDF5_PACKAGE_EXTLIBS)
endif (NOT HDF5_EXTERNALLY_CONFIGURED)
#-----------------------------------------------------------------------------
# Option to use threadsafe
# Note: Currently CMake only allows configuring of threadsafe on
# non-Cygwin WINDOWS.
#-----------------------------------------------------------------------------
if (WIN32)
  option (HDF5_ENABLE_THREADSAFE "Enable thread-safety" OFF)
  if (HDF5_ENABLE_THREADSAFE)
    # check for unsupported options
    message (STATUS " **** thread-safety option not supported with static library **** ")
    message (STATUS " **** thread-safety option will not be used building static library **** ")
    if (HDF5_ENABLE_PARALLEL)
      if (NOT ALLOW_UNSUPPORTED)
        message (FATAL_ERROR " **** parallel and thread-safety options are not supported **** ")
      else (NOT ALLOW_UNSUPPORTED)
        message (STATUS " **** Allowing unsupported parallel and thread-safety options **** ")
      endif (NOT ALLOW_UNSUPPORTED)
    endif (HDF5_ENABLE_PARALLEL)
    if (HDF5_BUILD_FORTRAN)
      if (NOT ALLOW_UNSUPPORTED)
        message (FATAL_ERROR " **** Fortran and thread-safety options are not supported **** ")
      else (NOT ALLOW_UNSUPPORTED)
        message (STATUS " **** Allowing unsupported Fortran and thread-safety options **** ")
      endif (NOT ALLOW_UNSUPPORTED)
    endif (HDF5_BUILD_FORTRAN)
    if (HDF5_BUILD_CPP_LIB)
      if (NOT ALLOW_UNSUPPORTED)
        message (FATAL_ERROR " **** C++ and thread-safety options are not supported **** ")
      else (NOT ALLOW_UNSUPPORTED)
        message (STATUS " **** Allowing unsupported C++ and thread-safety options **** ")
      endif (NOT ALLOW_UNSUPPORTED)
    endif (HDF5_BUILD_CPP_LIB)
    if (HDF5_BUILD_HL_LIB)
      if (NOT ALLOW_UNSUPPORTED)
        message (FATAL_ERROR " **** HL and thread-safety options are not supported **** ")
      else (NOT ALLOW_UNSUPPORTED)
        message (STATUS " **** Allowing unsupported HL and thread-safety options **** ")
      endif (NOT ALLOW_UNSUPPORTED)
    endif (HDF5_BUILD_HL_LIB)
    if (H5_HAVE_IOEO)
      message (STATUS " **** Win32 threads requires WINVER>=0x600 (Windows Vista/7/8) **** ")
      set (H5_HAVE_WIN_THREADS 1)
    else (H5_HAVE_IOEO)
      if (NOT H5_HAVE_PTHREAD_H)
        message (FATAL_ERROR " **** thread-safe option requires Win32 threads or Pthreads **** ")
      endif (NOT H5_HAVE_PTHREAD_H)
    endif (H5_HAVE_IOEO)
  endif (HDF5_ENABLE_THREADSAFE)
endif (WIN32)

#-----------------------------------------------------------------------------
# Add the HDF5 Library Target to the build
#-----------------------------------------------------------------------------
add_subdirectory (${HDF5_SOURCE_DIR}/src ${PROJECT_BINARY_DIR}/src)

if (HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "SVN" OR HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "TGZ")
  if (ZLIB_FOUND AND ZLIB_USE_EXTERNAL)
    if (BUILD_SHARED_LIBS)
      add_dependencies (${HDF5_LIBSH_TARGET} ZLIB)
    else()
      ADD_DEPENDENCIES (${HDF5_LIB_TARGET} ZLIB)
    endif (BUILD_SHARED_LIBS)
  endif (ZLIB_FOUND AND ZLIB_USE_EXTERNAL)
  if (SZIP_FOUND AND SZIP_USE_EXTERNAL)
    if (BUILD_SHARED_LIBS)
      add_dependencies (${HDF5_LIBSH_TARGET} SZIP)
    else()
      ADD_DEPENDENCIES (${HDF5_LIB_TARGET} SZIP)
    endif (BUILD_SHARED_LIBS)
  endif (SZIP_FOUND AND SZIP_USE_EXTERNAL)
endif (HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "SVN" OR HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "TGZ")

#-----------------------------------------------------------------------------
# Dashboard and Testing Settings
#-----------------------------------------------------------------------------
option (BUILD_TESTING "Build HDF5 Unit Testing" ON)
if (BUILD_TESTING)
  set (DART_TESTING_TIMEOUT 1200
      CACHE INTEGER
      "Timeout in seconds for each test (default 1200=20minutes)"
  )

  option (HDF5_TEST_VFD "Execute tests with different VFDs" OFF)
  mark_as_advanced (HDF5_TEST_VFD)
  if (HDF5_TEST_VFD)
    option (HDF5_TEST_FHEAP_VFD "Execute tests with different VFDs" ON)
    mark_as_advanced (HDF5_TEST_FHEAP_VFD)
  endif (HDF5_TEST_VFD)

  option (HDF_TEST_EXPRESS "Control testing framework (0-3)" "0")
  mark_as_advanced (HDF_TEST_EXPRESS)

  include (${HDF5_SOURCE_DIR}/CTestConfig.cmake)
  configure_file (${HDF_RESOURCES_DIR}/CTestCustom.cmake ${HDF5_BINARY_DIR}/CTestCustom.ctest @ONLY)

  enable_testing ()
  include (CTest)

  if (NOT HDF5_EXTERNALLY_CONFIGURED)
    if (EXISTS "${HDF5_SOURCE_DIR}/test" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/test")
      add_subdirectory (${HDF5_SOURCE_DIR}/tools/lib ${PROJECT_BINARY_DIR}/tools/lib)
      add_subdirectory (${HDF5_SOURCE_DIR}/test      ${PROJECT_BINARY_DIR}/test)
    endif (EXISTS "${HDF5_SOURCE_DIR}/test" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/test")
    if (H5_HAVE_PARALLEL)
      if (EXISTS "${HDF5_SOURCE_DIR}/testpar" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/testpar")
        add_subdirectory (${HDF5_SOURCE_DIR}/testpar ${PROJECT_BINARY_DIR}/testpar)
      endif (EXISTS "${HDF5_SOURCE_DIR}/testpar" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/testpar")
    endif (H5_HAVE_PARALLEL)
  endif (NOT HDF5_EXTERNALLY_CONFIGURED)
endif (BUILD_TESTING)

#-----------------------------------------------------------------------------
# Option to build HDF5 Tools
#-----------------------------------------------------------------------------
if (EXISTS "${HDF5_SOURCE_DIR}/tools" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/tools")
  option (HDF5_BUILD_TOOLS  "Build HDF5 Tools" ON)
  if (HDF5_BUILD_TOOLS)
    add_subdirectory (${HDF5_SOURCE_DIR}/tools ${PROJECT_BINARY_DIR}/tools)
  endif (HDF5_BUILD_TOOLS)
endif (EXISTS "${HDF5_SOURCE_DIR}/tools" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/tools")

#-----------------------------------------------------------------------------
# Option to build examples
#-----------------------------------------------------------------------------
if (EXISTS "${HDF5_SOURCE_DIR}/examples" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/examples")
  option (HDF5_BUILD_EXAMPLES  "Build HDF5 Library Examples" ON)
  if (HDF5_BUILD_EXAMPLES)
    add_subdirectory (${HDF5_SOURCE_DIR}/examples ${PROJECT_BINARY_DIR}/examples)
  endif (HDF5_BUILD_EXAMPLES)
endif (EXISTS "${HDF5_SOURCE_DIR}/examples" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/examples")

#-----------------------------------------------------------------------------
# Option to build High Level API's
#-----------------------------------------------------------------------------
if (EXISTS "${HDF5_SOURCE_DIR}/hl" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/hl")
  option (HDF5_BUILD_HL_LIB  "Build HIGH Level HDF5 Library" ON)
  if (HDF5_BUILD_HL_LIB)
    set (H5_INCLUDE_HL 1)
    add_subdirectory (${HDF5_SOURCE_DIR}/hl ${PROJECT_BINARY_DIR}/hl)
  endif (HDF5_BUILD_HL_LIB)
endif (EXISTS "${HDF5_SOURCE_DIR}/hl" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/hl")

#-----------------------------------------------------------------------------
# Option to build Fortran bindings/tests/examples
# Make sure this appears before the CONFIGURE_FILE step
# so that fortran name mangling is detected before writing H5pubconf.h
#-----------------------------------------------------------------------------
# Set default name mangling : overridden by Fortran detection in fortran dir
set (H5_FC_FUNC  "H5_FC_FUNC(name,NAME) name ## _")
set (H5_FC_FUNC_ "H5_FC_FUNC_(name,NAME) name ## _")
if (EXISTS "${HDF5_SOURCE_DIR}/fortran" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/fortran")
  option (HDF5_BUILD_FORTRAN "Build FORTRAN support" OFF)
  option (SKIP_HDF5_FORTRAN_SHARED "Do not build the fortran shared libraries" OFF)
  if (HDF5_BUILD_FORTRAN)
    if (BUILD_SHARED_LIBS AND APPLE)
      if (NOT ALLOW_UNSUPPORTED)
        message (STATUS " **** Shared FORTRAN libraries are unsupported **** ")
        set (SKIP_HDF5_FORTRAN_SHARED ON)
      else (NOT ALLOW_UNSUPPORTED)
        message (STATUS " **** Allowing unsupported Fortran shared libraries **** ")
      endif (NOT ALLOW_UNSUPPORTED)
    endif (BUILD_SHARED_LIBS AND APPLE)
    option (HDF5_ENABLE_F2003 "Enable FORTRAN 2003 Standard" ON)
    include (${HDF_RESOURCES_EXT_DIR}/HDFUseFortran.cmake)
    if (HDF5_ENABLE_F2003)
      if (NOT FORTRAN_HAVE_ISO_C_BINDING)
        set (HDF5_ENABLE_F2003 OFF)
      endif (NOT FORTRAN_HAVE_ISO_C_BINDING)
    endif (HDF5_ENABLE_F2003)

    add_subdirectory (${HDF5_SOURCE_DIR}/fortran ${PROJECT_BINARY_DIR}/fortran)
    if (HDF5_BUILD_HL_LIB)
      if (EXISTS "${HDF5_SOURCE_DIR}/hl/fortran" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/hl/fortran")
        #-- Build the High Level Fortran source codes
        add_subdirectory (${HDF5_SOURCE_DIR}/hl/fortran ${PROJECT_BINARY_DIR}/hl/fortran)
      endif (EXISTS "${HDF5_SOURCE_DIR}/hl/fortran" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/hl/fortran")
    endif (HDF5_BUILD_HL_LIB)
  endif (HDF5_BUILD_FORTRAN)
endif (EXISTS "${HDF5_SOURCE_DIR}/fortran" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/fortran")

#-----------------------------------------------------------------------------
# Option to build HDF5 C++ Library
#-----------------------------------------------------------------------------
if (EXISTS "${HDF5_SOURCE_DIR}/c++" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/c++")
  option (HDF5_BUILD_CPP_LIB  "Build HDF5 C++ Library" ON)
  if (HDF5_BUILD_CPP_LIB)
    # check for unsupported options
    if (HDF5_ENABLE_PARALLEL)
      if (NOT ALLOW_UNSUPPORTED)
        message (FATAL_ERROR " **** Parallel and C++ options are mutually exclusive **** ")
      else (NOT ALLOW_UNSUPPORTED)
        message (STATUS " **** Allowing unsupported Parallel and C++ options **** ")
      endif (NOT ALLOW_UNSUPPORTED)
    endif (HDF5_ENABLE_PARALLEL)
    if (CMAKE_NO_STD_NAMESPACE)
      set (H5_NO_STD 1)
    endif (CMAKE_NO_STD_NAMESPACE)
    add_subdirectory (${HDF5_SOURCE_DIR}/c++ ${PROJECT_BINARY_DIR}/c++)
    if (HDF5_BUILD_HL_LIB)
      if (EXISTS "${HDF5_SOURCE_DIR}/hl/c++" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/hl/c++")
        #-- Build the High Level Fortran source codes
        add_subdirectory (${HDF5_SOURCE_DIR}/hl/c++ ${PROJECT_BINARY_DIR}/hl/c++)
      endif (EXISTS "${HDF5_SOURCE_DIR}/hl/c++" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/hl/c++")
    endif (HDF5_BUILD_HL_LIB)
  endif (HDF5_BUILD_CPP_LIB)
endif (EXISTS "${HDF5_SOURCE_DIR}/c++" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/c++")

#-----------------------------------------------------------------------------
# Check if Fortran's default real is double precision. If it is and HL is
# being built then configure should fail due to bug HDFFV-889.
#-----------------------------------------------------------------------------
 if (HDF5_BUILD_FORTRAN AND HDF5_BUILD_HL_LIB )
   if (NOT FORTRAN_DEFAULT_REAL_NOT_DOUBLE)
     message (FATAL_ERROR " **** Fortran high-level routines are not supported when the default REAL is DOUBLE PRECISION, use HDF5_BUILD_HL_LIB:BOOL=OFF **** ")
   endif (NOT FORTRAN_DEFAULT_REAL_NOT_DOUBLE)
 endif (HDF5_BUILD_FORTRAN AND HDF5_BUILD_HL_LIB )

#-----------------------------------------------------------------------------
# Generate the H5pubconf.h file containing user settings needed by compilation
#-----------------------------------------------------------------------------
configure_file (${HDF_RESOURCES_DIR}/H5pubconf.h.in ${HDF5_BINARY_DIR}/H5pubconf.h @ONLY)

include (CMakeInstallation.cmake)
