cmake_minimum_required(VERSION 3.10.2 FATAL_ERROR)

set(NIFTI_MAX_VALIDATED_CMAKE_VERSION "3.13.1")
if("${CMAKE_VERSION}" VERSION_LESS_EQUAL "${NIFTI_MAX_VALIDATED_CMAKE_VERSION}")
  # As of 2018-12-04 NIFTI has been validated to build with cmake version 3.13.1 new policies.
  # Set and use the newest cmake policies that are validated to work
  set(NIFTI_CMAKE_POLICY_VERSION "${CMAKE_VERSION}")
else()
  set(NIFTI_CMAKE_POLICY_VERSION "${NIFTI_MAX_VALIDATED_CMAKE_VERSION}")
endif()
cmake_policy(VERSION ${NIFTI_CMAKE_POLICY_VERSION})

set(NIFTI_HOMEPAGE_URL "https://nifti-imaging.github.io")
project(NIFTI
        VERSION 2.1.0
        DESCRIPTION "Niftilib is a set of i/o libraries for reading and writing files in the nifti-1 data format. nifti-1 is a binary file format for storing medical image data, e.g. magnetic resonance image (MRI) and functional MRI (fMRI) brain images."
        LANGUAGES C)

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

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_INCLUDE_CURRENT_DIR_IN_INTERFACE ON)


# add option to build shared libraries. The default is OFF to maintain the
# current build behavior
option( BUILD_SHARED_LIBS "Toggle building shared libraries." OFF)

#When including nifti as a subpackage, a prefix is often needed to avoid conflicts with sytem installed libraries.
set_if_not_defined(NIFTI_PACKAGE_PREFIX "")

# Set default shared library version
# This library version will be applied to all libraries in the package
# unless it is not explicitely for a certain lib.
set(NIFTI_SHAREDLIB_VERSION ${NIFTI_VERSION_MAJOR}.${NIFTI_VERSION_MINOR}.${NIFTI_VERSION_PATCH} )
string( REGEX MATCH "^[0-9]+" NIFTI_SHAREDLIB_SOVERSION ${NIFTI_SHAREDLIB_VERSION})
if(BUILD_SHARED_LIBS AND NOT NIFTI_LIBRARY_PROPERTIES)
  set(NIFTI_LIBRARY_PROPERTIES ${DEFAULT_SHARED_LIBS}
    BUILD_SHARED_LIBS TRUE POSITION_INDEPENDENT_CODE TRUE VERSION ${NIFTI_SHAREDLIB_VERSION} SOVERSION ${NIFTI_SHAREDLIB_SOVERSION}
  )
endif()

set_if_not_defined(NIFTI_INSTALL_RUNTIME_DIR "${CMAKE_INSTALL_PREFIX}/bin")
set_if_not_defined(NIFTI_INSTALL_LIBRARY_DIR "${CMAKE_INSTALL_PREFIX}/lib")
set_if_not_defined(NIFTI_INSTALL_INCLUDE_DIR "${CMAKE_INSTALL_PREFIX}/include/nifti")
set_if_not_defined(NIFTI_ZLIB_LIBRARIES "")
if(NOT NIFTI_ZLIB_LIBRARIES) # If using a custom zlib library, skip the find package
  ###  USE AS STAND ALONE PACKAGE
  find_package(ZLIB REQUIRED)
  set(NIFTI_ZLIB_LIBRARIES ${ZLIB_LIBRARIES})
endif()
#message(STATUS "---------------------ZLIB -${NIFTI_ZLIB_LIBRARIES}--")
add_definitions(-DHAVE_ZLIB)
set_if_not_defined(NIFTI_INSTALL_NO_DEVELOPMENT 0)
set_if_not_defined(NIFTI_INSTALL_NO_LIBRARIES 0)

#######################################################################
enable_testing()
include(CTest)
#Needs an if clause and more work before testing can take place.
set_if_not_defined(NIFTI_BUILD_TESTING ${BUILD_TESTING})
if (NIFTI_BUILD_TESTING AND CMAKE_VERSION GREATER_EQUAL 3.11.0) # CMAKE VERSION 3.11.0 needed for fetching data with cmake

  include(FetchContent) # fetch data a configure time to simplify tests
  # If new or changed data is needed, add that data to the https://github.com/NIFTI-Imaging/nifti-test-data repo
  # make a new release, and then update the URL and hash (shasum -a 256 <downloaded tarball>).
  FetchContent_Declare( fetch_testing_data
          URL      https://github.com/NIFTI-Imaging/nifti-test-data/archive/v3.0.0.tar.gz
          URL_HASH SHA256=8a22554ca196f707f7c8cdc102e1a30117cd67da41d269390952683076ffcac8
          )
  FetchContent_GetProperties(fetch_testing_data)
  if(NOT fetch_testing_data)
    set(FETCHCONTENT_QUIET OFF)
    message(STATUS "Downloading testing data... please wait")
    FetchContent_Populate( fetch_testing_data )
    message(STATUS "download complete.")
  endif()
endif()

#######################################################################
add_subdirectory(znzlib)
add_subdirectory(niftilib)

option(USE_NIFTICDF_CODE "Build nifticdf library and tools" ON)
mark_as_advanced(USE_NIFTICDF_CODE)
if(USE_NIFTICDF_CODE)
    add_subdirectory(nifticdf)
endif()

option(NIFTI_BUILD_APPLICATIONS "Build various utility tools" ON)
mark_as_advanced(NIFTI_BUILD_APPLICATIONS)

option(USE_NIFTI2_CODE "Build the nifti2 library and tools" ON)
mark_as_advanced(USE_NIFTI2_CODE)
include(CMakeDependentOption)
cmake_dependent_option(USE_CIFTI_CODE "Build the cifti library and tools" OFF "USE_NIFTI2_CODE" OFF)
mark_as_advanced(USE_CIFTI_CODE)

if( USE_NIFTI2_CODE )
  add_subdirectory(nifti2)
  if( USE_CIFTI_CODE )
    add_subdirectory(cifti)
  endif()
endif()

option(USE_FSL_CODE "If OFF, The copyright of this code is questionable for inclusion with nifti." OFF)
mark_as_advanced(USE_FSL_CODE)
# the order of add_subdirectory is important! fsliolob has to preceed examples
# as otherwise FSLIOLIB_SOURCE_DIR is undefined and hence the examples
# will fail to compile
if(USE_FSL_CODE)
  add_subdirectory(fsliolib)
endif()

set_if_not_defined(NIFTI_INSTALL_EXPORT_NAME "NIFTITargets")
if(NIFTI_INSTALL_EXPORT_NAME EQUAL "NIFTITargets")
  #######################################################################
  # CMake itself and can use some CMake facilities for creating the package files.
  # This allows for find_package(NIFTI 2.1.0 NO_MODULE) to work for pulling in
  # NIFTI libraries into an external project
  include(CMakePackageConfigHelpers)

  write_basic_package_version_file(
        "${CMAKE_CURRENT_BINARY_DIR}/NIFTI/NIFTIConfigVersion.cmake"
        VERSION ${NIFTI_VERSION}
        COMPATIBILITY AnyNewerVersion
  )

  export(EXPORT NIFTITargets
        FILE "${CMAKE_CURRENT_BINARY_DIR}/NIFTI/NIFTITargets.cmake"
        NAMESPACE ${NIFTI_PACKAGE_PREFIX}NIFTI::
        )
  configure_file(cmake/NIFTIConfig.cmake
        "${CMAKE_CURRENT_BINARY_DIR}/NIFTI/NIFTIConfig.cmake"
        COPYONLY
        )

  set(ConfigPackageLocation lib/cmake/NIFTI)
  install(EXPORT NIFTITargets
        FILE          NIFTITargets.cmake
        NAMESPACE     ${NIFTI_PACKAGE_PREFIX}NIFTI::
        DESTINATION   ${ConfigPackageLocation}
        )
  install(FILES
           cmake/NIFTIConfig.cmake
           "${CMAKE_CURRENT_BINARY_DIR}/NIFTI/NIFTIConfigVersion.cmake"
        DESTINATION   ${ConfigPackageLocation}
        COMPONENT     Development
  )
endif()

#######################################################################
## Add the cpack configuration settings last
option(NIFTI_USE_PACKAGING "Configure the packaging options for NIFTI" OFF)
mark_as_advanced(NIFTI_USE_PACKAGING)
if(NIFTI_USE_PACKAGING)
  include(cmake/NIFTICPackConfig.cmake)
endif()
