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

if(0) # ITK -- start: do not use Git to get the version
set(NIFTI_HOMEPAGE_URL "https://nifti-imaging.github.io")
execute_process(COMMAND git "describe" "--tags"
  OUTPUT_VARIABLE GIT_REPO_VERSION_UNCLEANED
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

#Extract the GIT_REPO_VERSION as composed of non-negative integer components,
# i.e. <major>[.<minor>[.<patch>[.<tweak>]]]
string(REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+((beta.*)?(alpha.*)?\\.[0-9]+(\\.g[0-9a-f]+)?)?"
 GIT_REPO_VERSION  "${GIT_REPO_VERSION_UNCLEANED}")

if( NOT GIT_REPO_VERSION )
  message(NOTE "Invalid git tag does not match required regular expression, "
               "can not extract version information from '${GIT_REPO_VERSION_UNCLEANED}'")
  # NOTE: cmake -DGIT_REPO_VERSION:STRING=<major>[.<minor>[.<patch>[.<tweak>]]] can be used
  #       to set the repo string for non-git repos.
  set(GIT_REPO_VERSION "0.0.0.0") #Manually set the version string for testing purposes
endif()
else() # ITK
set(GIT_REPO_VERSION "0.0.0.0")
endif() # ITK -- end: do not use Git to get the version
project(NIFTI
        VERSION ${GIT_REPO_VERSION}
        DESCRIPTION "Niftilib is a set of i/o libraries for reading and writing files in the nifti-1 data format. nifti-1 is a binary file format for storing medical image data, e.g. magnetic resonance image (MRI) and functional MRI (fMRI) brain images."
        LANGUAGES C)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake)
include(nifti_macros)

set_property(GLOBAL PROPERTY nifti_installed_targets)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_INCLUDE_CURRENT_DIR_IN_INTERFACE ON)
include(CMakeDependentOption)

if("${CMAKE_VERSION}" VERSION_GREATER_EQUAL "3.13")
set(CMAKE_VER_AT_LEAST_3_13 1 CACHE BOOL "Is set for cmake >=3.13")
mark_as_advanced(CMAKE_VER_AT_LEAST_3_13)
endif()

# add option to build shared libraries. The default is OFF to maintain the
# current build behavior
option( BUILD_SHARED_LIBS "Toggle building shared libraries." OFF)

# Include executables as part of the build
option(NIFTI_BUILD_APPLICATIONS "Build various utility tools" ON)
mark_as_advanced(NIFTI_BUILD_APPLICATIONS)

#When including nifti as a subpackage, a prefix is often needed to avoid conflicts with system installed libraries.
set_if_not_defined(NIFTI_PACKAGE_PREFIX "")
set(PACKAGE_NAME ${NIFTI_PACKAGE_PREFIX}NIFTI)
set_if_not_defined(NIFTI_INSTALL_EXPORT_NAME ${PACKAGE_NAME}Targets)

# Set default shared library version
# This library version will be applied to all libraries in the package
# unless it is not explicitly for a certain lib.
set(NIFTI_SHAREDLIB_VERSION ${GIT_REPO_VERSION} )
if(BUILD_SHARED_LIBS AND NOT NIFTI_LIBRARY_PROPERTIES)
  set(NIFTI_LIBRARY_PROPERTIES ${DEFAULT_SHARED_LIBS}
    BUILD_SHARED_LIBS TRUE POSITION_INDEPENDENT_CODE TRUE
  )
endif()

set_if_not_defined(NIFTI_INSTALL_RUNTIME_DIR bin)
set_if_not_defined(NIFTI_INSTALL_LIBRARY_DIR lib)
set_if_not_defined(NIFTI_INSTALL_ARCHIVE_DIR ${NIFTI_INSTALL_LIBRARY_DIR})
set_if_not_defined(NIFTI_INSTALL_INCLUDE_DIR include/nifti)
set_if_not_defined(NIFTI_INSTALL_MAN_DIR share/man/man1)
set_if_not_defined(NIFTI_INSTALL_DOC_DIR share/doc/${PROJECT_NAME})
set_if_not_defined(NIFTI_ZLIB_LIBRARIES "")
set_if_not_defined(ZNZ_COMPILE_DEF "")
if(NOT NIFTI_ZLIB_LIBRARIES) # If using a custom zlib library, skip the find package
  ###  USE AS STAND ALONE PACKAGE
  find_package(ZLIB REQUIRED)
  set(NIFTI_ZLIB_LIBRARIES ${ZLIB_LIBRARIES})
endif()
#message(STATUS "---------------------ZLIB -${NIFTI_ZLIB_LIBRARIES}--")
add_definitions(-DHAVE_ZLIB)

set_if_not_defined(NIFTI_INSTALL_NO_DOCS TRUE)

# Include test to verify linking in installed executables
# The test should only be added when applications are built, and no prefix string is set.
cmake_dependent_option(
  TEST_INSTALL "Add a test to check that linking to SO libraries occurs correctly during installation." ON
  "${NIFTI_PACKAGE_PREFIX};${NIFTI_BUILD_APPLICATIONS};${BUILD_SHARED_LIBS};${NIFTI_BUILD_TESTING}" OFF
)

# Add a default attempt at setting the run path correctly for binaries
if(NOT CMAKE_SKIP_INSTALL_RPATH)
  file(
    RELATIVE_PATH
    relDir
    ${CMAKE_INSTALL_PREFIX}/${NIFTI_INSTALL_RUNTIME_DIR}
    ${CMAKE_INSTALL_PREFIX}/${NIFTI_INSTALL_LIBRARY_DIR}
  )
  if(APPLE)
    set_if_not_defined(CMAKE_INSTALL_RPATH  "@loader_path/${relDir}")
  else()
    set_if_not_defined(CMAKE_INSTALL_RPATH   "\$ORIGIN/${relDir}")
  endif()
endif()

if(NOT NIFTI_INSTALL_NO_DOCS)
      find_program(HELP2MAN help2man REQUIRED)
      set(MAN_DIR ${PROJECT_BINARY_DIR}/manpages)
      file(MAKE_DIRECTORY ${MAN_DIR})
endif()
#######################################################################

set_if_not_defined(BUILD_TESTING ON)
set_if_not_defined(NIFTI_BUILD_TESTING ${BUILD_TESTING})
if (NIFTI_BUILD_TESTING )
  include(CTest)
  enable_testing()
  #Needs an if clause and more work before testing can take place.
  option(
    DOWNLOAD_TEST_DATA
    "Allow download of data used for some tests. Can be used in conjunction with ctest label filter '-LE NEEDS_DATA'"
    ON
    )
  set_if_not_defined(NIFTI_SHELL_SCRIPT_TESTS "ON")
  if(DOWNLOAD_TEST_DATA)
  if ( CMAKE_VERSION VERSION_LESS 3.11.0 )
    # CMAKE VERSION 3.11.0 needed for fetching data with cmake
    message(FATAL_ERROR "ERROR:  The testing framework for nifti_clib requires CMake version 3.11.0 or greater")
  endif()

  include(FetchContent) # fetch data a configure time to simplify tests
  # If new or changed data is needed, add that data to the https://github.com/NIFTI-Imaging/nifti-test-data repo
  # make a new release, and then update the URL and hash (shasum -a 256 <downloaded tarball>).
  FetchContent_Declare( fetch_testing_data
          URL      https://github.com/NIFTI-Imaging/nifti-test-data/archive/v3.0.2.tar.gz
          URL_HASH SHA256=5dafec078151018da7aaf3c941bd31f246f590bc34fa3fef29ce77a773db16a6
          )
  FetchContent_GetProperties(fetch_testing_data)
  if(NOT fetch_testing_data_POPULATED)
    set(FETCHCONTENT_QUIET OFF)
    FetchContent_Populate( fetch_testing_data )
  endif()
  endif()
endif()

#######################################################################
# Find unix math libraries
if(EMSCRIPTEN OR WASI)
    set(NIFTI_SYSTEM_MATH_LIB m)
elseif(NOT WIN32)
    find_library(NIFTI_SYSTEM_MATH_LIB m)
else()
    set(NIFTI_SYSTEM_MATH_LIB "")
endif()
mark_as_advanced(NIFTI_SYSTEM_MATH_LIB)
#######################################################################
add_subdirectory(znzlib)
add_subdirectory(niftilib)

option(USE_NIFTICDF_CODE "Build nifticdf library and tools" ON)
mark_as_advanced(USE_NIFTICDF_CODE)
if(USE_NIFTICDF_CODE)
    add_subdirectory(nifticdf)
endif()

option(USE_NIFTI2_CODE "Build the nifti2 library and tools" ON)
mark_as_advanced(USE_NIFTI2_CODE)
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
# the order of add_subdirectory is important! fsliolob has to precede, examples
# as otherwise FSLIOLIB_SOURCE_DIR is undefined and hence the examples
# will fail to compile
if(USE_FSL_CODE)
  add_subdirectory(fsliolib)
endif()

# Allow an uninstall (with some risk of messiness)

if(NOT TARGET uninstall)
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/cmake_uninstall.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake" IMMEDIATE @ONLY
  )

  add_custom_target(
    uninstall COMMAND ${CMAKE_COMMAND} -P
                      ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake
  )
endif()

# Report installed targets
get_property(nifti_installed_targets GLOBAL PROPERTY nifti_installed_targets)
message(STATUS "nifti_installed_targets: ${nifti_installed_targets}")

if(CMAKE_VER_AT_LEAST_3_13)
  # Target installation for CMake versions >=3.13
  foreach(targ ${nifti_installed_targets})
    install_nifti_target(${targ})
  endforeach()
endif()

if(NOT NIFTI_INSTALL_NO_DOCS)
  install(
      DIRECTORY ${MAN_DIR}/
      DESTINATION ${NIFTI_INSTALL_MAN_DIR}
      FILES_MATCHING REGEX ".*.1.gz"
      PERMISSIONS OWNER_READ WORLD_READ
      )
  install(
    FILES ${PROJECT_SOURCE_DIR}/README.md
    DESTINATION ${NIFTI_INSTALL_DOC_DIR}
    )
  install(
    DIRECTORY ${PROJECT_SOURCE_DIR}/real_easy/
    DESTINATION ${NIFTI_INSTALL_DOC_DIR}/examples
    )
endif()

if(NIFTI_INSTALL_EXPORT_NAME STREQUAL "NIFTITargets")
  #######################################################################
  # CMake itself and can use some CMake facilities for creating the package files.
  # This allows for find_package(NIFTI 2.1.0 NO_MODULE) to work for pulling in
  # NIFTI libraries into an external project
  include(CMakePackageConfigHelpers)
  set(CONFIG_SETUP_DIR ${CMAKE_CURRENT_BINARY_DIR}/${PACKAGE_NAME})
  set(ConfigPackageLocation share/cmake/${PACKAGE_NAME})

  write_basic_package_version_file(
        ${CONFIG_SETUP_DIR}/${PACKAGE_NAME}ConfigVersion.cmake
        VERSION ${GIT_REPO_VERSION}
        COMPATIBILITY AnyNewerVersion
  )

  configure_file(cmake/NIFTIConfig.cmake
        ${CONFIG_SETUP_DIR}/${PACKAGE_NAME}Config.cmake
        COPYONLY
        )


  install(EXPORT ${NIFTI_INSTALL_EXPORT_NAME}
        FILE NIFTITargets.cmake
        NAMESPACE     ${PACKAGE_NAME}::
        DESTINATION   ${ConfigPackageLocation}
        COMPONENT     Development
        )

  install(FILES
           ${CONFIG_SETUP_DIR}/${PACKAGE_NAME}Config.cmake
           ${CONFIG_SETUP_DIR}/${PACKAGE_NAME}ConfigVersion.cmake
        COMPONENT     Development
        DESTINATION   ${ConfigPackageLocation}
  )
endif()

#######################################################################
## Add the cpack configuration settings last
option(NIFTI_USE_PACKAGING "Configure the packaging options for NIFTI" OFF)
mark_as_advanced(NIFTI_USE_PACKAGING)
if(NIFTI_USE_PACKAGING)
  include(cmake/NIFTICPackConfig.cmake)
endif()
