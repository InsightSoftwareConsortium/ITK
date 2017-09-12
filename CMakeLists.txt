cmake_minimum_required(VERSION 2.8.9 FATAL_ERROR)

foreach(p
    CMP0025 # CMake 3.0
    CMP0042 # CMake 3.0
    CMP0054 # CMake 3.1
    CMP0056 # CMake 3.2
    CMP0058 # CMake 3.3
    CMP0063 # CMake 3.3.2
    )
  if(POLICY ${p})
    cmake_policy(SET ${p} NEW)
  endif()
endforeach()

project(ITK NONE)
include(${CMAKE_CURRENT_SOURCE_DIR}/CMake/ITKInitializeBuildType.cmake)
enable_language(C)
enable_language(CXX)

include(CMakeDependentOption)
#
# use ExternalProject
include(ExternalProject)

if( CMAKE_HOST_WIN32 AND NOT ITK_SKIP_PATH_LENGTH_CHECKS )

  string( LENGTH "${CMAKE_CURRENT_SOURCE_DIR}" n )
  if( n GREATER 50 )
    message(
      FATAL_ERROR
      "ITK source code directory path length is too long (${n} > 50)."
      "Please move the ITK source code directory to a directory with a shorter path."
      )
  endif()

  string( LENGTH "${CMAKE_CURRENT_BINARY_DIR}" n )
  if( n GREATER 50 )
    message(
      FATAL_ERROR
      "ITK build directory path length is too long (${n} > 50)."
      "Please set the ITK build directory to a directory with a shorter path."
      )
  endif()

endif()

#-----------------------------------------------------------------------------
if(NOT EXISTS "${ITK_SOURCE_DIR}/.ExternalData/README.rst")
  # This file is always present in version-controlled source trees
  # so we must have been extracted from a source tarball with no
  # data objects needed for testing.  Turn off tests by default
  # since enabling them requires network access or manual data
  # store configuration.
  option(BUILD_TESTING "Build the testing tree." OFF)
endif()
include(CTest)
mark_as_advanced(CLEAR BUILD_TESTING)

set(ITK_CMAKE_DIR ${ITK_SOURCE_DIR}/CMake)
set(CMAKE_MODULE_PATH ${ITK_CMAKE_DIR} ${CMAKE_MODULE_PATH})
include(ITKDownloadSetup)
include(PreventInSourceBuilds)
include(PreventInBuildInstalls)
include(ITKModuleMacros)
include(ITKExternalData)
include(ITKModuleTest)
include(itkCheckSourceTree)


set(main_project_name ${_ITKModuleMacros_DEFAULT_LABEL})

#-----------------------------------------------------------------------------
# ITK version number.
set(ITK_VERSION_MAJOR "4")
set(ITK_VERSION_MINOR "12")
set(ITK_VERSION_PATCH "2")

if(NOT ITK_INSTALL_RUNTIME_DIR)
  set(ITK_INSTALL_RUNTIME_DIR bin)
endif()
if(NOT ITK_INSTALL_LIBRARY_DIR)
  set(ITK_INSTALL_LIBRARY_DIR lib)
endif()
if(NOT ITK_INSTALL_ARCHIVE_DIR)
  set(ITK_INSTALL_ARCHIVE_DIR lib)
endif()
if(NOT ITK_INSTALL_INCLUDE_DIR)
  set(ITK_INSTALL_INCLUDE_DIR include/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR})
endif()
if(NOT ITK_INSTALL_DATA_DIR)
  set(ITK_INSTALL_DATA_DIR share/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR})
endif()
if(NOT ITK_INSTALL_DOC_DIR)
  set(ITK_INSTALL_DOC_DIR share/doc/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR})
endif()
if(NOT ITK_INSTALL_PACKAGE_DIR)
  set(ITK_INSTALL_PACKAGE_DIR "lib/cmake/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")
endif()

# Override CMake's built-in add_* commands: assign LABELS to tests and targets
# automatically. Depends on the CMake variable itk-module being set to the
# "current" module when add_* is called.
macro(verify_itk_module_is_set)
  if("" STREQUAL "${itk-module}")
    message(FATAL_ERROR "CMake variable itk-module is not set")
  endif()
endmacro()

#-----------------------------------------------------------------------------
# Do we try to use system libraries by default?
option(ITK_USE_SYSTEM_LIBRARIES "Use the system's libraries by default.
If this is not set during the initial configuration, it will have no effect." OFF)
mark_as_advanced(ITK_USE_SYSTEM_LIBRARIES)

#-----------------------------------------------------------------------------
# Enable the download and use of BrainWeb datasets.
# When this data is available, additional 3D tests are enabled.
option(ITK_USE_BRAINWEB_DATA "Download and use BrainWeb data for advanced testing" OFF)
mark_as_advanced(ITK_USE_BRAINWEB_DATA)
if(ITK_BRAINWEB_DATA_ROOT)
  message(WARNING
    "ITK_BRAINWEB_DATA_ROOT is not longer supported!"
    "Please update to ITK_USE_BRAINWEB_DATA."
    )
endif()

#-----------------------------------------------------------------------------
# CMAKE_C_COMPILER_ARG1 is a CMake internal variable. It should not be used
# in ITK's configuration. If it is set, it most likely means that
# a developer is configuring ITK to be built with a launcher such as ccache
# or distcc. This should be done with the CMake variable
# CMAKE_C_COMPILER_LAUNCHER_FLAG available in CMake > 3.4.0 .
if(CMAKE_C_COMPILER_ARG1)
  message(WARNING "The CMake variable CMAKE_C_COMPILER_ARG1 is set. This most\
  likely mean that the developer is trying to use ccache or distcc. The CMake\
  variable CMAKE_C_COMPILER_LAUNCHER_FLAG should be used instead\
  (CMake>=3.4 required)")
endif()

#-----------------------------------------------------------------------------
# ITK build configuration options.
option(BUILD_SHARED_LIBS "Build ITK with shared libraries." OFF)
set(ITK_BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS})

option(ITK_DYNAMIC_LOADING "Support run-time loading of shared libraries" ON)
mark_as_advanced(ITK_DYNAMIC_LOADING)

#-----------------------------------------------------------------------------
# Wrapping options
option(ITK_WRAP_PYTHON "Build Python support" OFF)
option(ITK_WRAP_JAVA "Build Java support (Currently not supported)" OFF)
option(ITK_WRAP_RUBY "Build Ruby support (Currently not supported)" OFF)
option(ITK_WRAP_PERL "Build Perl support (Currently not supported)" OFF)
option(ITK_WRAP_TCL "Build Tcl support (Currently not supported)" OFF)
mark_as_advanced(ITK_WRAP_JAVA)
mark_as_advanced(ITK_WRAP_RUBY)
mark_as_advanced(ITK_WRAP_PERL)
mark_as_advanced(ITK_WRAP_TCL)

if(ITK_WRAP_PYTHON OR ITK_WRAP_JAVA OR ITK_WRAP_RUBY OR ITK_WRAP_PERL OR ITK_WRAP_TCL)
  if(NOT ITK_DYNAMIC_LOADING)
    message(FATAL_ERROR "Wrapping requires ITK_DYNAMIC_LOADING to be ON")
  endif()
  # ITK_WRAPPING is an internal variable
  set(ITK_WRAPPING ON CACHE INTERNAL "Build external languages support" FORCE)
else()
  set(ITK_WRAPPING OFF CACHE INTERNAL "Build external languages support" FORCE)
endif()
# Enable BridgeNumPy by default when Python wrapping is enabled
if(ITK_WRAP_PYTHON AND NOT DEFINED Module_BridgeNumPy)
  set(Module_BridgeNumPy ON CACHE BOOL "Enable the BridgeNumPy module.")
endif()

include(ITKSetStandardCompilerFlags)
#---------------------------------------------------------------
# run try compiles and tests for ITK
include(itkTestFriendTemplatedFunction)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ITK_REQUIRED_C_FLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ITK_REQUIRED_CXX_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
if(NOT CMAKE_POSITION_INDEPENDENT_CODE)
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif()

# Setup build locations.
if(NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ITK_BINARY_DIR}/bin)
endif()
if(NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${ITK_BINARY_DIR}/lib)
endif()
if(NOT CMAKE_ARCHIVE_OUTPUT_DIRECTORY)
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${ITK_BINARY_DIR}/lib)
endif()
set(ITK_MODULES_DIR "${ITK_BINARY_DIR}/${ITK_INSTALL_PACKAGE_DIR}/Modules")


#-----------------------------------------------------------------------------
# Provide compatibility options.
option(ITKV3_COMPATIBILITY "Enable compatibility with ITK3.x when possible." OFF)
option(ITK_LEGACY_REMOVE "Remove all legacy code completely." OFF)
if(ITK_WRAPPING)
  option(ITK_LEGACY_SILENT "Silence all legacy code messages." ON)
else()
  option(ITK_LEGACY_SILENT "Silence all legacy code messages." OFF)
endif()
# The ITK_FUTURE_LEGACY_REMOVE is a very advanced feature only
# available to developers at configuration time by manually
# adding this to the CMakeCache.txt -DITK_FUTURE_LEGACY_REMOVE:BOOL=ON
set( ITK_FUTURE_LEGACY_REMOVE 0 CACHE INTERNAL "Remove code that will become legacy in future releases completely." )
mark_as_advanced(ITK_LEGACY_SILENT ITK_LEGACY_REMOVE ITK_V3_COMPATIBILITY)

# The disabling of legacy code, and the ITKv3 compatibility option can not both be
# requested. If removal of legacy code is requested, then ITKV3_COMPATIBILITY must
# be off.
if(ITKV3_COMPATIBILITY AND ITK_LEGACY_REMOVE)
  message(FATAL_ERROR "Invlaid configuration: ITKV3_COMPATIBILITY AND ITK_LEGACY_REMOVE can not both be ON")
endif()

#-----------------------------------------------------------------------------
# ITK build classes that are in the review process
# ITK_USE_REVIEW is deprecated, only kept for backward compatibility
if (ITK_USE_REVIEW AND NOT Module_ITKReview)
  message(WARNING "ITK_USE_REVIEW is deprecated, please use Module_ITKReview to turn Review module ON/OFF")
  set(Module_ITKReview ON CACHE BOOL "Module containing code from the Review directory of ITKv3." FORCE)
endif()

#-----------------------------------------------------------------------------
# ITK uses KWStyle for checking the coding style
include(${ITK_SOURCE_DIR}/Utilities/KWStyle/KWStyle.cmake)

#-----------------------------------------------------------------------------
# Build the Examples that are illustrated in the Software Guide.
option(BUILD_EXAMPLES "Build the examples from the ITK Software Guide." OFF)

#-----------------------------------------------------------------------------
# Enable GPU support. Requires OpenCL to be installed
option(ITK_USE_GPU "GPU acceleration via OpenCL" OFF)
mark_as_advanced(ITK_USE_GPU)

if( ITK_USE_GPU )
   include(itkOpenCL)
endif()

#-----------------------------------------------------------------------------
# Manage FFT v3 Options
#
# ITK_USE_FFTWD -- use double precision fftw
if(DEFINED USE_FFTWD)
  set(ITK_USE_FFTWD_DEFAULT ${USE_FFTWD})
else()
  set(ITK_USE_FFTWD_DEFAULT OFF)
endif()
option(ITK_USE_FFTWD "Use double precision fftw if found" ${ITK_USE_FFTWD_DEFAULT})
mark_as_advanced(ITK_USE_FFTWD)
#
# ITK_USE_FFTWF -- use single precision fftw
if(DEFINED USE_FFTWF)
  set(ITK_USE_FFTWF_DEFAULT ${USE_FFTWF})
else()
  set(ITK_USE_FFTWF_DEFAULT OFF)
endif()
option(ITK_USE_FFTWF "Use single precision fftw if found" ${ITK_USE_FFTWF_DEFAULT})
mark_as_advanced(ITK_USE_FFTWF)

# ITK_USE_SYSTEM_FFTW -- locate a readybuilt fftw installation
if(DEFINED USE_SYSTEM_FFTW)
  set(ITK_USE_SYSTEM_FFTW_DEFAULT ${USE_SYSTEM_FFTW})
else()
  set(ITK_USE_SYSTEM_FFTW_DEFAULT ${ITK_USE_SYSTEM_LIBRARIES})
endif()
option(ITK_USE_SYSTEM_FFTW "Use an installed version of fftw" ${ITK_USE_SYSTEM_FFTW_DEFAULT})
mark_as_advanced(ITK_USE_SYSTEM_FFTW)


if( ITK_USE_FFTWD OR ITK_USE_FFTWF )
  include(itkExternal_FFTW)
endif()

if(NOT ITKV3_COMPATIBILITY)
  set(ITK_USE_64BITS_IDS_DEFAULT "OFF")
  # Note WIN32 is true when targeting any windows system
  if(CMAKE_SIZEOF_VOID_P EQUAL "8" AND WIN32)
    set(ITK_USE_64BITS_IDS_DEFAULT "ON")
  endif()
  option(ITK_USE_64BITS_IDS "When ON, ITK will use 64-bit integer types instead of long types for sizes and indexes. This is needed for managing images larger than 4Gb in some platforms." "${ITK_USE_64BITS_IDS_DEFAULT}")
  mark_as_advanced(ITK_USE_64BITS_IDS)
endif()

# ITK turn on concept checking
option(ITK_USE_CONCEPT_CHECKING "Turn on concept checking to give helpful errors at compile time if a type cannot be used as a template parameter." ON)
mark_as_advanced(ITK_USE_CONCEPT_CHECKING)
option(ITK_USE_STRICT_CONCEPT_CHECKING "Turn on Strict concept checking to give more stringent errors at compile time if a type cannot be used as a template parameter." OFF)
mark_as_advanced(ITK_USE_STRICT_CONCEPT_CHECKING)

#-----------------------------------------------------------------------------
# Mandatory wrapping options.
# These are not correctly set when using cmake without the -DITK_WRAP_PYTHON:BOOL=ON flag.
# In this case we need to tell the user that he has to change the flags manually to the right values.
if(ITK_WRAPPING)
  set(CMAKE_POSITION_INDEPENDENT_CODE 1)

  if(NOT ITK_LEGACY_SILENT)
    message(FATAL_ERROR "Wrapping requires to disable legacy code, please set ITK_LEGACY_SILENT=ON")
  endif()
endif()

#----------------------------------------------------------------------------
set(ITK_TEST_OUTPUT_DIR "${ITK_BINARY_DIR}/Testing/Temporary")

# Configure the default ITK_DATA_ROOT for the location of ITK Data.
set(ITK_DATA_ROOT ${ITK_SOURCE_DIR}/Testing/Data)

# Location of ITK Example Data.
set(ITK_EXAMPLE_DATA_ROOT "${ITK_SOURCE_DIR}/Examples/Data")

#This flag is used in particular, to enable some tests that require large memory to run.
# This probably can be discovered at configuration time by CMake. (Check with CMake developers).
set(ITK_COMPUTER_MEMORY_SIZE 1 CACHE STRING "Provide here the size of your RAM Memory in GigaBytes")
mark_as_advanced(ITK_COMPUTER_MEMORY_SIZE)

#This flag sets the floating point type used in itk::ImageBase for
# spacing/direction/origin to single precision
option(ITK_USE_FLOAT_SPACE_PRECISION "Use single precision for origin/spacing/directions in itk::Image" OFF)
mark_as_advanced(ITK_USE_FLOAT_SPACE_PRECISION)

#----------------------------------------------------------------------
# Make sure remote modules are downloaded before sorting out the module
# dependencies.
add_subdirectory(Modules/Remote)

# Enable modules according to user inputs and the module dependency DAG.
include(ITKModuleEnablement)

#----------------------------------------------------------------------
# Generate ITKConfig.cmake for the build tree.
set(ITK_CONFIG_CODE "
set(ITK_MODULES_DIR \"${ITK_MODULES_DIR}\")")
set(ITK_CONFIG_CMAKE_DIR "${ITK_SOURCE_DIR}/CMake")
set(ITK_USE_FILE "${ITK_CONFIG_CMAKE_DIR}/UseITK.cmake")
set(ITK_CONFIG_TARGETS_CONDITION " AND NOT ITK_BINARY_DIR")
set(ITK_CONFIG_TARGETS_FILE "${ITK_BINARY_DIR}/ITKTargets.cmake")
set(ITK_CONFIG_MODULE_API_FILE "${ITK_SOURCE_DIR}/CMake/ITKModuleAPI.cmake")
configure_file(CMake/ITKConfig.cmake.in ITKConfig.cmake @ONLY)

# Generate ITKConfig.cmake for the install tree.
set(ITK_CONFIG_CODE "
# Compute the installation prefix from this ITKConfig.cmake file location.
get_filename_component(ITK_INSTALL_PREFIX \"\${CMAKE_CURRENT_LIST_FILE}\" PATH)")
# Construct the proper number of get_filename_component(... PATH)
# calls to compute the installation prefix.
string(REGEX REPLACE "/" ";" _count "${ITK_INSTALL_PACKAGE_DIR}")
foreach(p ${_count})
  set(ITK_CONFIG_CODE "${ITK_CONFIG_CODE}
get_filename_component(ITK_INSTALL_PREFIX \"\${ITK_INSTALL_PREFIX}\" PATH)")
endforeach()
set(ITK_CONFIG_CODE "${ITK_CONFIG_CODE}
set(ITK_MODULES_DIR \"\${ITK_INSTALL_PREFIX}/${ITK_INSTALL_PACKAGE_DIR}/Modules\")")
set(ITK_USE_FILE "\${ITK_INSTALL_PREFIX}/${ITK_INSTALL_PACKAGE_DIR}/UseITK.cmake")
set(ITK_CONFIG_CMAKE_DIR "\${ITK_INSTALL_PREFIX}/${ITK_INSTALL_PACKAGE_DIR}")
set(ITK_CONFIG_TARGETS_CONDITION "")
set(ITK_CONFIG_TARGETS_FILE "\${ITK_INSTALL_PREFIX}/${ITK_INSTALL_PACKAGE_DIR}/ITKTargets.cmake")
set(ITK_CONFIG_MODULE_API_FILE "\${ITK_INSTALL_PREFIX}/${ITK_INSTALL_PACKAGE_DIR}/ITKModuleAPI.cmake")
if(NOT ITK_USE_SYSTEM_FFTW)
  # Location installed with the FFTW ExternalProject.
  set(FFTW_LIBDIR "\${ITK_INSTALL_PREFIX}/lib/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")
  set(FFTW_INCLUDE_PATH "\${ITK_INSTALL_PREFIX}/include/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}/Algorithms")
endif()
configure_file(CMake/ITKConfig.cmake.in CMakeFiles/ITKConfig.cmake @ONLY)

#----------------------------------------------------------------------------
# Configure maintenance scripts
configure_file(Utilities/Maintenance/doSingleKWStyleUncrustifyFix.sh.in
  Utilities/Maintenance/doSingleKWStyleUncrustifyFix.sh @ONLY)

#-----------------------------------------------------------------------------
configure_file(CMake/ITKConfigVersion.cmake.in ITKConfigVersion.cmake @ONLY)

install(FILES ${ITK_BINARY_DIR}/CMakeFiles/ITKConfig.cmake
              ${ITK_BINARY_DIR}/ITKConfigVersion.cmake
              CMake/ITKModuleAPI.cmake
              CMake/UseITK.cmake
              CMake/itkImageIOFactoryRegisterManager.h.in
              CMake/itkTransformIOFactoryRegisterManager.h.in
  DESTINATION ${ITK_INSTALL_PACKAGE_DIR}
  COMPONENT Development)
get_property(ITKTargets_MODULES GLOBAL PROPERTY ITKTargets_MODULES)
if(ITKTargets_MODULES)
  install(EXPORT ITKTargets DESTINATION ${ITK_INSTALL_PACKAGE_DIR}
          COMPONENT Development)
else()
  set(CMAKE_CONFIGURABLE_FILE_CONTENT "# No targets!")
  configure_file(${CMAKE_ROOT}/Modules/CMakeConfigurableFile.in
                 ${ITK_BINARY_DIR}/CMakeFiles/ITKTargets.cmake @ONLY)
  install(FILES ${ITK_BINARY_DIR}/CMakeFiles/ITKTargets.cmake
          DESTINATION ${ITK_INSTALL_PACKAGE_DIR} COMPONENT Development)
endif()

install(FILES "LICENSE" "NOTICE" "README.txt" DESTINATION ${ITK_INSTALL_DOC_DIR} COMPONENT Runtime)

if(BUILD_TESTING)
  add_subdirectory(Utilities/InstallTest)
endif()



#-----------------------------------------------------------------------------
# The subdirectories added below this line should use only the public
# interface with find_package(ITK). Set ITK_DIR to use this ITK build.
set(ITK_DIR "${ITK_BINARY_DIR}")

if(ITK_WRAPPING)
  add_subdirectory(Wrapping)
endif()

if(BUILD_EXAMPLES)
  add_subdirectory(Examples)
endif()

#----------------------------------------------------------------------
# Provide an option for generating documentation.
add_subdirectory(Utilities/Doxygen)

# Create target to download data from the ITKData group. This must come after
# all tests have been added that reference the group, so we put it last.
include(${ITK_CMAKE_DIR}/ExternalData.cmake)
ExternalData_Add_Target(ITKData)
