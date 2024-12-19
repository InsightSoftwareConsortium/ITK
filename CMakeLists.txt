# ==== Define cmake build policies that affect compilation and linkage default behaviors
#
# Set the ITK_NEWEST_VALIDATED_POLICIES_VERSION string to the newest cmake version
# policies that provide successful builds. By setting ITK_NEWEST_VALIDATED_POLICIES_VERSION
# to a value greater than the oldest policies, all policies between
# ITK_OLDEST_VALIDATED_POLICIES_VERSION and CMAKE_VERSION (used for this build)
# are set to their NEW behaivor, thereby suppressing policy warnings related to policies
# between the ITK_OLDEST_VALIDATED_POLICIES_VERSION and CMAKE_VERSION.
#
# CMake versions greater than the ITK_NEWEST_VALIDATED_POLICIES_VERSION policies will
# continue to generate policy warnings "CMake Warning (dev)...Policy CMP0XXX is not set:"
#
set(ITK_OLDEST_VALIDATED_POLICIES_VERSION "3.22.1")
set(ITK_NEWEST_VALIDATED_POLICIES_VERSION "3.29.0")
cmake_minimum_required(VERSION ${ITK_OLDEST_VALIDATED_POLICIES_VERSION}...${ITK_NEWEST_VALIDATED_POLICIES_VERSION}
                       FATAL_ERROR)

#
# Now enumerate specific policies newer than ITK_NEWEST_VALIDATED_POLICIES_VERSION
# that may need to be individually set to NEW/OLD
#
foreach(pnew )
  if(POLICY ${pnew})
    cmake_policy(SET ${pnew} NEW)
  endif()
endforeach()
foreach(pold "")
  if(POLICY ${pold})
    cmake_policy(SET ${pold} OLD)
  endif()
endforeach()

# ====
include(${CMAKE_CURRENT_SOURCE_DIR}/CMake/ITKInitializeCXXStandard.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/CMake/ITKInitializeBuildType.cmake)

# ==== Define language standard configurations requiring at least c++17 standard
if(CMAKE_CXX_STANDARD EQUAL "98" OR CMAKE_CXX_STANDARD LESS "17")
  message(FATAL_ERROR "C++98 to C++14 are no longer supported in ITK version 5.4 and greater.")
endif()

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH})

include(itkVersion)
set(ITK_VERSION "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}.${ITK_VERSION_PATCH}")
project(
  ITK
  VERSION ${ITK_VERSION}
  DESCRIPTION
    "The Insight Toolkit (ITK) is an open-source, cross-platform toolkit for N-dimensional scientific image processing, segmentation, and registration."
  HOMEPAGE_URL "https://itk.org"
  LANGUAGES CXX C)

set(ITK_CMAKE_DIR "${ITK_SOURCE_DIR}/CMake")

if(APPLE)
  include(itkApple)
endif()

include(itkCompilerChecks)
include(itkSupportMacros)

# Configure CMake variables that will be used in the module macros
# ITK_USE_FILE is used when a remote module includes an example directory
# that can be configured as an independent project.
set(ITK_CONFIG_CMAKE_DIR "${ITK_SOURCE_DIR}/CMake")
set(ITK_USE_FILE "${ITK_CONFIG_CMAKE_DIR}/UseITK.cmake")
# ITKInternalConfig.cmake is used to handle find_package(ITK) calls. This is useful
# to remote module examples which are set up as independent projects that can be copied
# outside of their original project and used without any modification.
configure_file(CMake/ITKInternalConfig.cmake ${ITK_BINARY_DIR}/CMakeTmp/ITKConfig.cmake COPYONLY)

include(CMakeDependentOption)
#
# use ExternalProject
include(ExternalProject)

if(CMAKE_HOST_WIN32)
  if(NOT DEFINED ITK_SKIP_PATH_LENGTH_CHECKS)
    if(${CMAKE_SYSTEM_VERSION} VERSION_GREATER "10.0.14392") # Win10 version 1607
      set(_LongPathKey LongPathsEnabled)
      execute_process(COMMAND reg query HKLM\\SYSTEM\\CurrentControlSet\\Control\\FileSystem /v ${_LongPathKey}
                      OUTPUT_VARIABLE _output)
      string(
        REGEX MATCH
              "${_LongPathKey}.*REG_DWORD.*[0-9]x([0-9])"
              _regex
              ${_output})
      if(${CMAKE_MATCH_1})
        set(ITK_SKIP_PATH_LENGTH_CHECKS
            0
            CACHE
              BOOL
              "Skips max path length checks. These checks can be disabled for Windows version 10.0.14393 and later if registry key HKLM\\SYSTEM\\CurrentControlSet\\Control\\FileSystem\\LongPathsEnabled is set to 1 and all tools support long paths. As of January 2018, MSBuild and Visual Studio do not support it yet."
        )
      endif()
    endif()
  endif()
endif()

if(CMAKE_HOST_WIN32 AND NOT ITK_SKIP_PATH_LENGTH_CHECKS)

  string(LENGTH "${CMAKE_CURRENT_SOURCE_DIR}" n)
  if(n GREATER 50)
    message(FATAL_ERROR "ITK source code directory path length is too long (${n} > 50)."
                        "Please move the ITK source code directory to a directory with a shorter path.")
  endif()

  string(LENGTH "${CMAKE_CURRENT_BINARY_DIR}" n)
  if(n GREATER 50)
    message(FATAL_ERROR "ITK build directory path length is too long (${n} > 50)."
                        "Please set the ITK build directory to a directory with a shorter path.")
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

if(MSVC) #-- Configure MSVC_STATIC_RUNTIME only if using MSVC environment
  option(ITK_MSVC_STATIC_RUNTIME_LIBRARY "Link to MSVC's static CRT (/MT and /MTd).
OFF (default) means link to regular, dynamic CRT (/MD and /MDd)." OFF)
  mark_as_advanced(ITK_MSVC_STATIC_RUNTIME_LIBRARY)
  set(ITK_MSVC_STATIC_RUNTIME_LIBRARY_value ${ITK_MSVC_STATIC_RUNTIME_LIBRARY})
  if(ITK_MSVC_STATIC_RUNTIME_LIBRARY)
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
    message(STATUS "Using MSVC's static CRT (/MT and /MTd)")
  else()
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>DLL")
    message(STATUS "Using MSVC's dynamic CRT (/MD and /MDd)")
  endif()
endif()

include(ITKDownloadSetup)
include(PreventInSourceBuilds)
include(PreventInBuildInstalls)
include(ITKModuleMacros)
include(ITKExternalData)
include(ITKModuleTest)
include(itkCheckSourceTree)

set(main_project_name ${_ITKModuleMacros_DEFAULT_LABEL})

#-----------------------------------------------------------------------------
configure_file(CMake/ITKConfigVersion.cmake.in ITKConfigVersion.cmake @ONLY)

if(NOT CMAKE_INSTALL_LIBDIR)
  set(CMAKE_INSTALL_LIBDIR lib)
endif()

if(NOT ITK_INSTALL_RUNTIME_DIR)
  set(ITK_INSTALL_RUNTIME_DIR bin)
endif()
if(NOT ITK_INSTALL_LIBRARY_DIR)
  set(ITK_INSTALL_LIBRARY_DIR ${CMAKE_INSTALL_LIBDIR})
endif()
if(NOT ITK_INSTALL_ARCHIVE_DIR)
  set(ITK_INSTALL_ARCHIVE_DIR ${CMAKE_INSTALL_LIBDIR})
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
  set(ITK_INSTALL_PACKAGE_DIR "${CMAKE_INSTALL_LIBDIR}/cmake/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")
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
  message(WARNING "ITK_BRAINWEB_DATA_ROOT is not longer supported!" "Please update to ITK_USE_BRAINWEB_DATA.")
endif()

#-----------------------------------------------------------------------------
# CMAKE_C_COMPILER_ARG1 is a CMake internal variable. It should not be used
# in ITK's configuration. If it is set, it most likely means that
# a developer is configuring ITK to be built with a launcher such as ccache
# or distcc. This should be done with the CMake variable
# CMAKE_C_COMPILER_LAUNCHER_FLAG.
if(CMAKE_C_COMPILER_ARG1)
  message(WARNING "The CMake variable CMAKE_C_COMPILER_ARG1 is set. This most\
  likely mean that the developer is trying to use ccache or distcc. The CMake\
  variable CMAKE_C_COMPILER_LAUNCHER_FLAG should be used instead.")
endif()

#-----------------------------------------------------------------------------
# ITK build configuration options.
option(BUILD_SHARED_LIBS "Build ITK with shared libraries." OFF)
set(ITK_BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS})

set(ITK_DYNAMIC_LOADING_DEFAULT ON)
if(WASI OR EMSCRIPTEN)
  set(ITK_DYNAMIC_LOADING_DEFAULT OFF)
endif()
option(ITK_DYNAMIC_LOADING "Support run-time loading of shared libraries" ${ITK_DYNAMIC_LOADING_DEFAULT})
mark_as_advanced(ITK_DYNAMIC_LOADING)

#-----------------------------------------------------------------------------
# Wrapping options
include(Wrapping/CMakeUtilityFunctions.cmake)
include(Wrapping/WrappingOptions.cmake)
include(ITKSetStandardCompilerFlags)
#---------------------------------------------------------------

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ITK_REQUIRED_C_FLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ITK_REQUIRED_CXX_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
if(NOT CMAKE_POSITION_INDEPENDENT_CODE)
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif()
# Because GoogleTest sets CMAKE_DEBUG_POSTFIX CMake CACHE variable to "d" when it is
# built in debug, we preemptively set it to an empty string to avoid having a postfix
# added to the ITK library names.
set(CMAKE_DEBUG_POSTFIX
    ""
    CACHE STRING "Generate debug library name with a postfix.")
mark_as_advanced(CMAKE_DEBUG_POSTFIX)

# Configure find_package behavior
if(NOT DEFINED CMAKE_FIND_USE_PACKAGE_REGISTRY)
  set(CMAKE_FIND_USE_PACKAGE_REGISTRY 0)
endif()

# Setup build locations for shared libraries ----START
#     ITK/CMakeLists.txt -- use ITK_BINARY_DIR as root
#     ITK/CMake/ITKModuleExternal.cmake -- use ITK_DIR as root
#
# The default path when not wrapping.  Restore standard build location
# if python wrapping is turned on, and then turned off.
if(NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
  set(NO_WRAP_CMAKE_LIBRARY_OUTPUT_DIRECTORY
      ${ITK_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}
      CACHE PATH "Shared library directory")
else()
  set(NO_WRAP_CMAKE_LIBRARY_OUTPUT_DIRECTORY
      ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
      CACHE PATH "Shared library directory")
endif()
if(NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)
  set(NO_WRAP_CMAKE_RUNTIME_OUTPUT_DIRECTORY
      ${ITK_BINARY_DIR}/bin
      CACHE PATH "Shared library directory")
else()
  set(NO_WRAP_CMAKE_RUNTIME_OUTPUT_DIRECTORY
      ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
      CACHE PATH "Shared library directory")
endif()

include(WrappingConfigCommon)
# Setup build locations for shared libraries ----STOP

if(CMAKE_CONFIGURATION_TYPES AND ITK_WRAPPING)
  add_custom_target(
    remove_all_wrapped_libs
    COMMAND ${CMAKE_COMMAND} -E rm -rf ${ITK_PYTHON_PACKAGE_DIR}/*.so ${ITK_PYTHON_PACKAGE_DIR}/*.dylib
            ${ITK_PYTHON_PACKAGE_DIR}/*.dll
    WORKING_DIRECTORY ${ITK_BINARY_DIR}
    COMMENT "A convenience target to cleanup library directories when
    switching between ITK_WRAP/nowrap, or between configs
    in a multi-config build.")
endif()

if(NOT CMAKE_ARCHIVE_OUTPUT_DIRECTORY)
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${ITK_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR})
endif()
set(ITK_MODULES_DIR "${ITK_BINARY_DIR}/${ITK_INSTALL_PACKAGE_DIR}/Modules")

#-----------------------------------------------------------------------------
# Provide compatibility options.
# During major release updates deprecated interface may be needed for backwards compatibility
cmake_dependent_option(
  ITK_LEGACY_REMOVE
  "Remove current legacy code completely."
  OFF
  "NOT ITK_WRAPPING"
  ON)

# During minor releases bugs may be identified that identify broken interface, or
# useless interfaces that need to be retained to not break backwards compatibilty.
# These ITK_FUTURE_LEGACY_REMOVE are another level of granularity for
# which backwards compatible features we want to maintain.
cmake_dependent_option(
  ITK_FUTURE_LEGACY_REMOVE
  "Completely remove compilation of code which will become deprecated by default in ITKv6."
  OFF
  "ITK_LEGACY_REMOVE"
  OFF)
cmake_dependent_option(
  ITK_LEGACY_SILENT
  "Silence all legacy code messages when ITK_LEGACY_REMOVE:BOOL=OFF."
  OFF
  "NOT ITK_LEGACY_REMOVE"
  OFF)

mark_as_advanced(
  ITK_LEGACY_SILENT
  ITK_LEGACY_REMOVE
  ITK_FUTURE_LEGACY_REMOVE
  )

if(ITKV4_COMPATIBILITY)
  message(FATAL_ERROR "ITKV4_COMPATIBILITY is removed starting in ITK version 6.0, this option is no longer valid.")
endif()

if(ITKV3_COMPATIBILITY)
  message(FATAL_ERROR "ITKV3_COMPATIBILITY is removed starting in ITK version 5.0, this option is no longer valid.")
endif()

# Chose flavour of Analyze reader
set(ITK_NIFTI_IO_ANALYZE_FLAVOR_DOC_STRING "Which flavour of Analyze reader to use by default")

set(ITK_NIFTI_IO_ANALYZE_FLAVOR
    "ITK4Warning"
    CACHE STRING ${ITK_NIFTI_IO_ANALYZE_FLAVOR_DOC_STRING})

set_property(
  CACHE ITK_NIFTI_IO_ANALYZE_FLAVOR
  PROPERTY STRINGS
           ITK4Warning
           ITK4
           SPM
           FSL
           Reject)

if(NOT
   ("${ITK_NIFTI_IO_ANALYZE_FLAVOR}" STREQUAL "ITK4Warning"
    OR "${ITK_NIFTI_IO_ANALYZE_FLAVOR}" STREQUAL "ITK4"
    OR "${ITK_NIFTI_IO_ANALYZE_FLAVOR}" STREQUAL "SPM"
    OR "${ITK_NIFTI_IO_ANALYZE_FLAVOR}" STREQUAL "FSL"
    OR "${ITK_NIFTI_IO_ANALYZE_FLAVOR}" STREQUAL "Reject"))
  set(ITK_NIFTI_IO_ANALYZE_FLAVOR
      "ITK4Warning"
      CACHE STRING ${ITK_NIFTI_IO_ANALYZE_FLAVOR_DOC_STRING} FORCE)
endif()
mark_as_advanced(ITK_NIFTI_IO_ANALYZE_FLAVOR)

# Allow non-orthogonal rotation matrix approximation in NIFTI files
option(ITK_NIFTI_IO_SFORM_PERMISSIVE_DEFAULT "Allow non-orthogonal rotation matrix in NIFTI sform by default" OFF)
mark_as_advanced(ITK_NIFTI_IO_SFORM_PERMISSIVE_DEFAULT)

#-----------------------------------------------------------------------------
# ITK build classes that are in the review process
# ITK_USE_REVIEW is deprecated, only kept for backward compatibility
if(ITK_USE_REVIEW AND NOT Module_ITKReview)
  message(WARNING "ITK_USE_REVIEW is deprecated, please use Module_ITKReview to turn Review module ON/OFF")
  set(Module_ITKReview
      ON
      CACHE BOOL "Module containing code from the Review directory of ITKv4." FORCE)
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

if(ITK_USE_GPU)
  include(itkOpenCL)
endif()

#-----------------------------------------------------------------------------
# Manage FFT v3 Options
#
option(ITK_USE_MKL "Use system Intel Math Kernel Library (MKL) for FFT computation." OFF)
mark_as_advanced(ITK_USE_MKL)
if(ITK_USE_MKL)
  # The following lines are only useful if one sets `ITK_USE_MKL` to ON in
  # the command line. Otherwise, the first CMake run will always initialize
  # these variables to false. The error message will help the user to set
  # the appropriate values.
  set(ITK_USE_FFTWD
      ON
      CACHE BOOL "Use double precision FFTW if found")
  set(ITK_USE_FFTWF
      ON
      CACHE BOOL "Use single precision FFTW if found")
  set(ITK_USE_SYSTEM_FFTW
      ON
      CACHE BOOL "Use an installed version of FFTW")
  if(NOT ITK_USE_FFTWD
     OR NOT ITK_USE_FFTWF
     OR NOT ITK_USE_SYSTEM_FFTW)
    message(FATAL_ERROR "ITK_USE_MKL is set to ON. ITK_USE_FFTWD, ITK_USE_FFTWF\
 and ITK_USE_SYSTEM_FFTW must be set to ON, too.")
  endif()
endif()
option(ITK_USE_CUFFTW "Use NVidia CUDA cuFFT with its FFTW interface for FFT computation." OFF)
mark_as_advanced(ITK_USE_CUFFTW)
if(ITK_USE_CUFFTW)
  # The following lines are only useful if one sets `ITK_USE_CUFFTW` to ON in
  # the command line. Otherwise, the first CMake run will always initialize
  # these variables to false. The error message will help the user to set
  # the appropriate values.
  set(ITK_USE_FFTWD
      ON
      CACHE BOOL "Use double precision FFTW if found")
  set(ITK_USE_FFTWF
      ON
      CACHE BOOL "Use single precision FFTW if found")
  set(ITK_USE_SYSTEM_FFTW
      ON
      CACHE BOOL "Use an installed version of FFTW")
  if(NOT ITK_USE_FFTWD
     OR NOT ITK_USE_FFTWF
     OR NOT ITK_USE_SYSTEM_FFTW)
    message(FATAL_ERROR "ITK_USE_CUFFTW is set to ON. ITK_USE_FFTWD, ITK_USE_FFTWF\
 and ITK_USE_SYSTEM_FFTW must be set to ON, too.")
  endif()
endif()
# ITK_USE_FFTWD -- use double precision FFTW
if(DEFINED USE_FFTWD)
  set(ITK_USE_FFTWD_DEFAULT ${USE_FFTWD})
else()
  set(ITK_USE_FFTWD_DEFAULT OFF)
endif()
option(ITK_USE_FFTWD "Use double precision FFTW if found" ${ITK_USE_FFTWD_DEFAULT})
mark_as_advanced(ITK_USE_FFTWD)
#
# ITK_USE_FFTWF -- use single precision FFTW
if(DEFINED USE_FFTWF)
  set(ITK_USE_FFTWF_DEFAULT ${USE_FFTWF})
else()
  set(ITK_USE_FFTWF_DEFAULT OFF)
endif()
option(ITK_USE_FFTWF "Use single precision FFTW if found" ${ITK_USE_FFTWF_DEFAULT})
mark_as_advanced(ITK_USE_FFTWF)

# ITK_USE_SYSTEM_FFTW -- locate a readybuilt FFTW installation
if(DEFINED USE_SYSTEM_FFTW)
  set(ITK_USE_SYSTEM_FFTW_DEFAULT ${USE_SYSTEM_FFTW})
else()
  set(ITK_USE_SYSTEM_FFTW_DEFAULT ${ITK_USE_SYSTEM_LIBRARIES})
endif()
option(ITK_USE_SYSTEM_FFTW "Use an installed version of FFTW" ${ITK_USE_SYSTEM_FFTW_DEFAULT})
mark_as_advanced(ITK_USE_SYSTEM_FFTW)

if(ITK_USE_FFTWD OR ITK_USE_FFTWF)
  include(itkExternal_FFTW)
  # This pollutes the global namespace, but is needed for backward compatibility
  include_directories(${FFTW_INCLUDE})
  link_directories(${FFTW_LIBDIR})
endif()

set(ITK_USE_64BITS_IDS_DEFAULT "OFF")
# Note WIN32 is true when targeting any windows system
if(CMAKE_SIZEOF_VOID_P EQUAL "8" AND WIN32)
  set(ITK_USE_64BITS_IDS_DEFAULT "ON")
endif()
option(
  ITK_USE_64BITS_IDS
  "When ON, ITK will use 64-bit integer types instead of long types for sizes and indexes. This is needed for managing images larger than 4Gb in some platforms."
  "${ITK_USE_64BITS_IDS_DEFAULT}")
mark_as_advanced(ITK_USE_64BITS_IDS)

# ITK turn on concept checking
option(
  ITK_USE_CONCEPT_CHECKING
  "Turn on concept checking to give helpful errors at compile time if a type cannot be used as a template parameter."
  ON)
mark_as_advanced(ITK_USE_CONCEPT_CHECKING)
if(ITK_USE_STRICT_CONCEPT_CHECKING)
  message(
    WARNING
      "Compilers requiring ITK_USE_STRICT_CONCEPT_CHECKING are no longer supported.  This variable is no longer used.")
endif()

#-----------------------------------------------------------------------------
# Mandatory wrapping options.
# These are not correctly set when using cmake without the -DITK_WRAP_PYTHON:BOOL=ON flag.
# In this case we need to tell the user that he has to change the flags manually to the right values.
if(ITK_WRAPPING)
  set(CMAKE_POSITION_INDEPENDENT_CODE 1)
endif()

#----------------------------------------------------------------------------
set(ITK_TEST_OUTPUT_DIR "${ITK_BINARY_DIR}/Testing/Temporary")

# Configure the default ITK_DATA_ROOT for the location of ITK Data.
set(ITK_DATA_ROOT ${ITK_SOURCE_DIR}/Testing/Data)

# Location of ITK Example Data.
set(ITK_EXAMPLE_DATA_ROOT "${ITK_SOURCE_DIR}/Examples/Data")

# This flag is used in particular, to enable some tests that require large memory to run.
# Test that have significant memory requirements should use
# "set_tests_properties(testname PROPERTIES RESOURCE_LOCK MEMORY_SIZE)",
# so that only 1 of the memory constrained tests is run at a time.  (NOTE: Other low
# memory tests that do not have the RESOURCE_LOCK requirement can be run at the same time.)
# Allow for tests that may use upto 80% of the computer memory to be used for testing.
cmake_host_system_information(RESULT ITK_COMPUTER_MEMORY_AVAILABLE_MB QUERY TOTAL_PHYSICAL_MEMORY)
math(EXPR EIGHTY_PERCENT_AVAILABLE_MEMORY_IN_GB "(${ITK_COMPUTER_MEMORY_AVAILABLE_MB} * 80) / ( 1024 * 100)")
set(ITK_COMPUTER_MEMORY_SIZE
    ${EIGHTY_PERCENT_AVAILABLE_MEMORY_IN_GB}
    CACHE STRING "Provide here the size of your RAM in gibibytes")
unset(EIGHTY_PERCENT_AVAILABLE_MEMORY_IN_GB)
unset(ITK_COMPUTER_MEMORY_AVAILABLE_MB)
mark_as_advanced(ITK_COMPUTER_MEMORY_SIZE)

#This flag sets the floating point type used in itk::ImageBase for
# spacing/direction/origin to single precision
option(ITK_USE_FLOAT_SPACE_PRECISION "Use single precision for origin/spacing/directions in itk::Image" OFF)
mark_as_advanced(ITK_USE_FLOAT_SPACE_PRECISION)
if( ITK_USE_FLOAT_SPACE_PRECISION AND ITK_WRAP_PYTHON)
  message(FATAL_ERROR "ITK_USE_FLOAT_SPACE_PRECISION=ON not supported with ITK_WRAP_PYTHON=ON")
endif()

# This flag allows to not build itkTestDrivers while still configuring all the tests and disable all
# tests that are implemented in the `test` folder in each ITK module. It does not disable tests
# that are implemented in the wrapping folder.
# This is used by dashboards to speed up compilation and testing on certain platforms.
option(DISABLE_MODULE_TESTS "Disable all ITK module tests in `test` folder" OFF)
mark_as_advanced(DISABLE_MODULE_TESTS)

#-----------------------------------------------------------------------------
# Need to set python variables before remote modules are included.
if(BUILD_TESTING
   OR ITK_BUILD_DOCUMENTATION
   OR ITK_WRAP_PYTHON)
  set(PYTHON_DEVELOPMENT_REQUIRED ${ITK_WRAP_PYTHON})
  include(${CMAKE_CURRENT_SOURCE_DIR}/CMake/ITKSetPython3Vars.cmake)

  if(ITK_WRAP_PYTHON AND NOT Python3_INCLUDE_DIRS)
    message(FATAL_ERROR "Python version ${Python3_VERSION} development environment not found for wrapping.")
  endif()
  if(NOT Python3_ROOT_DIR)
    get_filename_component(Python3_ROOT_DIR ${Python3_EXECUTABLE} DIRECTORY)
  endif()
endif()

#-----------------------------------------------------------------------------
# Configure Eigen3 before ITKModuleEnablement
# No download or build is performed. Generates Targets and Config.cmake files for Eigen
include(itkExternal_Eigen3)

#----------------------------------------------------------------------
# Make sure remote modules are downloaded before sorting out the module
# dependencies.
add_subdirectory(Modules/Remote)

# Enable modules according to user inputs and the module dependency DAG.
include(ITKModuleEnablement)

# Setup clang-tidy for code best-practices enforcement for C++11
include(ITKClangTidySetup)
#----------------------------------------------------------------------
# Generate ITKConfig.cmake for the build tree.
set(ITK_CONFIG_CODE "
set(ITK_MODULES_DIR \"${ITK_MODULES_DIR}\")")

set(ITK_CONFIG_TARGETS_CONDITION " AND NOT ITK_BINARY_DIR")
set(ITK_CONFIG_TARGETS_FILE "${ITK_BINARY_DIR}/ITKTargets.cmake")
set(ITK_CONFIG_MODULE_API_FILE "${ITK_SOURCE_DIR}/CMake/ITKModuleAPI.cmake")
configure_file(CMake/ITKConfig.cmake.in ITKConfig.cmake @ONLY)

# Generate ITKConfig.cmake for the install tree.
set(ITK_CONFIG_CODE
    "
# Compute the installation prefix from this ITKConfig.cmake file location.
get_filename_component(ITK_INSTALL_PREFIX \"\${CMAKE_CURRENT_LIST_FILE}\" PATH)")
# Construct the proper number of get_filename_component(... PATH)
# calls to compute the installation prefix.
string(
  REGEX
  REPLACE "/"
          ";"
          _count
          "${ITK_INSTALL_PACKAGE_DIR}")
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
  set(FFTW_LIBDIR "\${ITK_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")
  set(FFTW_INCLUDE_PATH "\${ITK_INSTALL_PREFIX}/include/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}/Algorithms")
endif()
configure_file(CMake/ITKConfig.cmake.in CMakeFiles/ITKConfig.cmake @ONLY)

#-----------------------------------------------------------------------------
install(
  FILES ${ITK_BINARY_DIR}/CMakeFiles/ITKConfig.cmake
        ${ITK_BINARY_DIR}/ITKConfigVersion.cmake
        CMake/ITKModuleAPI.cmake
        CMake/UseITK.cmake
        CMake/ITKFactoryRegistration.cmake
        CMake/itkFFTImageFilterInitFactoryRegisterManager.h.in
        CMake/itkImageIOFactoryRegisterManager.h.in
        CMake/itkTransformIOFactoryRegisterManager.h.in
        CMake/itkMeshIOFactoryRegisterManager.h.in
        CMake/ITKInitializeCXXStandard.cmake
  DESTINATION ${ITK_INSTALL_PACKAGE_DIR}
  COMPONENT Development)
get_property(ITKTargets_MODULES GLOBAL PROPERTY ITKTargets_MODULES)
if(ITKTargets_MODULES)
  install(
    EXPORT ITKTargets
    DESTINATION ${ITK_INSTALL_PACKAGE_DIR}
    COMPONENT Development)
else()
  set(CMAKE_CONFIGURABLE_FILE_CONTENT "# No targets!")
  configure_file(${CMAKE_ROOT}/Modules/CMakeConfigurableFile.in ${ITK_BINARY_DIR}/CMakeFiles/ITKTargets.cmake @ONLY)
  install(
    FILES ${ITK_BINARY_DIR}/CMakeFiles/ITKTargets.cmake
    DESTINATION ${ITK_INSTALL_PACKAGE_DIR}
    COMPONENT Development)
endif()

install(
  FILES "LICENSE" "NOTICE" "README.md"
  DESTINATION ${ITK_INSTALL_DOC_DIR}
  COMPONENT Runtime)

if(BUILD_TESTING)
  # If building the testing, write the test costs (i.e. time to run)
  # analysis to disk to more easily review long-running test times
  set(CTEST_COST_DATA_FILE
      "${CMAKE_BINARY_DIR}/test_cost.list"
      CACHE FILEPATH "Configurable path to CTest cost data file.")
  mark_as_advanced(CTEST_COST_DATA_FILE)
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
include(ExternalData)
ExternalData_add_target(ITKData)

mark_as_advanced(
  FORCE
  Module_AdaptiveDenoising
  Module_AnalyzeObjectLabelMap
  Module_AnisotropicDiffusionLBR
  Module_BioCell
  Module_BoneEnhancement
  Module_BoneMorphometry
  Module_BSplineGradient
  Module_Cleaver
  Module_Cuberille
  Module_CudaCommon
  Module_FixedPointInverseDisplacementField
  Module_GenericLabelInterpolator
  Module_GrowCut
  Module_HASI
  Module_HigherOrderAccurateGradient
  Module_IOFDF
  Module_IOMeshSTL
  Module_IOOpenSlide
  Module_IOScanco
  Module_IOTransformDCMTK
  Module_IsotropicWavelets
  Module_LabelErodeDilate
  Module_LesionSizingToolkit
  Module_MeshNoise
  Module_MeshToPolyData
  Module_MGHIO
  Module_MinimalPathExtraction
  Module_Montage
  Module_MorphologicalContourInterpolation
  Module_MultipleImageIterator
  Module_ParabolicMorphology
  Module_PerformanceBenchmarking
  Module_PhaseSymmetry
  Module_PolarTransform
  Module_PrincipalComponentsAnalysis
  Module_RLEImage
  Module_RTK
  Module_SCIFIO
  Module_Shape
  Module_SimpleITKFilters
  Module_SkullStrip
  Module_SmoothingRecursiveYvvGaussianFilter
  Module_SphinxExamples
  Module_SplitComponents
  Module_Strain
  Module_SubdivisionQuadEdgeMeshFilter
  Module_TextureFeatures
  Module_Thickness3D
  Module_TotalVariation
  Module_TubeTK
  Module_TwoProjectionRegistration
  Module_Ultrasound
  Module_VariationalRegistration
  Module_VkFFTBackend
  Module_WebAssemblyInterface)

mark_as_advanced(FORCE NSIS_EXECUTABLE WIX_EXECUTABLE)

# Create an IDE folder structure similar to our source structure
if("${CMAKE_GENERATOR}" MATCHES "Xcode|Visual Studio|KDevelop")
  set_property(GLOBAL PROPERTY USE_FOLDERS ON)
  function(itk_organize_targets dir folder)
    itk_get_all_targets_recursive(targets ${dir})
    foreach(t ${targets})
      # INTERFACE_LIBRARY warns about non-whitelisted properties for CMake < 3.19
      get_target_property(type ${t} TYPE)
      if(NOT
         "${type}"
         STREQUAL
         "INTERFACE_LIBRARY")
        get_target_property(f ${t} SOURCE_DIR)
        string(
          REPLACE ${dir}
                  ""
                  f
                  ${f})
        set_target_properties(${t} PROPERTIES FOLDER "${folder}/${f}")
      endif()
    endforeach()
  endfunction()
  macro(itk_get_all_targets_recursive targets dir)
    get_property(
      subdirectories
      DIRECTORY ${dir}
      PROPERTY SUBDIRECTORIES)
    foreach(subdir ${subdirectories})
      itk_get_all_targets_recursive(${targets} ${subdir})
    endforeach()
    get_property(
      current_targets
      DIRECTORY ${dir}
      PROPERTY BUILDSYSTEM_TARGETS)
    list(APPEND ${targets} ${current_targets})
  endmacro()
  itk_organize_targets(${CMAKE_SOURCE_DIR} ITK)
endif()
