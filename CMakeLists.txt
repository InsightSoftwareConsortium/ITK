cmake_minimum_required(VERSION 3.10.2 FATAL_ERROR)

if(CMAKE_CXX_STANDARD EQUAL "98" )
   message(FATAL_ERROR "CMAKE_CXX_STANDARD:STRING=98 is not supported in ITK version 5 and greater.")
endif()

#####
##  Set the default target properties for ITK
if(NOT CMAKE_CXX_STANDARD)
  set(CMAKE_CXX_STANDARD 11) # Supported values are ``11``, ``14``, and ``17``.
endif()
if(NOT CMAKE_CXX_STANDARD_REQUIRED)
  set(CMAKE_CXX_STANDARD_REQUIRED ON)
endif()
if(NOT CMAKE_CXX_EXTENSIONS)
  set(CMAKE_CXX_EXTENSIONS OFF)
endif()

foreach(p
    ## Only policies introduced after the cmake_minimum_required
    ## version need to explicitly be set to NEW.
    CMP0070 #3.10.0 Define ``file(GENERATE)`` behavior for relative paths.
    CMP0071 #3.10.0 Let ``AUTOMOC`` and ``AUTOUIC`` process ``GENERATED`` files.
    CMP0074 #3.12.0 `find_package()`` uses ``<PackageName>_ROOT`` variables.
    )
  if(POLICY ${p})
    cmake_policy(SET ${p} NEW)
  endif()
endforeach()

include(${CMAKE_CURRENT_SOURCE_DIR}/CMake/ITKInitializeBuildType.cmake)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH})

include(itkVersion)
set(ITK_VERSION
    "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}.${ITK_VERSION_PATCH}")
project(ITK
  VERSION ${ITK_VERSION}
  LANGUAGES CXX C)

set(ITK_CMAKE_DIR "${ITK_SOURCE_DIR}/CMake")

if (APPLE)
  include(itkApple)
endif ()

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

if( CMAKE_HOST_WIN32 )
  if(NOT DEFINED ITK_SKIP_PATH_LENGTH_CHECKS)
    if(${CMAKE_SYSTEM_VERSION} VERSION_GREATER "10.0.14392") # Win10 version 1607
      set(_LongPathKey LongPathsEnabled)
      execute_process(COMMAND reg query HKLM\\SYSTEM\\CurrentControlSet\\Control\\FileSystem /v ${_LongPathKey} OUTPUT_VARIABLE _output)
      string(REGEX MATCH "${_LongPathKey}.*REG_DWORD.*[0-9]x([0-9])" _regex ${_output})
      if (${CMAKE_MATCH_1})
        set(ITK_SKIP_PATH_LENGTH_CHECKS 0 CACHE BOOL "Skips max path length checks. These checks can be disabled for Windows version 10.0.14393 and later if registry key HKLM\\SYSTEM\\CurrentControlSet\\Control\\FileSystem\\LongPathsEnabled is set to 1 and all tools support long paths. As of January 2018, MSBuild and Visual Studio do not support it yet.")
      endif()
    endif()
  endif()
endif()

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
set(CMAKE_DEBUG_POSTFIX "" CACHE STRING "Generate debug library name with a postfix.")
mark_as_advanced(CMAKE_DEBUG_POSTFIX)

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
option(ITKV4_COMPATIBILITY "Enable compatibility with ITK4.x when possible." OFF)
# During major release updates deprecated interface may be needed for backwards compatibility
option(ITK_LEGACY_REMOVE "Remove current legacy code completely." OFF)
# During minor releases bugs may be identified that identify broken interface, or
# useless interfaces that need to be retained to not break backwards compatibilty.
# These ITK_FUTURE_LEGACY_REMOVE are another level of granularity for
# which backwards compatible features we want to maintain.
option(ITK_FUTURE_LEGACY_REMOVE "Completely remove compilation of code which will become deprecated by default in ITKv5." OFF)
if(ITK_WRAPPING)
  option(ITK_LEGACY_SILENT "Silence all legacy code messages." ON)
else()
  option(ITK_LEGACY_SILENT "Silence all legacy code messages." OFF)
endif()
mark_as_advanced(ITK_LEGACY_SILENT ITK_LEGACY_REMOVE ITK_FUTURE_LEGACY_REMOVE ITKV4_COMPATIBILITY)

# The disabling of legacy code, and the ITKv4 compatibility option can not both be
# requested. If removal of legacy code is requested, then ITKV4_COMPATIBILITY must
# be off.
if(ITKV4_COMPATIBILITY AND ITK_LEGACY_REMOVE)
  message(FATAL_ERROR "Invalid configuration: ITKV4_COMPATIBILITY AND ITK_LEGACY_REMOVE can not both be ON")
endif()
if (NOT ITK_LEGACY_SILENT AND ITKV3_COMPATIBILITY)
  message(WARNING "ITKV3_COMPATIBILITY is removed starting in ITK version 5.0.")
endif()

#-----------------------------------------------------------------------------
# ITK build classes that are in the review process
# ITK_USE_REVIEW is deprecated, only kept for backward compatibility
if (ITK_USE_REVIEW AND NOT Module_ITKReview)
  message(WARNING "ITK_USE_REVIEW is deprecated, please use Module_ITKReview to turn Review module ON/OFF")
  set(Module_ITKReview ON CACHE BOOL "Module containing code from the Review directory of ITKv4." FORCE)
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
option(ITK_USE_MKL "Use system Intel Math Kernel Library (MKL) for FFT computation." OFF)
mark_as_advanced(ITK_USE_MKL)
if(ITK_USE_MKL)
  # The following lines are only useful if one sets `ITK_USE_MKL` to ON in
  # the command line. Otherwise, the first CMake run will always initialize
  # these variables to false. The error message will help the user to set
  # the appropriate values.
  set(ITK_USE_FFTWD ON CACHE BOOL "Use double precision fftw if found")
  set(ITK_USE_FFTWF ON CACHE BOOL "Use single precision fftw if found")
  set(ITK_USE_SYSTEM_FFTW ON CACHE BOOL "Use an installed version of fftw")
  if(NOT ITK_USE_FFTWD OR NOT ITK_USE_FFTWF OR NOT ITK_USE_SYSTEM_FFTW)
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
  set(ITK_USE_FFTWD ON CACHE BOOL "Use double precision fftw if found")
  set(ITK_USE_FFTWF ON CACHE BOOL "Use single precision fftw if found")
  set(ITK_USE_SYSTEM_FFTW ON CACHE BOOL "Use an installed version of fftw")
  if(NOT ITK_USE_FFTWD OR NOT ITK_USE_FFTWF OR NOT ITK_USE_SYSTEM_FFTW)
    message(FATAL_ERROR "ITK_USE_CUFFTW is set to ON. ITK_USE_FFTWD, ITK_USE_FFTWF\
 and ITK_USE_SYSTEM_FFTW must be set to ON, too.")
  endif()
endif()
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

set(ITK_USE_64BITS_IDS_DEFAULT "OFF")
# Note WIN32 is true when targeting any windows system
if(CMAKE_SIZEOF_VOID_P EQUAL "8" AND WIN32)
  set(ITK_USE_64BITS_IDS_DEFAULT "ON")
endif()
option(ITK_USE_64BITS_IDS "When ON, ITK will use 64-bit integer types instead of long types for sizes and indexes. This is needed for managing images larger than 4Gb in some platforms." "${ITK_USE_64BITS_IDS_DEFAULT}")
mark_as_advanced(ITK_USE_64BITS_IDS)

# ITK turn on concept checking
option(ITK_USE_CONCEPT_CHECKING "Turn on concept checking to give helpful errors at compile time if a type cannot be used as a template parameter." ON)
mark_as_advanced(ITK_USE_CONCEPT_CHECKING)
if(ITK_USE_STRICT_CONCEPT_CHECKING)
  message(WARNING "Compilers requiring ITK_USE_STRICT_CONCEPT_CHECKING are no longer suppported.  This variable is no longer used.")
endif()

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
set(ITK_COMPUTER_MEMORY_SIZE 1 CACHE STRING "Provide here the size of your RAM in gibibytes")
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
install(FILES ${ITK_BINARY_DIR}/CMakeFiles/ITKConfig.cmake
              ${ITK_BINARY_DIR}/ITKConfigVersion.cmake
              CMake/ITKModuleAPI.cmake
              CMake/UseITK.cmake
              CMake/itkImageIOFactoryRegisterManager.h.in
              CMake/itkTransformIOFactoryRegisterManager.h.in
              CMake/itkMeshIOFactoryRegisterManager.h.in
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

install(FILES "LICENSE" "NOTICE" "README.md" DESTINATION ${ITK_INSTALL_DOC_DIR} COMPONENT Runtime)

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
include(ExternalData)
ExternalData_Add_Target(ITKData)
