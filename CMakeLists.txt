if(WIN32)
  cmake_minimum_required(VERSION 2.8.8 FATAL_ERROR)
else()
  cmake_minimum_required(VERSION 2.8.6 FATAL_ERROR)
endif()

foreach(p
    CMP0025 # CMake 3.0
    CMP0042 # CMake 3.0
    CMP0056 # CMake 3.2
    CMP0058 # CMake 3.3
    )
  if(POLICY ${p})
    cmake_policy(SET ${p} NEW)
  endif()
endforeach()

project(ITK)

include(CMakeDependentOption)
#
# use ExternalProject
include(ExternalProject)

if( CMAKE_HOST_WIN32 )

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

set(CMAKE_MODULE_PATH ${ITK_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH})
include(PreventInSourceBuilds)
include(PreventInBuildInstalls)
include(ITKModuleMacros)
include(ITKExternalData)
include(itkCheckSourceTree)

set(main_project_name ${_ITKModuleMacros_DEFAULT_LABEL})

#-----------------------------------------------------------------------------
# ITK version number.
set(ITK_VERSION_MAJOR "4")
set(ITK_VERSION_MINOR "8")
set(ITK_VERSION_PATCH "0")

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
# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()


#-----------------------------------------------------------------------------
# Enable running cppcheck for each module on its source and test directories.
option(ITK_CPPCHECK_TEST "Run cppcheck for static code analysis" OFF)
mark_as_advanced(ITK_CPPCHECK_TEST)

#-----------------------------------------------------------------------------
# Forbid downloading resources from the network during a build. This helps
# when building on systems without network connectivity to determine which
# resources much be obtained manually and made available to the build.
option(ITK_FORBID_DOWNLOADS "Do not download source code or data from the network" OFF)
mark_as_advanced(ITK_FORBID_DOWNLOADS)
macro(itk_download_attempt_check _name)
  if(ITK_FORBID_DOWNLOADS)
    message(SEND_ERROR "Attempted to download ${_name} when ITK_FORBID_DOWNLOADS is ON")
  endif()
endmacro()

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
# ITK wrapper for add_test that automatically sets the test's LABELS property
# to the value of its containing module.
#
function(itk_add_test)
  # Add tests with data in the ITKData group.
  ExternalData_add_test(ITKData ${ARGN})

  if("NAME" STREQUAL "${ARGV0}")
    set(_iat_testname ${ARGV1})
  else()
    set(_iat_testname ${ARGV0})
  endif()

  if(itk-module)
    set(_label ${itk-module})
    if(TARGET ${itk-module}-all AND "${ARGN}" MATCHES "DATA{")
      add_dependencies(${itk-module}-all ITKData)
    endif()
  else()
    set(_label ${main_project_name})
  endif()

  set_property(TEST ${_iat_testname} PROPERTY LABELS ${_label})
endfunction()

#-----------------------------------------------------------------------------
# ITK function to ignore a test
#
function(itk_tests_ignore)
  set_property(GLOBAL APPEND PROPERTY CTEST_CUSTOM_TESTS_IGNORE ${ARGN})
endfunction()

#-----------------------------------------------------------------------------
# ITK function to ignore a test during MemCheck
#
function(itk_memcheck_ignore)
  set_property(GLOBAL APPEND PROPERTY CTEST_CUSTOM_MEMCHECK_IGNORE ${ARGN})
endfunction()

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
  # ITK_WRAPPING is an internal variable
  set(ITK_WRAPPING ON CACHE INTERNAL "Build external languages support" FORCE)
else()
  set(ITK_WRAPPING OFF CACHE INTERNAL "Build external languages support" FORCE)
endif()

#-----------------------------------------------------------------------------
# ITK build configuration options.
if(ITK_WRAPPING)
  option(BUILD_SHARED_LIBS "Build ITK with shared libraries." ON)
else()
  option(BUILD_SHARED_LIBS "Build ITK with shared libraries." OFF)
endif()
set(ITK_BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS})

option(ITK_DYNAMIC_LOADING "Support loading of dynamic libraries" ON)

include(ITKSetStandardCompilerFlags)
#---------------------------------------------------------------
# run try compiles and tests for ITK
include(itkTestFriendTemplatedFunction)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ITK_REQUIRED_C_FLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ITK_REQUIRED_CXX_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")

include(CTest)

include( CppcheckTargets )

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
# By default, ITK builds the Examples that are illustrated in the Software Guide
option(BUILD_EXAMPLES "Build the Examples directory." ON)

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
  set(ITK_USE_SYSTEM_FFTW_DEFAULT OFF)
endif()
option(ITK_USE_SYSTEM_FFTW "Use an installed version of fftw" ${ITK_USE_SYSTEM_FFTW_DEFAULT})
mark_as_advanced(ITK_USE_SYSTEM_FFTW)


if( ITK_USE_FFTWD OR ITK_USE_FFTWF )
  include(itkExternal_FFTW)
endif()

if(NOT ITKV3_COMPATIBILITY)
  option(ITK_USE_64BITS_IDS "When ON, ITK will use 64 bits integers to index pixels. This is needed for managing images larger than 4Gb in some platforms." OFF)
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

  if(NOT ITK_BUILD_SHARED_LIBS)
    message(FATAL_ERROR "Wrapping requires shared libs, please set BUILD_SHARED_LIBS=ON")
  endif()

  if(ITK_USE_64BITS_IDS AND WIN32)
    message(FATAL_ERROR "Wrapping with ITK_USE_64BITS_IDS is not supported on Windows, please set ITK_USE_64BITS_IDS=OFF")
  endif()

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
set(ITK_USE_FILE "${ITK_SOURCE_DIR}/CMake/UseITK.cmake")
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
ExternalData_Add_Target(ITKData)
