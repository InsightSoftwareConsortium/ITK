project(WrapITK)

# check cmake version
# Require 2.4.2 for MATH and LIST commands (REMOVE_ITEM is not the same in
# 2.4.1)
cmake_minimum_required(VERSION 2.4.2 FATAL_ERROR)

# needed for the insight journal
include(IJMacros.txt)

enable_testing()

if(CMAKE_COMPILER_IS_GNUCXX)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w")
endif(CMAKE_COMPILER_IS_GNUCXX)

###############################################################################
# Config ITKWrap
###############################################################################

# Set up options needed for ConfigureWrapping

# load the language autodetection stuff for the insight journal
include(LanguageAutoDetection.cmake)

# set WrapITK version
set(WrapITK_VERSION_MAJOR 0)
set(WrapITK_VERSION_MINOR 2)
set(WrapITK_VERSION_PATCH 1)


option(WRAP_ITK_TCL "Build cswig Tcl wrapper support." OFF)
option(WRAP_ITK_PYTHON "Build cswig Python wrapper support." OFF)
option(WRAP_ITK_JAVA "Build cswig Java wrapper support." OFF)
# option(WRAP_ITK_PERL "Build cswig Perl wrapper support." OFF)

option(WRAP_unsigned_char "Wrap unsigned char type" OFF)
option(WRAP_unsigned_short "Wrap unsigned short type" ON)
option(WRAP_unsigned_long "Wrap unsigned long type" OFF)

option(WRAP_signed_char "Wrap signed char type" OFF)
option(WRAP_signed_short "Wrap signed short type" OFF)
option(WRAP_signed_long "Wrap signed long type" OFF)

option(WRAP_float "Wrap float type" ON)
option(WRAP_double "Wrap double type" OFF)

option(WRAP_vector_float "Wrap vector float type" ON)
option(WRAP_vector_double "Wrap vector double type" OFF)

option(WRAP_covariant_vector_float "Wrap covariant vector float type" ON)
option(WRAP_covariant_vector_double "Wrap covariant vector double type" OFF)

option(WRAP_rgb_unsigned_char "Wrap RGB< unsigned char > type" OFF)
option(WRAP_rgb_unsigned_short "Wrap RGB< unsigned short > type" ON)

option(WRAP_rgba_unsigned_char "Wrap RGBA< unsigned char > type" OFF)
option(WRAP_rgba_unsigned_short "Wrap RGBA< unsigned short > type" ON)

option(WRAP_complex_float "Wrap complex<float> type" ON)
option(WRAP_complex_double "Wrap complex<double> type" OFF)

set(WRAP_ITK_DIMS "2;3" CACHE STRING "dimensions available separated by semicolons (;)")

option(INSTALL_WRAP_ITK_COMPATIBILITY "Install FindWrapITK.cmake and WrapITK.pth (so cmake and python can find WrapITK) as part of the standard installation process. This requires admin privileges. These files can be separately installed by making the install_wrapitk_compatibility target. " ON)

set(WRAP_ITK_INSTALL_PREFIX "/lib/InsightToolkit/WrapITK/" CACHE PATH "subpath where where most of WrapITK files will be installed")


# Output directories.
set(WRAP_ITK_CONFIG_DIR "${WrapITK_SOURCE_DIR}/ConfigurationInputs")
set(WRAP_ITK_CMAKE_DIR "${WrapITK_SOURCE_DIR}")
set(WRAP_ITK_JAVA_DIR "${WrapITK_BINARY_DIR}/Java")
if(NOT LIBRARY_OUTPUT_PATH)
  set(LIBRARY_OUTPUT_PATH ${WrapITK_BINARY_DIR}/lib CACHE INTERNAL "Single output directory for building all libraries.")
endif(NOT LIBRARY_OUTPUT_PATH)
if(NOT EXECUTABLE_OUTPUT_PATH)
  set(EXECUTABLE_OUTPUT_PATH ${WrapITK_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all executables.")
endif(NOT EXECUTABLE_OUTPUT_PATH)
mark_as_advanced(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH WRAP_ITK_INSTALL_PREFIX)
set(CXX_TEST_PATH ${EXECUTABLE_OUTPUT_PATH})

# Now include ConfigureWrapping
include(ConfigureWrapping.cmake)

# Now make sure that itk.swg is copied to WRAPPER_SWIG_LIBRARY_OUTPUT_DIR, which
# is set in ConfigureWrapping. We do this so there is only one place that swig
# files needed for a WrapITK build are placed: in the binary directory. (For
# external projects, we look in the WrapITK binary dir and the project's binary
# dir.)
set(src "${WrapITK_SOURCE_DIR}/itk.swg")
set(tgt "${WRAPPER_SWIG_LIBRARY_OUTPUT_DIR}/itk.swg")
exec_program(${CMAKE_COMMAND} ARGS -E copy_if_different "\"${src}\"" "\"${tgt}\"")

add_subdirectory(SwigRuntime)

###############################################################################
# Configure CPack
###############################################################################

# CPack was introduced in cmake 2.4
set(CMAKE_VERSION "${CMAKE_CACHE_MAJOR_VERSION}.${CMAKE_CACHE_MINOR_VERSION}.${CMAKE_CACHE_RELEASE_VERSION}")
# if("${CMAKE_VERSION}" STRGREATER "2.4")
#   set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Enhanced languages support for the Insight Toolkit")
#Ê  set(CPACK_PACKAGE_VENDOR "Gaetan Lehmann, Zachary Pincus")
#   set(CPACK_PACKAGE_VERSION_MAJOR "${WrapITK_VERSION_MAJOR}")
#   set(CPACK_PACKAGE_VERSION_MINOR "${WrapITK_VERSION_MINOR}")
#   set(CPACK_PACKAGE_VERSION_PATCH "${WrapITK_VERSION_PATCH}")
#   # set(CPACK_PACKAGE_INSTALL_DIRECTORY "WrapITK-${WrapITK_VERSION_MAJOR}.${WrapITK_VERSION_MINOR}")
#   # set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")
#   set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/Documentation/Copyright/ITKCopyright.txt")
#   include(CPack)
# endif("${CMAKE_VERSION}" STRGREATER "2.4")

###############################################################################
# Configure specific wrapper modules
###############################################################################

set(WRAP_ITK_MODULES
  VXLNumerics
  Base
  BaseNumerics
  BaseTransforms
  BaseSpatialObject)

# set Iterators module to OFF by default
# it is currently usable only when applying patches to ITK, and will only
# take place and time to build
# TODO: remove this line when iterators will be usable without patch
option(WRAP_Iterators "Build Iterators module." OFF)

# Set FFTW to on if USE_FFTWF or USE_FFTWD are on.
if(USE_FFTWD OR USE_FFTWF)
  option(WRAP_FFTW "Build FFTW module." ON)
else(USE_FFTWD OR USE_FFTWF)
  option(WRAP_FFTW "Build FFTW module." OFF)
endif(USE_FFTWD OR USE_FFTWF)


set(optional_modules
  BinaryMorphology
  Calculators
  Compose
  DeformableTransforms
  Denoising
  DistanceMap
  EdgesAndContours
  FFT
  FFTW
  Filtering
  IntensityFilters
  Interpolators
  IO
  Iterators
  LevelSet
  Morphology
  #Patented
  PixelMath
  Registration
  Resize
  Review
  SegmentationAndThreshold
  SegmentationValidation
  SimpleFilters
  UnaryPixelMath)

foreach(module ${optional_modules})
  option(WRAP_${module} "Build ${module} module." ON)
  mark_as_advanced(WRAP_${module})
  if(WRAP_${module})
    set(WRAP_ITK_MODULES ${WRAP_ITK_MODULES} ${module})
  endif(WRAP_${module})
endforeach(module)

foreach(module ${WRAP_ITK_MODULES})
  add_subdirectory(Modules/${module})
endforeach(module)

###############################################################################
# Configure language-specific features
###############################################################################
if(WRAP_ITK_TCL)
  add_subdirectory(Tcl)
endif(WRAP_ITK_TCL)

if(WRAP_ITK_PERL)
  add_subdirectory(Perl)
endif(WRAP_ITK_PERL)

if(WRAP_ITK_PYTHON)
  add_subdirectory(Python)
endif(WRAP_ITK_PYTHON)

if(WRAP_ITK_JAVA)
  add_subdirectory(Java)
endif(WRAP_ITK_JAVA)

###############################################################################
# Configure WrapITKConfig.cmake and FindWrapITK.cmake for external projects
###############################################################################
# First, configure for use directly from the build dir.
set(CONFIG_CONFIG_DIR "${WrapITK_SOURCE_DIR}/ConfigurationInputs")
set(CONFIG_CMAKE_DIR "${WrapITK_SOURCE_DIR}")
set(CONFIG_WRAP_ITK_SWIG_LIBRARY_DIRECTORY "${WRAPPER_SWIG_LIBRARY_OUTPUT_DIR}")
set(CONFIG_WRAP_ITK_MASTER_INDEX_DIRECTORY "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}")
set(CONFIG_WRAP_ITK_INSTALL_LOCATION "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}")
set(CONFIG_WRAP_ITK_INSTALL_PREFIX "${WRAP_ITK_INSTALL_PREFIX}")
set(CONFIG_WRAP_ITK_MODULES "${WRAP_ITK_MODULES}")
set(CONFIG_WRAP_ITK_PYTHON_MODULE "${WrapITK_BINARY_DIR}/Python/\\\${CTEST_CONFIGURATION_TYPE}")
set(CONFIG_WRAP_ITK_PYTHON_LIBS "${LIBRARY_OUTPUT_PATH}/\\\${CTEST_CONFIGURATION_TYPE}")
set(CONFIG_WRAP_ITK_LIB_DIR  "${LIBRARY_OUTPUT_PATH}")
configure_file("${WRAP_ITK_CONFIG_DIR}/WrapITKConfig.cmake.in"
               "${WrapITK_BINARY_DIR}/WrapITKConfig.cmake"
               @ONLY IMMEDIATE)

set(CONFIG_WRAP_ITK_DIR "${WrapITK_BINARY_DIR}")
configure_file("${WRAP_ITK_CONFIG_DIR}/FindWrapITK.cmake.in"
               "${WrapITK_BINARY_DIR}/FindWrapITK.cmake"
               @ONLY IMMEDIATE)

# Second, configure for installed use. Ensure that no absolute paths
# go into the installed config file, so that it is as portable as possible.
set(CONFIG_CONFIG_DIR "\${WrapITK_DIR}/Configuration/Inputs")
set(CONFIG_CMAKE_DIR "\${WrapITK_DIR}/Configuration")
set(CONFIG_WRAP_ITK_SWIG_LIBRARY_DIRECTORY "\${WrapITK_DIR}/SWIG")
set(CONFIG_WRAP_ITK_MASTER_INDEX_DIRECTORY "\${WrapITK_DIR}/ClassIndex")
set(CONFIG_WRAP_ITK_INSTALL_LOCATION "\${WrapITK_DIR}")
set(CONFIG_WRAP_ITK_PYTHON_MODULE "\${WrapITK_DIR}/Python/")
set(CONFIG_WRAP_ITK_PYTHON_LIBS "\${WrapITK_DIR}/lib")
set(CONFIG_WRAP_ITK_LIB_DIR  "\${WrapITK_DIR}/lib")
configure_file("${WRAP_ITK_CONFIG_DIR}/WrapITKConfig.cmake.in"
               "${WrapITK_BINARY_DIR}/InstallOnly/WrapITKConfig.cmake"
               @ONLY IMMEDIATE)

set(CONFIG_WRAP_ITK_DIR "${CMAKE_INSTALL_PREFIX}/${WRAP_ITK_INSTALL_PREFIX}")
configure_file("${WRAP_ITK_CONFIG_DIR}/FindWrapITK.cmake.in"
               "${WrapITK_BINARY_DIR}/InstallOnly/FindWrapITK.cmake"
               @ONLY IMMEDIATE)


install_files("${WRAP_ITK_INSTALL_PREFIX}/Configuration" FILES
 ConfigureWrapping.cmake
 CMakeUtilityFunctions.cmake
 CreateCableSwigInputs.cmake
 CreateLanguageSupport.cmake
 CreateWrapperLibrary.cmake
 WrapBasicTypes.cmake
 WrapITKTypes.cmake)

install_files("${WRAP_ITK_INSTALL_PREFIX}/Configuration/Inputs" FILES
 ConfigurationInputs/wrap_ITKLang.cxx.in
 ConfigurationInputs/wrap_ITK.cxx.in
 ConfigurationInputs/wrap_.cxx.in
 ConfigurationInputs/Master.mdx.in
 ConfigurationInputs/gcc_xml.inc.in
 ConfigurationInputs/empty.depend.in
 ConfigurationInputs/typemaps.swg.in)

install_files("${WRAP_ITK_INSTALL_PREFIX}/Configuration/Inputs/LanguageSupport" FILES
  ConfigurationInputs/LanguageSupport/ModuleLoader.py.in
  ConfigurationInputs/LanguageSupport/ModuleConfig.py.in
  ConfigurationInputs/LanguageSupport/ExternalProjectConfig.py.in)

install_files("${WRAP_ITK_INSTALL_PREFIX}/SWIG" FILES "itk.swg")

install_files("${WRAP_ITK_INSTALL_PREFIX}/" FILES "${WrapITK_BINARY_DIR}/InstallOnly/WrapITKConfig.cmake")

# Add a custom target to drive installation of the WrapITK-compatibility files
if(INSTALL_WRAP_ITK_COMPATIBILITY)
  # if we should by default install the files, make a custom target that will
  # always be built.
  CREATE_INSTALL_AT_ABSOLUTE_PATH_TARGET(install_wrapitk_compatibility DEFAULT
    "Installing WrapITK compatibility files. Requires admin priviliges: turn off INSTALL_WRAP_ITK_COMPATIBILITY in cmake to disable.")
else(INSTALL_WRAP_ITK_COMPATIBILITY)
  # otherwise, add a target that will install the files only when that target is
  # manually built.
  CREATE_INSTALL_AT_ABSOLUTE_PATH_TARGET(install_wrapitk_compatibility ON_DEMAND
    "Installing WrapITK compatibility files. Requires admin priviliges.")
endif(INSTALL_WRAP_ITK_COMPATIBILITY)

INSTALL_AT_ABSOLUTE_PATH(install_wrapitk_compatibility "${CMAKE_ROOT}/Modules" "${WrapITK_BINARY_DIR}/InstallOnly/FindWrapITK.cmake")



install_files("${WRAP_ITK_INSTALL_PREFIX}/Configuration/Python" FILES "${WRAP_ITK_CMAKE_DIR}/Python/CreateLanguageSupport.cmake")
install_files("${WRAP_ITK_INSTALL_PREFIX}/Configuration/Java" FILES "${WRAP_ITK_CMAKE_DIR}/Java/CreateLanguageSupport.cmake")
install_files("${WRAP_ITK_INSTALL_PREFIX}/Configuration/Tcl" FILES "${WRAP_ITK_CMAKE_DIR}/Tcl/CreateLanguageSupport.cmake")

install_files("${WRAP_ITK_INSTALL_PREFIX}/Configuration/Python" FILES "${WRAP_ITK_CMAKE_DIR}/Python/ConfigureWrapping.cmake")
install_files("${WRAP_ITK_INSTALL_PREFIX}/Configuration/Java" FILES "${WRAP_ITK_CMAKE_DIR}/Java/ConfigureWrapping.cmake")
install_files("${WRAP_ITK_INSTALL_PREFIX}/Configuration/Tcl" FILES "${WRAP_ITK_CMAKE_DIR}/Tcl/ConfigureWrapping.cmake")

install_files("${WRAP_ITK_INSTALL_PREFIX}/Configuration/Inputs/LanguageSupport" FILES "${WRAP_ITK_CMAKE_DIR}/ConfigurationInputs/LanguageSupport/ExternalProjectConfig.pth.in")
