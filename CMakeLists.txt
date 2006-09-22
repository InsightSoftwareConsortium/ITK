# This is the root ITK CMakeLists file.
CMAKE_MINIMUM_REQUIRED(VERSION 2.4)

PROJECT(ITK)
INCLUDE_REGULAR_EXPRESSION("^([Nn]rrd|itk|vtk|vnl|vcl|vxl|v3p|f2c|netlib|ce|itpack|DICOM|meta|png|dbh|tif|jpeg|zlib|gdcm).*$")
SOURCE_GROUP("XML Files" REGULAR_EXPRESSION "[.]xml$")


# On Visual Studio 8 MS deprecated C. This removes all 1.276E1265 security
# warnings
IF(WIN32)
  IF(NOT BORLAND)
    IF(NOT CYGWIN)
      IF(NOT MINGW)
        IF(NOT ITK_ENABLE_VISUAL_STUDIO_DEPRECATED_C_WARNINGS)
          ADD_DEFINITIONS(
            -D_CRT_FAR_MAPPINGS_NO_DEPRECATE
            -D_CRT_IS_WCTYPE_NO_DEPRECATE
            -D_CRT_MANAGED_FP_NO_DEPRECATE
            -D_CRT_NONSTDC_NO_DEPRECATE
            -D_CRT_SECURE_NO_DEPRECATE
            -D_CRT_SECURE_NO_DEPRECATE_GLOBALS
            -D_CRT_SETERRORMODE_BEEP_SLEEP_NO_DEPRECATE
            -D_CRT_TIME_FUNCTIONS_NO_DEPRECATE
            -D_CRT_VCCLRIT_NO_DEPRECATE
            -D_SCL_SECURE_NO_DEPRECATE
            )
        ENDIF(NOT ITK_ENABLE_VISUAL_STUDIO_DEPRECATED_C_WARNINGS)
      ENDIF(NOT MINGW)
    ENDIF(NOT CYGWIN)
  ENDIF(NOT BORLAND)
ENDIF(WIN32)


# If CMake is the version we're expecting, don't show the
# CMAKE_BACKWARDS_COMPATIBILITY option. If the version is higher than the
# minimal version required, then show the backward compatibility option.
IF("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" MATCHES "^2\\.4$")
  MARK_AS_ADVANCED(FORCE CMAKE_BACKWARDS_COMPATIBILITY)
ELSE("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" MATCHES "^2\\.4$")
  MARK_AS_ADVANCED(CLEAR CMAKE_BACKWARDS_COMPATIBILITY)
ENDIF("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" MATCHES "^2\\.4$")

#-----------------------------------------------------------------------------
# ITK version number.  An even minor number corresponds to releases.
SET(ITK_VERSION_MAJOR "2")
SET(ITK_VERSION_MINOR "9")
SET(ITK_VERSION_PATCH "0")

# Version string should not include patch level.  The major.minor is
# enough to distinguish available features of the toolkit.
SET(ITK_VERSION_STRING "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")

#-----------------------------------------------------------------------------
# Load some macros.
INCLUDE(${ITK_SOURCE_DIR}/Utilities/itkThirdParty.cmake)

#-----------------------------------------------------------------------------
# Test for some required system information.
INCLUDE (${CMAKE_ROOT}/Modules/CMakeBackwardCompatibilityC.cmake)
INCLUDE (${CMAKE_ROOT}/Modules/CMakeBackwardCompatibilityCXX.cmake)

#-----------------------------------------------------------------------------
# Configure Dart testing support.  This should be done before any
# MESSAGE(FATAL_ERROR ...) commands are invoked.
INCLUDE(${CMAKE_ROOT}/Modules/Dart.cmake)
MARK_AS_ADVANCED(TCL_TCLSH DART_ROOT)
ENABLE_TESTING()
IF(BUILD_TESTING)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/InsightLogo.gif
                 ${ITK_BINARY_DIR}/Testing/HTML/TestingResults/Icons/Logo.gif
                 COPYONLY IMMEDIATE)
  SET(BUILDNAME "${BUILDNAME}" CACHE STRING "Name of build on the dashboard")
  MARK_AS_ADVANCED(BUILDNAME)
ENDIF(BUILD_TESTING)

#-----------------------------------------------------------------------------
# Output directories.
IF(NOT LIBRARY_OUTPUT_PATH)
  SET (LIBRARY_OUTPUT_PATH ${ITK_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all libraries.")
ENDIF(NOT LIBRARY_OUTPUT_PATH)
IF(NOT EXECUTABLE_OUTPUT_PATH)
  SET (EXECUTABLE_OUTPUT_PATH ${ITK_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all executables.")
ENDIF(NOT EXECUTABLE_OUTPUT_PATH)
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)
SET(ITK_LIBRARY_PATH "${LIBRARY_OUTPUT_PATH}")
SET(ITK_EXECUTABLE_PATH "${EXECUTABLE_OUTPUT_PATH}")
SET(CXX_TEST_PATH ${EXECUTABLE_OUTPUT_PATH})

SET(CMAKE_MODULE_PATH ${ITK_SOURCE_DIR}/CMake)
# Deal with FFT v3 Options
MARK_AS_ADVANCED(USE_FFTWD)
OPTION(USE_FFTWD "Use double precision FFTW if found" OFF)
MARK_AS_ADVANCED(USE_FFTWF)
OPTION(USE_FFTWF "Use single precision FFTW if found" OFF)
IF( USE_FFTWD OR USE_FFTWF )
    FIND_PACKAGE( FFTW )
ENDIF( USE_FFTWD OR USE_FFTWF )

# Deal with SGI SCSL Options
OPTION(USE_SCSL "Use the SGI SCSL High Performance Computing math library" OFF)
MARK_AS_ADVANCED(USE_SCSL)
IF( USE_SCSL )
    FIND_PACKAGE( SCSL )
ENDIF( USE_SCSL )

# Put "." in the rpath of all ITK shared libraries.
LINK_DIRECTORIES(.)

#-----------------------------------------------------------------------------
# ITK build configuration options.
OPTION(BUILD_SHARED_LIBS "Build ITK with shared libraries." OFF)
SET(ITK_BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS})

IF(NOT ITK_NO_LIBRARY_VERSION)
  # This setting of SOVERSION assumes that any API change
  # will increment either the minor or major version number of ITK.
  SET(ITK_LIBRARY_PROPERTIES 
      VERSION   "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}.${ITK_VERSION_PATCH}"
      SOVERSION "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}"
     )
ENDIF(NOT ITK_NO_LIBRARY_VERSION)

#-----------------------------------------------------------------------------
# ITK build patented algorithms
OPTION(ITK_USE_PATENTED "Build algorithms that are covered with Patents. It is YOUR responsibility to obtain a license from the patent holder in order to use these methods." OFF)
MARK_AS_ADVANCED(ITK_USE_PATENTED)

#-----------------------------------------------------------------------------
# ITK build classes that are in the review process
OPTION(ITK_USE_REVIEW "Build algorithms that are candidates to be included in the Toolkit. This classes are not covered by the Backward Compatibility Policy, nor the Copyright Policy of ITK." OFF)
MARK_AS_ADVANCED(ITK_USE_REVIEW)

#-----------------------------------------------------------------------------
# ITK uses KWStyle for checking the coding style
OPTION(ITK_USE_KWSTYLE "Run KWStyle in order to check for violations of the coding standard." OFF)
MARK_AS_ADVANCED(ITK_USE_KWSTYLE)

IF(ITK_USE_KWSTYLE)
  FIND_PROGRAM(KWSTYLE_EXECUTABLE
  NAMES KWStyle 
  PATHS
  /usr/local/bin
  )
  MARK_AS_ADVANCED(KWSTYLE_EXECUTABLE)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${ITK_BINARY_DIR}/KWStyleReport.txt
    COMMAND ${KWSTYLE_EXECUTABLE}
    ARGS    -xml ${ITK_BINARY_DIR}/ITK.kws.xml -o ${ITK_SOURCE_DIR}/Utilities/KWStyle/ITKOverwrite.txt -vim -D ${ITK_BINARY_DIR}/ITKFiles.txt  
    COMMENT "Coding Style Checker"
    )
  ADD_CUSTOM_TARGET(StyleCheck DEPENDS ${ITK_BINARY_DIR}/KWStyleReport.txt)
ENDIF(ITK_USE_KWSTYLE)

#-----------------------------------------------------------------------------
# ITK turn on concept checking
IF( NOT BORLAND )
  OPTION(ITK_USE_CONCEPT_CHECKING "Turn on concept checking to give helpful errors at compile time if a type cannot be used as a template parameter." OFF)
  MARK_AS_ADVANCED(ITK_USE_CONCEPT_CHECKING)
ELSE( NOT BORLAND )
  SET(ITK_USE_CONCEPT_CHECKING OFF)
  MARK_AS_ADVANCED(ITK_USE_CONCEPT_CHECKING)
ENDIF( NOT BORLAND )

#-----------------------------------------------------------------------------
# Installation vars.
# ITK_INSTALL_BIN_DIR          - binary dir (executables)
# ITK_INSTALL_LIB_DIR          - library dir (libs)
# ITK_INSTALL_DATA_DIR         - share dir (say, examples, data, etc)
# ITK_INSTALL_INCLUDE_DIR      - include dir (headers)
# ITK_INSTALL_PACKAGE_DIR      - package/export configuration files
# ITK_INSTALL_NO_DEVELOPMENT   - do not install development files
# ITK_INSTALL_NO_RUNTIME       - do not install runtime files
# ITK_INSTALL_NO_DOCUMENTATION - do not install documentation files

IF(NOT ITK_INSTALL_BIN_DIR)
  SET(ITK_INSTALL_BIN_DIR "/bin")
ENDIF(NOT ITK_INSTALL_BIN_DIR)

IF(NOT ITK_INSTALL_LIB_DIR)
  SET(ITK_INSTALL_LIB_DIR "/lib/InsightToolkit")
ENDIF(NOT ITK_INSTALL_LIB_DIR)

IF(NOT ITK_INSTALL_DATA_DIR)
  SET(ITK_INSTALL_DATA_DIR "/share/InsightToolkit")
ENDIF(NOT ITK_INSTALL_DATA_DIR)

IF(NOT ITK_INSTALL_INCLUDE_DIR)
  SET(ITK_INSTALL_INCLUDE_DIR "/include/InsightToolkit")
ENDIF(NOT ITK_INSTALL_INCLUDE_DIR)

IF(NOT ITK_INSTALL_PACKAGE_DIR)
  SET(ITK_INSTALL_PACKAGE_DIR ${ITK_INSTALL_LIB_DIR}
    CACHE INTERNAL "")
ENDIF(NOT ITK_INSTALL_PACKAGE_DIR)

IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  SET(ITK_INSTALL_NO_DEVELOPMENT 0)
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)

IF(NOT ITK_INSTALL_NO_RUNTIME)
  SET(ITK_INSTALL_NO_RUNTIME 0)
ENDIF(NOT ITK_INSTALL_NO_RUNTIME)

IF(NOT ITK_INSTALL_NO_DOCUMENTATION)
  SET(ITK_INSTALL_NO_DOCUMENTATION 0)
ENDIF(NOT ITK_INSTALL_NO_DOCUMENTATION)

SET(ITK_INSTALL_NO_LIBRARIES)
IF(ITK_BUILD_SHARED_LIBS)
  IF(ITK_INSTALL_NO_RUNTIME AND ITK_INSTALL_NO_DEVELOPMENT)
    SET(ITK_INSTALL_NO_LIBRARIES 1)
  ENDIF(ITK_INSTALL_NO_RUNTIME AND ITK_INSTALL_NO_DEVELOPMENT)
ELSE(ITK_BUILD_SHARED_LIBS)
  IF(ITK_INSTALL_NO_DEVELOPMENT)
    SET(ITK_INSTALL_NO_LIBRARIES 1)
  ENDIF(ITK_INSTALL_NO_DEVELOPMENT)
ENDIF(ITK_BUILD_SHARED_LIBS)

# set ITK_DIR so it can be used by subprojects
SET(ITK_DIR "${CMAKE_BINARY_DIR}" CACHE INTERNAL "ITK dir to be used by subprojects")


#-----------------------------------------------------------------------------
# Find CableSwig in the source dir
IF(EXISTS ${ITK_SOURCE_DIR}/Utilities/CableSwig)
  SET(CMAKE_MODULE_PATH ${ITK_SOURCE_DIR}/Utilities/CableSwig/SWIG/CMake)

  # CableSwig is included in the source distribution.
  SET(ITK_BUILD_CABLESWIG 1)
  SET(CableSwig_DIR ${ITK_BINARY_DIR}/Utilities/CableSwig CACHE PATH "CableSwig_DIR: The directory containing CableSwigConfig.cmake.")
  SET(CableSwig_FOUND 1)
  SET(CableSwig_INSTALL_ROOT ${ITK_INSTALL_LIB_DIR}/CSwig)
  INCLUDE(${CableSwig_DIR}/CableSwigConfig.cmake OPTIONAL) 
ENDIF(EXISTS ${ITK_SOURCE_DIR}/Utilities/CableSwig)



IF(WIN32)
   IF(BUILD_SHARED_LIBS)
     SET(ITK_COMMON_BUILD_TYPE "SHARED")
   ELSE(BUILD_SHARED_LIBS)
     SET(ITK_COMMON_BUILD_TYPE "STATIC")
   ENDIF(BUILD_SHARED_LIBS)
   SET(BUILD_SHARED_LIBS OFF)
ENDIF(WIN32)

OPTION(BUILD_EXAMPLES "Build the Examples directory." ON)

OPTION(ITK_USE_SYSTEM_VXL "Use an outside build of VXL." OFF)
MARK_AS_ADVANCED(ITK_USE_SYSTEM_VXL)
IF(ITK_USE_SYSTEM_VXL)
  FIND_PACKAGE(VXL)
  IF(VXL_FOUND)
    INCLUDE(${VXL_CMAKE_DIR}/UseVXL.cmake)
    SET(VXL_NUMERICS_LIBRARIES vnl vnl_algo netlib vcl)
  ELSE(VXL_FOUND)
    MESSAGE(FATAL_ERROR "Must set VXL_DIR for ITK_USE_SYSTEM_VXL.")
  ENDIF(VXL_FOUND)
ELSE(ITK_USE_SYSTEM_VXL)
  SET(VXL_NETLIB_INCLUDE_DIR ${ITK_SOURCE_DIR}/Utilities/vxl/v3p/netlib)
  SET(VXL_NUMERICS_LIBRARIES itkvnl itkvnl_algo itknetlib itkvcl)
ENDIF(ITK_USE_SYSTEM_VXL)


# Provide options to use system versions of third-party libraries.
ITK_THIRD_PARTY_OPTION(ZLIB zlib)
ITK_THIRD_PARTY_OPTION(PNG  png)
ITK_THIRD_PARTY_OPTION(TIFF tiff)

#-----------------------------------------------------------------------------
# Dispatch the build into the proper subdirectories.
SUBDIRS(Utilities Code)

SUBDIRS(Wrapping)

IF (BUILD_TESTING)
  SUBDIRS(Testing)
ENDIF (BUILD_TESTING)

IF(BUILD_EXAMPLES)
  SUBDIRS(Examples)
ENDIF(BUILD_EXAMPLES)

#-----------------------------------------------------------------------------
# ITK requires special compiler flags on some platforms.
IF(CMAKE_COMPILER_IS_GNUCXX)
  SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -ftemplate-depth-50")
  IF(APPLE)
    SET(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -no-cpp-precomp")
    SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -no-cpp-precomp")
    SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -Wno-long-double")
  ENDIF(APPLE)
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

IF(UNIX)
  IF(NOT CMAKE_COMPILER_IS_GNUCXX)
    INCLUDE(${ITK_SOURCE_DIR}/CMake/TestNO_ICC_IDYNAMIC_NEEDED.cmake)
    TESTNO_ICC_IDYNAMIC_NEEDED(NO_ICC_IDYNAMIC_NEEDED ${ITK_SOURCE_DIR}/CMake )
    IF(NO_ICC_IDYNAMIC_NEEDED)
      SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS}")
    ELSE(NO_ICC_IDYNAMIC_NEEDED)
      SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -i_dynamic")
    ENDIF(NO_ICC_IDYNAMIC_NEEDED)
  ENDIF(NOT CMAKE_COMPILER_IS_GNUCXX)
ENDIF(UNIX)

IF(BORLAND)
    SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -w- -whid -waus -wpar")
    SET(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -w- -whid -waus -wpar")
ENDIF(BORLAND)

# Force the use of ansi cxx flags (i.e. -LANG:std on sgi )
IF("x${CMAKE_ANSI_CXXFLAGS}" MATCHES "^x.*[^ ]")
  SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} ${CMAKE_ANSI_CXXFLAGS}")
ENDIF("x${CMAKE_ANSI_CXXFLAGS}" MATCHES "^x.*[^ ]")
IF("x${CMAKE_ANSI_CFLAGS}" MATCHES "^x.*[^ ]")
  SET(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} ${CMAKE_ANSI_CFLAGS}")
ENDIF("x${CMAKE_ANSI_CFLAGS}" MATCHES "^x.*[^ ]")

IF(CMAKE_SYSTEM MATCHES "IRIX.*")
  IF(NOT CMAKE_COMPILER_IS_GNUCXX)
    SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -woff 15,84,1110,1209,1424,1468,3102,3210,3439  -no_auto_include -ptused -no_prelink")
    SET(ITK_REQUIRED_LINK_FLAGS "${ITK_REQUIRED_LINK_FLAGS}  -Wl,-woff15")
  ENDIF(NOT CMAKE_COMPILER_IS_GNUCXX)
ENDIF(CMAKE_SYSTEM MATCHES "IRIX.*")

IF(MINGW)
  LINK_LIBRARIES(gdi32)
ENDIF(MINGW)

# for the gnu compiler a -D_PTHREADS is needed on sun
# for the native compiler a -mt flag is needed on the sun
IF(CMAKE_USE_PTHREADS)
  IF(CMAKE_SYSTEM MATCHES "SunOS.*")
    IF(CMAKE_COMPILER_IS_GNUCXX)
      SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -D_PTHREADS")
    ELSE(CMAKE_COMPILER_IS_GNUCXX)
      SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -mt")
      SET(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -mt")
    ENDIF(CMAKE_COMPILER_IS_GNUCXX)
  ENDIF(CMAKE_SYSTEM MATCHES "SunOS.*")
ENDIF(CMAKE_USE_PTHREADS)

IF(WIN32)
  SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -DNOMINMAX")
ENDIF(WIN32)

# mingw thread support
IF(MINGW)
  SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -mthreads")
  SET(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -mthreads")
  SET(ITK_REQUIRED_LINK_FLAGS "${ITK_REQUIRED_LINK_FLAGS} -mthreads")
ENDIF(MINGW)

#-----------------------------------------------------------------------------
# Find platform-specific differences in the handling of IEEE floating point 
# special values.
INCLUDE(${ITK_SOURCE_DIR}/CMake/CheckBigBitfield.cmake)
CHECK_BIG_BITFIELD(BIGBITFIELD_VALUE ${ITK_SOURCE_DIR}/CMake)
IF(BIGBITFIELD_VALUE)
   SET(BIGBITFIELD 1 CACHE INTERNAL "System handles bit-fields larger than 32 bits.")
ELSE(BIGBITFIELD_VALUE)
   SET(BIGBITFIELD 0 CACHE INTERNAL "System handles bit-fields larger than 32 bits.")
ENDIF(BIGBITFIELD_VALUE)

INCLUDE(${ITK_SOURCE_DIR}/CMake/TestQnanhibit.cmake)
TEST_QNANHIBIT(QNANHIBIT_VALUE ${ITK_SOURCE_DIR}/CMake)
IF(QNANHIBIT_VALUE)
   SET(QNANHIBIT 1 CACHE INTERNAL "The 22nd bit of 32-bit floating-point quiet NaN.")
ELSE(QNANHIBIT_VALUE)
   SET(QNANHIBIT 0 CACHE INTERNAL "The 22nd bit of 32-bit floating-point quiet NaN.")
ENDIF(QNANHIBIT_VALUE)

INCLUDE(CheckTypeSize)

CHECK_TYPE_SIZE("long long" ITK_SIZEOF_LONG_LONG)
CHECK_TYPE_SIZE("__int64"   ITK_SIZEOF___INT64)

IF(ITK_SIZEOF___INT64)
  IF("ITK_TYPE_SAME_LONG_AND___INT64" MATCHES "^ITK_TYPE_SAME_LONG_AND___INT64$")
    MESSAGE(STATUS "Checking whether long and __int64 are the same type")
    TRY_COMPILE(ITK_TYPE_SAME_LONG_AND___INT64
      ${ITK_BINARY_DIR}/CMakeTmp
      ${ITK_SOURCE_DIR}/CMake/itkTestCompareTypes.cxx
      COMPILE_DEFINITIONS
      -DITK_TEST_COMPARE_TYPE_1=long
      -DITK_TEST_COMPARE_TYPE_2=__int64
      OUTPUT_VARIABLE OUTPUT)
    IF(ITK_TYPE_SAME_LONG_AND___INT64)
      MESSAGE(STATUS "Checking whether long and __int64 are the same type -- yes")
      SET(ITK_TYPE_SAME_LONG_AND___INT64 1 CACHE INTERNAL "Whether long and __int64 are the same type")
    ELSE(ITK_TYPE_SAME_LONG_AND___INT64)
      MESSAGE(STATUS "Checking whether long and __int64 are the same type -- no")
      SET(ITK_TYPE_SAME_LONG_AND___INT64 0 CACHE INTERNAL "Whether long and __int64 are the same type")
      WRITE_FILE(${CMAKE_BINARY_DIR}/CMakeError.log
        "Determining whether long and __int64 are the same type "
        "failed with the following output:\n"
        "${OUTPUT}\n" APPEND)
    ENDIF(ITK_TYPE_SAME_LONG_AND___INT64)
  ENDIF("ITK_TYPE_SAME_LONG_AND___INT64" MATCHES "^ITK_TYPE_SAME_LONG_AND___INT64$")
ENDIF(ITK_SIZEOF___INT64)

IF(ITK_SIZEOF_LONG_LONG)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/CMake/itkTestStreamLongLong.cxx.in
    ${ITK_BINARY_DIR}/CMake/itkTestStreamLongLong.cxx @ONLY IMMEDIATE)
  IF("ITK_OSTREAM_SUPPORTS_LONG_LONG" MATCHES "^ITK_OSTREAM_SUPPORTS_LONG_LONG$")
    MESSAGE(STATUS "Checking if ostream supports long long")
    TRY_COMPILE(ITK_OSTREAM_SUPPORTS_LONG_LONG
      ${ITK_BINARY_DIR}
      ${ITK_BINARY_DIR}/CMake/itkTestStreamLongLong.cxx
      COMPILE_DEFINITIONS -DITK_TEST_OSTREAM_LONG_LONG
      OUTPUT_VARIABLE OUTPUT)
    IF(ITK_OSTREAM_SUPPORTS_LONG_LONG)
      MESSAGE(STATUS "Checking if ostream supports long long -- yes")
      SET(ITK_OSTREAM_SUPPORTS_LONG_LONG 1 CACHE INTERNAL "Whether ostream supports long long")
    ELSE(ITK_OSTREAM_SUPPORTS_LONG_LONG)
      MESSAGE(STATUS "Checking if ostream supports long long -- no")
      SET(ITK_OSTREAM_SUPPORTS_LONG_LONG 0 CACHE INTERNAL "Whether ostream supports long long")
      WRITE_FILE(${CMAKE_BINARY_DIR}/CMakeError.log
        "Determining if ostream supports long long "
        "failed with the following output:\n"
        "${OUTPUT}\n" APPEND)
    ENDIF(ITK_OSTREAM_SUPPORTS_LONG_LONG)
  ENDIF("ITK_OSTREAM_SUPPORTS_LONG_LONG" MATCHES "^ITK_OSTREAM_SUPPORTS_LONG_LONG$")
  IF("ITK_ISTREAM_SUPPORTS_LONG_LONG" MATCHES "^ITK_ISTREAM_SUPPORTS_LONG_LONG$")
    MESSAGE(STATUS "Checking if istream supports long long")
    TRY_COMPILE(ITK_ISTREAM_SUPPORTS_LONG_LONG
      ${ITK_BINARY_DIR}
      ${ITK_BINARY_DIR}/CMake/itkTestStreamLongLong.cxx
      COMPILE_DEFINITIONS -DITK_TEST_ISTREAM_LONG_LONG
      OUTPUT_VARIABLE OUTPUT)
    IF(ITK_ISTREAM_SUPPORTS_LONG_LONG)
      MESSAGE(STATUS "Checking if istream supports long long -- yes")
      SET(ITK_ISTREAM_SUPPORTS_LONG_LONG 1 CACHE INTERNAL "Whether istream supports long long")
    ELSE(ITK_ISTREAM_SUPPORTS_LONG_LONG)
      MESSAGE(STATUS "Checking if istream supports long long -- no")
      SET(ITK_ISTREAM_SUPPORTS_LONG_LONG 0 CACHE INTERNAL "Whether istream supports long long")
      WRITE_FILE(${CMAKE_BINARY_DIR}/CMakeError.log
        "Determining if istream supports long long "
        "failed with the following output:\n"
        "${OUTPUT}\n" APPEND)
    ENDIF(ITK_ISTREAM_SUPPORTS_LONG_LONG)
  ENDIF("ITK_ISTREAM_SUPPORTS_LONG_LONG" MATCHES "^ITK_ISTREAM_SUPPORTS_LONG_LONG$")
ENDIF(ITK_SIZEOF_LONG_LONG)

#
# See if compiler preprocessor has the __FUNCTION__ directive used by itkExceptionMacro
#
INCLUDE(${ITK_SOURCE_DIR}/CMake/CheckCPPDirective.cmake)
CHECK_CPP_DIRECTIVE_EXISTS(__FUNCTION__ ITK_CPP_FUNCTION)

#-----------------------------------------------------------------------------
# Configure the default ITK_DATA_ROOT for the location of ITK Data.
FIND_PATH(ITK_DATA_ROOT ITKData.readme ${ITK_SOURCE_DIR}/Testing/Data $ENV{ITK_DATA_ROOT})
MARK_AS_ADVANCED(ITK_DATA_ROOT)


#-----------------------------------------------------------------------------
# Configure files with settings for use by the build.
CONFIGURE_FILE(${ITK_SOURCE_DIR}/itkConfigure.h.in 
  ${ITK_BINARY_DIR}/itkConfigure.h)
IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  INSTALL_FILES(${ITK_INSTALL_INCLUDE_DIR} FILES 
    ${ITK_BINARY_DIR}/itkConfigure.h)
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)

CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Doxygen/doxygen.config.in
               ${ITK_BINARY_DIR}/doxygen.config)

CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Doxygen/itkdoxygen.pl.in
               ${ITK_BINARY_DIR}/itkdoxygen.pl)

CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/KWStyle/ITK.kws.xml.in
               ${ITK_BINARY_DIR}/ITK.kws.xml)

CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/KWStyle/ITKFiles.txt.in
               ${ITK_BINARY_DIR}/ITKFiles.txt)

#-----------------------------------------------------------------------------
# The entire ITK tree should use the same include path, except for the
# Wrapping directory.

# Create the list of include directories needed for ITK header files.
INCLUDE(${ITK_SOURCE_DIR}/itkIncludeDirectories.cmake)

# This should be the only INCLUDE_DIRECTORIES command in the entire
# tree, except for the Utilities and Wrapping directories.  We need to
# do this in one place to make sure the order is correct.
INCLUDE_DIRECTORIES(
  ${ITK_INCLUDE_DIRS_BUILD_TREE}
  ${ITK_INCLUDE_DIRS_BUILD_TREE_CXX}
  ${ITK_INCLUDE_DIRS_SYSTEM}
)

#-----------------------------------------------------------------------------
# ITK_USE_SYSTEM_GDCM is provided so that you can use an installed or external
# version of gdcm. If its on, you must specify the variables 
# Important: INCLUDE(${GDCM_USE_FILE}) *has* to be set after the
# INCLUDE_DIRECTORIES that sets all the include paths for ITK, otherwise
# if GDCM was build with VTK support, including it will bring the VTK
# include paths as well, and the wrong DICOMParser will be picked.

OPTION(ITK_USE_SYSTEM_GDCM "Use an outside build of GDCM." OFF)
MARK_AS_ADVANCED(ITK_USE_SYSTEM_GDCM)
IF(ITK_USE_SYSTEM_GDCM)
  FIND_PACKAGE(GDCM)
  IF(GDCM_FOUND)
    INCLUDE(${GDCM_USE_FILE})
    SET(ITK_GDCM_LIBRARIES gdcm)
  ELSE(GDCM_FOUND)
    MESSAGE(FATAL_ERROR "Must set GDCM_DIR for ITK_USE_SYSTEM_GDCM.")
  ENDIF(GDCM_FOUND)
ELSE(ITK_USE_SYSTEM_GDCM)
  SET(ITK_GDCM_LIBRARIES itkgdcm)
  SET(GDCM_INCLUDE_DIR
    ${ITK_SOURCE_DIR}/Utilities/gdcm/src
    )
ENDIF(ITK_USE_SYSTEM_GDCM)

#-----------------------------------------------------------------------------
# Help other projects use ITK.

# Copy the UseITK.cmake file to the binary tree for backward compatability.
CONFIGURE_FILE(${ITK_SOURCE_DIR}/UseITK.cmake.in
               ${ITK_BINARY_DIR}/UseITK.cmake COPYONLY IMMEDIATE)

# Save the compiler settings so another project can import them.
INCLUDE(${CMAKE_ROOT}/Modules/CMakeExportBuildSettings.cmake)
CMAKE_EXPORT_BUILD_SETTINGS(${ITK_BINARY_DIR}/ITKBuildSettings.cmake)

# Save library dependencies.
EXPORT_LIBRARY_DEPENDENCIES(${ITK_BINARY_DIR}/ITKLibraryDepends.cmake)

# Create the ITKConfig.cmake file containing the ITK configuration.
INCLUDE (${ITK_SOURCE_DIR}/itkGenerateITKConfig.cmake)

IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  INSTALL_FILES(${ITK_INSTALL_PACKAGE_DIR} FILES
    ${ITK_BINARY_DIR}/ITKBuildSettings.cmake
    ${ITK_BINARY_DIR}/ITKLibraryDepends.cmake
    ${ITK_BINARY_DIR}/UseITK.cmake
    )
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)

#-----------------------------------------------------------------------------
# Add compiler flags ITK needs to work on this platform.  This must be
# done AFTER the call to CMAKE_EXPORT_BUILD_SETTINGS.

SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ITK_REQUIRED_C_FLAGS}")
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ITK_REQUIRED_CXX_FLAGS}")
SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
SET(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
SET(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
#-----------------------------------------------------------------------------
# The frename-registers option does not work due to a bug in the gnu compiler.
# It must be removed or data errors will be produced and incorrect results
# will be produced.  This is first documented in the gcc4 man page.
IF(CMAKE_COMPILER_IS_GNUCXX)
  SET(ALL_FLAGS "${CMAKE_C_FLAGS} ${CMAKE_CXX_FLAGS} ${CMAKE_EXE_LINKER_FLAGS} ${CMAKE_SHARED_LINKER_FLAGS} ${CMAKE_MODULE_LINKER_FLAGS}" )
  SEPARATE_ARGUMENTS(ALL_FLAGS)
  FOREACH(COMP_OPTION ${ALL_FLAGS})
    #  MESSAGE("${COMP_OPTION} being checked.")
    IF("${COMP_OPTION}" STREQUAL "-frename-registers")
      MESSAGE(FATAL_ERROR "-frename-registers causes runtime bugs.  It must be removed from your compilation options.")
    ENDIF("${COMP_OPTION}" STREQUAL "-frename-registers")
    IF("${COMP_OPTION}" STREQUAL "-ffloat-store")
      MESSAGE(FATAL_ERROR "-ffloat-store causes runtime bugs on gcc 3.2.3 (appearently not on gcc 3.4.3, but the exact criteria is not known).  It must be removed from your compilation options.")
    ENDIF("${COMP_OPTION}" STREQUAL "-ffloat-store")
  ENDFOREACH(COMP_OPTION)
ENDIF(CMAKE_COMPILER_IS_GNUCXX)


