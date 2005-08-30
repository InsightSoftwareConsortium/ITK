# This is the root ITK CMakeLists file.
CMAKE_MINIMUM_REQUIRED(VERSION 1.8)

PROJECT(ITK)
INCLUDE_REGULAR_EXPRESSION("^([Nn]rrd|itk|vtk|vnl|vcl|vxl|f2c|netlib|ce|itpack|DICOM|meta|png|dbh|tif|jpeg|zlib|gdcm).*$")
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

# Don't build anything unless the version of CMake is high enough.
# The matching ELSE/ENDIF should be the last lines in the file.
IF("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" GREATER 1.7)

# If CMake is the version we're expecting, don't show the
# CMAKE_BACKWARDS_COMPATIBILITY option.
IF("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" MATCHES "^1\\.8$")
  MARK_AS_ADVANCED(FORCE CMAKE_BACKWARDS_COMPATIBILITY)
ELSE("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" MATCHES "^1\\.8$")
  MARK_AS_ADVANCED(CLEAR CMAKE_BACKWARDS_COMPATIBILITY)
ENDIF("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" MATCHES "^1\\.8$")

#-----------------------------------------------------------------------------
# ITK version number.  An even minor number corresponds to releases.
SET(ITK_VERSION_MAJOR "2")
SET(ITK_VERSION_MINOR "3")
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
IF(BUILD_TESTING)
  ENABLE_TESTING()
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/InsightLogo.gif
                 ${ITK_BINARY_DIR}/Testing/HTML/TestingResults/Icons/Logo.gif
                 COPYONLY IMMEDIATE)
  SET(BUILDNAME "${BUILDNAME}" CACHE STRING "Name of build on the dashboard")
  MARK_AS_ADVANCED(BUILDNAME)
ENDIF(BUILD_TESTING)

#-----------------------------------------------------------------------------
# Output directories.
SET (LIBRARY_OUTPUT_PATH ${ITK_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all libraries.")
SET (EXECUTABLE_OUTPUT_PATH ${ITK_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all executables.")
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)
SET(ITK_LIBRARY_PATH "${LIBRARY_OUTPUT_PATH}")
SET(ITK_EXECUTABLE_PATH "${EXECUTABLE_OUTPUT_PATH}")
SET(CXX_TEST_PATH ${EXECUTABLE_OUTPUT_PATH})

SET(CMAKE_MODULE_PATH ${ITK_SOURCE_DIR}/CMake)
# Deal with FFT v3 Options
OPTION(USE_FFTW "Use the FFTW v3 library" OFF)
MARK_AS_ADVANCED(USE_FFTW)
IF( USE_FFTW )
    FIND_PACKAGE( FFTW )
ENDIF( USE_FFTW )

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

OPTION(ITK_CSWIG_TCL "Build cswig Tcl wrapper support (requires CableSwig)." OFF)
OPTION(ITK_CSWIG_PYTHON "Build cswig Python wrapper support (requires CableSwig)." OFF)
OPTION(ITK_CSWIG_JAVA "Build cswig Java wrapper support " OFF)

# Options for building some of the most common core templates explicitly only
# once.  This has only been tested on gcc compilers.
OPTION(ITK_EXPLICIT_INSTANTIATION "BETA-Try to explicitly build some of the core templates explictitly rather than over and over again." OFF)
MARK_AS_ADVANCED(ITK_EXPLICIT_INSTANTIATION)
IF( ITK_EXPLICIT_INSTANTIATION )
 ADD_DEFINITIONS(-DITK_EXPLICIT_INSTANTIATION)
ENDIF( ITK_EXPLICIT_INSTANTIATION )
# perl support does not work, contact bill hoffman at kitware
# if you are interested in perl wrapping.  It is close, but
# not there yet.
#OPTION(ITK_CSWIG_PERL "Build cswig Perl wrapper support " OFF)

#-----------------------------------------------------------------------------
# ITK build patented algorithms
OPTION(ITK_USE_PATENTED "Build algorithms that are covered with Patents. It is YOUR responsibility to obtain a license from the patent holder in order to use these methods." OFF)
MARK_AS_ADVANCED(ITK_USE_PATENTED)

#-----------------------------------------------------------------------------
# Do we need CableSwig?
SET(ITK_NEED_CableSwig 0)

IF(ITK_CSWIG_TCL)
  SET(ITK_NEED_CableSwig 1)
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
  SET(ITK_NEED_CableSwig 1)
ENDIF(ITK_CSWIG_PYTHON)

IF(ITK_CSWIG_JAVA)
  SET(ITK_NEED_CableSwig 1)
ENDIF(ITK_CSWIG_JAVA)

IF(ITK_CSWIG_PERL)
  SET(ITK_NEED_CableSwig 1)
ENDIF(ITK_CSWIG_PERL)

IF(ITK_NEED_CableSwig)
  IF(NOT BUILD_SHARED_LIBS)
    MESSAGE(FATAL_ERROR "Wrapping requires a shared build, change BUILD_SHARED_LIBS to ON")
  ENDIF(NOT BUILD_SHARED_LIBS)
ENDIF(ITK_NEED_CableSwig)

#-----------------------------------------------------------------------------
# Find CableSwig if necessary.
IF(ITK_NEED_CableSwig)
  IF(EXISTS ${ITK_SOURCE_DIR}/Utilities/CableSwig)
    SET(CMAKE_MODULE_PATH ${ITK_SOURCE_DIR}/Utilities/CableSwig/SWIG/CMake)

    # CableSwig is included in the source distribution.
    SET(ITK_BUILD_CABLESWIG 1)
    SET(CableSwig_DIR ${ITK_BINARY_DIR}/Utilities/CableSwig)
    SET(CableSwig_FOUND 1)
    SET(CableSwig_INSTALL_ROOT /lib/InsightToolkit/CSwig)
    INCLUDE(${CableSwig_DIR}/CableSwigConfig.cmake OPTIONAL) 
  ELSE(EXISTS ${ITK_SOURCE_DIR}/Utilities/CableSwig)
    # CableSwig is not included in the source distribution.  Find it.
    FIND_PACKAGE(CableSwig)
    SET(CMAKE_MODULE_PATH ${CableSwig_DIR}/SWIG/CMake)

    # Hack for CMake 1.6 compatability.
    IF(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} LESS 1.7)
      SET(CableSwig_FOUND ${CABLESWIG_FOUND})
      SET(CableSwig_DIR ${CABLESWIG_DIR})
    ENDIF(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} LESS 1.7)
  ENDIF(EXISTS ${ITK_SOURCE_DIR}/Utilities/CableSwig)
  
  IF(NOT CableSwig_FOUND)
    # We have not found CableSwig.  Complain.
    MESSAGE(FATAL_ERROR "CableSwig is required for CSwig Wrapping.")
  ENDIF(NOT CableSwig_FOUND)
ENDIF(ITK_NEED_CableSwig)

#-----------------------------------------------------------------------------


#-----------------------------------------------------------------------------
# Find wrapping language API libraries.
IF(ITK_CSWIG_TCL)
  FIND_PACKAGE(TCL)
  # Hide useless settings provided by FindTCL.
  FOREACH(entry TCL_LIBRARY_DEBUG
                TK_LIBRARY_DEBUG
                TCL_STUB_LIBRARY
                TCL_STUB_LIBRARY_DEBUG
                TK_STUB_LIBRARY
                TK_STUB_LIBRARY_DEBUG
                TK_WISH)
    SET(${entry} "${${entry}}" CACHE INTERNAL "This value is not used by ITK.")
  ENDFOREACH(entry)
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
  INCLUDE(${CMAKE_ROOT}/Modules/FindPythonLibs.cmake)
  FIND_PROGRAM(PYTHON_EXECUTABLE
    NAMES python python2.3 python2.2 python2.1 python2.0 python1.6 python1.5 
    PATHS
    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.3\\InstallPath]
    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.2\\InstallPath]
    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.1\\InstallPath]
    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.0\\InstallPath]
    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\1.6\\InstallPath]
    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\1.5\\InstallPath]
    )
  MARK_AS_ADVANCED(PYTHON_EXECUTABLE)
ENDIF(ITK_CSWIG_PYTHON)

IF(ITK_CSWIG_JAVA)
  FIND_PACKAGE(Java)
  FIND_PACKAGE(JNI)
ENDIF(ITK_CSWIG_JAVA)
IF(ITK_CSWIG_PERL)
  FIND_PACKAGE(Perl)
  FIND_PACKAGE(PerlLibs)
ENDIF(ITK_CSWIG_PERL)

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
  SET(VXL_NUMERICS_LIBRARIES itkvnl itkvnl_algo itknetlib)
ENDIF(ITK_USE_SYSTEM_VXL)

# Provide options to use system versions of third-party libraries.
ITK_THIRD_PARTY_OPTION(ZLIB zlib)
ITK_THIRD_PARTY_OPTION(PNG  png)
ITK_THIRD_PARTY_OPTION(TIFF tiff)

MARK_AS_ADVANCED(ITK_CSWIG_PYTHON)
MARK_AS_ADVANCED(ITK_CSWIG_TCL)
MARK_AS_ADVANCED(ITK_CSWIG_JAVA)
MARK_AS_ADVANCED(ITK_CSWIG_PERL)
MARK_AS_ADVANCED(CABLE_INDEX)
MARK_AS_ADVANCED(CSWIG)
MARK_AS_ADVANCED(GCCXML)

#-----------------------------------------------------------------------------
# Dispatch the build into the proper subdirectories.
SUBDIRS(Utilities Code)

# set a variable to determine if 
# the Wrapping/CSwig directory should be used
SET(ITK_CSWIG_DIR 0)
IF(ITK_CSWIG_TCL)
  SET(ITK_CSWIG_DIR 1)
ENDIF(ITK_CSWIG_TCL)
IF(ITK_CSWIG_PYTHON)
  SET(ITK_CSWIG_DIR 1)
ENDIF(ITK_CSWIG_PYTHON)
IF(ITK_CSWIG_JAVA)
  SET(ITK_CSWIG_DIR 1)
ENDIF(ITK_CSWIG_JAVA)

IF(ITK_CSWIG_DIR)
  SUBDIRS(Wrapping/CSwig)
ENDIF(ITK_CSWIG_DIR)

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

IF("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" LESS 2.0)
ELSE("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" LESS 2.0)
INCLUDE(CheckTypeSize)
ENDIF("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" LESS 2.0)

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


#-----------------------------------------------------------------------------
# Configure the default ITK_DATA_ROOT for the location of ITK Data.
FIND_PATH(ITK_DATA_ROOT ITKData.readme ${ITK_SOURCE_DIR}/Testing/Data $ENV{ITK_DATA_ROOT})
MARK_AS_ADVANCED(ITK_DATA_ROOT)


#-----------------------------------------------------------------------------
# Configure files with settings for use by the build.
CONFIGURE_FILE(${ITK_SOURCE_DIR}/itkConfigure.h.in 
               ${ITK_BINARY_DIR}/itkConfigure.h)
CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Doxygen/doxygen.config.in
               ${ITK_BINARY_DIR}/doxygen.config)
CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Doxygen/itkdoxygen.pl.in
               ${ITK_BINARY_DIR}/itkdoxygen.pl)

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
# Help other projects use ITK.

# Copy the UseITK.cmake file to the binary tree for backward compatability.
CONFIGURE_FILE(${ITK_SOURCE_DIR}/UseITK.cmake.in
               ${ITK_BINARY_DIR}/UseITK.cmake COPYONLY IMMEDIATE)

# Save the compiler settings so another project can import them.
INCLUDE(${CMAKE_ROOT}/Modules/CMakeExportBuildSettings.cmake)
CMAKE_EXPORT_BUILD_SETTINGS(${ITK_BINARY_DIR}/ITKBuildSettings.cmake)
INSTALL_FILES(/lib/InsightToolkit .cmake ITKBuildSettings)

# Save library dependencies.
EXPORT_LIBRARY_DEPENDENCIES(${ITK_BINARY_DIR}/ITKLibraryDepends.cmake)

# Create the ITKConfig.cmake file containing the ITK configuration.
INCLUDE (${ITK_SOURCE_DIR}/itkGenerateITKConfig.cmake)

# Install some files.
INSTALL_FILES(/include/InsightToolkit .h itkConfigure)
INSTALL_FILES(/lib/InsightToolkit .cmake UseITK ITKLibraryDepends)

#-----------------------------------------------------------------------------
# Add compiler flags ITK needs to work on this platform.  This must be
# done AFTER the call to CMAKE_EXPORT_BUILD_SETTINGS.

SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ITK_REQUIRED_C_FLAGS}")
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ITK_REQUIRED_CXX_FLAGS}")
SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
SET(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
SET(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")

# If the version of CMake was too old, complain and build nothing.
# These should be the last lines in this file.
ELSE("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" GREATER 1.7)
  MESSAGE(SEND_ERROR
          "This version of CMake is too old to build this version of ITK.  "
          "Please upgrade to CMake 1.8.")
ENDIF("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" GREATER 1.7)

