# This is the root ITK CMakeLists file.
CMAKE_MINIMUM_REQUIRED(VERSION 2.4)
IF(COMMAND CMAKE_POLICY)
  CMAKE_POLICY(SET CMP0003 NEW)
  CMAKE_POLICY(SET CMP0054 OLD)
ENDIF(COMMAND CMAKE_POLICY)

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

# Visual Studio 2012 and later forbid macro-izing keywords (C4005 & C1189)
IF (MSVC)
  IF(NOT (MSVC_VERSION LESS 1700))
    ADD_DEFINITIONS(-D_ALLOW_KEYWORD_MACROS)
  ENDIF(NOT (MSVC_VERSION LESS 1700))
ENDIF(MSVC)

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
SET(ITK_VERSION_MAJOR "3")
SET(ITK_VERSION_MINOR "20")
SET(ITK_VERSION_PATCH "1")

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
FIND_PACKAGE(Tclsh)
MARK_AS_ADVANCED(TCL_TCLSH DART_ROOT)
ENABLE_TESTING()
IF(BUILD_TESTING)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/CMake/RemoveTemporaryFiles.cmake.in
    ${ITK_BINARY_DIR}/CMake/RemoveTemporaryFiles.cmake @ONLY IMMEDIATE)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/CMake/itkSampleBuildTest.cmake.in
    ${ITK_BINARY_DIR}/CMake/itkSampleBuildTest.cmake @ONLY)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/CMake/CTestCustom.ctest.in
    ${ITK_BINARY_DIR}/CMake/CTestCustom.ctest @ONLY)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/CMake/CTestCustom.ctest.in
    ${ITK_BINARY_DIR}/Testing/Code/Common/CTestCustom.ctest @ONLY)
  FILE(WRITE ${ITK_BINARY_DIR}/CTestCustom.cmake
    "INCLUDE(\"${ITK_BINARY_DIR}/CMake/CTestCustom.ctest\")\n")

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
  SET(msg "ATTENTION: You have enabled the use of FFTW,")
  SET(msg "${msg} this library is distributed under a GPL license.")
  SET(msg "${msg} By enabling this option, the binary of the ITK libraries")
  SET(msg "${msg} that you are going to build will be covered by a GPL license,")
  SET(msg "${msg} and so it will be any executable that you link against these libraries.")
  MESSAGE("${msg}")
  FIND_PACKAGE( FFTW )
ENDIF( USE_FFTWD OR USE_FFTWF )

# Deal with SGI SCSL Options
OPTION(USE_SCSL "Use the SGI SCSL High Performance Computing math library" OFF)
MARK_AS_ADVANCED(USE_SCSL)
IF( USE_SCSL )
    FIND_PACKAGE( SCSL )
ENDIF( USE_SCSL )

# Deal with MINC2 options
MARK_AS_ADVANCED(ITK_USE_MINC2)
OPTION(ITK_USE_MINC2 "Use MINC2 File Format" OFF)
IF( ITK_USE_MINC2 )
  FIND_PACKAGE( MINC2 )
  IF( NOT MINC2_FOUND )
    MESSAGE("You requested to use MINC2 but some components are missing")
  ENDIF( NOT MINC2_FOUND )
ENDIF( ITK_USE_MINC2 )


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
# Provide compatibility options.
OPTION(ITK_LEGACY_REMOVE "Remove all legacy code completely." OFF)
OPTION(ITK_LEGACY_SILENT "Silence all legacy code messages." OFF)
MARK_AS_ADVANCED(ITK_LEGACY_REMOVE ITK_LEGACY_SILENT)

SET(GDCM_LEGACY_SILENT ${ITK_LEGACY_SILENT})

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
INCLUDE(${ITK_SOURCE_DIR}/Utilities/KWStyle/KWStyle.cmake)

#-----------------------------------------------------------------------------
# ITK turn on concept checking
IF( NOT BORLAND )
  OPTION(ITK_USE_CONCEPT_CHECKING "Turn on concept checking to give helpful errors at compile time if a type cannot be used as a template parameter." ON)
  MARK_AS_ADVANCED(ITK_USE_CONCEPT_CHECKING)
  OPTION(ITK_USE_STRICT_CONCEPT_CHECKING "Turn on Strict concept checking to give more stringent errors at compile time if a type cannot be used as a template parameter." OFF)
  MARK_AS_ADVANCED(ITK_USE_STRICT_CONCEPT_CHECKING)
ELSE( NOT BORLAND )
  SET(ITK_USE_CONCEPT_CHECKING OFF)
  MARK_AS_ADVANCED(ITK_USE_CONCEPT_CHECKING)
  SET(ITK_USE_STRICT_CONCEPT_CHECKING OFF)
  MARK_AS_ADVANCED(ITK_USE_STRICT_CONCEPT_CHECKING)
ENDIF( NOT BORLAND )

#-----------------------------------------------------------------------------
# Make the itk::Image assume the behavior of the itk::OrientedImage
MARK_AS_ADVANCED(ITK_IMAGE_BEHAVES_AS_ORIENTED_IMAGE)
OPTION(ITK_IMAGE_BEHAVES_AS_ORIENTED_IMAGE "Makes itk::Image behave like itk::OrientedImage" ON)

# ITK turn on experimental version of accelerated image registration
OPTION(ITK_USE_OPTIMIZED_REGISTRATION_METHODS "Turn on the experimental version of accelerated registration methods." OFF)
MARK_AS_ADVANCED(ITK_USE_OPTIMIZED_REGISTRATION_METHODS)
IF(ITK_USE_OPTIMIZED_REGISTRATION_METHODS)
  IF(NOT ITK_USE_REVIEW)
    MESSAGE(FATAL_ERROR "ITK_USE_REVIEW is currently OFF but it should be ON if you want to use the experimental optimized registration methods.")
  ENDIF(NOT ITK_USE_REVIEW)
  # Warn the user about the implications of turning optimized registration methods ON
  SET(msg "Attention: You have chosen to enable the use of Optimized Registration Methods.")
  SET(msg "${msg} These methods add multi-threading capabilities to several image metrics")
  SET(msg "${msg} as well as some Transforms and Interpolators. In order to take full advantage")
  SET(msg "${msg} of these new features you will have to also add some new calls to these components.")
  SET(msg "${msg} Please look at the files in the Examples/Review directory for further details.")

# display the message during the setup
MESSAGE("${msg}")


ENDIF(ITK_USE_OPTIMIZED_REGISTRATION_METHODS)

#-----------------------------------------------------------------------------
# ITK use the new statistics framework
OPTION( ITK_USE_REVIEW_STATISTICS "Use the experimental statistics framework." OFF)
MARK_AS_ADVANCED( ITK_USE_REVIEW_STATISTICS )
IF(ITK_USE_REVIEW_STATISTICS)
  IF(NOT ITK_USE_REVIEW)
    MESSAGE(FATAL_ERROR "ITK_USE_REVIEW is currently OFF but it should be ON if you want to use the experimental statistics framework.")
  ENDIF(NOT ITK_USE_REVIEW)

  # Warn the user about the implications of using the new statistics framework.
  SET(msg "Attention: You have chosen to use the new statistics framework instead of the existing framework.")
  SET(msg "${msg} The statistics classes have been re-designed/refactored and in several")
  SET(msg "${msg} cases do not conform to the API of the older framework. You may need to")
  SET(msg "${msg} change your code to use the new API.")
  MESSAGE("${msg}")
ENDIF(ITK_USE_REVIEW_STATISTICS)

#-----------------------------------------------------------------------------
# ITK turn on experimental version of accelerated image registration
OPTION(ITK_USE_CONSOLIDATED_MORPHOLOGY "Turn on the experimental consolidated morphology." OFF)
MARK_AS_ADVANCED(ITK_USE_CONSOLIDATED_MORPHOLOGY)

IF(ITK_USE_CONSOLIDATED_MORPHOLOGY)
  IF(NOT ITK_USE_REVIEW)
    MESSAGE(FATAL_ERROR "ITK_USE_REVIEW is currently OFF but it should be ON if you want to use the experimental consolidated morphology.")
  ENDIF(NOT ITK_USE_REVIEW)
  # Warn the user about the implications of turning consolidated morphology ON
  SET(msg "Attention: You have chosen to enable the use of consolidated morphology.")
  # TODO: warn more than that
  # display the message during the setup
  MESSAGE("${msg}")
ENDIF(ITK_USE_CONSOLIDATED_MORPHOLOGY)


#-----------------------------------------------------------------------------
# ITK turn on the use of experimental IO factories for the TransformFileReader and TransformFileWriter
OPTION(ITK_USE_TRANSFORM_IO_FACTORIES "Turn on the experimental IO factories for reading and writing transforms." OFF)
MARK_AS_ADVANCED(ITK_USE_TRANSFORM_IO_FACTORIES)
IF(ITK_USE_TRANSFORM_IO_FACTORIES)
  IF(NOT ITK_USE_REVIEW)
    MESSAGE(FATAL_ERROR "ITK_USE_REVIEW is currently OFF but it should be ON if you want to use the experimental transform IO factories.")
  ENDIF(NOT ITK_USE_REVIEW)
ENDIF(ITK_USE_TRANSFORM_IO_FACTORIES)

#-----------------------------------------------------------------------------
# ITK turn on the correct usage of oriented images in gradient computations and image registration.
OPTION(ITK_USE_ORIENTED_IMAGE_DIRECTION "Turn on correct use of oriented images direction in gradient computation and image registration." ON)
MARK_AS_ADVANCED(ITK_USE_ORIENTED_IMAGE_DIRECTION)


#-----------------------------------------------------------------------------
# ITK turn on the correct usage of centered-pixel coordinates.
OPTION(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY "Turn on correct usage of centered-pixel coordinates." ON)
MARK_AS_ADVANCED(ITK_USE_CENTERED_PIXEL_COORDINATES_CONSISTENTLY)

#-----------------------------------------------------------------------------
# ITK turn on the correct usage of centered-pixel coordinates.
OPTION(ITK_USE_REGION_VALIDATION_IN_ITERATORS "Turn on region validation in Iterator constructors." ON)
MARK_AS_ADVANCED(ITK_USE_REGION_VALIDATION_IN_ITERATORS)


#-----------------------------------------------------------------------------
# ITK turn on the use of Template Meta Programming techniques for unrolling loops of array-like operations
OPTION(ITK_USE_TEMPLATE_META_PROGRAMMING_LOOP_UNROLLING "Turn on the use of Template Meta-Programming techniques for unrolling for-loops at compile time." ON)
MARK_AS_ADVANCED(ITK_USE_TEMPLATE_META_PROGRAMMING_LOOP_UNROLLING)

OPTION(ITK_USE_DEPRECATED_LEVELSET_INTERPOLATION "Turn on the use of a sparse field level set surface interpolation method that has been deprecated since ITK 3.6. Turn this flag ON to exactly replicate the behavior of ITK<=3.4." OFF)
MARK_AS_ADVANCED(ITK_USE_DEPRECATED_LEVELSET_INTERPOLATION)

OPTION(ITK_USE_DEPRECATED_FAST_MARCHING "Turn on incorrect handling of TrialPoints in itk::FastMarchingImageFilter result of a bug fixed in ITK 3.18. Turn this flag ON to exactly replicate the behavior of ITK<=3.16." OFF)
MARK_AS_ADVANCED(ITK_USE_DEPRECATED_FAST_MARCHING)


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

OPTION(ITK_USE_FLAT_DIRECTORY_INSTALL  "When ON, ITK will install all the header files in a single directory. This reduces the number of directories that must be searched for headers in projects based on ITK" OFF)
MARK_AS_ADVANCED(ITK_USE_FLAT_DIRECTORY_INSTALL)

IF(NOT ITK_INSTALL_BIN_DIR)
  # Solaris: unlike libraries, 64 bit executables should be installed in "bin${sun64}" only
  # if the 32 version is installed in  "bin/sparcv7" or "bin/i86", and a isaexec
  # is copied in "bin" with the name of the executable, so the system can choose the best
  # executable. If only a 32 or 64 bit version is available, the executable should go in
  # bin. So it seems better to let the packagers deal with that.
  # Ref: http://docs.sun.com/app/docs/doc/816-5138/dev-env-13?a=view
  SET(ITK_INSTALL_BIN_DIR "/bin")
ENDIF(NOT ITK_INSTALL_BIN_DIR)

IF(NOT ITK_INSTALL_LIB_DIR)
  # 64 bit libraries are not installed in the same directories on solaris
  # Ref: http://docs.sun.com/app/docs/doc/816-5138/6mba6ua5m?a=view
  SET(sun64 )
  IF(CMAKE_SYSTEM MATCHES "SunOS.*" AND CMAKE_SIZEOF_VOID_P EQUAL 8)
    EXEC_PROGRAM("uname"
      ARGS "-p"
      OUTPUT_VARIABLE sunproc
    )
    IF("${sunproc}" STREQUAL "sparc")
      SET(sun64 "/sparcv9")
    ENDIF("${sunproc}" STREQUAL "sparc")

    IF("${sunproc}" STREQUAL "i386")
      SET(sun64 "/amd64")
    ENDIF("${sunproc}" STREQUAL "i386")

    IF("${sun64}" STREQUAL "")
      # sun64 is not set - solaris has been ported to a new architecture?
      MESSAGE("Warning: Unknown processor '${sunproc}'. The libraries may not be installed in the right place.")
    ENDIF("${sun64}" STREQUAL "")
  ENDIF(CMAKE_SYSTEM MATCHES "SunOS.*" AND CMAKE_SIZEOF_VOID_P EQUAL 8)

  SET(ITK_INSTALL_LIB_DIR "/lib/InsightToolkit${sun64}")
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

# Because INSTALL_* commands require a leading / and because INSTALL (cmake 2.4
# and newer) requires no leading / to install under INSTALL_PREFIX, we
# are stripping the leading /. In the future, there should be no leading
# / in any install directory variables
STRING(REGEX REPLACE "^/" "" ITK_INSTALL_LIB_DIR_CM24 "${ITK_INSTALL_LIB_DIR}")
STRING(REGEX REPLACE "^/" "" ITK_INSTALL_BIN_DIR_CM24 "${ITK_INSTALL_BIN_DIR}")
STRING(REGEX REPLACE "^/" "" ITK_INSTALL_INCLUDE_DIR_CM24 "${ITK_INSTALL_INCLUDE_DIR}")
STRING(REGEX REPLACE "^/" "" ITK_INSTALL_PACKAGE_DIR_CM24 "${ITK_INSTALL_PACKAGE_DIR}")
STRING(REGEX REPLACE "^/" "" ITK_INSTALL_DOXYGEN_DIR_CM24 "${ITK_INSTALL_DOXYGEN_DIR}")
STRING(REGEX REPLACE "^/" "" ITK_INSTALL_TCL_DIR_CM24 "${ITK_INSTALL_TCL_DIR}")
STRING(REGEX REPLACE "^/" "" ITK_INSTALL_JAVA_DIR_CM24 "${ITK_INSTALL_JAVA_DIR}")

# Make sure the user does not try to install on top of the build tree.
IF(WIN32 OR APPLE OR UNIX)
  STRING(TOLOWER "${CMAKE_INSTALL_PREFIX}" _PREFIX)
  STRING(TOLOWER "${ITK_BINARY_DIR}" _BUILD)
ELSE(WIN32 OR APPLE OR UNIX)
  SET(_PREFIX "${CMAKE_INSTALL_PREFIX}")
  SET(_BUILD "${ITK_BINARY_DIR}")
ENDIF(WIN32 OR APPLE OR UNIX)
IF("${_PREFIX}" STREQUAL "${_BUILD}")
  MESSAGE(FATAL_ERROR
    "The current CMAKE_INSTALL_PREFIX points at the build tree:\n"
    "  ${CMAKE_INSTALL_PREFIX}\n"
    "This is not supported."
    )
ENDIF("${_PREFIX}" STREQUAL "${_BUILD}")

# set ITK_DIR so it can be used by subprojects
SET(ITK_DIR "${CMAKE_BINARY_DIR}" CACHE INTERNAL "ITK dir to be used by subprojects")

#-----------------------------------------------------------------------------
# Set up CPack support, to be able to distribute ITK binary packages. It will
# be particulary useful to distributed prebuilt ITK witk extended languages
# support enable
#
INCLUDE(InstallRequiredSystemLibraries)
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Insight Toolkit")
SET(CPACK_PACKAGE_VENDOR "ISC")
SET(CPACK_PACKAGE_VERSION_MAJOR "${ITK_VERSION_MAJOR}")
SET(CPACK_PACKAGE_VERSION_MINOR "${ITK_VERSION_MINOR}")
SET(CPACK_PACKAGE_VERSION_PATCH "${ITK_VERSION_PATCH}")
SET(CPACK_PACKAGE_INSTALL_DIRECTORY "InsightToolkit-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")
# SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/Copyright/ITKCopyright.txt")
INCLUDE(CPack)


#-----------------------------------------------------------------------------
# Perform a setup of ITK Wrapping. This will provide CMake options for
# individual wrapping as well as determine if CableSwig is required. If it is,
# ITK_NEED_CableSwig will be set. This file also tries to locate CableSwig by
# searching first in the source tree of ITK, and if that fails, it searches
# for a binary built of CableSwig.
#
INCLUDE(${ITK_SOURCE_DIR}/Wrapping/itkWrapSetup.cmake)


IF(WIN32)
  # Some libraries (e.g. vxl libs) have no dllexport markup, so we can
  # build full shared libraries only with the GNU toolchain.
  IF(CMAKE_COMPILER_IS_GNUCXX)
    # CMake adds --enable-all-exports on Cygwin (since Cygwin is
    # supposed to be UNIX-like), but we need to add it explicitly for
    # a native windows build with the MinGW tools.
    IF(MINGW)
      SET(CMAKE_SHARED_LIBRARY_CREATE_C_FLAGS
        "-shared -Wl,--export-all-symbols -Wl,--enable-auto-import")
      SET(CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS
        "-shared -Wl,--export-all-symbols -Wl,--enable-auto-import")
      SET(CMAKE_EXE_LINKER_FLAGS "-Wl,--enable-auto-import")
    ENDIF(MINGW)
    IF(CYGWIN)
      SET(CMAKE_EXE_LINKER_FLAGS "-Wl,--enable-auto-import")
    ENDIF(CYGWIN)
  ELSE(CMAKE_COMPILER_IS_GNUCXX)
   IF(BUILD_SHARED_LIBS)
     SET(ITK_COMMON_BUILD_TYPE "SHARED")
   ELSE(BUILD_SHARED_LIBS)
     SET(ITK_COMMON_BUILD_TYPE "STATIC")
   ENDIF(BUILD_SHARED_LIBS)
   SET(BUILD_SHARED_LIBS OFF)
  ENDIF(CMAKE_COMPILER_IS_GNUCXX)
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
  SET(VXL_NUMERICS_LIBRARIES itkvnl_algo)
ENDIF(ITK_USE_SYSTEM_VXL)


# Provide options to use system versions of third-party libraries.
ITK_THIRD_PARTY_OPTION(ZLIB zlib)
ITK_THIRD_PARTY_OPTION(PNG  png)
ITK_THIRD_PARTY_OPTION(TIFF tiff)

OPTION(ITK_USE_LIBXML2 "Use LIBXML2." OFF)
MARK_AS_ADVANCED(ITK_USE_LIBXML2)
ITK_THIRD_PARTY_OPTION(LIBXML2 libxml2)

#-----------------------------------------------------------------------------
# If choose to build documentation, then search for Doxygen executables.
OPTION(BUILD_DOXYGEN "Build Doxygen Documentation" OFF)
IF(BUILD_DOXYGEN)
  FIND_PACKAGE(Doxygen)
ENDIF(BUILD_DOXYGEN)

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
  SET(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -Wall -Wno-uninitialized -Wno-unused-parameter")
  SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -ftemplate-depth-50 -Wall")
  INCLUDE(${ITK_SOURCE_DIR}/CMake/itkCheckCXXAcceptsFlags.cmake)
  itkCHECK_CXX_ACCEPTS_FLAGS("-Wno-deprecated" CXX_HAS_DEPRECATED_FLAG)
  IF(CXX_HAS_DEPRECATED_FLAG)    
    SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -Wno-deprecated")
  ENDIF(CXX_HAS_DEPRECATED_FLAG)    
  IF(APPLE)
    # -no-cpp-precomp and -Wno-long-double were compiler flags present
    # only in Apple's gcc and not in the FSF gcc. The flags are obsolete
    # and totally removed in gcc 4.2 and later. I believe they are only
    # needed with gcc 3.3 and earlier.
    itkCHECK_CXX_ACCEPTS_FLAGS("-no-cpp-precomp" CXX_HAS_CPP_PRECOMP_FLAG)
    IF(CXX_HAS_CPP_PRECOMP_FLAG)    
      SET(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -no-cpp-precomp")
      SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -no-cpp-precomp")
    ENDIF(CXX_HAS_CPP_PRECOMP_FLAG)    
    itkCHECK_CXX_ACCEPTS_FLAGS("-Wno-long-double" CXX_HAS_LONGDOUBLE_FLAG)
    IF(CXX_HAS_LONGDOUBLE_FLAG)    
      SET(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -Wno-long-double")
      SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -Wno-long-double")
    ENDIF(CXX_HAS_LONGDOUBLE_FLAG)    

    OPTION(ITK_USE_64BITS_APPLE_TRUNCATION_WARNING "Turn on warnings on 64bits to 32bits truncations." OFF)
    MARK_AS_ADVANCED(ITK_USE_64BITS_APPLE_TRUNCATION_WARNING)

  ENDIF(APPLE)
  
  # gcc must have -msse2 option to enable sse2 support
  IF(VNL_CONFIG_ENABLE_SSE2 OR VNL_CONFIG_ENABLE_SSE2_ROUNDING)
    SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -msse2")
  ENDIF(VNL_CONFIG_ENABLE_SSE2 OR VNL_CONFIG_ENABLE_SSE2_ROUNDING)
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


#
#  This line is required for gcc 4.3.3, since ITK uses deprecated header files
#  by default.
#
INCLUDE(TestForANSIStreamHeaders)


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

# Add flags for the SUN compiler to provide all the methods for std::allocator.
#
INCLUDE(${ITK_SOURCE_DIR}/CMake/itkCheckCXXAcceptsFlags.cmake)
itkCHECK_CXX_ACCEPTS_FLAGS("-features=no%anachronisms" SUN_COMPILER)
IF(SUN_COMPILER)
  itkCHECK_CXX_ACCEPTS_FLAGS("-library=stlport4" SUN_COMPILER_HAS_STL_PORT_4)
  IF(SUN_COMPILER_HAS_STL_PORT_4)
    SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -library=stlport4")
  ENDIF(SUN_COMPILER_HAS_STL_PORT_4)
ENDIF(SUN_COMPILER)

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

#-----------------------------------------------------------------------------
# Test for an Objective-C++ compiler.
SET(ITK_OBJCXX_COMPILER_WORKS 0)
IF(APPLE)
  TRY_COMPILE(ITK_OBJCXX_COMPILER_WORKS
              ${ITK_BINARY_DIR}/CMakeTmp
              ${ITK_SOURCE_DIR}/CMake/itkTestObjCxxCompiler.mm
              OUTPUT_VARIABLE ITK_OBJCXX_TRY_COMPILE_OUTPUT)
  IF( ITK_OBJCXX_COMPILER_WORKS )
    FILE(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
        "Determining if the Objective-C compiler works passed with "
        "the following output:\n${ITK_OBJCXX_TRY_COMPILE_OUTPUT}\n\n")
  ELSE( ITK_OBJCXX_COMPILER_WORKS )
    FILE(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
      "Determining if the Objective-C compiler works failed with "
      "the following output:\n${ITK_OBJCXX_TRY_COMPILE_OUTPUT}\n\n")
  ENDIF( ITK_OBJCXX_COMPILER_WORKS )
ENDIF(APPLE)

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


#-----------------------------------------------------------------------------
#
# Check types such as long long and __int64
INCLUDE(${ITK_SOURCE_DIR}/CMake/itkTestTypes.cmake)


#-----------------------------------------------------------------------------
# check if the platform has the header file "fenv.h" which has been added as part of the C99 standard
INCLUDE(CheckIncludeFiles)
CHECK_INCLUDE_FILES(fenv.h HAVE_FENV_H)

#-----------------------------------------------------------------------------
# check if the platform has some widely used header files
CHECK_INCLUDE_FILES("unistd.h" HAVE_UNISTD_H)

#
# See if compiler preprocessor has the __FUNCTION__ directive used by itkExceptionMacro
#
INCLUDE(${ITK_SOURCE_DIR}/CMake/CheckCPPDirective.cmake)
CHECK_CPP_DIRECTIVE_EXISTS(__FUNCTION__ ITK_CPP_FUNCTION)

#
# See if compiler support template partial specialization
#
TRY_COMPILE(ITK_SUPPORTS_TEMPLATE_PARTIAL_SPECIALIZATION
      ${ITK_BINARY_DIR}/CMakeTmp
      ${ITK_SOURCE_DIR}/CMake/itkTestTemplatePartialSpecialization.cxx
      OUTPUT_VARIABLE ITK_SUPPORTS_TEMPLATE_PARTIAL_SPECIALIZATION_OUTPUT)

#
# Figure out how the compiler likes to declare templated functions as friends
# of a templated class.
#
INCLUDE(${ITK_SOURCE_DIR}/CMake/itkTestFriendTemplatedFunction.cmake)


#-----------------------------------------------------------------------------
# check if the platform has c style IO functions that uses wchar_t * filenames
# For some reason I can't manage to get a usable result from CHECK_FUNCTION_EXISTS
TRY_COMPILE(ITK_SUPPORTS_WCHAR_T_FILENAME_CSTYLEIO
      ${ITK_BINARY_DIR}/CMakeTmp
      ${ITK_SOURCE_DIR}/CMake/itkTestCStyleIOWCharFilename.cxx
      OUTPUT_VARIABLE ITK_SUPPORTS_WCHAR_T_FILENAME_CSTYLEIO_OUTPUT)

#-----------------------------------------------------------------------------
# check if the platform has IO streams constructors that uses wchar_t * filenames
TRY_COMPILE(ITK_SUPPORTS_WCHAR_T_FILENAME_IOSTREAMS_CONSTRUCTORS
      ${ITK_BINARY_DIR}/CMakeTmp
      ${ITK_SOURCE_DIR}/CMake/itkTestIOStreamsWCharFilenameConstructors.cxx
      OUTPUT_VARIABLE ITK_SUPPORTS_WCHAR_T_FILENAME_IOSTREAMS_CONSTRUCTORS_OUTPUT)

#-----------------------------------------------------------------------------
# check if the platform can compile fdstream.hpp
IF(HAVE_UNISTD_H)
  TRY_COMPILE(ITK_SUPPORTS_FDSTREAM_HPP
    ${ITK_BINARY_DIR}/CMakeTmp
    ${ITK_SOURCE_DIR}/CMake/itkTestFDStream.cxx
    CMAKE_FLAGS -DINCLUDE_DIRECTORIES:STRING=${ITK_SOURCE_DIR}/Utilities/itkExtHdrs
    COMPILE_DEFINITIONS -DITK_HAVE_UNISTD_H=${HAVE_UNISTD_H}
    OUTPUT_VARIABLE ITK_SUPPORTS_FDSTREAM_HPP_OUTPUT)
ELSE(HAVE_UNISTD_H)
  TRY_COMPILE(ITK_SUPPORTS_FDSTREAM_HPP
    ${ITK_BINARY_DIR}/CMakeTmp
    ${ITK_SOURCE_DIR}/CMake/itkTestFDStream.cxx
    CMAKE_FLAGS -DINCLUDE_DIRECTORIES:STRING=${ITK_SOURCE_DIR}/Utilities/itkExtHdrs
    OUTPUT_VARIABLE ITK_SUPPORTS_FDSTREAM_HPP_OUTPUT)
ENDIF(HAVE_UNISTD_H)

#-----------------------------------------------------------------------------
# Configure the default ITK_DATA_ROOT for the location of ITK Data.
FIND_PATH(ITK_DATA_ROOT ITKData.readme ${ITK_SOURCE_DIR}/Testing/Data $ENV{ITK_DATA_ROOT})
MARK_AS_ADVANCED(ITK_DATA_ROOT)

#-----------------------------------------------------------------------------
# Configure the default ITK_BRAINWEB_DATA_ROOT for the location of ITK Data.
# When this data is available, additional 3D tests are enabled.
FIND_PATH(ITK_BRAINWEB_DATA_ROOT brainweb165a10f17.mha 
  DOC "Directory with data taken from http://public.kitware.com/pub/itk/Data/BrainWeb/")
MARK_AS_ADVANCED(ITK_BRAINWEB_DATA_ROOT)

#-----------------------------------------------------------------------------
# Configure the default ITK_MRI_UNC_DATABASE_DATA_ROOT for the location of the
# database of healthy volunteers MRI brain images.
# When this data is available, additional 3D tests are enabled.
FIND_PATH(ITK_MRI_UNC_DATABASE_DATA_ROOT LICENSE.txt
  DOC "Directory with data taken from http://public.kitware.com/pub/itk/Data/HealthyVolunteersMRIDatabase/")
MARK_AS_ADVANCED(ITK_MRI_UNC_DATABASE_DATA_ROOT)

#-----------------------------------------------------------------------------
# Configure the default ITK_OSIRIX_DATA_ROOT for the location of Data from the Osirix web site.
# When this data is available, additional DICOM tests are enabled.
FIND_PATH(ITK_OSIRIX_DATA_ROOT CEREBRIX
  DOC "Directory with data taken from http://www.osirix-viewer.com/Downloads.html")
MARK_AS_ADVANCED(ITK_OSIRIX_DATA_ROOT)


#-----------------------------------------------------------------------------
# Configure the default ITK_VISIBLEHUMAN_DATA_ROOT for the location of Data from the Visible Human Project
# When this data is available, additional Examples are enabled.
FIND_PATH(ITK_VISIBLEHUMAN_DATA_ROOT Male/Fullcolor/fullbody
  DOC "Directory with data taken from http://www.nlm.nih.gov/research/visible/getting_data.html")
MARK_AS_ADVANCED(ITK_VISIBLEHUMAN_DATA_ROOT)


#-----------------------------------------------------------------------------
# This flag is used in particular, to enable some tests that require large memory to run.
# This probably can be discovered at configuration time by CMake. (Check with CMake developers).
SET(ITK_COMPUTER_MEMORY_SIZE 1 CACHE STRING "Provide here the size of your RAM Memory in GigaBytes") 
MARK_AS_ADVANCED(ITK_COMPUTER_MEMORY_SIZE)

#-----------------------------------------------------------------------------
# Configure files with settings for use by the build.
CONFIGURE_FILE(${ITK_SOURCE_DIR}/itkConfigure.h.in 
  ${ITK_BINARY_DIR}/itkConfigure.h)
IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  INSTALL(FILES ${ITK_BINARY_DIR}/itkConfigure.h
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24}
    COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)

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
    IF( "${GDCM_MAJOR_VERSION}" LESS 2.0 )
      SET(ITK_GDCM_LIBRARIES gdcm)
    ELSE( "${GDCM_MAJOR_VERSION}" LESS 2.0 )
      IF(ITK_USE_SYSTEM_TIFF)
        # Long story short. ITKIO is NOT linked to itkjpeg8 explicitely. itkjpeg8 dep is being
        # pulled from linking to itktiff. However in case of ITK_SYSTEM_TIFF the FindTIFF module
        # do not explicitely link to the system jpeg. One can check however that on most GNU/Linux system
        # $ ldd /usr/lib/libtiff.so
        #    linux-vdso.so.1 =>  (0x00007fff67fff000)
        #    libjpeg.so.62 => /usr/lib/libjpeg.so.62 (0x00007f635f9f2000)
        # As a side note, ITK_SYSTEM_TIFF is somewhat robust as itkjpeg8 dep can also be pulled from
        # linking to the default gdcm libraries since they use the itkjpeg libraries.
        # I do not know the correct fix:
        # 1. Have FindTIFF link to jpeg8 (which would be the right thing to do for static system libtiff)
        # 2. Have ITKIO explicitely link to itkjpeg8
        # For the time being let's explicitely pull the dep to itkjpeg8 ourselves.
        SET(ITK_GDCM_LIBRARIES gdcmMSFF itkjpeg8)
      ELSE(ITK_USE_SYSTEM_TIFF)
        SET(ITK_GDCM_LIBRARIES gdcmMSFF)
      ENDIF(ITK_USE_SYSTEM_TIFF)
    ENDIF( "${GDCM_MAJOR_VERSION}" LESS 2.0 )
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
  INSTALL(FILES
    ${ITK_BINARY_DIR}/ITKBuildSettings.cmake
    ${ITK_BINARY_DIR}/ITKLibraryDepends.cmake
    ${ITK_BINARY_DIR}/UseITK.cmake
    DESTINATION ${ITK_INSTALL_PACKAGE_DIR_CM24}
    COMPONENT Development
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
    IF("${COMP_OPTION}" STREQUAL "-frename-registers")
      MESSAGE(FATAL_ERROR "-frename-registers causes runtime bugs.  It must be removed from your compilation options.")
    ENDIF("${COMP_OPTION}" STREQUAL "-frename-registers")
    IF("${COMP_OPTION}" STREQUAL "-ffloat-store")
      MESSAGE(FATAL_ERROR "-ffloat-store causes runtime bugs on gcc 3.2.3 (appearently not on gcc 3.4.3, but the exact criteria is not known).  It must be removed from your compilation options.")
    ENDIF("${COMP_OPTION}" STREQUAL "-ffloat-store")
  ENDFOREACH(COMP_OPTION)
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

