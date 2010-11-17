# This is the root ITK CMakeLists file.
cmake_minimum_required(VERSION 2.8.2)
if(COMMAND CMAKE_POLICY)
  cmake_policy(SET CMP0003 NEW)
endif(COMMAND CMAKE_POLICY)

#-----------------------------------------------------------------------------
# Sanity check -- prevent in-source builds
#
include(${CMAKE_CURRENT_SOURCE_DIR}/CMake/PreventInSourceBuilds.cmake)

project(ITK)
include_regular_expression("^([Nn]rrd|itk|vtk|vnl|vcl|vxl|v3p|f2c|netlib|ce|itpack|DICOM|meta|png|dbh|tif|jpeg|zlib|gdcm).*$")
source_group("XML Files" REGULAR_EXPRESSION "[.]xml$")

# On Visual Studio 8 MS deprecated C. This removes all 1.276E1265 security
# warnings
if(WIN32)
    if(NOT CYGWIN)
      if(NOT MINGW)
        if(NOT ITK_ENABLE_VISUAL_STUDIO_DEPRECATED_C_WARNINGS)
          add_definitions(
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
        endif(NOT ITK_ENABLE_VISUAL_STUDIO_DEPRECATED_C_WARNINGS)
      endif(NOT MINGW)
    endif(NOT CYGWIN)
endif(WIN32)


# If CMake is the version we're expecting, don't show the
# CMAKE_BACKWARDS_COMPATIBILITY option. If the version is higher than the
# minimal version required, then show the backward compatibility option.
if("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" MATCHES "^2\\.4$")
  mark_as_advanced(FORCE CMAKE_BACKWARDS_COMPATIBILITY)
else("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" MATCHES "^2\\.4$")
  mark_as_advanced(CLEAR CMAKE_BACKWARDS_COMPATIBILITY)
endif("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" MATCHES "^2\\.4$")

#-----------------------------------------------------------------------------
# ITK version number.  An even minor number corresponds to releases.
set(ITK_VERSION_MAJOR "4")
set(ITK_VERSION_MINOR "0")
set(ITK_VERSION_PATCH "0")

# Version string should not include patch level.  The major.minor is
# enough to distinguish available features of the toolkit.
set(ITK_VERSION_STRING "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")

#-----------------------------------------------------------------------------
# Load some macros.
include(${ITK_SOURCE_DIR}/Utilities/itkThirdParty.cmake)

#-----------------------------------------------------------------------------
# Test for some required system information.
include(${CMAKE_ROOT}/Modules/CMakeBackwardCompatibilityC.cmake)
include(${CMAKE_ROOT}/Modules/CMakeBackwardCompatibilityCXX.cmake)

#-----------------------------------------------------------------------------
# Configure Dart testing support.  This should be done before any
# message(FATAL_ERROR ...) commands are invoked.
include(${CMAKE_ROOT}/Modules/Dart.cmake)
find_package(Tclsh)
mark_as_advanced(TCL_TCLSH DART_ROOT)
enable_testing()
if(BUILD_TESTING)
  configure_file(${ITK_SOURCE_DIR}/CMake/RemoveTemporaryFiles.cmake.in
    ${ITK_BINARY_DIR}/CMake/RemoveTemporaryFiles.cmake @ONLY IMMEDIATE)
  configure_file(${ITK_SOURCE_DIR}/CMake/itkSampleBuildTest.cmake.in
    ${ITK_BINARY_DIR}/CMake/itkSampleBuildTest.cmake @ONLY)
  configure_file(${ITK_SOURCE_DIR}/CMake/CTestCustom.ctest.in
    ${ITK_BINARY_DIR}/CMake/CTestCustom.ctest @ONLY)
  configure_file(${ITK_SOURCE_DIR}/CMake/CTestCustom.ctest.in
    ${ITK_BINARY_DIR}/Testing/Code/Common/CTestCustom.ctest @ONLY)
  file(WRITE ${ITK_BINARY_DIR}/CTestCustom.cmake
    "include(\"${ITK_BINARY_DIR}/CMake/CTestCustom.ctest\")\n")

  configure_file(${ITK_SOURCE_DIR}/InsightLogo.gif
                 ${ITK_BINARY_DIR}/Testing/HTML/TestingResults/Icons/Logo.gif
                 COPYONLY IMMEDIATE)
  set(BUILDNAME "${BUILDNAME}" CACHE STRING "Name of build on the dashboard")
  mark_as_advanced(BUILDNAME)
endif(BUILD_TESTING)

#-----------------------------------------------------------------------------
# Output directories.
if(NOT LIBRARY_OUTPUT_PATH)
  set(LIBRARY_OUTPUT_PATH ${ITK_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all libraries.")
endif(NOT LIBRARY_OUTPUT_PATH)
if(NOT EXECUTABLE_OUTPUT_PATH)
  set(EXECUTABLE_OUTPUT_PATH ${ITK_BINARY_DIR}/bin CACHE INTERNAL "Single output directory for building all executables.")
endif(NOT EXECUTABLE_OUTPUT_PATH)
mark_as_advanced(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)

set(ITK_LIBRARY_PATH "${LIBRARY_OUTPUT_PATH}")
set(ITK_EXECUTABLE_PATH "${EXECUTABLE_OUTPUT_PATH}")
set(CXX_TEST_PATH ${EXECUTABLE_OUTPUT_PATH})

set(CMAKE_MODULE_PATH ${ITK_SOURCE_DIR}/CMake)

include(itkCheckSourceTree)

# Deal with FFT v3 Options
mark_as_advanced(USE_FFTWD)
option(USE_FFTWD "Use double precision FFTW if found" OFF)
mark_as_advanced(USE_FFTWF)
option(USE_FFTWF "Use single precision FFTW if found" OFF)
if( USE_FFTWD OR USE_FFTWF )
  set(msg "ATTENTION: You have enabled the use of FFTW,")
  set(msg "${msg} this library is distributed under a GPL license.")
  set(msg "${msg} By enabling this option, the binary of the ITK libraries")
  set(msg "${msg} that you are going to build will be covered by a GPL license,")
  set(msg "${msg} and so it will be any executable that you link against these libraries.")
  message("${msg}")
  find_package( FFTW )
endif( USE_FFTWD OR USE_FFTWF )

# Deal with MINC2 options
mark_as_advanced(ITK_USE_MINC2)
option(ITK_USE_MINC2 "Use MINC2 File Format" OFF)
if( ITK_USE_MINC2 )
  find_package( MINC2 )
  if( NOT MINC2_FOUND )
    message("You requested to use MINC2 but some components are missing")
  endif( NOT MINC2_FOUND )
endif( ITK_USE_MINC2 )


#-----------------------------------------------------------------------------
# ITK build configuration options.
option(BUILD_SHARED_LIBS "Build ITK with shared libraries." OFF)
set(ITK_BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS})

if(NOT ITK_NO_LIBRARY_VERSION)
  # This setting of SOVERSION assumes that any API change
  # will increment either the minor or major version number of ITK.
  set(ITK_LIBRARY_PROPERTIES
      VERSION   "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}.${ITK_VERSION_PATCH}"
      SOVERSION "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}"
     )
endif(NOT ITK_NO_LIBRARY_VERSION)

#-----------------------------------------------------------------------------
# Provide compatibility options.
option(ITK_LEGACY_REMOVE "Remove all legacy code completely." OFF)
option(ITK_LEGACY_SILENT "Silence all legacy code messages." OFF)
mark_as_advanced(ITK_LEGACY_REMOVE ITK_LEGACY_SILENT)

#-----------------------------------------------------------------------------
# ITK build classes that are in the review process
option(ITK_USE_REVIEW "Build algorithms that are candidates to be included in the Toolkit. This classes are not covered by the Backward Compatibility Policy, nor the Copyright Policy of ITK." OFF)
mark_as_advanced(ITK_USE_REVIEW)

#-----------------------------------------------------------------------------
# ITK uses KWStyle for checking the coding style
include(${ITK_SOURCE_DIR}/Utilities/KWStyle/KWStyle.cmake)

#-----------------------------------------------------------------------------
# ITK turn on concept checking
option(ITK_USE_CONCEPT_CHECKING "Turn on concept checking to give helpful errors at compile time if a type cannot be used as a template parameter." ON)
mark_as_advanced(ITK_USE_CONCEPT_CHECKING)
option(ITK_USE_STRICT_CONCEPT_CHECKING "Turn on Strict concept checking to give more stringent errors at compile time if a type cannot be used as a template parameter." OFF)
mark_as_advanced(ITK_USE_STRICT_CONCEPT_CHECKING)

#-----------------------------------------------------------------------------
# ITK turn on the use of experimental IO factories for the TransformFileReader and TransformFileWriter
option(ITK_USE_TRANSFORM_IO_FACTORIES "Turn on the experimental IO factories for reading and writing transforms." OFF)
mark_as_advanced(ITK_USE_TRANSFORM_IO_FACTORIES)
if(ITK_USE_TRANSFORM_IO_FACTORIES)
  if(NOT ITK_USE_REVIEW)
    message(FATAL_ERROR "ITK_USE_REVIEW is currently OFF but it should be ON if you want to use the experimental transform IO factories.")
  endif(NOT ITK_USE_REVIEW)
endif(ITK_USE_TRANSFORM_IO_FACTORIES)

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

option(ITK_USE_FLAT_DIRECTORY_INSTALL  "When ON, ITK will install all the header files in a single directory. This reduces the number of directories that must be searched for headers in projects based on ITK" OFF)
mark_as_advanced(ITK_USE_FLAT_DIRECTORY_INSTALL)

set(ITK_VERSIONED_INSTALL_DIRECTORY "InsightToolkit-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")

if(NOT ITK_INSTALL_BIN_DIR)
  # Solaris: unlike libraries, 64 bit executables should be installed in "bin${sun64}" only
  # if the 32 version is installed in  "bin/sparcv7" or "bin/i86", and a isaexec
  # is copied in "bin" with the name of the executable, so the system can choose the best
  # executable. If only a 32 or 64 bit version is available, the executable should go in
  # bin. So it seems better to let the packagers deal with that.
  # Ref: http://docs.sun.com/app/docs/doc/816-5138/dev-env-13?a=view
  set(ITK_INSTALL_BIN_DIR "/bin")
endif(NOT ITK_INSTALL_BIN_DIR)

if(NOT ITK_INSTALL_LIB_DIR)
  # 64 bit libraries are not installed in the same directories on solaris
  # Ref: http://docs.sun.com/app/docs/doc/816-5138/6mba6ua5m?a=view
  set(sun64 )
  if(CMAKE_SYSTEM MATCHES "SunOS.*" AND CMAKE_SIZEOF_VOID_P EQUAL 8)
    exec_program("uname"
      ARGS "-p"
      OUTPUT_VARIABLE sunproc
    )
    if("${sunproc}" STREQUAL "sparc")
      set(sun64 "/sparcv9")
    endif("${sunproc}" STREQUAL "sparc")

    if("${sunproc}" STREQUAL "i386")
      set(sun64 "/amd64")
    endif("${sunproc}" STREQUAL "i386")

    if("${sun64}" STREQUAL "")
      # sun64 is not set - solaris has been ported to a new architecture?
      message("Warning: Unknown processor '${sunproc}'. The libraries may not be installed in the right place.")
    endif("${sun64}" STREQUAL "")
  endif(CMAKE_SYSTEM MATCHES "SunOS.*" AND CMAKE_SIZEOF_VOID_P EQUAL 8)

  set(ITK_INSTALL_LIB_DIR "/lib/${ITK_VERSIONED_INSTALL_DIRECTORY}${sun64}")
endif(NOT ITK_INSTALL_LIB_DIR)

if(NOT ITK_INSTALL_DATA_DIR)
  set(ITK_INSTALL_DATA_DIR "/share/${ITK_VERSIONED_INSTALL_DIRECTORY}")
endif(NOT ITK_INSTALL_DATA_DIR)

if(NOT ITK_INSTALL_INCLUDE_DIR)
  set(ITK_INSTALL_INCLUDE_DIR "/include/${ITK_VERSIONED_INSTALL_DIRECTORY}")
endif(NOT ITK_INSTALL_INCLUDE_DIR)

if(NOT ITK_INSTALL_PACKAGE_DIR)
  set(ITK_INSTALL_PACKAGE_DIR ${ITK_INSTALL_LIB_DIR}
    CACHE INTERNAL "")
endif(NOT ITK_INSTALL_PACKAGE_DIR)

if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  set(ITK_INSTALL_NO_DEVELOPMENT 0)
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)

if(NOT ITK_INSTALL_NO_RUNTIME)
  set(ITK_INSTALL_NO_RUNTIME 0)
endif(NOT ITK_INSTALL_NO_RUNTIME)

if(NOT ITK_INSTALL_NO_DOCUMENTATION)
  set(ITK_INSTALL_NO_DOCUMENTATION 0)
endif(NOT ITK_INSTALL_NO_DOCUMENTATION)

set(ITK_INSTALL_NO_LIBRARIES)
if(ITK_BUILD_SHARED_LIBS)
  if(ITK_INSTALL_NO_RUNTIME AND ITK_INSTALL_NO_DEVELOPMENT)
    set(ITK_INSTALL_NO_LIBRARIES 1)
  endif(ITK_INSTALL_NO_RUNTIME AND ITK_INSTALL_NO_DEVELOPMENT)
else(ITK_BUILD_SHARED_LIBS)
  if(ITK_INSTALL_NO_DEVELOPMENT)
    set(ITK_INSTALL_NO_LIBRARIES 1)
  endif(ITK_INSTALL_NO_DEVELOPMENT)
endif(ITK_BUILD_SHARED_LIBS)

# Because INSTALL_* commands require a leading / and because install(cmake 2.4
# and newer) requires no leading / to install under INSTALL_PREFIX, we
# are stripping the leading /. In the future, there should be no leading
# / in any install directory variables
string(REGEX REPLACE "^/" "" ITK_INSTALL_LIB_DIR_CM24 "${ITK_INSTALL_LIB_DIR}")
string(REGEX REPLACE "^/" "" ITK_INSTALL_BIN_DIR_CM24 "${ITK_INSTALL_BIN_DIR}")
string(REGEX REPLACE "^/" "" ITK_INSTALL_INCLUDE_DIR_CM24 "${ITK_INSTALL_INCLUDE_DIR}")
string(REGEX REPLACE "^/" "" ITK_INSTALL_PACKAGE_DIR_CM24 "${ITK_INSTALL_PACKAGE_DIR}")
string(REGEX REPLACE "^/" "" ITK_INSTALL_DOXYGEN_DIR_CM24 "${ITK_INSTALL_DOXYGEN_DIR}")
string(REGEX REPLACE "^/" "" ITK_INSTALL_TCL_DIR_CM24 "${ITK_INSTALL_TCL_DIR}")
string(REGEX REPLACE "^/" "" ITK_INSTALL_JAVA_DIR_CM24 "${ITK_INSTALL_JAVA_DIR}")

# Make sure the user does not try to install on top of the build tree.
if(WIN32 OR APPLE OR UNIX)
  string(TOLOWER "${CMAKE_INSTALL_PREFIX}" _PREFIX)
  string(TOLOWER "${ITK_BINARY_DIR}" _BUILD)
else(WIN32 OR APPLE OR UNIX)
  set(_PREFIX "${CMAKE_INSTALL_PREFIX}")
  set(_BUILD "${ITK_BINARY_DIR}")
endif(WIN32 OR APPLE OR UNIX)
if("${_PREFIX}" STREQUAL "${_BUILD}")
  message(FATAL_ERROR
    "The current CMAKE_INSTALL_PREFIX points at the build tree:\n"
    "  ${CMAKE_INSTALL_PREFIX}\n"
    "This is not supported."
    )
endif("${_PREFIX}" STREQUAL "${_BUILD}")

# set ITK_DIR so it can be used by subprojects
set(ITK_DIR "${CMAKE_BINARY_DIR}" CACHE INTERNAL "ITK dir to be used by subprojects")

#-----------------------------------------------------------------------------
# Set up CPack support, to be able to distribute ITK binary packages. It will
# be particulary useful to distributed prebuilt ITK witk extended languages
# support enable
#
include(InstallRequiredSystemLibraries)
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Insight Toolkit")
set(CPACK_PACKAGE_VENDOR "ISC")
set(CPACK_PACKAGE_VERSION_MAJOR "${ITK_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${ITK_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${ITK_VERSION_PATCH}")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "${ITK_VERSIONED_INSTALL_DIRECTORY}")
# set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
include(CPack)


#-----------------------------------------------------------------------------
# Perform a setup of ITK Wrapping. This will provide CMake options for
# individual wrapping as well as determine if CableSwig is required. If it is,
# ITK_NEED_CableSwig will be set. This file also tries to locate CableSwig by
# searching first in the source tree of ITK, and if that fails, it searches
# for a binary built of CableSwig.
#
include(${ITK_SOURCE_DIR}/Wrapping/itkWrapSetup.cmake)


if(WIN32)
  # Some libraries (e.g. vxl libs) have no dllexport markup, so we can
  # build full shared libraries only with the GNU toolchain.
  if(CMAKE_COMPILER_IS_GNUCXX)
    # CMake adds --enable-all-exports on Cygwin (since Cygwin is
    # supposed to be UNIX-like), but we need to add it explicitly for
    # a native windows build with the MinGW tools.
    if(MINGW)
      set(CMAKE_SHARED_LIBRARY_CREATE_C_FLAGS
        "-shared -Wl,--export-all-symbols -Wl,--enable-auto-import")
      set(CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS
        "-shared -Wl,--export-all-symbols -Wl,--enable-auto-import")
      set(CMAKE_EXE_LINKER_FLAGS "-Wl,--enable-auto-import")
    endif(MINGW)
    if(CYGWIN)
      set(CMAKE_EXE_LINKER_FLAGS "-Wl,--enable-auto-import")
    endif(CYGWIN)
  else(CMAKE_COMPILER_IS_GNUCXX)
   if(BUILD_SHARED_LIBS)
     set(ITK_COMMON_BUILD_TYPE "SHARED")
   else(BUILD_SHARED_LIBS)
     set(ITK_COMMON_BUILD_TYPE "STATIC")
   endif(BUILD_SHARED_LIBS)
   set(BUILD_SHARED_LIBS OFF)
  endif(CMAKE_COMPILER_IS_GNUCXX)
endif(WIN32)

option(BUILD_EXAMPLES "Build the Examples directory." ON)

option(ITK_USE_SYSTEM_VXL "Use an outside build of VXL." OFF)
mark_as_advanced(ITK_USE_SYSTEM_VXL)
if(ITK_USE_SYSTEM_VXL)
  find_package(VXL)
  if(VXL_FOUND)
    include(${VXL_CMAKE_DIR}/UseVXL.cmake)
    set(VXL_NUMERICS_LIBRARIES vnl vnl_algo netlib vcl)
  else(VXL_FOUND)
    message(FATAL_ERROR "Must set VXL_DIR for ITK_USE_SYSTEM_VXL.")
  endif(VXL_FOUND)
else(ITK_USE_SYSTEM_VXL)
  set(VXL_NETLIB_INCLUDE_DIR ${ITK_SOURCE_DIR}/Utilities/vxl/v3p/netlib)
  set(VXL_NUMERICS_LIBRARIES itkvnl_algo)
endif(ITK_USE_SYSTEM_VXL)


# Provide options to use system versions of third-party libraries.
ITK_THIRD_PARTY_OPTION(ZLIB zlib)
ITK_THIRD_PARTY_OPTION(PNG  png)
ITK_THIRD_PARTY_OPTION(JPEG jpeg)
ITK_THIRD_PARTY_OPTION(OpenJPEG openjpeg)
ITK_THIRD_PARTY_OPTION(TIFF tiff)

option(ITK_USE_LIBXML2 "Use LIBXML2." OFF)
mark_as_advanced(ITK_USE_LIBXML2)
ITK_THIRD_PARTY_OPTION(LIBXML2 libxml2)

#-----------------------------------------------------------------------------
# If choose to build documentation, then search for Doxygen executables.
option(BUILD_DOXYGEN "Build Doxygen Documentation" OFF)
if(BUILD_DOXYGEN)
  find_package(Doxygen)
endif(BUILD_DOXYGEN)


#-----------------------------------------------------------------------------
# ITK requires special compiler flags on some platforms.
if(CMAKE_COMPILER_IS_GNUCXX)
  set(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -Wall -Wno-uninitialized -Wno-unused-parameter")
  set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -ftemplate-depth-50 -Wall")
  include(${ITK_SOURCE_DIR}/CMake/itkCheckCXXAcceptsFlags.cmake)
  itkCHECK_CXX_ACCEPTS_FLAGS("-Wno-deprecated" CXX_HAS_DEPRECATED_FLAG)
  if(CXX_HAS_DEPRECATED_FLAG)
    set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -Wno-deprecated")
  endif(CXX_HAS_DEPRECATED_FLAG)
  if(APPLE)
    # -no-cpp-precomp and -Wno-long-double were compiler flags present
    # only in Apple's gcc and not in the FSF gcc. The flags are obsolete
    # and totally removed in gcc 4.2 and later. I believe they are only
    # needed with gcc 3.3 and earlier.
    itkCHECK_CXX_ACCEPTS_FLAGS("-no-cpp-precomp" CXX_HAS_CPP_PRECOMP_FLAG)
    if(CXX_HAS_CPP_PRECOMP_FLAG)
      set(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -no-cpp-precomp")
      set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -no-cpp-precomp")
    endif(CXX_HAS_CPP_PRECOMP_FLAG)
    itkCHECK_CXX_ACCEPTS_FLAGS("-Wno-long-double" CXX_HAS_LONGDOUBLE_FLAG)
    if(CXX_HAS_LONGDOUBLE_FLAG)
      set(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -Wno-long-double")
      set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -Wno-long-double")
    endif(CXX_HAS_LONGDOUBLE_FLAG)

    option(ITK_USE_64BITS_APPLE_TRUNCATION_WARNING "Turn on warnings on 64bits to 32bits truncations." OFF)
    mark_as_advanced(ITK_USE_64BITS_APPLE_TRUNCATION_WARNING)

  endif(APPLE)

  # gcc must have -msse2 option to enable sse2 support
  if(VNL_CONFIG_ENABLE_SSE2 OR VNL_CONFIG_ENABLE_SSE2_ROUNDING)
    set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -msse2")
  endif(VNL_CONFIG_ENABLE_SSE2 OR VNL_CONFIG_ENABLE_SSE2_ROUNDING)
endif(CMAKE_COMPILER_IS_GNUCXX)

if(UNIX)
  if(NOT CMAKE_COMPILER_IS_GNUCXX)
    include(${ITK_SOURCE_DIR}/CMake/TestNO_ICC_IDYNAMIC_NEEDED.cmake)
    TESTNO_ICC_IDYNAMIC_NEEDED(NO_ICC_IDYNAMIC_NEEDED ${ITK_SOURCE_DIR}/CMake )
    if(NO_ICC_IDYNAMIC_NEEDED)
      set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS}")
    else(NO_ICC_IDYNAMIC_NEEDED)
      set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -i_dynamic")
    endif(NO_ICC_IDYNAMIC_NEEDED)
  endif(NOT CMAKE_COMPILER_IS_GNUCXX)
endif(UNIX)


#
#  This line is required for gcc 4.3.3, since ITK uses deprecated header files
#  by default.
#
include(TestForANSIStreamHeaders)


# Force the use of ansi cxx flags (i.e. -LANG:std on sgi )
if("x${CMAKE_ANSI_CXXFLAGS}" MATCHES "^x.*[^ ]")
  set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} ${CMAKE_ANSI_CXXFLAGS}")
endif("x${CMAKE_ANSI_CXXFLAGS}" MATCHES "^x.*[^ ]")
if("x${CMAKE_ANSI_CFLAGS}" MATCHES "^x.*[^ ]")
  set(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} ${CMAKE_ANSI_CFLAGS}")
endif("x${CMAKE_ANSI_CFLAGS}" MATCHES "^x.*[^ ]")

if(MINGW)
  link_libraries(gdi32)
endif(MINGW)

# Add flags for the SUN compiler to provide all the methods for std::allocator.
#
include(${ITK_SOURCE_DIR}/CMake/itkCheckCXXAcceptsFlags.cmake)
itkCHECK_CXX_ACCEPTS_FLAGS("-features=no%anachronisms" SUN_COMPILER)
if(SUN_COMPILER)
  itkCHECK_CXX_ACCEPTS_FLAGS("-library=stlport4" SUN_COMPILER_HAS_STL_PORT_4)
  if(SUN_COMPILER_HAS_STL_PORT_4)
    set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -library=stlport4")
  endif(SUN_COMPILER_HAS_STL_PORT_4)
endif(SUN_COMPILER)

# for the gnu compiler a -D_PTHREADS is needed on sun
# for the native compiler a -mt flag is needed on the sun
if(CMAKE_USE_PTHREADS)
  if(CMAKE_SYSTEM MATCHES "SunOS.*")
    if(CMAKE_COMPILER_IS_GNUCXX)
      set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -D_PTHREADS")
    else(CMAKE_COMPILER_IS_GNUCXX)
      set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -mt")
      set(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -mt")
    endif(CMAKE_COMPILER_IS_GNUCXX)
  endif(CMAKE_SYSTEM MATCHES "SunOS.*")
endif(CMAKE_USE_PTHREADS)

if(WIN32)
  set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -DNOMINMAX")
endif(WIN32)

#-----------------------------------------------------------------------------
# Test for an Objective-C++ compiler.
set(ITK_OBJCXX_COMPILER_WORKS 0)
if(APPLE)
  try_compile(ITK_OBJCXX_COMPILER_WORKS
              ${ITK_BINARY_DIR}/CMakeTmp
              ${ITK_SOURCE_DIR}/CMake/itkTestObjCxxCompiler.mm
              OUTPUT_VARIABLE ITK_OBJCXX_TRY_COMPILE_OUTPUT)
  if( ITK_OBJCXX_COMPILER_WORKS )
    file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
        "Determining if the Objective-C compiler works passed with "
        "the following output:\n${ITK_OBJCXX_TRY_COMPILE_OUTPUT}\n\n")
  else( ITK_OBJCXX_COMPILER_WORKS )
    file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
      "Determining if the Objective-C compiler works failed with "
      "the following output:\n${ITK_OBJCXX_TRY_COMPILE_OUTPUT}\n\n")
  endif( ITK_OBJCXX_COMPILER_WORKS )
endif(APPLE)

# mingw thread support
if(MINGW)
  set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -mthreads")
  set(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -mthreads")
  set(ITK_REQUIRED_LINK_FLAGS "${ITK_REQUIRED_LINK_FLAGS} -mthreads")
endif(MINGW)

#-----------------------------------------------------------------------------
# Find platform-specific differences in the handling of IEEE floating point
# special values.
include(${ITK_SOURCE_DIR}/CMake/CheckBigBitfield.cmake)
CHECK_BIG_BITFIELD(BIGBITFIELD_VALUE ${ITK_SOURCE_DIR}/CMake)
if(BIGBITFIELD_VALUE)
   set(BIGBITFIELD 1 CACHE INTERNAL "System handles bit-fields larger than 32 bits.")
else(BIGBITFIELD_VALUE)
   set(BIGBITFIELD 0 CACHE INTERNAL "System handles bit-fields larger than 32 bits.")
endif(BIGBITFIELD_VALUE)

include(${ITK_SOURCE_DIR}/CMake/TestQnanhibit.cmake)
TEST_QNANHIBIT(QNANHIBIT_VALUE ${ITK_SOURCE_DIR}/CMake)
if(QNANHIBIT_VALUE)
   set(QNANHIBIT 1 CACHE INTERNAL "The 22nd bit of 32-bit floating-point quiet NaN.")
else(QNANHIBIT_VALUE)
   set(QNANHIBIT 0 CACHE INTERNAL "The 22nd bit of 32-bit floating-point quiet NaN.")
endif(QNANHIBIT_VALUE)

#-----------------------------------------------------------------------------
# turn on exceptions for floating point errors
option(ITK_USE_FLOATINGPOINTEXCEPTIONS
  "Enable Floating Point Exceptions for Testing"
  OFF)
mark_as_advanced(ITK_USE_FLOATINGPOINTEXCEPTIONS)


include(CheckTypeSize)


#-----------------------------------------------------------------------------
# Place all check includes below ( before itkConfigure.h is generated )

# check if the platform has the header file "fenv.h" which has been added as part of the C99 standard
include(CheckIncludeFiles)
CHECK_INCLUDE_FILES(fenv.h HAVE_FENV_H)

# check if the platform has some widely used header files
CHECK_INCLUDE_FILES("unistd.h" HAVE_UNISTD_H)

# check if the platform has the header file "stdint.h" which has been added as part of the C99 standard
CHECK_INCLUDE_FILES("stdint.h" HAVE_STDINT_H)

# every system should have this header
CHECK_INCLUDE_FILES("stddef.h" HAVE_STDDEF_H)
CHECK_INCLUDE_FILES("sys/types.h" HAVE_SYS_TYPES_H)

#-----------------------------------------------------------------------------


#
# See if compiler preprocessor has the __FUNCTION__ directive used by itkExceptionMacro
#
include(${ITK_SOURCE_DIR}/CMake/CheckCPPDirective.cmake)
CHECK_CPP_DIRECTIVE_EXISTS(__FUNCTION__ ITK_CPP_FUNCTION)

#
# See if compiler support template partial specialization
#
try_compile(ITK_SUPPORTS_TEMPLATE_PARTIAL_SPECIALIZATION
      ${ITK_BINARY_DIR}/CMakeTmp
      ${ITK_SOURCE_DIR}/CMake/itkTestTemplatePartialSpecialization.cxx
      OUTPUT_VARIABLE ITK_SUPPORTS_TEMPLATE_PARTIAL_SPECIALIZATION_OUTPUT)

#
# Figure out how the compiler likes to declare templated functions as friends
# of a templated class.
#
include(${ITK_SOURCE_DIR}/CMake/itkTestFriendTemplatedFunction.cmake)


#-----------------------------------------------------------------------------
# check if the platform has c style IO functions that uses wchar_t * filenames
# For some reason I can't manage to get a usable result from CHECK_FUNCTION_EXISTS
try_compile(ITK_SUPPORTS_WCHAR_T_FILENAME_CSTYLEIO
      ${ITK_BINARY_DIR}/CMakeTmp
      ${ITK_SOURCE_DIR}/CMake/itkTestCStyleIOWCharFilename.cxx
      OUTPUT_VARIABLE ITK_SUPPORTS_WCHAR_T_FILENAME_CSTYLEIO_OUTPUT)

#-----------------------------------------------------------------------------
# check if the platform has IO streams constructors that uses wchar_t * filenames
try_compile(ITK_SUPPORTS_WCHAR_T_FILENAME_IOSTREAMS_CONSTRUCTORS
      ${ITK_BINARY_DIR}/CMakeTmp
      ${ITK_SOURCE_DIR}/CMake/itkTestIOStreamsWCharFilenameConstructors.cxx
      OUTPUT_VARIABLE ITK_SUPPORTS_WCHAR_T_FILENAME_IOSTREAMS_CONSTRUCTORS_OUTPUT)

#-----------------------------------------------------------------------------
# check if the platform can compile fdstream.hpp
if(HAVE_UNISTD_H)
  try_compile(ITK_SUPPORTS_FDSTREAM_HPP
    ${ITK_BINARY_DIR}/CMakeTmp
    ${ITK_SOURCE_DIR}/CMake/itkTestFDStream.cxx
    CMAKE_FLAGS -DINCLUDE_DIRECTORIES:STRING=${ITK_SOURCE_DIR}/Utilities/itkExtHdrs
    COMPILE_DEFINITIONS -DITK_HAVE_UNISTD_H=${HAVE_UNISTD_H}
    OUTPUT_VARIABLE ITK_SUPPORTS_FDSTREAM_HPP_OUTPUT)
else(HAVE_UNISTD_H)
  try_compile(ITK_SUPPORTS_FDSTREAM_HPP
    ${ITK_BINARY_DIR}/CMakeTmp
    ${ITK_SOURCE_DIR}/CMake/itkTestFDStream.cxx
    CMAKE_FLAGS -DINCLUDE_DIRECTORIES:STRING=${ITK_SOURCE_DIR}/Utilities/itkExtHdrs
    OUTPUT_VARIABLE ITK_SUPPORTS_FDSTREAM_HPP_OUTPUT)
endif(HAVE_UNISTD_H)

#-----------------------------------------------------------------------------
# Configure the default ITK_DATA_ROOT for the location of ITK Data.
find_path(ITK_DATA_ROOT ITKData.readme ${ITK_SOURCE_DIR}/Testing/Data $ENV{ITK_DATA_ROOT})
if(NOT ITK_DATA_ROOT)
  if(BUILD_TESTING)
    message(WARNING "ITK_DATA_ROOT was not found.  ctest will not run properly.")
  endif()
endif()
mark_as_advanced(ITK_DATA_ROOT)

#-----------------------------------------------------------------------------
# Configure the default ITK_BRAINWEB_DATA_ROOT for the location of ITK Data.
# When this data is available, additional 3D tests are enabled.
find_path(ITK_BRAINWEB_DATA_ROOT brainweb165a10f17.mha
  DOC "Directory with data taken from http://public.kitware.com/pub/itk/Data/BrainWeb/")
mark_as_advanced(ITK_BRAINWEB_DATA_ROOT)

#-----------------------------------------------------------------------------
# Configure the default ITK_MRI_UNC_DATABASE_DATA_ROOT for the location of the
# database of healthy volunteers MRI brain images.
# When this data is available, additional 3D tests are enabled.
find_path(ITK_MRI_UNC_DATABASE_DATA_ROOT LICENSE.txt
  DOC "Directory with data taken from http://public.kitware.com/pub/itk/Data/HealthyVolunteersMRIDatabase/")
mark_as_advanced(ITK_MRI_UNC_DATABASE_DATA_ROOT)

#-----------------------------------------------------------------------------
# Configure the default ITK_OSIRIX_DATA_ROOT for the location of Data from the Osirix web site.
# When this data is available, additional DICOM tests are enabled.
find_path(ITK_OSIRIX_DATA_ROOT CEREBRIX
  DOC "Directory with data taken from http://www.osirix-viewer.com/Downloads.html")
mark_as_advanced(ITK_OSIRIX_DATA_ROOT)

#-----------------------------------------------------------------------------
# Configure the default ITK_VISIBLEHUMAN_DATA_ROOT for the location of Data from the Visible Human Project
# When this data is available, additional Examples are enabled.
find_path(ITK_VISIBLEHUMAN_DATA_ROOT Male/Fullcolor/fullbody
  DOC "Directory with data taken from http://www.nlm.nih.gov/research/visible/getting_data.html")
mark_as_advanced(ITK_VISIBLEHUMAN_DATA_ROOT)


#-----------------------------------------------------------------------------
# This flag is used in particular, to enable some tests that require large memory to run.
# This probably can be discovered at configuration time by CMake. (Check with CMake developers).
set(ITK_COMPUTER_MEMORY_SIZE 1 CACHE STRING "Provide here the size of your RAM Memory in GigaBytes")
mark_as_advanced(ITK_COMPUTER_MEMORY_SIZE)

#-----------------------------------------------------------------------------
# Configure files with settings for use by the build.
configure_file(${ITK_SOURCE_DIR}/itkConfigure.h.in
  ${ITK_BINARY_DIR}/itkConfigure.h)
if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  install(FILES ${ITK_BINARY_DIR}/itkConfigure.h
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24}
    COMPONENT Development)
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)

#-----------------------------------------------------------------------------
# The entire ITK tree should use the same include path, except for the
# Wrapping directory.

# Create the list of include directories needed for ITK header files.
include(${ITK_SOURCE_DIR}/itkIncludeDirectories.cmake)

# This should be the only INCLUDE_DIRECTORIES command in the entire
# tree, except for the Utilities and Wrapping directories.  We need to
# do this in one place to make sure the order is correct.
include_directories(
  ${ITK_INCLUDE_DIRS_BUILD_TREE}
  ${ITK_INCLUDE_DIRS_BUILD_TREE_CXX}
  ${ITK_INCLUDE_DIRS_SYSTEM}
)

#-----------------------------------------------------------------------------
# ITK_USE_SYSTEM_GDCM is provided so that you can use an installed or external
# version of gdcm. If its on, you must specify the variables
# Important: include(${GDCM_USE_FILE}) *has* to be set after the
# INCLUDE_DIRECTORIES that sets all the include paths for ITK, otherwise
# if GDCM was build with VTK support, including it will bring the VTK
# include paths as well, and the wrong DICOMParser will be picked.

option(ITK_USE_SYSTEM_GDCM "Use an outside build of GDCM." OFF)
mark_as_advanced(ITK_USE_SYSTEM_GDCM)
if(ITK_USE_SYSTEM_GDCM)
  find_package(GDCM REQUIRED)
  if(GDCM_FOUND)
    include(${GDCM_USE_FILE})
    if( "${GDCM_MAJOR_VERSION}" LESS 2.0 )
      message(FATAL_ERROR "You need at least version 2.0")
    else( "${GDCM_MAJOR_VERSION}" LESS 2.0 )
      if(ITK_USE_SYSTEM_TIFF)
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
        set(ITK_GDCM_LIBRARIES gdcmMSFF)
      else(ITK_USE_SYSTEM_TIFF)
        set(ITK_GDCM_LIBRARIES gdcmMSFF)
      endif(ITK_USE_SYSTEM_TIFF)
    endif( "${GDCM_MAJOR_VERSION}" LESS 2.0 )
  endif(GDCM_FOUND)
else(ITK_USE_SYSTEM_GDCM)
  set(ITK_GDCM_LIBRARIES gdcmMSFF)
  set(GDCM_INCLUDE_DIR
    ${ITK_SOURCE_DIR}/Utilities/gdcm/Source/MediaStorageAndFileFormat
    )
endif(ITK_USE_SYSTEM_GDCM)

#-----------------------------------------------------------------------------
# Help other projects use ITK.

# Copy the UseITK.cmake file to the binary tree for backward compatability.
configure_file(${ITK_SOURCE_DIR}/UseITK.cmake.in
               ${ITK_BINARY_DIR}/UseITK.cmake COPYONLY IMMEDIATE)

# Save the compiler settings so another project can import them.
include(${CMAKE_ROOT}/Modules/CMakeExportBuildSettings.cmake)
CMAKE_EXPORT_BUILD_SETTINGS(${ITK_BINARY_DIR}/ITKBuildSettings.cmake)

# Save library dependencies.
export_library_dependencies(${ITK_BINARY_DIR}/ITKLibraryDepends.cmake)

# Create the ITKConfig.cmake file containing the ITK configuration.
include(${ITK_SOURCE_DIR}/itkGenerateITKConfig.cmake)

if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  install(FILES
    ${ITK_BINARY_DIR}/ITKBuildSettings.cmake
    ${ITK_BINARY_DIR}/ITKLibraryDepends.cmake
    ${ITK_BINARY_DIR}/UseITK.cmake
    DESTINATION ${ITK_INSTALL_PACKAGE_DIR_CM24}
    COMPONENT Development
    )
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)

#-----------------------------------------------------------------------------
# Add compiler flags ITK needs to work on this platform.  This must be
# done AFTER the call to CMAKE_EXPORT_BUILD_SETTINGS.

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ITK_REQUIRED_C_FLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ITK_REQUIRED_CXX_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
#-----------------------------------------------------------------------------
# The frename-registers option does not work due to a bug in the gnu compiler.
# It must be removed or data errors will be produced and incorrect results
# will be produced.  This is first documented in the gcc4 man page.
if(CMAKE_COMPILER_IS_GNUCXX)
  set(ALL_FLAGS "${CMAKE_C_FLAGS} ${CMAKE_CXX_FLAGS} ${CMAKE_EXE_LINKER_FLAGS} ${CMAKE_SHARED_LINKER_FLAGS} ${CMAKE_MODULE_LINKER_FLAGS}" )
  separate_arguments(ALL_FLAGS)
  foreach(COMP_OPTION ${ALL_FLAGS})
    if("${COMP_OPTION}" STREQUAL "-frename-registers")
      message(FATAL_ERROR "-frename-registers causes runtime bugs.  It must be removed from your compilation options.")
    endif("${COMP_OPTION}" STREQUAL "-frename-registers")
    if("${COMP_OPTION}" STREQUAL "-ffloat-store")
      message(FATAL_ERROR "-ffloat-store causes runtime bugs on gcc 3.2.3 (appearently not on gcc 3.4.3, but the exact criteria is not known).  It must be removed from your compilation options.")
    endif("${COMP_OPTION}" STREQUAL "-ffloat-store")
  endforeach(COMP_OPTION)
endif(CMAKE_COMPILER_IS_GNUCXX)

#-----------------------------------------------------------------------------
# Dispatch the build into the proper subdirectories.
add_subdirectory(Utilities)
add_subdirectory(Code)

add_subdirectory(Wrapping)

if(BUILD_TESTING)
  add_subdirectory(Testing)
endif(BUILD_TESTING)

if(BUILD_EXAMPLES)
  add_subdirectory(Examples)
endif(BUILD_EXAMPLES)

