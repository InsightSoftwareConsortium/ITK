PROJECT(GDCM)
CMAKE_MINIMUM_REQUIRED(VERSION 2.6.2) # Sorry I want the cpack/component
#-----------------------------------------------------------------------------
# New cmake policy thingy
IF(COMMAND CMAKE_POLICY)
  CMAKE_POLICY(SET CMP0003 NEW)
ENDIF(COMMAND CMAKE_POLICY)
MARK_AS_ADVANCED(CMAKE_BACKWARDS_COMPATIBILITY CMAKE_BUILD_TYPE CMAKE_INSTALL_PREFIX)
SET(GDCM_CMAKE_DIR "${GDCM_SOURCE_DIR}/CMake" CACHE INTERNAL "")
SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${GDCM_CMAKE_DIR}")

SET(GDCM_PACKAGE_DESCRIPTION_SUMMARY "GDCM - Grassroots DICOM. GDCM is yet another DICOM library.")
SET(GDCM_PACKAGE_CONTACT "GDCM Developers <gdcm-developers@lists.sourceforge.net>")

# TODO
# http://www.vtk.org/pipermail/vtkusers/2007-May/090968.html
#
# False memory leak reports are caused by VTK dlls loading *before* MFC dlls.
# You have to use the linker's /delayload flag to avoid this issue.
#
# The /delayload flag should be correct by default in the MFC examples if you
# are using CVS VTK... If you are using a previous version of VTK (5.0 or
# earlier) then you will have to figure out a way to link with that flag.
#
# See the CVS version of files in VTK/GUISupport/MFC for details. Or grep the
# VTK source tree for "DELAYLOAD"

#-----------------------------------------------------------------------------
# Disallow insource build since I never test that
STRING(COMPARE EQUAL "${GDCM_SOURCE_DIR}" "${GDCM_BINARY_DIR}" INSOURCE)
GET_FILENAME_COMPONENT(PARENTDIR ${GDCM_BINARY_DIR} PATH)
STRING(COMPARE EQUAL "${GDCM_SOURCE_DIR}" "${PARENTDIR}" INSOURCESUBDIR)
IF(INSOURCE OR INSOURCESUBDIR)
  MESSAGE(FATAL_ERROR "GDCM requires an out of source Build. "
    "Please create a separate binary directory and run CMake there.")
ENDIF(INSOURCE OR INSOURCESUBDIR)

#-----------------------------------------------------------------------------
SET(GDCM_MAJOR_VERSION 2)
SET(GDCM_MINOR_VERSION 1)
SET(GDCM_BUILD_VERSION 0)
SET(GDCM_VERSION
  "${GDCM_MAJOR_VERSION}.${GDCM_MINOR_VERSION}.${GDCM_BUILD_VERSION}")
SET(GDCM_API_VERSION
  "${GDCM_MAJOR_VERSION}.${GDCM_MINOR_VERSION}")
SET(GDCM_LIBRARY_PROPERTIES ${GDCM_LIBRARY_PROPERTIES}
  VERSION "${GDCM_VERSION}"
  SOVERSION "${GDCM_API_VERSION}"
)
#SET(GDCM_EXECUTABLE_PROPERTIES ${GDCM_EXECUTABLE_PROPERTIES}
#  VERSION "${GDCM_MAJOR_VERSION}.${GDCM_MINOR_VERSION}"
#)
SET(GDCM_EXECUTABLE_PROPERTIES)
IF(GDCM_NO_EXECUTABLE_PROPERTIES)
  SET(GDCM_EXECUTABLE_PROPERTIES)
  SET(python_site_package "python/site-packages")
ELSE(GDCM_NO_EXECUTABLE_PROPERTIES)
  SET(python_site_package "")
ENDIF(GDCM_NO_EXECUTABLE_PROPERTIES)

#-----------------------------------------------------------------------------
INCLUDE(${GDCM_SOURCE_DIR}/CMake/UseCopyright.cmake)
CREATE_COPYRIGHT_FILE(${CMAKE_CURRENT_BINARY_DIR}/Copyright.txt)
APPEND_COPYRIGHT(${CMAKE_CURRENT_SOURCE_DIR}/Copyright.txt)
APPEND_COPYRIGHT(${CMAKE_CURRENT_SOURCE_DIR}/CMake/COPYING-CMAKE-SCRIPTS)

#-----------------------------------------------------------------------------
IF(GDCM_MINOR_VERSION MATCHES "[02468]$")
  # Are we building a release branch / tag (read: even number)?
  # By default dashboard are expected to run with Design by Contract on
  # to trigger any of the assert, but on the other hand no user really
  # can figure out they need to change this value
  # So unless the user *specifically* requested a particular cmake_build_type
  # do the work internally and append the NDEBUG def flag (hopefully portable)
  IF(NOT CMAKE_BUILD_TYPE)
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DNDEBUG")
  ENDIF(NOT CMAKE_BUILD_TYPE)
  # Since we are on a release branch, chance is that people don't care about testing
  # let's disable it for them
  SET(GDCM_BUILD_TESTING_DEFAULT OFF)
ELSE(GDCM_MINOR_VERSION MATCHES "[02468]$")
  SET(GDCM_BUILD_TESTING_DEFAULT ON)
ENDIF(GDCM_MINOR_VERSION MATCHES "[02468]$")

#-----------------------------------------------------------------------------
# Disable deprecation warnings for standard C and STL functions in VS2005
# and later
IF(MSVC_VERSION EQUAL 1400 OR MSVC_VERSION GREATER 1400)
  ADD_DEFINITIONS(-D_CRT_SECURE_NO_DEPRECATE -D_CRT_NONSTDC_NO_DEPRECATE -D_CRT_SECURE_NO_WARNINGS)
  ADD_DEFINITIONS(-D_SCL_SECURE_NO_DEPRECATE -D_SCL_SECURE_NO_WARNINGS)
ENDIF(MSVC_VERSION EQUAL 1400 OR MSVC_VERSION GREATER 1400)

#-----------------------------------------------------------------------------
# Build shared lib by default
OPTION(GDCM_BUILD_SHARED_LIBS "Build GDCM with shared libraries." OFF)
SET(BUILD_SHARED_LIBS ${GDCM_BUILD_SHARED_LIBS})

IF(BUILD_SHARED_LIBS)
IF(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} GREATER 2.6)
  SET(NAMELINK_ONLY NAMELINK_ONLY)
  SET(NAMELINK_SKIP NAMELINK_SKIP)
ELSE(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} GREATER 2.6)
  SET(NAMELINK_ONLY)
  SET(NAMELINK_SKIP)
ENDIF(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} GREATER 2.6)
ENDIF(BUILD_SHARED_LIBS)


#-----------------------------------------------------------------------------
SET (EXECUTABLE_OUTPUT_PATH ${GDCM_BINARY_DIR}/bin CACHE PATH "Single output directory for building all executables.")
SET (LIBRARY_OUTPUT_PATH ${GDCM_BINARY_DIR}/bin CACHE PATH "Single output directory for building all libraries.")
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)

#-----------------------------------------------------------------------------
# Adding GDCM_DATA_ROOT
FIND_PATH(GDCM_DATA_ROOT gdcm-ACR-LibIDO.acr
  ${GDCM_SOURCE_DIR}/../gdcmData
  $ENV{GDCM_DATA_ROOT}
  $ENV{PUB_DICT_PATH}/../../gdcmData
  )
MARK_AS_ADVANCED(GDCM_DATA_ROOT)
# You can define a path where you extra the famous D. Clunie spacing dataset
# http://www.dclunie.com/images/pixelspacingtestimages.zip
# for example:
# find $HOME/pixelspacingtestimages
# $HOME/pixelspacingtestimages
# $HOME/pixelspacingtestimages/DISCIMG
# $HOME/pixelspacingtestimages/DISCIMG/DICOMDIR
# $HOME/pixelspacingtestimages/DISCIMG/IMAGES
# $HOME/pixelspacingtestimages/DISCIMG/IMAGES/DXIMAGE
# $HOME/pixelspacingtestimages/DISCIMG/IMAGES/MGIMAGE
# $HOME/pixelspacingtestimages/DISCIMG/IMAGES/CRIMAGE
MARK_AS_ADVANCED(GDCM_PIXEL_SPACING_DATA_ROOT)

#-----------------------------------------------------------------------------
FIND_PATH(GDCM_DATA_EXTRA_ROOT gdcmData.tar.gz
  ${GDCM_SOURCE_DIR}/../gdcmDataExtra
  $ENV{GDCM_DATA_EXTRA_ROOT}
  $ENV{PUB_DICT_PATH}/../../gdcmDataExtra
  )
MARK_AS_ADVANCED(GDCM_DATA_EXTRA_ROOT)

# Define a temp directory in which we can output stuff
SET(GDCM_TEMP_DIRECTORY "${GDCM_BINARY_DIR}/Testing/Temporary" CACHE PATH "Path to a valid temp directory")
MARK_AS_ADVANCED(GDCM_TEMP_DIRECTORY)

#-----------------------------------------------------------------------------
INCLUDE (${CMAKE_ROOT}/Modules/CheckIncludeFile.cmake)
INCLUDE (${CMAKE_ROOT}/Modules/CheckIncludeFiles.cmake)
# Check if header file exists and add it to the list.
MACRO(CHECK_INCLUDE_FILE_CONCAT FILE VARIABLE)
  CHECK_INCLUDE_FILES("${UUID_INCLUDES};${FILE}" ${VARIABLE})
  IF(${VARIABLE})
    SET(UUID_INCLUDES ${UUID_INCLUDES} ${FILE})
  ENDIF(${VARIABLE})
ENDMACRO(CHECK_INCLUDE_FILE_CONCAT)

CHECK_INCLUDE_FILE("stdint.h"       GDCM_HAVE_STDINT_H)
IF(UNIX) #Avoid polluting Win32 cmakecache
  CHECK_INCLUDE_FILE("inttypes.h"     GDCM_HAVE_INTTYPES_H)
ENDIF(UNIX)

#INCLUDE(${GDCM_SOURCE_DIR}/CMake/gdcmPlatformCxxTests.cmake)
#
#GDCM_PLATFORM_CXX_TEST(GDCM_CXX_HAS_FUNCTION
#    "Checking whether compiler has __FUNCTION__" DIRECT)


#-----------------------------------------------------------------------------
# Build the main lib...
IF(MSVC)
  INCLUDE_DIRECTORIES(
    "${GDCM_SOURCE_DIR}/Utilities/C99"
  )
  # Process the install rules from C99
  SUBDIRS(Utilities/C99)
ENDIF(MSVC)

# --------------------------------------------------------------------------
# Configure the export configuration

# You will also need to define a value for the following variables:
# GDCM_INSTALL_BIN_DIR          - binary dir (executables)
# GDCM_INSTALL_LIB_DIR          - library dir (libs)
# GDCM_INSTALL_DATA_DIR         - share dir (say, examples, data, etc)
# GDCM_INSTALL_INCLUDE_DIR      - include dir (headers)
# GDCM_INSTALL_PACKAGE_DIR      - package/export configuration files
# GDCM_VTK_INSTALL_PACKAGE_DIR  - VTK package/export configuration files
# GDCM_INSTALL_NO_DEVELOPMENT   - do not install development files
# GDCM_INSTALL_NO_RUNTIME       - do not install runtime files
# GDCM_INSTALL_NO_DOCUMENTATION - do not install documentation files

# --------------------------------------------------------------------------
# Install directories

STRING(TOLOWER ${PROJECT_NAME} projectname)
SET(subdir "${projectname}-${GDCM_MAJOR_VERSION}.${GDCM_MINOR_VERSION}")

IF(NOT GDCM_INSTALL_BIN_DIR)
  SET(GDCM_INSTALL_BIN_DIR "bin")
ENDIF(NOT GDCM_INSTALL_BIN_DIR)

IF(NOT GDCM_INSTALL_LIB_DIR)
  #SET(GDCM_INSTALL_LIB_DIR "lib/${PROJECT_NAME}")
  SET(GDCM_INSTALL_LIB_DIR "lib")
ENDIF(NOT GDCM_INSTALL_LIB_DIR)

IF(NOT GDCM_INSTALL_DATA_DIR)
  SET(GDCM_INSTALL_DATA_DIR "share/${subdir}")
ENDIF(NOT GDCM_INSTALL_DATA_DIR)

IF(NOT GDCM_INSTALL_INCLUDE_DIR)
  SET(GDCM_INSTALL_INCLUDE_DIR "include/${subdir}")
ENDIF(NOT GDCM_INSTALL_INCLUDE_DIR)

IF(NOT GDCM_INSTALL_DOC_DIR)
  SET(GDCM_INSTALL_DOC_DIR "share/doc/${subdir}")
ENDIF(NOT GDCM_INSTALL_DOC_DIR)

IF(NOT GDCM_INSTALL_MAN_DIR)
  SET(GDCM_INSTALL_MAN_DIR "share/man")
ENDIF(NOT GDCM_INSTALL_MAN_DIR)

IF(NOT GDCM_INSTALL_PACKAGE_DIR)
  SET(GDCM_INSTALL_PACKAGE_DIR ${GDCM_INSTALL_LIB_DIR}/${subdir}
    CACHE INTERNAL "")
ENDIF(NOT GDCM_INSTALL_PACKAGE_DIR)

IF(NOT GDCM_VTK_INSTALL_PACKAGE_DIR)
  SET(GDCM_VTK_INSTALL_PACKAGE_DIR ${VTK_INSTALL_PACKAGE_DIR})
ENDIF(NOT GDCM_VTK_INSTALL_PACKAGE_DIR)

IF(NOT GDCM_INSTALL_NO_DEVELOPMENT)
  SET(GDCM_INSTALL_NO_DEVELOPMENT 0)
ENDIF(NOT GDCM_INSTALL_NO_DEVELOPMENT)

IF(NOT GDCM_INSTALL_NO_RUNTIME)
  SET(GDCM_INSTALL_NO_RUNTIME 0)
ENDIF(NOT GDCM_INSTALL_NO_RUNTIME)

IF(NOT GDCM_INSTALL_NO_DOCUMENTATION)
  SET(GDCM_INSTALL_NO_DOCUMENTATION 0)
ENDIF(NOT GDCM_INSTALL_NO_DOCUMENTATION)

SET(GDCM_INSTALL_NO_LIBRARIES)
IF(GDCM_BUILD_SHARED_LIBS)
  IF(GDCM_INSTALL_NO_RUNTIME AND GDCM_INSTALL_NO_DEVELOPMENT)
    SET(GDCM_INSTALL_NO_LIBRARIES 1)
  ENDIF(GDCM_INSTALL_NO_RUNTIME AND GDCM_INSTALL_NO_DEVELOPMENT)
ELSE(GDCM_BUILD_SHARED_LIBS)
  IF(GDCM_INSTALL_NO_DEVELOPMENT)
    SET(GDCM_INSTALL_NO_LIBRARIES 1)
  ENDIF(GDCM_INSTALL_NO_DEVELOPMENT)
ENDIF(GDCM_BUILD_SHARED_LIBS)

#-----------------------------------------------------------------------------
#System stuff, mainly for packager or paranoid people with up-to-date lib moto
OPTION(GDCM_USE_SYSTEM_ZLIB "Use system zlib" OFF)
#OPTION(GDCM_USE_SYSTEM_POLARSSL "Use system polarssl (xyssl)" OFF)
OPTION(GDCM_USE_SYSTEM_OPENSSL  "Use system OpenSSL" OFF)
IF(UNIX)
  # Will search for the uuid_generate symbols.
  # Can be in libSystem.dylib or libuuid.so
  OPTION(GDCM_USE_SYSTEM_UUID "Use system uuid" OFF)
ENDIF(UNIX)
OPTION(GDCM_USE_SYSTEM_EXPAT "Use system expat" OFF)
OPTION(GDCM_USE_SYSTEM_LJPEG "Use system ljpeg (ijg lib)" OFF)
OPTION(GDCM_USE_SYSTEM_OPENJPEG "Use system openjpeg (1.x)" OFF)
MARK_AS_ADVANCED(GDCM_USE_SYSTEM_ZLIB GDCM_USE_SYSTEM_UUID GDCM_USE_SYSTEM_EXPAT GDCM_USE_SYSTEM_OPENSSL)
MARK_AS_ADVANCED(GDCM_USE_SYSTEM_OPENJPEG GDCM_USE_SYSTEM_LJPEG)
OPTION(GDCM_USE_SYSTEM_POPPLER "Use system poppler (pdf)" OFF)
IF(GDCM_USE_SYSTEM_POPPLER)
  FIND_PACKAGE(Poppler REQUIRED)
ENDIF(GDCM_USE_SYSTEM_POPPLER)
MARK_AS_ADVANCED(GDCM_USE_SYSTEM_POPPLER)

OPTION(GDCM_USE_SYSTEM_PODOFO "Use system podofo (pdf)" OFF)
IF(GDCM_USE_SYSTEM_PODOFO)
  #FIND_PACKAGE(PODOFO REQUIRED)
  IF(GDCM_USE_SYSTEM_POPPLER)
    MESSAGE(FATAL_ERROR "Choose only one podofo vs poppler")
  ENDIF(GDCM_USE_SYSTEM_POPPLER)
ENDIF(GDCM_USE_SYSTEM_PODOFO)
MARK_AS_ADVANCED(GDCM_USE_SYSTEM_PODOFO)

IF(GDCM_USE_SYSTEM_LJPEG)
  FIND_PACKAGE(LJPEG REQUIRED)
  SET(GDCM_LJPEG_LIBRARIES ${LJPEG_LIBRARIES})
ELSE(GDCM_USE_SYSTEM_LJPEG)
  SET(GDCM_LJPEG_LIBRARIES gdcmjpeg8 gdcmjpeg12 gdcmjpeg16)
ENDIF(GDCM_USE_SYSTEM_LJPEG)

IF(GDCM_USE_SYSTEM_OPENJPEG)
  # For some reason I cannot specify the version I want.
  # FIND_PACKAGE(OpenJPEG 2.0 REQUIRED)
  # oh well we should handle both 1.0 and 2.0 anyway...
  FIND_PACKAGE(OpenJPEG REQUIRED)
  SET(GDCM_OPENJPEG_LIBRARIES ${OPENJPEG_LIBRARIES})
ELSE(GDCM_USE_SYSTEM_OPENJPEG)
  SET(GDCM_OPENJPEG_LIBRARIES gdcmopenjpeg)
ENDIF(GDCM_USE_SYSTEM_OPENJPEG)

# Very advanced user option:
# This will cause building of the broken JPEG library released by the Standford PVRG group:
OPTION(GDCM_USE_PVRG "Use pvrg lib, only turn it on if you know what you are doing." OFF)
MARK_AS_ADVANCED(GDCM_USE_PVRG)

OPTION(GDCM_USE_KAKADU "Use kakadu lib, only turn it on if you know what you are doing." OFF)
MARK_AS_ADVANCED(GDCM_USE_KAKADU)

IF(GDCM_USE_PVRG)
  OPTION(GDCM_USE_SYSTEM_PVRG "Use system PVRG" OFF)
  MARK_AS_ADVANCED(GDCM_USE_SYSTEM_PVRG)
  IF(GDCM_USE_SYSTEM_PVRG)
    FIND_PACKAGE(PVRGJPEG REQUIRED)
  ENDIF(GDCM_USE_SYSTEM_PVRG)
ENDIF(GDCM_USE_PVRG)

IF(GDCM_USE_KAKADU)
  OPTION(GDCM_USE_SYSTEM_KAKADU "Use system KAKADU " ON)
  MARK_AS_ADVANCED(GDCM_USE_SYSTEM_KAKADU)
  IF(GDCM_USE_SYSTEM_KAKADU)
    FIND_PACKAGE(KAKADU REQUIRED)
  ELSE(GDCM_USE_SYSTEM_KAKADU)
    MESSAGE(FATAL_ERROR "Not Implemented")
  ENDIF(GDCM_USE_SYSTEM_KAKADU)
ENDIF(GDCM_USE_KAKADU)

IF(GDCM_USE_SYSTEM_ZLIB)
  # If user say so, then this is a requirement !
  FIND_PACKAGE(ZLIB REQUIRED)
  SET(GDCM_ZLIB_LIBRARIES ${ZLIB_LIBRARIES})
ELSE(GDCM_USE_SYSTEM_ZLIB)
  SET(GDCM_ZLIB_LIBRARIES "gdcmzlib")
ENDIF(GDCM_USE_SYSTEM_ZLIB)

#IF(GDCM_USE_SYSTEM_POLARSSL)
#  # If user say so, then this is a requirement !
#  FIND_PACKAGE(POLARSSL REQUIRED)
#  SET(GDCM_POLARSSL_LIBRARIES ${POLARSSL_LIBRARIES})
#
#ELSE(GDCM_USE_SYSTEM_POLARSSL)
#  SET(GDCM_POLARSSL_LIBRARIES "gdcmpolarssl")
#ENDIF(GDCM_USE_SYSTEM_POLARSSL)

IF(GDCM_USE_SYSTEM_OPENSSL)
  FIND_PACKAGE(OpenSSL REQUIRED)
ENDIF(GDCM_USE_SYSTEM_OPENSSL)

IF(GDCM_USE_SYSTEM_UUID)
  # If user say so, then this is a requirement !
  FIND_PACKAGE(UUID REQUIRED)
  SET(GDCM_UUID_LIBRARIES ${UUID_LIBRARIES})
ELSE(GDCM_USE_SYSTEM_UUID)
  SET(GDCM_UUID_LIBRARIES "gdcmuuid")
ENDIF(GDCM_USE_SYSTEM_UUID)

IF(GDCM_USE_SYSTEM_EXPAT)
  # If user say so, then this is a requirement !
  FIND_PACKAGE(EXPAT REQUIRED)
  SET(GDCM_EXPAT_LIBRARIES ${EXPAT_LIBRARIES})
ELSE(GDCM_USE_SYSTEM_EXPAT)
  SET(GDCM_EXPAT_LIBRARIES "gdcmexpat")
ENDIF(GDCM_USE_SYSTEM_EXPAT)


#-----------------------------------------------------------------------------
OPTION(GDCM_BUILD_EXAMPLES "Build GDCM examples." OFF)
SET(BUILD_EXAMPLES ${GDCM_BUILD_EXAMPLES})

#-----------------------------------------------------------------------------
# Add the testing directories
OPTION(GDCM_BUILD_TESTING "Build testing." ${GDCM_BUILD_TESTING_DEFAULT})
SET(BUILD_TESTING ${GDCM_BUILD_TESTING} CACHE BOOL "" FORCE)
# Hide BUILD_TESTING as user tend to always turn all options on and then complains when something
# does not work 'by default'
MARK_AS_ADVANCED(BUILD_TESTING) # GDCM_BUILD_TESTING)

#-----------------------------------------------------------------------------
IF(GDCM_TESTING_USE_LC_NUMERIC)
  SET(CMAKE_TESTDRIVER_BEFORE_TESTMAIN
    "setlocale(LC_ALL,\"fr_FR.UTF-8\");std::locale::global(std::locale(\"fr_FR.UTF-8\"));"
    )
ENDIF(GDCM_TESTING_USE_LC_NUMERIC)

#-----------------------------------------------------------------------------
# Python install
#FIND_PACKAGE(PythonInterp REQUIRED)
#MACRO(GET_PYTHON_SITE_PACKAGE dir)
#  EXECUTE_PROCESS(
#  COMMAND ${PYTHON_EXECUTABLE} "-c" "from distutils import sysconfig; print sysconfig.get_python_lib()"
#  #WORKING_DIRECTORY @LIBRARY_OUTPUT_PATH@
#  RESULT_VARIABLE import_res
#  OUTPUT_VARIABLE import_output
#  ERROR_VARIABLE  import_error
#    OUTPUT_STRIP_TRAILING_WHITESPACE
#  )
#  #SET(dir ${import_output})
#ENDMACRO(GET_PYTHON_SITE_PACKAGE)
#
#
#GET_PYTHON_SITE_PACKAGE(python_site_package)
#IF(import_output)
#STRING(LENGTH ${import_output} len)
## let's remove the "/usr/lib" part...
#MATH(EXPR fileend "${len} - 9")
#STRING(SUBSTRING ${import_output} 9 ${fileend} dummy1)
#IF(UNIX)
#  SET(python_site_package ${dummy1})
#  MESSAGE(${python_site_package})
#ENDIF(UNIX)
#ENDIF(import_output)

# On unix one have: "python2.4/site-packages"
# while on Win32: "c:/Python24/Lib/site-packages/"
# give up for now and place python modules stuff in a general 'python' subdir

# Typical runtime env should be then
# (assuming CMAKE_INSTALL_PREFIX:PATH=/tmp/local)
# One would do:
#   export PYTHONPATH=/tmp/local/lib/python/site-packages/gdcm-2.1
#   export LD_LIBRARY_PATH=/tmp/local/lib/
#   python
#   > import gdcm


#-----------------------------------------------------------------------------
# Wrapping
OPTION(GDCM_WRAP_PYTHON "build python wrapping" OFF)
IF(GDCM_BUILD_WRAPPING)
  MESSAGE(STATUS "GDCM_BUILD_WRAPPING option is deprecate, please specify explicitely which target language to wrap")
  SET(GDCM_WRAP_PYTHON ON CACHE BOOL "(deprecated) build python wrapping" FORCE)
ENDIF(GDCM_BUILD_WRAPPING)
OPTION(GDCM_WRAP_PHP "php wrapping (experimental !)" OFF)
OPTION(GDCM_WRAP_JAVA "build java wrapping" OFF)
OPTION(GDCM_WRAP_CSHARP "build csharp wrapping" OFF)
MARK_AS_ADVANCED(GDCM_WRAP_PHP) #GDCM_WRAP_JAVA) # GDCM_WRAP_CSHARP)
MARK_AS_ADVANCED(GDCM_USE_RLE)
MARK_AS_ADVANCED(GDCM_USE_ACTIVIZ)
#MARK_AS_ADVANCED(GDCM_TEST_BOOSTRAP)
OPTION(GDCM_USE_JPEGLS "Build GDCM with JPEG-LS support" ON)
MARK_AS_ADVANCED(GDCM_USE_JPEGLS)
IF(GDCM_WRAP_CSHARP)
FIND_PACKAGE(CSharp REQUIRED)
INCLUDE(${CSharp_USE_FILE})
#  cmake_minimum_required(VERSION 2.6)
#  enable_language(CSharp)
ENDIF(GDCM_WRAP_CSHARP)

SET(GDCM_LIBRARY_DIR ${LIBRARY_OUTPUT_PATH}/${CMAKE_CFG_INTDIR})
SET(GDCM_EXECUTABLE_DIR ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR})

SUBDIRS(Wrapping)
IF(GDCM_WRAP_CSHARP)
  ADD_SUBDIRECTORY(Wrapping/Csharp)
ENDIF(GDCM_WRAP_CSHARP)

# After Wrapping please
IF(BUILD_EXAMPLES)
  SUBDIRS(Examples)
ENDIF(BUILD_EXAMPLES)

#-----------------------------------------------------------------------------
# Special CMake Module required when doing Python Testing
IF(BUILD_TESTING AND GDCM_WRAP_PYTHON)
  INCLUDE(${GDCM_SOURCE_DIR}/CMake/UsePythonTest.cmake)
ENDIF(BUILD_TESTING AND GDCM_WRAP_PYTHON)

# Special CMake Module required when doing C# Testing
IF(BUILD_TESTING AND GDCM_WRAP_CSHARP)
  INCLUDE(${GDCM_SOURCE_DIR}/CMake/UseCSharpTest.cmake)
ENDIF(BUILD_TESTING AND GDCM_WRAP_CSHARP)

# Special CMake Module required when doing Java Testing
IF(BUILD_TESTING AND GDCM_WRAP_JAVA)
  INCLUDE(${GDCM_SOURCE_DIR}/CMake/UseJavaTest.cmake)
ENDIF(BUILD_TESTING AND GDCM_WRAP_JAVA)

#-----------------------------------------------------------------------------
# Need pthread for the following class:
CHECK_INCLUDE_FILE("pthread.h"      GDCM_HAVE_PTHREAD_H)

# Big endian thing:
INCLUDE (${CMAKE_ROOT}/Modules/TestBigEndian.cmake)
TEST_BIG_ENDIAN(GDCM_WORDS_BIGENDIAN)


SUBDIRS(
  Utilities
  Source
  )

IF(BUILD_TESTING)
  CONFIGURE_FILE(${GDCM_SOURCE_DIR}/CMake/CTestCustom.ctest.in
      ${GDCM_BINARY_DIR}/CMake/CTestCustom.ctest @ONLY)
  FILE(WRITE ${GDCM_BINARY_DIR}/CTestCustom.cmake
    "INCLUDE(\"${GDCM_BINARY_DIR}/CMake/CTestCustom.ctest\")\n")
  MARK_AS_ADVANCED(DART_TESTING_TIMEOUT)
  ENABLE_TESTING()
  INCLUDE(CTest)
  SUBDIRS(Testing)
  IF(NOT GDCM_DATA_ROOT)
    MESSAGE("If you want to build the test suite, you must set GDCM_DATA_ROOT (advanced option) "
    "to the full path name of the gdcmData directory; if you don't want, disable GDCM_BUILD_TESTING.\n"
    "What is gdcmData? Please read: http://sourceforge.net/apps/mediawiki/gdcm/index.php?title=General_questions#What_is_gdcmData_.3F")
  ENDIF(NOT GDCM_DATA_ROOT)
ENDIF(BUILD_TESTING)


#-----------------------------------------------------------------------------
OPTION(GDCM_DOCUMENTATION "Build source documentation using doxygen." OFF)
IF(GDCM_DOCUMENTATION)
  OPTION(GDCM_PDF_DOCUMENTATION "Build source doxygen using doxygen as pdf" ON)
  MARK_AS_ADVANCED(GDCM_PDF_DOCUMENTATION)
ENDIF(GDCM_DOCUMENTATION)


#-----------------------------------------------------------------------------
OPTION(GDCM_USE_VTK "vtk bridge ?" OFF)
IF(GDCM_USE_VTK AND GDCM_WRAP_CSHARP)
  OPTION(GDCM_USE_ACTIVIZ "vtk/Activiz bridge ?" OFF)
ENDIF(GDCM_USE_VTK AND GDCM_WRAP_CSHARP)

IF(GDCM_USE_VTK)
  OPTION(GDCM_USE_PARAVIEW "paraview plugin ?" OFF)
  SUBDIRS(Utilities/VTK)
ENDIF(GDCM_USE_VTK)

#-----------------------------------------------------------------------------
#OPTION(GDCM_USE_ITK "itk bridge ?" OFF)
#MARK_AS_ADVANCED(GDCM_USE_ITK)
IF(GDCM_USE_ITK_I_UNDERSTAND_WHAT_I_AM_DOING)
  #MESSAGE(FATAL_ERROR "Do not use, instead prefer: ITK_USE_SYSTEM_GDCM, that was added after ITK 3.8")
  SUBDIRS(Utilities/Insight)
ENDIF(GDCM_USE_ITK_I_UNDERSTAND_WHAT_I_AM_DOING)

#-----------------------------------------------------------------------------
OPTION(GDCM_USE_WXWIDGETS "wxWidgets bridge ?" OFF)
MARK_AS_ADVANCED(GDCM_USE_WXWIDGETS)
IF(GDCM_USE_WXWIDGETS)
  SUBDIRS(Utilities/wxWidgets)
ENDIF(GDCM_USE_WXWIDGETS)

#-----------------------------------------------------------------------------
OPTION(GDCM_BUILD_APPLICATIONS "apps ?" OFF)
SET(BUILD_APPLICATIONS ${GDCM_BUILD_APPLICATIONS})
IF(BUILD_APPLICATIONS)
  SUBDIRS(Applications)
ENDIF(BUILD_APPLICATIONS)

#-----------------------------------------------------------------------------
# CPack stuff
IF(EXISTS "${CMAKE_ROOT}/Modules/CPack.cmake")
  IF(EXISTS "${CMAKE_ROOT}/Modules/InstallRequiredSystemLibraries.cmake")
    SET(CMAKE_INSTALL_MFC_LIBRARIES 0)
    SET(CMAKE_INSTALL_DEBUG_LIBRARIES 0)
    INCLUDE(${CMAKE_ROOT}/Modules/InstallRequiredSystemLibraries.cmake)
  ENDIF(EXISTS "${CMAKE_ROOT}/Modules/InstallRequiredSystemLibraries.cmake")

  SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY ${GDCM_PACKAGE_DESCRIPTION_SUMMARY})
  SET(CPACK_PACKAGE_VENDOR "GDCM")
  SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_BINARY_DIR}/Copyright.txt")
  SET(CPACK_RESOURCE_FILE_LICENSE    "${CMAKE_CURRENT_BINARY_DIR}/Copyright.txt")
  SET(CPACK_PACKAGE_VERSION_MAJOR "${GDCM_MAJOR_VERSION}")
  SET(CPACK_PACKAGE_VERSION_MINOR "${GDCM_MINOR_VERSION}")
  SET(CPACK_PACKAGE_VERSION_PATCH "${GDCM_BUILD_VERSION}")
  SET(CPACK_PACKAGE_INSTALL_DIRECTORY "GDCM ${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}")
  SET(CPACK_SOURCE_PACKAGE_FILE_NAME "gdcm-${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")

  #SET(CPACK_PACKAGE_INSTALL_DIRECTORY "CMake ${CMake_VERSION_MAJOR}.${CMake_VERSION_MINOR}")
  IF(WIN32 AND NOT UNIX)
    #STRING(REGEX REPLACE "/" "\\\\\\\\" CPACK_PACKAGE_ICON "${CMAKE_CURRENT_SOURCE_DIR}/Utilities/Release/GDCMInstall.bmp")

    # There is a bug in NSI that does not handle full unix paths properly. Make
    # sure there is at least one set of four (4) backlasshes.
    #SET(CPACK_PACKAGE_ICON "${CMake_SOURCE_DIR}/Utilities/Release\\\\InstallIcon.bmp")
    SET(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\gdcmviewer.exe")
    SET(CPACK_NSIS_DISPLAY_NAME "${CPACK_PACKAGE_INSTALL_DIRECTORY}")
    SET(CPACK_NSIS_HELP_LINK "http://gdcm.sourceforge.net")
    SET(CPACK_NSIS_URL_INFO_ABOUT "http://gdcm.sourceforge.net")
    SET(CPACK_NSIS_MODIFY_PATH ON)
    IF(${CMAKE_INSTALL_PREFIX} MATCHES ${CPACK_PACKAGE_INSTALL_DIRECTORY})
    ELSE(${CMAKE_INSTALL_PREFIX} MATCHES ${CPACK_PACKAGE_INSTALL_DIRECTORY})
    STRING(REPLACE ${PROJECT_NAME} ${CPACK_PACKAGE_INSTALL_DIRECTORY} dummy ${CMAKE_INSTALL_PREFIX} )
    SET(CMAKE_INSTALL_PREFIX ${dummy} CACHE PATH "" FORCE)
    ENDIF(${CMAKE_INSTALL_PREFIX} MATCHES ${CPACK_PACKAGE_INSTALL_DIRECTORY})

  ELSE(WIN32 AND NOT UNIX)
    # http://www.vtk.org/Wiki/CMake:CPackConfiguration
    # List of files to be stripped. Starting with CMake 2.6.0 CPACK_STRIP_FILES will be a boolean variable which enables
    # stripping of all files (a list of files evaluates to TRUE in CMake, so this change is compatible).
    SET(CPACK_STRIP_FILES "bin/gdcmdump;bin/gdcmconv;bin/gdcmscanner;bin/gdcminfo;bin/gdcmraw;bin/gdcmimg;bin/gdcmtar")
    SET(CPACK_SOURCE_STRIP_FILES "")
    SET(CPACK_PACKAGE_EXECUTABLES "gdcmviewer" "VIEWER")

  ENDIF(WIN32 AND NOT UNIX)

  # cygwin stff also copied from cmake
  #IF(NOT DEFINED CPACK_PACKAGE_FILE_NAME)
  #  # if the CPACK_PACKAGE_FILE_NAME is not defined by the cache
  #  # default to source package - system, on cygwin system is not
  #  # needed
  #  IF(CYGWIN)
  #    SET(CPACK_PACKAGE_FILE_NAME "${CPACK_SOURCE_PACKAGE_FILE_NAME}")
  #  ELSE(CYGWIN)
  #    SET(CPACK_PACKAGE_FILE_NAME
  #      "${CPACK_SOURCE_PACKAGE_FILE_NAME}-${CPACK_SYSTEM_NAME}")
  #  ENDIF(CYGWIN)
  #ENDIF(NOT DEFINED CPACK_PACKAGE_FILE_NAME)

  #Cygwin stuff copied from cmake
  IF(NOT DEFINED CPACK_SYSTEM_NAME)
    # make sure package is not Cygwin-unknown, for Cygwin just
    # cygwin is good for the system name
    IF("${CMAKE_SYSTEM_NAME}" STREQUAL "CYGWIN")
      SET(CPACK_SYSTEM_NAME Cygwin)
    ELSE("${CMAKE_SYSTEM_NAME}" STREQUAL "CYGWIN")
      #SET(CMAKE_SYSTEM_PROCESSOR "x86_64")
      #SET(CPACK_GENERATOR "TGZ;TBZ2")
      #SET(CPACK_SOURCE_GENERATOR "TGZ;TBZ2")
      SET(CPACK_SYSTEM_NAME ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR})
    ENDIF("${CMAKE_SYSTEM_NAME}" STREQUAL "CYGWIN")
  ENDIF(NOT DEFINED CPACK_SYSTEM_NAME)

  # Need to set the architecture for debian package
  SET(CPACK_PACKAGE_CONTACT ${GDCM_PACKAGE_CONTACT})
  SET(CPACK_DEBIAN_PACKAGE_DEPENDS "libc6, libstdc++6 (>= 4.0.2-4), libuuid1, zlib1g (>= 1:1.2.1), libgcc1 (>= 1:4.0.2), libexpat1, swig") # bug: missing dep to python...
  SET(CPACK_DEBIAN_PACKAGE_SUGGESTS "dcmtk")
  SET(CPACK_SOURCE_IGNORE_FILES "/debian/" "/CVS/" "/\\\\.svn/" "TODO")
  SET(CPACK_IGNORE_FILES ${CPACK_SOURCE_IGNORE_FILES})

  # List executables
  #SET(CPACK_PACKAGE_EXECUTABLES "gdcmviewer" "VIEWER")
  IF(CYGWIN)
    SET(CPACK_CYGWIN_PATCH_NUMBER 1)
    SET(CPACK_CYGWIN_BUILD_SCRIPT
    "${GDCM_BINARY_DIR}/@CPACK_PACKAGE_FILE_NAME@-@CPACK_CYGWIN_PATCH_NUMBER@.sh")
    SET(CPACK_CYGWIN_PATCH_FILE
  "${GDCM_BINARY_DIR}/@CPACK_PACKAGE_FILE_NAME@-@CPACK_CYGWIN_PATCH_NUMBER@.patch")
    CONFIGURE_FILE(${GDCM_SOURCE_DIR}/CMake/Release/cygwin-patch.diff.in ${CPACK_CYGWIN_PATCH_FILE})
    CONFIGURE_FILE(${GDCM_SOURCE_DIR}/CMake/Release/cygwin-package.sh.in ${CPACK_CYGWIN_BUILD_SCRIPT})
  ENDIF(CYGWIN)

  # Tell CPack all of the components to install. The "ALL"
  # refers to the fact that this is the set of components that
  # will be included when CPack is instructed to put everything
  # into the binary installer (the default behavior).
  set(components)
  if(GDCM_BUILD_APPLICATIONS)
    list(APPEND components Applications)
  endif(GDCM_BUILD_APPLICATIONS)
  list(APPEND components Libraries)
  list(APPEND components Headers)
  if(GDCM_WRAP_PYTHON)
    list(APPEND components PythonModule)
  endif(GDCM_WRAP_PYTHON)
  if(GDCM_WRAP_CSHARP)
    list(APPEND components CSharpModule)
  endif(GDCM_WRAP_CSHARP)
  if(GDCM_WRAP_JAVA)
    list(APPEND components JavaModule)
  endif(GDCM_WRAP_JAVA)
  if(GDCM_USE_VTK)
    foreach(comp ${components})
      list(APPEND components VTK${comp})
    endforeach(comp)
  endif(GDCM_USE_VTK)
  if(GDCM_USE_PARAVIEW)
    list(APPEND components ParaViewModule)
  endif(GDCM_USE_PARAVIEW)
  # Ok this is the complete list of all components:
  set(CPACK_COMPONENTS_ALL ${components})

  # Set the displayed names for each of the components to install.
  # These will be displayed in the list of components inside the installer.
  set(CPACK_COMPONENT_APPLICATIONS_DISPLAY_NAME "GDCM Applications")
  set(CPACK_COMPONENT_LIBRARIES_DISPLAY_NAME "GDCM Libraries")
  set(CPACK_COMPONENT_HEADERS_DISPLAY_NAME "GDCM C/C++ Headers")
  set(CPACK_COMPONENT_PYTHONMODULE_DISPLAY_NAME "GDCM Python Module")
  set(CPACK_COMPONENT_CSHARPMODULE_DISPLAY_NAME "GDCM C# Module")
  set(CPACK_COMPONENT_JAVAMODULE_DISPLAY_NAME "GDCM Java Module")
  set(CPACK_COMPONENT_VTKAPPLICATIONS_DISPLAY_NAME "VTK/GDCM Applications")
  set(CPACK_COMPONENT_VTKLIBRARIES_DISPLAY_NAME "VTK/GDCM Libraries")
  set(CPACK_COMPONENT_VTKHEADERS_DISPLAY_NAME "VTK/GDCM C/C++ Headers")
  set(CPACK_COMPONENT_VTKPYTHONMODULE_DISPLAY_NAME "VTK/GDCM Python Module")
  set(CPACK_COMPONENT_VTKCSHARPMODULE_DISPLAY_NAME "VTK/GDCM C# Module")
  set(CPACK_COMPONENT_VTKJAVAMODULE_DISPLAY_NAME "VTK/GDCM Java Module")
  set(CPACK_COMPONENT_PARAVIEWMODULE_DISPLAY_NAME "ParaView Module")

  # Provide descriptions for each of the components to install.
  # When the user hovers the mouse over the name of a component,
  # the description will be shown in the "Description" box in the
  # installer. If no descriptions are provided, the "Description"
  # box will be removed.
  set(CPACK_COMPONENT_APPLICATIONS_DESCRIPTION
    "Command line applications that uses GDCM: gdcmconv, gdcmdump, gdcminfo, gdcmscanner, gdcmimg, gdcmanon")
  set(CPACK_COMPONENT_LIBRARIES_DESCRIPTION
    "Libraries used to build programs with GDCM")
  set(CPACK_COMPONENT_HEADERS_DESCRIPTION
    "C/C++ header files for use with GDCM")
  set(CPACK_COMPONENT_PYTHONMODULE_DESCRIPTION
    "Python Module for GDCM")
  set(CPACK_COMPONENT_CSHARPMODULE_DESCRIPTION
    "C# Module for GDCM")
  set(CPACK_COMPONENT_JAVAMODULE_DESCRIPTION
    "Java Module for GDCM")
  set(CPACK_COMPONENT_VTKAPPLICATIONS_DESCRIPTION
    "Command line applications that uses GDCM: gdcmviewer & gdcm2vtk")
  set(CPACK_COMPONENT_VTKLIBRARIES_DESCRIPTION
    "Libraries used to build programs with VTK/GDCM")
  set(CPACK_COMPONENT_VTKHEADERS_DESCRIPTION
    "C/C++ header files for use with VTK/GDCM")
  set(CPACK_COMPONENT_VTKPYTHONMODULE_DESCRIPTION
    "Python Module for VTK/GDCM")
  set(CPACK_COMPONENT_VTKCSHARPMODULE_DESCRIPTION
    "C# Module for VTK/GDCM")
  set(CPACK_COMPONENT_VTKJAVAMODULE_DESCRIPTION
    "Java Module for VTK/GDCM")
  set(CPACK_COMPONENT_PARAVIEWMODULE_DESCRIPTION
    "ParaView Module for VTK/GDCM")


  # Put the components into two different groups: "Runtime" and "Development"
  set(CPACK_COMPONENT_APPLICATIONS_GROUP "Runtime")
  set(CPACK_COMPONENT_PYTHONMODULE_GROUP "Development")
  set(CPACK_COMPONENT_CSHARPMODULE_GROUP "Development")
  set(CPACK_COMPONENT_JAVAMODULE_GROUP "Development")
  set(CPACK_COMPONENT_LIBRARIES_GROUP "Development")
  set(CPACK_COMPONENT_HEADERS_GROUP "Development")
  set(CPACK_COMPONENT_VTKAPPLICATIONS_GROUP "Runtime")
  set(CPACK_COMPONENT_VTKPYTHONMODULE_GROUP "Development")
  set(CPACK_COMPONENT_VTKCSHARPMODULE_GROUP "Development")
  set(CPACK_COMPONENT_VTKJAVAMODULE_GROUP "Development")
  set(CPACK_COMPONENT_VTKLIBRARIES_GROUP "Development")
  set(CPACK_COMPONENT_VTKHEADERS_GROUP "Development")
  set(CPACK_COMPONENT_PARAVIEWMODULE_GROUP "Runtime")
  # Expand the "Development" group by default, since we have so few components.
  # Also, provide this group with a description.
  set(CPACK_COMPONENT_GROUP_DEVELOPMENT_EXPANDED ON)
  set(CPACK_COMPONENT_GROUP_DEVELOPMENT_DESCRIPTION
    "All of the tools you'll ever need to develop software using GDCM")

  # It doesn't make sense to install the headers without the libraries
  # (because you could never use the headers!), so make the headers component
  # depend on the libraries component.
  set(CPACK_COMPONENT_HEADERS_DEPENDS Libraries)
  set(CPACK_COMPONENT_APPLICATIONS_DEPENDS Libraries)
  set(CPACK_COMPONENT_PYTHONMODULE_DEPENDS Libraries)
  set(CPACK_COMPONENT_CSHARPMODULE_DEPENDS Libraries)
  set(CPACK_COMPONENT_JAVAMODULE_DEPENDS Libraries)
  set(CPACK_COMPONENT_VTKHEADERS_DEPENDS VTKLibraries)
  set(CPACK_COMPONENT_VTKLIBRARIES_DEPENDS Libraries)
  set(CPACK_COMPONENT_VTKAPPLICATIONS_DEPENDS VTKLibraries)
  set(CPACK_COMPONENT_VTKPYTHONMODULE_DEPENDS VTKLibraries)
  set(CPACK_COMPONENT_VTKCSHARPMODULE_DEPENDS VTKLibraries)
  set(CPACK_COMPONENT_VTKJAVAMODULE_DEPENDS VTKLibraries)
  set(CPACK_COMPONENT_PARAVIEWMODULE_DEPENDS VTKLibraries)

  # Create two installation types with pre-selected components.
  # The "Developer" installation has just the library and headers,
  # while the "Full" installation has everything.
  set(CPACK_ALL_INSTALL_TYPES Full Developer)
  set(CPACK_INSTALL_TYPE_FULL_DISPLAY_NAME "Everything")
  set(CPACK_COMPONENT_LIBRARIES_INSTALL_TYPES Developer Full)
  set(CPACK_COMPONENT_HEADERS_INSTALL_TYPES Developer Full)
  set(CPACK_COMPONENT_APPLICATIONS_INSTALL_TYPES Full)
  set(CPACK_COMPONENT_PYTHONMODULE_INSTALL_TYPES Developer Full)
  set(CPACK_COMPONENT_CSHARPMODULE_INSTALL_TYPES Developer Full)
  set(CPACK_COMPONENT_JAVAMODULE_INSTALL_TYPES Developer Full)
  set(CPACK_COMPONENT_VTKLIBRARIES_INSTALL_TYPES Developer Full)
  set(CPACK_COMPONENT_VTKHEADERS_INSTALL_TYPES Developer Full)
  set(CPACK_COMPONENT_VTKAPPLICATIONS_INSTALL_TYPES Full)
  set(CPACK_COMPONENT_VTKPYTHONMODULE_INSTALL_TYPES Developer Full)
  set(CPACK_COMPONENT_VTKCSHARPMODULE_INSTALL_TYPES Developer Full)
  set(CPACK_COMPONENT_VTKJAVAMODULE_INSTALL_TYPES Developer Full)
  set(CPACK_COMPONENT_PARAVIEWMODULE_INSTALL_TYPES Developer Full)

  INCLUDE(CPack)

ENDIF(EXISTS "${CMAKE_ROOT}/Modules/CPack.cmake")


MACRO(PROCESSONEDIR DIRNAME myoutput)
  FILE(GLOB files
    ${DIRNAME}/*
  )
  FOREACH(file ${files})
    IF(IS_DIRECTORY ${file})
      IF("${file}" MATCHES ".svn")
        #MESSAGE("${file} is svn dir")
      ELSE("${file}" MATCHES ".svn")
        #MESSAGE("${file} is dir")
        PROCESSONEDIR(${file} mytests2)
        LIST(APPEND ${myoutput} ${mytests2})
      ENDIF("${file}" MATCHES ".svn")
    ELSE(IS_DIRECTORY ${file})
      #MESSAGE("${file} is file")
      GET_FILENAME_COMPONENT(filename ${file} NAME)
      IF("${filename}" MATCHES "Test" AND ${filename} MATCHES ".cxx")
        STRING(LENGTH ${filename} filename_length)
        #MESSAGE("${filename} is test, ${filename_length}")
        MATH(EXPR fileend "${filename_length} - 4 - 4") # Need to remove 'Test' and '.cxx'
        STRING(SUBSTRING ${filename} 4 ${fileend} classname)
        #MESSAGE("${classname} is tested")
        LIST(APPEND ${myoutput} ${classname})
      ENDIF("${filename}" MATCHES "Test" AND ${filename} MATCHES ".cxx")
    ENDIF(IS_DIRECTORY ${file})
  ENDFOREACH(file)
ENDMACRO(PROCESSONEDIR)

#PROCESSONEDIR(
#  ${CMAKE_CURRENT_SOURCE_DIR}/Testing/Source/Common
#  theoutput
#)
#MESSAGE("${theoutput}")

#-----------------------------------------------------------------------------
# we need to link against CoreFoundation so that we can use CFBundle to get the executable path.
IF(APPLE)
  FIND_LIBRARY(COREFOUNDATION_LIBRARY CoreFoundation )
ENDIF(APPLE)

#-----------------------------------------------------------------------------
# Need to be the last operation:
SET(GDCM_INCLUDE_PATH
  "${GDCM_SOURCE_DIR}/Source/Common"
  "${GDCM_BINARY_DIR}/Source/Common"
  "${GDCM_SOURCE_DIR}/Source/DataStructureAndEncodingDefinition"
  "${GDCM_SOURCE_DIR}/Source/MediaStorageAndFileFormat"
  "${GDCM_SOURCE_DIR}/Source/DataDictionary"
  "${GDCM_SOURCE_DIR}/Source/InformationObjectDefinition"
  )
IF(MSVC)
  SET(GDCM_INCLUDE_PATH
    ${GDCM_INCLUDE_PATH}
    "${GDCM_SOURCE_DIR}/Utilities/C99"
  )
ENDIF(MSVC)
IF(GDCM_USE_VTK)
    SET(GDCM_INCLUDE_PATH
      ${GDCM_INCLUDE_PATH}
      "${GDCM_SOURCE_DIR}/Utilities/VTK"
      )
ENDIF(GDCM_USE_VTK)
SET(GDCM_LIBRARY_DIRS ${LIBRARY_OUTPUT_PATH})
SUBDIRS(CMake/ExportConfiguration)
