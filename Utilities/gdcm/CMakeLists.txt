PROJECT(GDCM)
CMAKE_MINIMUM_REQUIRED(VERSION 1.8)

#-----------------------------------------------------------------------------
# GDCM version number, usefull for packaging and doxygen doc:
SET(GDCM_MAJOR_VERSION 0)
SET(GDCM_MINOR_VERSION 6)
SET(GDCM_BUILD_VERSION 0)
SET(GDCM_VERSION 
    "${GDCM_MAJOR_VERSION}.${GDCM_MINOR_VERSION}.${GDCM_BUILD_VERSION}")

#-----------------------------------------------------------------------------
# datadir in CMake:
SET(GDCM_DATA_DIR "/share/gdcm/" CACHE INTERNAL 
  "Install location for data (relative to prefix).")
MARK_AS_ADVANCED(GDCM_DATA_DIR)

#-----------------------------------------------------------------------------
#OPTION(GDCM_DEBUG "Turn verbosity of some statement ON." OFF)
#MARK_AS_ADVANCED(GDCM_DEBUG)
SET(GDCM_DEBUG 0)

#-----------------------------------------------------------------------------
# Build directory on which many applications depend
SUBDIRS(
  Dicts
  src
)

# Set the place for the public dictionary, must be set before gdcmConfigure.h
SET( GDCM_PUB_DICT_PATH "${CMAKE_INSTALL_PREFIX}${GDCM_DATA_DIR}")

#-----------------------------------------------------------------------------
# SunOS + old gcc fixes:
INCLUDE (${CMAKE_ROOT}/Modules/CMakeBackwardCompatibilityCXX.cmake)
IF(CMAKE_NO_ANSI_STRING_STREAM)
  SET(GDCM_NO_ANSI_STRING_STREAM 1)
ENDIF(CMAKE_NO_ANSI_STRING_STREAM)
INCLUDE (${CMAKE_ROOT}/Modules/TestBigEndian.cmake)
TEST_BIG_ENDIAN(GDCM_WORDS_BIGENDIAN)

INCLUDE (${CMAKE_ROOT}/Modules/CheckIncludeFile.cmake)
CHECK_INCLUDE_FILE("stdint.h" CMAKE_HAVE_STDINT_H)

CONFIGURE_FILE(${GDCM_SOURCE_DIR}/gdcmConfigure.h.in
               ${GDCM_BINARY_DIR}/gdcmConfigure.h @ONLY IMMEDIATE)

INSTALL_FILES(/include/InsightToolkit .h gdcmConfigure.h)

