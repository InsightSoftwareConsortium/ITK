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
# Test if compiler defines the pseudo-macro __FUNCTION__
IF("GDCM_COMPILER_HAS_FUNCTION" MATCHES "^GDCM_COMPILER_HAS_FUNCTION$")
  MESSAGE(STATUS "Checking support for __FUNCTION__ in compiler")
  TRY_COMPILE(GDCM_COMPILER_HAS_FUNCTION
              ${GDCM_BINARY_DIR}/CMakeTmp/Function
              ${GDCM_SOURCE_DIR}/gdcmTestFUNCTION.cxx
              OUTPUT_VARIABLE OUTPUT)
  IF(GDCM_COMPILER_HAS_FUNCTION)
    MESSAGE(STATUS "Checking support for __FUNCTION__ -- yes")
    SET(GDCM_COMPILER_HAS_FUNCTION 1 CACHE INTERNAL "Support for extention C __FUNCTION__")
    WRITE_FILE(${CMAKE_BINARY_DIR}/CMakeOutput.log
      "Determining if the C compiler supports __FUNCTION__ "
      "passed with the following output:\n"
      "${OUTPUT}\n" APPEND)
  ELSE(GDCM_COMPILER_HAS_FUNCTION)
    MESSAGE(STATUS "Checking support for __FUNCTION__ -- no")
    SET(GDCM_COMPILER_HAS_FUNCTION 0 CACHE INTERNAL "Support for extension C __FUNCTION__")
    WRITE_FILE(${CMAKE_BINARY_DIR}/CMakeError.log
      "Determining if the C compiler supports __FUNCTION__ "
      "failed with the following output:\n"
      "${OUTPUT}\n" APPEND)
  ENDIF(GDCM_COMPILER_HAS_FUNCTION)
ENDIF("GDCM_COMPILER_HAS_FUNCTION" MATCHES "^GDCM_COMPILER_HAS_FUNCTION$")

#-----------------------------------------------------------------------------
# Build directory on which many applications depend
SUBDIRS(
  Dicts
  src
)

# Set the place for the public dictionary, must be set before gdcmConfigure.h
SET( GDCM_PUB_DICT_PATH "${CMAKE_INSTALL_PREFIX}${GDCM_DATA_DIR}")

#-----------------------------------------------------------------------------
INCLUDE (${CMAKE_ROOT}/Modules/TestBigEndian.cmake)
TEST_BIG_ENDIAN(GDCM_WORDS_BIGENDIAN)

INCLUDE (${CMAKE_ROOT}/Modules/CheckIncludeFile.cmake)
CHECK_INCLUDE_FILE("stdint.h" CMAKE_HAVE_STDINT_H)

# Need those header for GetMacAddress in Util
CHECK_INCLUDE_FILE("unistd.h"       CMAKE_HAVE_UNISTD_H)
CHECK_INCLUDE_FILE("stdlib.h"       CMAKE_HAVE_STDLIB_H)
CHECK_INCLUDE_FILE("sys/ioctl.h"    CMAKE_HAVE_SYS_IOCTL_H)
CHECK_INCLUDE_FILE("sys/socket.h"   CMAKE_HAVE_SYS_SOCKET_H)
CHECK_INCLUDE_FILE("sys/sockio.h"   CMAKE_HAVE_SYS_SOCKIO_H)
CHECK_INCLUDE_FILE("net/if.h"       CMAKE_HAVE_NET_IF_H)
CHECK_INCLUDE_FILE("netinet/in.h"   CMAKE_HAVE_NETINET_IN_H)
CHECK_INCLUDE_FILE("net/if_dl.h"    CMAKE_HAVE_NET_IF_DL_H)
CHECK_INCLUDE_FILE("net/if_arp.h"   CMAKE_HAVE_NET_IF_ARP_H)

CONFIGURE_FILE(${GDCM_SOURCE_DIR}/gdcmConfigure.h.in
               ${GDCM_BINARY_DIR}/gdcmConfigure.h @ONLY IMMEDIATE)

INSTALL_FILES(/include/InsightToolkit/gdcm/src .h gdcmConfigure.h)

