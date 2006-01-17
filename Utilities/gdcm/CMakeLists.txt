PROJECT(GDCM)
CMAKE_MINIMUM_REQUIRED(VERSION 2.0)

#-----------------------------------------------------------------------------
# GDCM version number, usefull for packaging and doxygen doc:
SET(GDCM_MAJOR_VERSION 1)
SET(GDCM_MINOR_VERSION 0)
SET(GDCM_BUILD_VERSION 0)
SET(GDCM_VERSION 
    "${GDCM_MAJOR_VERSION}.${GDCM_MINOR_VERSION}.${GDCM_BUILD_VERSION}")

#-----------------------------------------------------------------------------
# datadir in CMake:
SET(GDCM_DATA_DIR "/share/gdcm/" CACHE INTERNAL 
  "Install location for data (relative to prefix).")
MARK_AS_ADVANCED(GDCM_DATA_DIR)

#-----------------------------------------------------------------------------
# Build shared lib by default
SET(GDCM_BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS})

#OPTION(GDCM_DEBUG "Turn verbosity of some statement ON." OFF)
#MARK_AS_ADVANCED(GDCM_DEBUG)
SET(GDCM_DEBUG 0)

#-----------------------------------------------------------------------------
# Test if compiler defines the pseudo-macro __FUNCTION__
IF("GDCM_COMPILER_HAS_FUNCTION" MATCHES "^GDCM_COMPILER_HAS_FUNCTION$")
  MESSAGE(STATUS "Checking support for __FUNCTION__ in compiler")
  TRY_COMPILE(GDCM_COMPILER_HAS_FUNCTION
              ${GDCM_BINARY_DIR}/CMakeTmp/Function
              ${GDCM_SOURCE_DIR}/CMake/gdcmTestFUNCTION.cxx
              OUTPUT_VARIABLE OUTPUT)
  IF(GDCM_COMPILER_HAS_FUNCTION)
    MESSAGE(STATUS "Checking support for __FUNCTION__ -- yes")
    SET(GDCM_COMPILER_HAS_FUNCTION 1 CACHE INTERNAL "Support for extension C __FUNCTION__")
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
# Test if os defines a length for sockaddr
IF("HAVE_SA_LEN" MATCHES "^HAVE_SA_LEN$")
  STRING(ASCII 35 POUND)
  FILE(WRITE ${GDCM_BINARY_DIR}/CMakeTmp/gdcmTestHAVESALEN.c
    "${POUND}include <sys/types.h>\n"
    "${POUND}include <sys/socket.h>\n"
    "int main() { struct sockaddr sa; sa.sa_len = 0; }")
  MESSAGE(STATUS "Checking if ifreq has a sa_len")
  TRY_COMPILE(HAVE_SA_LEN
              ${GDCM_BINARY_DIR}
              ${GDCM_BINARY_DIR}/CMakeTmp/gdcmTestHAVESALEN.c
              OUTPUT_VARIABLE OUTPUT)
  IF(HAVE_SA_LEN)
    MESSAGE(STATUS "Checking if ifreq has a sa_len -- yes")
    SET(HAVE_SA_LEN 1 CACHE INTERNAL "Support if ifreq has a sa_len")
    WRITE_FILE(${CMAKE_BINARY_DIR}/CMakeOutput.log
      "Determining if ifreq has a sa_len"
      "passed with the following output:\n"
      "${OUTPUT}\n" APPEND)
  ELSE(HAVE_SA_LEN)
    MESSAGE(STATUS "Checking if ifreq has a sa_len -- no")
    SET(HAVE_SA_LEN 0 CACHE INTERNAL "Support if ifreq has a sa_len")
    WRITE_FILE(${CMAKE_BINARY_DIR}/CMakeError.log
      "Determining if ifreq has a sa_len"
      "failed with the following output:\n"
      "${OUTPUT}\n" APPEND)
  ENDIF(HAVE_SA_LEN)
ENDIF("HAVE_SA_LEN" MATCHES "^HAVE_SA_LEN$")

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
CHECK_INCLUDE_FILE("stdint.h"       CMAKE_HAVE_STDINT_H)
CHECK_INCLUDE_FILE("inttypes.h"     CMAKE_HAVE_INTTYPES_H)

# Check if header file exists and add it to the list.
INCLUDE (${CMAKE_ROOT}/Modules/CheckIncludeFiles.cmake)
MACRO(CHECK_INCLUDE_FILE_CONCAT FILE VARIABLE)
  CHECK_INCLUDE_FILES("${GDCM_INCLUDES};${FILE}" ${VARIABLE})
  IF(${VARIABLE})
    SET(GDCM_INCLUDES ${GDCM_INCLUDES} ${FILE})
  ENDIF(${VARIABLE})
ENDMACRO(CHECK_INCLUDE_FILE_CONCAT)

# Need those header for GetMacAddress in Util
# This is tricky as you need to figure out the proper order to
# test the inclusion of files
CHECK_INCLUDE_FILE_CONCAT("unistd.h"       CMAKE_HAVE_UNISTD_H)
CHECK_INCLUDE_FILE_CONCAT("stdlib.h"       CMAKE_HAVE_STDLIB_H)
CHECK_INCLUDE_FILE_CONCAT("sys/ioctl.h"    CMAKE_HAVE_SYS_IOCTL_H)
CHECK_INCLUDE_FILE_CONCAT("sys/socket.h"   CMAKE_HAVE_SYS_SOCKET_H)
CHECK_INCLUDE_FILE_CONCAT("sys/sockio.h"   CMAKE_HAVE_SYS_SOCKIO_H)
CHECK_INCLUDE_FILE_CONCAT("net/if.h"       CMAKE_HAVE_NET_IF_H)
CHECK_INCLUDE_FILE_CONCAT("netinet/in.h"   CMAKE_HAVE_NETINET_IN_H)
CHECK_INCLUDE_FILE_CONCAT("net/if_dl.h"    CMAKE_HAVE_NET_IF_DL_H)
CHECK_INCLUDE_FILE_CONCAT("net/if_arp.h"   CMAKE_HAVE_NET_IF_ARP_H)

#-----------------------------------------------------------------------------

CONFIGURE_FILE(${GDCM_SOURCE_DIR}/gdcmConfigure.h.in
               ${GDCM_BINARY_DIR}/gdcmConfigure.h @ONLY IMMEDIATE)

INSTALL_FILES(/include/InsightToolkit/gdcm/src .h gdcmConfigure.h)

