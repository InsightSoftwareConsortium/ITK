IF(NOT JPEG_NAMESPACE)
  SET(JPEG_NAMESPACE "JPEG")
  SET(JPEG_STANDALONE 1)
  SET(JPEG_INSTALL_BIN_DIR bin)
  SET(JPEG_INSTALL_LIB_DIR lib)
  SET(JPEG_INSTALL_INCLUDE_DIR include/ljpeg-62)
  OPTION(JPEG_BUILD_SHARED_LIBS "Build IJG/JPEG with shared libraries." OFF)
  SET(BUILD_SHARED_LIBS ${JPEG_BUILD_SHARED_LIBS})
ENDIF(NOT JPEG_NAMESPACE)
# In all cases:
STRING(TOLOWER ${JPEG_NAMESPACE} JPEG_LIBRARY_NAME)


PROJECT(${JPEG_NAMESPACE} C)
CMAKE_MINIMUM_REQUIRED(VERSION 2.2)

# Do full dependency headers.
INCLUDE_REGULAR_EXPRESSION("^.*$")

SET(JPEG_LIBRARY_PROPERTIES ${JPEG_LIBRARY_PROPERTIES}
  VERSION "${GDCM_VERSION}"
  SOVERSION "${GDCM_MAJOR_VERSION}.${GDCM_MINOR_VERSION}"
)


# memmgr back ends: compile only one of these into a working library
# (For now, let's use the mode that requires the image fit into memory.
# This is the recommended mode for Win32 anyway.)
SET(systemdependent_SRCS jmemnobs.c)

# library object files common to compression and decompression
SET(common_SRCS
jcomapi.c jutils.c jerror.c jmemmgr.c
#jmemsrc.c
)

# compression library object files
SET(compression_SRCS
jcapimin.c jcapistd.c jctrans.c jcparam.c  jcinit.c
jcmaster.c jcmarker.c jcmainct.c jcprepct.c jccoefct.c jccolor.c
jcsample.c jchuff.c jcphuff.c jcdctmgr.c jfdctfst.c jfdctflt.c
jdatadst.c
)

# Lossy (DCT) codec:
SET(comp_lossy_SRCS
jfdctint.c
jclossy.c

# lossless
jclossls.c

#jdarith.c
jcodec.c
jcscale.c
jcshuff.c
jclhuff.c
jcpred.c
jcdiffct.c
)

# decompression library object files
SET(decompression_SRCS
jdapimin.c jdapistd.c jdtrans.c  jdmaster.c
jdinput.c jdmarker.c jdhuff.c jdphuff.c jdmainct.c jdcoefct.c
jdpostct.c jddctmgr.c jidctfst.c jidctflt.c jidctint.c jidctred.c
jdsample.c jdcolor.c jquant1.c jquant2.c jdmerge.c
jdatasrc.c
)

SET(decomp_lossy_SRCS
 jdlossls.c jdlossy.c
 #jcarith.c
jdlhuff.c
jddiffct.c
jdpred.c
jdscale.c
jdshuff.c
)

SET(JPEG_HEADER_FILES
  jchuff.h
  jdct.h
  jdhuff.h
  jerror.h
  jinclude.h
  jmemsys.h
  jmorecfg.h
  jpegint.h
  jpeglib.h
  jversion.h
  jlossy.h
  jlossls.h
  )

SET(JPEG_SOURCES)
FOREACH(src
  ${systemdependent_SRCS}
  ${common_SRCS}
  ${compression_SRCS}
  ${decompression_SRCS}
  ${comp_lossy_SRCS}
  ${decomp_lossy_SRCS}
  )
  SET(JPEG_SOURCES ${JPEG_SOURCES}
    "${CMAKE_CURRENT_SOURCE_DIR}/${src}")
ENDFOREACH(src)

IF (WIN32)
  IF (BUILD_SHARED_LIBS)
    SET(JPEGDLL 1)
  ELSE (BUILD_SHARED_LIBS)
    SET(JPEGSTATIC 1)
  ENDIF (BUILD_SHARED_LIBS)
ENDIF (WIN32)

# By default support DICOMObjects bug, if this is a speed penalty for you
# you should consider disabling it
SET(SUPPORT_DICOMOBJECTS_BUG ON)

# Generate three libraries: jpeg 8bits, 12bits and 16bits
# using only one source directory

#-----------------------------------------------------------------------------
INCLUDE (${CMAKE_ROOT}/Modules/CheckIncludeFile.cmake)
CHECK_INCLUDE_FILE("stddef.h"       HAVE_STDDEF_H)
CHECK_INCLUDE_FILE("stdlib.h"       HAVE_STDLIB_H)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})
# Particular case jpegcmake.h needs to be specified either 8, 12 or 16 bits:
# also configure the name mangling
FOREACH(bits 8 12 16)
  SET(CMAKE_BITS_IN_JSAMPLE ${bits})
  CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/jpegcmake.h.in
                 ${CMAKE_CURRENT_BINARY_DIR}/${bits}/jpegcmake.h
                 @ONLY)
  SET(MANGLE_PREFIX ${JPEG_LIBRARY_NAME}${bits})
  CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/mangle_jpeg.h.in
                 ${CMAKE_CURRENT_BINARY_DIR}/${bits}/mangle_jpeg${bits}bits.h
                 @ONLY)


  # FIXME FIXME FIXME
  CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/jmorecfg.h
                 ${CMAKE_CURRENT_BINARY_DIR}/${bits}/jmorecfg.h
                 COPYONLY)
  CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/jconfig.h
                 ${CMAKE_CURRENT_BINARY_DIR}/${bits}/jconfig.h
                 COPYONLY)
  CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/jpeglib.h
                 ${CMAKE_CURRENT_BINARY_DIR}/${bits}/jpeglib.h
                 COPYONLY)
  CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/jerror.h
                 ${CMAKE_CURRENT_BINARY_DIR}/${bits}/jerror.h
                 COPYONLY)
  CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/jinclude.h
                 ${CMAKE_CURRENT_BINARY_DIR}/${bits}/jinclude.h
                 COPYONLY)
ENDFOREACH(bits)

# I could not get the depencencies in CMake to work properly so instead
# have multiple directory for each target that will ensure proper recompilation
# of the objects files
SUBDIRS(8 12 16)

IF(NOT JPEG_INSTALL_NO_DEVELOPMENT)
  FILE(GLOB header_files "*.h")
  INSTALL(FILES ${header_files}
    DESTINATION ${JPEG_INSTALL_INCLUDE_DIR}/gdcmjpeg COMPONENT Headers
  )
ENDIF(NOT JPEG_INSTALL_NO_DEVELOPMENT)
