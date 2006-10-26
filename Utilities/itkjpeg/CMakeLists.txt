PROJECT(ITKJPEG)

INCLUDE_REGULAR_EXPRESSION("^(jchuff|jconfig|jdct|jdhuff|jerror|jinclude|jmemsys|jmorecfg|jpegint|jpeglib|jversion|jpeg).*$")

#MESSAGE(FATAL_ERROR ${ITKJPEG_SOURCE_DIR})
#INCLUDE_DIRECTORIES(${ITKJPEG_SOURCE_DIR})
#INCLUDE_DIRECTORIES(${ITKJPEG_BINARY_DIR})


# memmgr back ends: compile only one of these into a working library
# (For now, let's use the mode that requires the image fit into memory.
# This is the recommended mode for Win32 anyway.)
SET(systemdependent_SRCS jmemnobs.c)

# library object files common to compression and decompression
SET(common_SRCS
jcomapi.c jutils.c jerror.c jmemmgr.c
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

jdarith.c
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
jcarith.c
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
#  jmorecfg.h     #this one need to be configured
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
    "${ITKJPEG_SOURCE_DIR}/${src}")
ENDFOREACH(src)

IF (WIN32)
  IF (BUILD_SHARED_LIBS)
    ADD_DEFINITIONS(-DJPEGDLL)
  ELSE (BUILD_SHARED_LIBS)
    SET(JPEGSTATIC 1)
  ENDIF (BUILD_SHARED_LIBS)
ENDIF (WIN32)

CONFIGURE_FILE(${ITKJPEG_SOURCE_DIR}/.NoDartCoverage
               ${ITKJPEG_BINARY_DIR}/.NoDartCoverage)
CONFIGURE_FILE(${ITKJPEG_SOURCE_DIR}/jpegDllConfig.h.in
               ${ITKJPEG_BINARY_DIR}/jpegDllConfig.h)

# Generate three libraries: jpeg 8bits, 12bits and 16bits
# using only one source directory

# Particular case jmorecfg.h needs to be specified either 8, 12 or 16 bits:
FOREACH(bits 8 12 16)
  SET(CMAKE_BITS_IN_JSAMPLE ${bits})
  CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/jmorecfg.h.in
                 ${CMAKE_CURRENT_BINARY_DIR}/${bits}/jmorecfg.h @ONLY IMMEDIATE)
  SET(BITS_TYPE_JPEG_MANGLE_NAME "itkjpeg/mangle_jpeg${bits}bits.h")
  CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/jconfig.h.in
                 ${CMAKE_CURRENT_BINARY_DIR}/${bits}/jconfig.h @ONLY IMMEDIATE)
  # Dummy copy for ITK to include <jpeg/8/jpeglib.h> with no conflict
  CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/jpeglib.h
                 ${CMAKE_CURRENT_BINARY_DIR}/${bits}/jpeglib.h COPYONLY IMMEDIATE)
  CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/jerror.h
                 ${CMAKE_CURRENT_BINARY_DIR}/${bits}/jerror.h COPYONLY IMMEDIATE)
  CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/jinclude.h
                 ${CMAKE_CURRENT_BINARY_DIR}/${bits}/jinclude.h COPYONLY IMMEDIATE)
ENDFOREACH(bits)

MACRO(ITK_JPEG_BODY BITRATE)
  INCLUDE_DIRECTORIES(BEFORE
    "${CMAKE_CURRENT_BINARY_DIR}"
    "${ITKJPEG_SOURCE_DIR}")
  ADD_LIBRARY(itkjpeg${BITRATE} ${JPEG_SOURCES})
  IF(ITK_LIBRARY_PROPERTIES)
    SET_TARGET_PROPERTIES(itkjpeg${BITRATE} PROPERTIES ${ITK_LIBRARY_PROPERTIES})
  ENDIF(ITK_LIBRARY_PROPERTIES)

  IF(NOT ITK_INSTALL_NO_LIBRARIES)
    INSTALL(TARGETS itkjpeg${BITRATE}
      RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
      LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
      ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development
      )
  ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)
ENDMACRO(ITK_JPEG_BODY)

SUBDIRS(8 12 16)

