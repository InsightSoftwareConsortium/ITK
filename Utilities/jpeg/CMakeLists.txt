PROJECT(ITKJPEG)

INCLUDE_REGULAR_EXPRESSION("^(jchuff|jconfig|jdct|jdhuff|jerror|jinclude|jmemsys|jmorecfg|jpegint|jpeglib|jversion|jpeg).*$")

INCLUDE_DIRECTORIES(${ITKJPEG_SOURCE_DIR})
INCLUDE_DIRECTORIES(${ITKJPEG_BINARY_DIR})

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
jcapimin.c jcapistd.c jctrans.c jcparam.c jdatadst.c jcinit.c
jcmaster.c jcmarker.c jcmainct.c jcprepct.c jccoefct.c jccolor.c
jcsample.c jchuff.c jcphuff.c jcdctmgr.c jfdctfst.c jfdctflt.c
jfdctint.c
)

# decompression library object files
SET(decompression_SRCS
jdapimin.c jdapistd.c jdtrans.c jdatasrc.c jdmaster.c
jdinput.c jdmarker.c jdhuff.c jdphuff.c jdmainct.c jdcoefct.c
jdpostct.c jddctmgr.c jidctfst.c jidctflt.c jidctint.c jidctred.c
jdsample.c jdcolor.c jquant1.c jquant2.c jdmerge.c
)


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

ADD_LIBRARY(itkjpeg ${systemdependent_SRCS} ${common_SRCS} ${compression_SRCS} ${decompression_SRCS})

INSTALL_TARGETS(/lib/InsightToolkit itkjpeg)
INSTALL_FILES(/include/InsightToolkit/Utilities/jpeg .h
  jpeglib jconfig jmorecfg jpegint jerror jpegDllConfig
)

