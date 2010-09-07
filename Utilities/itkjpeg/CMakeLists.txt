PROJECT(ITKJPEG)

INCLUDE_REGULAR_EXPRESSION("^(jchuff|jconfig|jdct|jdhuff|jerror|jinclude|jmemsys|jmorecfg|jpegint|jpeglib|jversion|jpeg).*$")

INCLUDE_DIRECTORIES(BEFORE
               "${CMAKE_CURRENT_BINARY_DIR}"
               "${ITKJPEG_SOURCE_DIR}")

# memmgr back ends: compile only one of these into a working library
# (For now, let's use the mode that requires the image fit into memory.
# This is the recommended mode for Win32 anyway.)
SET(systemdependent_SRCS jmemnobs.c)

# library object files common to compression and decompression
SET(common_SRCS
jcomapi.c jutils.c jerror.c jmemmgr.c
jaricom.c
)

# compression library object files
SET(compression_SRCS
jcapimin.c jcapistd.c jctrans.c jcparam.c jdatadst.c jcinit.c
jcmaster.c jcmarker.c jcmainct.c jcprepct.c jccoefct.c jccolor.c
jcsample.c jchuff.c  jcdctmgr.c jfdctfst.c jfdctflt.c
jfdctint.c
#jcphuff.c
jcarith.c
)

# decompression library object files
SET(decompression_SRCS
jdapimin.c jdapistd.c jdtrans.c jdatasrc.c jdmaster.c
jdinput.c jdmarker.c jdhuff.c  jdmainct.c jdcoefct.c
jdpostct.c jddctmgr.c jidctfst.c jidctflt.c jidctint.c
#jidctred.c
jdsample.c jdcolor.c jquant1.c jquant2.c jdmerge.c
#jdphuff.c
jdarith.c
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

# Apply user-defined properties to the library target.
IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(itkjpeg PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

IF(NOT ITK_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS itkjpeg
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development
    )
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)
IF(NOT VTK_INSTALL_NO_DEVELOPMENT)
  INSTALL(FILES
    ${VTKJPEG_BINARY_DIR}/jpegDllConfig.h
    ${VTKJPEG_SOURCE_DIR}/jpeglib.h
    ${VTKJPEG_SOURCE_DIR}/itk_jpeg_mangle.h
    ${VTKJPEG_SOURCE_DIR}/jconfig.h
    ${VTKJPEG_SOURCE_DIR}/jmorecfg.h
    ${VTKJPEG_SOURCE_DIR}/jerror.h
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24}/itkjpeg
    COMPONENT Development)
ENDIF(NOT VTK_INSTALL_NO_DEVELOPMENT)
