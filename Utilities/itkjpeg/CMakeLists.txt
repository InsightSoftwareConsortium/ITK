project(ITKJPEG)

include_regular_expression("^(jchuff|jconfig|jdct|jdhuff|jerror|jinclude|jmemsys|jmorecfg|jpegint|jpeglib|jversion|jpeg).*$")

include_directories(BEFORE
               "${CMAKE_CURRENT_BINARY_DIR}"
               "${ITKJPEG_SOURCE_DIR}")

# memmgr back ends: compile only one of these into a working library
# (For now, let's use the mode that requires the image fit into memory.
# This is the recommended mode for Win32 anyway.)
set(systemdependent_SRCS jmemnobs.c)

# library object files common to compression and decompression
set(common_SRCS
jcomapi.c jutils.c jerror.c jmemmgr.c
jaricom.c
)

# compression library object files
set(compression_SRCS
jcapimin.c jcapistd.c jctrans.c jcparam.c jdatadst.c jcinit.c
jcmaster.c jcmarker.c jcmainct.c jcprepct.c jccoefct.c jccolor.c
jcsample.c jchuff.c  jcdctmgr.c jfdctfst.c jfdctflt.c
jfdctint.c
#jcphuff.c
jcarith.c
)

# decompression library object files
set(decompression_SRCS
jdapimin.c jdapistd.c jdtrans.c jdatasrc.c jdmaster.c
jdinput.c jdmarker.c jdhuff.c  jdmainct.c jdcoefct.c
jdpostct.c jddctmgr.c jidctfst.c jidctflt.c jidctint.c
#jidctred.c
jdsample.c jdcolor.c jquant1.c jquant2.c jdmerge.c
#jdphuff.c
jdarith.c
)


if(WIN32)
  if(BUILD_SHARED_LIBS)
    add_definitions(-DJPEGDLL)
  else(BUILD_SHARED_LIBS)
    set(JPEGSTATIC 1)
  endif(BUILD_SHARED_LIBS)
endif(WIN32)

configure_file(${ITKJPEG_SOURCE_DIR}/.NoDartCoverage
               ${ITKJPEG_BINARY_DIR}/.NoDartCoverage)
configure_file(${ITKJPEG_SOURCE_DIR}/jpegDllConfig.h.in
               ${ITKJPEG_BINARY_DIR}/jpegDllConfig.h)

add_library(itkjpeg ${systemdependent_SRCS} ${common_SRCS} ${compression_SRCS} ${decompression_SRCS})

# Apply user-defined properties to the library target.
if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(itkjpeg PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif(ITK_LIBRARY_PROPERTIES)

if(NOT ITK_INSTALL_NO_LIBRARIES)
  install(TARGETS itkjpeg
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development
    )
endif(NOT ITK_INSTALL_NO_LIBRARIES)
if(NOT VTK_INSTALL_NO_DEVELOPMENT)
  install(FILES
    ${VTKJPEG_BINARY_DIR}/jpegDllConfig.h
    ${VTKJPEG_SOURCE_DIR}/jpeglib.h
    ${VTKJPEG_SOURCE_DIR}/itk_jpeg_mangle.h
    ${VTKJPEG_SOURCE_DIR}/jconfig.h
    ${VTKJPEG_SOURCE_DIR}/jmorecfg.h
    ${VTKJPEG_SOURCE_DIR}/jerror.h
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24}/itkjpeg
    COMPONENT Development)
endif(NOT VTK_INSTALL_NO_DEVELOPMENT)
