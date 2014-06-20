PROJECT(ITK3P_OpenJPEG C)
INCLUDE_REGULAR_EXPRESSION("^.*$")

# Defines the source code for the library
SET(OpenJPEG_SRCS
  tcd.c
  bio.c
  cio.c
  dwt.c
  event.c
  image.c
  j2k.c
  j2k_lib.c
  jp2.c
  jpt.c
  mct.c
  mqc.c
  openjpeg.c
  pi.c
  raw.c
  t1.c
  tgt.c
  profile.c
  invert.c
  function_list.c
  t2.c
  bio.h
  cio.h
  dwt.h
  event.h
  image.h
  j2k.h
  j2k_lib.h
  jp2.h
  jpt.h
  mct.h
  mqc.h
  openjpeg.h
  pi.h
  int.h
  raw.h
  t1.h
  t2.h
  tcd.h
  tgt.h
  profile.h
  invert.h
  function_list.h
)

# Pass proper definition to preprocessor to generate shared lib
IF(BUILD_SHARED_LIBS)
  SET(OPJ_STATIC 0)
  IF(WIN32)
    ADD_DEFINITIONS(-DOPJ_EXPORTS)
  ENDIF(WIN32)
ELSE(BUILD_SHARED_LIBS)
  SET(OPJ_STATIC 1)
ENDIF(BUILD_SHARED_LIBS)

CONFIGURE_FILE(${ITK3P_OpenJPEG_SOURCE_DIR}/openjpegDllConfig.h.in
               ${ITK3P_OpenJPEG_BINARY_DIR}/openjpegDllConfig.h)

CONFIGURE_FILE(${ITK3P_OpenJPEG_SOURCE_DIR}/opj_configure.h.in
               ${ITK3P_OpenJPEG_BINARY_DIR}/opj_configure.h)

#-----------------------------------------------------------------------------
# configure name mangling to allow multiple libraries to coexist
# peacefully
if(EXISTS ${ITK3P_OpenJPEG_SOURCE_DIR}/openjpeg_mangle.h.in)
set(MANGLE_PREFIX itk)
configure_file(${ITK3P_OpenJPEG_SOURCE_DIR}/openjpeg_mangle.h.in
               ${ITK3P_OpenJPEG_BINARY_DIR}/openjpeg_mangle.h
             @ONLY)
else()
  file(WRITE ${ITK3P_OpenJPEG_BINARY_DIR}/openjpeg_mangle.h "/* No mangling used */\n")
endif()

INCLUDE_DIRECTORIES("${ITK3P_OpenJPEG_BINARY_DIR}")

# Create the library
SET(OPENJPEG_LIBRARY_NAME ${OPJ_PREFIX}openjpeg )
ADD_LIBRARY(${OPENJPEG_LIBRARY_NAME} ${OpenJPEG_SRCS})

IF(UNIX)
  TARGET_LINK_LIBRARIES(${OPENJPEG_LIBRARY_NAME} m)
ENDIF(UNIX)

# Install library
IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(${OPENJPEG_LIBRARY_NAME} PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

INSTALL(TARGETS ${OPENJPEG_LIBRARY_NAME}
  EXPORT ${ITK3P_INSTALL_EXPORT_NAME}
  RUNTIME DESTINATION ${ITK3P_INSTALL_RUNTIME_DIR} COMPONENT RuntimeLibraries
  LIBRARY DESTINATION ${ITK3P_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
  ARCHIVE DESTINATION ${ITK3P_INSTALL_ARCHIVE_DIR} COMPONENT Development
  )

install(FILES
  ${ITK3P_OpenJPEG_BINARY_DIR}/openjpegDllConfig.h
  ${ITK3P_OpenJPEG_SOURCE_DIR}/openjpeg.h
  ${ITK3P_OpenJPEG_BINARY_DIR}/openjpeg_mangle.h
  DESTINATION ${ITK3P_INSTALL_INCLUDE_DIR}
  COMPONENT Development
  )
