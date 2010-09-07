PROJECT(ITKOpenJPEG C)
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

CONFIGURE_FILE(${ITKOpenJPEG_SOURCE_DIR}/.NoDartCoverage
               ${ITKOpenJPEG_BINARY_DIR}/.NoDartCoverage)


# Pass proper definition to preprocessor to generate shared lib
IF(WIN32)
  IF(BUILD_SHARED_LIBS)
    ADD_DEFINITIONS(-DOPJ_EXPORTS)
  ELSE(BUILD_SHARED_LIBS)
    ADD_DEFINITIONS(-DOPJ_STATIC)
  ENDIF(BUILD_SHARED_LIBS)
ENDIF(WIN32)

# Create the library
ADD_LIBRARY(${OPJ_PREFIX}openjpeg ${OpenJPEG_SRCS})

IF(UNIX)
  TARGET_LINK_LIBRARIES(${OPENJPEG_LIBRARY_NAME} m)
ENDIF(UNIX)

# Install library
IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(${OPJ_PREFIX}openjpeg PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

IF(NOT ITK_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS ${OPJ_PREFIX}openjpeg
    EXPORT ITKTargets
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)
