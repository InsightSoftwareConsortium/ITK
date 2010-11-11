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
IF(BUILD_SHARED_LIBS)
  SET(OPJ_STATIC 0)
  IF(WIN32)
    ADD_DEFINITIONS(-DOPJ_EXPORTS)
  ENDIF(WIN32)
ELSE(BUILD_SHARED_LIBS)
  SET(OPJ_STATIC 1)
ENDIF(BUILD_SHARED_LIBS)

CONFIGURE_FILE(${ITKOpenJPEG_SOURCE_DIR}/openjpegDllConfig.h.in
               ${ITKOpenJPEG_BINARY_DIR}/openjpegDllConfig.h)

INCLUDE_DIRECTORIES("${ITKOpenJPEG_BINARY_DIR}")

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

IF(NOT ITK_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS ${OPENJPEG_LIBRARY_NAME}
    EXPORT ITKTargets
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)

if(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set(ITK_INSTALL_INCLUDE_DIR_CM24_UTILITIES_OPENJPEG ${ITK_INSTALL_INCLUDE_DIR_CM24})
else()
  set(ITK_INSTALL_INCLUDE_DIR_CM24_UTILITIES_OPENJPEG ${ITK_INSTALL_INCLUDE_DIR_CM24}/Utilities/openjpeg)
endif()

if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  install(FILES
    ${ITKOpenJPEG_SOURCE_DIR}/openjpeg.h
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24_UTILITIES_OPENJPEG}
    COMPONENT Development
  )
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)
