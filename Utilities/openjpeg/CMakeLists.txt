PROJECT(ITKOpenJPEG C)
INCLUDE_REGULAR_EXPRESSION("^.*$")
# Defines the source code for the library
SET(OpenJPEG_SRCS
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
  t2.c
  tcd.c
  tgt.c
)

CONFIGURE_FILE(${ITKOpenJPEG_SOURCE_DIR}/.NoDartCoverage
               ${ITKOpenJPEG_BINARY_DIR}/.NoDartCoverage)

# Pass proper definition to preprocessor to generate shared lib
IF (WIN32)
  IF (BUILD_SHARED_LIBS)
    ADD_DEFINITIONS(-DOPJ_EXPORTS)
  ELSE (BUILD_SHARED_LIBS)
    ADD_DEFINITIONS(-DOPJ_STATIC)
  ENDIF (BUILD_SHARED_LIBS)
ENDIF (WIN32)

# Create the library
ADD_LIBRARY(${OPJ_PREFIX}openjpeg ${OpenJPEG_SRCS})

# Install library
IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(${OPJ_PREFIX}openjpeg PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)
INSTALL_TARGETS(/lib/InsightToolkit ${OPJ_PREFIX}openjpeg)

