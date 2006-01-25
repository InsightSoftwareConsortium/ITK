PROJECT(ITKOpenJPEG C)
INCLUDE_REGULAR_EXPRESSION("^.*$")
# Defines the source code for the library
SET(OpenJPEG_SRCS
  bio.c
  cio.c
  dwt.c
  event.c
  fix.c
  image.c
  int.c
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

# Handle the dllimport/dllexport for building shared lib on Win32
IF (WIN32)
  IF (BUILD_SHARED_LIBS)
    ADD_DEFINITIONS(-DOPJ_SHARED)
  ELSE (BUILD_SHARED_LIBS)
    ADD_DEFINITIONS(-DOPJ_STATIC)
  ENDIF (BUILD_SHARED_LIBS)
ENDIF (WIN32)

# Create the library
ADD_LIBRARY(${OPJ_PREFIX}openjpeg ${OpenJPEG_SRCS})

INSTALL_TARGETS(/lib/InsightToolkit ${OPJ_PREFIX}openjpeg)

