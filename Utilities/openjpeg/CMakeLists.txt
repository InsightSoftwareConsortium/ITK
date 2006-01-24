PROJECT(ITKOpenJPEG C)
INCLUDE_REGULAR_EXPRESSION("^.*$")

# Create the lib
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

IF (WIN32)
  IF (BUILD_SHARED_LIBS)
    ADD_DEFINITIONS(-DOPJ_SHARED)
  ELSE (BUILD_SHARED_LIBS)
    ADD_DEFINITIONS(-DOPJ_STATIC)
  ENDIF (BUILD_SHARED_LIBS)
ENDIF (WIN32)

ADD_LIBRARY(itkOpenJPEG ${OpenJPEG_SRCS})

INSTALL_TARGETS(/lib/InsightToolkit itkOpenJPEG)

