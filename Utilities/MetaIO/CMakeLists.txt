PROJECT(MetaIO)

SOURCE_FILES ( MetaIO_SRCS
  metaTypes
  metaUtils
  metaObject
  metaImage
  metaImageUtils
  metaTube
  metaEllipse
  metaScene
  metaBlob
  metaSurface
  metaLine
)

ADD_LIBRARY (
  MetaIO MetaIO_SRCS
)

INSTALL_TARGETS(/lib/insight MetaIO)

INCLUDE_DIRECTORIES (
  ${MetaIO_SOURCE_DIR}
)

LINK_DIRECTORIES (
  ${MetaIO_BINARY_DIR}
)

LINK_LIBRARIES (
  MetaIO
)

#IF (BUILD_TESTING)
#SUBDIRS(
#tests
#)
#ENDIF (BUILD_TESTING)

IF(UNIX)
  IF (NOT ITK_USE_SYSTEM_STDLIB)
  LINK_LIBRARIES (
  -lCio
  )
  ENDIF(NOT ITK_USE_SYSTEM_STDLIB)
  LINK_LIBRARIES (
  -lm
  )
ENDIF(UNIX)

