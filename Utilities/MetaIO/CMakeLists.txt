PROJECT(MetaIO)

SET ( MetaIO_SRCS
  metaTypes.h
  metaUtils.cxx
  metaObject.cxx
  metaImage.cxx
  metaImageUtils.cxx
  metaTube.cxx
  metaEllipse.cxx
  metaScene.cxx
  metaBlob.cxx
  metaSurface.cxx
  metaLine.cxx
)

ADD_LIBRARY (
  MetaIO ${MetaIO_SRCS}
)

INSTALL_TARGETS(/lib/InsightToolkit MetaIO)
