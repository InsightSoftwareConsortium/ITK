ADD_LIBRARY(ITKMetaIO
  metaBlob.cxx
  metaEllipse.cxx
  metaGroup.cxx
  metaGaussian.cxx
  metaImage.cxx
  metaImageUtils.cxx
  metaLandmark.cxx
  metaLine.cxx
  metaMesh.cxx
  metaMesh.h
  metaObject.cxx
  metaScene.cxx
  metaSurface.cxx
  metaTube.cxx
  metaTypes.h
  metaUtils.cxx
)

#SUBDIRS(tests)

INSTALL_TARGETS(/lib/InsightToolkit ITKMetaIO)
INSTALL_FILES(/include/InsightToolkit/Utilities/MetaIO "(\\.h)$")
TARGET_LINK_LIBRARIES(ITKMetaIO ${ITK_ZLIB_LIBRARIES})
