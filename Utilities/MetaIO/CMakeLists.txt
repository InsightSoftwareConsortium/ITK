ADD_LIBRARY(ITKMetaIO
  metaTypes.h
  metaUtils.cxx
  metaObject.cxx
  metaImage.cxx
  metaImageUtils.cxx
  metaTube.cxx
  metaEllipse.cxx
  metaGroup.cxx
  metaScene.cxx
  metaBlob.cxx
  metaSurface.cxx
  metaLine.cxx
  metaLandmark.cxx
)

INSTALL_TARGETS(/lib/InsightToolkit ITKMetaIO)
INSTALL_FILES(/include/InsightToolkit/Utilities/MetaIO "(\\.h)$")
