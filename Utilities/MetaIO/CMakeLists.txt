ADD_LIBRARY(ITKMetaIO
  metaBlob.cxx
  metaEllipse.cxx
  metaGroup.cxx
  metaImage.cxx
  metaImageUtils.cxx
  metaLandmark.cxx
  metaLine.cxx
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
