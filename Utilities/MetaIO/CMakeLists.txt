ADD_LIBRARY(ITKMetaIO
  metaArrow.cxx
  metaBlob.cxx
  metaCommand.h
  metaCommand.cxx
  metaContour.h
  metaContour.cxx
  metaDTITube.cxx
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
  metaVesselTube.cxx
  metaTransform.cxx
  metaTubeGraph.cxx
)

#SUBDIRS(tests)

IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(ITKMetaIO PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

TARGET_LINK_LIBRARIES(ITKMetaIO ${ITK_ZLIB_LIBRARIES})

IF(NOT ITK_INSTALL_NO_LIBRARIES)
  INSTALL_TARGETS(
    ${ITK_INSTALL_LIB_DIR}
    RUNTIME_DIRECTORY ${ITK_INSTALL_BIN_DIR}
    ITKMetaIO)
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)

IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  INSTALL_FILES(${ITK_INSTALL_INCLUDE_DIR}/Utilities/MetaIO "(\\.h)$")
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)
