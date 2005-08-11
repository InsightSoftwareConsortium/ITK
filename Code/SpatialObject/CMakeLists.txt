# Sources of non-templated classes.
SET(ITKSpatialObject_SRCS
  itkCylinderSpatialObject.cxx
  itkSpatialObjectFactoryBase.cxx
  itkSPHARMCoefSpatialObject.cxx
  itkMetaSPHARMCoefConverter.cxx
)

ADD_LIBRARY(ITKSpatialObject ${ITKSpatialObject_SRCS})
TARGET_LINK_LIBRARIES(ITKSpatialObject ITKNumerics ITKCommon ITKMetaIO) 
INSTALL_TARGETS(/lib/InsightToolkit ITKSpatialObject)
INSTALL_FILES(/include/InsightToolkit/SpatialObject "(\\.h|\\.txx)$")
