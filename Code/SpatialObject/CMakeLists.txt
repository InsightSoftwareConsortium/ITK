# Sources of non-templated classes.
SET(ITKSpatialObject_SRCS
  itkCylinderSpatialObject.cxx
)

ADD_LIBRARY(ITKSpatialObject ${ITKSpatialObject_SRCS})
INSTALL_TARGETS(/lib/InsightToolkit ITKSpatialObject)
INSTALL_FILES(/include/InsightToolkit/SpatialObject "(\\.h|\\.txx)$")
