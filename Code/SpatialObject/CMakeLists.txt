INCLUDE_DIRECTORIES( 
  ${ITK_SOURCE_DIR}/Code/Common 
  ${ITK_SOURCE_DIR}/Code/SpatialObject 
)

SOURCE_FILES(
  ITKSpatialObject_SRCS 
  itkTubeSpatialObject 
  itkTubeNetworkSpatialObject
)

ADD_LIBRARY(
  ITKSpatialObject 
  ITKSpatialObject_SRCS
)
