set(ITKSpatialObjects_SRCS
  itkMetaEvent.cxx
  itkSpatialObjectFactoryBase.cxx
  itkSpatialObjectProperty.cxx
  itkDTITubeSpatialObjectPoint.cxx
 )

itk_module_add_library(ITKSpatialObjects ${ITKSpatialObjects_SRCS})
