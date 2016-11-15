set(ITKSpatialObjects_SRCS
  itkMetaEvent.cxx
  itkCylinderSpatialObject.cxx
  itkSpatialObjectFactoryBase.cxx
  itkArrowSpatialObject.cxx
 )

itk_module_add_library(ITKSpatialObjects ${ITKSpatialObjects_SRCS})
