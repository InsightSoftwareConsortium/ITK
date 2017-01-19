set(ITKTransformFactory_SRCS
  itkTransformFactoryBase.cxx
  itkTransformFactoryBaseRegisterFloatParameters.cxx
  itkTransformFactoryBaseRegisterDoubleParameters.cxx
  )

add_definitions("-DITK_TRANSFORM_FACTORY_MAX_DIM=${ITK_TRANSFORM_FACTORY_MAX_DIM}")
itk_module_add_library(ITKTransformFactory ${ITKTransformFactory_SRCS})
