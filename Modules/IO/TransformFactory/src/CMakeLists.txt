set(ITKTransformFactory_SRCS
  itkTransformFactoryBase.cxx
  itkTransformFactoryBaseRegisterFloatParameters.cxx
  itkTransformFactoryBaseRegisterDoubleParameters.cxx
  )

itk_module_add_library(ITKTransformFactory ${ITKTransformFactory_SRCS})
