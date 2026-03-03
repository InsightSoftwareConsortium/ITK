set(
  ITKIOTransformBase_SRCS
  itkCompositeTransformIOHelper.cxx
  itkTransformFileReader.cxx
  itkTransformFileWriterSpecializations.cxx
  itkTransformIOBase.cxx
  itkTransformIOFactory.cxx
)

itk_module_add_library(ITKIOTransformBase ${ITKIOTransformBase_SRCS})
target_compile_definitions(
  ITKIOTransformBase
  PRIVATE
    "ITK_TRANSFORM_FACTORY_MAX_DIM=${ITK_TRANSFORM_FACTORY_MAX_DIM}"
)
