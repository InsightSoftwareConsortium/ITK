set(ITKIOTransformBase_SRCS
  itkCompositeTransformIOHelper.cxx
  itkTransformFileReader.cxx
  itkTransformFileWriterSpecializations.cxx
  itkTransformIOBase.cxx
  itkTransformIOFactory.cxx
  )

itk_module_add_library(ITKIOTransformBase ${ITKIOTransformBase_SRCS})
