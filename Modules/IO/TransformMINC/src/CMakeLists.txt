set(
  ITKIOTransformMINC_SRC
  itkMINCTransformIO.cxx
  itkMINCTransformIOFactory.cxx
)

itk_module_add_library(
  ITKIOTransformMINC
  ${ITKIOTransformMINC_SRC}
)
