configure_file(
  itkMINCImageIOConfigurePrivate.h.in
  itkMINCImageIOConfigurePrivate.h
  @ONLY
)
include_directories("${CMAKE_CURRENT_BINARY_DIR}")

set(
  ITKIOTransformMINC_SRC
  itkMINCTransformIO.cxx
  itkMINCTransformIOFactory.cxx
)

itk_module_add_library(
  ITKIOTransformMINC
  ${ITKIOTransformMINC_SRC}
)
