set(
  ITKIOVTK_SRCS
  itkVTKImageIO.cxx
  itkVTKImageIOFactory.cxx
)

itk_module_add_library(ITKIOVTK ${ITKIOVTK_SRCS})
