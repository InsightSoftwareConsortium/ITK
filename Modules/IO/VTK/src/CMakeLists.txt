set(
  ITKIOVTK_SRCS
  itkVTKImageIOFactory.cxx
  itkVTKImageIO.cxx
  itkVTIImageIOFactory.cxx
  itkVTIImageIO.cxx
)

itk_module_add_library(ITKIOVTK ${ITKIOVTK_SRCS})
