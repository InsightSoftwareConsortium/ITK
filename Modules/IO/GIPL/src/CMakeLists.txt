set(
  ITKIOGIPL_SRCS
  itkGiplImageIO.cxx
  itkGiplImageIOFactory.cxx
)

itk_module_add_library(ITKIOGIPL ${ITKIOGIPL_SRCS})
