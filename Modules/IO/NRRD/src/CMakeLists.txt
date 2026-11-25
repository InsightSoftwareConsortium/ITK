set(
  ITKIONRRD_SRCS
  itkNrrdImageIO.cxx
  itkNrrdImageIOFactory.cxx
)

itk_module_add_library(ITKIONRRD ${ITKIONRRD_SRCS})
