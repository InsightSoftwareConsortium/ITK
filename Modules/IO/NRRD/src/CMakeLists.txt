set(ITKIONRRD_SRCS
  itkNrrdImageIOFactory.cxx
  itkNrrdImageIO.cxx
  )

itk_module_add_library(ITKIONRRD ${ITKIONRRD_SRCS})
