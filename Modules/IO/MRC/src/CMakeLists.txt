set(ITKIOMRC_SRCS
  itkMRCHeaderObject.cxx
  itkMRCImageIO.cxx
  itkMRCImageIOFactory.cxx
  )

itk_module_add_library(ITKIOMRC ${ITKIOMRC_SRCS})
