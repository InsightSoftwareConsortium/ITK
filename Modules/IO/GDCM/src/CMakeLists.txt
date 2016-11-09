set(ITKIOGDCM_SRCS
  itkGDCMImageIO.cxx
  itkGDCMImageIOFactory.cxx
  itkGDCMSeriesFileNames.cxx
  )

itk_module_add_library(ITKIOGDCM ${ITKIOGDCM_SRCS})
