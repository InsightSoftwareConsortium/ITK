set(ITKIOLSM_SRCS
  itkLSMImageIOFactory.cxx
  itkLSMImageIO.cxx
  )

itk_module_add_library(ITKIOLSM ${ITKIOLSM_SRCS})
