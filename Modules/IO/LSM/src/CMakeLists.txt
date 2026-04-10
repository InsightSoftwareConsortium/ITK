set(
  ITKIOLSM_SRCS
  itkLSMImageIO.cxx
  itkLSMImageIOFactory.cxx
)

itk_module_add_library(ITKIOLSM ${ITKIOLSM_SRCS})
