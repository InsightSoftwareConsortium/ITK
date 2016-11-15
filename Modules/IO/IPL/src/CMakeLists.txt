set(ITKIOIPL_SRCS
  itkIPLCommonImageIO.cxx
  itkIPLFileNameList.cxx
  )

itk_module_add_library(ITKIOIPL ${ITKIOIPL_SRCS})
