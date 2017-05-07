set(IOScanco_SRCS
  itkScancoImageIO.cxx
  itkScancoImageIOFactory.cxx
  )

itk_module_add_library(IOScanco ${IOScanco_SRCS})
