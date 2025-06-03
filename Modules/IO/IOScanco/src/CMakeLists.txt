set(IOScanco_SRCS
  itkScancoDataManipulation.cxx
  itkScancoImageIO.cxx
  itkScancoImageIOFactory.cxx
  )

itk_module_add_library(IOScanco ${IOScanco_SRCS})
