set(IOScanco_SRCS
  itkScancoDataManipulation.cxx
  itkScancoHeaderIO.cxx
  itkAIMHeaderIO.cxx
  itkISQHeaderIO.cxx
  itkScancoImageIO.cxx
  itkScancoImageIOFactory.cxx
  )

itk_module_add_library(IOScanco ${IOScanco_SRCS})
