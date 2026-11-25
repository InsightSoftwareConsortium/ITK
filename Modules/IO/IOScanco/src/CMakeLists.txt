set(
  IOScanco_SRCS
  itkScancoDataManipulation.cxx
  itkScancoHeaderIO.cxx
  itkAIMHeaderIO.cxx
  itkISQHeaderIO.cxx
  itkScancoImageIO.cxx
  itkScancoImageIOFactory.cxx
)

itk_module_add_library(IOScanco ${IOScanco_SRCS})

target_compile_definitions(IOScanco PRIVATE _CRT_SECURE_NO_WARNINGS)
