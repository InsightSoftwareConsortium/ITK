set(ITKIOSiemens_SRCS
  itkSiemensVisionImageIOFactory.cxx
  itkSiemensVisionImageIO.cxx
  )

itk_module_add_library(ITKIOSiemens ${ITKIOSiemens_SRCS})
