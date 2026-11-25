set(
  ITKIOSiemens_SRCS
  itkSiemensVisionImageIO.cxx
  itkSiemensVisionImageIOFactory.cxx
)

itk_module_add_library(ITKIOSiemens ${ITKIOSiemens_SRCS})
