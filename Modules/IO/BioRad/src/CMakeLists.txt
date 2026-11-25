set(
  ITKIOBioRad_SRCS
  itkBioRadImageIO.cxx
  itkBioRadImageIOFactory.cxx
)

itk_module_add_library(ITKIOBioRad ${ITKIOBioRad_SRCS})
