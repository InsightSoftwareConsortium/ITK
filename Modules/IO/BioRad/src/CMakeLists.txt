set(ITKIOBioRad_SRCS
  itkBioRadImageIOFactory.cxx
  itkBioRadImageIO.cxx
  )

itk_module_add_library(ITKIOBioRad ${ITKIOBioRad_SRCS})
