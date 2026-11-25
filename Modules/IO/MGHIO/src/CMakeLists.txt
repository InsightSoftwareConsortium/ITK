set(
  MGHIO_SRC
  itkMGHImageIOFactory.cxx
  itkMGHImageIO.cxx
)

itk_module_add_library(MGHIO ${MGHIO_SRC})
