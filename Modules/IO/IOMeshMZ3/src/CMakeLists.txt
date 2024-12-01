set(IOMeshMZ3_SRCS
  itkMZ3MeshIO.cxx itkMZ3MeshIOFactory.cxx
  )

itk_module_add_library(IOMeshMZ3 ${IOMeshMZ3_SRCS})
