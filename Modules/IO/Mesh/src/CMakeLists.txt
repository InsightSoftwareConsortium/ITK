set(ITKIOMesh_SRCS
  itkOFFMeshIO.cxx
  itkOFFMeshIOFactory.cxx
  )

itk_module_add_library(ITKIOMesh ${ITKIOMesh_SRCS})
