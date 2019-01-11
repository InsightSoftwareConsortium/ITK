set(ITKIOMesh_SRCS
  itkOBJMeshIO.cxx
  itkOBJMeshIOFactory.cxx
  itkOFFMeshIO.cxx
  itkOFFMeshIOFactory.cxx
  )

itk_module_add_library(ITKIOMesh ${ITKIOMesh_SRCS})
