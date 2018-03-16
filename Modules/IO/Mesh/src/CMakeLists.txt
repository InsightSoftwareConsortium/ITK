set(ITKIOMesh_SRCS
  itkFreeSurferAsciiMeshIO.cxx
  itkFreeSurferAsciiMeshIOFactory.cxx
  itkFreeSurferBinaryMeshIO.cxx
  itkFreeSurferBinaryMeshIOFactory.cxx
  itkGiftiMeshIO.cxx
  itkGiftiMeshIOFactory.cxx
  itkOBJMeshIO.cxx
  itkOBJMeshIOFactory.cxx
  itkOFFMeshIO.cxx
  itkOFFMeshIOFactory.cxx
  )

itk_module_add_library(ITKIOMesh ${ITKIOMesh_SRCS})
