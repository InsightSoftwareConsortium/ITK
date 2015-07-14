set(ITKIOMesh_SRC
  itkMeshFileReaderException.cxx
  itkBYUMeshIO.cxx
  itkBYUMeshIOFactory.cxx
  itkFreeSurferAsciiMeshIO.cxx
  itkFreeSurferAsciiMeshIOFactory.cxx
  itkFreeSurferBinaryMeshIO.cxx
  itkFreeSurferBinaryMeshIOFactory.cxx
  itkGiftiMeshIO.cxx
  itkGiftiMeshIOFactory.cxx
  itkMeshIOBase.cxx
  itkMeshIOFactory.cxx
  itkOBJMeshIO.cxx
  itkOBJMeshIOFactory.cxx
  itkOFFMeshIO.cxx
  itkOFFMeshIOFactory.cxx
  itkVTKPolyDataMeshIO.cxx
  itkVTKPolyDataMeshIOFactory.cxx
)

add_library(ITKIOMesh ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOMesh_SRC})
itk_module_link_dependencies()

itk_module_target(ITKIOMesh)
