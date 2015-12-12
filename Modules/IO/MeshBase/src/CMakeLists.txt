set(ITKIOMeshBase_SRC
  itkMeshFileReaderException.cxx
  itkMeshFileWriterException.cxx
  itkMeshIOBase.cxx
  itkMeshIOFactory.cxx
  itkMeshIOInstantiation.cxx
)

add_library(ITKIOMeshBase ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOMeshBase_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOMeshBase)
