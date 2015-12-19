set(ITKIOMeshBase_SRC
  itkMeshFileReader.cxx
  itkMeshFileReaderException.cxx
  itkMeshFileWriter.cxx
  itkMeshFileWriterException.cxx
  itkMeshIOBase.cxx
  itkMeshIOFactory.cxx
)

add_library(ITKIOMeshBase ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOMeshBase_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOMeshBase)
