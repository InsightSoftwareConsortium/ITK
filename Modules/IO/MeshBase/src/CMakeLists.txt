set(ITKIOMeshBase_SRCS
  itkMeshFileReaderException.cxx
  itkMeshFileWriterException.cxx
  itkMeshIOBase.cxx
  itkMeshIOFactory.cxx
)

itk_module_add_library(ITKIOMeshBase ${ITKIOMeshBase_SRCS})
