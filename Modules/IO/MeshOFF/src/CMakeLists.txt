set(ITKIOMeshOFF_SRCS
  itkOFFMeshIO.cxx
  itkOFFMeshIOFactory.cxx
  )

itk_module_add_library(ITKIOMeshOFF ${ITKIOMeshOFF_SRCS})
