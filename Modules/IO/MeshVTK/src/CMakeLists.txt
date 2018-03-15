set(ITKIOMeshVTK_SRCS
  itkVTKPolyDataMeshIO.cxx
  itkVTKPolyDataMeshIOFactory.cxx
  )

itk_module_add_library(ITKIOMeshVTK ${ITKIOMeshVTK_SRCS})
