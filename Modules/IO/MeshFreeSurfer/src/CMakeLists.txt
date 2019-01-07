set(ITKIOMeshFreeSurfer_SRCS
  itkFreeSurferAsciiMeshIO.cxx
  itkFreeSurferAsciiMeshIOFactory.cxx
  itkFreeSurferBinaryMeshIO.cxx
  itkFreeSurferBinaryMeshIOFactory.cxx
  )

itk_module_add_library(ITKIOMeshFreeSurfer ${ITKIOMeshFreeSurfer_SRCS})
