set(ITKIOMeshGifti_SRCS
  itkGiftiMeshIO.cxx
  itkGiftiMeshIOFactory.cxx
  )

itk_module_add_library(ITKIOMeshGifti ${ITKIOMeshGifti_SRCS})
