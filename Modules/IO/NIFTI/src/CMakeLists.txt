set(ITKIONIFTI_SRCS
  itkNiftiImageIOFactory.cxx
  itkNiftiImageIO.cxx
  )

itk_module_add_library(ITKIONIFTI ${ITKIONIFTI_SRCS})
