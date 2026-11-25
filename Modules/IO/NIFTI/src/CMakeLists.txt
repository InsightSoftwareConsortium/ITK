configure_file(
  itkNiftiImageIOConfigurePrivate.h.in
  itkNiftiImageIOConfigurePrivate.h
  @ONLY
)
include_directories("${CMAKE_CURRENT_BINARY_DIR}")

set(
  ITKIONIFTI_SRCS
  itkNiftiImageIO.cxx
  itkNiftiImageIOFactory.cxx
)

itk_module_add_library(ITKIONIFTI ${ITKIONIFTI_SRCS})
