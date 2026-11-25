configure_file(
  itkMINCImageIOConfigurePrivate.h.in
  itkMINCImageIOConfigurePrivate.h
  @ONLY
)
include_directories("${CMAKE_CURRENT_BINARY_DIR}")

set(
  ITKIOMINC_SRC
  itkMINCImageIO.cxx
  itkMINCImageIOFactory.cxx
)

itk_module_add_library(
  ITKIOMINC
  ${ITKIOMINC_SRC}
)
