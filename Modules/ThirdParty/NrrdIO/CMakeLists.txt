project(ITK-NrrdIO)

set(ITK-NrrdIO_INCLUDE_DIRS
  ${ITK-NrrdIO_BINARY_DIR}/src/NrrdIO
  ${ITK-NrrdIO_SOURCE_DIR}/src/NrrdIO
  )
set(ITK-NrrdIO_LIBRARIES ITKNrrdIO)

itk_module_impl()
