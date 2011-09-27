project(ITKNrrdIO)
set(ITKNrrdIO_THIRD_PARTY 1)

set(ITKNrrdIO_INCLUDE_DIRS
  ${ITKNrrdIO_BINARY_DIR}/src/NrrdIO
  ${ITKNrrdIO_SOURCE_DIR}/src/NrrdIO
  )
set(ITKNrrdIO_LIBRARIES ITKNrrdIO)

itk_module_impl()
