project(ITK-NrrdIO)

set(ITK-NrrdIO-target ITKNrrdIO)
set(ITK-NrrdIO_INCLUDE_DIRS
  ${ITK-NrrdIO_BINARY_DIR}/src/NrrdIO
  ${ITK-NrrdIO_SOURCE_DIR}/src
  )
set(ITK-NrrdIO_LIBRARIES ITKNrrdIO)

itk_module_impl()
