project(ITK-OpenJPEG)
set(ITK-OpenJPEG_THIRD_PARTY 1)

set(ITK-OpenJPEG_INCLUDE_DIRS
  ${ITK-OpenJPEG_BINARY_DIR}/src/openjpeg
  ${ITK-OpenJPEG_SOURCE_DIR}/src/openjpeg
  )
set(ITK-OpenJPEG_LIBRARIES itkopenjpeg)

itk_module_impl()
