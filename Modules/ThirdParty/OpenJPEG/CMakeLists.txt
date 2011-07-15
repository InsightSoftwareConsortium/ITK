project(ITKOpenJPEG)
set(ITKOpenJPEG_THIRD_PARTY 1)

set(ITKOpenJPEG_INCLUDE_DIRS
  ${ITKOpenJPEG_BINARY_DIR}/src/openjpeg
  ${ITKOpenJPEG_SOURCE_DIR}/src/openjpeg
  )
set(ITKOpenJPEG_LIBRARIES itkopenjpeg)

itk_module_impl()
