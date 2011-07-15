project(ITKDICOMParser)
set(ITKDICOMParser_THIRD_PARTY 1)

set(ITKDICOMParser_INCLUDE_DIRS
  ${ITKDICOMParser_BINARY_DIR}/src/DICOMParser
  ${ITKDICOMParser_SOURCE_DIR}/src/DICOMParser
  )
set(ITKDICOMParser_LIBRARIES ITKDICOMParser)

itk_module_impl()
