project(ITK-DICOMParser)
set(ITK-DICOMParser_THIRD_PARTY 1)

set(ITK-DICOMParser_INCLUDE_DIRS
  ${ITK-DICOMParser_BINARY_DIR}/src/DICOMParser
  ${ITK-DICOMParser_SOURCE_DIR}/src/DICOMParser
  )
set(ITK-DICOMParser_LIBRARIES ITKDICOMParser)

itk_module_impl()
