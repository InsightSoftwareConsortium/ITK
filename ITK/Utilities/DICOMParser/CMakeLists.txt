cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-DICOMParser)
find_package(itk-base 4.0 EXACT REQUIRED)

set(ITK-DICOMParser-target ITKDICOMParser )
set(ITK-DICOMParser_INCLUDE_DIRS
  ${ITK-DICOMParser_BINARY_DIR}/src/DICOMParser
  ${ITK-DICOMParser_SOURCE_DIR}/src/DICOMParser
  )
set(ITK-DICOMParser_LIBRARIES ITKDICOMParser)

itk_module_impl()
