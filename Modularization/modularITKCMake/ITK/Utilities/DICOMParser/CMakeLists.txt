cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-DICOMParser)
find_package(itk-base 4.0 EXACT REQUIRED)

set(DICOMPARSER_NAMESPACE itkdicomparser)

set(ITK-DICOMParser-target dicomparser )
set(ITK-DICOMParser_INCLUDE_DIRS  ${ITK-DICOMParser_BINARY_DIR})
set(ITK-DICOMParser_LIBRARIES dicomparser)

itk_module_impl()
