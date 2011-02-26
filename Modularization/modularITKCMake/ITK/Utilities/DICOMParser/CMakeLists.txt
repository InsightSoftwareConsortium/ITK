cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-DICOMParser)
find_package(itk-base 4.0 EXACT REQUIRED)
set(ITK-DICOMParser_LIBRARIES ITK-DICOMParser)


CONFIGURE_FILE(src/DICOMCMakeConfig.h.in DICOMCMakeConfig.h)


itk_module_impl()

install(FILES ${ITK-DICOMParser_BINARY_DIR}/DICOMCMakeConfig.h
DESTINATION ${ITK-DICOMParser_INSTALL_INCLUDE_DIR})
