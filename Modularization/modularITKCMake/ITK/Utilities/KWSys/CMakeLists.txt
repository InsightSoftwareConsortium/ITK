cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-KWSys)
find_package(itk-base 4.0 EXACT REQUIRED)

set(KWSYS_HEADER_ROOT ${ITK-KWSys_BINARY_DIR}/src)
set(ITK-KWSys_INCLUDE_DIRS
  ${KWSYS_HEADER_ROOT}
  )
set(ITK-KWSys-target itksys)
set(ITK-KWSys_LIBRARIES itksys)
itk_module_impl()
