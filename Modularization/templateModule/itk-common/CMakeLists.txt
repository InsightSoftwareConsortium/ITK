cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(itk-common)
find_package(itk-base 4.0 EXACT REQUIRED)

set(ITK_VERSION_MAJOR "4")
set(ITK_VERSION_MINOR "0")
set(ITK_VERSION_PATCH "0")

include(CMakeBackwardCompatibilityC)
include(CMakeBackwardCompatibilityCXX)

configure_file(src/itkConfigure.h.in itkConfigure.h)
configure_file(src/itkFloatingPointExceptionsConfigure.h.in  itkFloatingPointExceptionsConfigure.h)

set(itk-common_INCLUDE_DIRS
  ${itk-common_BINARY_DIR}
  )

itk_module_impl()

install(FILES
  ${itk-common_BINARY_DIR}/itkConfigure.h
  DESTINATION ${itk-common_INSTALL_INCLUDE_DIR}
  )
