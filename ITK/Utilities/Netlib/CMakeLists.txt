cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-Netlib)
find_package(itk-base 4.0 EXACT REQUIRED)

set(ITK-Netlib-target itkNetlibSlatec)
set(ITK-Netlib_INCLUDE_DIRS
   ${ITK-Netlib_BINARY_DIR})
set(ITK-Netlib_LIBRARIES itkNetlibSlatec)

itk_module_impl()
