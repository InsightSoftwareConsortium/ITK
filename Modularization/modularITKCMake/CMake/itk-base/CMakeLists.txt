cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(itk-base NONE)
include(itk-base-config.cmake)
install(FILES
  itk-base-config.cmake
  itk-base-config-version.cmake
  itk-module-config-build.cmake.in
  itk-module-config-install.cmake.in
  itk-module-config-version.cmake.in
  DESTINATION lib/cmake/itk-base-${ITK_VERSION}
  )
