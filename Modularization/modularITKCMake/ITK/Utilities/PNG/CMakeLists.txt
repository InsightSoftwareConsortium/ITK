cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-PNG)
find_package(itk-base 4.0 EXACT REQUIRED)

option(ITK_USE_SYSTEM_PNG "Use system-installed png" OFF)
mark_as_advanced(ITK_USE_SYSTEM_PNG)

if(ITK_USE_SYSTEM_PNG)
  find_package(PNG REQUIRED)
  set(ITK-PNG_INCLUDE_DIRS
    ${PNG_INCLUDE_DIRS}
    )
  set(ITK-PNG_LIBRARIES itkpng)
  set(itk-module-config-install "
list(APPEND ITK-PNG_LIBRARIES \"${PNG_LIBRARIES}\")
list(APPEND ITK-PNG_INCLUDE_DIRS \"${ITK-PNG_INCLUDE_DIRS}\")
")
  set(itk-module-config-build "
list(APPEND ITK-PNG_LIBRARIES \"${PNG_LIBRARIES}\")
list(APPEND ITK-PNG_INCLUDE_DIRS \"${ITK-PNG_INCLUDE_DIRS}\")
")
  set(itk-module-no-include 1)
  set(itk-module-no-src 1)
else()
  set(ITK-PNG-target itkpng)
  set(ITK-PNG_INCLUDE_DIRS
    ${ITK-PNG_BINARY_DIR}
    ${ITK-PNG_SOURCE_DIR}/src
    ${ITK-PNG_SOURCE_DIR}/include/itkpng
    )
  set(ITK-PNG_LIBRARIES itkpng)
endif()

configure_file(src/itk_png.h.in itk_png.h)

itk_module_impl()

install(FILES ${ITK-PNG_BINARY_DIR}/itk_png.h
  DESTINATION ${ITK-PNG_INSTALL_INCLUDE_DIR})
