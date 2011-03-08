cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-JPEG)
find_package(itk-base 4.0 EXACT REQUIRED)

option(ITK_USE_SYSTEM_JPEG "Use an outside build of JPEG." OFF)
mark_as_advanced(ITK_USE_SYSTEM_JPEG)

if(ITK_USE_SYSTEM_JPEG)
  find_package(JPEG REQUIRED)
  set(ITK-JPEG_INCLUDE_DIRS
   ${JPEG_INCLUDE_DIRS}
)
  set(ITK-JPEG_LIBRARIES itkjpeg)
  set(itk-module-config-install "
list(APPEND ITK-JPEG_LIBRARIES \"${JPEG_LIBRARIES}\")
list(APPEND ITK-JPEG_INCLUDE_DIRS \"${ITK-JPEG_INCLUDE_DIRS}\")
")
  set(itk-module-config-build "
list(APPEND ITK-JPEG_LIBRARIES \"${JPEG_LIBRARIES}\")
list(APPEND ITK-JPEG_LIBRARY_DIRS \"${ITK-JPEG_LIBRARY_DIR}\")
")
  set(itk-module-no-include 1)
  set(itk-module-no-src 1)
else()
  set(ITK-JPEG-target itkjpeg)
  set(ITK-JPEG_INCLUDE_DIRS
    ${ITK-JPEG_BINARY_DIR}
    ${ITK-JPEG_SOURCE_DIR}/include/itkjpeg
    )
  set(ITK-JPEG_LIBRARIES itkjpeg)
endif()

configure_file(src/itk_jpeg.h.in itk_jpeg.h)

itk_module_impl()

install(FILES ${ITK-JPEG_BINARY_DIR}/itk_jpeg.h
  DESTINATION ${ITK-JPEG_INSTALL_INCLUDE_DIR})
