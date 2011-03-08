cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-ZLIB)
find_package(itk-base 4.0 EXACT REQUIRED)

option(ITK_USE_SYSTEM_ZLIB "Use system-installed zlib" OFF)
mark_as_advanced(ITK_USE_SYSTEM_ZLIB)

if(ITK_USE_SYSTEM_ZLIB)
  find_package(ZLIB REQUIRED)
  set(ITK-ZLIB_INCLUDE_DIRS
    ${ZLIB_INCLUDE_DIRS}
    )
  set(ITK-ZLIB_LIBRARIES itkzlib)
  set(itk-module-config-install "
list(APPEND ITK-ZLIB_LIBRARIES \"${ZLIB_LIBRARIES}\")
list(APPEND ITK-ZLIB_INCLUDE_DIRS \"${ITK-ZLIB_INCLUDE_DIRS}\")
")
  set(itk-module-config-build "
list(APPEND ITK-ZLIB_LIBRARIES \"${ZLIB_LIBRARIES}\")
list(APPEND ITK-ZLIB_INCLUDE_DIRS \"${ITK-ZLIB_INCLUDE_DIRS}\")
")
  set(itk-module-no-include 1)
  set(itk-module-no-src 1)
else()
  set(ITK-ZLIB-target itkzlib)
  set(ITK-ZLIB_INCLUDE_DIRS
    ${ITK-ZLIB_BINARY_DIR}
    )
  set(ITK-ZLIB_LIBRARIES itkzlib)
endif()

configure_file(src/itk_zlib.h.in itk_zlib.h)

itk_module_impl()

install(FILES ${ITK-ZLIB_BINARY_DIR}/itk_zlib.h
  DESTINATION ${ITK-ZLIB_INSTALL_INCLUDE_DIR})
