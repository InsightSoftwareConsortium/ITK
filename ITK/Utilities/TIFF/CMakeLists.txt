cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-TIFF)
find_package(itk-base 4.0 EXACT REQUIRED)

option(ITK_USE_SYSTEM_TIFF "Use system-installed tiff" OFF)
mark_as_advanced(ITK_USE_SYSTEM_TIFF)

if(ITK_USE_SYSTEM_TIFF)
  find_package(TIFF REQUIRED)
  set(ITK-TIFF_INCLUDE_DIRS
    ${TIFF_INCLUDE_DIRS}
    )
  set(ITK-TIFF_LIBRARIES itktiff)
  set(itk-module-config-install "
list(APPEND ITK-TIFF_LIBRARIES \"${TIFF_LIBRARIES}\")
list(APPEND ITK-TIFF_INCLUDE_DIRS \"${ITK-TIFF_INCLUDE_DIRS}\")
")
  set(itk-module-config-build "
list(APPEND ITK-TIFF_LIBRARIES \"${TIFF_LIBRARIES}\")
list(APPEND ITK-TIFF_INCLUDE_DIRS \"${ITK-TIFF_INCLUDE_DIRS}\")
")
  set(itk-module-no-include 1)
  set(itk-module-no-src 1)
else()
  set(ITK-TIFF-target itktiff)
  set(ITK-TIFF_INCLUDE_DIRS
    ${ITK-TIFF_BINARY_DIR}/src
    ${ITK-TIFF_SOURCE_DIR}/src
    )
  set(ITK-TIFF_LIBRARIES itktiff)
endif()

configure_file(src/itk_tiff.h.in src/itk_tiff.h)

itk_module_impl()

install(FILES ${ITK-TIFF_BINARY_DIR}/src/itk_tiff.h
  DESTINATION ${ITK-TIFF_INSTALL_INCLUDE_DIR})
