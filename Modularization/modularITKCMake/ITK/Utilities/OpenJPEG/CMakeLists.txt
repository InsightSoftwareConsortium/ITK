cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-OpenJPEG)
find_package(itk-base 4.0 EXACT REQUIRED)

option(ITK_USE_SYSTEM_OPENJPEG "Use system-installed openjpeg" OFF)
mark_as_advanced(ITK_USE_SYSTEM_OPENJPEG)

if(ITK_USE_SYSTEM_OPENJPEG)
  find_package(OPENJPEG REQUIRED)
  set(ITK-OpenJPEG_INCLUDE_DIRS
    ${OPENJPEG_INCLUDE_DIRS}
    )
  set(ITK-OpenJPEG_LIBRARIES itkopenjpeg)
  set(itk-module-config-install "
list(APPEND ITK-OpenJPEG_LIBRARIES \"${OPENJPEG_LIBRARIES}\")
list(APPEND ITK-OpenJPEG_INCLUDE_DIRS \"${ITK-OpenJPEG_INCLUDE_DIRS}\")
")
  set(itk-module-config-build "
list(APPEND ITK-OpenJPEG_LIBRARIES \"${OPENJPEG_LIBRARIES}\")
list(APPEND ITK-OpenJPEG_INCLUDE_DIRS \"${ITK-OpenJPEG_INCLUDE_DIRS}\")
")
  set(itk-module-no-include 1)
  set(itk-module-no-src 1)
else()
  set(ITK-OpenJPEG-target itkopenjpeg)
  set(ITK-OpenJPEG_INCLUDE_DIRS
    ${ITK-OpenJPEG_BINARY_DIR}
    ${ITK-OpenJPEG_SOURCE_DIR}/src
    )
  set(ITK-OpenJPEG_LIBRARIES itkopenjpeg)
endif()


itk_module_impl()
