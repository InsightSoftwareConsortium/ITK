cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-Expat)
find_package(itk-base 4.0 EXACT REQUIRED)

option(ITK_USE_SYSTEM_EXPAT "Use system-installed EXPAT" OFF)
mark_as_advanced(ITK_USE_SYSTEM_EXPAT)

if(ITK_USE_SYSTEM_EXPAT)
  find_package(EXPAT REQUIRED)
  set(ITK-Expat_INCLUDE_DIRS
    ${EXPAT_INCLUDE_DIRS}
    )
  set(ITK-Expat_LIBRARIES ITKEXPAT)
  set(itk-module-config-install "
list(APPEND ITK-Expat_LIBRARIES \"${EXPAT_LIBRARIES}\")
list(APPEND ITK-Expat_INCLUDE_DIRS \"${ITK-Expat_INCLUDE_DIRS}\")
")
  set(itk-module-config-build "
list(APPEND ITK-Expat_LIBRARIES \"${EXPAT_LIBRARIES}\")
list(APPEND ITK-Expat_INCLUDE_DIRS \"${ITK-Expat_INCLUDE_DIRS}\")
")
  set(itk-module-no-src 1)
else()
  set(ITK-Expat-target ITKEXPAT)
  set(ITK-Expat_INCLUDE_DIRS
    ${ITK-Expat_BINARY_DIR}
    )
  set(ITK-Expat_LIBRARIES ITKEXPAT)
endif()


itk_module_impl()
