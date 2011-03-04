cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-NrrdIO)
find_package(itk-base 4.0 EXACT REQUIRED)

option(ITK_USE_SYSTEM_NRRD "Use system-installed nrrd" OFF)
mark_as_advanced(ITK_USE_SYSTEM_NRRD)

if(ITK_USE_SYSTEM_NRRD)
  find_package(NRRD REQUIRED)
  set(ITK-NrrdIO_INCLUDE_DIRS
    ${NRRD_INCLUDE_DIRS}
    )
  set(ITK-NrrdIO_LIBRARIES itknrrd)
  set(itk-module-config-install "
list(APPEND ITK-NrrdIO_LIBRARIES \"${NRRD_LIBRARIES}\")
list(APPEND ITK-NrrdIO_INCLUDE_DIRS \"${ITK-NrrdIO_INCLUDE_DIRS}\")
")
  set(itk-module-config-build "
list(APPEND ITK-NrrdIO_LIBRARIES \"${NRRD_LIBRARIES}\")
list(APPEND ITK-NrrdIO_INCLUDE_DIRS \"${ITK-NrrdIO_INCLUDE_DIRS}\")
")
  set(itk-module-no-include 1)
  set(itk-module-no-src 1)
else()
  set(ITK-NrrdIO-target itknrrd)
  set(ITK-NrrdIO_INCLUDE_DIRS
    ${ITK-NrrdIO_BINARY_DIR}
    ${ITK-NrrdIO_SOURCE_DIR}/include
    )
  set(ITK-NrrdIO_LIBRARIES itknrrd)
endif()


itk_module_impl()
