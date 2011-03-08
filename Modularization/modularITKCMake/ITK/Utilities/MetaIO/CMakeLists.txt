cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-MetaIO)
find_package(itk-base 4.0 EXACT REQUIRED)

option(ITK_USE_SYSTEM_METAIO "Use an outside build of METAIO." OFF)
mark_as_advanced(ITK_USE_SYSTEM_METAIO)

if(ITK_USE_SYSTEM_METAIO)
  find_package(METAIO REQUIRED)
  set(ITK-MetaIO_INCLUDE_DIRS
    ${METAIO_INCLUDE_DIRS}
    )
  set(ITK-MetaIO_LIBRARIES ITKMetaIO)
  set(itk-module-config-install "
list(APPEND itk_LIBRARIES \"${ZLIB_LIBRARIES}\")
list(APPEND ITK-MetaIO_INCLUDE_DIRS \"${ITK-MetaIO_INCLUDE_DIRS}\")
")
  set(itk-module-config-build "
list(APPEND ITK-MetaIO_LIBRARIES \"${ZLIB_LIBRARIES}\")
list(APPEND ITK-MetaIO_INCLUDE_DIRS \"${ITK-MetaIO_INCLUDE_DIRS}\")
")
  set(itk-module-no-include 1)
  set(itk-module-no-src 1)
else()
  set(ITK-MetaIO-target ITKMetaIO)
  set(ITK-MetaIO_INCLUDE_DIRS
    ${ITK-MetaIO_BINARY_DIR}
    ${ITK-MetaIO_SOURCE_DIR}/src/MetaIO
    )
  set(ITK-MetaIO_LIBRARIES ITKMetaIO)
endif()

itk_module_impl()

if (NOT ITK_USE_SYTEM_METAIO)
  itk_module_target_name(ITKMetaIO)
  itk_module_target_export(ITKMetaIO)
endif()
