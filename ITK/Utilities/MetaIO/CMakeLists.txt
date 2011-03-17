project(ITK-MetaIO)

set(ITK-MetaIO_INCLUDE_DIRS
  ${ITK-MetaIO_SOURCE_DIR}/src/MetaIO
  )
set(ITK-MetaIO_LIBRARIES ITKMetaIO)

itk_module_impl()
