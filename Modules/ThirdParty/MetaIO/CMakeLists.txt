project(ITKMetaIO)
set(ITKMetaIO_THIRD_PARTY 1)

set(ITKMetaIO_INCLUDE_DIRS
  ${ITKMetaIO_BINARY_DIR}/src/MetaIO
  ${ITKMetaIO_SOURCE_DIR}/src/MetaIO
  )
set(ITKMetaIO_LIBRARIES ITKMetaIO)

itk_module_impl()
