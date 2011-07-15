project(ITKExpat)
set(ITKExpat_THIRD_PARTY 1)

set(ITKExpat_INCLUDE_DIRS
  ${ITKExpat_BINARY_DIR}/src/expat
  ${ITKExpat_SOURCE_DIR}/src/expat
  )
set(ITKExpat_LIBRARIES ITKEXPAT)

itk_module_impl()
