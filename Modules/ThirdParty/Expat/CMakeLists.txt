project(ITK-Expat)
set(ITK-Expat_THIRD_PARTY 1)

set(ITK-Expat_INCLUDE_DIRS
  ${ITK-Expat_BINARY_DIR}/src/expat
  ${ITK-Expat_SOURCE_DIR}/src/expat
  )
set(ITK-Expat_LIBRARIES ITKEXPAT)

itk_module_impl()
