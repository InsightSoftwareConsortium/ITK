project(ITKKWSys)

set(KWSYS_HEADER_ROOT ${ITKKWSys_BINARY_DIR}/src)
set(ITKKWSys_INCLUDE_DIRS
  ${KWSYS_HEADER_ROOT}
  )
set(ITKKWSys_LIBRARIES itksys)
itk_module_impl()
