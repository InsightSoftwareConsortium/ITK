project(ITK-KWSys)

set(KWSYS_HEADER_ROOT ${ITK-KWSys_BINARY_DIR}/src)
set(ITK-KWSys_INCLUDE_DIRS
  ${KWSYS_HEADER_ROOT}
  )
set(ITK-KWSys_LIBRARIES itksys)
itk_module_impl()
