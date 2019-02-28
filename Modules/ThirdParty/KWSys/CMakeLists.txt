project(ITKKWSys)
set(ITKKWSys_THIRD_PARTY 1)

set(KWSYS_HEADER_ROOT ${ITKKWSys_BINARY_DIR}/src)
set(ITKKWSys_INCLUDE_DIRS
  ${KWSYS_HEADER_ROOT}
  )
set(ITKKWSys_LIBRARIES itksys)

# Use Unicode instead of ASCII by default
set(KWSYS_ENCODING_DEFAULT_CODEPAGE CP_UTF8)

itk_module_impl()
