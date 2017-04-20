project(ITKNetlib)
set(ITKNetlib_THIRD_PARTY 1)

set(ITKNetlib_INCLUDE_DIRS
   ${ITKNetlib_BINARY_DIR})
set(ITKNetlib_LIBRARIES itkNetlibSlatec)

itk_module_impl()
