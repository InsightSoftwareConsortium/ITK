project(ITKNetlib)
set(ITKNetlib_THIRD_PARTY 1)

set(ITKNetlib_INCLUDE_DIRS
   ${ITKNetlib_BINARY_DIR})
set(ITKNetlib_LIBRARIES itkNetlibSlatec)
set(ITK_LIBRARY_PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS FALSE)

itk_module_impl()
