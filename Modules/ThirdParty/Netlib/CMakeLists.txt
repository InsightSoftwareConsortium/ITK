project(ITK-Netlib)
set(ITK-Netlib_THIRD_PARTY 1)

set(ITK-Netlib_INCLUDE_DIRS
   ${ITK-Netlib_BINARY_DIR})
set(ITK-Netlib_LIBRARIES itkNetlibSlatec)

itk_module_impl()
