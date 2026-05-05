project(IOMeshMZ3)

set(IOMeshMZ3_LIBRARIES IOMeshMZ3)

set(ITK_DIR ${CMAKE_BINARY_DIR})
itk_module_impl()

itk_module_examples()
