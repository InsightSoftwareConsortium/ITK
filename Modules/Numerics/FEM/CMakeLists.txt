project(ITKFEM)
set(ITKFEM_LIBRARIES ITKFEM)
if(ITK_TEMPLATE_VISIBILITY_DEFAULT)
  add_definitions( "-DVNL_TEMPLATE_EXPORT=__attribute__((visibility(\"default\")))")
endif()
itk_module_impl()
# Because dsrc2c.c is 'C' and not 'c++' (this is only required for CMake 2.8.12, and could
# be removed when ITK minimum requirement is CMake 3.0)
set_property(TARGET ITKFEM PROPERTY VISIBILITY_INLINES_HIDDEN 0)
