
set(ITKVNLInstantiation_SRCS
vnl_svd_fixed+double.2.3-.cxx
vnl_svd_fixed+double.3.2-.cxx
vnl_svd_fixed+float.2.3-.cxx
vnl_svd_fixed+float.3.2-.cxx
vnl_fortran_copy_fixed+double.2.3-.cxx
vnl_fortran_copy_fixed+double.3.2-.cxx
vnl_fortran_copy_fixed+float.2.3-.cxx
vnl_fortran_copy_fixed+float.3.2-.cxx

vnl_matrix_fixed+double.3.12-.cxx
vnl_matrix_fixed_ref+double.2.3-.cxx
vnl_matrix_fixed_ref+double.3.12-.cxx
vnl_matrix_fixed_ref+double.4.3-.cxx
  )

itk_module_add_library(ITKVNLInstantiation ${ITKVNLInstantiation_SRCS})
target_link_libraries(ITKVNLInstantiation LINK_PUBLIC ${ITKVNL_LIBRARIES})
