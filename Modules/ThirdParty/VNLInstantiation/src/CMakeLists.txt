
set(ITKVNLInstantiation_SRCS
  vnl_fft_base+float.3-.cxx
  vnl_fft_base+float.4-.cxx
  vnl_matrix_fixed+double.1.1-.cxx
  vnl_matrix_fixed+double.5.5-.cxx
  vnl_matrix_fixed_ref+double.2.2-.cxx
  vnl_matrix_fixed_ref+double.2.3-.cxx
  vnl_fft_base+double.3-.cxx
  vnl_fft_base+double.4-.cxx
  vnl_matrix_fixed+double.3.12-.cxx
  vnl_matrix_fixed+double.2.3-.cxx

# dims 1-6 provided by vnl upstream ThirdParty
  vnl_matrix_fixed+float.7.7-.cxx
  vnl_matrix_fixed+float.8.8-.cxx
  vnl_matrix_fixed+float.9.9-.cxx
  vnl_matrix_fixed+float.10.10-.cxx
  
# dims 2-4,6 provided by vnl upstream ThirdParty
  vnl_matrix_fixed+double.5.5-.cxx
  vnl_matrix_fixed+double.7.7-.cxx
  vnl_matrix_fixed+double.8.8-.cxx
  vnl_matrix_fixed+double.9.9-.cxx
  vnl_matrix_fixed+double.10.10-.cxx

  vnl_c_vector+char-.cxx
  vnl_c_vector+short-.cxx
  vnl_matrix+short-.cxx
  vnl_matrix_fixed_ref+double.3.12-.cxx
  vnl_matrix_fixed_ref+double.4.4-.cxx
  vnl_matrix+char-.cxx
  vnl_vector+char-.cxx
  vnl_matrix_fixed_ref+double.4.3-.cxx
  vnl_vector+short-.cxx
  vnl_svd_fixed+double.1.1-.cxx
  vnl_svd_fixed+double.2.2-.cxx
  vnl_svd_fixed+double.2.3-.cxx
  vnl_svd_fixed+double.3.2-.cxx
  vnl_svd_fixed+double.4.4-.cxx
  vnl_svd_fixed+double.5.5-.cxx
  vnl_svd_fixed+double.6.6-.cxx
  vnl_svd_fixed+double.7.7-.cxx
  vnl_svd_fixed+double.8.8-.cxx
  vnl_svd_fixed+double.9.9-.cxx
  vnl_svd_fixed+double.10.10-.cxx
  vnl_svd_fixed+float.1.1-.cxx
  vnl_svd_fixed+float.2.2-.cxx
  vnl_svd_fixed+float.2.3-.cxx
  vnl_svd_fixed+float.4.4-.cxx
  vnl_svd_fixed+float.5.5-.cxx
  vnl_svd_fixed+float.6.6-.cxx
  vnl_svd_fixed+float.7.7-.cxx
  vnl_svd_fixed+float.8.8-.cxx
  vnl_svd_fixed+float.9.9-.cxx
  vnl_svd_fixed+float.10.10-.cxx
  vnl_fortran_copy_fixed+double.1.1-.cxx
  vnl_fortran_copy_fixed+double.2.2-.cxx
  vnl_fortran_copy_fixed+double.2.3-.cxx
  vnl_fortran_copy_fixed+double.3.2-.cxx
  vnl_fortran_copy_fixed+double.4.4-.cxx
  vnl_fortran_copy_fixed+double.5.5-.cxx
  vnl_fortran_copy_fixed+double.6.6-.cxx
  vnl_fortran_copy_fixed+double.7.7-.cxx
  vnl_fortran_copy_fixed+double.8.8-.cxx
  vnl_fortran_copy_fixed+double.9.9-.cxx
  vnl_fortran_copy_fixed+double.10.10-.cxx
  vnl_fortran_copy_fixed+float.1.1-.cxx
  vnl_fortran_copy_fixed+float.4.4-.cxx
  vnl_fortran_copy_fixed+float.5.5-.cxx
  vnl_fortran_copy_fixed+float.2.2-.cxx
  vnl_fortran_copy_fixed+float.2.3-.cxx
  vnl_fortran_copy_fixed+float.3.2-.cxx
  vnl_fortran_copy_fixed+float.4.4-.cxx
  vnl_fortran_copy_fixed+float.5.5-.cxx
  vnl_fortran_copy_fixed+float.6.6-.cxx
  vnl_fortran_copy_fixed+float.7.7-.cxx
  vnl_fortran_copy_fixed+float.8.8-.cxx
  vnl_fortran_copy_fixed+float.9.9-.cxx
  vnl_fortran_copy_fixed+float.10.10-.cxx
  vnl_diag_matrix_fixed+double.1-.cxx
  vnl_diag_matrix_fixed+double.2-.cxx
  vnl_diag_matrix_fixed+double.5-.cxx
  vnl_diag_matrix_fixed+double.6-.cxx
  vnl_diag_matrix_fixed+double.7-.cxx
  vnl_diag_matrix_fixed+double.8-.cxx
  vnl_diag_matrix_fixed+double.9-.cxx
  vnl_diag_matrix_fixed+double.10-.cxx
  vnl_diag_matrix_fixed+float.1-.cxx
  vnl_diag_matrix_fixed+float.2-.cxx
  vnl_diag_matrix_fixed+float.5-.cxx
  vnl_diag_matrix_fixed+float.6-.cxx
  vnl_diag_matrix_fixed+float.7-.cxx
  vnl_diag_matrix_fixed+float.8-.cxx
  vnl_diag_matrix_fixed+float.9-.cxx
  vnl_diag_matrix_fixed+float.10-.cxx

  itk_vnl_matlab_print+float-.cxx
  itk_vnl_matlab_print+double-.cxx
  )

itk_module_add_library(ITKVNLInstantiation ${ITKVNLInstantiation_SRCS})
target_link_libraries(ITKVNLInstantiation LINK_PUBLIC ${ITKVNL_LIBRARIES})
