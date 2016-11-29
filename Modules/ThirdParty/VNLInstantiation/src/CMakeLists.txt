set(ITKVNLInstantiation_SRCS
  vnl_vector+ulonglong-.cxx
  vnl_fft_base+float.3-.cxx
  vnl_fft_base+float.4-.cxx
  vnl_matrix_fixed_ref+double.2.2-.cxx
  vnl_matrix_fixed_ref+double.2.3-.cxx
  vnl_fft_base+double.3-.cxx
  vnl_fft_base+double.4-.cxx
  vnl_c_vector+ushort-.cxx
  vnl_vector+ushort-.cxx
  vnl_matrix_fixed+double.3.12-.cxx
  vnl_c_vector+char-.cxx
  vnl_c_vector+short-.cxx
  vnl_matrix+short-.cxx
  vnl_matrix_fixed_ref+double.3.12-.cxx
  vnl_matrix_fixed_ref+double.4.4-.cxx
  vnl_matrix+char-.cxx
  vnl_vector+char-.cxx
  vnl_matrix+longlong-.cxx
  vnl_c_vector+ulonglong-.cxx
  vnl_matrix_fixed_ref+double.4.3-.cxx
  vnl_vector+longlong-.cxx
  vnl_matrix+ulonglong-.cxx
  vnl_c_vector+longlong-.cxx
  vnl_vector+short-.cxx
  vnl_matrix+ushort-.cxx
  )

itk_module_add_library(ITKVNLInstantiation ${ITKVNLInstantiation_SRCS})
target_link_libraries(ITKVNLInstantiation LINK_PUBLIC ${ITKVNL_LIBRARIES})
