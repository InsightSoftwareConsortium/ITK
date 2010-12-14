set(itk-common_SRCS
LIST_OF_CXX_FILES
)

if(WIN32)
   set(itk-common_SRCS ${itk-common_SRCS};itkWin32OutputWindow.cxx;itkSmartPointerForwardReferenceProcessObject.cxx)
endif(WIN32)


# Instantiations not provided by vnl but used by ITK.
add_library(itkvnl_inst
  vnl_c_vector+short-.cxx
  vnl_c_vector+ushort-.cxx
  vnl_c_vector+char-.cxx
  vnl_matrix+ushort-.cxx
  vnl_matrix+short-.cxx
  vnl_matrix+char-.cxx
  vnl_matrix_fixed+double.3.12-.cxx
  vnl_matrix_fixed_ref+double.2.2-.cxx
  vnl_matrix_fixed_ref+double.2.3-.cxx
  vnl_matrix_fixed_ref+double.3.12-.cxx
  vnl_matrix_fixed_ref+double.4.3-.cxx
  vnl_matrix_fixed_ref+double.4.4-.cxx
  vnl_vector+short-.cxx
  vnl_vector+ushort-.cxx
  vnl_vector+char-.cxx
  vnl_vector+size_t-.cxx
  vnl_fft_base+double.3-.cxx
  vnl_fft_base+float.3-.cxx
  )
itk_module_target(itkvnl_inst)

#### genearting libraries
add_library(itk-common ${itk-common_SRCS})
target_link_libraries(itkvnl_inst ${itk-vnl_LIBRARIES})
target_link_libraries(itk-common  itksys itkvnl_inst )

if(UNIX)
  target_link_libraries(itk-common ${CMAKE_THREAD_LIBS} ${CMAKE_DL_LIBS} -lm)
endif(UNIX)

itk_module_target(itk-common)
