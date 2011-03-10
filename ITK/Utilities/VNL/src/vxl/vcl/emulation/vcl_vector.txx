#ifndef vcl_emulation_vector_txx_
#define vcl_emulation_vector_txx_

// -*- c++ -*-
#include <vcl_vector.h>

#if  !defined ( __STL_DEFAULT_TYPE_PARAM )
#define __VECTOR_INSTANTIATE(T) \
  template class __vector__<T,vcl_alloc >;\
  template class vcl_vector<T >
#else
#define __VECTOR_INSTANTIATE(T) \
  template class vcl_vector<T , vcl_alloc >
#endif

#undef VCL_VECTOR_INSTANTIATE
#define VCL_VECTOR_INSTANTIATE(T) \
__VECTOR_INSTANTIATE(T);\
template class __vector_base<T , vcl_alloc >

#endif // vcl_emulation_vector_txx_
