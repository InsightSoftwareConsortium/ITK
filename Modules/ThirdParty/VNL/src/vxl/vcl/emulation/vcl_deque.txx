#ifndef vcl_emulation_deque_txx_
#define vcl_emulation_deque_txx_

#include "vcl_deque.h"
#include "vcl_algorithm.txx"
#include "vcl_iterator.txx"

#if  !defined ( __STL_DEFAULT_TYPE_PARAM )
#define __DEQUE_INSTANTIATE(T) \
  template class __deque__<T,vcl_alloc >;\
  template class vcl_deque<T >
#else
#define __DEQUE_INSTANTIATE(T) \
  template class vcl_deque<T , vcl_alloc >
#endif

#undef VCL_DEQUE_INSTANTIATE
#define VCL_DEQUE_INSTANTIATE(T) \
template struct __deque_iterator<T >;\
template struct __deque_const_iterator<T >;\
__DEQUE_INSTANTIATE(T);\
template class __deque_base<T , vcl_alloc >;\
template class vcl_reverse_bidirectional_iterator<__deque_iterator<T >, T , T  &, vcl_ptrdiff_t>;\
template class vcl_reverse_bidirectional_iterator<__deque_const_iterator<T >, T , T  const &, vcl_ptrdiff_t>;\
/* VCL_FILL_INSTANTIATE(vcl_deque<T >::iterator, T); */\
VCL_ITER_RA_INSTANTIATE(__deque_iterator<T >);\
VCL_ITER_RA_INSTANTIATE(__deque_const_iterator<T >)

#endif // vcl_emulation_deque_txx_
