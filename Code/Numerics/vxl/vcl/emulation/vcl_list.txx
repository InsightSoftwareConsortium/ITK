#include "vcl_list.h"
#include "vcl_algorithm.txx"
#include "vcl_iterator.txx"

#if  !defined ( __STL_DEFAULT_TYPE_PARAM )
#define __LIST_INSTANTIATE(T) \
  template class __list__<T,vcl_alloc >;\
  template class vcl_list<T >
#else
#define __LIST_INSTANTIATE(T) \
  template class vcl_list<T , vcl_alloc >
#endif

#undef VCL_LIST_INSTANTIATE
#define VCL_LIST_INSTANTIATE(T)\
template struct __list_node<T >;\
template struct __list_iterator<T >;\
template struct __list_const_iterator<T >;\
__LIST_INSTANTIATE(T);\
template class __list_base<T , vcl_alloc >;\
template class reverse_bidirectional_iterator<__list_iterator<T >, T , T  &, ptrdiff_t>;\
template class reverse_bidirectional_iterator<__list_const_iterator<T >, T , T  const &, ptrdiff_t>;\
VCL_SWAP_INSTANTIATE(__list_node<T >*);\
VCL_ITER_BD_INSTANTIATE(__list_iterator<T >);\
VCL_ITER_BD_Distance_INSTANTIATE(__list_iterator<T >, size_t);\
VCL_ITER_BD_INSTANTIATE(__list_const_iterator<T >);\
template class vcl_simple_alloc<__list_node<T >, vcl_alloc >
