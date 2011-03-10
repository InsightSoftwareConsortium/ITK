#ifndef vcl_sunpro_deque_txx_
#define vcl_sunpro_deque_txx_
/*
  fsm
*/

#include <vcl_deque.h>
#include <vcl_algorithm.txx>
#include <vcl_iterator.h>

#undef VCL_DEQUE_INSTANTIATE
#define VCL_DEQUE_INSTANTIATE(T) \
template class vcl_deque<T >; \
VCL_FILL_INSTANTIATE(vcl_deque<T >::iterator, T); \
VCL_COPY_INSTANTIATE(T const *, vcl_deque<T >::iterator); \
VCL_COPY_INSTANTIATE(vcl_deque<T >::iterator, vcl_back_insert_iterator<vcl_deque<T > >); \
VCL_COPY_INSTANTIATE(vcl_deque<T >::const_iterator, vcl_back_insert_iterator<vcl_deque<T > >); \
VCL_COPY_INSTANTIATE(vcl_deque<T >::iterator, vcl_deque<T >::iterator); \
VCL_COPY_INSTANTIATE(vcl_deque<T >::const_iterator, vcl_deque<T >::iterator); \
VCL_COPY_BACKWARD_INSTANTIATE(vcl_deque<T >::iterator, vcl_deque<T >::iterator)

#endif // vcl_sunpro_deque_txx_
