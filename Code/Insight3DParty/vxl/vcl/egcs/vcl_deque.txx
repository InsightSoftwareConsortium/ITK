#ifndef vcl_egcs_deque_cxx_
#define vcl_egcs_deque_cxx_

#include <vcl_deque.h>

#undef VCL_DEQUE_INSTANTIATE
#define VCL_DEQUE_INSTANTIATE(T) \
template class deque<T >

#endif
