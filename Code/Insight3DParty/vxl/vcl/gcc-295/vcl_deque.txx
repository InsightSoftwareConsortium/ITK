#ifndef vcl_gcc295_deque_cxx_
#define vcl_gcc295_deque_cxx_

#include <vcl_deque.h>

#undef  VCL_DEQUE_INSTANTIATE
#define VCL_DEQUE_INSTANTIATE(T) \
template class vcl_deque<T >; \
template class _Deque_base<T, allocator<T >, 0>

#endif
