#ifndef vcl_gcc_libstdcxx_v3_deque_cxx_
#define vcl_gcc_libstdcxx_v3_deque_cxx_

#include <vcl_deque.h>

#undef VCL_DEQUE_INSTANTIATE
#define VCL_DEQUE_INSTANTIATE(T) \
template class vcl_deque<T >

#endif
