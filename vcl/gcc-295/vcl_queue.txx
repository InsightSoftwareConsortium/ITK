#ifndef vcl_gcc295_queue_txx_
#define vcl_gcc295_queue_txx_

#include <vcl_queue.h>

#undef VCL_QUEUE_INSTANTIATE
#define VCL_QUEUE_INSTANTIATE(T) \
template class vcl_queue<T >

#include <vcl_cstddef.h>


#undef VCL_PRIORITY_QUEUE_INSTANTIATE
#define VCL_PRIORITY_QUEUE_INSTANTIATE(I, T, C) \
template class vcl_priority_queue<T, I, C >; \
template void __push_heap<T*, vcl_ptrdiff_t, T, C >(T*, vcl_ptrdiff_t, vcl_ptrdiff_t, T, C); \
template void __adjust_heap<T*, vcl_ptrdiff_t, T, C >(T*, vcl_ptrdiff_t, vcl_ptrdiff_t, T, C)

#endif // vcl_gcc295_queue_txx_
