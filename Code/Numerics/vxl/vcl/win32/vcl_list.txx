#ifndef vcl_win32_list_txx_
#define vcl_win32_list_txx_

#include <vcl_list.h>

#define VCL_LIST_INSTANTIATE_ITERATOR(Inp, Fwd, Diff) \
template Fwd std::copy(Inp, Inp, Fwd);\
template Fwd std::copy_backward(Inp, Inp, Fwd);\
template void std::advance(Fwd&, Diff)

#undef VCL_LIST_INSTANTIATE
#define VCL_LIST_INSTANTIATE(T)

#endif
