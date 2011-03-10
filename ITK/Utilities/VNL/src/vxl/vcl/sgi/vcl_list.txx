#ifndef vcl_sgi_list_txx_
#define vcl_sgi_list_txx_

#include <vcl_list.h>

#define VCL_LIST_INSTANTIATE_ITERATOR(Inp, Fwd, Diff) \
template Fwd std::copy(Inp, Inp, Fwd);\
template Fwd std::copy_backward(Inp, Inp, Fwd);\
template void std::advance(Fwd&, Diff)

#undef VCL_LIST_INSTANTIATE
#if VCL_USE_NATIVE_STL
#if defined(VCL_SGI_CC_730)
#define VCL_LIST_INSTANTIATE(T) \
template class std::list<T >
#else
#define VCL_LIST_INSTANTIATE(T) \
template class std::list<T,std::__default_alloc_template<true,0> >
#endif
#else
#define VCL_LIST_INSTANTIATE(T)
#endif

#endif // vcl_sgi_list_txx_
