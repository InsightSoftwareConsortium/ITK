#ifndef vcl_sgi_list_txx_
#define vcl_sgi_list_txx_

#include <vcl_list.h>

#define VCL_LIST_INSTANTIATE_ITERATOR(Inp, Fwd, Diff) \
template Fwd std::copy(Inp, Inp, Fwd);\
template Fwd std::copy_backward(Inp, Inp, Fwd);\
template void std::advance(Fwd&, Diff)


#if VCL_USE_NATIVE_STL
#define VCL_LIST_INSTANTIATE(T) \
template class std::list<T,std::__default_alloc_template<true,0> >
#else
#define VCL_LIST_INSTANTIATE(T)
#endif

#endif
