#ifndef vcl_sgi_set_txx_
#define vcl_sgi_set_txx_

#include <vcl_set.h>
#undef VCL_SET_INSTANTIATE
#if VCL_USE_NATIVE_STL
#define VCL_SET_INSTANTIATE(T, Comp) \
template class std::rb_tree<T,T,std::identity<T >,Comp,std::__default_alloc_template<true,0> >
#else
#define VCL_SET_INSTANTIATE(T, Comp)
#endif

#endif // vcl_sgi_set_txx_
