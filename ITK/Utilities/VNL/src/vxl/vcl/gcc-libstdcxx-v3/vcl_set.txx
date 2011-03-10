#ifndef vcl_gcc_libstdcxx_v3_set_txx_
#define vcl_gcc_libstdcxx_v3_set_txx_

#include <vcl_set.h>

#undef VCL_SET_INSTANTIATE
#define VCL_SET_INSTANTIATE(T, Comp) \
template class vcl_set<T, Comp >

#endif // vcl_gcc_libstdcxx_v3_set_txx_
