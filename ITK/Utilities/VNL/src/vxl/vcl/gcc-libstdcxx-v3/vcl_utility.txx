#ifndef vcl_gcc_libstdcxx_v3_utility_txx_
#define vcl_gcc_libstdcxx_v3_utility_txx_

#include <vcl_utility.h>

#undef VCL_PAIR_INSTANTIATE
#define VCL_PAIR_INSTANTIATE(T1, T2) \
template class vcl_pair<T1, T2 >

// swap can't be defined if either T1 or T2 is a const type
#define VCL_PAIR_INSTANTIATE_const(T1, T2) \
template class vcl_pair<T1, T2 >

#endif // vcl_gcc_libstdcxx_v3_utility_txx_
