#ifndef vcl_sunpro_utility_txx_
#define vcl_sunpro_utility_txx_
/*
  fsm
*/

#include <vcl_utility.h>

#undef VCL_PAIR_INSTANTIATE
#define VCL_PAIR_INSTANTIATE(T1, T2) \
template class vcl_pair<T1, T2 >

#endif
