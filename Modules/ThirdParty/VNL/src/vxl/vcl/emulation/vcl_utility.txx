// This is vcl/emulation/vcl_utility.txx
#ifndef vcl_emulation_utility_txx_
#define vcl_emulation_utility_txx_

#include "vcl_utility.h"
#include "vcl_pair.h"
#include "vcl_algorithm.txx"

// fsm: what's this CONTAINABLE0 thing? I commented it out
// in favour of CONTAINABLE.

#undef VCL_PAIR_INSTANTIATE
#define VCL_PAIR_INSTANTIATE(T1, T2) \
template class vcl_pair<T1, T2 >; \
template struct vcl_select1st<vcl_pair<T1, T2 > >; \
/*VCL_CONTAINABLE0_INSTANTIATE(vcl_pair<T1 VCL_COMMA T2 >);*/ \
VCL_CONTAINABLE_INSTANTIATE(vcl_pair<T1 VCL_COMMA T2 >); \
VCL_SWAP_INSTANTIATE(vcl_pair<T1 VCL_COMMA T2 >)

// swap can't be defined if either T1 or T2 is a const type
#undef VCL_PAIR_const_INSTANTIATE
#define VCL_PAIR_const_INSTANTIATE(T1, T2) \
template class vcl_pair<T1, T2 >; \
template struct vcl_select1st<vcl_pair<T1, T2 > >; \
/*VCL_CONTAINABLE0_INSTANTIATE(vcl_pair<T1 VCL_COMMA T2 >);*/ \
VCL_CONTAINABLE_INSTANTIATE(vcl_pair<T1 VCL_COMMA T2 >)

#endif // vcl_emulation_utility_txx_
