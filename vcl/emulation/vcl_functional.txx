#ifndef vcl_emulation_functional_txx_
#define vcl_emulation_functional_txx_
/*
  fsm
*/
#include <vcl_functional.h>

// --- Unary templates ---
// Templates with one type mentioned, no requirements on type

#define VCL_UNARY_INSTANTIATE(T) \
template struct vcl_identity<T >

#define VCL_LESS_INSTANTIATE(T) \
template struct vcl_less<T >; \
VCL_UNARY_INSTANTIATE(vcl_less<T >)

#define VCL_COMPARISONS_INSTANTIATE(T) \
VCL_OPERATOR_NE_INSTANTIATE(T);\
VCL_INSTANTIATE_INLINE(bool operator >  (T const &, T const &));\
VCL_INSTANTIATE_INLINE(bool operator <= (T const &, T const &));\
VCL_INSTANTIATE_INLINE(bool operator >= (T const &, T const &))

#endif // vcl_emulation_functional_txx_
