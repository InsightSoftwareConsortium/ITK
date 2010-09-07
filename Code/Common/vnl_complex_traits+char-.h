#ifndef __vnl_complex_traits_plus_char__h
#define __vnl_complex_traits_plus_char__h

#include <vnl/vnl_complex_traits.h>
// The following macro is a complement to the ones
// in vxl/core/vnl/vnl_complex_traits.h lines 34-49.
#define VCL_DEFINE_SPECIALIZATION_MACRO(T)                                        \
  VCL_DEFINE_SPECIALIZATION struct vnl_complex_traits< T > {                      \
    enum { isreal = true };                                                       \
    static T conjugate(T x) { return x; }                                         \
    static vcl_complex< T > complexify(T x) { return vcl_complex< T >(x, (T)0); } \
  }
// end of macro
VCL_DEFINE_SPECIALIZATION_MACRO(char);
#undef VCL_DEFINE_SPECIALIZATION_MACRO

#endif
