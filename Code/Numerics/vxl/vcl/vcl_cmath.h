#ifndef vcl_cmath_h_
#define vcl_cmath_h_
//: 
// \file
// \brief Templated version of the C math library
// [26.5.6] In addition to the signatures from the C header
// <math.h>, the C++ header <cmath> adds certain overloaded
// forms of the usual double-precision functions.
// The following signatures should be available for float, 
// double and long double :
// \verbatim
//  T abs(T );
//  T acos(T );
//  T asin(T );
//  T atan(T );
//  T atan2(T, T );
//  T ceil(T );
//  T cos(T );
//  T cosh(T );
//  T exp(T );
//  T fabs(T );
//  T floor(T );
//  T fmod(T, T);
//  T frexp(T, int *);
//  T ldexp(float, int);
//  T log(T );
//  T log10(T );
//  T modf(T, T *);
//  T pow(T, T );
//  T pow(T, int );
//  T sin(T );
//  T sinh(T );
//  T sqrt(T );
//  T tan(T );
//  T tanh(T );
// \endverbatim

#include "vcl_compiler.h"

#if defined(VCL_GCC)
# include "gcc/vcl_cmath.h"
#elif defined(VCL_SGI_CC)
# include "sgi/vcl_cmath.h"
#elif defined(VCL_VC) 
# include "win32/vcl_cmath.h"
#elif defined(VCL_BORLAND) 
# include "borland/vcl_cmath.h"
#elif defined(VCL_SUNPRO_CC)
# include "sunpro/vcl_cmath.h"
#elif defined(VCL_METRO_WERKS)
# include "mwerks/vcl_cmath.h"
#else
# include "iso/vcl_cmath.h"
#endif

#endif // vcl_cmath_h_
