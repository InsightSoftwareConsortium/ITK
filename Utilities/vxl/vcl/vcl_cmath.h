#ifndef vcl_cmath_h_
#define vcl_cmath_h_
//: 
// \file
// \brief Templated version of the C math library.
// \date 23 Feb 2000
// [26.5.6] In addition to the signatures from the C header
// <math.h>, the C++ header <cmath> adds certain overloaded
// forms of the usual double-precision functions.
// The following signatures should be available for float, 
// double and long double :
//
// \code
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
//  T ldexp(T, int);
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
// \endcode

#include "vcl_compiler.h"

#if defined(VCL_STLPORT)
# include "stlport/vcl_cmath.h"
#elif defined(VCL_GCC)
# include "gcc/vcl_cmath.h"
#elif defined(VCL_SGI_CC)
# include "sgi/vcl_cmath.h"
#elif defined(VCL_VC60)
# include "win32-vc60/vcl_cmath.h"
#elif defined(VCL_VC71)     // C++ .NET 2003 is iso compliant
# include "iso/vcl_cmath.h" 
#elif defined(VCL_VC70)     // C++ .NET earlier than 2003 is not iso compliant
# include "win32-vc70/vcl_cmath.h"
#elif defined(VCL_SUNPRO_CC)
# include "sunpro/vcl_cmath.h"
#elif defined(VCL_METRO_WERKS)
# include "mwerks/vcl_cmath.h"
#elif defined(VCL_BORLAND_55)
# include "borland55/vcl_cmath.h"
#else
# include "iso/vcl_cmath.h"
#endif

#endif // vcl_cmath_h_
