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
#elif defined(VCL_VC_60)
# include "win32-vc60/vcl_cmath.h"
                            // C++ .NET 2003 is iso compliant
#elif defined(VCL_VC_70)     // C++ .NET earlier than 2003 is not iso compliant
# include "win32-vc70/vcl_cmath.h"
#elif defined(VCL_VC_8) || defined(VCL_VC_9)     // C++ .NET earlier than 2003 is not iso compliant
# include "win32-vc8/vcl_cmath.h"
#elif defined(VCL_SUNPRO_CC)
# include "sunpro/vcl_cmath.h"
#elif defined(VCL_METRO_WERKS)
# include "mwerks/vcl_cmath.h"
#elif defined(VCL_BORLAND_55)
# include "borland55/vcl_cmath.h"
#else
# include "iso/vcl_cmath.h"
#endif


#if !VCL_COMPLEX_POW_WORKS && !defined VCL_CMATH_POW_DECLARED
// deal with corrections to pow(complex...)
# undef vcl_pow
# define vcl_pow vcl_pow
// VCL_CMATH_POW_DECLARED is true if vcl_pow has been declared
// to an inline functions rather than defined to std::pow
# define VCL_CMATH_POW_DECLARED 1
inline float vcl_pow(float x, float y) { return std::pow(x, y); }
inline double vcl_pow(double x, double y) { return std::pow(x, y); }
inline long double vcl_pow(long double x, long double y) { return std::pow(x, y); }
#endif

#endif // vcl_cmath_h_
