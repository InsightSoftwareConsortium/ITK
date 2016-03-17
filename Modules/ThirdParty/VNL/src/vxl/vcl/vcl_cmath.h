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
#include "iso/vcl_cmath.h"


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
