#ifndef vcl_mwerks_cmath_h_
#define vcl_mwerks_cmath_h_

#include <cmath>
#define vcl_generic_cmath_STD /* */
#include "generic/vcl_cmath.h"

// the following functions are declared in both <cmath> and <complex>
#undef  vcl_abs
#define vcl_abs vcl_abs
inline float vcl_abs(float x) { return ::abs(x); }
inline double vcl_abs(double x) { return ::abs(x); }
inline long double vcl_abs(long double x) { return ::abs(x); }

#undef  vcl_sqrt
#define vcl_sqrt vcl_sqrt
inline float vcl_sqrt(float x) { return ::sqrt(x); }
inline double vcl_sqrt(double x) { return ::sqrt(x); }
inline long double vcl_sqrt(long double x) { return ::sqrt(x); }

#undef  vcl_exp
#define vcl_exp vcl_exp
inline float vcl_exp(float x) { return ::exp(x); }
inline double vcl_exp(double x) { return ::exp(x); }
inline long double vcl_exp(long double x) { return ::exp(x); }

#undef  vcl_log
#define vcl_log vcl_log
inline float vcl_log(float x) { return ::log(x); }
inline double vcl_log(double x) { return ::log(x); }
inline long double vcl_log(long double x) { return ::log(x); }

#undef  vcl_log10
#define vcl_log10 vcl_log10
inline float vcl_log10(float x) { return ::log10(x); }
inline double vcl_log10(double x) { return ::log10(x); }
inline long double vcl_log10(long double x) { return ::log10(x); }

#undef  vcl_pow
#define vcl_pow vcl_pow
// VCL_CMATH_POW_DECLARED is true if vcl_pow has been declared
// as an inline functions rather than defined to std::pow
#if defined VCL_CMATH_POW_DECLARED
  ** Error **
#else
# define VCL_CMATH_POW_DECLARED 1
#endif
inline float vcl_pow(float x, float y) { return ::pow(x, y); }
inline double vcl_pow(double x, double y) { return ::pow(x, y); }
inline long double vcl_pow(long double x, long double y) { return ::pow(x, y); }

#undef  vcl_cos
#define vcl_cos vcl_cos
inline float vcl_cos(float x) { return ::cos(x); }
inline double vcl_cos(double x) { return ::cos(x); }
inline long double vcl_cos(long double x) { return ::cos(x); }

#undef  vcl_cosh
#define vcl_cosh vcl_cosh
inline float vcl_cosh(float x) { return ::cosh(x); }
inline double vcl_cosh(double x) { return ::cosh(x); }
inline long double vcl_cosh(long double x) { return ::cosh(x); }

#undef  vcl_sin
#define vcl_sin vcl_sin
inline float vcl_sin(float x) { return ::sin(x); }
inline double vcl_sin(double x) { return ::sin(x); }
inline long double vcl_sin(long double x) { return ::sin(x); }

#undef  vcl_sinh
#define vcl_sinh vcl_sinh
inline float vcl_sinh(float x) { return ::sinh(x); }
inline double vcl_sinh(double x) { return ::sinh(x); }
inline long double vcl_sinh(long double x) { return ::sinh(x); }

#undef  vcl_tan
#define vcl_tan vcl_tan
inline float vcl_tan(float x) { return ::tan(x); }
inline double vcl_tan(double x) { return ::tan(x); }
inline long double vcl_tan(long double x) { return ::tan(x); }

#undef  vcl_tanh
#define vcl_tanh vcl_tanh
inline float vcl_tanh(float x) { return ::tanh(x); }
inline double vcl_tanh(double x) { return ::tanh(x); }
inline long double vcl_tanh(long double x) { return ::tanh(x); }

#endif
