#ifndef vcl_win32_vc60_cmath_h_
#define vcl_win32_vc60_cmath_h_

#include <cmath>

inline long double abs(long double  x) { return x >= 0 ? x : -x; }
inline double abs(double  x) { return x >= 0 ? x : -x; }
inline float  abs(float x) { return x >= 0 ? x : -x; }
inline float  sqrt(float f) { return float(::sqrt(double(f))); }
inline long double  sqrt(long double f) { return (long double)(::sqrt(double(f))); }

# define vcl_generic_cmath_STD

#include "../generic/vcl_cmath.h"

#endif // vcl_win32_vc60_cmath_h_
