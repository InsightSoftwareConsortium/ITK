#ifndef vcl_borland_cmath_h_
#define vcl_borland_cmath_h_

// 1. include system header
#include <cmath>

inline double abs(long double  x) { return x >= 0 ? x : -x; }
inline double abs(double  x) { return x >= 0 ? x : -x; }
inline float  abs(float x) { return x >= 0 ? x : -x; }

// abs
#ifndef vcl_abs
#define vcl_abs ::abs
#endif

// rest of cmath can be defined using generic/vcl_complex.h
#define vcl_generic_cmath_STD std
#include "../generic/vcl_cmath.h"


#endif // vcl_cmath_h_
