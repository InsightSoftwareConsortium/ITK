#ifndef vcl_borland55_cmath_h_
#define vcl_borland55_cmath_h_

// This header's vcl_abs must be consistent with vcl_complex.h's version.

// Standard version mostly works.
#include "../iso/vcl_cmath.h"

// Replace abs from standard version.
#undef vcl_abs
#define vcl_abs vcl_abs

#define vcl_abs_define(T) inline T vcl_abs(T x) { return x >= 0 ? x : -x; }
vcl_abs_define(long double)
vcl_abs_define(double)
vcl_abs_define(float)
vcl_abs_define(char)
vcl_abs_define(signed char)
vcl_abs_define(short)
vcl_abs_define(int)
vcl_abs_define(long)
#undef vcl_abs_define

#endif // vcl_borland55_cmath_h_
