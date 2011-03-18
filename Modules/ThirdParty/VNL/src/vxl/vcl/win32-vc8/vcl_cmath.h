#ifndef vcl_win32_vc_8_cmath_h_
#define vcl_win32_vc_8_cmath_h_

#include <cmath>

// VC8 does not declare abs(__int 64) - so need to rewrite all vcl_abs

#ifndef vcl_abs
# define vcl_abs vcl_abs
#endif

#define vcl_generic_cmath_STD std
#include "../generic/vcl_cmath.h"

inline float  vcl_abs(float x) { return x >= 0.0f ? x : -x; }
inline double vcl_abs(double  x) { return x >= 0.0 ? x : -x; }
inline long double vcl_abs(long double  x) { return x >= 0.0 ? x : -x; }

#endif // vcl_win32_vc_8_cmath_h_
