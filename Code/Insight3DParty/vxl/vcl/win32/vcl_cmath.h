#ifndef vcl_win32_cmath_h_
#define vcl_win32_cmath_h_

// 1. include system header
#include <cmath>

inline double abs(double  x) { return x >= 0 ? x : -x; }
inline float  abs(float x) { return x >= 0 ? x : -x; }

// abs
#ifndef vcl_abs
#define vcl_abs ::abs
#endif
// acos
#ifndef vcl_acos
#define vcl_acos ::acos
#endif
// asin
#ifndef vcl_asin
#define vcl_asin ::asin
#endif
// atan
#ifndef vcl_atan
#define vcl_atan ::atan
#endif
// atan2
#ifndef vcl_atan2
#define vcl_atan2 ::atan2
#endif
// ceil
#ifndef vcl_ceil
#define vcl_ceil ::ceil
#endif
// cos
#ifndef vcl_cos
#define vcl_cos ::cos
#endif
// cosh
#ifndef vcl_cosh
#define vcl_cosh ::cosh
#endif
// exp
#ifndef vcl_exp
#define vcl_exp ::exp
#endif
// fabs
#ifndef vcl_fabs
#define vcl_fabs ::fabs
#endif
// floor
#ifndef vcl_floor
#define vcl_floor ::floor
#endif
// fmod
#ifndef vcl_fmod
#define vcl_fmod ::fmod
#endif
// frexp
#ifndef vcl_frexp
#define vcl_frexp ::frexp
#endif
// ldexp
#ifndef vcl_ldexp
#define vcl_ldexp ::ldexp
#endif
// log
#ifndef vcl_log
#define vcl_log ::log
#endif
// log10
#ifndef vcl_log10
#define vcl_log10 ::log10
#endif
// modf
#ifndef vcl_modf
#define vcl_modf ::modf
#endif
// pow
#ifndef vcl_pow
#define vcl_pow ::pow
#endif
// sin
#ifndef vcl_sin
#define vcl_sin ::sin
#endif
// sinh
#ifndef vcl_sinh
#define vcl_sinh ::sinh
#endif
// sqrt
#ifndef vcl_sqrt
#define vcl_sqrt ::sqrt
#endif
// tan
#ifndef vcl_tan
#define vcl_tan ::tan
#endif
// tanh
#ifndef vcl_tanh
#define vcl_tanh ::tanh
#endif


#endif // vcl_cmath_h_
