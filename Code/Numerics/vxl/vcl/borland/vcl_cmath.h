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
// acos
#ifndef vcl_acos
#define vcl_acos std::acos
#endif
// asin
#ifndef vcl_asin
#define vcl_asin std::asin
#endif
// atan
#ifndef vcl_atan
#define vcl_atan std::atan
#endif
// atan2
#ifndef vcl_atan2
#define vcl_atan2 std::atan2
#endif
// ceil
#ifndef vcl_ceil
#define vcl_ceil std::ceil
#endif
// cos
#ifndef vcl_cos
#define vcl_cos std::cos
#endif
// cosh
#ifndef vcl_cosh
#define vcl_cosh std::cosh
#endif
// exp
#ifndef vcl_exp
#define vcl_exp std::exp
#endif
// fabs
#ifndef vcl_fabs
#define vcl_fabs std::fabs
#endif
// floor
#ifndef vcl_floor
#define vcl_floor std::floor
#endif
// fmod
#ifndef vcl_fmod
#define vcl_fmod std::fmod
#endif
// frexp
#ifndef vcl_frexp
#define vcl_frexp std::frexp
#endif
// ldexp
#ifndef vcl_ldexp
#define vcl_ldexp std::ldexp
#endif
// log
#ifndef vcl_log
#define vcl_log std::log
#endif
// log10
#ifndef vcl_log10
#define vcl_log10 std::log10
#endif
// modf
#ifndef vcl_modf
#define vcl_modf std::modf
#endif
// pow
#ifndef vcl_pow
#define vcl_pow std::pow
#endif
// sin
#ifndef vcl_sin
#define vcl_sin std::sin
#endif
// sinh
#ifndef vcl_sinh
#define vcl_sinh std::sinh
#endif
// sqrt
#ifndef vcl_sqrt
#define vcl_sqrt std::sqrt
#endif
// tan
#ifndef vcl_tan
#define vcl_tan std::tan
#endif
// tanh
#ifndef vcl_tanh
#define vcl_tanh std::tanh
#endif


#endif // vcl_cmath_h_
