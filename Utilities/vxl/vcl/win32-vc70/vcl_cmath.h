#ifndef vcl_win32_vc70_cmath_h_
#define vcl_win32_vc70_cmath_h_

#include <cmath>

// VC7 does not declare all cmath function overloads.

#ifndef vcl_abs
# define vcl_abs vcl_abs
#endif
#ifndef vcl_acos
# define vcl_acos vcl_acos
#endif
#ifndef vcl_asin
# define vcl_asin vcl_asin
#endif
#ifndef vcl_atan
# define vcl_atan vcl_atan
#endif
#ifndef vcl_atan2
# define vcl_atan2 vcl_atan2
#endif
#ifndef vcl_ceil
# define vcl_ceil vcl_ceil
#endif
#ifndef vcl_cos
# define vcl_cos vcl_cos
#endif
#ifndef vcl_cosh
# define vcl_cosh vcl_cosh
#endif
#ifndef vcl_exp
# define vcl_exp vcl_exp
#endif
#ifndef vcl_fabs
# define vcl_fabs vcl_fabs
#endif
#ifndef vcl_floor
# define vcl_floor vcl_floor
#endif
#ifndef vcl_fmod
# define vcl_fmod vcl_fmod
#endif
#ifndef vcl_frexp
# define vcl_frexp vcl_frexp
#endif
#ifndef vcl_ldexp
# define vcl_ldexp vcl_ldexp
#endif
#ifndef vcl_log
# define vcl_log vcl_log
#endif
#ifndef vcl_log10
# define vcl_log10 vcl_log10
#endif
#ifndef vcl_modf
# define vcl_modf vcl_modf
#endif
#ifndef vcl_pow
# define vcl_pow vcl_pow
#endif
#ifndef vcl_sin
# define vcl_sin vcl_sin
#endif
#ifndef vcl_sinh
# define vcl_sinh vcl_sinh
#endif
#ifndef vcl_sqrt
# define vcl_sqrt vcl_sqrt
#endif
#ifndef vcl_tan
# define vcl_tan vcl_tan
#endif
#ifndef vcl_tanh
# define vcl_tanh vcl_tanh
#endif

#define vcl_generic_cmath_STD std
#include "../generic/vcl_cmath.h"

inline float  vcl_abs(float x) { return x >= 0.0f ? x : -x; }
inline double vcl_abs(double  x) { return x >= 0.0 ? x : -x; }
inline long double vcl_abs(long double  x) { return x >= 0.0 ? x : -x; } 

inline float vcl_acos( float f ) { return std::acosf(f); }
inline double vcl_acos( double f ) { return std::acos(f); }
inline long double vcl_acos( long double f ) { return std::acosl(f); }

inline float vcl_asin( float f ) { return std::asinf(f); }
inline double vcl_asin( double f ) { return std::asin(f); }
inline long double vcl_asin( long double f ) { return std::asinl(f); }

inline float vcl_atan( float f ) { return std::atanf(f); }
inline double vcl_atan( double f ) { return std::atan(f); }
inline long double vcl_atan( long double f ) { return std::atanl(f); }

inline float vcl_atan2( float f, float g ) { return std::atan2f(f,g); }
inline double vcl_atan2( double f, double g ) { return std::atan2(f,g); }
inline long double vcl_atan2( long double f, long double g ) { return std::atan2l(f,g); }

inline float vcl_ceil( float f ) { return std::ceilf(f); }
inline double vcl_ceil( double f ) { return std::ceil(f); }
inline long double vcl_ceil( long double f ) { return std::ceill(f); }

inline float vcl_cos( float f ) { return std::cosf(f); }
inline double vcl_cos( double f ) { return std::cos(f); }
inline long double vcl_cos( long double f ) { return std::cosl(f); }

inline float vcl_cosh( float f ) { return std::coshf(f); }
inline double vcl_cosh( double f ) { return std::cosh(f); }
inline long double vcl_cosh( long double f ) { return std::coshl(f); }

inline float vcl_exp( float f ) { return std::expf(f); }
inline double vcl_exp( double f ) { return std::exp(f); }
inline long double vcl_exp( long double f ) { return std::expl(f); }

inline float vcl_fabs( float f ) { return std::fabsf(f); }
inline double vcl_fabs( double f ) { return std::fabs(f); }
inline long double vcl_fabs( long double f ) { return std::fabsl(f); }

inline float vcl_floor( float f ) { return std::floorf(f); }
inline double vcl_floor( double f ) { return std::floor(f); }
inline long double vcl_floor( long double f ) { return std::floorl(f); }

inline float vcl_fmod( float f, float g) { return std::fmodf(f,g); }
inline double vcl_fmod( double f, double g) { return std::fmod(f,g); }
inline long double vcl_fmod( long double f, long double g) { return std::fmodl(f,g); }

inline float vcl_frexp( float f, int* ip) { return std::frexpf(f, ip); }
inline double vcl_frexp( double f, int* ip) { return std::frexp(f, ip); }
inline long double vcl_frexp( long double f, int* ip) { return std::frexpl(f, ip); }

inline float vcl_ldexp( float f, int i) { return std::ldexpf(f, i); }
inline double vcl_ldexp( double f, int i) { return std::ldexp(f, i); }
inline long double vcl_ldexp( long double f, int i) { return std::ldexpl(f, i); }

inline float vcl_log( float f ) { return std::logf(f); }
inline double vcl_log( double f ) { return std::log(f); }
inline long double vcl_log( long double f ) { return std::logl(f); }

inline float vcl_log10( float f ) { return std::log10f(f); }
inline double vcl_log10( double f ) { return std::log10(f); }
inline long double vcl_log10( long double f ) { return std::log10l(f); }

inline float vcl_modf( float f, float* fp ) { return std::modff(f, fp); }
inline double vcl_modf( double f, double* fp ) { return std::modf(f, fp); }
inline long double vcl_modf( long double f, long double* fp ) { return std::modfl(f, fp); }

inline float vcl_pow( float f, float g ) { return std::powf(f,g); }
inline double vcl_pow( double f, double g ) { return std::pow(f,g); }
inline long double vcl_pow( long double f, long double g ) { return std::powl(f,g); }

inline float vcl_pow( float f, int i ) { return std::powf(f, float(i)); }
inline double vcl_pow( double f, int i ) { return std::pow(f, double(i)); }
inline long double vcl_pow( long double f, int i ) { return std::powl(f, (long double)i); }

inline float vcl_sin( float f ) { return std::sinf(f); }
inline double vcl_sin( double f ) { return std::sin(f); }
inline long double vcl_sin( long double f ) { return std::sinl(f); }

inline float vcl_sinh( float f ) { return std::sinhf(f); }
inline double vcl_sinh( double f ) { return std::sinh(f); }
inline long double vcl_sinh( long double f ) { return std::sinhl(f); }

inline float vcl_sqrt( float f ) { return std::sqrtf(f); }
inline double vcl_sqrt( double f ) { return std::sqrt(f); }
inline long double vcl_sqrt( long double f ) { return std::sqrtl(f); }

inline float vcl_tan( float f ) { return std::tanf(f); }
inline double vcl_tan( double f ) { return std::tan(f); }
inline long double vcl_tan( long double f ) { return std::tanl(f); }

inline float vcl_tanh( float f ) { return std::tanhf(f); }
inline double vcl_tanh( double f ) { return std::tanh(f); }
inline long double vcl_tanh( long double f ) { return std::tanhl(f); }

#endif // vcl_win32_vc70_cmath_h_
