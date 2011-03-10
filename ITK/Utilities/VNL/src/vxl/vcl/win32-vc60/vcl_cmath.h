#ifndef vcl_win32_vc60_cmath_h_
#define vcl_win32_vc60_cmath_h_

#include <cmath>

// VC6 does not declare the cmath functions in the std namespace.

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

inline float vcl_acos( float f ) { return ::acosf(f); }
inline double vcl_acos( double f ) { return ::acos(f); }
inline long double vcl_acos( long double f ) { return ::acosl(f); }

inline float vcl_asin( float f ) { return ::asinf(f); }
inline double vcl_asin( double f ) { return ::asin(f); }
inline long double vcl_asin( long double f ) { return ::asinl(f); }

inline float vcl_atan( float f ) { return ::atanf(f); }
inline double vcl_atan( double f ) { return ::atan(f); }
inline long double vcl_atan( long double f ) { return ::atanl(f); }

inline float vcl_atan2( float f, float g ) { return ::atan2f(f,g); }
inline double vcl_atan2( double f, double g ) { return ::atan2(f,g); }
inline long double vcl_atan2( long double f, long double g ) { return ::atan2l(f,g); }

inline float vcl_ceil( float f ) { return ::ceilf(f); }
inline double vcl_ceil( double f ) { return ::ceil(f); }
inline long double vcl_ceil( long double f ) { return ::ceill(f); }

inline float vcl_cos( float f ) { return ::cosf(f); }
inline double vcl_cos( double f ) { return ::cos(f); }
inline long double vcl_cos( long double f ) { return ::cosl(f); }

inline float vcl_cosh( float f ) { return ::coshf(f); }
inline double vcl_cosh( double f ) { return ::cosh(f); }
inline long double vcl_cosh( long double f ) { return ::coshl(f); }

inline float vcl_exp( float f ) { return ::expf(f); }
inline double vcl_exp( double f ) { return ::exp(f); }
inline long double vcl_exp( long double f ) { return ::expl(f); }

inline float vcl_fabs( float f ) { return ::fabsf(f); }
inline double vcl_fabs( double f ) { return ::fabs(f); }
inline long double vcl_fabs( long double f ) { return ::fabsl(f); }

inline float vcl_floor( float f ) { return ::floorf(f); }
inline double vcl_floor( double f ) { return ::floor(f); }
inline long double vcl_floor( long double f ) { return ::floorl(f); }

inline float vcl_fmod( float f, float g) { return ::fmodf(f,g); }
inline double vcl_fmod( double f, double g) { return ::fmod(f,g); }
inline long double vcl_fmod( long double f, long double g) { return ::fmodl(f,g); }

inline float vcl_frexp( float f, int* ip) { return ::frexpf(f, ip); }
inline double vcl_frexp( double f, int* ip) { return ::frexp(f, ip); }
inline long double vcl_frexp( long double f, int* ip) { return ::frexpl(f, ip); }

inline float vcl_ldexp( float f, int i) { return ::ldexpf(f, i); }
inline double vcl_ldexp( double f, int i) { return ::ldexp(f, i); }
inline long double vcl_ldexp( long double f, int i) { return ::ldexpl(f, i); }

inline float vcl_log( float f ) { return ::logf(f); }
inline double vcl_log( double f ) { return ::log(f); }
inline long double vcl_log( long double f ) { return ::logl(f); }

inline float vcl_log10( float f ) { return ::log10f(f); }
inline double vcl_log10( double f ) { return ::log10(f); }
inline long double vcl_log10( long double f ) { return ::log10l(f); }

inline float vcl_modf( float f, float* fp ) { return ::modff(f, fp); }
inline double vcl_modf( double f, double* fp ) { return ::modf(f, fp); }
inline long double vcl_modf( long double f, long double* fp ) { return ::modfl(f, fp); }

// VCL_CMATH_POW_DECLARED is true if vcl_pow has been declared
// as an inline functions rather than defined to std::pow
# if defined VCL_CMATH_POW_DECLARED
  ** Error **
# else
#  define VCL_CMATH_POW_DECLARED 1
# endif
inline float vcl_pow( float f, float g ) { return ::powf(f,g); }
inline double vcl_pow( double f, double g ) { return ::pow(f,g); }
inline long double vcl_pow( long double f, long double g ) { return ::powl(f,g); }

inline float vcl_pow( float f, int i ) { return ::powf(f, float(i)); }
inline double vcl_pow( double f, int i ) { return ::pow(f, double(i)); }
inline long double vcl_pow( long double f, int i ) { return ::powl(f, (long double)i); }

inline float vcl_sin( float f ) { return ::sinf(f); }
inline double vcl_sin( double f ) { return ::sin(f); }
inline long double vcl_sin( long double f ) { return ::sinl(f); }

inline float vcl_sinh( float f ) { return ::sinhf(f); }
inline double vcl_sinh( double f ) { return ::sinh(f); }
inline long double vcl_sinh( long double f ) { return ::sinhl(f); }

inline float vcl_sqrt( float f ) { return ::sqrtf(f); }
inline double vcl_sqrt( double f ) { return ::sqrt(f); }
inline long double vcl_sqrt( long double f ) { return ::sqrtl(f); }

inline float vcl_tan( float f ) { return ::tanf(f); }
inline double vcl_tan( double f ) { return ::tan(f); }
inline long double vcl_tan( long double f ) { return ::tanl(f); }

inline float vcl_tanh( float f ) { return ::tanhf(f); }
inline double vcl_tanh( double f ) { return ::tanh(f); }
inline long double vcl_tanh( long double f ) { return ::tanhl(f); }

#endif // vcl_win32_vc60_cmath_h_
