#ifndef vcl_sgi_cmath_h_
#define vcl_sgi_cmath_h_

#if defined(VCL_SGI_CC_720) || (defined(VCL_CXX_HAS_HEADER_CMATH) && !VCL_CXX_HAS_HEADER_CMATH)
# include <math.h> // e.g. SGI CC 7.30

//
// NB. The functions vcl_blah which are overloaded
// for complex<> arguments cannot be handled by
// #define vcl_blah ::blah
//

# undef  vcl_acos
# define vcl_acos vcl_acos
inline float       vcl_acos(float       x) { return ::acosf(x); }
inline double      vcl_acos(double      x) { return ::acos(x); }
inline long double vcl_acos(long double x) { return ::acosl(x); }

# undef  vcl_asin
# define vcl_asin vcl_asin
inline float       vcl_asin(float       x) { return ::asinf(x); }
inline double      vcl_asin(double      x) { return ::asin(x); }
inline long double vcl_asin(long double x) { return ::asinl(x); }

# undef  vcl_atan
# define vcl_atan vcl_atan
inline float       vcl_atan(float       x) { return ::atanf(x); }
inline double      vcl_atan(double      x) { return ::atan(x); }
inline long double vcl_atan(long double x) { return ::atanl(x); }

# undef  vcl_atan2
# define vcl_atan2 vcl_atan2
inline float       vcl_atan2(float       y,float       x){return ::atan2f(y, x);}
inline double      vcl_atan2(double      y,double      x){return ::atan2(y, x);}
inline long double vcl_atan2(long double y,long double x){return ::atan2l(y, x);}

# undef  vcl_ceil
# define vcl_ceil vcl_ceil
inline float       vcl_ceil(float       x){return ::ceilf(x);}
inline double      vcl_ceil(double      x){return ::ceil(x);}
inline long double vcl_ceil(long double x){return ::ceill(x);}

# undef  vcl_cos
# define vcl_cos vcl_cos
inline float       vcl_cos(float       x) { return ::cosf(x); }
inline double      vcl_cos(double      x) { return ::cos(x); }
inline long double vcl_cos(long double x) { return ::cosl(x); }

# undef  vcl_cosh
# define vcl_cosh vcl_cosh
inline float       vcl_cosh(float       x) { return ::coshf(x); }
inline double      vcl_cosh(double      x) { return ::cosh(x); }
inline long double vcl_cosh(long double x) { return ::coshl(x); }

# undef  vcl_exp
# define vcl_exp vcl_exp
inline float       vcl_exp(float       x) { return ::expf(x); }
inline double      vcl_exp(double      x) { return ::exp(x); }
inline long double vcl_exp(long double x) { return ::expl(x); }

# undef  vcl_fabs
# define vcl_fabs vcl_fabs
inline float       vcl_fabs(float       x) { return ::fabsf(x); }
inline double      vcl_fabs(double      x) { return ::fabs(x); }
inline long double vcl_fabs(long double x) { return ::fabsl(x); }

# undef  vcl_floor
# define vcl_floor vcl_floor
inline float       vcl_floor(float       x) { return ::floorf(x); }
inline double      vcl_floor(double      x) { return ::floor(x); }
inline long double vcl_floor(long double x) { return ::floorl(x); }

# undef  vcl_fmod
# define vcl_fmod vcl_fmod
inline float       vcl_fmod(float       x, float       y) { return ::fmodf(x,y); }
inline double      vcl_fmod(double      x, double      y) { return ::fmod(x,y); }
inline long double vcl_fmod(long double x, long double y) { return ::fmodl(x,y); }

# undef  vcl_frexp
# define vcl_frexp vcl_frexp
inline float       vcl_frexp(float       x, int *y) { return ::frexp((double)x,y); }
inline double      vcl_frexp(double      x, int *y) { return ::frexp(x,y); }
inline long double vcl_frexp(long double x, int *y) { return ::frexpl(x,y); }

# undef  vcl_ldexp
# define vcl_ldexp vcl_ldexp
inline float       vcl_ldexp(float       x, int y) { return ::ldexp((double)x,y); }
inline double      vcl_ldexp(double      x, int y) { return ::ldexp(x,y); }
inline long double vcl_ldexp(long double x, int y) { return ::ldexpl(x,y); }

# undef  vcl_log
# define vcl_log vcl_log
inline float       vcl_log(float       x) { return ::logf(x); }
inline double      vcl_log(double      x) { return ::log(x); }
inline long double vcl_log(long double x) { return ::logl(x); }

# undef vcl_log10
# define vcl_log10 vcl_log10
inline float       vcl_log10(float       x) { return ::log10f(x); }
inline double      vcl_log10(double      x) { return ::log10(x); }
inline long double vcl_log10(long double x) { return ::log10l(x); }

# undef  vcl_modf
# define vcl_modf vcl_modf
inline float       vcl_modf(float       x, float       *y) { return ::modff(x,y); }
inline double      vcl_modf(double      x, double      *y) { return ::modf(x,y); }
inline long double vcl_modf(long double x, long double *y) { return ::modfl(x,y); }

# undef  vcl_pow
# define vcl_pow vcl_pow
// VCL_CMATH_POW_DECLARED is true if vcl_pow has been declared
// as an inline functions rather than defined to std::pow
# if defined VCL_CMATH_POW_DECLARED
  ** Error **
# else
#  define VCL_CMATH_POW_DECLARED 1
# endif
inline float       vcl_pow(float       x, float       a) { return ::powf(x, a); }
inline double      vcl_pow(double      x, double      a) { return ::pow(x, a); }
inline long double vcl_pow(long double x, long double a) { return ::powl(x, a); }

# undef  vcl_sin
# define vcl_sin vcl_sin
inline float       vcl_sin(float       x) { return ::sinf(x); }
inline double      vcl_sin(double      x) { return ::sin(x); }
inline long double vcl_sin(long double x) { return ::sinl(x); }

# undef  vcl_sinh
# define vcl_sinh vcl_sinh
inline float       vcl_sinh(float       x) { return ::sinhf(x); }
inline double      vcl_sinh(double      x) { return ::sinh(x); }
inline long double vcl_sinh(long double x) { return ::sinhl(x); }

# undef  vcl_sqrt
# define vcl_sqrt vcl_sqrt
inline float       vcl_sqrt(float  x) { return ::sqrtf(x); }
inline double      vcl_sqrt(double x) { return ::sqrt(x); }
inline long double vcl_sqrt(long double x) { return ::sqrtl(x); }

# undef  vcl_tan
# define vcl_tan vcl_tan
inline float       vcl_tan(float       x) { return ::tanf(x); }
inline double      vcl_tan(double      x) { return ::tan(x); }
inline long double vcl_tan(long double x) { return ::tanl(x); }

# undef  vcl_tanh
# define vcl_tanh vcl_tanh
inline float       vcl_tanh(float       x) { return ::tanhf(x); }
inline double      vcl_tanh(double      x) { return ::tanh(x); }
inline long double vcl_tanh(long double x) { return ::tanhl(x); }

# undef  vcl_abs
# define vcl_abs vcl_abs
inline float       vcl_abs (float       x) { return ::fabsf(x); }
inline double      vcl_abs (double      x) { return ::fabs(x); }
inline long double vcl_abs (long double x) { return ::fabsl(x); }

#else // iso
# include "../iso/vcl_cmath.h"
#endif

#endif // vcl_sgi_cmath_h_
