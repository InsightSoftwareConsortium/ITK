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
inline float  vcl_acos(float  x) { return ::acos(x); }
inline double vcl_acos(double x) { return ::acos(x); }

# undef  vcl_asin
# define vcl_asin vcl_asin
inline float  vcl_asin(float  x) { return ::asin(x); }
inline double vcl_asin(double x) { return ::asin(x); }

# undef  vcl_atan
# define vcl_atan vcl_atan
inline float  vcl_atan(float  x) { return ::atan(x); }
inline double vcl_atan(double x) { return ::atan(x); }

# undef  vcl_atan2
# define vcl_atan2 vcl_atan2
inline float  vcl_atan2(float  y, float  x) { return ::atan2(y, x); }
inline double vcl_atan2(double y, double x) { return ::atan2(y, x); }

# define vcl_ceil  ::ceil

# undef  vcl_cos
# define vcl_cos vcl_cos
inline float  vcl_cos(float  x) { return ::cos(x); }
inline double vcl_cos(double x) { return ::cos(x); }

# undef  vcl_cosh
# define vcl_cosh vcl_cosh
inline float  vcl_cosh(float  x) { return ::cosh(x); }
inline double vcl_cosh(double x) { return ::cosh(x); }

# undef  vcl_exp
# define vcl_exp vcl_exp
inline float  vcl_exp(float  x) { return ::exp(x); }
inline double vcl_exp(double x) { return ::exp(x); }

# define vcl_fabs  ::fabs
# define vcl_floor ::floor
# define vcl_fmod  ::fmod
# define vcl_frexp ::frexp
# define vcl_ldexp ::ldexp

# undef  vcl_log
# define vcl_log vcl_log
inline float  vcl_log(float  x) { return ::log(x); }
inline double vcl_log(double x) { return ::log(x); }

# define vcl_log10 ::log10
# define vcl_modf  ::modf

# undef  vcl_pow
# define vcl_pow vcl_pow
inline float  vcl_pow(float  x, float  a) { return ::pow(x, a); }
inline double vcl_pow(double x, double a) { return ::pow(x, a); }

# undef  vcl_sin
# define vcl_sin vcl_sin
inline float  vcl_sin(float  x) { return ::sin(x); }
inline double vcl_sin(double x) { return ::sin(x); }

# undef  vcl_sinh
# define vcl_sinh vcl_sinh
inline float  vcl_sinh(float  x) { return ::sinh(x); }
inline double vcl_sinh(double x) { return ::sinh(x); }

# undef  vcl_sqrt
# define vcl_sqrt vcl_sqrt
inline float  vcl_sqrt(float  x) { return ::sqrt(x); }
inline double vcl_sqrt(double x) { return ::sqrt(x); }

# undef  vcl_tan
# define vcl_tan vcl_tan
inline float  vcl_tan(float  x) { return ::tan(x); }
inline double vcl_tan(double x) { return ::tan(x); }

# undef  vcl_tanh
# define vcl_tanh vcl_tanh
inline float  vcl_tanh(float  x) { return ::tanh(x); }
inline double vcl_tanh(double x) { return ::tanh(x); }

# undef  vcl_abs
# define vcl_abs vcl_abs
inline float       vcl_abs (float       x) { return (x >= 0) ? x : -x; }
inline double      vcl_abs (double      x) { return (x >= 0) ? x : -x; }
inline long double vcl_abs (long double x) { return (x >= 0) ? x : -x; }

#else // iso
# include "../iso/vcl_cmath.h"
#endif

#endif // vcl_cmath_h_
