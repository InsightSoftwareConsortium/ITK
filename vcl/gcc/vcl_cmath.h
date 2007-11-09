#ifndef vcl_gcc_cmath_h_
#define vcl_gcc_cmath_h_

#include "../iso/vcl_cmath.h"

// 1.5 fix system header.
#if defined (linux) && defined (__OPTIMIZE__)
// * avoid infinite recursion when calling vnl_math::isfinite().
// * avoid symbol in object file being called vnl_math::_isinf.
# undef isinf
// * avoid that vnl_math::isnan is redefined in <math.h>.
# undef isnan
#endif

// libstdc++.so.4 and higher defines sqrtf etc., but only as weak symbols.
// On the other hand, on solaris, /usr/lib/libm.so does not define sqrtf etc.
// Hence these explicit (and potentially inefficient) (re)definitions:
#if defined(sun) && defined(VCL_GCC_31)
inline float       acosf(float       x) { return (float)::acos(x); }
inline long double acosl(long double x) { return (long double)::acos(x); }

inline float       asinf(float       x) { return (float)::asin(x); }
inline long double asinl(long double x) { return (long double)::asin(x); }

inline float       atanf(float       x) { return (float)::atan(x); }
inline long double atanl(long double x) { return (long double)::atan(x); }

inline float       atan2f(float       y,float       x){return (float)::atan2(y, x);}
inline long double atan2l(long double y,long double x){return (long double)::atan2(y, x);}

inline float       ceilf(float       x){return (float)::ceil(x);}
inline long double ceill(long double x){return (long double)::ceil(x);}

inline float       cosf(float       x) { return (float)::cos(x); }
inline long double cosl(long double x) { return (long double)::cos(x); }

inline float       coshf(float       x) { return (float)::cosh(x); }
inline long double coshl(long double x) { return (long double)::cosh(x); }

inline float       expf(float       x) { return (float)::exp(x); }
inline long double expl(long double x) { return (long double)::exp(x); }

inline float       fabsf(float       x) { return (float)::fabs(x); }
inline long double fabsl(long double x) { return (long double)::fabs(x); }

inline float       floorf(float       x) { return (float)::floor(x); }
inline long double floorl(long double x) { return (long double)::floor(x); }

inline float       fmodf(float       x, float       y) { return (float)::fmod(x,y); }
inline long double fmodl(long double x, long double y) { return (long double)::fmod(x,y); }

inline float       frexpf(float       x, int *y) { return (float)::frexp(x,y); }
inline long double frexpl(long double x, int *y) { return (long double)::frexp(x,y); }

inline float       ldexpf(float       x, int y) { return (float)::ldexp(x,y); }
inline long double ldexpl(long double x, int y) { return (long double)::ldexp(x,y); }

inline float       logf(float       x) { return (float)::log(x); }
inline long double logl(long double x) { return (long double)::log(x); }

inline float       log10f(float       x) { return (float)::log10(x); }
inline long double log10l(long double x) { return (long double)::log10(x); }

inline float       modff(float       x, float       *y) { double f,g=::modf(x,&f); *y=(float)f; return (float)g; }
inline long double modfl(long double x, long double *y) { double f,g=::modf(x,&f); *y=(long double)f; return (long double)g; }

inline float       powf(float       x, float       a) { return (float)::pow(x, a); }
inline long double powl(long double x, long double a) { return (long double)::pow(x, a); }

inline float       sinf(float       x) { return (float)::sin(x); }
inline long double sinl(long double x) { return (long double)::sin(x); }

inline float       sinhf(float       x) { return (float)::sinh(x); }
inline long double sinhl(long double x) { return (long double)::sinh(x); }

inline float       sqrtf(float  x) { return (float)::sqrt(x); }
inline long double sqrtl(long double x) { return (long double)::sqrt(x); }

inline float       tanf(float       x) { return (float)::tan(x); }
inline long double tanl(long double x) { return (long double)::tan(x); }

inline float       tanhf(float       x) { return (float)::tanh(x); }
inline long double tanhl(long double x) { return (long double)::tanh(x); }

inline float       absf (float       x) { return (float)::fabs(x); }
inline long double absl (long double x) { return (long double)::fabs(x); }
#endif // sun

#endif // vcl_gcc_cmath_h_
