#ifndef vcl_win32_vc60_cmath_h_
#define vcl_win32_vc60_cmath_h_

#include <cmath>

// VC6 does not declare the cmath functions in the std namespace.
// If we define vcl_abs, for example, to ::abs, we have conflicts
// with std::abs(std::complex<T>) which *is* declared in the
// std namespace. To avoid these issues, we inject the appropriate
// functions into the std namespace.

namespace std {
  inline float  abs(float x) { return x >= 0.0f ? x : -x; }
  inline double abs(double  x) { return x >= 0.0 ? x : -x; }
  inline long double abs(long double  x) { return x >= 0.0 ? x : -x; } 

  inline float acos( float f ) { return ::acosf(f ); }
  inline double acos( double f ) { return (double)::acos(double(f) ); }
  inline long double acos( long double f ) { return (long double)::acos(double(f) ); }

  inline float asin( float f ) { return ::asinf(f ); }
  inline double asin( double f ) { return (double)::asin(double(f) ); }
  inline long double asin( long double f ) { return (long double)::asin(double(f) ); }

  inline float atan( float f ) { return ::atanf(f ); }
  inline double atan( double f ) { return (double)::atan(double(f) ); }
 inline long double atan( long double f ) { return (long double)::atan(double(f) ); }

  inline float atan2( float f, float g ) { return ::atan2f(f,g ); }
  inline double atan2( double f, double g ) { return (double)::atan2(double(f),double(g) ); }
  inline long double atan2( long double f, long double g ) { return (long double)::atan2(double(f),double(g) ); }

  inline float ceil( float f ) { return ::ceilf(f ); }
  inline double ceil( double f ) { return (double)::ceil(double(f) ); }
  inline long double ceil( long double f ) { return (long double)::ceil(double(f) ); }

  inline float cos( float f ) { return ::cosf(f ); }
  inline double cos( double f ) { return (double)::cos(double(f) ); }
  inline long double cos( long double f ) { return (long double)::cos(double(f) ); }

  inline float cosh( float f ) { return ::coshf(f ); }
  inline double cosh( double f ) { return (double)::cosh(double(f) ); }
  inline long double cosh( long double f ) { return (long double)::cosh(double(f) ); }

  inline float exp( float f ) { return ::expf(f ); }
  inline double exp( double f ) { return (double)::exp(double(f) ); }
  inline long double exp( long double f ) { return (long double)::exp(double(f) ); }

  inline float fabs( float f ) { return ::fabsf(f ); }
  inline double fabs( double f ) { return (double)::fabs(double(f) ); }
  inline long double fabs( long double f ) { return (long double)::fabs(double(f) ); }

  inline float floor( float f ) { return ::floorf(f ); }
  inline double floor( double f ) { return (double)::floor(double(f) ); }
  inline long double floor( long double f ) { return (long double)::floor(double(f) ); }

  inline float fmod( float f, float g) { return ::fmodf(f,g); }
  inline double fmod( double f, double g) { return (double)::fmod(double(f),double(g)); }
  inline long double fmod( long double f, long double g) { return (long double)::fmod(double(f),double(g)); }

  inline float frexp( float f, int* ip) { return ::frexpf(f, ip); }
  inline double frexp( double f, int* ip) { return (double)::frexp(double(f), ip); }
  inline long double frexp( long double f, int* ip) { return (long double)::frexp(double(f), ip); }

  inline float ldexp( float f, int i) { return ::ldexpf(f, i); }
  inline double ldexp( double f, int i) { return (double)::ldexp(double(f), i); }
  inline long double ldexp( long double f, int i) { return (long double)::ldexp(double(f), i); }

  inline float log( float f ) { return ::logf(f ); }
  inline double log( double f ) { return (double)::log(double(f) ); }
  inline long double log( long double f ) { return (long double)::log(double(f) ); }

  inline float log10( float f ) { return ::log10f(f ); }
  inline double log10( double f ) { return (double)::log10(double(f) ); }
  inline long double log10( long double f ) { return (long double)::log10(double(f) ); }

  inline float modf( float f, float* fp ) { return ::modff(f, fp ); }
  inline double modf( double f, double* fp ) { return (double)::modf(double(f), (double*)fp ); }
  inline long double modf( long double f, long double* fp ) { return (long double)::modf(double(f), (double*)fp ); }

  inline float pow( float f, float g ) { return ::powf(f,g ); }
  inline double pow( double f, double g ) { return (double)::pow(double(f),double(g) ); }
  inline long double pow( long double f, long double g ) { return (long double)::pow(double(f),double(g) ); }

  inline float pow( float f, int i ) { return ::powf(f, float(i) ); }
  inline double pow( double f, int i ) { return ::pow(f, double(i) ); }
  inline long double pow( long double f, int i ) { return (long double)::pow(double(f), double(i) ); }

  inline float sin( float f ) { return ::sinf(f ); }
  inline double sin( double f ) { return (double)::sin(double(f) ); }
  inline long double sin( long double f ) { return (long double)::sin(double(f) ); }

  inline float sinh( float f ) { return ::sinhf(f ); }
  inline double sinh( double f ) { return (double)::sinh(double(f) ); }
  inline long double sinh( long double f ) { return (long double)::sinh(double(f) ); }

  inline float sqrt( float f ) { return ::sqrtf(f ); }
  inline double sqrt( double f ) { return (double)::sqrt(double(f) ); }
  inline long double sqrt( long double f ) { return (long double)::sqrt(double(f) ); }

  inline float tan( float f ) { return ::tanf(f ); }
  inline double tan( double f ) { return (double)::tan(double(f) ); }
  inline long double tan( long double f ) { return (long double)::tan(double(f) ); }

  inline float tanh( float f ) { return ::tanhf(f ); }
  inline double tanh( double f ) { return (double)::tanh(double(f) ); }
  inline long double tanh( long double f ) { return (long double)::tanh(double(f) ); }
}

# define vcl_generic_cmath_STD std

#include "../generic/vcl_cmath.h"

#endif // vcl_win32_vc60_cmath_h_
