#ifndef vcl_win32_vc70_cmath_h_
#define vcl_win32_vc70_cmath_h_

#include <cmath>

namespace std {
  inline float  abs(float x) { return x >= 0.0f ? x : -x; }
  inline double abs(double  x) { return x >= 0.0 ? x : -x; }
  inline long double abs(long double  x) { return x >= 0.0 ? x : -x; } 

  inline float acos( float f ) { return std::acosf(f ); }
  inline long double acos( long double f ) { return (long double)std::acos(double(f) ); }

  inline float asin( float f ) { return std::asinf(f ); }
  inline long double asin( long double f ) { return (long double)std::asin(double(f) ); }

  inline float atan( float f ) { return std::atanf(f ); }
  inline long double atan( long double f ) { return (long double)std::atan(double(f) ); }

  inline float atan2( float f, float g ) { return std::atan2f(f,g ); }
  inline long double atan2( long double f, long double g ) { return (long double)std::atan2(double(f),double(g) ); }

  inline float ceil( float f ) { return std::ceilf(f ); }
  inline long double ceil( long double f ) { return (long double)std::ceil(double(f) ); }

  inline float cos( float f ) { return std::cosf(f ); }
  inline long double cos( long double f ) { return (long double)std::cos(double(f) ); }

  inline float cosh( float f ) { return std::coshf(f ); }
  inline long double cosh( long double f ) { return (long double)std::cosh(double(f) ); }

  inline float exp( float f ) { return std::expf(f ); }
  inline long double exp( long double f ) { return (long double)std::exp(double(f) ); }

  inline float fabs( float f ) { return std::fabsf(f ); }
  inline long double fabs( long double f ) { return (long double)std::fabs(double(f) ); }

  inline float floor( float f ) { return std::floorf(f ); }
  inline long double floor( long double f ) { return (long double)std::floor(double(f) ); }

  inline float fmod( float f, float g) { return std::fmodf(f,g); }
  inline long double fmod( long double f, long double g) { return (long double)std::fmod(double(f),double(g)); }

  inline float frexp( float f, int* ip) { return std::frexpf(f, ip); }
  inline long double frexp( long double f, int* ip) { return (long double)std::frexp(double(f), ip); }

  inline float ldexp( float f, int i) { return std::ldexpf(f, i); }
  inline long double ldexp( long double f, int i) { return (long double)std::ldexp(double(f), i); }

  inline float log( float f ) { return std::logf(f ); }
  inline long double log( long double f ) { return (long double)std::log(double(f) ); }

  inline float log10( float f ) { return std::log10f(f ); }
  inline long double log10( long double f ) { return (long double)std::log10(double(f) ); }

  inline float modf( float f, float* fp ) { return std::modff(f, fp ); }
  inline long double modf( long double f, long double* fp ) { return (long double)std::modf(double(f), (double*)fp ); }

  inline float pow( float f, float g ) { return std::powf(f,g ); }
  inline long double pow( long double f, long double g ) { return (long double)std::pow(double(f),double(g) ); }

  inline float pow( float f, int i ) { return std::powf(f, float(i) ); }
  inline double pow( double f, int i ) { return std::pow(f, double(i) ); }
  inline long double pow( long double f, int i ) { return (long double)std::pow(double(f), double(i) ); }

  inline float sin( float f ) { return std::sinf(f ); }
  inline long double sin( long double f ) { return (long double)std::sin(double(f) ); }

  inline float sinh( float f ) { return std::sinhf(f ); }
  inline long double sinh( long double f ) { return (long double)std::sinh(double(f) ); }

  inline float sqrt( float f ) { return std::sqrtf(f ); }
  inline long double sqrt( long double f ) { return (long double)std::sqrt(double(f) ); }

  inline float tan( float f ) { return std::tanf(f ); }
  inline long double tan( long double f ) { return (long double)std::tan(double(f) ); }

  inline float tanh( float f ) { return std::tanhf(f ); }
  inline long double tanh( long double f ) { return (long double)std::tanh(double(f) ); }
}

#define vcl_generic_cmath_STD std

#include "../generic/vcl_cmath.h"

#endif // vcl_win32_vc70_cmath_h_
