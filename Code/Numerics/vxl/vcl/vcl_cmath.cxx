/*
  fsm@robots.ox.ac.uk
*/
#ifdef __GNUC__
#pragma implementation
#endif
#include "vcl_cmath.h"

#ifdef __GNUC__
double tickle_cmath_inlines()
{
  float f = vcl_abs(1.0f);
  double d = vcl_abs(1.0);
  return f + d;
}
#endif


#if defined(__SUNPRO_CC) && (__SUNPRO_CC == 0x500)
// these symbols go missing at link time. I suspect
// they are not provided in the run-time library.
// fsm
namespace std {
  double abs(double x) { return x >= 0 ? x : -x; }
  float atan2(float y, float x) { return float(std::atan2(double(y), double(x))); }
  float sqrt(float x) { return float(std::sqrt(double(x))); }
  float ceil(float x) { return float(std::ceil(double(x))); }
  float floor(float x) { return float(std::floor(double(x))); }
  float sin(float x) { return float(std::sin(double(x))); }
  float fabs(float x) { return float(std::fabs(double(x))); }
  float cos(float x) { return float(std::cos(double(x))); }
  float pow(float x, float a) { return float(std::pow(double(x), double(a))); }
  float exp(float x) { return float(std::exp(double(x))); }
  float log(float x) { return float(std::log(double(x))); }
  float acos(float x) { return float(std::acos(double(x))); }
}
#endif
