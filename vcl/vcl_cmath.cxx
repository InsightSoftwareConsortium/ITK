// This is vcl/vcl_cmath.cxx

//:
// \file
// \author fsm

#include "vcl_cmath.h"

#ifdef __GNUC__
double tickle_cmath_inlines()
{
  float f = vcl_abs(1.0f);
  double d = vcl_abs(1.0);
  return f + d;
}
#endif

