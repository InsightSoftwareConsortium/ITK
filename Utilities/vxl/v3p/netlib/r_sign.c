#include "f2c.h"
#include "netlib.h"

#ifdef KR_headers
real r_sign(a,b) const real *a, *b;
#else
real r_sign(const real *a, const real *b)
#endif
{
  real x = (*a >= 0 ? *a : - *a);
  return *b >= 0 ? x : -x;
}
