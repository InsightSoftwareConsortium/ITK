#include "f2c.h"
#include "netlib.h"

double d_sign(const doublereal *a, const doublereal *b)
{
  double x = (*a >= 0 ? *a : - *a);
  return *b >= 0 ? x : -x;
}
