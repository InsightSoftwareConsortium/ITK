#include "f2c.h"
#include "netlib.h"

#define log10e 0.43429448190325182765

#ifdef KR_headers
double log();
double d_lg10(x) const doublereal *x;
#else
extern double log(double); /* #include <math.h> */
double d_lg10(const doublereal *x)
#endif
{
  return( log10e * log(*x) );
}
