#include "f2c.h"
#include "netlib.h"
extern double pow(double,double); /* #include <math.h> */

/* extern "C" */
double pow_dd(const double *x, const double *y) {
  return pow(*x, *y);
}
