#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* A slightly more efficient implementation for complex square roots, */
/* which does not use any of hypot(), atan2(), cos(), sin(). */
/* Author: Peter Vanroose, June 2001. */

/* Note that the imaginary part of the returned value will never be negative. */
/* The other complex square root is just minus the one returned here.  */

void z_sqrt(doublecomplex *ret_value, const doublecomplex *z)
{
  doublereal w = z_abs(z);

  ret_value->r = sqrt((w+z->r)/2.);
  ret_value->i = sqrt((w-z->r)/2.);
  if (z->i < 0.)
    ret_value->r = - ret_value->r;
  return;

#if 0
  /* was: (fsm) */
  doublereal a = z->r;
  doublereal b = z->i;
  doublereal r = hypot(a,b);
  doublereal theta = atan2(b,a);
  theta *= 0.5;
  r = sqrt(r);
  ret_value->r = r * cos(theta);
  ret_value->i = r * sin(theta);
#endif
}

