/*
  fsm@robots.ox.ac.uk
*/
#include <math.h>
#include "f2c.h"

/* This is here until I find the fortran code for c_sqrt(). */

void c_sqrt(complex       *dst, complex       *src)
{
  real       a = src->r;
  real       b = src->i;

  real       theta = atan2(b,a);
  real       r = hypot(a,b);

  theta *= 0.5;
  r = sqrt(r);

  dst->r = r * cos(theta);
  dst->i = r * sin(theta);
}

