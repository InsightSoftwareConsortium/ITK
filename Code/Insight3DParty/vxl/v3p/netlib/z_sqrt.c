/*
  fsm@robots.ox.ac.uk
*/
#include <math.h>
#include "f2c.h"

/* This is here until I find the fortran code for z_sqrt(). */

void z_sqrt(doublecomplex *dst, doublecomplex *src)
{
  doublereal a = src->r;
  doublereal b = src->i;

  doublereal theta = atan2(b,a);
  doublereal r = hypot(a,b);

  theta *= 0.5;
  r = sqrt(r);

  dst->r = r * cos(theta);
  dst->i = r * sin(theta);
}

