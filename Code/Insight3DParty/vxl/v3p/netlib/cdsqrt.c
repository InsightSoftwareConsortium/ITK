/*
  fsm@robots.ox.ac.uk
*/
#include <math.h>
#include "f2c.h"

doublereal cdsqrt_(doublecomplex const *z) {
  return hypot(z->r, z->i);
}
