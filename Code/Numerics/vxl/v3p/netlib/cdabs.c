/*
  fsm@robots.ox.ac.uk
*/
#include <math.h>
#include "f2c.h"

doublereal cdabs_(doublecomplex const *z) {
  return hypot(z->r, z->i); /* is this what cdabs_() is supposed to do? */
}
