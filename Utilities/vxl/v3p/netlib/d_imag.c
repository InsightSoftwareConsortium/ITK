#include "f2c.h"
#include "netlib.h"

doublereal d_imag(const doublecomplex *z)
{
  return  z->i;
}
