#include "f2c.h"
#include "netlib.h"

#ifdef KR_headers
double z_abs(z) const doublecomplex *z;
#else
double z_abs(const doublecomplex *z)
#endif
{
  return( f__cabs( z->r, z->i ) );
}
