#include "f2c.h"
#include "netlib.h"

#ifdef KR_headers
extern double f__cabs();
#else
extern double f__cabs(double, double);
#endif

#ifdef KR_headers
real c_abs(z) const complex *z;
#else
real c_abs(const complex *z)
#endif
{
return( (real)f__cabs( z->r, z->i ) );
}
