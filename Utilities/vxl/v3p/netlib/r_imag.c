#include "f2c.h"
#include "netlib.h"

#ifdef KR_headers
real r_imag(z) const complex *z;
#else
real r_imag(const complex *z)
#endif
{
    return z->i;
}
