#include "f2c.h"
#include "netlib.h"

#ifdef KR_headers
VOID r_cnjg(r, z) complex *r, const complex *z;
#else
VOID r_cnjg(complex *r, const complex *z)
#endif
{
    r->r = z->r;
    r->i = - z->i;
}
