#include "f2c.h"
#include "netlib.h"

 VOID
#ifdef KR_headers
d_cnjg(r, z) doublecomplex *r; const doublecomplex *z;
#else
d_cnjg(doublecomplex *r, const doublecomplex *z)
#endif
{
    r->r = z->r;
    r->i = - z->i;
}
