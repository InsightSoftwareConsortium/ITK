#include "f2c.h"
#include "netlib.h"

#ifdef KR_headers
real pow_ri(ap, bp) const real *ap; const integer *bp;
#else
real pow_ri(const real *ap, const integer *bp)
#endif
{
real pow, x;
integer n;
unsigned long u;

pow = 1;
x = *ap;
n = *bp;

if(n != 0)
        {
        if(n < 0)
                {
                n = -n;
                x = 1/x;
                }
        for(u = n; ; )
                {
                if(u & 01)
                        pow *= x;
                if(u >>= 1)
                        x *= x;
                else
                        break;
                }
        }
return pow;
}
