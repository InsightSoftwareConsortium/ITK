#include "f2c.h"
#include "netlib.h"

doublereal pythag_(a, b)
const doublereal *a, *b;
{
    /* Local variables */
    static doublereal p, r, s, t, u;

/* ********************************************************************** */
/*     finds sqrt(a**2+b**2) without overflow or destructive underflow    */
/* ********************************************************************** */

    p = max(abs(*a),abs(*b));
    if (p == 0.) {
        return p;
    }
    r = min(abs(*a),abs(*b)) / p;
    r *= r;
    while ((t = r+4.) != 4.) {
        s = r / t;
        u = s * 2. + 1.;
        p *= u;
        u = s / u;
        r *= u * u;
    }
    return p;
} /* pythag_ */
