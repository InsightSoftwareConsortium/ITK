#include "f2c.h"
#include "netlib.h"

doublereal dasum_(n, dx, incx)
const integer *n;
const doublereal *dx;
const integer *incx;
{
    /* Local variables */
    static integer i, m, nincx;
    static doublereal dtemp;

/*     takes the sum of the absolute values. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 3/93 to return if incx .le. 0. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    dtemp = 0.;
    if (*n <= 0 || *incx <= 0) {
        return dtemp;
    }
    if (*incx == 1) {

/*        code for increment equal to 1 */

        m = *n % 6;
        for (i = 0; i < m; ++i) {
            dtemp += abs(dx[i]);
        }
        for (i = m; i < *n; i += 6) {
            dtemp += abs(dx[i]) + abs(dx[i+1]) + abs(dx[i+2]) + abs(dx[i+3]) + abs(dx[i+4]) + abs(dx[i+5]);
        }
        return dtemp;
    }

/*        code for increment not equal to 1 */

    nincx = *n * *incx;
    for (i = 0; i < nincx; i += *incx) {
        dtemp += abs(dx[i]);
    }
    return dtemp;
} /* dasum_ */
