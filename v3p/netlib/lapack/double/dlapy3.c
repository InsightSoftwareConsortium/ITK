/* lapack/double/dlapy3.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
        on Microsoft Windows system, link with libf2c.lib;
        on Linux or Unix systems, link with .../path/to/libf2c.a -lm
        or, if you install libf2c.a in a standard place, with -lf2c -lm
        -- in that order, at the end of the command line, as in
                cc *.o -lf2c -lm
        Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

                http://www.netlib.org/f2c/libf2c.zip
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "v3p_netlib.h"

/*<       DOUBLE PRECISION FUNCTION DLAPY3( X, Y, Z ) >*/
doublereal dlapy3_(doublereal *x, doublereal *y, doublereal *z__)
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal w, xabs, yabs, zabs;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*     .. Scalar Arguments .. */
/*<       DOUBLE PRECISION   X, Y, Z >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLAPY3 returns sqrt(x**2+y**2+z**2), taking care not to cause */
/*  unnecessary overflow. */

/*  Arguments */
/*  ========= */

/*  X       (input) DOUBLE PRECISION */
/*  Y       (input) DOUBLE PRECISION */
/*  Z       (input) DOUBLE PRECISION */
/*          X, Y and Z specify the values x, y and z. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO >*/
/*<       PARAMETER          ( ZERO = 0.0D0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       DOUBLE PRECISION   W, XABS, YABS, ZABS >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       XABS = ABS( X ) >*/
    xabs = abs(*x);
/*<       YABS = ABS( Y ) >*/
    yabs = abs(*y);
/*<       ZABS = ABS( Z ) >*/
    zabs = abs(*z__);
/*<       W = MAX( XABS, YABS, ZABS ) >*/
/* Computing MAX */
    d__1 = max(xabs,yabs);
    w = max(d__1,zabs);
/*<       IF( W.EQ.ZERO ) THEN >*/
    if (w == 0.) {
/*<          DLAPY3 = ZERO >*/
        ret_val = 0.;
/*<       ELSE >*/
    } else {
/*<    >*/
/* Computing 2nd power */
        d__1 = xabs / w;
/* Computing 2nd power */
        d__2 = yabs / w;
/* Computing 2nd power */
        d__3 = zabs / w;
        ret_val = w * sqrt(d__1 * d__1 + d__2 * d__2 + d__3 * d__3);
/*<       END IF >*/
    }
/*<       RETURN >*/
    return ret_val;

/*     End of DLAPY3 */

/*<       END >*/
} /* dlapy3_ */

#ifdef __cplusplus
        }
#endif
