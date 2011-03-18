/* lapack/double/dlapy2.f -- translated by f2c (version 20050501).
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

/*<       DOUBLE PRECISION FUNCTION DLAPY2( X, Y ) >*/
doublereal dlapy2_(doublereal *x, doublereal *y)
{
    /* System generated locals */
    doublereal ret_val, d__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal w, z__, xabs, yabs;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*     .. Scalar Arguments .. */
/*<       DOUBLE PRECISION   X, Y >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary */
/*  overflow. */

/*  Arguments */
/*  ========= */

/*  X       (input) DOUBLE PRECISION */
/*  Y       (input) DOUBLE PRECISION */
/*          X and Y specify the values x and y. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO >*/
/*<       PARAMETER          ( ZERO = 0.0D0 ) >*/
/*<       DOUBLE PRECISION   ONE >*/
/*<       PARAMETER          ( ONE = 1.0D0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       DOUBLE PRECISION   W, XABS, YABS, Z >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, MIN, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       XABS = ABS( X ) >*/
    xabs = abs(*x);
/*<       YABS = ABS( Y ) >*/
    yabs = abs(*y);
/*<       W = MAX( XABS, YABS ) >*/
    w = max(xabs,yabs);
/*<       Z = MIN( XABS, YABS ) >*/
    z__ = min(xabs,yabs);
/*<       IF( Z.EQ.ZERO ) THEN >*/
    if (z__ == 0.) {
/*<          DLAPY2 = W >*/
        ret_val = w;
/*<       ELSE >*/
    } else {
/*<          DLAPY2 = W*SQRT( ONE+( Z / W )**2 ) >*/
/* Computing 2nd power */
        d__1 = z__ / w;
        ret_val = w * sqrt(d__1 * d__1 + 1.);
/*<       END IF >*/
    }
/*<       RETURN >*/
    return ret_val;

/*     End of DLAPY2 */

/*<       END >*/
} /* dlapy2_ */

#ifdef __cplusplus
        }
#endif
