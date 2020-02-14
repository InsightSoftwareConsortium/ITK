/* lapack/single/slapy2.f -- translated by f2c (version 20050501).
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

/*<       REAL             FUNCTION SLAPY2( X, Y ) >*/
doublereal slapy2_(real *x, real *y)
{
    /* System generated locals */
    real ret_val, r__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    real w, z__, xabs, yabs;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*     .. Scalar Arguments .. */
/*<       REAL               X, Y >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  SLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary */
/*  overflow. */

/*  Arguments */
/*  ========= */

/*  X       (input) REAL */
/*  Y       (input) REAL */
/*          X and Y specify the values x and y. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       REAL               ZERO >*/
/*<       PARAMETER          ( ZERO = 0.0E0 ) >*/
/*<       REAL               ONE >*/
/*<       PARAMETER          ( ONE = 1.0E0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       REAL               W, XABS, YABS, Z >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, MIN, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       XABS = ABS( X ) >*/
    xabs = dabs(*x);
/*<       YABS = ABS( Y ) >*/
    yabs = dabs(*y);
/*<       W = MAX( XABS, YABS ) >*/
    w = dmax(xabs,yabs);
/*<       Z = MIN( XABS, YABS ) >*/
    z__ = dmin(xabs,yabs);
/*<       IF( Z.EQ.ZERO ) THEN >*/
    if (z__ == (float)0.) {
/*<          SLAPY2 = W >*/
        ret_val = w;
/*<       ELSE >*/
    } else {
/*<          SLAPY2 = W*SQRT( ONE+( Z / W )**2 ) >*/
/* Computing 2nd power */
        r__1 = z__ / w;
        ret_val = w * sqrt(r__1 * r__1 + (float)1.);
/*<       END IF >*/
    }
/*<       RETURN >*/
    return ret_val;

/*     End of SLAPY2 */

/*<       END >*/
} /* slapy2_ */

#ifdef __cplusplus
        }
#endif
