/* blas/dnrm2.f -- translated by f2c (version 20050501).
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

/*<       DOUBLE PRECISION FUNCTION DNRM2 ( N, X, INCX ) >*/
doublereal dnrm2_(integer *n, doublereal *x, integer *incx)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal ret_val, d__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer ix;
    doublereal ssq, norm, scale, absxi;

/*     .. Scalar Arguments .. */
/*<       INTEGER                           INCX, N >*/
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION                  X( * ) >*/
/*     .. */

/*  DNRM2 returns the euclidean norm of a vector via the function */
/*  name, so that */

/*     DNRM2 := sqrt( x'*x ) */



/*  -- This version written on 25-October-1982. */
/*     Modified on 14-October-1993 to inline the call to DLASSQ. */
/*     Sven Hammarling, Nag Ltd. */


/*     .. Parameters .. */
/*<       DOUBLE PRECISION      ONE         , ZERO >*/
/*<       PARAMETER           ( ONE = 1.0D+0, ZERO = 0.0D+0 ) >*/
/*     .. Local Scalars .. */
/*<       INTEGER               IX >*/
/*<       DOUBLE PRECISION      ABSXI, NORM, SCALE, SSQ >*/
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC             ABS, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */
/*<       IF( N.LT.1 .OR. INCX.LT.1 )THEN >*/
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n < 1 || *incx < 1) {
/*<          NORM  = ZERO >*/
        norm = 0.;
/*<       ELSE IF( N.EQ.1 )THEN >*/
    } else if (*n == 1) {
/*<          NORM  = ABS( X( 1 ) ) >*/
        norm = abs(x[1]);
/*<       ELSE >*/
    } else {
/*<          SCALE = ZERO >*/
        scale = 0.;
/*<          SSQ   = ONE >*/
        ssq = 1.;
/*        The following loop is equivalent to this call to the LAPACK */
/*        auxiliary routine: */
/*        CALL DLASSQ( N, X, INCX, SCALE, SSQ ) */

/*<          DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX >*/
        i__1 = (*n - 1) * *incx + 1;
        i__2 = *incx;
        for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
/*<             IF( X( IX ).NE.ZERO )THEN >*/
            if (x[ix] != 0.) {
/*<                ABSXI = ABS( X( IX ) ) >*/
                absxi = (d__1 = x[ix], abs(d__1));
/*<                IF( SCALE.LT.ABSXI )THEN >*/
                if (scale < absxi) {
/*<                   SSQ   = ONE   + SSQ*( SCALE/ABSXI )**2 >*/
/* Computing 2nd power */
                    d__1 = scale / absxi;
                    ssq = ssq * (d__1 * d__1) + 1.;
/*<                   SCALE = ABSXI >*/
                    scale = absxi;
/*<                ELSE >*/
                } else {
/*<                   SSQ   = SSQ   +     ( ABSXI/SCALE )**2 >*/
/* Computing 2nd power */
                    d__1 = absxi / scale;
                    ssq += d__1 * d__1;
/*<                END IF >*/
                }
/*<             END IF >*/
            }
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<          NORM  = SCALE * SQRT( SSQ ) >*/
        norm = scale * sqrt(ssq);
/*<       END IF >*/
    }

/*<       DNRM2 = NORM >*/
    ret_val = norm;
/*<       RETURN >*/
    return ret_val;

/*     End of DNRM2. */

/*<       END >*/
} /* dnrm2_ */

#ifdef __cplusplus
        }
#endif
