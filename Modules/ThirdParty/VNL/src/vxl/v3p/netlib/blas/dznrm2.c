/* blas/dznrm2.f -- translated by f2c (version 20050501).
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

/*<       DOUBLE PRECISION FUNCTION DZNRM2( N, X, INCX ) >*/
doublereal dznrm2_(integer *n, doublecomplex *x, integer *incx)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal ret_val, d__1;

    /* Builtin functions */
    double d_imag(doublecomplex *), sqrt(doublereal);

    /* Local variables */
    integer ix;
    doublereal ssq, temp, norm, scale;

/*     .. Scalar Arguments .. */
/*<       INTEGER                           INCX, N >*/
/*     .. Array Arguments .. */
/*<       COMPLEX*16                        X( * ) >*/
/*     .. */

/*  DZNRM2 returns the euclidean norm of a vector via the function */
/*  name, so that */

/*     DZNRM2 := sqrt( conjg( x' )*x ) */



/*  -- This version written on 25-October-1982. */
/*     Modified on 14-October-1993 to inline the call to ZLASSQ. */
/*     Sven Hammarling, Nag Ltd. */


/*     .. Parameters .. */
/*<       DOUBLE PRECISION      ONE         , ZERO >*/
/*<       PARAMETER           ( ONE = 1.0D+0, ZERO = 0.0D+0 ) >*/
/*     .. Local Scalars .. */
/*<       INTEGER               IX >*/
/*<       DOUBLE PRECISION      NORM, SCALE, SSQ, TEMP >*/
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC             ABS, DIMAG, DBLE, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */
/*<       IF( N.LT.1 .OR. INCX.LT.1 )THEN >*/
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n < 1 || *incx < 1) {
/*<          NORM  = ZERO >*/
        norm = 0.;
/*<       ELSE >*/
    } else {
/*<          SCALE = ZERO >*/
        scale = 0.;
/*<          SSQ   = ONE >*/
        ssq = 1.;
/*        The following loop is equivalent to this call to the LAPACK */
/*        auxiliary routine: */
/*        CALL ZLASSQ( N, X, INCX, SCALE, SSQ ) */

/*<          DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX >*/
        i__1 = (*n - 1) * *incx + 1;
        i__2 = *incx;
        for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
/*<             IF( DBLE( X( IX ) ).NE.ZERO )THEN >*/
            i__3 = ix;
            if (x[i__3].r != 0.) {
/*<                TEMP = ABS( DBLE( X( IX ) ) ) >*/
                i__3 = ix;
                temp = (d__1 = x[i__3].r, abs(d__1));
/*<                IF( SCALE.LT.TEMP )THEN >*/
                if (scale < temp) {
/*<                   SSQ   = ONE   + SSQ*( SCALE/TEMP )**2 >*/
/* Computing 2nd power */
                    d__1 = scale / temp;
                    ssq = ssq * (d__1 * d__1) + 1.;
/*<                   SCALE = TEMP >*/
                    scale = temp;
/*<                ELSE >*/
                } else {
/*<                   SSQ   = SSQ   +     ( TEMP/SCALE )**2 >*/
/* Computing 2nd power */
                    d__1 = temp / scale;
                    ssq += d__1 * d__1;
/*<                END IF >*/
                }
/*<             END IF >*/
            }
/*<             IF( DIMAG( X( IX ) ).NE.ZERO )THEN >*/
            if (d_imag(&x[ix]) != 0.) {
/*<                TEMP = ABS( DIMAG( X( IX ) ) ) >*/
                temp = (d__1 = d_imag(&x[ix]), abs(d__1));
/*<                IF( SCALE.LT.TEMP )THEN >*/
                if (scale < temp) {
/*<                   SSQ   = ONE   + SSQ*( SCALE/TEMP )**2 >*/
/* Computing 2nd power */
                    d__1 = scale / temp;
                    ssq = ssq * (d__1 * d__1) + 1.;
/*<                   SCALE = TEMP >*/
                    scale = temp;
/*<                ELSE >*/
                } else {
/*<                   SSQ   = SSQ   +     ( TEMP/SCALE )**2 >*/
/* Computing 2nd power */
                    d__1 = temp / scale;
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

/*<       DZNRM2 = NORM >*/
    ret_val = norm;
/*<       RETURN >*/
    return ret_val;

/*     End of DZNRM2. */

/*<       END >*/
} /* dznrm2_ */

#ifdef __cplusplus
        }
#endif
