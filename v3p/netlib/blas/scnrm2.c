/* blas/scnrm2.f -- translated by f2c (version 20050501).
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

/*<       REAL             FUNCTION SCNRM2( N, X, INCX ) >*/
doublereal scnrm2_(integer *n, complex *x, integer *incx)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    real ret_val, r__1;

    /* Builtin functions */
    double r_imag(complex *), sqrt(doublereal);

    /* Local variables */
    integer ix;
    real ssq, temp, norm, scale;

/*     .. Scalar Arguments .. */
/*<       INTEGER                           INCX, N >*/
/*     .. Array Arguments .. */
/*<       COMPLEX                           X( * ) >*/
/*     .. */

/*  SCNRM2 returns the euclidean norm of a vector via the function */
/*  name, so that */

/*     SCNRM2 := sqrt( conjg( x' )*x ) */



/*  -- This version written on 25-October-1982. */
/*     Modified on 14-October-1993 to inline the call to CLASSQ. */
/*     Sven Hammarling, Nag Ltd. */


/*     .. Parameters .. */
/*<       REAL                  ONE         , ZERO >*/
/*<       PARAMETER           ( ONE = 1.0E+0, ZERO = 0.0E+0 ) >*/
/*     .. Local Scalars .. */
/*<       INTEGER               IX >*/
/*<       REAL                  NORM, SCALE, SSQ, TEMP >*/
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC             ABS, AIMAG, REAL, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */
/*<       IF( N.LT.1 .OR. INCX.LT.1 )THEN >*/
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n < 1 || *incx < 1) {
/*<          NORM  = ZERO >*/
        norm = (float)0.;
/*<       ELSE >*/
    } else {
/*<          SCALE = ZERO >*/
        scale = (float)0.;
/*<          SSQ   = ONE >*/
        ssq = (float)1.;
/*        The following loop is equivalent to this call to the LAPACK */
/*        auxiliary routine: */
/*        CALL CLASSQ( N, X, INCX, SCALE, SSQ ) */

/*<          DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX >*/
        i__1 = (*n - 1) * *incx + 1;
        i__2 = *incx;
        for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
/*<             IF( REAL( X( IX ) ).NE.ZERO )THEN >*/
            i__3 = ix;
            if (x[i__3].r != (float)0.) {
/*<                TEMP = ABS( REAL( X( IX ) ) ) >*/
                i__3 = ix;
                temp = (r__1 = x[i__3].r, dabs(r__1));
/*<                IF( SCALE.LT.TEMP )THEN >*/
                if (scale < temp) {
/*<                   SSQ   = ONE   + SSQ*( SCALE/TEMP )**2 >*/
/* Computing 2nd power */
                    r__1 = scale / temp;
                    ssq = ssq * (r__1 * r__1) + (float)1.;
/*<                   SCALE = TEMP >*/
                    scale = temp;
/*<                ELSE >*/
                } else {
/*<                   SSQ   = SSQ   +     ( TEMP/SCALE )**2 >*/
/* Computing 2nd power */
                    r__1 = temp / scale;
                    ssq += r__1 * r__1;
/*<                END IF >*/
                }
/*<             END IF >*/
            }
/*<             IF( AIMAG( X( IX ) ).NE.ZERO )THEN >*/
            if (r_imag(&x[ix]) != (float)0.) {
/*<                TEMP = ABS( AIMAG( X( IX ) ) ) >*/
                temp = (r__1 = r_imag(&x[ix]), dabs(r__1));
/*<                IF( SCALE.LT.TEMP )THEN >*/
                if (scale < temp) {
/*<                   SSQ   = ONE   + SSQ*( SCALE/TEMP )**2 >*/
/* Computing 2nd power */
                    r__1 = scale / temp;
                    ssq = ssq * (r__1 * r__1) + (float)1.;
/*<                   SCALE = TEMP >*/
                    scale = temp;
/*<                ELSE >*/
                } else {
/*<                   SSQ   = SSQ   +     ( TEMP/SCALE )**2 >*/
/* Computing 2nd power */
                    r__1 = temp / scale;
                    ssq += r__1 * r__1;
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

/*<       SCNRM2 = NORM >*/
    ret_val = norm;
/*<       RETURN >*/
    return ret_val;

/*     End of SCNRM2. */

/*<       END >*/
} /* scnrm2_ */

#ifdef __cplusplus
        }
#endif
