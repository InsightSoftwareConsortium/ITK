/* lapack/complex16/zlassq.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE ZLASSQ( N, X, INCX, SCALE, SUMSQ ) >*/
/* Subroutine */ int zlassq_(integer *n, doublecomplex *x, integer *incx,
        doublereal *scale, doublereal *sumsq)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    double d_imag(doublecomplex *);

    /* Local variables */
    integer ix;
    doublereal temp1;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INCX, N >*/
/*<       DOUBLE PRECISION   SCALE, SUMSQ >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         X( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLASSQ returns the values scl and ssq such that */

/*     ( scl**2 )*ssq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq, */

/*  where x( i ) = abs( X( 1 + ( i - 1 )*INCX ) ). The value of sumsq is */
/*  assumed to be at least unity and the value of ssq will then satisfy */

/*     1.0 .le. ssq .le. ( sumsq + 2*n ). */

/*  scale is assumed to be non-negative and scl returns the value */

/*     scl = max( scale, abs( real( x( i ) ) ), abs( aimag( x( i ) ) ) ), */
/*            i */

/*  scale and sumsq must be supplied in SCALE and SUMSQ respectively. */
/*  SCALE and SUMSQ are overwritten by scl and ssq respectively. */

/*  The routine makes only one pass through the vector X. */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The number of elements to be used from the vector X. */

/*  X       (input) COMPLEX*16 array, dimension (N) */
/*          The vector x as described above. */
/*             x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n. */

/*  INCX    (input) INTEGER */
/*          The increment between successive values of the vector X. */
/*          INCX > 0. */

/*  SCALE   (input/output) DOUBLE PRECISION */
/*          On entry, the value  scale  in the equation above. */
/*          On exit, SCALE is overwritten with the value  scl . */

/*  SUMSQ   (input/output) DOUBLE PRECISION */
/*          On entry, the value  sumsq  in the equation above. */
/*          On exit, SUMSQ is overwritten with the value  ssq . */

/* ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO >*/
/*<       PARAMETER          ( ZERO = 0.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            IX >*/
/*<       DOUBLE PRECISION   TEMP1 >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, DBLE, DIMAG >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       IF( N.GT.0 ) THEN >*/
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n > 0) {
/*<          DO 10 IX = 1, 1 + ( N-1 )*INCX, INCX >*/
        i__1 = (*n - 1) * *incx + 1;
        i__2 = *incx;
        for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
/*<             IF( DBLE( X( IX ) ).NE.ZERO ) THEN >*/
            i__3 = ix;
            if (x[i__3].r != 0.) {
/*<                TEMP1 = ABS( DBLE( X( IX ) ) ) >*/
                i__3 = ix;
                temp1 = (d__1 = x[i__3].r, abs(d__1));
/*<                IF( SCALE.LT.TEMP1 ) THEN >*/
                if (*scale < temp1) {
/*<                   SUMSQ = 1 + SUMSQ*( SCALE / TEMP1 )**2 >*/
/* Computing 2nd power */
                    d__1 = *scale / temp1;
                    *sumsq = *sumsq * (d__1 * d__1) + 1;
/*<                   SCALE = TEMP1 >*/
                    *scale = temp1;
/*<                ELSE >*/
                } else {
/*<                   SUMSQ = SUMSQ + ( TEMP1 / SCALE )**2 >*/
/* Computing 2nd power */
                    d__1 = temp1 / *scale;
                    *sumsq += d__1 * d__1;
/*<                END IF >*/
                }
/*<             END IF >*/
            }
/*<             IF( DIMAG( X( IX ) ).NE.ZERO ) THEN >*/
            if (d_imag(&x[ix]) != 0.) {
/*<                TEMP1 = ABS( DIMAG( X( IX ) ) ) >*/
                temp1 = (d__1 = d_imag(&x[ix]), abs(d__1));
/*<                IF( SCALE.LT.TEMP1 ) THEN >*/
                if (*scale < temp1) {
/*<                   SUMSQ = 1 + SUMSQ*( SCALE / TEMP1 )**2 >*/
/* Computing 2nd power */
                    d__1 = *scale / temp1;
                    *sumsq = *sumsq * (d__1 * d__1) + 1;
/*<                   SCALE = TEMP1 >*/
                    *scale = temp1;
/*<                ELSE >*/
                } else {
/*<                   SUMSQ = SUMSQ + ( TEMP1 / SCALE )**2 >*/
/* Computing 2nd power */
                    d__1 = temp1 / *scale;
                    *sumsq += d__1 * d__1;
/*<                END IF >*/
                }
/*<             END IF >*/
            }
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of ZLASSQ */

/*<       END >*/
} /* zlassq_ */

#ifdef __cplusplus
        }
#endif
