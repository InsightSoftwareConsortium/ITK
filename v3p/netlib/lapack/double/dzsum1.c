/* lapack/double/dzsum1.f -- translated by f2c (version 20090411).
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

/*<       DOUBLE PRECISION FUNCTION DZSUM1( N, CX, INCX ) >*/
doublereal dzsum1_(integer *n, doublecomplex *cx, integer *incx)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal ret_val;

    /* Builtin functions */
    double z_abs(doublecomplex *);

    /* Local variables */
    integer i__, nincx;
    doublereal stemp;


/*  -- LAPACK auxiliary routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INCX, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         CX( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DZSUM1 takes the sum of the absolute values of a complex */
/*  vector and returns a double precision result. */

/*  Based on DZASUM from the Level 1 BLAS. */
/*  The change is to use the 'genuine' absolute value. */

/*  Contributed by Nick Higham for use with ZLACON. */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The number of elements in the vector CX. */

/*  CX      (input) COMPLEX*16 array, dimension (N) */
/*          The vector whose elements will be summed. */

/*  INCX    (input) INTEGER */
/*          The spacing between successive values of CX.  INCX > 0. */

/*  ===================================================================== */

/*     .. Local Scalars .. */
/*<       INTEGER            I, NINCX >*/
/*<       DOUBLE PRECISION   STEMP >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       DZSUM1 = 0.0D0 >*/
    /* Parameter adjustments */
    --cx;

    /* Function Body */
    ret_val = 0.;
/*<       STEMP = 0.0D0 >*/
    stemp = 0.;
/*<    >*/
    if (*n <= 0) {
        return ret_val;
    }
/*<    >*/
    if (*incx == 1) {
        goto L20;
    }

/*     CODE FOR INCREMENT NOT EQUAL TO 1 */

/*<       NINCX = N*INCX >*/
    nincx = *n * *incx;
/*<       DO 10 I = 1, NINCX, INCX >*/
    i__1 = nincx;
    i__2 = *incx;
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {

/*        NEXT LINE MODIFIED. */

/*<          STEMP = STEMP + ABS( CX( I ) ) >*/
        stemp += z_abs(&cx[i__]);
/*<    10 CONTINUE >*/
/* L10: */
    }
/*<       DZSUM1 = STEMP >*/
    ret_val = stemp;
/*<       RETURN >*/
    return ret_val;

/*     CODE FOR INCREMENT EQUAL TO 1 */

/*<    20 CONTINUE >*/
L20:
/*<       DO 30 I = 1, N >*/
    i__2 = *n;
    for (i__ = 1; i__ <= i__2; ++i__) {

/*        NEXT LINE MODIFIED. */

/*<          STEMP = STEMP + ABS( CX( I ) ) >*/
        stemp += z_abs(&cx[i__]);
/*<    30 CONTINUE >*/
/* L30: */
    }
/*<       DZSUM1 = STEMP >*/
    ret_val = stemp;
/*<       RETURN >*/
    return ret_val;

/*     End of DZSUM1 */

/*<       END >*/
} /* dzsum1_ */

#ifdef __cplusplus
        }
#endif
