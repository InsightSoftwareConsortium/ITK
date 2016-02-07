/* lapack/util/izmax1.f -- translated by f2c (version 20090411).
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

/*<       INTEGER          FUNCTION IZMAX1( N, CX, INCX ) >*/
integer izmax1_(integer *n, doublecomplex *cx, integer *incx)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Builtin functions */
    double z_abs(doublecomplex *);

    /* Local variables */
    integer i__, ix;
    doublereal smax;


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

/*  IZMAX1 finds the index of the element whose real part has maximum */
/*  absolute value. */

/*  Based on IZAMAX from Level 1 BLAS. */
/*  The change is to use the 'genuine' absolute value. */

/*  Contributed by Nick Higham for use with ZLACON. */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The number of elements in the vector CX. */

/*  CX      (input) COMPLEX*16 array, dimension (N) */
/*          The vector whose elements will be summed. */

/*  INCX    (input) INTEGER */
/*          The spacing between successive values of CX.  INCX >= 1. */

/* ===================================================================== */

/*     .. Local Scalars .. */
/*<       INTEGER            I, IX >*/
/*<       DOUBLE PRECISION   SMAX >*/
/*<       COMPLEX*16         ZDUM >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS >*/
/*     .. */
/*     .. Statement Functions .. */
/*<       DOUBLE PRECISION   CABS1 >*/
/*     .. */
/*     .. Statement Function definitions .. */

/*     NEXT LINE IS THE ONLY MODIFICATION. */
/*<       CABS1( ZDUM ) = ABS( ZDUM ) >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       IZMAX1 = 0 >*/
    /* Parameter adjustments */
    --cx;

    /* Function Body */
    ret_val = 0;
/*<    >*/
    if (*n < 1) {
        return ret_val;
    }
/*<       IZMAX1 = 1 >*/
    ret_val = 1;
/*<    >*/
    if (*n == 1) {
        return ret_val;
    }
/*<    >*/
    if (*incx == 1) {
        goto L30;
    }

/*     CODE FOR INCREMENT NOT EQUAL TO 1 */

/*<       IX = 1 >*/
    ix = 1;
/*<       SMAX = CABS1( CX( 1 ) ) >*/
    smax = z_abs(&cx[1]);
/*<       IX = IX + INCX >*/
    ix += *incx;
/*<       DO 20 I = 2, N >*/
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*<    >*/
        if (z_abs(&cx[ix]) <= smax) {
            goto L10;
        }
/*<          IZMAX1 = I >*/
        ret_val = i__;
/*<          SMAX = CABS1( CX( IX ) ) >*/
        smax = z_abs(&cx[ix]);
/*<    10    CONTINUE >*/
L10:
/*<          IX = IX + INCX >*/
        ix += *incx;
/*<    20 CONTINUE >*/
/* L20: */
    }
/*<       RETURN >*/
    return ret_val;

/*     CODE FOR INCREMENT EQUAL TO 1 */

/*<    30 CONTINUE >*/
L30:
/*<       SMAX = CABS1( CX( 1 ) ) >*/
    smax = z_abs(&cx[1]);
/*<       DO 40 I = 2, N >*/
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*<    >*/
        if (z_abs(&cx[i__]) <= smax) {
            goto L40;
        }
/*<          IZMAX1 = I >*/
        ret_val = i__;
/*<          SMAX = CABS1( CX( I ) ) >*/
        smax = z_abs(&cx[i__]);
/*<    40 CONTINUE >*/
L40:
        ;
    }
/*<       RETURN >*/
    return ret_val;

/*     End of IZMAX1 */

/*<       END >*/
} /* izmax1_ */

#ifdef __cplusplus
        }
#endif
