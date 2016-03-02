/* lapack/complex16/zlacgv.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE ZLACGV( N, X, INCX ) >*/
/* Subroutine */ int zlacgv_(integer *n, doublecomplex *x, integer *incx)
{
    /* System generated locals */
    integer i__1, i__2;
    doublecomplex z__1;

    /* Builtin functions */
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    integer i__, ioff;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INCX, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         X( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLACGV conjugates a complex vector of length N. */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The length of the vector X.  N >= 0. */

/*  X       (input/output) COMPLEX*16 array, dimension */
/*                         (1+(N-1)*abs(INCX)) */
/*          On entry, the vector of length N to be conjugated. */
/*          On exit, X is overwritten with conjg(X). */

/*  INCX    (input) INTEGER */
/*          The spacing between successive elements of X. */

/* ===================================================================== */

/*     .. Local Scalars .. */
/*<       INTEGER            I, IOFF >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          DCONJG >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       IF( INCX.EQ.1 ) THEN >*/
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*incx == 1) {
/*<          DO 10 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             X( I ) = DCONJG( X( I ) ) >*/
            i__2 = i__;
            d_cnjg(&z__1, &x[i__]);
            x[i__2].r = z__1.r, x[i__2].i = z__1.i;
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<       ELSE >*/
    } else {
/*<          IOFF = 1 >*/
        ioff = 1;
/*<    >*/
        if (*incx < 0) {
            ioff = 1 - (*n - 1) * *incx;
        }
/*<          DO 20 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             X( IOFF ) = DCONJG( X( IOFF ) ) >*/
            i__2 = ioff;
            d_cnjg(&z__1, &x[ioff]);
            x[i__2].r = z__1.r, x[i__2].i = z__1.i;
/*<             IOFF = IOFF + INCX >*/
            ioff += *incx;
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<       END IF >*/
    }
/*<       RETURN >*/
    return 0;

/*     End of ZLACGV */

/*<       END >*/
} /* zlacgv_ */

#ifdef __cplusplus
        }
#endif
