/* lapack/complex16/zrot.f -- translated by f2c (version 20090411).
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

/*<       SUBROUTINE ZROT( N, CX, INCX, CY, INCY, C, S ) >*/
/* Subroutine */ int zrot_(integer *n, doublecomplex *cx, integer *incx,
        doublecomplex *cy, integer *incy, doublereal *c__, doublecomplex *s)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublecomplex z__1, z__2, z__3, z__4;

    /* Builtin functions */
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    integer i__, ix, iy;
    doublecomplex stemp;


/*  -- LAPACK auxiliary routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INCX, INCY, N >*/
/*<       DOUBLE PRECISION   C >*/
/*<       COMPLEX*16         S >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         CX( * ), CY( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZROT   applies a plane rotation, where the cos (C) is real and the */
/*  sin (S) is complex, and the vectors CX and CY are complex. */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The number of elements in the vectors CX and CY. */

/*  CX      (input/output) COMPLEX*16 array, dimension (N) */
/*          On input, the vector X. */
/*          On output, CX is overwritten with C*X + S*Y. */

/*  INCX    (input) INTEGER */
/*          The increment between successive values of CY.  INCX <> 0. */

/*  CY      (input/output) COMPLEX*16 array, dimension (N) */
/*          On input, the vector Y. */
/*          On output, CY is overwritten with -CONJG(S)*X + C*Y. */

/*  INCY    (input) INTEGER */
/*          The increment between successive values of CY.  INCX <> 0. */

/*  C       (input) DOUBLE PRECISION */
/*  S       (input) COMPLEX*16 */
/*          C and S define a rotation */
/*             [  C          S  ] */
/*             [ -conjg(S)   C  ] */
/*          where C*C + S*CONJG(S) = 1.0. */

/* ===================================================================== */

/*     .. Local Scalars .. */
/*<       INTEGER            I, IX, IY >*/
/*<       COMPLEX*16         STEMP >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          DCONJG >*/
/*     .. */
/*     .. Executable Statements .. */

/*<    >*/
    /* Parameter adjustments */
    --cy;
    --cx;

    /* Function Body */
    if (*n <= 0) {
        return 0;
    }
/*<    >*/
    if (*incx == 1 && *incy == 1) {
        goto L20;
    }

/*     Code for unequal increments or equal increments not equal to 1 */

/*<       IX = 1 >*/
    ix = 1;
/*<       IY = 1 >*/
    iy = 1;
/*<    >*/
    if (*incx < 0) {
        ix = (-(*n) + 1) * *incx + 1;
    }
/*<    >*/
    if (*incy < 0) {
        iy = (-(*n) + 1) * *incy + 1;
    }
/*<       DO 10 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          STEMP = C*CX( IX ) + S*CY( IY ) >*/
        i__2 = ix;
        z__2.r = *c__ * cx[i__2].r, z__2.i = *c__ * cx[i__2].i;
        i__3 = iy;
        z__3.r = s->r * cy[i__3].r - s->i * cy[i__3].i, z__3.i = s->r * cy[
                i__3].i + s->i * cy[i__3].r;
        z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
        stemp.r = z__1.r, stemp.i = z__1.i;
/*<          CY( IY ) = C*CY( IY ) - DCONJG( S )*CX( IX ) >*/
        i__2 = iy;
        i__3 = iy;
        z__2.r = *c__ * cy[i__3].r, z__2.i = *c__ * cy[i__3].i;
        d_cnjg(&z__4, s);
        i__4 = ix;
        z__3.r = z__4.r * cx[i__4].r - z__4.i * cx[i__4].i, z__3.i = z__4.r *
                cx[i__4].i + z__4.i * cx[i__4].r;
        z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
        cy[i__2].r = z__1.r, cy[i__2].i = z__1.i;
/*<          CX( IX ) = STEMP >*/
        i__2 = ix;
        cx[i__2].r = stemp.r, cx[i__2].i = stemp.i;
/*<          IX = IX + INCX >*/
        ix += *incx;
/*<          IY = IY + INCY >*/
        iy += *incy;
/*<    10 CONTINUE >*/
/* L10: */
    }
/*<       RETURN >*/
    return 0;

/*     Code for both increments equal to 1 */

/*<    20 CONTINUE >*/
L20:
/*<       DO 30 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          STEMP = C*CX( I ) + S*CY( I ) >*/
        i__2 = i__;
        z__2.r = *c__ * cx[i__2].r, z__2.i = *c__ * cx[i__2].i;
        i__3 = i__;
        z__3.r = s->r * cy[i__3].r - s->i * cy[i__3].i, z__3.i = s->r * cy[
                i__3].i + s->i * cy[i__3].r;
        z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
        stemp.r = z__1.r, stemp.i = z__1.i;
/*<          CY( I ) = C*CY( I ) - DCONJG( S )*CX( I ) >*/
        i__2 = i__;
        i__3 = i__;
        z__2.r = *c__ * cy[i__3].r, z__2.i = *c__ * cy[i__3].i;
        d_cnjg(&z__4, s);
        i__4 = i__;
        z__3.r = z__4.r * cx[i__4].r - z__4.i * cx[i__4].i, z__3.i = z__4.r *
                cx[i__4].i + z__4.i * cx[i__4].r;
        z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
        cy[i__2].r = z__1.r, cy[i__2].i = z__1.i;
/*<          CX( I ) = STEMP >*/
        i__2 = i__;
        cx[i__2].r = stemp.r, cx[i__2].i = stemp.i;
/*<    30 CONTINUE >*/
/* L30: */
    }
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* zrot_ */

#ifdef __cplusplus
        }
#endif
