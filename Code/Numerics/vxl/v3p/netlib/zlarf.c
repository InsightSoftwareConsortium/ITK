/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublecomplex c_b3 = {0.,0.};
static doublecomplex c_b5 = {1.,0.};
static integer c__1 = 1;

/* Subroutine */ int zlarf_(side, m, n, v, incv, tau, c, ldc, work, side_len)
char *side;
integer *m, *n;
doublecomplex *v;
integer *incv;
doublecomplex *tau, *c;
integer *ldc;
doublecomplex *work;
ftnlen side_len;
{
    /* System generated locals */
    integer c_dim1, c_offset;
    doublecomplex z__1;

    /* Local variables */
    extern logical lsame_();
    extern /* Subroutine */ int zgerc_(), zgemv_();


/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLARF applies a complex elementary reflector H to a complex M-by-N */
/*  matrix C, from either the left or the right. H is represented in the
*/
/*  form */

/*        H = I - tau * v * v' */

/*  where tau is a complex scalar and v is a complex vector. */

/*  If tau = 0, then H is taken to be the unit matrix. */

/*  To apply H' (the conjugate transpose of H), supply conjg(tau) instead
*/
/*  tau. */

/*  Arguments */
/*  ========= */

/*  SIDE    (input) CHARACTER*1 */
/*          = 'L': form  H * C */
/*          = 'R': form  C * H */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix C. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix C. */

/*  V       (input) COMPLEX*16 array, dimension */
/*                     (1 + (M-1)*abs(INCV)) if SIDE = 'L' */
/*                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R' */
/*          The vector v in the representation of H. V is not used if */
/*          TAU = 0. */

/*  INCV    (input) INTEGER */
/*          The increment between elements of v. INCV <> 0. */

/*  TAU     (input) COMPLEX*16 */
/*          The value tau in the representation of H. */

/*  C       (input/output) COMPLEX*16 array, dimension (LDC,N) */
/*          On entry, the M-by-N matrix C. */
/*          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
*/
/*          or C * H if SIDE = 'R'. */

/*  LDC     (input) INTEGER */
/*          The leading dimension of the array C. LDC >= max(1,M). */

/*  WORK    (workspace) COMPLEX*16 array, dimension */
/*                         (N) if SIDE = 'L' */
/*                      or (M) if SIDE = 'R' */

/*  =====================================================================
*/

/*     .. Parameters .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Executable Statements .. */

    /* Parameter adjustments */
    --work;
    c_dim1 = *ldc;
    c_offset = c_dim1 + 1;
    c -= c_offset;
    --v;

    /* Function Body */
    if (lsame_(side, "L", 1L, 1L)) {

/*        Form  H * C */

        if (tau->r != 0. || tau->i != 0.) {

/*           w := C' * v */

            zgemv_("Conjugate transpose", m, n, &c_b5, &c[c_offset], ldc, &v[
                    1], incv, &c_b3, &work[1], &c__1, 19L);

/*           C := C - v * w' */

            z__1.r = -tau->r, z__1.i = -tau->i;
            zgerc_(m, n, &z__1, &v[1], incv, &work[1], &c__1, &c[c_offset],
                    ldc);
        }
    } else {

/*        Form  C * H */

        if (tau->r != 0. || tau->i != 0.) {

/*           w := C * v */

            zgemv_("No transpose", m, n, &c_b5, &c[c_offset], ldc, &v[1],
                    incv, &c_b3, &work[1], &c__1, 12L);

/*           C := C - w * v' */

            z__1.r = -tau->r, z__1.i = -tau->i;
            zgerc_(m, n, &z__1, &work[1], &c__1, &v[1], incv, &c[c_offset],
                    ldc);
        }
    }
    return 0;

/*     End of ZLARF */

} /* zlarf_ */

