/* lapack/complex16/zlarf.f -- translated by f2c (version 20050501).
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

/* Table of constant values */

static doublecomplex c_b1 = {1.,0.};
static doublecomplex c_b2 = {0.,0.};
static integer c__1 = 1;

/*<       SUBROUTINE ZLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK ) >*/
/* Subroutine */ int zlarf_(char *side, integer *m, integer *n, doublecomplex
        *v, integer *incv, doublecomplex *tau, doublecomplex *c__, integer *
        ldc, doublecomplex *work, ftnlen side_len)
{
    /* System generated locals */
    integer c_dim1, c_offset;
    doublecomplex z__1;

    /* Local variables */
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int zgerc_(integer *, integer *, doublecomplex *,
            doublecomplex *, integer *, doublecomplex *, integer *,
            doublecomplex *, integer *), zgemv_(char *, integer *, integer *,
            doublecomplex *, doublecomplex *, integer *, doublecomplex *,
            integer *, doublecomplex *, doublecomplex *, integer *, ftnlen);
    (void)side_len;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          SIDE >*/
/*<       INTEGER            INCV, LDC, M, N >*/
/*<       COMPLEX*16         TAU >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         C( LDC, * ), V( * ), WORK( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLARF applies a complex elementary reflector H to a complex M-by-N */
/*  matrix C, from either the left or the right. H is represented in the */
/*  form */

/*        H = I - tau * v * v' */

/*  where tau is a complex scalar and v is a complex vector. */

/*  If tau = 0, then H is taken to be the unit matrix. */

/*  To apply H' (the conjugate transpose of H), supply conjg(tau) instead */
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
/*          On exit, C is overwritten by the matrix H * C if SIDE = 'L', */
/*          or C * H if SIDE = 'R'. */

/*  LDC     (input) INTEGER */
/*          The leading dimension of the array C. LDC >= max(1,M). */

/*  WORK    (workspace) COMPLEX*16 array, dimension */
/*                         (N) if SIDE = 'L' */
/*                      or (M) if SIDE = 'R' */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       COMPLEX*16         ONE, ZERO >*/
/*<    >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           ZGEMV, ZGERC >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       IF( LSAME( SIDE, 'L' ) ) THEN >*/
    /* Parameter adjustments */
    --v;
    c_dim1 = *ldc;
    c_offset = 1 + c_dim1;
    c__ -= c_offset;
    --work;

    /* Function Body */
    if (lsame_(side, "L", (ftnlen)1, (ftnlen)1)) {

/*        Form  H * C */

/*<          IF( TAU.NE.ZERO ) THEN >*/
        if (tau->r != 0. || tau->i != 0.) {

/*           w := C' * v */

/*<    >*/
            zgemv_("Conjugate transpose", m, n, &c_b1, &c__[c_offset], ldc, &
                    v[1], incv, &c_b2, &work[1], &c__1, (ftnlen)19);

/*           C := C - v * w' */

/*<             CALL ZGERC( M, N, -TAU, V, INCV, WORK, 1, C, LDC ) >*/
            z__1.r = -tau->r, z__1.i = -tau->i;
            zgerc_(m, n, &z__1, &v[1], incv, &work[1], &c__1, &c__[c_offset],
                    ldc);
/*<          END IF >*/
        }
/*<       ELSE >*/
    } else {

/*        Form  C * H */

/*<          IF( TAU.NE.ZERO ) THEN >*/
        if (tau->r != 0. || tau->i != 0.) {

/*           w := C * v */

/*<    >*/
            zgemv_("No transpose", m, n, &c_b1, &c__[c_offset], ldc, &v[1],
                    incv, &c_b2, &work[1], &c__1, (ftnlen)12);

/*           C := C - w * v' */

/*<             CALL ZGERC( M, N, -TAU, WORK, 1, V, INCV, C, LDC ) >*/
            z__1.r = -tau->r, z__1.i = -tau->i;
            zgerc_(m, n, &z__1, &work[1], &c__1, &v[1], incv, &c__[c_offset],
                    ldc);
/*<          END IF >*/
        }
/*<       END IF >*/
    }
/*<       RETURN >*/
    return 0;

/*     End of ZLARF */

/*<       END >*/
} /* zlarf_ */

#ifdef __cplusplus
        }
#endif
