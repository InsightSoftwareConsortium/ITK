/* lapack/complex16/zgeqr2.f -- translated by f2c (version 20090411).
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

static integer c__1 = 1;

/*<       SUBROUTINE ZGEQR2( M, N, A, LDA, TAU, WORK, INFO ) >*/
/* Subroutine */ int zgeqr2_(integer *m, integer *n, doublecomplex *a,
        integer *lda, doublecomplex *tau, doublecomplex *work, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublecomplex z__1;

    /* Builtin functions */
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    integer i__, k;
    doublecomplex alpha;
    extern /* Subroutine */ int zlarf_(char *, integer *, integer *,
            doublecomplex *, integer *, doublecomplex *, doublecomplex *,
            integer *, doublecomplex *, ftnlen), xerbla_(char *, integer *,
            ftnlen), zlarfg_(integer *, doublecomplex *, doublecomplex *,
            integer *, doublecomplex *);


/*  -- LAPACK routine (version 3.2.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     June 2010 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INFO, LDA, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZGEQR2 computes a QR factorization of a complex m by n matrix A: */
/*  A = Q * R. */

/*  Arguments */
/*  ========= */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix A.  M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix A.  N >= 0. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N) */
/*          On entry, the m by n matrix A. */
/*          On exit, the elements on and above the diagonal of the array */
/*          contain the min(m,n) by n upper trapezoidal matrix R (R is */
/*          upper triangular if m >= n); the elements below the diagonal, */
/*          with the array TAU, represent the unitary matrix Q as a */
/*          product of elementary reflectors (see Further Details). */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,M). */

/*  TAU     (output) COMPLEX*16 array, dimension (min(M,N)) */
/*          The scalar factors of the elementary reflectors (see Further */
/*          Details). */

/*  WORK    (workspace) COMPLEX*16 array, dimension (N) */

/*  INFO    (output) INTEGER */
/*          = 0: successful exit */
/*          < 0: if INFO = -i, the i-th argument had an illegal value */

/*  Further Details */
/*  =============== */

/*  The matrix Q is represented as a product of elementary reflectors */

/*     Q = H(1) H(2) . . . H(k), where k = min(m,n). */

/*  Each H(i) has the form */

/*     H(i) = I - tau * v * v' */

/*  where tau is a complex scalar, and v is a complex vector with */
/*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i), */
/*  and tau in TAU(i). */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       COMPLEX*16         ONE >*/
/*<       PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            I, K >*/
/*<       COMPLEX*16         ALPHA >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA, ZLARF, ZLARFG >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          DCONJG, MAX, MIN >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input arguments */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --tau;
    --work;

    /* Function Body */
    *info = 0;
/*<       IF( M.LT.0 ) THEN >*/
    if (*m < 0) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( LDA.LT.MAX( 1, M ) ) THEN >*/
    } else if (*lda < max(1,*m)) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZGEQR2', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZGEQR2", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       K = MIN( M, N ) >*/
    k = min(*m,*n);

/*<       DO 10 I = 1, K >*/
    i__1 = k;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Generate elementary reflector H(i) to annihilate A(i+1:m,i) */

/*<    >*/
        i__2 = *m - i__ + 1;
/* Computing MIN */
        i__3 = i__ + 1;
        zlarfg_(&i__2, &a[i__ + i__ * a_dim1], &a[min(i__3,*m) + i__ * a_dim1]
                , &c__1, &tau[i__]);
/*<          IF( I.LT.N ) THEN >*/
        if (i__ < *n) {

/*           Apply H(i)' to A(i:m,i+1:n) from the left */

/*<             ALPHA = A( I, I ) >*/
            i__2 = i__ + i__ * a_dim1;
            alpha.r = a[i__2].r, alpha.i = a[i__2].i;
/*<             A( I, I ) = ONE >*/
            i__2 = i__ + i__ * a_dim1;
            a[i__2].r = 1., a[i__2].i = 0.;
/*<    >*/
            i__2 = *m - i__ + 1;
            i__3 = *n - i__;
            d_cnjg(&z__1, &tau[i__]);
            zlarf_("Left", &i__2, &i__3, &a[i__ + i__ * a_dim1], &c__1, &z__1,
                     &a[i__ + (i__ + 1) * a_dim1], lda, &work[1], (ftnlen)4);
/*<             A( I, I ) = ALPHA >*/
            i__2 = i__ + i__ * a_dim1;
            a[i__2].r = alpha.r, a[i__2].i = alpha.i;
/*<          END IF >*/
        }
/*<    10 CONTINUE >*/
/* L10: */
    }
/*<       RETURN >*/
    return 0;

/*     End of ZGEQR2 */

/*<       END >*/
} /* zgeqr2_ */

#ifdef __cplusplus
        }
#endif
