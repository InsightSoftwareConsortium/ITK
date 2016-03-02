/* lapack/single/sgerq2.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE SGERQ2( M, N, A, LDA, TAU, WORK, INFO ) >*/
/* Subroutine */ int sgerq2_(integer *m, integer *n, real *a, integer *lda,
        real *tau, real *work, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    integer i__, k;
    real aii;
    extern /* Subroutine */ int slarf_(char *, integer *, integer *, real *,
            integer *, real *, real *, integer *, real *, ftnlen), xerbla_(
            char *, integer *, ftnlen), slarfg_(integer *, real *, real *,
            integer *, real *);


/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INFO, LDA, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       REAL               A( LDA, * ), TAU( * ), WORK( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  SGERQ2 computes an RQ factorization of a real m by n matrix A: */
/*  A = R * Q. */

/*  Arguments */
/*  ========= */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix A.  M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix A.  N >= 0. */

/*  A       (input/output) REAL array, dimension (LDA,N) */
/*          On entry, the m by n matrix A. */
/*          On exit, if m <= n, the upper triangle of the subarray */
/*          A(1:m,n-m+1:n) contains the m by m upper triangular matrix R; */
/*          if m >= n, the elements on and above the (m-n)-th subdiagonal */
/*          contain the m by n upper trapezoidal matrix R; the remaining */
/*          elements, with the array TAU, represent the orthogonal matrix */
/*          Q as a product of elementary reflectors (see Further */
/*          Details). */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,M). */

/*  TAU     (output) REAL array, dimension (min(M,N)) */
/*          The scalar factors of the elementary reflectors (see Further */
/*          Details). */

/*  WORK    (workspace) REAL array, dimension (M) */

/*  INFO    (output) INTEGER */
/*          = 0: successful exit */
/*          < 0: if INFO = -i, the i-th argument had an illegal value */

/*  Further Details */
/*  =============== */

/*  The matrix Q is represented as a product of elementary reflectors */

/*     Q = H(1) H(2) . . . H(k), where k = min(m,n). */

/*  Each H(i) has the form */

/*     H(i) = I - tau * v * v' */

/*  where tau is a real scalar, and v is a real vector with */
/*  v(n-k+i+1:n) = 0 and v(n-k+i) = 1; v(1:n-k+i-1) is stored on exit in */
/*  A(m-k+i,1:n-k+i-1), and tau in TAU(i). */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       REAL               ONE >*/
/*<       PARAMETER          ( ONE = 1.0E+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            I, K >*/
/*<       REAL               AII >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           SLARF, SLARFG, XERBLA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MAX, MIN >*/
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
/*<          CALL XERBLA( 'SGERQ2', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("SGERQ2", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       K = MIN( M, N ) >*/
    k = min(*m,*n);

/*<       DO 10 I = K, 1, -1 >*/
    for (i__ = k; i__ >= 1; --i__) {

/*        Generate elementary reflector H(i) to annihilate */
/*        A(m-k+i,1:n-k+i-1) */

/*<    >*/
        i__1 = *n - k + i__;
        slarfg_(&i__1, &a[*m - k + i__ + (*n - k + i__) * a_dim1], &a[*m - k
                + i__ + a_dim1], lda, &tau[i__]);

/*        Apply H(i) to A(1:m-k+i-1,1:n-k+i) from the right */

/*<          AII = A( M-K+I, N-K+I ) >*/
        aii = a[*m - k + i__ + (*n - k + i__) * a_dim1];
/*<          A( M-K+I, N-K+I ) = ONE >*/
        a[*m - k + i__ + (*n - k + i__) * a_dim1] = (float)1.;
/*<    >*/
        i__1 = *m - k + i__ - 1;
        i__2 = *n - k + i__;
        slarf_("Right", &i__1, &i__2, &a[*m - k + i__ + a_dim1], lda, &tau[
                i__], &a[a_offset], lda, &work[1], (ftnlen)5);
/*<          A( M-K+I, N-K+I ) = AII >*/
        a[*m - k + i__ + (*n - k + i__) * a_dim1] = aii;
/*<    10 CONTINUE >*/
/* L10: */
    }
/*<       RETURN >*/
    return 0;

/*     End of SGERQ2 */

/*<       END >*/
} /* sgerq2_ */

#ifdef __cplusplus
        }
#endif
