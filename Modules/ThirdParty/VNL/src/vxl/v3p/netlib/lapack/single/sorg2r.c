/* lapack/single/sorg2r.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE SORG2R( M, N, K, A, LDA, TAU, WORK, INFO ) >*/
/* Subroutine */ int sorg2r_(integer *m, integer *n, integer *k, real *a,
        integer *lda, real *tau, real *work, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    real r__1;

    /* Local variables */
    integer i__, j, l;
    extern /* Subroutine */ int sscal_(integer *, real *, real *, integer *),
            slarf_(char *, integer *, integer *, real *, integer *, real *,
            real *, integer *, real *, ftnlen), xerbla_(char *, integer *,
            ftnlen);


/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INFO, K, LDA, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       REAL               A( LDA, * ), TAU( * ), WORK( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  SORG2R generates an m by n real matrix Q with orthonormal columns, */
/*  which is defined as the first n columns of a product of k elementary */
/*  reflectors of order m */

/*        Q  =  H(1) H(2) . . . H(k) */

/*  as returned by SGEQRF. */

/*  Arguments */
/*  ========= */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix Q. M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix Q. M >= N >= 0. */

/*  K       (input) INTEGER */
/*          The number of elementary reflectors whose product defines the */
/*          matrix Q. N >= K >= 0. */

/*  A       (input/output) REAL array, dimension (LDA,N) */
/*          On entry, the i-th column must contain the vector which */
/*          defines the elementary reflector H(i), for i = 1,2,...,k, as */
/*          returned by SGEQRF in the first k columns of its array */
/*          argument A. */
/*          On exit, the m-by-n matrix Q. */

/*  LDA     (input) INTEGER */
/*          The first dimension of the array A. LDA >= max(1,M). */

/*  TAU     (input) REAL array, dimension (K) */
/*          TAU(i) must contain the scalar factor of the elementary */
/*          reflector H(i), as returned by SGEQRF. */

/*  WORK    (workspace) REAL array, dimension (N) */

/*  INFO    (output) INTEGER */
/*          = 0: successful exit */
/*          < 0: if INFO = -i, the i-th argument has an illegal value */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       REAL               ONE, ZERO >*/
/*<       PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            I, J, L >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           SLARF, SSCAL, XERBLA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MAX >*/
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
/*<       ELSE IF( N.LT.0 .OR. N.GT.M ) THEN >*/
    } else if (*n < 0 || *n > *m) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( K.LT.0 .OR. K.GT.N ) THEN >*/
    } else if (*k < 0 || *k > *n) {
/*<          INFO = -3 >*/
        *info = -3;
/*<       ELSE IF( LDA.LT.MAX( 1, M ) ) THEN >*/
    } else if (*lda < max(1,*m)) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'SORG2R', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("SORG2R", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<    >*/
    if (*n <= 0) {
        return 0;
    }

/*     Initialise columns k+1:n to columns of the unit matrix */

/*<       DO 20 J = K + 1, N >*/
    i__1 = *n;
    for (j = *k + 1; j <= i__1; ++j) {
/*<          DO 10 L = 1, M >*/
        i__2 = *m;
        for (l = 1; l <= i__2; ++l) {
/*<             A( L, J ) = ZERO >*/
            a[l + j * a_dim1] = (float)0.;
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<          A( J, J ) = ONE >*/
        a[j + j * a_dim1] = (float)1.;
/*<    20 CONTINUE >*/
/* L20: */
    }

/*<       DO 40 I = K, 1, -1 >*/
    for (i__ = *k; i__ >= 1; --i__) {

/*        Apply H(i) to A(i:m,i:n) from the left */

/*<          IF( I.LT.N ) THEN >*/
        if (i__ < *n) {
/*<             A( I, I ) = ONE >*/
            a[i__ + i__ * a_dim1] = (float)1.;
/*<    >*/
            i__1 = *m - i__ + 1;
            i__2 = *n - i__;
            slarf_("Left", &i__1, &i__2, &a[i__ + i__ * a_dim1], &c__1, &tau[
                    i__], &a[i__ + (i__ + 1) * a_dim1], lda, &work[1], (
                    ftnlen)4);
/*<          END IF >*/
        }
/*<    >*/
        if (i__ < *m) {
            i__1 = *m - i__;
            r__1 = -tau[i__];
            sscal_(&i__1, &r__1, &a[i__ + 1 + i__ * a_dim1], &c__1);
        }
/*<          A( I, I ) = ONE - TAU( I ) >*/
        a[i__ + i__ * a_dim1] = (float)1. - tau[i__];

/*        Set A(1:i-1,i) to zero */

/*<          DO 30 L = 1, I - 1 >*/
        i__1 = i__ - 1;
        for (l = 1; l <= i__1; ++l) {
/*<             A( L, I ) = ZERO >*/
            a[l + i__ * a_dim1] = (float)0.;
/*<    30    CONTINUE >*/
/* L30: */
        }
/*<    40 CONTINUE >*/
/* L40: */
    }
/*<       RETURN >*/
    return 0;

/*     End of SORG2R */

/*<       END >*/
} /* sorg2r_ */

#ifdef __cplusplus
        }
#endif
