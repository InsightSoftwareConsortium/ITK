/* lapack/double/dorgr2.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE DORGR2( M, N, K, A, LDA, TAU, WORK, INFO ) >*/
/* Subroutine */ int dorgr2_(integer *m, integer *n, integer *k, doublereal *
        a, integer *lda, doublereal *tau, doublereal *work, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Local variables */
    integer i__, j, l, ii;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *), dlarf_(char *, integer *, integer *, doublereal *,
            integer *, doublereal *, doublereal *, integer *, doublereal *,
            ftnlen), xerbla_(char *, integer *, ftnlen);


/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INFO, K, LDA, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DORGR2 generates an m by n real matrix Q with orthonormal rows, */
/*  which is defined as the last m rows of a product of k elementary */
/*  reflectors of order n */

/*        Q  =  H(1) H(2) . . . H(k) */

/*  as returned by DGERQF. */

/*  Arguments */
/*  ========= */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix Q. M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix Q. N >= M. */

/*  K       (input) INTEGER */
/*          The number of elementary reflectors whose product defines the */
/*          matrix Q. M >= K >= 0. */

/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) */
/*          On entry, the (m-k+i)-th row must contain the vector which */
/*          defines the elementary reflector H(i), for i = 1,2,...,k, as */
/*          returned by DGERQF in the last k rows of its array argument */
/*          A. */
/*          On exit, the m by n matrix Q. */

/*  LDA     (input) INTEGER */
/*          The first dimension of the array A. LDA >= max(1,M). */

/*  TAU     (input) DOUBLE PRECISION array, dimension (K) */
/*          TAU(i) must contain the scalar factor of the elementary */
/*          reflector H(i), as returned by DGERQF. */

/*  WORK    (workspace) DOUBLE PRECISION array, dimension (M) */

/*  INFO    (output) INTEGER */
/*          = 0: successful exit */
/*          < 0: if INFO = -i, the i-th argument has an illegal value */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ONE, ZERO >*/
/*<       PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            I, II, J, L >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DLARF, DSCAL, XERBLA >*/
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
/*<       ELSE IF( N.LT.M ) THEN >*/
    } else if (*n < *m) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( K.LT.0 .OR. K.GT.M ) THEN >*/
    } else if (*k < 0 || *k > *m) {
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
/*<          CALL XERBLA( 'DORGR2', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("DORGR2", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<    >*/
    if (*m <= 0) {
        return 0;
    }

/*<       IF( K.LT.M ) THEN >*/
    if (*k < *m) {

/*        Initialise rows 1:m-k to rows of the unit matrix */

/*<          DO 20 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 10 L = 1, M - K >*/
            i__2 = *m - *k;
            for (l = 1; l <= i__2; ++l) {
/*<                A( L, J ) = ZERO >*/
                a[l + j * a_dim1] = 0.;
/*<    10       CONTINUE >*/
/* L10: */
            }
/*<    >*/
            if (j > *n - *m && j <= *n - *k) {
                a[*m - *n + j + j * a_dim1] = 1.;
            }
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<       END IF >*/
    }

/*<       DO 40 I = 1, K >*/
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          II = M - K + I >*/
        ii = *m - *k + i__;

/*        Apply H(i) to A(1:m-k+i,1:n-k+i) from the right */

/*<          A( II, N-M+II ) = ONE >*/
        a[ii + (*n - *m + ii) * a_dim1] = 1.;
/*<    >*/
        i__2 = ii - 1;
        i__3 = *n - *m + ii;
        dlarf_("Right", &i__2, &i__3, &a[ii + a_dim1], lda, &tau[i__], &a[
                a_offset], lda, &work[1], (ftnlen)5);
/*<          CALL DSCAL( N-M+II-1, -TAU( I ), A( II, 1 ), LDA ) >*/
        i__2 = *n - *m + ii - 1;
        d__1 = -tau[i__];
        dscal_(&i__2, &d__1, &a[ii + a_dim1], lda);
/*<          A( II, N-M+II ) = ONE - TAU( I ) >*/
        a[ii + (*n - *m + ii) * a_dim1] = 1. - tau[i__];

/*        Set A(m-k+i,n-k+i+1:n) to zero */

/*<          DO 30 L = N - M + II + 1, N >*/
        i__2 = *n;
        for (l = *n - *m + ii + 1; l <= i__2; ++l) {
/*<             A( II, L ) = ZERO >*/
            a[ii + l * a_dim1] = 0.;
/*<    30    CONTINUE >*/
/* L30: */
        }
/*<    40 CONTINUE >*/
/* L40: */
    }
/*<       RETURN >*/
    return 0;

/*     End of DORGR2 */

/*<       END >*/
} /* dorgr2_ */

#ifdef __cplusplus
        }
#endif
