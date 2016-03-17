/* lapack/single/slaset.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE SLASET( UPLO, M, N, ALPHA, BETA, A, LDA ) >*/
/* Subroutine */ int slaset_(char *uplo, integer *m, integer *n, real *alpha,
        real *beta, real *a, integer *lda, ftnlen uplo_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;

    /* Local variables */
    integer i__, j;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    (void)uplo_len;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          UPLO >*/
/*<       INTEGER            LDA, M, N >*/
/*<       REAL               ALPHA, BETA >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       REAL               A( LDA, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  SLASET initializes an m-by-n matrix A to BETA on the diagonal and */
/*  ALPHA on the offdiagonals. */

/*  Arguments */
/*  ========= */

/*  UPLO    (input) CHARACTER*1 */
/*          Specifies the part of the matrix A to be set. */
/*          = 'U':      Upper triangular part is set; the strictly lower */
/*                      triangular part of A is not changed. */
/*          = 'L':      Lower triangular part is set; the strictly upper */
/*                      triangular part of A is not changed. */
/*          Otherwise:  All of the matrix A is set. */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix A.  M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix A.  N >= 0. */

/*  ALPHA   (input) REAL */
/*          The constant to which the offdiagonal elements are to be set. */

/*  BETA    (input) REAL */
/*          The constant to which the diagonal elements are to be set. */

/*  A       (input/output) REAL array, dimension (LDA,N) */
/*          On exit, the leading m-by-n submatrix of A is set as follows: */

/*          if UPLO = 'U', A(i,j) = ALPHA, 1<=i<=j-1, 1<=j<=n, */
/*          if UPLO = 'L', A(i,j) = ALPHA, j+1<=i<=m, 1<=j<=n, */
/*          otherwise,     A(i,j) = ALPHA, 1<=i<=m, 1<=j<=n, i.ne.j, */

/*          and, for all UPLO, A(i,i) = BETA, 1<=i<=min(m,n). */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,M). */

/* ===================================================================== */

/*     .. Local Scalars .. */
/*<       INTEGER            I, J >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MIN >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       IF( LSAME( UPLO, 'U' ) ) THEN >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    if (lsame_(uplo, "U", (ftnlen)1, (ftnlen)1)) {

/*        Set the strictly upper triangular or trapezoidal part of the */
/*        array to ALPHA. */

/*<          DO 20 J = 2, N >*/
        i__1 = *n;
        for (j = 2; j <= i__1; ++j) {
/*<             DO 10 I = 1, MIN( J-1, M ) >*/
/* Computing MIN */
            i__3 = j - 1;
            i__2 = min(i__3,*m);
            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                A( I, J ) = ALPHA >*/
                a[i__ + j * a_dim1] = *alpha;
/*<    10       CONTINUE >*/
/* L10: */
            }
/*<    20    CONTINUE >*/
/* L20: */
        }

/*<       ELSE IF( LSAME( UPLO, 'L' ) ) THEN >*/
    } else if (lsame_(uplo, "L", (ftnlen)1, (ftnlen)1)) {

/*        Set the strictly lower triangular or trapezoidal part of the */
/*        array to ALPHA. */

/*<          DO 40 J = 1, MIN( M, N ) >*/
        i__1 = min(*m,*n);
        for (j = 1; j <= i__1; ++j) {
/*<             DO 30 I = J + 1, M >*/
            i__2 = *m;
            for (i__ = j + 1; i__ <= i__2; ++i__) {
/*<                A( I, J ) = ALPHA >*/
                a[i__ + j * a_dim1] = *alpha;
/*<    30       CONTINUE >*/
/* L30: */
            }
/*<    40    CONTINUE >*/
/* L40: */
        }

/*<       ELSE >*/
    } else {

/*        Set the leading m-by-n submatrix to ALPHA. */

/*<          DO 60 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 50 I = 1, M >*/
            i__2 = *m;
            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                A( I, J ) = ALPHA >*/
                a[i__ + j * a_dim1] = *alpha;
/*<    50       CONTINUE >*/
/* L50: */
            }
/*<    60    CONTINUE >*/
/* L60: */
        }
/*<       END IF >*/
    }

/*     Set the first min(M,N) diagonal elements to BETA. */

/*<       DO 70 I = 1, MIN( M, N ) >*/
    i__1 = min(*m,*n);
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          A( I, I ) = BETA >*/
        a[i__ + i__ * a_dim1] = *beta;
/*<    70 CONTINUE >*/
/* L70: */
    }

/*<       RETURN >*/
    return 0;

/*     End of SLASET */

/*<       END >*/
} /* slaset_ */

#ifdef __cplusplus
        }
#endif
