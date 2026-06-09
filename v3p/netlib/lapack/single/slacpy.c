/* lapack/single/slacpy.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE SLACPY( UPLO, M, N, A, LDA, B, LDB ) >*/
/* Subroutine */ int slacpy_(char *uplo, integer *m, integer *n, real *a,
        integer *lda, real *b, integer *ldb, ftnlen uplo_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2;

    /* Local variables */
    integer i__, j;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    (void)uplo_len;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          UPLO >*/
/*<       INTEGER            LDA, LDB, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       REAL               A( LDA, * ), B( LDB, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  SLACPY copies all or part of a two-dimensional matrix A to another */
/*  matrix B. */

/*  Arguments */
/*  ========= */

/*  UPLO    (input) CHARACTER*1 */
/*          Specifies the part of the matrix A to be copied to B. */
/*          = 'U':      Upper triangular part */
/*          = 'L':      Lower triangular part */
/*          Otherwise:  All of the matrix A */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix A.  M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix A.  N >= 0. */

/*  A       (input) REAL array, dimension (LDA,N) */
/*          The m by n matrix A.  If UPLO = 'U', only the upper triangle */
/*          or trapezoid is accessed; if UPLO = 'L', only the lower */
/*          triangle or trapezoid is accessed. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,M). */

/*  B       (output) REAL array, dimension (LDB,N) */
/*          On exit, B = A in the locations specified by UPLO. */

/*  LDB     (input) INTEGER */
/*          The leading dimension of the array B.  LDB >= max(1,M). */

/*  ===================================================================== */

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
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1;
    b -= b_offset;

    /* Function Body */
    if (lsame_(uplo, "U", (ftnlen)1, (ftnlen)1)) {
/*<          DO 20 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 10 I = 1, MIN( J, M ) >*/
            i__2 = min(j,*m);
            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                B( I, J ) = A( I, J ) >*/
                b[i__ + j * b_dim1] = a[i__ + j * a_dim1];
/*<    10       CONTINUE >*/
/* L10: */
            }
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<       ELSE IF( LSAME( UPLO, 'L' ) ) THEN >*/
    } else if (lsame_(uplo, "L", (ftnlen)1, (ftnlen)1)) {
/*<          DO 40 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 30 I = J, M >*/
            i__2 = *m;
            for (i__ = j; i__ <= i__2; ++i__) {
/*<                B( I, J ) = A( I, J ) >*/
                b[i__ + j * b_dim1] = a[i__ + j * a_dim1];
/*<    30       CONTINUE >*/
/* L30: */
            }
/*<    40    CONTINUE >*/
/* L40: */
        }
/*<       ELSE >*/
    } else {
/*<          DO 60 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 50 I = 1, M >*/
            i__2 = *m;
            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                B( I, J ) = A( I, J ) >*/
                b[i__ + j * b_dim1] = a[i__ + j * a_dim1];
/*<    50       CONTINUE >*/
/* L50: */
            }
/*<    60    CONTINUE >*/
/* L60: */
        }
/*<       END IF >*/
    }
/*<       RETURN >*/
    return 0;

/*     End of SLACPY */

/*<       END >*/
} /* slacpy_ */

#ifdef __cplusplus
        }
#endif
