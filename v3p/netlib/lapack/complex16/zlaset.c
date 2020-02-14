/* lapack/complex16/zlaset.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE ZLASET( UPLO, M, N, ALPHA, BETA, A, LDA ) >*/
/* Subroutine */ int zlaset_(char *uplo, integer *m, integer *n,
        doublecomplex *alpha, doublecomplex *beta, doublecomplex *a, integer *
        lda, ftnlen uplo_len)
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
/*<       COMPLEX*16         ALPHA, BETA >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         A( LDA, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLASET initializes a 2-D array A to BETA on the diagonal and */
/*  ALPHA on the offdiagonals. */

/*  Arguments */
/*  ========= */

/*  UPLO    (input) CHARACTER*1 */
/*          Specifies the part of the matrix A to be set. */
/*          = 'U':      Upper triangular part is set. The lower triangle */
/*                      is unchanged. */
/*          = 'L':      Lower triangular part is set. The upper triangle */
/*                      is unchanged. */
/*          Otherwise:  All of the matrix A is set. */

/*  M       (input) INTEGER */
/*          On entry, M specifies the number of rows of A. */

/*  N       (input) INTEGER */
/*          On entry, N specifies the number of columns of A. */

/*  ALPHA   (input) COMPLEX*16 */
/*          All the offdiagonal array elements are set to ALPHA. */

/*  BETA    (input) COMPLEX*16 */
/*          All the diagonal array elements are set to BETA. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N) */
/*          On entry, the m by n matrix A. */
/*          On exit, A(i,j) = ALPHA, 1 <= i <= m, 1 <= j <= n, i.ne.j; */
/*                   A(i,i) = BETA , 1 <= i <= min(m,n) */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,M). */

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

    /* Function Body */
    if (lsame_(uplo, "U", (ftnlen)1, (ftnlen)1)) {

/*        Set the diagonal to BETA and the strictly upper triangular */
/*        part of the array to ALPHA. */

/*<          DO 20 J = 2, N >*/
        i__1 = *n;
        for (j = 2; j <= i__1; ++j) {
/*<             DO 10 I = 1, MIN( J-1, M ) >*/
/* Computing MIN */
            i__3 = j - 1;
            i__2 = min(i__3,*m);
            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                A( I, J ) = ALPHA >*/
                i__3 = i__ + j * a_dim1;
                a[i__3].r = alpha->r, a[i__3].i = alpha->i;
/*<    10       CONTINUE >*/
/* L10: */
            }
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<          DO 30 I = 1, MIN( N, M ) >*/
        i__1 = min(*n,*m);
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             A( I, I ) = BETA >*/
            i__2 = i__ + i__ * a_dim1;
            a[i__2].r = beta->r, a[i__2].i = beta->i;
/*<    30    CONTINUE >*/
/* L30: */
        }

/*<       ELSE IF( LSAME( UPLO, 'L' ) ) THEN >*/
    } else if (lsame_(uplo, "L", (ftnlen)1, (ftnlen)1)) {

/*        Set the diagonal to BETA and the strictly lower triangular */
/*        part of the array to ALPHA. */

/*<          DO 50 J = 1, MIN( M, N ) >*/
        i__1 = min(*m,*n);
        for (j = 1; j <= i__1; ++j) {
/*<             DO 40 I = J + 1, M >*/
            i__2 = *m;
            for (i__ = j + 1; i__ <= i__2; ++i__) {
/*<                A( I, J ) = ALPHA >*/
                i__3 = i__ + j * a_dim1;
                a[i__3].r = alpha->r, a[i__3].i = alpha->i;
/*<    40       CONTINUE >*/
/* L40: */
            }
/*<    50    CONTINUE >*/
/* L50: */
        }
/*<          DO 60 I = 1, MIN( N, M ) >*/
        i__1 = min(*n,*m);
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             A( I, I ) = BETA >*/
            i__2 = i__ + i__ * a_dim1;
            a[i__2].r = beta->r, a[i__2].i = beta->i;
/*<    60    CONTINUE >*/
/* L60: */
        }

/*<       ELSE >*/
    } else {

/*        Set the array to BETA on the diagonal and ALPHA on the */
/*        offdiagonal. */

/*<          DO 80 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 70 I = 1, M >*/
            i__2 = *m;
            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                A( I, J ) = ALPHA >*/
                i__3 = i__ + j * a_dim1;
                a[i__3].r = alpha->r, a[i__3].i = alpha->i;
/*<    70       CONTINUE >*/
/* L70: */
            }
/*<    80    CONTINUE >*/
/* L80: */
        }
/*<          DO 90 I = 1, MIN( M, N ) >*/
        i__1 = min(*m,*n);
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             A( I, I ) = BETA >*/
            i__2 = i__ + i__ * a_dim1;
            a[i__2].r = beta->r, a[i__2].i = beta->i;
/*<    90    CONTINUE >*/
/* L90: */
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of ZLASET */

/*<       END >*/
} /* zlaset_ */

#ifdef __cplusplus
        }
#endif
