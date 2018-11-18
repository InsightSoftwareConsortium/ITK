/* blas/ztrmm.f -- translated by f2c (version 20050501).
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

/*<    >*/
/* Subroutine */ int ztrmm_(char *side, char *uplo, char *transa, char *diag,
        integer *m, integer *n, doublecomplex *alpha, doublecomplex *a,
        integer *lda, doublecomplex *b, integer *ldb, ftnlen side_len, ftnlen
        uplo_len, ftnlen transa_len, ftnlen diag_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2, i__3, i__4, i__5,
            i__6;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    integer i__, j, k, info;
    doublecomplex temp;
    logical lside;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    integer nrowa;
    logical upper;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    logical noconj, nounit;
    (void)side_len;
    (void)uplo_len;
    (void)transa_len;
    (void)diag_len;

/*     .. Scalar Arguments .. */
/*<       CHARACTER*1        SIDE, UPLO, TRANSA, DIAG >*/
/*<       INTEGER            M, N, LDA, LDB >*/
/*<       COMPLEX*16         ALPHA >*/
/*     .. Array Arguments .. */
/*<       COMPLEX*16         A( LDA, * ), B( LDB, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZTRMM  performs one of the matrix-matrix operations */

/*     B := alpha*op( A )*B,   or   B := alpha*B*op( A ) */

/*  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or */
/*  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of */

/*     op( A ) = A   or   op( A ) = A'   or   op( A ) = conjg( A' ). */

/*  Parameters */
/*  ========== */

/*  SIDE   - CHARACTER*1. */
/*           On entry,  SIDE specifies whether  op( A ) multiplies B from */
/*           the left or right as follows: */

/*              SIDE = 'L' or 'l'   B := alpha*op( A )*B. */

/*              SIDE = 'R' or 'r'   B := alpha*B*op( A ). */

/*           Unchanged on exit. */

/*  UPLO   - CHARACTER*1. */
/*           On entry, UPLO specifies whether the matrix A is an upper or */
/*           lower triangular matrix as follows: */

/*              UPLO = 'U' or 'u'   A is an upper triangular matrix. */

/*              UPLO = 'L' or 'l'   A is a lower triangular matrix. */

/*           Unchanged on exit. */

/*  TRANSA - CHARACTER*1. */
/*           On entry, TRANSA specifies the form of op( A ) to be used in */
/*           the matrix multiplication as follows: */

/*              TRANSA = 'N' or 'n'   op( A ) = A. */

/*              TRANSA = 'T' or 't'   op( A ) = A'. */

/*              TRANSA = 'C' or 'c'   op( A ) = conjg( A' ). */

/*           Unchanged on exit. */

/*  DIAG   - CHARACTER*1. */
/*           On entry, DIAG specifies whether or not A is unit triangular */
/*           as follows: */

/*              DIAG = 'U' or 'u'   A is assumed to be unit triangular. */

/*              DIAG = 'N' or 'n'   A is not assumed to be unit */
/*                                  triangular. */

/*           Unchanged on exit. */

/*  M      - INTEGER. */
/*           On entry, M specifies the number of rows of B. M must be at */
/*           least zero. */
/*           Unchanged on exit. */

/*  N      - INTEGER. */
/*           On entry, N specifies the number of columns of B.  N must be */
/*           at least zero. */
/*           Unchanged on exit. */

/*  ALPHA  - COMPLEX*16      . */
/*           On entry,  ALPHA specifies the scalar  alpha. When  alpha is */
/*           zero then  A is not referenced and  B need not be set before */
/*           entry. */
/*           Unchanged on exit. */

/*  A      - COMPLEX*16       array of DIMENSION ( LDA, k ), where k is m */
/*           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'. */
/*           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k */
/*           upper triangular part of the array  A must contain the upper */
/*           triangular matrix  and the strictly lower triangular part of */
/*           A is not referenced. */
/*           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k */
/*           lower triangular part of the array  A must contain the lower */
/*           triangular matrix  and the strictly upper triangular part of */
/*           A is not referenced. */
/*           Note that when  DIAG = 'U' or 'u',  the diagonal elements of */
/*           A  are not referenced either,  but are assumed to be  unity. */
/*           Unchanged on exit. */

/*  LDA    - INTEGER. */
/*           On entry, LDA specifies the first dimension of A as declared */
/*           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then */
/*           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r' */
/*           then LDA must be at least max( 1, n ). */
/*           Unchanged on exit. */

/*  B      - COMPLEX*16       array of DIMENSION ( LDB, n ). */
/*           Before entry,  the leading  m by n part of the array  B must */
/*           contain the matrix  B,  and  on exit  is overwritten  by the */
/*           transformed matrix. */

/*  LDB    - INTEGER. */
/*           On entry, LDB specifies the first dimension of B as declared */
/*           in  the  calling  (sub)  program.   LDB  must  be  at  least */
/*           max( 1, m ). */
/*           Unchanged on exit. */


/*  Level 3 Blas routine. */

/*  -- Written on 8-February-1989. */
/*     Jack Dongarra, Argonne National Laboratory. */
/*     Iain Duff, AERE Harwell. */
/*     Jeremy Du Croz, Numerical Algorithms Group Ltd. */
/*     Sven Hammarling, Numerical Algorithms Group Ltd. */


/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA >*/
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          DCONJG, MAX >*/
/*     .. Local Scalars .. */
/*<       LOGICAL            LSIDE, NOCONJ, NOUNIT, UPPER >*/
/*<       INTEGER            I, INFO, J, K, NROWA >*/
/*<       COMPLEX*16         TEMP >*/
/*     .. Parameters .. */
/*<       COMPLEX*16         ONE >*/
/*<       PARAMETER        ( ONE  = ( 1.0D+0, 0.0D+0 ) ) >*/
/*<       COMPLEX*16         ZERO >*/
/*<       PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) ) >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input parameters. */

/*<       LSIDE  = LSAME( SIDE  , 'L' ) >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1;
    b -= b_offset;

    /* Function Body */
    lside = lsame_(side, "L", (ftnlen)1, (ftnlen)1);
/*<       IF( LSIDE )THEN >*/
    if (lside) {
/*<          NROWA = M >*/
        nrowa = *m;
/*<       ELSE >*/
    } else {
/*<          NROWA = N >*/
        nrowa = *n;
/*<       END IF >*/
    }
/*<       NOCONJ = LSAME( TRANSA, 'T' ) >*/
    noconj = lsame_(transa, "T", (ftnlen)1, (ftnlen)1);
/*<       NOUNIT = LSAME( DIAG  , 'N' ) >*/
    nounit = lsame_(diag, "N", (ftnlen)1, (ftnlen)1);
/*<       UPPER  = LSAME( UPLO  , 'U' ) >*/
    upper = lsame_(uplo, "U", (ftnlen)1, (ftnlen)1);

/*<       INFO   = 0 >*/
    info = 0;
/*<    >*/
    if (! lside && ! lsame_(side, "R", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = 1 >*/
        info = 1;
/*<    >*/
    } else if (! upper && ! lsame_(uplo, "L", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = 2 >*/
        info = 2;
/*<    >*/
    } else if (! lsame_(transa, "N", (ftnlen)1, (ftnlen)1) && ! lsame_(transa,
             "T", (ftnlen)1, (ftnlen)1) && ! lsame_(transa, "C", (ftnlen)1, (
            ftnlen)1)) {
/*<          INFO = 3 >*/
        info = 3;
/*<    >*/
    } else if (! lsame_(diag, "U", (ftnlen)1, (ftnlen)1) && ! lsame_(diag,
            "N", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = 4 >*/
        info = 4;
/*<       ELSE IF( M  .LT.0               )THEN >*/
    } else if (*m < 0) {
/*<          INFO = 5 >*/
        info = 5;
/*<       ELSE IF( N  .LT.0               )THEN >*/
    } else if (*n < 0) {
/*<          INFO = 6 >*/
        info = 6;
/*<       ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN >*/
    } else if (*lda < max(1,nrowa)) {
/*<          INFO = 9 >*/
        info = 9;
/*<       ELSE IF( LDB.LT.MAX( 1, M     ) )THEN >*/
    } else if (*ldb < max(1,*m)) {
/*<          INFO = 11 >*/
        info = 11;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 )THEN >*/
    if (info != 0) {
/*<          CALL XERBLA( 'ZTRMM ', INFO ) >*/
        xerbla_("ZTRMM ", &info, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible. */

/*<    >*/
    if (*n == 0) {
        return 0;
    }

/*     And when  alpha.eq.zero. */

/*<       IF( ALPHA.EQ.ZERO )THEN >*/
    if (alpha->r == 0. && alpha->i == 0.) {
/*<          DO 20, J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 10, I = 1, M >*/
            i__2 = *m;
            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                B( I, J ) = ZERO >*/
                i__3 = i__ + j * b_dim1;
                b[i__3].r = 0., b[i__3].i = 0.;
/*<    10       CONTINUE >*/
/* L10: */
            }
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Start the operations. */

/*<       IF( LSIDE )THEN >*/
    if (lside) {
/*<          IF( LSAME( TRANSA, 'N' ) )THEN >*/
        if (lsame_(transa, "N", (ftnlen)1, (ftnlen)1)) {

/*           Form  B := alpha*A*B. */

/*<             IF( UPPER )THEN >*/
            if (upper) {
/*<                DO 50, J = 1, N >*/
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
/*<                   DO 40, K = 1, M >*/
                    i__2 = *m;
                    for (k = 1; k <= i__2; ++k) {
/*<                      IF( B( K, J ).NE.ZERO )THEN >*/
                        i__3 = k + j * b_dim1;
                        if (b[i__3].r != 0. || b[i__3].i != 0.) {
/*<                         TEMP = ALPHA*B( K, J ) >*/
                            i__3 = k + j * b_dim1;
                            z__1.r = alpha->r * b[i__3].r - alpha->i * b[i__3]
                                    .i, z__1.i = alpha->r * b[i__3].i +
                                    alpha->i * b[i__3].r;
                            temp.r = z__1.r, temp.i = z__1.i;
/*<                         DO 30, I = 1, K - 1 >*/
                            i__3 = k - 1;
                            for (i__ = 1; i__ <= i__3; ++i__) {
/*<                            B( I, J ) = B( I, J ) + TEMP*A( I, K ) >*/
                                i__4 = i__ + j * b_dim1;
                                i__5 = i__ + j * b_dim1;
                                i__6 = i__ + k * a_dim1;
                                z__2.r = temp.r * a[i__6].r - temp.i * a[i__6]
                                        .i, z__2.i = temp.r * a[i__6].i +
                                        temp.i * a[i__6].r;
                                z__1.r = b[i__5].r + z__2.r, z__1.i = b[i__5]
                                        .i + z__2.i;
                                b[i__4].r = z__1.r, b[i__4].i = z__1.i;
/*<    30                   CONTINUE >*/
/* L30: */
                            }
/*<    >*/
                            if (nounit) {
                                i__3 = k + k * a_dim1;
                                z__1.r = temp.r * a[i__3].r - temp.i * a[i__3]
                                        .i, z__1.i = temp.r * a[i__3].i +
                                        temp.i * a[i__3].r;
                                temp.r = z__1.r, temp.i = z__1.i;
                            }
/*<                         B( K, J ) = TEMP >*/
                            i__3 = k + j * b_dim1;
                            b[i__3].r = temp.r, b[i__3].i = temp.i;
/*<                      END IF >*/
                        }
/*<    40             CONTINUE >*/
/* L40: */
                    }
/*<    50          CONTINUE >*/
/* L50: */
                }
/*<             ELSE >*/
            } else {
/*<                DO 80, J = 1, N >*/
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
/*<                   DO 70 K = M, 1, -1 >*/
                    for (k = *m; k >= 1; --k) {
/*<                      IF( B( K, J ).NE.ZERO )THEN >*/
                        i__2 = k + j * b_dim1;
                        if (b[i__2].r != 0. || b[i__2].i != 0.) {
/*<                         TEMP      = ALPHA*B( K, J ) >*/
                            i__2 = k + j * b_dim1;
                            z__1.r = alpha->r * b[i__2].r - alpha->i * b[i__2]
                                    .i, z__1.i = alpha->r * b[i__2].i +
                                    alpha->i * b[i__2].r;
                            temp.r = z__1.r, temp.i = z__1.i;
/*<                         B( K, J ) = TEMP >*/
                            i__2 = k + j * b_dim1;
                            b[i__2].r = temp.r, b[i__2].i = temp.i;
/*<    >*/
                            if (nounit) {
                                i__2 = k + j * b_dim1;
                                i__3 = k + j * b_dim1;
                                i__4 = k + k * a_dim1;
                                z__1.r = b[i__3].r * a[i__4].r - b[i__3].i *
                                        a[i__4].i, z__1.i = b[i__3].r * a[
                                        i__4].i + b[i__3].i * a[i__4].r;
                                b[i__2].r = z__1.r, b[i__2].i = z__1.i;
                            }
/*<                         DO 60, I = K + 1, M >*/
                            i__2 = *m;
                            for (i__ = k + 1; i__ <= i__2; ++i__) {
/*<                            B( I, J ) = B( I, J ) + TEMP*A( I, K ) >*/
                                i__3 = i__ + j * b_dim1;
                                i__4 = i__ + j * b_dim1;
                                i__5 = i__ + k * a_dim1;
                                z__2.r = temp.r * a[i__5].r - temp.i * a[i__5]
                                        .i, z__2.i = temp.r * a[i__5].i +
                                        temp.i * a[i__5].r;
                                z__1.r = b[i__4].r + z__2.r, z__1.i = b[i__4]
                                        .i + z__2.i;
                                b[i__3].r = z__1.r, b[i__3].i = z__1.i;
/*<    60                   CONTINUE >*/
/* L60: */
                            }
/*<                      END IF >*/
                        }
/*<    70             CONTINUE >*/
/* L70: */
                    }
/*<    80          CONTINUE >*/
/* L80: */
                }
/*<             END IF >*/
            }
/*<          ELSE >*/
        } else {

/*           Form  B := alpha*A'*B   or   B := alpha*conjg( A' )*B. */

/*<             IF( UPPER )THEN >*/
            if (upper) {
/*<                DO 120, J = 1, N >*/
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
/*<                   DO 110, I = M, 1, -1 >*/
                    for (i__ = *m; i__ >= 1; --i__) {
/*<                      TEMP = B( I, J ) >*/
                        i__2 = i__ + j * b_dim1;
                        temp.r = b[i__2].r, temp.i = b[i__2].i;
/*<                      IF( NOCONJ )THEN >*/
                        if (noconj) {
/*<    >*/
                            if (nounit) {
                                i__2 = i__ + i__ * a_dim1;
                                z__1.r = temp.r * a[i__2].r - temp.i * a[i__2]
                                        .i, z__1.i = temp.r * a[i__2].i +
                                        temp.i * a[i__2].r;
                                temp.r = z__1.r, temp.i = z__1.i;
                            }
/*<                         DO 90, K = 1, I - 1 >*/
                            i__2 = i__ - 1;
                            for (k = 1; k <= i__2; ++k) {
/*<                            TEMP = TEMP + A( K, I )*B( K, J ) >*/
                                i__3 = k + i__ * a_dim1;
                                i__4 = k + j * b_dim1;
                                z__2.r = a[i__3].r * b[i__4].r - a[i__3].i *
                                        b[i__4].i, z__2.i = a[i__3].r * b[
                                        i__4].i + a[i__3].i * b[i__4].r;
                                z__1.r = temp.r + z__2.r, z__1.i = temp.i +
                                        z__2.i;
                                temp.r = z__1.r, temp.i = z__1.i;
/*<    90                   CONTINUE >*/
/* L90: */
                            }
/*<                      ELSE >*/
                        } else {
/*<    >*/
                            if (nounit) {
                                d_cnjg(&z__2, &a[i__ + i__ * a_dim1]);
                                z__1.r = temp.r * z__2.r - temp.i * z__2.i,
                                        z__1.i = temp.r * z__2.i + temp.i *
                                        z__2.r;
                                temp.r = z__1.r, temp.i = z__1.i;
                            }
/*<                         DO 100, K = 1, I - 1 >*/
                            i__2 = i__ - 1;
                            for (k = 1; k <= i__2; ++k) {
/*<                            TEMP = TEMP + DCONJG( A( K, I ) )*B( K, J ) >*/
                                d_cnjg(&z__3, &a[k + i__ * a_dim1]);
                                i__3 = k + j * b_dim1;
                                z__2.r = z__3.r * b[i__3].r - z__3.i * b[i__3]
                                        .i, z__2.i = z__3.r * b[i__3].i +
                                        z__3.i * b[i__3].r;
                                z__1.r = temp.r + z__2.r, z__1.i = temp.i +
                                        z__2.i;
                                temp.r = z__1.r, temp.i = z__1.i;
/*<   100                   CONTINUE >*/
/* L100: */
                            }
/*<                      END IF >*/
                        }
/*<                      B( I, J ) = ALPHA*TEMP >*/
                        i__2 = i__ + j * b_dim1;
                        z__1.r = alpha->r * temp.r - alpha->i * temp.i,
                                z__1.i = alpha->r * temp.i + alpha->i *
                                temp.r;
                        b[i__2].r = z__1.r, b[i__2].i = z__1.i;
/*<   110             CONTINUE >*/
/* L110: */
                    }
/*<   120          CONTINUE >*/
/* L120: */
                }
/*<             ELSE >*/
            } else {
/*<                DO 160, J = 1, N >*/
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
/*<                   DO 150, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      TEMP = B( I, J ) >*/
                        i__3 = i__ + j * b_dim1;
                        temp.r = b[i__3].r, temp.i = b[i__3].i;
/*<                      IF( NOCONJ )THEN >*/
                        if (noconj) {
/*<    >*/
                            if (nounit) {
                                i__3 = i__ + i__ * a_dim1;
                                z__1.r = temp.r * a[i__3].r - temp.i * a[i__3]
                                        .i, z__1.i = temp.r * a[i__3].i +
                                        temp.i * a[i__3].r;
                                temp.r = z__1.r, temp.i = z__1.i;
                            }
/*<                         DO 130, K = I + 1, M >*/
                            i__3 = *m;
                            for (k = i__ + 1; k <= i__3; ++k) {
/*<                            TEMP = TEMP + A( K, I )*B( K, J ) >*/
                                i__4 = k + i__ * a_dim1;
                                i__5 = k + j * b_dim1;
                                z__2.r = a[i__4].r * b[i__5].r - a[i__4].i *
                                        b[i__5].i, z__2.i = a[i__4].r * b[
                                        i__5].i + a[i__4].i * b[i__5].r;
                                z__1.r = temp.r + z__2.r, z__1.i = temp.i +
                                        z__2.i;
                                temp.r = z__1.r, temp.i = z__1.i;
/*<   130                   CONTINUE >*/
/* L130: */
                            }
/*<                      ELSE >*/
                        } else {
/*<    >*/
                            if (nounit) {
                                d_cnjg(&z__2, &a[i__ + i__ * a_dim1]);
                                z__1.r = temp.r * z__2.r - temp.i * z__2.i,
                                        z__1.i = temp.r * z__2.i + temp.i *
                                        z__2.r;
                                temp.r = z__1.r, temp.i = z__1.i;
                            }
/*<                         DO 140, K = I + 1, M >*/
                            i__3 = *m;
                            for (k = i__ + 1; k <= i__3; ++k) {
/*<                            TEMP = TEMP + DCONJG( A( K, I ) )*B( K, J ) >*/
                                d_cnjg(&z__3, &a[k + i__ * a_dim1]);
                                i__4 = k + j * b_dim1;
                                z__2.r = z__3.r * b[i__4].r - z__3.i * b[i__4]
                                        .i, z__2.i = z__3.r * b[i__4].i +
                                        z__3.i * b[i__4].r;
                                z__1.r = temp.r + z__2.r, z__1.i = temp.i +
                                        z__2.i;
                                temp.r = z__1.r, temp.i = z__1.i;
/*<   140                   CONTINUE >*/
/* L140: */
                            }
/*<                      END IF >*/
                        }
/*<                      B( I, J ) = ALPHA*TEMP >*/
                        i__3 = i__ + j * b_dim1;
                        z__1.r = alpha->r * temp.r - alpha->i * temp.i,
                                z__1.i = alpha->r * temp.i + alpha->i *
                                temp.r;
                        b[i__3].r = z__1.r, b[i__3].i = z__1.i;
/*<   150             CONTINUE >*/
/* L150: */
                    }
/*<   160          CONTINUE >*/
/* L160: */
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       ELSE >*/
    } else {
/*<          IF( LSAME( TRANSA, 'N' ) )THEN >*/
        if (lsame_(transa, "N", (ftnlen)1, (ftnlen)1)) {

/*           Form  B := alpha*B*A. */

/*<             IF( UPPER )THEN >*/
            if (upper) {
/*<                DO 200, J = N, 1, -1 >*/
                for (j = *n; j >= 1; --j) {
/*<                   TEMP = ALPHA >*/
                    temp.r = alpha->r, temp.i = alpha->i;
/*<    >*/
                    if (nounit) {
                        i__1 = j + j * a_dim1;
                        z__1.r = temp.r * a[i__1].r - temp.i * a[i__1].i,
                                z__1.i = temp.r * a[i__1].i + temp.i * a[i__1]
                                .r;
                        temp.r = z__1.r, temp.i = z__1.i;
                    }
/*<                   DO 170, I = 1, M >*/
                    i__1 = *m;
                    for (i__ = 1; i__ <= i__1; ++i__) {
/*<                      B( I, J ) = TEMP*B( I, J ) >*/
                        i__2 = i__ + j * b_dim1;
                        i__3 = i__ + j * b_dim1;
                        z__1.r = temp.r * b[i__3].r - temp.i * b[i__3].i,
                                z__1.i = temp.r * b[i__3].i + temp.i * b[i__3]
                                .r;
                        b[i__2].r = z__1.r, b[i__2].i = z__1.i;
/*<   170             CONTINUE >*/
/* L170: */
                    }
/*<                   DO 190, K = 1, J - 1 >*/
                    i__1 = j - 1;
                    for (k = 1; k <= i__1; ++k) {
/*<                      IF( A( K, J ).NE.ZERO )THEN >*/
                        i__2 = k + j * a_dim1;
                        if (a[i__2].r != 0. || a[i__2].i != 0.) {
/*<                         TEMP = ALPHA*A( K, J ) >*/
                            i__2 = k + j * a_dim1;
                            z__1.r = alpha->r * a[i__2].r - alpha->i * a[i__2]
                                    .i, z__1.i = alpha->r * a[i__2].i +
                                    alpha->i * a[i__2].r;
                            temp.r = z__1.r, temp.i = z__1.i;
/*<                         DO 180, I = 1, M >*/
                            i__2 = *m;
                            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                            B( I, J ) = B( I, J ) + TEMP*B( I, K ) >*/
                                i__3 = i__ + j * b_dim1;
                                i__4 = i__ + j * b_dim1;
                                i__5 = i__ + k * b_dim1;
                                z__2.r = temp.r * b[i__5].r - temp.i * b[i__5]
                                        .i, z__2.i = temp.r * b[i__5].i +
                                        temp.i * b[i__5].r;
                                z__1.r = b[i__4].r + z__2.r, z__1.i = b[i__4]
                                        .i + z__2.i;
                                b[i__3].r = z__1.r, b[i__3].i = z__1.i;
/*<   180                   CONTINUE >*/
/* L180: */
                            }
/*<                      END IF >*/
                        }
/*<   190             CONTINUE >*/
/* L190: */
                    }
/*<   200          CONTINUE >*/
/* L200: */
                }
/*<             ELSE >*/
            } else {
/*<                DO 240, J = 1, N >*/
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
/*<                   TEMP = ALPHA >*/
                    temp.r = alpha->r, temp.i = alpha->i;
/*<    >*/
                    if (nounit) {
                        i__2 = j + j * a_dim1;
                        z__1.r = temp.r * a[i__2].r - temp.i * a[i__2].i,
                                z__1.i = temp.r * a[i__2].i + temp.i * a[i__2]
                                .r;
                        temp.r = z__1.r, temp.i = z__1.i;
                    }
/*<                   DO 210, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      B( I, J ) = TEMP*B( I, J ) >*/
                        i__3 = i__ + j * b_dim1;
                        i__4 = i__ + j * b_dim1;
                        z__1.r = temp.r * b[i__4].r - temp.i * b[i__4].i,
                                z__1.i = temp.r * b[i__4].i + temp.i * b[i__4]
                                .r;
                        b[i__3].r = z__1.r, b[i__3].i = z__1.i;
/*<   210             CONTINUE >*/
/* L210: */
                    }
/*<                   DO 230, K = J + 1, N >*/
                    i__2 = *n;
                    for (k = j + 1; k <= i__2; ++k) {
/*<                      IF( A( K, J ).NE.ZERO )THEN >*/
                        i__3 = k + j * a_dim1;
                        if (a[i__3].r != 0. || a[i__3].i != 0.) {
/*<                         TEMP = ALPHA*A( K, J ) >*/
                            i__3 = k + j * a_dim1;
                            z__1.r = alpha->r * a[i__3].r - alpha->i * a[i__3]
                                    .i, z__1.i = alpha->r * a[i__3].i +
                                    alpha->i * a[i__3].r;
                            temp.r = z__1.r, temp.i = z__1.i;
/*<                         DO 220, I = 1, M >*/
                            i__3 = *m;
                            for (i__ = 1; i__ <= i__3; ++i__) {
/*<                            B( I, J ) = B( I, J ) + TEMP*B( I, K ) >*/
                                i__4 = i__ + j * b_dim1;
                                i__5 = i__ + j * b_dim1;
                                i__6 = i__ + k * b_dim1;
                                z__2.r = temp.r * b[i__6].r - temp.i * b[i__6]
                                        .i, z__2.i = temp.r * b[i__6].i +
                                        temp.i * b[i__6].r;
                                z__1.r = b[i__5].r + z__2.r, z__1.i = b[i__5]
                                        .i + z__2.i;
                                b[i__4].r = z__1.r, b[i__4].i = z__1.i;
/*<   220                   CONTINUE >*/
/* L220: */
                            }
/*<                      END IF >*/
                        }
/*<   230             CONTINUE >*/
/* L230: */
                    }
/*<   240          CONTINUE >*/
/* L240: */
                }
/*<             END IF >*/
            }
/*<          ELSE >*/
        } else {

/*           Form  B := alpha*B*A'   or   B := alpha*B*conjg( A' ). */

/*<             IF( UPPER )THEN >*/
            if (upper) {
/*<                DO 280, K = 1, N >*/
                i__1 = *n;
                for (k = 1; k <= i__1; ++k) {
/*<                   DO 260, J = 1, K - 1 >*/
                    i__2 = k - 1;
                    for (j = 1; j <= i__2; ++j) {
/*<                      IF( A( J, K ).NE.ZERO )THEN >*/
                        i__3 = j + k * a_dim1;
                        if (a[i__3].r != 0. || a[i__3].i != 0.) {
/*<                         IF( NOCONJ )THEN >*/
                            if (noconj) {
/*<                            TEMP = ALPHA*A( J, K ) >*/
                                i__3 = j + k * a_dim1;
                                z__1.r = alpha->r * a[i__3].r - alpha->i * a[
                                        i__3].i, z__1.i = alpha->r * a[i__3]
                                        .i + alpha->i * a[i__3].r;
                                temp.r = z__1.r, temp.i = z__1.i;
/*<                         ELSE >*/
                            } else {
/*<                            TEMP = ALPHA*DCONJG( A( J, K ) ) >*/
                                d_cnjg(&z__2, &a[j + k * a_dim1]);
                                z__1.r = alpha->r * z__2.r - alpha->i *
                                        z__2.i, z__1.i = alpha->r * z__2.i +
                                        alpha->i * z__2.r;
                                temp.r = z__1.r, temp.i = z__1.i;
/*<                         END IF >*/
                            }
/*<                         DO 250, I = 1, M >*/
                            i__3 = *m;
                            for (i__ = 1; i__ <= i__3; ++i__) {
/*<                            B( I, J ) = B( I, J ) + TEMP*B( I, K ) >*/
                                i__4 = i__ + j * b_dim1;
                                i__5 = i__ + j * b_dim1;
                                i__6 = i__ + k * b_dim1;
                                z__2.r = temp.r * b[i__6].r - temp.i * b[i__6]
                                        .i, z__2.i = temp.r * b[i__6].i +
                                        temp.i * b[i__6].r;
                                z__1.r = b[i__5].r + z__2.r, z__1.i = b[i__5]
                                        .i + z__2.i;
                                b[i__4].r = z__1.r, b[i__4].i = z__1.i;
/*<   250                   CONTINUE >*/
/* L250: */
                            }
/*<                      END IF >*/
                        }
/*<   260             CONTINUE >*/
/* L260: */
                    }
/*<                   TEMP = ALPHA >*/
                    temp.r = alpha->r, temp.i = alpha->i;
/*<                   IF( NOUNIT )THEN >*/
                    if (nounit) {
/*<                      IF( NOCONJ )THEN >*/
                        if (noconj) {
/*<                         TEMP = TEMP*A( K, K ) >*/
                            i__2 = k + k * a_dim1;
                            z__1.r = temp.r * a[i__2].r - temp.i * a[i__2].i,
                                    z__1.i = temp.r * a[i__2].i + temp.i * a[
                                    i__2].r;
                            temp.r = z__1.r, temp.i = z__1.i;
/*<                      ELSE >*/
                        } else {
/*<                         TEMP = TEMP*DCONJG( A( K, K ) ) >*/
                            d_cnjg(&z__2, &a[k + k * a_dim1]);
                            z__1.r = temp.r * z__2.r - temp.i * z__2.i,
                                    z__1.i = temp.r * z__2.i + temp.i *
                                    z__2.r;
                            temp.r = z__1.r, temp.i = z__1.i;
/*<                      END IF >*/
                        }
/*<                   END IF >*/
                    }
/*<                   IF( TEMP.NE.ONE )THEN >*/
                    if (temp.r != 1. || temp.i != 0.) {
/*<                      DO 270, I = 1, M >*/
                        i__2 = *m;
                        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                         B( I, K ) = TEMP*B( I, K ) >*/
                            i__3 = i__ + k * b_dim1;
                            i__4 = i__ + k * b_dim1;
                            z__1.r = temp.r * b[i__4].r - temp.i * b[i__4].i,
                                    z__1.i = temp.r * b[i__4].i + temp.i * b[
                                    i__4].r;
                            b[i__3].r = z__1.r, b[i__3].i = z__1.i;
/*<   270                CONTINUE >*/
/* L270: */
                        }
/*<                   END IF >*/
                    }
/*<   280          CONTINUE >*/
/* L280: */
                }
/*<             ELSE >*/
            } else {
/*<                DO 320, K = N, 1, -1 >*/
                for (k = *n; k >= 1; --k) {
/*<                   DO 300, J = K + 1, N >*/
                    i__1 = *n;
                    for (j = k + 1; j <= i__1; ++j) {
/*<                      IF( A( J, K ).NE.ZERO )THEN >*/
                        i__2 = j + k * a_dim1;
                        if (a[i__2].r != 0. || a[i__2].i != 0.) {
/*<                         IF( NOCONJ )THEN >*/
                            if (noconj) {
/*<                            TEMP = ALPHA*A( J, K ) >*/
                                i__2 = j + k * a_dim1;
                                z__1.r = alpha->r * a[i__2].r - alpha->i * a[
                                        i__2].i, z__1.i = alpha->r * a[i__2]
                                        .i + alpha->i * a[i__2].r;
                                temp.r = z__1.r, temp.i = z__1.i;
/*<                         ELSE >*/
                            } else {
/*<                            TEMP = ALPHA*DCONJG( A( J, K ) ) >*/
                                d_cnjg(&z__2, &a[j + k * a_dim1]);
                                z__1.r = alpha->r * z__2.r - alpha->i *
                                        z__2.i, z__1.i = alpha->r * z__2.i +
                                        alpha->i * z__2.r;
                                temp.r = z__1.r, temp.i = z__1.i;
/*<                         END IF >*/
                            }
/*<                         DO 290, I = 1, M >*/
                            i__2 = *m;
                            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                            B( I, J ) = B( I, J ) + TEMP*B( I, K ) >*/
                                i__3 = i__ + j * b_dim1;
                                i__4 = i__ + j * b_dim1;
                                i__5 = i__ + k * b_dim1;
                                z__2.r = temp.r * b[i__5].r - temp.i * b[i__5]
                                        .i, z__2.i = temp.r * b[i__5].i +
                                        temp.i * b[i__5].r;
                                z__1.r = b[i__4].r + z__2.r, z__1.i = b[i__4]
                                        .i + z__2.i;
                                b[i__3].r = z__1.r, b[i__3].i = z__1.i;
/*<   290                   CONTINUE >*/
/* L290: */
                            }
/*<                      END IF >*/
                        }
/*<   300             CONTINUE >*/
/* L300: */
                    }
/*<                   TEMP = ALPHA >*/
                    temp.r = alpha->r, temp.i = alpha->i;
/*<                   IF( NOUNIT )THEN >*/
                    if (nounit) {
/*<                      IF( NOCONJ )THEN >*/
                        if (noconj) {
/*<                         TEMP = TEMP*A( K, K ) >*/
                            i__1 = k + k * a_dim1;
                            z__1.r = temp.r * a[i__1].r - temp.i * a[i__1].i,
                                    z__1.i = temp.r * a[i__1].i + temp.i * a[
                                    i__1].r;
                            temp.r = z__1.r, temp.i = z__1.i;
/*<                      ELSE >*/
                        } else {
/*<                         TEMP = TEMP*DCONJG( A( K, K ) ) >*/
                            d_cnjg(&z__2, &a[k + k * a_dim1]);
                            z__1.r = temp.r * z__2.r - temp.i * z__2.i,
                                    z__1.i = temp.r * z__2.i + temp.i *
                                    z__2.r;
                            temp.r = z__1.r, temp.i = z__1.i;
/*<                      END IF >*/
                        }
/*<                   END IF >*/
                    }
/*<                   IF( TEMP.NE.ONE )THEN >*/
                    if (temp.r != 1. || temp.i != 0.) {
/*<                      DO 310, I = 1, M >*/
                        i__1 = *m;
                        for (i__ = 1; i__ <= i__1; ++i__) {
/*<                         B( I, K ) = TEMP*B( I, K ) >*/
                            i__2 = i__ + k * b_dim1;
                            i__3 = i__ + k * b_dim1;
                            z__1.r = temp.r * b[i__3].r - temp.i * b[i__3].i,
                                    z__1.i = temp.r * b[i__3].i + temp.i * b[
                                    i__3].r;
                            b[i__2].r = z__1.r, b[i__2].i = z__1.i;
/*<   310                CONTINUE >*/
/* L310: */
                        }
/*<                   END IF >*/
                    }
/*<   320          CONTINUE >*/
/* L320: */
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of ZTRMM . */

/*<       END >*/
} /* ztrmm_ */

#ifdef __cplusplus
        }
#endif
