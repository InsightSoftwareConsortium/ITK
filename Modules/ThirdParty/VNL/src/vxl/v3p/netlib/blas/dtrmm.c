/* blas/dtrmm.f -- translated by f2c (version 20050501).
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
/* Subroutine */ int dtrmm_(char *side, char *uplo, char *transa, char *diag,
        integer *m, integer *n, doublereal *alpha, doublereal *a, integer *
        lda, doublereal *b, integer *ldb, ftnlen side_len, ftnlen uplo_len,
        ftnlen transa_len, ftnlen diag_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2, i__3;

    /* Local variables */
    integer i__, j, k, info;
    doublereal temp;
    logical lside;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    integer nrowa;
    logical upper;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    logical nounit;
    (void)side_len;
    (void)uplo_len;
    (void)transa_len;
    (void)diag_len;

/*     .. Scalar Arguments .. */
/*<       CHARACTER*1        SIDE, UPLO, TRANSA, DIAG >*/
/*<       INTEGER            M, N, LDA, LDB >*/
/*<       DOUBLE PRECISION   ALPHA >*/
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   A( LDA, * ), B( LDB, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DTRMM  performs one of the matrix-matrix operations */

/*     B := alpha*op( A )*B,   or   B := alpha*B*op( A ), */

/*  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or */
/*  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of */

/*     op( A ) = A   or   op( A ) = A'. */

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

/*              TRANSA = 'C' or 'c'   op( A ) = A'. */

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

/*  ALPHA  - DOUBLE PRECISION. */
/*           On entry,  ALPHA specifies the scalar  alpha. When  alpha is */
/*           zero then  A is not referenced and  B need not be set before */
/*           entry. */
/*           Unchanged on exit. */

/*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m */
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

/*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ). */
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
/*<       INTRINSIC          MAX >*/
/*     .. Local Scalars .. */
/*<       LOGICAL            LSIDE, NOUNIT, UPPER >*/
/*<       INTEGER            I, INFO, J, K, NROWA >*/
/*<       DOUBLE PRECISION   TEMP >*/
/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ONE         , ZERO >*/
/*<       PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 ) >*/
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
/*<          CALL XERBLA( 'DTRMM ', INFO ) >*/
        xerbla_("DTRMM ", &info, (ftnlen)6);
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
    if (*alpha == 0.) {
/*<          DO 20, J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 10, I = 1, M >*/
            i__2 = *m;
            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                B( I, J ) = ZERO >*/
                b[i__ + j * b_dim1] = 0.;
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
                        if (b[k + j * b_dim1] != 0.) {
/*<                         TEMP = ALPHA*B( K, J ) >*/
                            temp = *alpha * b[k + j * b_dim1];
/*<                         DO 30, I = 1, K - 1 >*/
                            i__3 = k - 1;
                            for (i__ = 1; i__ <= i__3; ++i__) {
/*<                            B( I, J ) = B( I, J ) + TEMP*A( I, K ) >*/
                                b[i__ + j * b_dim1] += temp * a[i__ + k *
                                        a_dim1];
/*<    30                   CONTINUE >*/
/* L30: */
                            }
/*<    >*/
                            if (nounit) {
                                temp *= a[k + k * a_dim1];
                            }
/*<                         B( K, J ) = TEMP >*/
                            b[k + j * b_dim1] = temp;
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
                        if (b[k + j * b_dim1] != 0.) {
/*<                         TEMP      = ALPHA*B( K, J ) >*/
                            temp = *alpha * b[k + j * b_dim1];
/*<                         B( K, J ) = TEMP >*/
                            b[k + j * b_dim1] = temp;
/*<    >*/
                            if (nounit) {
                                b[k + j * b_dim1] *= a[k + k * a_dim1];
                            }
/*<                         DO 60, I = K + 1, M >*/
                            i__2 = *m;
                            for (i__ = k + 1; i__ <= i__2; ++i__) {
/*<                            B( I, J ) = B( I, J ) + TEMP*A( I, K ) >*/
                                b[i__ + j * b_dim1] += temp * a[i__ + k *
                                        a_dim1];
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

/*           Form  B := alpha*A'*B. */

/*<             IF( UPPER )THEN >*/
            if (upper) {
/*<                DO 110, J = 1, N >*/
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
/*<                   DO 100, I = M, 1, -1 >*/
                    for (i__ = *m; i__ >= 1; --i__) {
/*<                      TEMP = B( I, J ) >*/
                        temp = b[i__ + j * b_dim1];
/*<    >*/
                        if (nounit) {
                            temp *= a[i__ + i__ * a_dim1];
                        }
/*<                      DO 90, K = 1, I - 1 >*/
                        i__2 = i__ - 1;
                        for (k = 1; k <= i__2; ++k) {
/*<                         TEMP = TEMP + A( K, I )*B( K, J ) >*/
                            temp += a[k + i__ * a_dim1] * b[k + j * b_dim1];
/*<    90                CONTINUE >*/
/* L90: */
                        }
/*<                      B( I, J ) = ALPHA*TEMP >*/
                        b[i__ + j * b_dim1] = *alpha * temp;
/*<   100             CONTINUE >*/
/* L100: */
                    }
/*<   110          CONTINUE >*/
/* L110: */
                }
/*<             ELSE >*/
            } else {
/*<                DO 140, J = 1, N >*/
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
/*<                   DO 130, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      TEMP = B( I, J ) >*/
                        temp = b[i__ + j * b_dim1];
/*<    >*/
                        if (nounit) {
                            temp *= a[i__ + i__ * a_dim1];
                        }
/*<                      DO 120, K = I + 1, M >*/
                        i__3 = *m;
                        for (k = i__ + 1; k <= i__3; ++k) {
/*<                         TEMP = TEMP + A( K, I )*B( K, J ) >*/
                            temp += a[k + i__ * a_dim1] * b[k + j * b_dim1];
/*<   120                CONTINUE >*/
/* L120: */
                        }
/*<                      B( I, J ) = ALPHA*TEMP >*/
                        b[i__ + j * b_dim1] = *alpha * temp;
/*<   130             CONTINUE >*/
/* L130: */
                    }
/*<   140          CONTINUE >*/
/* L140: */
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
/*<                DO 180, J = N, 1, -1 >*/
                for (j = *n; j >= 1; --j) {
/*<                   TEMP = ALPHA >*/
                    temp = *alpha;
/*<    >*/
                    if (nounit) {
                        temp *= a[j + j * a_dim1];
                    }
/*<                   DO 150, I = 1, M >*/
                    i__1 = *m;
                    for (i__ = 1; i__ <= i__1; ++i__) {
/*<                      B( I, J ) = TEMP*B( I, J ) >*/
                        b[i__ + j * b_dim1] = temp * b[i__ + j * b_dim1];
/*<   150             CONTINUE >*/
/* L150: */
                    }
/*<                   DO 170, K = 1, J - 1 >*/
                    i__1 = j - 1;
                    for (k = 1; k <= i__1; ++k) {
/*<                      IF( A( K, J ).NE.ZERO )THEN >*/
                        if (a[k + j * a_dim1] != 0.) {
/*<                         TEMP = ALPHA*A( K, J ) >*/
                            temp = *alpha * a[k + j * a_dim1];
/*<                         DO 160, I = 1, M >*/
                            i__2 = *m;
                            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                            B( I, J ) = B( I, J ) + TEMP*B( I, K ) >*/
                                b[i__ + j * b_dim1] += temp * b[i__ + k *
                                        b_dim1];
/*<   160                   CONTINUE >*/
/* L160: */
                            }
/*<                      END IF >*/
                        }
/*<   170             CONTINUE >*/
/* L170: */
                    }
/*<   180          CONTINUE >*/
/* L180: */
                }
/*<             ELSE >*/
            } else {
/*<                DO 220, J = 1, N >*/
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
/*<                   TEMP = ALPHA >*/
                    temp = *alpha;
/*<    >*/
                    if (nounit) {
                        temp *= a[j + j * a_dim1];
                    }
/*<                   DO 190, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      B( I, J ) = TEMP*B( I, J ) >*/
                        b[i__ + j * b_dim1] = temp * b[i__ + j * b_dim1];
/*<   190             CONTINUE >*/
/* L190: */
                    }
/*<                   DO 210, K = J + 1, N >*/
                    i__2 = *n;
                    for (k = j + 1; k <= i__2; ++k) {
/*<                      IF( A( K, J ).NE.ZERO )THEN >*/
                        if (a[k + j * a_dim1] != 0.) {
/*<                         TEMP = ALPHA*A( K, J ) >*/
                            temp = *alpha * a[k + j * a_dim1];
/*<                         DO 200, I = 1, M >*/
                            i__3 = *m;
                            for (i__ = 1; i__ <= i__3; ++i__) {
/*<                            B( I, J ) = B( I, J ) + TEMP*B( I, K ) >*/
                                b[i__ + j * b_dim1] += temp * b[i__ + k *
                                        b_dim1];
/*<   200                   CONTINUE >*/
/* L200: */
                            }
/*<                      END IF >*/
                        }
/*<   210             CONTINUE >*/
/* L210: */
                    }
/*<   220          CONTINUE >*/
/* L220: */
                }
/*<             END IF >*/
            }
/*<          ELSE >*/
        } else {

/*           Form  B := alpha*B*A'. */

/*<             IF( UPPER )THEN >*/
            if (upper) {
/*<                DO 260, K = 1, N >*/
                i__1 = *n;
                for (k = 1; k <= i__1; ++k) {
/*<                   DO 240, J = 1, K - 1 >*/
                    i__2 = k - 1;
                    for (j = 1; j <= i__2; ++j) {
/*<                      IF( A( J, K ).NE.ZERO )THEN >*/
                        if (a[j + k * a_dim1] != 0.) {
/*<                         TEMP = ALPHA*A( J, K ) >*/
                            temp = *alpha * a[j + k * a_dim1];
/*<                         DO 230, I = 1, M >*/
                            i__3 = *m;
                            for (i__ = 1; i__ <= i__3; ++i__) {
/*<                            B( I, J ) = B( I, J ) + TEMP*B( I, K ) >*/
                                b[i__ + j * b_dim1] += temp * b[i__ + k *
                                        b_dim1];
/*<   230                   CONTINUE >*/
/* L230: */
                            }
/*<                      END IF >*/
                        }
/*<   240             CONTINUE >*/
/* L240: */
                    }
/*<                   TEMP = ALPHA >*/
                    temp = *alpha;
/*<    >*/
                    if (nounit) {
                        temp *= a[k + k * a_dim1];
                    }
/*<                   IF( TEMP.NE.ONE )THEN >*/
                    if (temp != 1.) {
/*<                      DO 250, I = 1, M >*/
                        i__2 = *m;
                        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                         B( I, K ) = TEMP*B( I, K ) >*/
                            b[i__ + k * b_dim1] = temp * b[i__ + k * b_dim1];
/*<   250                CONTINUE >*/
/* L250: */
                        }
/*<                   END IF >*/
                    }
/*<   260          CONTINUE >*/
/* L260: */
                }
/*<             ELSE >*/
            } else {
/*<                DO 300, K = N, 1, -1 >*/
                for (k = *n; k >= 1; --k) {
/*<                   DO 280, J = K + 1, N >*/
                    i__1 = *n;
                    for (j = k + 1; j <= i__1; ++j) {
/*<                      IF( A( J, K ).NE.ZERO )THEN >*/
                        if (a[j + k * a_dim1] != 0.) {
/*<                         TEMP = ALPHA*A( J, K ) >*/
                            temp = *alpha * a[j + k * a_dim1];
/*<                         DO 270, I = 1, M >*/
                            i__2 = *m;
                            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                            B( I, J ) = B( I, J ) + TEMP*B( I, K ) >*/
                                b[i__ + j * b_dim1] += temp * b[i__ + k *
                                        b_dim1];
/*<   270                   CONTINUE >*/
/* L270: */
                            }
/*<                      END IF >*/
                        }
/*<   280             CONTINUE >*/
/* L280: */
                    }
/*<                   TEMP = ALPHA >*/
                    temp = *alpha;
/*<    >*/
                    if (nounit) {
                        temp *= a[k + k * a_dim1];
                    }
/*<                   IF( TEMP.NE.ONE )THEN >*/
                    if (temp != 1.) {
/*<                      DO 290, I = 1, M >*/
                        i__1 = *m;
                        for (i__ = 1; i__ <= i__1; ++i__) {
/*<                         B( I, K ) = TEMP*B( I, K ) >*/
                            b[i__ + k * b_dim1] = temp * b[i__ + k * b_dim1];
/*<   290                CONTINUE >*/
/* L290: */
                        }
/*<                   END IF >*/
                    }
/*<   300          CONTINUE >*/
/* L300: */
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of DTRMM . */

/*<       END >*/
} /* dtrmm_ */

#ifdef __cplusplus
        }
#endif
