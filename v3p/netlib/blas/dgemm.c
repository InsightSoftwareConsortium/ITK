/* blas/dgemm.f -- translated by f2c (version 20050501).
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
/* Subroutine */ int dgemm_(char *transa, char *transb, integer *m, integer *
        n, integer *k, doublereal *alpha, doublereal *a, integer *lda,
        doublereal *b, integer *ldb, doublereal *beta, doublereal *c__,
        integer *ldc, ftnlen transa_len, ftnlen transb_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, i__1, i__2,
            i__3;

    /* Local variables */
    integer i__, j, l, info;
    logical nota, notb;
    doublereal temp;
    integer ncola;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    integer nrowa, nrowb;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    (void)transa_len;
    (void)transb_len;

/*     .. Scalar Arguments .. */
/*<       CHARACTER*1        TRANSA, TRANSB >*/
/*<       INTEGER            M, N, K, LDA, LDB, LDC >*/
/*<       DOUBLE PRECISION   ALPHA, BETA >*/
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DGEMM  performs one of the matrix-matrix operations */

/*     C := alpha*op( A )*op( B ) + beta*C, */

/*  where  op( X ) is one of */

/*     op( X ) = X   or   op( X ) = X', */

/*  alpha and beta are scalars, and A, B and C are matrices, with op( A ) */
/*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix. */

/*  Parameters */
/*  ========== */

/*  TRANSA - CHARACTER*1. */
/*           On entry, TRANSA specifies the form of op( A ) to be used in */
/*           the matrix multiplication as follows: */

/*              TRANSA = 'N' or 'n',  op( A ) = A. */

/*              TRANSA = 'T' or 't',  op( A ) = A'. */

/*              TRANSA = 'C' or 'c',  op( A ) = A'. */

/*           Unchanged on exit. */

/*  TRANSB - CHARACTER*1. */
/*           On entry, TRANSB specifies the form of op( B ) to be used in */
/*           the matrix multiplication as follows: */

/*              TRANSB = 'N' or 'n',  op( B ) = B. */

/*              TRANSB = 'T' or 't',  op( B ) = B'. */

/*              TRANSB = 'C' or 'c',  op( B ) = B'. */

/*           Unchanged on exit. */

/*  M      - INTEGER. */
/*           On entry,  M  specifies  the number  of rows  of the  matrix */
/*           op( A )  and of the  matrix  C.  M  must  be at least  zero. */
/*           Unchanged on exit. */

/*  N      - INTEGER. */
/*           On entry,  N  specifies the number  of columns of the matrix */
/*           op( B ) and the number of columns of the matrix C. N must be */
/*           at least zero. */
/*           Unchanged on exit. */

/*  K      - INTEGER. */
/*           On entry,  K  specifies  the number of columns of the matrix */
/*           op( A ) and the number of rows of the matrix op( B ). K must */
/*           be at least  zero. */
/*           Unchanged on exit. */

/*  ALPHA  - DOUBLE PRECISION. */
/*           On entry, ALPHA specifies the scalar alpha. */
/*           Unchanged on exit. */

/*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is */
/*           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise. */
/*           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k */
/*           part of the array  A  must contain the matrix  A,  otherwise */
/*           the leading  k by m  part of the array  A  must contain  the */
/*           matrix A. */
/*           Unchanged on exit. */

/*  LDA    - INTEGER. */
/*           On entry, LDA specifies the first dimension of A as declared */
/*           in the calling (sub) program. When  TRANSA = 'N' or 'n' then */
/*           LDA must be at least  max( 1, m ), otherwise  LDA must be at */
/*           least  max( 1, k ). */
/*           Unchanged on exit. */

/*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is */
/*           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise. */
/*           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n */
/*           part of the array  B  must contain the matrix  B,  otherwise */
/*           the leading  n by k  part of the array  B  must contain  the */
/*           matrix B. */
/*           Unchanged on exit. */

/*  LDB    - INTEGER. */
/*           On entry, LDB specifies the first dimension of B as declared */
/*           in the calling (sub) program. When  TRANSB = 'N' or 'n' then */
/*           LDB must be at least  max( 1, k ), otherwise  LDB must be at */
/*           least  max( 1, n ). */
/*           Unchanged on exit. */

/*  BETA   - DOUBLE PRECISION. */
/*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is */
/*           supplied as zero then C need not be set on input. */
/*           Unchanged on exit. */

/*  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ). */
/*           Before entry, the leading  m by n  part of the array  C must */
/*           contain the matrix  C,  except when  beta  is zero, in which */
/*           case C need not be set on entry. */
/*           On exit, the array  C  is overwritten by the  m by n  matrix */
/*           ( alpha*op( A )*op( B ) + beta*C ). */

/*  LDC    - INTEGER. */
/*           On entry, LDC specifies the first dimension of C as declared */
/*           in  the  calling  (sub)  program.   LDC  must  be  at  least */
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
/*<       LOGICAL            NOTA, NOTB >*/
/*<       INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB >*/
/*<       DOUBLE PRECISION   TEMP >*/
/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ONE         , ZERO >*/
/*<       PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 ) >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not */
/*     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows */
/*     and  columns of  A  and the  number of  rows  of  B  respectively. */

/*<       NOTA  = LSAME( TRANSA, 'N' ) >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1;
    b -= b_offset;
    c_dim1 = *ldc;
    c_offset = 1 + c_dim1;
    c__ -= c_offset;

    /* Function Body */
    nota = lsame_(transa, "N", (ftnlen)1, (ftnlen)1);
/*<       NOTB  = LSAME( TRANSB, 'N' ) >*/
    notb = lsame_(transb, "N", (ftnlen)1, (ftnlen)1);
/*<       IF( NOTA )THEN >*/
    if (nota) {
/*<          NROWA = M >*/
        nrowa = *m;
/*<          NCOLA = K >*/
//        ncola = *k;
/*<       ELSE >*/
    } else {
/*<          NROWA = K >*/
        nrowa = *k;
/*<          NCOLA = M >*/
//        ncola = *m;
/*<       END IF >*/
    }
/*<       IF( NOTB )THEN >*/
    if (notb) {
/*<          NROWB = K >*/
        nrowb = *k;
/*<       ELSE >*/
    } else {
/*<          NROWB = N >*/
        nrowb = *n;
/*<       END IF >*/
    }

/*     Test the input parameters. */

/*<       INFO = 0 >*/
    info = 0;
/*<    >*/
    if (! nota && ! lsame_(transa, "C", (ftnlen)1, (ftnlen)1) && ! lsame_(
            transa, "T", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = 1 >*/
        info = 1;
/*<    >*/
    } else if (! notb && ! lsame_(transb, "C", (ftnlen)1, (ftnlen)1) && !
            lsame_(transb, "T", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = 2 >*/
        info = 2;
/*<       ELSE IF( M  .LT.0               )THEN >*/
    } else if (*m < 0) {
/*<          INFO = 3 >*/
        info = 3;
/*<       ELSE IF( N  .LT.0               )THEN >*/
    } else if (*n < 0) {
/*<          INFO = 4 >*/
        info = 4;
/*<       ELSE IF( K  .LT.0               )THEN >*/
    } else if (*k < 0) {
/*<          INFO = 5 >*/
        info = 5;
/*<       ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN >*/
    } else if (*lda < max(1,nrowa)) {
/*<          INFO = 8 >*/
        info = 8;
/*<       ELSE IF( LDB.LT.MAX( 1, NROWB ) )THEN >*/
    } else if (*ldb < max(1,nrowb)) {
/*<          INFO = 10 >*/
        info = 10;
/*<       ELSE IF( LDC.LT.MAX( 1, M     ) )THEN >*/
    } else if (*ldc < max(1,*m)) {
/*<          INFO = 13 >*/
        info = 13;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 )THEN >*/
    if (info != 0) {
/*<          CALL XERBLA( 'DGEMM ', INFO ) >*/
        xerbla_("DGEMM ", &info, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible. */

/*<    >*/
    if (*m == 0 || *n == 0 || ((*alpha == 0. || *k == 0) && *beta == 1.)) {
        return 0;
    }

/*     And if  alpha.eq.zero. */

/*<       IF( ALPHA.EQ.ZERO )THEN >*/
    if (*alpha == 0.) {
/*<          IF( BETA.EQ.ZERO )THEN >*/
        if (*beta == 0.) {
/*<             DO 20, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                DO 10, I = 1, M >*/
                i__2 = *m;
                for (i__ = 1; i__ <= i__2; ++i__) {
/*<                   C( I, J ) = ZERO >*/
                    c__[i__ + j * c_dim1] = 0.;
/*<    10          CONTINUE >*/
/* L10: */
                }
/*<    20       CONTINUE >*/
/* L20: */
            }
/*<          ELSE >*/
        } else {
/*<             DO 40, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                DO 30, I = 1, M >*/
                i__2 = *m;
                for (i__ = 1; i__ <= i__2; ++i__) {
/*<                   C( I, J ) = BETA*C( I, J ) >*/
                    c__[i__ + j * c_dim1] = *beta * c__[i__ + j * c_dim1];
/*<    30          CONTINUE >*/
/* L30: */
                }
/*<    40       CONTINUE >*/
/* L40: */
            }
/*<          END IF >*/
        }
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Start the operations. */

/*<       IF( NOTB )THEN >*/
    if (notb) {
/*<          IF( NOTA )THEN >*/
        if (nota) {

/*           Form  C := alpha*A*B + beta*C. */

/*<             DO 90, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                IF( BETA.EQ.ZERO )THEN >*/
                if (*beta == 0.) {
/*<                   DO 50, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( I, J ) = ZERO >*/
                        c__[i__ + j * c_dim1] = 0.;
/*<    50             CONTINUE >*/
/* L50: */
                    }
/*<                ELSE IF( BETA.NE.ONE )THEN >*/
                } else if (*beta != 1.) {
/*<                   DO 60, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( I, J ) = BETA*C( I, J ) >*/
                        c__[i__ + j * c_dim1] = *beta * c__[i__ + j * c_dim1];
/*<    60             CONTINUE >*/
/* L60: */
                    }
/*<                END IF >*/
                }
/*<                DO 80, L = 1, K >*/
                i__2 = *k;
                for (l = 1; l <= i__2; ++l) {
/*<                   IF( B( L, J ).NE.ZERO )THEN >*/
                    if (b[l + j * b_dim1] != 0.) {
/*<                      TEMP = ALPHA*B( L, J ) >*/
                        temp = *alpha * b[l + j * b_dim1];
/*<                      DO 70, I = 1, M >*/
                        i__3 = *m;
                        for (i__ = 1; i__ <= i__3; ++i__) {
/*<                         C( I, J ) = C( I, J ) + TEMP*A( I, L ) >*/
                            c__[i__ + j * c_dim1] += temp * a[i__ + l *
                                    a_dim1];
/*<    70                CONTINUE >*/
/* L70: */
                        }
/*<                   END IF >*/
                    }
/*<    80          CONTINUE >*/
/* L80: */
                }
/*<    90       CONTINUE >*/
/* L90: */
            }
/*<          ELSE >*/
        } else {

/*           Form  C := alpha*A'*B + beta*C */

/*<             DO 120, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                DO 110, I = 1, M >*/
                i__2 = *m;
                for (i__ = 1; i__ <= i__2; ++i__) {
/*<                   TEMP = ZERO >*/
                    temp = 0.;
/*<                   DO 100, L = 1, K >*/
                    i__3 = *k;
                    for (l = 1; l <= i__3; ++l) {
/*<                      TEMP = TEMP + A( L, I )*B( L, J ) >*/
                        temp += a[l + i__ * a_dim1] * b[l + j * b_dim1];
/*<   100             CONTINUE >*/
/* L100: */
                    }
/*<                   IF( BETA.EQ.ZERO )THEN >*/
                    if (*beta == 0.) {
/*<                      C( I, J ) = ALPHA*TEMP >*/
                        c__[i__ + j * c_dim1] = *alpha * temp;
/*<                   ELSE >*/
                    } else {
/*<                      C( I, J ) = ALPHA*TEMP + BETA*C( I, J ) >*/
                        c__[i__ + j * c_dim1] = *alpha * temp + *beta * c__[
                                i__ + j * c_dim1];
/*<                   END IF >*/
                    }
/*<   110          CONTINUE >*/
/* L110: */
                }
/*<   120       CONTINUE >*/
/* L120: */
            }
/*<          END IF >*/
        }
/*<       ELSE >*/
    } else {
/*<          IF( NOTA )THEN >*/
        if (nota) {

/*           Form  C := alpha*A*B' + beta*C */

/*<             DO 170, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                IF( BETA.EQ.ZERO )THEN >*/
                if (*beta == 0.) {
/*<                   DO 130, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( I, J ) = ZERO >*/
                        c__[i__ + j * c_dim1] = 0.;
/*<   130             CONTINUE >*/
/* L130: */
                    }
/*<                ELSE IF( BETA.NE.ONE )THEN >*/
                } else if (*beta != 1.) {
/*<                   DO 140, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( I, J ) = BETA*C( I, J ) >*/
                        c__[i__ + j * c_dim1] = *beta * c__[i__ + j * c_dim1];
/*<   140             CONTINUE >*/
/* L140: */
                    }
/*<                END IF >*/
                }
/*<                DO 160, L = 1, K >*/
                i__2 = *k;
                for (l = 1; l <= i__2; ++l) {
/*<                   IF( B( J, L ).NE.ZERO )THEN >*/
                    if (b[j + l * b_dim1] != 0.) {
/*<                      TEMP = ALPHA*B( J, L ) >*/
                        temp = *alpha * b[j + l * b_dim1];
/*<                      DO 150, I = 1, M >*/
                        i__3 = *m;
                        for (i__ = 1; i__ <= i__3; ++i__) {
/*<                         C( I, J ) = C( I, J ) + TEMP*A( I, L ) >*/
                            c__[i__ + j * c_dim1] += temp * a[i__ + l *
                                    a_dim1];
/*<   150                CONTINUE >*/
/* L150: */
                        }
/*<                   END IF >*/
                    }
/*<   160          CONTINUE >*/
/* L160: */
                }
/*<   170       CONTINUE >*/
/* L170: */
            }
/*<          ELSE >*/
        } else {

/*           Form  C := alpha*A'*B' + beta*C */

/*<             DO 200, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                DO 190, I = 1, M >*/
                i__2 = *m;
                for (i__ = 1; i__ <= i__2; ++i__) {
/*<                   TEMP = ZERO >*/
                    temp = 0.;
/*<                   DO 180, L = 1, K >*/
                    i__3 = *k;
                    for (l = 1; l <= i__3; ++l) {
/*<                      TEMP = TEMP + A( L, I )*B( J, L ) >*/
                        temp += a[l + i__ * a_dim1] * b[j + l * b_dim1];
/*<   180             CONTINUE >*/
/* L180: */
                    }
/*<                   IF( BETA.EQ.ZERO )THEN >*/
                    if (*beta == 0.) {
/*<                      C( I, J ) = ALPHA*TEMP >*/
                        c__[i__ + j * c_dim1] = *alpha * temp;
/*<                   ELSE >*/
                    } else {
/*<                      C( I, J ) = ALPHA*TEMP + BETA*C( I, J ) >*/
                        c__[i__ + j * c_dim1] = *alpha * temp + *beta * c__[
                                i__ + j * c_dim1];
/*<                   END IF >*/
                    }
/*<   190          CONTINUE >*/
/* L190: */
                }
/*<   200       CONTINUE >*/
/* L200: */
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of DGEMM . */

/*<       END >*/
} /* dgemm_ */

#ifdef __cplusplus
        }
#endif
