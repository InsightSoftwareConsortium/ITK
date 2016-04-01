/* blas/zgemm.f -- translated by f2c (version 20050501).
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
/* Subroutine */ int zgemm_(char *transa, char *transb, integer *m, integer *
        n, integer *k, doublecomplex *alpha, doublecomplex *a, integer *lda,
        doublecomplex *b, integer *ldb, doublecomplex *beta, doublecomplex *
        c__, integer *ldc, ftnlen transa_len, ftnlen transb_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, i__1, i__2,
            i__3, i__4, i__5, i__6;
    doublecomplex z__1, z__2, z__3, z__4;

    /* Builtin functions */
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    integer i__, j, l, info;
    logical nota, notb;
    doublecomplex temp;
    logical conja, conjb;
    integer ncola;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    integer nrowa, nrowb;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    (void)transa_len;
    (void)transb_len;

/*     .. Scalar Arguments .. */
/*<       CHARACTER*1        TRANSA, TRANSB >*/
/*<       INTEGER            M, N, K, LDA, LDB, LDC >*/
/*<       COMPLEX*16         ALPHA, BETA >*/
/*     .. Array Arguments .. */
/*<       COMPLEX*16         A( LDA, * ), B( LDB, * ), C( LDC, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZGEMM  performs one of the matrix-matrix operations */

/*     C := alpha*op( A )*op( B ) + beta*C, */

/*  where  op( X ) is one of */

/*     op( X ) = X   or   op( X ) = X'   or   op( X ) = conjg( X' ), */

/*  alpha and beta are scalars, and A, B and C are matrices, with op( A ) */
/*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix. */

/*  Parameters */
/*  ========== */

/*  TRANSA - CHARACTER*1. */
/*           On entry, TRANSA specifies the form of op( A ) to be used in */
/*           the matrix multiplication as follows: */

/*              TRANSA = 'N' or 'n',  op( A ) = A. */

/*              TRANSA = 'T' or 't',  op( A ) = A'. */

/*              TRANSA = 'C' or 'c',  op( A ) = conjg( A' ). */

/*           Unchanged on exit. */

/*  TRANSB - CHARACTER*1. */
/*           On entry, TRANSB specifies the form of op( B ) to be used in */
/*           the matrix multiplication as follows: */

/*              TRANSB = 'N' or 'n',  op( B ) = B. */

/*              TRANSB = 'T' or 't',  op( B ) = B'. */

/*              TRANSB = 'C' or 'c',  op( B ) = conjg( B' ). */

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

/*  ALPHA  - COMPLEX*16      . */
/*           On entry, ALPHA specifies the scalar alpha. */
/*           Unchanged on exit. */

/*  A      - COMPLEX*16       array of DIMENSION ( LDA, ka ), where ka is */
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

/*  B      - COMPLEX*16       array of DIMENSION ( LDB, kb ), where kb is */
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

/*  BETA   - COMPLEX*16      . */
/*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is */
/*           supplied as zero then C need not be set on input. */
/*           Unchanged on exit. */

/*  C      - COMPLEX*16       array of DIMENSION ( LDC, n ). */
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
/*<       INTRINSIC          DCONJG, MAX >*/
/*     .. Local Scalars .. */
/*<       LOGICAL            CONJA, CONJB, NOTA, NOTB >*/
/*<       INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB >*/
/*<       COMPLEX*16         TEMP >*/
/*     .. Parameters .. */
/*<       COMPLEX*16         ONE >*/
/*<       PARAMETER        ( ONE  = ( 1.0D+0, 0.0D+0 ) ) >*/
/*<       COMPLEX*16         ZERO >*/
/*<       PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) ) >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not */
/*     conjugated or transposed, set  CONJA and CONJB  as true if  A  and */
/*     B  respectively are to be  transposed but  not conjugated  and set */
/*     NROWA, NCOLA and  NROWB  as the number of rows and  columns  of  A */
/*     and the number of rows of  B  respectively. */

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
/*<       CONJA = LSAME( TRANSA, 'C' ) >*/
    conja = lsame_(transa, "C", (ftnlen)1, (ftnlen)1);
/*<       CONJB = LSAME( TRANSB, 'C' ) >*/
    conjb = lsame_(transb, "C", (ftnlen)1, (ftnlen)1);
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
    if (! nota && ! conja && ! lsame_(transa, "T", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = 1 >*/
        info = 1;
/*<    >*/
    } else if (! notb && ! conjb && ! lsame_(transb, "T", (ftnlen)1, (ftnlen)
            1)) {
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
/*<          CALL XERBLA( 'ZGEMM ', INFO ) >*/
        xerbla_("ZGEMM ", &info, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible. */

/*<    >*/
    if (*m == 0 || *n == 0 || (((alpha->r == 0. && alpha->i == 0.) || *k == 0) &&
             (beta->r == 1. && beta->i == 0.))) {
        return 0;
    }

/*     And when  alpha.eq.zero. */

/*<       IF( ALPHA.EQ.ZERO )THEN >*/
    if (alpha->r == 0. && alpha->i == 0.) {
/*<          IF( BETA.EQ.ZERO )THEN >*/
        if (beta->r == 0. && beta->i == 0.) {
/*<             DO 20, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                DO 10, I = 1, M >*/
                i__2 = *m;
                for (i__ = 1; i__ <= i__2; ++i__) {
/*<                   C( I, J ) = ZERO >*/
                    i__3 = i__ + j * c_dim1;
                    c__[i__3].r = 0., c__[i__3].i = 0.;
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
                    i__3 = i__ + j * c_dim1;
                    i__4 = i__ + j * c_dim1;
                    z__1.r = beta->r * c__[i__4].r - beta->i * c__[i__4].i,
                            z__1.i = beta->r * c__[i__4].i + beta->i * c__[
                            i__4].r;
                    c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
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
                if (beta->r == 0. && beta->i == 0.) {
/*<                   DO 50, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( I, J ) = ZERO >*/
                        i__3 = i__ + j * c_dim1;
                        c__[i__3].r = 0., c__[i__3].i = 0.;
/*<    50             CONTINUE >*/
/* L50: */
                    }
/*<                ELSE IF( BETA.NE.ONE )THEN >*/
                } else if (beta->r != 1. || beta->i != 0.) {
/*<                   DO 60, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( I, J ) = BETA*C( I, J ) >*/
                        i__3 = i__ + j * c_dim1;
                        i__4 = i__ + j * c_dim1;
                        z__1.r = beta->r * c__[i__4].r - beta->i * c__[i__4]
                                .i, z__1.i = beta->r * c__[i__4].i + beta->i *
                                 c__[i__4].r;
                        c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/*<    60             CONTINUE >*/
/* L60: */
                    }
/*<                END IF >*/
                }
/*<                DO 80, L = 1, K >*/
                i__2 = *k;
                for (l = 1; l <= i__2; ++l) {
/*<                   IF( B( L, J ).NE.ZERO )THEN >*/
                    i__3 = l + j * b_dim1;
                    if (b[i__3].r != 0. || b[i__3].i != 0.) {
/*<                      TEMP = ALPHA*B( L, J ) >*/
                        i__3 = l + j * b_dim1;
                        z__1.r = alpha->r * b[i__3].r - alpha->i * b[i__3].i,
                                z__1.i = alpha->r * b[i__3].i + alpha->i * b[
                                i__3].r;
                        temp.r = z__1.r, temp.i = z__1.i;
/*<                      DO 70, I = 1, M >*/
                        i__3 = *m;
                        for (i__ = 1; i__ <= i__3; ++i__) {
/*<                         C( I, J ) = C( I, J ) + TEMP*A( I, L ) >*/
                            i__4 = i__ + j * c_dim1;
                            i__5 = i__ + j * c_dim1;
                            i__6 = i__ + l * a_dim1;
                            z__2.r = temp.r * a[i__6].r - temp.i * a[i__6].i,
                                    z__2.i = temp.r * a[i__6].i + temp.i * a[
                                    i__6].r;
                            z__1.r = c__[i__5].r + z__2.r, z__1.i = c__[i__5]
                                    .i + z__2.i;
                            c__[i__4].r = z__1.r, c__[i__4].i = z__1.i;
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
/*<          ELSE IF( CONJA )THEN >*/
        } else if (conja) {

/*           Form  C := alpha*conjg( A' )*B + beta*C. */

/*<             DO 120, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                DO 110, I = 1, M >*/
                i__2 = *m;
                for (i__ = 1; i__ <= i__2; ++i__) {
/*<                   TEMP = ZERO >*/
                    temp.r = 0., temp.i = 0.;
/*<                   DO 100, L = 1, K >*/
                    i__3 = *k;
                    for (l = 1; l <= i__3; ++l) {
/*<                      TEMP = TEMP + DCONJG( A( L, I ) )*B( L, J ) >*/
                        d_cnjg(&z__3, &a[l + i__ * a_dim1]);
                        i__4 = l + j * b_dim1;
                        z__2.r = z__3.r * b[i__4].r - z__3.i * b[i__4].i,
                                z__2.i = z__3.r * b[i__4].i + z__3.i * b[i__4]
                                .r;
                        z__1.r = temp.r + z__2.r, z__1.i = temp.i + z__2.i;
                        temp.r = z__1.r, temp.i = z__1.i;
/*<   100             CONTINUE >*/
/* L100: */
                    }
/*<                   IF( BETA.EQ.ZERO )THEN >*/
                    if (beta->r == 0. && beta->i == 0.) {
/*<                      C( I, J ) = ALPHA*TEMP >*/
                        i__3 = i__ + j * c_dim1;
                        z__1.r = alpha->r * temp.r - alpha->i * temp.i,
                                z__1.i = alpha->r * temp.i + alpha->i *
                                temp.r;
                        c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/*<                   ELSE >*/
                    } else {
/*<                      C( I, J ) = ALPHA*TEMP + BETA*C( I, J ) >*/
                        i__3 = i__ + j * c_dim1;
                        z__2.r = alpha->r * temp.r - alpha->i * temp.i,
                                z__2.i = alpha->r * temp.i + alpha->i *
                                temp.r;
                        i__4 = i__ + j * c_dim1;
                        z__3.r = beta->r * c__[i__4].r - beta->i * c__[i__4]
                                .i, z__3.i = beta->r * c__[i__4].i + beta->i *
                                 c__[i__4].r;
                        z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
                        c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/*<                   END IF >*/
                    }
/*<   110          CONTINUE >*/
/* L110: */
                }
/*<   120       CONTINUE >*/
/* L120: */
            }
/*<          ELSE >*/
        } else {

/*           Form  C := alpha*A'*B + beta*C */

/*<             DO 150, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                DO 140, I = 1, M >*/
                i__2 = *m;
                for (i__ = 1; i__ <= i__2; ++i__) {
/*<                   TEMP = ZERO >*/
                    temp.r = 0., temp.i = 0.;
/*<                   DO 130, L = 1, K >*/
                    i__3 = *k;
                    for (l = 1; l <= i__3; ++l) {
/*<                      TEMP = TEMP + A( L, I )*B( L, J ) >*/
                        i__4 = l + i__ * a_dim1;
                        i__5 = l + j * b_dim1;
                        z__2.r = a[i__4].r * b[i__5].r - a[i__4].i * b[i__5]
                                .i, z__2.i = a[i__4].r * b[i__5].i + a[i__4]
                                .i * b[i__5].r;
                        z__1.r = temp.r + z__2.r, z__1.i = temp.i + z__2.i;
                        temp.r = z__1.r, temp.i = z__1.i;
/*<   130             CONTINUE >*/
/* L130: */
                    }
/*<                   IF( BETA.EQ.ZERO )THEN >*/
                    if (beta->r == 0. && beta->i == 0.) {
/*<                      C( I, J ) = ALPHA*TEMP >*/
                        i__3 = i__ + j * c_dim1;
                        z__1.r = alpha->r * temp.r - alpha->i * temp.i,
                                z__1.i = alpha->r * temp.i + alpha->i *
                                temp.r;
                        c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/*<                   ELSE >*/
                    } else {
/*<                      C( I, J ) = ALPHA*TEMP + BETA*C( I, J ) >*/
                        i__3 = i__ + j * c_dim1;
                        z__2.r = alpha->r * temp.r - alpha->i * temp.i,
                                z__2.i = alpha->r * temp.i + alpha->i *
                                temp.r;
                        i__4 = i__ + j * c_dim1;
                        z__3.r = beta->r * c__[i__4].r - beta->i * c__[i__4]
                                .i, z__3.i = beta->r * c__[i__4].i + beta->i *
                                 c__[i__4].r;
                        z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
                        c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/*<                   END IF >*/
                    }
/*<   140          CONTINUE >*/
/* L140: */
                }
/*<   150       CONTINUE >*/
/* L150: */
            }
/*<          END IF >*/
        }
/*<       ELSE IF( NOTA )THEN >*/
    } else if (nota) {
/*<          IF( CONJB )THEN >*/
        if (conjb) {

/*           Form  C := alpha*A*conjg( B' ) + beta*C. */

/*<             DO 200, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                IF( BETA.EQ.ZERO )THEN >*/
                if (beta->r == 0. && beta->i == 0.) {
/*<                   DO 160, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( I, J ) = ZERO >*/
                        i__3 = i__ + j * c_dim1;
                        c__[i__3].r = 0., c__[i__3].i = 0.;
/*<   160             CONTINUE >*/
/* L160: */
                    }
/*<                ELSE IF( BETA.NE.ONE )THEN >*/
                } else if (beta->r != 1. || beta->i != 0.) {
/*<                   DO 170, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( I, J ) = BETA*C( I, J ) >*/
                        i__3 = i__ + j * c_dim1;
                        i__4 = i__ + j * c_dim1;
                        z__1.r = beta->r * c__[i__4].r - beta->i * c__[i__4]
                                .i, z__1.i = beta->r * c__[i__4].i + beta->i *
                                 c__[i__4].r;
                        c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/*<   170             CONTINUE >*/
/* L170: */
                    }
/*<                END IF >*/
                }
/*<                DO 190, L = 1, K >*/
                i__2 = *k;
                for (l = 1; l <= i__2; ++l) {
/*<                   IF( B( J, L ).NE.ZERO )THEN >*/
                    i__3 = j + l * b_dim1;
                    if (b[i__3].r != 0. || b[i__3].i != 0.) {
/*<                      TEMP = ALPHA*DCONJG( B( J, L ) ) >*/
                        d_cnjg(&z__2, &b[j + l * b_dim1]);
                        z__1.r = alpha->r * z__2.r - alpha->i * z__2.i,
                                z__1.i = alpha->r * z__2.i + alpha->i *
                                z__2.r;
                        temp.r = z__1.r, temp.i = z__1.i;
/*<                      DO 180, I = 1, M >*/
                        i__3 = *m;
                        for (i__ = 1; i__ <= i__3; ++i__) {
/*<                         C( I, J ) = C( I, J ) + TEMP*A( I, L ) >*/
                            i__4 = i__ + j * c_dim1;
                            i__5 = i__ + j * c_dim1;
                            i__6 = i__ + l * a_dim1;
                            z__2.r = temp.r * a[i__6].r - temp.i * a[i__6].i,
                                    z__2.i = temp.r * a[i__6].i + temp.i * a[
                                    i__6].r;
                            z__1.r = c__[i__5].r + z__2.r, z__1.i = c__[i__5]
                                    .i + z__2.i;
                            c__[i__4].r = z__1.r, c__[i__4].i = z__1.i;
/*<   180                CONTINUE >*/
/* L180: */
                        }
/*<                   END IF >*/
                    }
/*<   190          CONTINUE >*/
/* L190: */
                }
/*<   200       CONTINUE >*/
/* L200: */
            }
/*<          ELSE >*/
        } else {

/*           Form  C := alpha*A*B'          + beta*C */

/*<             DO 250, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                IF( BETA.EQ.ZERO )THEN >*/
                if (beta->r == 0. && beta->i == 0.) {
/*<                   DO 210, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( I, J ) = ZERO >*/
                        i__3 = i__ + j * c_dim1;
                        c__[i__3].r = 0., c__[i__3].i = 0.;
/*<   210             CONTINUE >*/
/* L210: */
                    }
/*<                ELSE IF( BETA.NE.ONE )THEN >*/
                } else if (beta->r != 1. || beta->i != 0.) {
/*<                   DO 220, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( I, J ) = BETA*C( I, J ) >*/
                        i__3 = i__ + j * c_dim1;
                        i__4 = i__ + j * c_dim1;
                        z__1.r = beta->r * c__[i__4].r - beta->i * c__[i__4]
                                .i, z__1.i = beta->r * c__[i__4].i + beta->i *
                                 c__[i__4].r;
                        c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/*<   220             CONTINUE >*/
/* L220: */
                    }
/*<                END IF >*/
                }
/*<                DO 240, L = 1, K >*/
                i__2 = *k;
                for (l = 1; l <= i__2; ++l) {
/*<                   IF( B( J, L ).NE.ZERO )THEN >*/
                    i__3 = j + l * b_dim1;
                    if (b[i__3].r != 0. || b[i__3].i != 0.) {
/*<                      TEMP = ALPHA*B( J, L ) >*/
                        i__3 = j + l * b_dim1;
                        z__1.r = alpha->r * b[i__3].r - alpha->i * b[i__3].i,
                                z__1.i = alpha->r * b[i__3].i + alpha->i * b[
                                i__3].r;
                        temp.r = z__1.r, temp.i = z__1.i;
/*<                      DO 230, I = 1, M >*/
                        i__3 = *m;
                        for (i__ = 1; i__ <= i__3; ++i__) {
/*<                         C( I, J ) = C( I, J ) + TEMP*A( I, L ) >*/
                            i__4 = i__ + j * c_dim1;
                            i__5 = i__ + j * c_dim1;
                            i__6 = i__ + l * a_dim1;
                            z__2.r = temp.r * a[i__6].r - temp.i * a[i__6].i,
                                    z__2.i = temp.r * a[i__6].i + temp.i * a[
                                    i__6].r;
                            z__1.r = c__[i__5].r + z__2.r, z__1.i = c__[i__5]
                                    .i + z__2.i;
                            c__[i__4].r = z__1.r, c__[i__4].i = z__1.i;
/*<   230                CONTINUE >*/
/* L230: */
                        }
/*<                   END IF >*/
                    }
/*<   240          CONTINUE >*/
/* L240: */
                }
/*<   250       CONTINUE >*/
/* L250: */
            }
/*<          END IF >*/
        }
/*<       ELSE IF( CONJA )THEN >*/
    } else if (conja) {
/*<          IF( CONJB )THEN >*/
        if (conjb) {

/*           Form  C := alpha*conjg( A' )*conjg( B' ) + beta*C. */

/*<             DO 280, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                DO 270, I = 1, M >*/
                i__2 = *m;
                for (i__ = 1; i__ <= i__2; ++i__) {
/*<                   TEMP = ZERO >*/
                    temp.r = 0., temp.i = 0.;
/*<                   DO 260, L = 1, K >*/
                    i__3 = *k;
                    for (l = 1; l <= i__3; ++l) {
/*<    >*/
                        d_cnjg(&z__3, &a[l + i__ * a_dim1]);
                        d_cnjg(&z__4, &b[j + l * b_dim1]);
                        z__2.r = z__3.r * z__4.r - z__3.i * z__4.i, z__2.i =
                                z__3.r * z__4.i + z__3.i * z__4.r;
                        z__1.r = temp.r + z__2.r, z__1.i = temp.i + z__2.i;
                        temp.r = z__1.r, temp.i = z__1.i;
/*<   260             CONTINUE >*/
/* L260: */
                    }
/*<                   IF( BETA.EQ.ZERO )THEN >*/
                    if (beta->r == 0. && beta->i == 0.) {
/*<                      C( I, J ) = ALPHA*TEMP >*/
                        i__3 = i__ + j * c_dim1;
                        z__1.r = alpha->r * temp.r - alpha->i * temp.i,
                                z__1.i = alpha->r * temp.i + alpha->i *
                                temp.r;
                        c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/*<                   ELSE >*/
                    } else {
/*<                      C( I, J ) = ALPHA*TEMP + BETA*C( I, J ) >*/
                        i__3 = i__ + j * c_dim1;
                        z__2.r = alpha->r * temp.r - alpha->i * temp.i,
                                z__2.i = alpha->r * temp.i + alpha->i *
                                temp.r;
                        i__4 = i__ + j * c_dim1;
                        z__3.r = beta->r * c__[i__4].r - beta->i * c__[i__4]
                                .i, z__3.i = beta->r * c__[i__4].i + beta->i *
                                 c__[i__4].r;
                        z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
                        c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/*<                   END IF >*/
                    }
/*<   270          CONTINUE >*/
/* L270: */
                }
/*<   280       CONTINUE >*/
/* L280: */
            }
/*<          ELSE >*/
        } else {

/*           Form  C := alpha*conjg( A' )*B' + beta*C */

/*<             DO 310, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                DO 300, I = 1, M >*/
                i__2 = *m;
                for (i__ = 1; i__ <= i__2; ++i__) {
/*<                   TEMP = ZERO >*/
                    temp.r = 0., temp.i = 0.;
/*<                   DO 290, L = 1, K >*/
                    i__3 = *k;
                    for (l = 1; l <= i__3; ++l) {
/*<                      TEMP = TEMP + DCONJG( A( L, I ) )*B( J, L ) >*/
                        d_cnjg(&z__3, &a[l + i__ * a_dim1]);
                        i__4 = j + l * b_dim1;
                        z__2.r = z__3.r * b[i__4].r - z__3.i * b[i__4].i,
                                z__2.i = z__3.r * b[i__4].i + z__3.i * b[i__4]
                                .r;
                        z__1.r = temp.r + z__2.r, z__1.i = temp.i + z__2.i;
                        temp.r = z__1.r, temp.i = z__1.i;
/*<   290             CONTINUE >*/
/* L290: */
                    }
/*<                   IF( BETA.EQ.ZERO )THEN >*/
                    if (beta->r == 0. && beta->i == 0.) {
/*<                      C( I, J ) = ALPHA*TEMP >*/
                        i__3 = i__ + j * c_dim1;
                        z__1.r = alpha->r * temp.r - alpha->i * temp.i,
                                z__1.i = alpha->r * temp.i + alpha->i *
                                temp.r;
                        c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/*<                   ELSE >*/
                    } else {
/*<                      C( I, J ) = ALPHA*TEMP + BETA*C( I, J ) >*/
                        i__3 = i__ + j * c_dim1;
                        z__2.r = alpha->r * temp.r - alpha->i * temp.i,
                                z__2.i = alpha->r * temp.i + alpha->i *
                                temp.r;
                        i__4 = i__ + j * c_dim1;
                        z__3.r = beta->r * c__[i__4].r - beta->i * c__[i__4]
                                .i, z__3.i = beta->r * c__[i__4].i + beta->i *
                                 c__[i__4].r;
                        z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
                        c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/*<                   END IF >*/
                    }
/*<   300          CONTINUE >*/
/* L300: */
                }
/*<   310       CONTINUE >*/
/* L310: */
            }
/*<          END IF >*/
        }
/*<       ELSE >*/
    } else {
/*<          IF( CONJB )THEN >*/
        if (conjb) {

/*           Form  C := alpha*A'*conjg( B' ) + beta*C */

/*<             DO 340, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                DO 330, I = 1, M >*/
                i__2 = *m;
                for (i__ = 1; i__ <= i__2; ++i__) {
/*<                   TEMP = ZERO >*/
                    temp.r = 0., temp.i = 0.;
/*<                   DO 320, L = 1, K >*/
                    i__3 = *k;
                    for (l = 1; l <= i__3; ++l) {
/*<                      TEMP = TEMP + A( L, I )*DCONJG( B( J, L ) ) >*/
                        i__4 = l + i__ * a_dim1;
                        d_cnjg(&z__3, &b[j + l * b_dim1]);
                        z__2.r = a[i__4].r * z__3.r - a[i__4].i * z__3.i,
                                z__2.i = a[i__4].r * z__3.i + a[i__4].i *
                                z__3.r;
                        z__1.r = temp.r + z__2.r, z__1.i = temp.i + z__2.i;
                        temp.r = z__1.r, temp.i = z__1.i;
/*<   320             CONTINUE >*/
/* L320: */
                    }
/*<                   IF( BETA.EQ.ZERO )THEN >*/
                    if (beta->r == 0. && beta->i == 0.) {
/*<                      C( I, J ) = ALPHA*TEMP >*/
                        i__3 = i__ + j * c_dim1;
                        z__1.r = alpha->r * temp.r - alpha->i * temp.i,
                                z__1.i = alpha->r * temp.i + alpha->i *
                                temp.r;
                        c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/*<                   ELSE >*/
                    } else {
/*<                      C( I, J ) = ALPHA*TEMP + BETA*C( I, J ) >*/
                        i__3 = i__ + j * c_dim1;
                        z__2.r = alpha->r * temp.r - alpha->i * temp.i,
                                z__2.i = alpha->r * temp.i + alpha->i *
                                temp.r;
                        i__4 = i__ + j * c_dim1;
                        z__3.r = beta->r * c__[i__4].r - beta->i * c__[i__4]
                                .i, z__3.i = beta->r * c__[i__4].i + beta->i *
                                 c__[i__4].r;
                        z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
                        c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/*<                   END IF >*/
                    }
/*<   330          CONTINUE >*/
/* L330: */
                }
/*<   340       CONTINUE >*/
/* L340: */
            }
/*<          ELSE >*/
        } else {

/*           Form  C := alpha*A'*B' + beta*C */

/*<             DO 370, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                DO 360, I = 1, M >*/
                i__2 = *m;
                for (i__ = 1; i__ <= i__2; ++i__) {
/*<                   TEMP = ZERO >*/
                    temp.r = 0., temp.i = 0.;
/*<                   DO 350, L = 1, K >*/
                    i__3 = *k;
                    for (l = 1; l <= i__3; ++l) {
/*<                      TEMP = TEMP + A( L, I )*B( J, L ) >*/
                        i__4 = l + i__ * a_dim1;
                        i__5 = j + l * b_dim1;
                        z__2.r = a[i__4].r * b[i__5].r - a[i__4].i * b[i__5]
                                .i, z__2.i = a[i__4].r * b[i__5].i + a[i__4]
                                .i * b[i__5].r;
                        z__1.r = temp.r + z__2.r, z__1.i = temp.i + z__2.i;
                        temp.r = z__1.r, temp.i = z__1.i;
/*<   350             CONTINUE >*/
/* L350: */
                    }
/*<                   IF( BETA.EQ.ZERO )THEN >*/
                    if (beta->r == 0. && beta->i == 0.) {
/*<                      C( I, J ) = ALPHA*TEMP >*/
                        i__3 = i__ + j * c_dim1;
                        z__1.r = alpha->r * temp.r - alpha->i * temp.i,
                                z__1.i = alpha->r * temp.i + alpha->i *
                                temp.r;
                        c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/*<                   ELSE >*/
                    } else {
/*<                      C( I, J ) = ALPHA*TEMP + BETA*C( I, J ) >*/
                        i__3 = i__ + j * c_dim1;
                        z__2.r = alpha->r * temp.r - alpha->i * temp.i,
                                z__2.i = alpha->r * temp.i + alpha->i *
                                temp.r;
                        i__4 = i__ + j * c_dim1;
                        z__3.r = beta->r * c__[i__4].r - beta->i * c__[i__4]
                                .i, z__3.i = beta->r * c__[i__4].i + beta->i *
                                 c__[i__4].r;
                        z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
                        c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/*<                   END IF >*/
                    }
/*<   360          CONTINUE >*/
/* L360: */
                }
/*<   370       CONTINUE >*/
/* L370: */
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of ZGEMM . */

/*<       END >*/
} /* zgemm_ */

#ifdef __cplusplus
        }
#endif
