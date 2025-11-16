/* lapack/complex16/ztrsyl.f -- translated by f2c (version 20090411).
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

/*<    >*/
/* Subroutine */ int ztrsyl_(char *trana, char *tranb, integer *isgn, integer
        *m, integer *n, doublecomplex *a, integer *lda, doublecomplex *b,
        integer *ldb, doublecomplex *c__, integer *ldc, doublereal *scale,
        integer *info, ftnlen trana_len, ftnlen tranb_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, i__1, i__2,
            i__3, i__4;
    doublereal d__1, d__2;
    doublecomplex z__1, z__2, z__3, z__4;

    /* Builtin functions */
    double d_imag(doublecomplex *);
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    integer j, k, l;
    doublecomplex a11;
    doublereal db;
    doublecomplex x11;
    doublereal da11;
    doublecomplex vec;
    doublereal dum[1], eps, sgn, smin;
    doublecomplex suml, sumr;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Double Complex */ VOID zdotc_(doublecomplex *, integer *,
            doublecomplex *, integer *, doublecomplex *, integer *), zdotu_(
            doublecomplex *, integer *, doublecomplex *, integer *,
            doublecomplex *, integer *);
    extern /* Subroutine */ int dlabad_(doublereal *, doublereal *);
    extern doublereal dlamch_(char *, ftnlen);
    doublereal scaloc;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    extern doublereal zlange_(char *, integer *, integer *, doublecomplex *,
            integer *, doublereal *, ftnlen);
    doublereal bignum;
    extern /* Subroutine */ int zdscal_(integer *, doublereal *,
            doublecomplex *, integer *);
    extern /* Double Complex */ VOID zladiv_(doublecomplex *, doublecomplex *,
             doublecomplex *);
    logical notrna, notrnb;
    doublereal smlnum;
    (void)trana_len;
    (void)tranb_len;

/*  -- LAPACK routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          TRANA, TRANB >*/
/*<       INTEGER            INFO, ISGN, LDA, LDB, LDC, M, N >*/
/*<       DOUBLE PRECISION   SCALE >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         A( LDA, * ), B( LDB, * ), C( LDC, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZTRSYL solves the complex Sylvester matrix equation: */

/*     op(A)*X + X*op(B) = scale*C or */
/*     op(A)*X - X*op(B) = scale*C, */

/*  where op(A) = A or A**H, and A and B are both upper triangular. A is */
/*  M-by-M and B is N-by-N; the right hand side C and the solution X are */
/*  M-by-N; and scale is an output scale factor, set <= 1 to avoid */
/*  overflow in X. */

/*  Arguments */
/*  ========= */

/*  TRANA   (input) CHARACTER*1 */
/*          Specifies the option op(A): */
/*          = 'N': op(A) = A    (No transpose) */
/*          = 'C': op(A) = A**H (Conjugate transpose) */

/*  TRANB   (input) CHARACTER*1 */
/*          Specifies the option op(B): */
/*          = 'N': op(B) = B    (No transpose) */
/*          = 'C': op(B) = B**H (Conjugate transpose) */

/*  ISGN    (input) INTEGER */
/*          Specifies the sign in the equation: */
/*          = +1: solve op(A)*X + X*op(B) = scale*C */
/*          = -1: solve op(A)*X - X*op(B) = scale*C */

/*  M       (input) INTEGER */
/*          The order of the matrix A, and the number of rows in the */
/*          matrices X and C. M >= 0. */

/*  N       (input) INTEGER */
/*          The order of the matrix B, and the number of columns in the */
/*          matrices X and C. N >= 0. */

/*  A       (input) COMPLEX*16 array, dimension (LDA,M) */
/*          The upper triangular matrix A. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A. LDA >= max(1,M). */

/*  B       (input) COMPLEX*16 array, dimension (LDB,N) */
/*          The upper triangular matrix B. */

/*  LDB     (input) INTEGER */
/*          The leading dimension of the array B. LDB >= max(1,N). */

/*  C       (input/output) COMPLEX*16 array, dimension (LDC,N) */
/*          On entry, the M-by-N right hand side matrix C. */
/*          On exit, C is overwritten by the solution matrix X. */

/*  LDC     (input) INTEGER */
/*          The leading dimension of the array C. LDC >= max(1,M) */

/*  SCALE   (output) DOUBLE PRECISION */
/*          The scale factor, scale, set <= 1 to avoid overflow in X. */

/*  INFO    (output) INTEGER */
/*          = 0: successful exit */
/*          < 0: if INFO = -i, the i-th argument had an illegal value */
/*          = 1: A and B have common or very close eigenvalues; perturbed */
/*               values were used to solve the equation (but the matrices */
/*               A and B are unchanged). */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ONE >*/
/*<       PARAMETER          ( ONE = 1.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            NOTRNA, NOTRNB >*/
/*<       INTEGER            J, K, L >*/
/*<    >*/
/*<       COMPLEX*16         A11, SUML, SUMR, VEC, X11 >*/
/*     .. */
/*     .. Local Arrays .. */
/*<       DOUBLE PRECISION   DUM( 1 ) >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       DOUBLE PRECISION   DLAMCH, ZLANGE >*/
/*<       COMPLEX*16         ZDOTC, ZDOTU, ZLADIV >*/
/*<       EXTERNAL           LSAME, DLAMCH, ZLANGE, ZDOTC, ZDOTU, ZLADIV >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DLABAD, XERBLA, ZDSCAL >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, DBLE, DCMPLX, DCONJG, DIMAG, MAX, MIN >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Decode and Test input parameters */

/*<       NOTRNA = LSAME( TRANA, 'N' ) >*/
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
    notrna = lsame_(trana, "N", (ftnlen)1, (ftnlen)1);
/*<       NOTRNB = LSAME( TRANB, 'N' ) >*/
    notrnb = lsame_(tranb, "N", (ftnlen)1, (ftnlen)1);

/*<       INFO = 0 >*/
    *info = 0;
/*<       IF( .NOT.NOTRNA .AND. .NOT.LSAME( TRANA, 'C' ) ) THEN >*/
    if (! notrna && ! lsame_(trana, "C", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( .NOT.NOTRNB .AND. .NOT.LSAME( TRANB, 'C' ) ) THEN >*/
    } else if (! notrnb && ! lsame_(tranb, "C", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( ISGN.NE.1 .AND. ISGN.NE.-1 ) THEN >*/
    } else if (*isgn != 1 && *isgn != -1) {
/*<          INFO = -3 >*/
        *info = -3;
/*<       ELSE IF( M.LT.0 ) THEN >*/
    } else if (*m < 0) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       ELSE IF( LDA.LT.MAX( 1, M ) ) THEN >*/
    } else if (*lda < max(1,*m)) {
/*<          INFO = -7 >*/
        *info = -7;
/*<       ELSE IF( LDB.LT.MAX( 1, N ) ) THEN >*/
    } else if (*ldb < max(1,*n)) {
/*<          INFO = -9 >*/
        *info = -9;
/*<       ELSE IF( LDC.LT.MAX( 1, M ) ) THEN >*/
    } else if (*ldc < max(1,*m)) {
/*<          INFO = -11 >*/
        *info = -11;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZTRSYL', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZTRSYL", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<       SCALE = ONE >*/
    *scale = 1.;
/*<    >*/
    if (*m == 0 || *n == 0) {
        return 0;
    }

/*     Set constants to control overflow */

/*<       EPS = DLAMCH( 'P' ) >*/
    eps = dlamch_("P", (ftnlen)1);
/*<       SMLNUM = DLAMCH( 'S' ) >*/
    smlnum = dlamch_("S", (ftnlen)1);
/*<       BIGNUM = ONE / SMLNUM >*/
    bignum = 1. / smlnum;
/*<       CALL DLABAD( SMLNUM, BIGNUM ) >*/
    dlabad_(&smlnum, &bignum);
/*<       SMLNUM = SMLNUM*DBLE( M*N ) / EPS >*/
    smlnum = smlnum * (doublereal) (*m * *n) / eps;
/*<       BIGNUM = ONE / SMLNUM >*/
    bignum = 1. / smlnum;
/*<    >*/
/* Computing MAX */
    d__1 = smlnum, d__2 = eps * zlange_("M", m, m, &a[a_offset], lda, dum, (
            ftnlen)1), d__1 = max(d__1,d__2), d__2 = eps * zlange_("M", n, n,
            &b[b_offset], ldb, dum, (ftnlen)1);
    smin = max(d__1,d__2);
/*<       SGN = ISGN >*/
    sgn = (doublereal) (*isgn);

/*<       IF( NOTRNA .AND. NOTRNB ) THEN >*/
    if (notrna && notrnb) {

/*        Solve    A*X + ISGN*X*B = scale*C. */

/*        The (K,L)th block of X is determined starting from */
/*        bottom-left corner column by column by */

/*            A(K,K)*X(K,L) + ISGN*X(K,L)*B(L,L) = C(K,L) - R(K,L) */

/*        Where */
/*                    M                        L-1 */
/*          R(K,L) = SUM [A(K,I)*X(I,L)] +ISGN*SUM [X(K,J)*B(J,L)]. */
/*                  I=K+1                      J=1 */

/*<          DO 30 L = 1, N >*/
        i__1 = *n;
        for (l = 1; l <= i__1; ++l) {
/*<             DO 20 K = M, 1, -1 >*/
            for (k = *m; k >= 1; --k) {

/*<    >*/
                i__2 = *m - k;
/* Computing MIN */
                i__3 = k + 1;
/* Computing MIN */
                i__4 = k + 1;
                zdotu_(&z__1, &i__2, &a[k + min(i__3,*m) * a_dim1], lda, &c__[
                        min(i__4,*m) + l * c_dim1], &c__1);
                suml.r = z__1.r, suml.i = z__1.i;
/*<                SUMR = ZDOTU( L-1, C( K, 1 ), LDC, B( 1, L ), 1 ) >*/
                i__2 = l - 1;
                zdotu_(&z__1, &i__2, &c__[k + c_dim1], ldc, &b[l * b_dim1 + 1]
                        , &c__1);
                sumr.r = z__1.r, sumr.i = z__1.i;
/*<                VEC = C( K, L ) - ( SUML+SGN*SUMR ) >*/
                i__2 = k + l * c_dim1;
                z__3.r = sgn * sumr.r, z__3.i = sgn * sumr.i;
                z__2.r = suml.r + z__3.r, z__2.i = suml.i + z__3.i;
                z__1.r = c__[i__2].r - z__2.r, z__1.i = c__[i__2].i - z__2.i;
                vec.r = z__1.r, vec.i = z__1.i;

/*<                SCALOC = ONE >*/
                scaloc = 1.;
/*<                A11 = A( K, K ) + SGN*B( L, L ) >*/
                i__2 = k + k * a_dim1;
                i__3 = l + l * b_dim1;
                z__2.r = sgn * b[i__3].r, z__2.i = sgn * b[i__3].i;
                z__1.r = a[i__2].r + z__2.r, z__1.i = a[i__2].i + z__2.i;
                a11.r = z__1.r, a11.i = z__1.i;
/*<                DA11 = ABS( DBLE( A11 ) ) + ABS( DIMAG( A11 ) ) >*/
                da11 = (d__1 = a11.r, abs(d__1)) + (d__2 = d_imag(&a11), abs(
                        d__2));
/*<                IF( DA11.LE.SMIN ) THEN >*/
                if (da11 <= smin) {
/*<                   A11 = SMIN >*/
                    a11.r = smin, a11.i = 0.;
/*<                   DA11 = SMIN >*/
                    da11 = smin;
/*<                   INFO = 1 >*/
                    *info = 1;
/*<                END IF >*/
                }
/*<                DB = ABS( DBLE( VEC ) ) + ABS( DIMAG( VEC ) ) >*/
                db = (d__1 = vec.r, abs(d__1)) + (d__2 = d_imag(&vec), abs(
                        d__2));
/*<                IF( DA11.LT.ONE .AND. DB.GT.ONE ) THEN >*/
                if (da11 < 1. && db > 1.) {
/*<    >*/
                    if (db > bignum * da11) {
                        scaloc = 1. / db;
                    }
/*<                END IF >*/
                }
/*<                X11 = ZLADIV( VEC*DCMPLX( SCALOC ), A11 ) >*/
                z__3.r = scaloc, z__3.i = 0.;
                z__2.r = vec.r * z__3.r - vec.i * z__3.i, z__2.i = vec.r *
                        z__3.i + vec.i * z__3.r;
                zladiv_(&z__1, &z__2, &a11);
                x11.r = z__1.r, x11.i = z__1.i;

/*<                IF( SCALOC.NE.ONE ) THEN >*/
                if (scaloc != 1.) {
/*<                   DO 10 J = 1, N >*/
                    i__2 = *n;
                    for (j = 1; j <= i__2; ++j) {
/*<                      CALL ZDSCAL( M, SCALOC, C( 1, J ), 1 ) >*/
                        zdscal_(m, &scaloc, &c__[j * c_dim1 + 1], &c__1);
/*<    10             CONTINUE >*/
/* L10: */
                    }
/*<                   SCALE = SCALE*SCALOC >*/
                    *scale *= scaloc;
/*<                END IF >*/
                }
/*<                C( K, L ) = X11 >*/
                i__2 = k + l * c_dim1;
                c__[i__2].r = x11.r, c__[i__2].i = x11.i;

/*<    20       CONTINUE >*/
/* L20: */
            }
/*<    30    CONTINUE >*/
/* L30: */
        }

/*<       ELSE IF( .NOT.NOTRNA .AND. NOTRNB ) THEN >*/
    } else if (! notrna && notrnb) {

/*        Solve    A' *X + ISGN*X*B = scale*C. */

/*        The (K,L)th block of X is determined starting from */
/*        upper-left corner column by column by */

/*            A'(K,K)*X(K,L) + ISGN*X(K,L)*B(L,L) = C(K,L) - R(K,L) */

/*        Where */
/*                   K-1                         L-1 */
/*          R(K,L) = SUM [A'(I,K)*X(I,L)] + ISGN*SUM [X(K,J)*B(J,L)] */
/*                   I=1                         J=1 */

/*<          DO 60 L = 1, N >*/
        i__1 = *n;
        for (l = 1; l <= i__1; ++l) {
/*<             DO 50 K = 1, M >*/
            i__2 = *m;
            for (k = 1; k <= i__2; ++k) {

/*<                SUML = ZDOTC( K-1, A( 1, K ), 1, C( 1, L ), 1 ) >*/
                i__3 = k - 1;
                zdotc_(&z__1, &i__3, &a[k * a_dim1 + 1], &c__1, &c__[l *
                        c_dim1 + 1], &c__1);
                suml.r = z__1.r, suml.i = z__1.i;
/*<                SUMR = ZDOTU( L-1, C( K, 1 ), LDC, B( 1, L ), 1 ) >*/
                i__3 = l - 1;
                zdotu_(&z__1, &i__3, &c__[k + c_dim1], ldc, &b[l * b_dim1 + 1]
                        , &c__1);
                sumr.r = z__1.r, sumr.i = z__1.i;
/*<                VEC = C( K, L ) - ( SUML+SGN*SUMR ) >*/
                i__3 = k + l * c_dim1;
                z__3.r = sgn * sumr.r, z__3.i = sgn * sumr.i;
                z__2.r = suml.r + z__3.r, z__2.i = suml.i + z__3.i;
                z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
                vec.r = z__1.r, vec.i = z__1.i;

/*<                SCALOC = ONE >*/
                scaloc = 1.;
/*<                A11 = DCONJG( A( K, K ) ) + SGN*B( L, L ) >*/
                d_cnjg(&z__2, &a[k + k * a_dim1]);
                i__3 = l + l * b_dim1;
                z__3.r = sgn * b[i__3].r, z__3.i = sgn * b[i__3].i;
                z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
                a11.r = z__1.r, a11.i = z__1.i;
/*<                DA11 = ABS( DBLE( A11 ) ) + ABS( DIMAG( A11 ) ) >*/
                da11 = (d__1 = a11.r, abs(d__1)) + (d__2 = d_imag(&a11), abs(
                        d__2));
/*<                IF( DA11.LE.SMIN ) THEN >*/
                if (da11 <= smin) {
/*<                   A11 = SMIN >*/
                    a11.r = smin, a11.i = 0.;
/*<                   DA11 = SMIN >*/
                    da11 = smin;
/*<                   INFO = 1 >*/
                    *info = 1;
/*<                END IF >*/
                }
/*<                DB = ABS( DBLE( VEC ) ) + ABS( DIMAG( VEC ) ) >*/
                db = (d__1 = vec.r, abs(d__1)) + (d__2 = d_imag(&vec), abs(
                        d__2));
/*<                IF( DA11.LT.ONE .AND. DB.GT.ONE ) THEN >*/
                if (da11 < 1. && db > 1.) {
/*<    >*/
                    if (db > bignum * da11) {
                        scaloc = 1. / db;
                    }
/*<                END IF >*/
                }

/*<                X11 = ZLADIV( VEC*DCMPLX( SCALOC ), A11 ) >*/
                z__3.r = scaloc, z__3.i = 0.;
                z__2.r = vec.r * z__3.r - vec.i * z__3.i, z__2.i = vec.r *
                        z__3.i + vec.i * z__3.r;
                zladiv_(&z__1, &z__2, &a11);
                x11.r = z__1.r, x11.i = z__1.i;

/*<                IF( SCALOC.NE.ONE ) THEN >*/
                if (scaloc != 1.) {
/*<                   DO 40 J = 1, N >*/
                    i__3 = *n;
                    for (j = 1; j <= i__3; ++j) {
/*<                      CALL ZDSCAL( M, SCALOC, C( 1, J ), 1 ) >*/
                        zdscal_(m, &scaloc, &c__[j * c_dim1 + 1], &c__1);
/*<    40             CONTINUE >*/
/* L40: */
                    }
/*<                   SCALE = SCALE*SCALOC >*/
                    *scale *= scaloc;
/*<                END IF >*/
                }
/*<                C( K, L ) = X11 >*/
                i__3 = k + l * c_dim1;
                c__[i__3].r = x11.r, c__[i__3].i = x11.i;

/*<    50       CONTINUE >*/
/* L50: */
            }
/*<    60    CONTINUE >*/
/* L60: */
        }

/*<       ELSE IF( .NOT.NOTRNA .AND. .NOT.NOTRNB ) THEN >*/
    } else if (! notrna && ! notrnb) {

/*        Solve    A'*X + ISGN*X*B' = C. */

/*        The (K,L)th block of X is determined starting from */
/*        upper-right corner column by column by */

/*            A'(K,K)*X(K,L) + ISGN*X(K,L)*B'(L,L) = C(K,L) - R(K,L) */

/*        Where */
/*                    K-1 */
/*           R(K,L) = SUM [A'(I,K)*X(I,L)] + */
/*                    I=1 */
/*                           N */
/*                     ISGN*SUM [X(K,J)*B'(L,J)]. */
/*                          J=L+1 */

/*<          DO 90 L = N, 1, -1 >*/
        for (l = *n; l >= 1; --l) {
/*<             DO 80 K = 1, M >*/
            i__1 = *m;
            for (k = 1; k <= i__1; ++k) {

/*<                SUML = ZDOTC( K-1, A( 1, K ), 1, C( 1, L ), 1 ) >*/
                i__2 = k - 1;
                zdotc_(&z__1, &i__2, &a[k * a_dim1 + 1], &c__1, &c__[l *
                        c_dim1 + 1], &c__1);
                suml.r = z__1.r, suml.i = z__1.i;
/*<    >*/
                i__2 = *n - l;
/* Computing MIN */
                i__3 = l + 1;
/* Computing MIN */
                i__4 = l + 1;
                zdotc_(&z__1, &i__2, &c__[k + min(i__3,*n) * c_dim1], ldc, &b[
                        l + min(i__4,*n) * b_dim1], ldb);
                sumr.r = z__1.r, sumr.i = z__1.i;
/*<                VEC = C( K, L ) - ( SUML+SGN*DCONJG( SUMR ) ) >*/
                i__2 = k + l * c_dim1;
                d_cnjg(&z__4, &sumr);
                z__3.r = sgn * z__4.r, z__3.i = sgn * z__4.i;
                z__2.r = suml.r + z__3.r, z__2.i = suml.i + z__3.i;
                z__1.r = c__[i__2].r - z__2.r, z__1.i = c__[i__2].i - z__2.i;
                vec.r = z__1.r, vec.i = z__1.i;

/*<                SCALOC = ONE >*/
                scaloc = 1.;
/*<                A11 = DCONJG( A( K, K )+SGN*B( L, L ) ) >*/
                i__2 = k + k * a_dim1;
                i__3 = l + l * b_dim1;
                z__3.r = sgn * b[i__3].r, z__3.i = sgn * b[i__3].i;
                z__2.r = a[i__2].r + z__3.r, z__2.i = a[i__2].i + z__3.i;
                d_cnjg(&z__1, &z__2);
                a11.r = z__1.r, a11.i = z__1.i;
/*<                DA11 = ABS( DBLE( A11 ) ) + ABS( DIMAG( A11 ) ) >*/
                da11 = (d__1 = a11.r, abs(d__1)) + (d__2 = d_imag(&a11), abs(
                        d__2));
/*<                IF( DA11.LE.SMIN ) THEN >*/
                if (da11 <= smin) {
/*<                   A11 = SMIN >*/
                    a11.r = smin, a11.i = 0.;
/*<                   DA11 = SMIN >*/
                    da11 = smin;
/*<                   INFO = 1 >*/
                    *info = 1;
/*<                END IF >*/
                }
/*<                DB = ABS( DBLE( VEC ) ) + ABS( DIMAG( VEC ) ) >*/
                db = (d__1 = vec.r, abs(d__1)) + (d__2 = d_imag(&vec), abs(
                        d__2));
/*<                IF( DA11.LT.ONE .AND. DB.GT.ONE ) THEN >*/
                if (da11 < 1. && db > 1.) {
/*<    >*/
                    if (db > bignum * da11) {
                        scaloc = 1. / db;
                    }
/*<                END IF >*/
                }

/*<                X11 = ZLADIV( VEC*DCMPLX( SCALOC ), A11 ) >*/
                z__3.r = scaloc, z__3.i = 0.;
                z__2.r = vec.r * z__3.r - vec.i * z__3.i, z__2.i = vec.r *
                        z__3.i + vec.i * z__3.r;
                zladiv_(&z__1, &z__2, &a11);
                x11.r = z__1.r, x11.i = z__1.i;

/*<                IF( SCALOC.NE.ONE ) THEN >*/
                if (scaloc != 1.) {
/*<                   DO 70 J = 1, N >*/
                    i__2 = *n;
                    for (j = 1; j <= i__2; ++j) {
/*<                      CALL ZDSCAL( M, SCALOC, C( 1, J ), 1 ) >*/
                        zdscal_(m, &scaloc, &c__[j * c_dim1 + 1], &c__1);
/*<    70             CONTINUE >*/
/* L70: */
                    }
/*<                   SCALE = SCALE*SCALOC >*/
                    *scale *= scaloc;
/*<                END IF >*/
                }
/*<                C( K, L ) = X11 >*/
                i__2 = k + l * c_dim1;
                c__[i__2].r = x11.r, c__[i__2].i = x11.i;

/*<    80       CONTINUE >*/
/* L80: */
            }
/*<    90    CONTINUE >*/
/* L90: */
        }

/*<       ELSE IF( NOTRNA .AND. .NOT.NOTRNB ) THEN >*/
    } else if (notrna && ! notrnb) {

/*        Solve    A*X + ISGN*X*B' = C. */

/*        The (K,L)th block of X is determined starting from */
/*        bottom-left corner column by column by */

/*           A(K,K)*X(K,L) + ISGN*X(K,L)*B'(L,L) = C(K,L) - R(K,L) */

/*        Where */
/*                    M                          N */
/*          R(K,L) = SUM [A(K,I)*X(I,L)] + ISGN*SUM [X(K,J)*B'(L,J)] */
/*                  I=K+1                      J=L+1 */

/*<          DO 120 L = N, 1, -1 >*/
        for (l = *n; l >= 1; --l) {
/*<             DO 110 K = M, 1, -1 >*/
            for (k = *m; k >= 1; --k) {

/*<    >*/
                i__1 = *m - k;
/* Computing MIN */
                i__2 = k + 1;
/* Computing MIN */
                i__3 = k + 1;
                zdotu_(&z__1, &i__1, &a[k + min(i__2,*m) * a_dim1], lda, &c__[
                        min(i__3,*m) + l * c_dim1], &c__1);
                suml.r = z__1.r, suml.i = z__1.i;
/*<    >*/
                i__1 = *n - l;
/* Computing MIN */
                i__2 = l + 1;
/* Computing MIN */
                i__3 = l + 1;
                zdotc_(&z__1, &i__1, &c__[k + min(i__2,*n) * c_dim1], ldc, &b[
                        l + min(i__3,*n) * b_dim1], ldb);
                sumr.r = z__1.r, sumr.i = z__1.i;
/*<                VEC = C( K, L ) - ( SUML+SGN*DCONJG( SUMR ) ) >*/
                i__1 = k + l * c_dim1;
                d_cnjg(&z__4, &sumr);
                z__3.r = sgn * z__4.r, z__3.i = sgn * z__4.i;
                z__2.r = suml.r + z__3.r, z__2.i = suml.i + z__3.i;
                z__1.r = c__[i__1].r - z__2.r, z__1.i = c__[i__1].i - z__2.i;
                vec.r = z__1.r, vec.i = z__1.i;

/*<                SCALOC = ONE >*/
                scaloc = 1.;
/*<                A11 = A( K, K ) + SGN*DCONJG( B( L, L ) ) >*/
                i__1 = k + k * a_dim1;
                d_cnjg(&z__3, &b[l + l * b_dim1]);
                z__2.r = sgn * z__3.r, z__2.i = sgn * z__3.i;
                z__1.r = a[i__1].r + z__2.r, z__1.i = a[i__1].i + z__2.i;
                a11.r = z__1.r, a11.i = z__1.i;
/*<                DA11 = ABS( DBLE( A11 ) ) + ABS( DIMAG( A11 ) ) >*/
                da11 = (d__1 = a11.r, abs(d__1)) + (d__2 = d_imag(&a11), abs(
                        d__2));
/*<                IF( DA11.LE.SMIN ) THEN >*/
                if (da11 <= smin) {
/*<                   A11 = SMIN >*/
                    a11.r = smin, a11.i = 0.;
/*<                   DA11 = SMIN >*/
                    da11 = smin;
/*<                   INFO = 1 >*/
                    *info = 1;
/*<                END IF >*/
                }
/*<                DB = ABS( DBLE( VEC ) ) + ABS( DIMAG( VEC ) ) >*/
                db = (d__1 = vec.r, abs(d__1)) + (d__2 = d_imag(&vec), abs(
                        d__2));
/*<                IF( DA11.LT.ONE .AND. DB.GT.ONE ) THEN >*/
                if (da11 < 1. && db > 1.) {
/*<    >*/
                    if (db > bignum * da11) {
                        scaloc = 1. / db;
                    }
/*<                END IF >*/
                }

/*<                X11 = ZLADIV( VEC*DCMPLX( SCALOC ), A11 ) >*/
                z__3.r = scaloc, z__3.i = 0.;
                z__2.r = vec.r * z__3.r - vec.i * z__3.i, z__2.i = vec.r *
                        z__3.i + vec.i * z__3.r;
                zladiv_(&z__1, &z__2, &a11);
                x11.r = z__1.r, x11.i = z__1.i;

/*<                IF( SCALOC.NE.ONE ) THEN >*/
                if (scaloc != 1.) {
/*<                   DO 100 J = 1, N >*/
                    i__1 = *n;
                    for (j = 1; j <= i__1; ++j) {
/*<                      CALL ZDSCAL( M, SCALOC, C( 1, J ), 1 ) >*/
                        zdscal_(m, &scaloc, &c__[j * c_dim1 + 1], &c__1);
/*<   100             CONTINUE >*/
/* L100: */
                    }
/*<                   SCALE = SCALE*SCALOC >*/
                    *scale *= scaloc;
/*<                END IF >*/
                }
/*<                C( K, L ) = X11 >*/
                i__1 = k + l * c_dim1;
                c__[i__1].r = x11.r, c__[i__1].i = x11.i;

/*<   110       CONTINUE >*/
/* L110: */
            }
/*<   120    CONTINUE >*/
/* L120: */
        }

/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of ZTRSYL */

/*<       END >*/
} /* ztrsyl_ */

#ifdef __cplusplus
        }
#endif
