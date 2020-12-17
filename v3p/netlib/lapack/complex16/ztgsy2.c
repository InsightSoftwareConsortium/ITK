/* lapack/complex16/ztgsy2.f -- translated by f2c (version 20090411).
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

static integer c__2 = 2;
static integer c__1 = 1;

/*<    >*/
/* Subroutine */ int ztgsy2_(char *trans, integer *ijob, integer *m, integer *
        n, doublecomplex *a, integer *lda, doublecomplex *b, integer *ldb,
        doublecomplex *c__, integer *ldc, doublecomplex *d__, integer *ldd,
        doublecomplex *e, integer *lde, doublecomplex *f, integer *ldf,
        doublereal *scale, doublereal *rdsum, doublereal *rdscal, integer *
        info, ftnlen trans_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, d_dim1,
            d_offset, e_dim1, e_offset, f_dim1, f_offset, i__1, i__2, i__3,
            i__4;
    doublecomplex z__1, z__2, z__3, z__4, z__5, z__6;

    /* Builtin functions */
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    integer i__, j, k;
    doublecomplex z__[4]        /* was [2][2] */, rhs[2];
    integer ierr, ipiv[2], jpiv[2];
    doublecomplex alpha;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int zscal_(integer *, doublecomplex *,
            doublecomplex *, integer *), zaxpy_(integer *, doublecomplex *,
            doublecomplex *, integer *, doublecomplex *, integer *), zgesc2_(
            integer *, doublecomplex *, integer *, doublecomplex *, integer *,
             integer *, doublereal *), zgetc2_(integer *, doublecomplex *,
            integer *, integer *, integer *, integer *);
    doublereal scaloc;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen), zlatdf_(
            integer *, integer *, doublecomplex *, integer *, doublecomplex *,
             doublereal *, doublereal *, integer *, integer *);
    logical notran;
    (void)trans_len;

/*  -- LAPACK auxiliary routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          TRANS >*/
/*<       INTEGER            IJOB, INFO, LDA, LDB, LDC, LDD, LDE, LDF, M, N >*/
/*<       DOUBLE PRECISION   RDSCAL, RDSUM, SCALE >*/
/*     .. */
/*     .. Array Arguments .. */
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZTGSY2 solves the generalized Sylvester equation */

/*              A * R - L * B = scale *   C               (1) */
/*              D * R - L * E = scale * F */

/*  using Level 1 and 2 BLAS, where R and L are unknown M-by-N matrices, */
/*  (A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M, */
/*  N-by-N and M-by-N, respectively. A, B, D and E are upper triangular */
/*  (i.e., (A,D) and (B,E) in generalized Schur form). */

/*  The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output */
/*  scaling factor chosen to avoid overflow. */

/*  In matrix notation solving equation (1) corresponds to solve */
/*  Zx = scale * b, where Z is defined as */

/*         Z = [ kron(In, A)  -kron(B', Im) ]             (2) */
/*             [ kron(In, D)  -kron(E', Im) ], */

/*  Ik is the identity matrix of size k and X' is the transpose of X. */
/*  kron(X, Y) is the Kronecker product between the matrices X and Y. */

/*  If TRANS = 'C', y in the conjugate transposed system Z'y = scale*b */
/*  is solved for, which is equivalent to solve for R and L in */

/*              A' * R  + D' * L   = scale *  C           (3) */
/*              R  * B' + L  * E'  = scale * -F */

/*  This case is used to compute an estimate of Dif[(A, D), (B, E)] = */
/*  = sigma_min(Z) using reverse communicaton with ZLACON. */

/*  ZTGSY2 also (IJOB >= 1) contributes to the computation in ZTGSYL */
/*  of an upper bound on the separation between to matrix pairs. Then */
/*  the input (A, D), (B, E) are sub-pencils of two matrix pairs in */
/*  ZTGSYL. */

/*  Arguments */
/*  ========= */

/*  TRANS   (input) CHARACTER*1 */
/*          = 'N', solve the generalized Sylvester equation (1). */
/*          = 'T': solve the 'transposed' system (3). */

/*  IJOB    (input) INTEGER */
/*          Specifies what kind of functionality to be performed. */
/*          =0: solve (1) only. */
/*          =1: A contribution from this subsystem to a Frobenius */
/*              norm-based estimate of the separation between two matrix */
/*              pairs is computed. (look ahead strategy is used). */
/*          =2: A contribution from this subsystem to a Frobenius */
/*              norm-based estimate of the separation between two matrix */
/*              pairs is computed. (DGECON on sub-systems is used.) */
/*          Not referenced if TRANS = 'T'. */

/*  M       (input) INTEGER */
/*          On entry, M specifies the order of A and D, and the row */
/*          dimension of C, F, R and L. */

/*  N       (input) INTEGER */
/*          On entry, N specifies the order of B and E, and the column */
/*          dimension of C, F, R and L. */

/*  A       (input) COMPLEX*16 array, dimension (LDA, M) */
/*          On entry, A contains an upper triangular matrix. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the matrix A. LDA >= max(1, M). */

/*  B       (input) COMPLEX*16 array, dimension (LDB, N) */
/*          On entry, B contains an upper triangular matrix. */

/*  LDB     (input) INTEGER */
/*          The leading dimension of the matrix B. LDB >= max(1, N). */

/*  C       (input/output) COMPLEX*16 array, dimension (LDC, N) */
/*          On entry, C contains the right-hand-side of the first matrix */
/*          equation in (1). */
/*          On exit, if IJOB = 0, C has been overwritten by the solution */
/*          R. */

/*  LDC     (input) INTEGER */
/*          The leading dimension of the matrix C. LDC >= max(1, M). */

/*  D       (input) COMPLEX*16 array, dimension (LDD, M) */
/*          On entry, D contains an upper triangular matrix. */

/*  LDD     (input) INTEGER */
/*          The leading dimension of the matrix D. LDD >= max(1, M). */

/*  E       (input) COMPLEX*16 array, dimension (LDE, N) */
/*          On entry, E contains an upper triangular matrix. */

/*  LDE     (input) INTEGER */
/*          The leading dimension of the matrix E. LDE >= max(1, N). */

/*  F       (input/output) COMPLEX*16 array, dimension (LDF, N) */
/*          On entry, F contains the right-hand-side of the second matrix */
/*          equation in (1). */
/*          On exit, if IJOB = 0, F has been overwritten by the solution */
/*          L. */

/*  LDF     (input) INTEGER */
/*          The leading dimension of the matrix F. LDF >= max(1, M). */

/*  SCALE   (output) DOUBLE PRECISION */
/*          On exit, 0 <= SCALE <= 1. If 0 < SCALE < 1, the solutions */
/*          R and L (C and F on entry) will hold the solutions to a */
/*          slightly perturbed system but the input matrices A, B, D and */
/*          E have not been changed. If SCALE = 0, R and L will hold the */
/*          solutions to the homogeneous system with C = F = 0. */
/*          Normally, SCALE = 1. */

/*  RDSUM   (input/output) DOUBLE PRECISION */
/*          On entry, the sum of squares of computed contributions to */
/*          the Dif-estimate under computation by ZTGSYL, where the */
/*          scaling factor RDSCAL (see below) has been factored out. */
/*          On exit, the corresponding sum of squares updated with the */
/*          contributions from the current sub-system. */
/*          If TRANS = 'T' RDSUM is not touched. */
/*          NOTE: RDSUM only makes sense when ZTGSY2 is called by */
/*          ZTGSYL. */

/*  RDSCAL  (input/output) DOUBLE PRECISION */
/*          On entry, scaling factor used to prevent overflow in RDSUM. */
/*          On exit, RDSCAL is updated w.r.t. the current contributions */
/*          in RDSUM. */
/*          If TRANS = 'T', RDSCAL is not touched. */
/*          NOTE: RDSCAL only makes sense when ZTGSY2 is called by */
/*          ZTGSYL. */

/*  INFO    (output) INTEGER */
/*          On exit, if INFO is set to */
/*            =0: Successful exit */
/*            <0: If INFO = -i, input argument number i is illegal. */
/*            >0: The matrix pairs (A, D) and (B, E) have common or very */
/*                close eigenvalues. */

/*  Further Details */
/*  =============== */

/*  Based on contributions by */
/*     Bo Kagstrom and Peter Poromaa, Department of Computing Science, */
/*     Umea University, S-901 87 Umea, Sweden. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       INTEGER            LDZ >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, LDZ = 2 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            NOTRAN >*/
/*<       INTEGER            I, IERR, J, K >*/
/*<       DOUBLE PRECISION   SCALOC >*/
/*<       COMPLEX*16         ALPHA >*/
/*     .. */
/*     .. Local Arrays .. */
/*<       INTEGER            IPIV( LDZ ), JPIV( LDZ ) >*/
/*<       COMPLEX*16         RHS( LDZ ), Z( LDZ, LDZ ) >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA, ZAXPY, ZGESC2, ZGETC2, ZLATDF, ZSCAL >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          DCMPLX, DCONJG, MAX >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Decode and test input parameters */

/*<       INFO = 0 >*/
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
    d_dim1 = *ldd;
    d_offset = 1 + d_dim1;
    d__ -= d_offset;
    e_dim1 = *lde;
    e_offset = 1 + e_dim1;
    e -= e_offset;
    f_dim1 = *ldf;
    f_offset = 1 + f_dim1;
    f -= f_offset;

    /* Function Body */
    *info = 0;
/*<       IERR = 0 >*/
    ierr = 0;
/*<       NOTRAN = LSAME( TRANS, 'N' ) >*/
    notran = lsame_(trans, "N", (ftnlen)1, (ftnlen)1);
/*<       IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'C' ) ) THEN >*/
    if (! notran && ! lsame_(trans, "C", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( NOTRAN ) THEN >*/
    } else if (notran) {
/*<          IF( ( IJOB.LT.0 ) .OR. ( IJOB.GT.2 ) ) THEN >*/
        if (*ijob < 0 || *ijob > 2) {
/*<             INFO = -2 >*/
            *info = -2;
/*<          END IF >*/
        }
/*<       END IF >*/
    }
/*<       IF( INFO.EQ.0 ) THEN >*/
    if (*info == 0) {
/*<          IF( M.LE.0 ) THEN >*/
        if (*m <= 0) {
/*<             INFO = -3 >*/
            *info = -3;
/*<          ELSE IF( N.LE.0 ) THEN >*/
        } else if (*n <= 0) {
/*<             INFO = -4 >*/
            *info = -4;
/*<          ELSE IF( LDA.LT.MAX( 1, M ) ) THEN >*/
        } else if (*lda < max(1,*m)) {
/*<             INFO = -5 >*/
            *info = -5;
/*<          ELSE IF( LDB.LT.MAX( 1, N ) ) THEN >*/
        } else if (*ldb < max(1,*n)) {
/*<             INFO = -8 >*/
            *info = -8;
/*<          ELSE IF( LDC.LT.MAX( 1, M ) ) THEN >*/
        } else if (*ldc < max(1,*m)) {
/*<             INFO = -10 >*/
            *info = -10;
/*<          ELSE IF( LDD.LT.MAX( 1, M ) ) THEN >*/
        } else if (*ldd < max(1,*m)) {
/*<             INFO = -12 >*/
            *info = -12;
/*<          ELSE IF( LDE.LT.MAX( 1, N ) ) THEN >*/
        } else if (*lde < max(1,*n)) {
/*<             INFO = -14 >*/
            *info = -14;
/*<          ELSE IF( LDF.LT.MAX( 1, M ) ) THEN >*/
        } else if (*ldf < max(1,*m)) {
/*<             INFO = -16 >*/
            *info = -16;
/*<          END IF >*/
        }
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZTGSY2', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZTGSY2", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       IF( NOTRAN ) THEN >*/
    if (notran) {

/*        Solve (I, J) - system */
/*           A(I, I) * R(I, J) - L(I, J) * B(J, J) = C(I, J) */
/*           D(I, I) * R(I, J) - L(I, J) * E(J, J) = F(I, J) */
/*        for I = M, M - 1, ..., 1; J = 1, 2, ..., N */

/*<          SCALE = ONE >*/
        *scale = 1.;
/*<          SCALOC = ONE >*/
        scaloc = 1.;
/*<          DO 30 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 20 I = M, 1, -1 >*/
            for (i__ = *m; i__ >= 1; --i__) {

/*              Build 2 by 2 system */

/*<                Z( 1, 1 ) = A( I, I ) >*/
                i__2 = i__ + i__ * a_dim1;
                z__[0].r = a[i__2].r, z__[0].i = a[i__2].i;
/*<                Z( 2, 1 ) = D( I, I ) >*/
                i__2 = i__ + i__ * d_dim1;
                z__[1].r = d__[i__2].r, z__[1].i = d__[i__2].i;
/*<                Z( 1, 2 ) = -B( J, J ) >*/
                i__2 = j + j * b_dim1;
                z__1.r = -b[i__2].r, z__1.i = -b[i__2].i;
                z__[2].r = z__1.r, z__[2].i = z__1.i;
/*<                Z( 2, 2 ) = -E( J, J ) >*/
                i__2 = j + j * e_dim1;
                z__1.r = -e[i__2].r, z__1.i = -e[i__2].i;
                z__[3].r = z__1.r, z__[3].i = z__1.i;

/*              Set up right hand side(s) */

/*<                RHS( 1 ) = C( I, J ) >*/
                i__2 = i__ + j * c_dim1;
                rhs[0].r = c__[i__2].r, rhs[0].i = c__[i__2].i;
/*<                RHS( 2 ) = F( I, J ) >*/
                i__2 = i__ + j * f_dim1;
                rhs[1].r = f[i__2].r, rhs[1].i = f[i__2].i;

/*              Solve Z * x = RHS */

/*<                CALL ZGETC2( LDZ, Z, LDZ, IPIV, JPIV, IERR ) >*/
                zgetc2_(&c__2, z__, &c__2, ipiv, jpiv, &ierr);
/*<    >*/
                if (ierr > 0) {
                    *info = ierr;
                }
/*<                IF( IJOB.EQ.0 ) THEN >*/
                if (*ijob == 0) {
/*<                   CALL ZGESC2( LDZ, Z, LDZ, RHS, IPIV, JPIV, SCALOC ) >*/
                    zgesc2_(&c__2, z__, &c__2, rhs, ipiv, jpiv, &scaloc);
/*<                   IF( SCALOC.NE.ONE ) THEN >*/
                    if (scaloc != 1.) {
/*<                      DO 10 K = 1, N >*/
                        i__2 = *n;
                        for (k = 1; k <= i__2; ++k) {
/*<    >*/
                            z__1.r = scaloc, z__1.i = 0.;
                            zscal_(m, &z__1, &c__[k * c_dim1 + 1], &c__1);
/*<    >*/
                            z__1.r = scaloc, z__1.i = 0.;
                            zscal_(m, &z__1, &f[k * f_dim1 + 1], &c__1);
/*<    10                CONTINUE >*/
/* L10: */
                        }
/*<                      SCALE = SCALE*SCALOC >*/
                        *scale *= scaloc;
/*<                   END IF >*/
                    }
/*<                ELSE >*/
                } else {
/*<    >*/
                    zlatdf_(ijob, &c__2, z__, &c__2, rhs, rdsum, rdscal, ipiv,
                             jpiv);
/*<                END IF >*/
                }

/*              Unpack solution vector(s) */

/*<                C( I, J ) = RHS( 1 ) >*/
                i__2 = i__ + j * c_dim1;
                c__[i__2].r = rhs[0].r, c__[i__2].i = rhs[0].i;
/*<                F( I, J ) = RHS( 2 ) >*/
                i__2 = i__ + j * f_dim1;
                f[i__2].r = rhs[1].r, f[i__2].i = rhs[1].i;

/*              Substitute R(I, J) and L(I, J) into remaining equation. */

/*<                IF( I.GT.1 ) THEN >*/
                if (i__ > 1) {
/*<                   ALPHA = -RHS( 1 ) >*/
                    z__1.r = -rhs[0].r, z__1.i = -rhs[0].i;
                    alpha.r = z__1.r, alpha.i = z__1.i;
/*<                   CALL ZAXPY( I-1, ALPHA, A( 1, I ), 1, C( 1, J ), 1 ) >*/
                    i__2 = i__ - 1;
                    zaxpy_(&i__2, &alpha, &a[i__ * a_dim1 + 1], &c__1, &c__[j
                            * c_dim1 + 1], &c__1);
/*<                   CALL ZAXPY( I-1, ALPHA, D( 1, I ), 1, F( 1, J ), 1 ) >*/
                    i__2 = i__ - 1;
                    zaxpy_(&i__2, &alpha, &d__[i__ * d_dim1 + 1], &c__1, &f[j
                            * f_dim1 + 1], &c__1);
/*<                END IF >*/
                }
/*<                IF( J.LT.N ) THEN >*/
                if (j < *n) {
/*<    >*/
                    i__2 = *n - j;
                    zaxpy_(&i__2, &rhs[1], &b[j + (j + 1) * b_dim1], ldb, &
                            c__[i__ + (j + 1) * c_dim1], ldc);
/*<    >*/
                    i__2 = *n - j;
                    zaxpy_(&i__2, &rhs[1], &e[j + (j + 1) * e_dim1], lde, &f[
                            i__ + (j + 1) * f_dim1], ldf);
/*<                END IF >*/
                }

/*<    20       CONTINUE >*/
/* L20: */
            }
/*<    30    CONTINUE >*/
/* L30: */
        }
/*<       ELSE >*/
    } else {

/*        Solve transposed (I, J) - system: */
/*           A(I, I)' * R(I, J) + D(I, I)' * L(J, J) = C(I, J) */
/*           R(I, I) * B(J, J) + L(I, J) * E(J, J)   = -F(I, J) */
/*        for I = 1, 2, ..., M, J = N, N - 1, ..., 1 */

/*<          SCALE = ONE >*/
        *scale = 1.;
/*<          SCALOC = ONE >*/
        scaloc = 1.;
/*<          DO 80 I = 1, M >*/
        i__1 = *m;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             DO 70 J = N, 1, -1 >*/
            for (j = *n; j >= 1; --j) {

/*              Build 2 by 2 system Z' */

/*<                Z( 1, 1 ) = DCONJG( A( I, I ) ) >*/
                d_cnjg(&z__1, &a[i__ + i__ * a_dim1]);
                z__[0].r = z__1.r, z__[0].i = z__1.i;
/*<                Z( 2, 1 ) = -DCONJG( B( J, J ) ) >*/
                d_cnjg(&z__2, &b[j + j * b_dim1]);
                z__1.r = -z__2.r, z__1.i = -z__2.i;
                z__[1].r = z__1.r, z__[1].i = z__1.i;
/*<                Z( 1, 2 ) = DCONJG( D( I, I ) ) >*/
                d_cnjg(&z__1, &d__[i__ + i__ * d_dim1]);
                z__[2].r = z__1.r, z__[2].i = z__1.i;
/*<                Z( 2, 2 ) = -DCONJG( E( J, J ) ) >*/
                d_cnjg(&z__2, &e[j + j * e_dim1]);
                z__1.r = -z__2.r, z__1.i = -z__2.i;
                z__[3].r = z__1.r, z__[3].i = z__1.i;


/*              Set up right hand side(s) */

/*<                RHS( 1 ) = C( I, J ) >*/
                i__2 = i__ + j * c_dim1;
                rhs[0].r = c__[i__2].r, rhs[0].i = c__[i__2].i;
/*<                RHS( 2 ) = F( I, J ) >*/
                i__2 = i__ + j * f_dim1;
                rhs[1].r = f[i__2].r, rhs[1].i = f[i__2].i;

/*              Solve Z' * x = RHS */

/*<                CALL ZGETC2( LDZ, Z, LDZ, IPIV, JPIV, IERR ) >*/
                zgetc2_(&c__2, z__, &c__2, ipiv, jpiv, &ierr);
/*<    >*/
                if (ierr > 0) {
                    *info = ierr;
                }
/*<                CALL ZGESC2( LDZ, Z, LDZ, RHS, IPIV, JPIV, SCALOC ) >*/
                zgesc2_(&c__2, z__, &c__2, rhs, ipiv, jpiv, &scaloc);
/*<                IF( SCALOC.NE.ONE ) THEN >*/
                if (scaloc != 1.) {
/*<                   DO 40 K = 1, N >*/
                    i__2 = *n;
                    for (k = 1; k <= i__2; ++k) {
/*<    >*/
                        z__1.r = scaloc, z__1.i = 0.;
                        zscal_(m, &z__1, &c__[k * c_dim1 + 1], &c__1);
/*<    >*/
                        z__1.r = scaloc, z__1.i = 0.;
                        zscal_(m, &z__1, &f[k * f_dim1 + 1], &c__1);
/*<    40             CONTINUE >*/
/* L40: */
                    }
/*<                   SCALE = SCALE*SCALOC >*/
                    *scale *= scaloc;
/*<                END IF >*/
                }

/*              Unpack solution vector(s) */

/*<                C( I, J ) = RHS( 1 ) >*/
                i__2 = i__ + j * c_dim1;
                c__[i__2].r = rhs[0].r, c__[i__2].i = rhs[0].i;
/*<                F( I, J ) = RHS( 2 ) >*/
                i__2 = i__ + j * f_dim1;
                f[i__2].r = rhs[1].r, f[i__2].i = rhs[1].i;

/*              Substitute R(I, J) and L(I, J) into remaining equation. */

/*<                DO 50 K = 1, J - 1 >*/
                i__2 = j - 1;
                for (k = 1; k <= i__2; ++k) {
/*<    >*/
                    i__3 = i__ + k * f_dim1;
                    i__4 = i__ + k * f_dim1;
                    d_cnjg(&z__4, &b[k + j * b_dim1]);
                    z__3.r = rhs[0].r * z__4.r - rhs[0].i * z__4.i, z__3.i =
                            rhs[0].r * z__4.i + rhs[0].i * z__4.r;
                    z__2.r = f[i__4].r + z__3.r, z__2.i = f[i__4].i + z__3.i;
                    d_cnjg(&z__6, &e[k + j * e_dim1]);
                    z__5.r = rhs[1].r * z__6.r - rhs[1].i * z__6.i, z__5.i =
                            rhs[1].r * z__6.i + rhs[1].i * z__6.r;
                    z__1.r = z__2.r + z__5.r, z__1.i = z__2.i + z__5.i;
                    f[i__3].r = z__1.r, f[i__3].i = z__1.i;
/*<    50          CONTINUE >*/
/* L50: */
                }
/*<                DO 60 K = I + 1, M >*/
                i__2 = *m;
                for (k = i__ + 1; k <= i__2; ++k) {
/*<    >*/
                    i__3 = k + j * c_dim1;
                    i__4 = k + j * c_dim1;
                    d_cnjg(&z__4, &a[i__ + k * a_dim1]);
                    z__3.r = z__4.r * rhs[0].r - z__4.i * rhs[0].i, z__3.i =
                            z__4.r * rhs[0].i + z__4.i * rhs[0].r;
                    z__2.r = c__[i__4].r - z__3.r, z__2.i = c__[i__4].i -
                            z__3.i;
                    d_cnjg(&z__6, &d__[i__ + k * d_dim1]);
                    z__5.r = z__6.r * rhs[1].r - z__6.i * rhs[1].i, z__5.i =
                            z__6.r * rhs[1].i + z__6.i * rhs[1].r;
                    z__1.r = z__2.r - z__5.r, z__1.i = z__2.i - z__5.i;
                    c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/*<    60          CONTINUE >*/
/* L60: */
                }

/*<    70       CONTINUE >*/
/* L70: */
            }
/*<    80    CONTINUE >*/
/* L80: */
        }
/*<       END IF >*/
    }
/*<       RETURN >*/
    return 0;

/*     End of ZTGSY2 */

/*<       END >*/
} /* ztgsy2_ */

#ifdef __cplusplus
        }
#endif
