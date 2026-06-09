/* lapack/double/dtgsy2.f -- translated by f2c (version 20050501).
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

static integer c__8 = 8;
static integer c__1 = 1;
static doublereal c_b27 = -1.;
static doublereal c_b42 = 1.;
static integer c__64 = 64;
static doublereal c_b54 = 0.;
static integer c__0 = 0;

/*<    >*/
/* Subroutine */ int dtgsy2_(char *trans, integer *ijob, integer *m, integer *
        n, doublereal *a, integer *lda, doublereal *b, integer *ldb,
        doublereal *c__, integer *ldc, doublereal *d__, integer *ldd,
        doublereal *e, integer *lde, doublereal *f, integer *ldf, doublereal *
        scale, doublereal *rdsum, doublereal *rdscal, integer *iwork, integer
        *pq, integer *info, ftnlen trans_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, d_dim1,
            d_offset, e_dim1, e_offset, f_dim1, f_offset, i__1, i__2, i__3;

    /* Local variables */
    integer i__, j, k, p, q;
    doublereal z__[64]  /* was [8][8] */;
    integer ie, je, mb, nb, ii, jj, is, js;
    doublereal rhs[8];
    integer isp1, jsp1;
    extern /* Subroutine */ int dger_(integer *, integer *, doublereal *,
            doublereal *, integer *, doublereal *, integer *, doublereal *,
            integer *);
    integer ierr, zdim, ipiv[8], jpiv[8];
    doublereal alpha;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *), dgemm_(char *, char *, integer *, integer *, integer *
            , doublereal *, doublereal *, integer *, doublereal *, integer *,
            doublereal *, doublereal *, integer *, ftnlen, ftnlen);
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int dgemv_(char *, integer *, integer *,
            doublereal *, doublereal *, integer *, doublereal *, integer *,
            doublereal *, doublereal *, integer *, ftnlen), dcopy_(integer *,
            doublereal *, integer *, doublereal *, integer *), daxpy_(integer
            *, doublereal *, doublereal *, integer *, doublereal *, integer *)
            , dgesc2_(integer *, doublereal *, integer *, doublereal *,
            integer *, integer *, doublereal *), dgetc2_(integer *,
            doublereal *, integer *, integer *, integer *, integer *),
            dlatdf_(integer *, integer *, doublereal *, integer *, doublereal
            *, doublereal *, doublereal *, integer *, integer *);
    doublereal scaloc;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    logical notran;
    (void)trans_len;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          TRANS >*/
/*<    >*/
/*<       DOUBLE PRECISION   RDSCAL, RDSUM, SCALE >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       INTEGER            IWORK( * ) >*/
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DTGSY2 solves the generalized Sylvester equation: */

/*              A * R - L * B = scale * C                (1) */
/*              D * R - L * E = scale * F, */

/*  using Level 1 and 2 BLAS. where R and L are unknown M-by-N matrices, */
/*  (A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M, */
/*  N-by-N and M-by-N, respectively, with real entries. (A, D) and (B, E) */
/*  must be in generalized Schur canonical form, i.e. A, B are upper */
/*  quasi triangular and D, E are upper triangular. The solution (R, L) */
/*  overwrites (C, F). 0 <= SCALE <= 1 is an output scaling factor */
/*  chosen to avoid overflow. */

/*  In matrix notation solving equation (1) corresponds to solve */
/*  Z*x = scale*b, where Z is defined as */

/*         Z = [ kron(In, A)  -kron(B', Im) ]             (2) */
/*             [ kron(In, D)  -kron(E', Im) ], */

/*  Ik is the identity matrix of size k and X' is the transpose of X. */
/*  kron(X, Y) is the Kronecker product between the matrices X and Y. */
/*  In the process of solving (1), we solve a number of such systems */
/*  where Dim(In), Dim(In) = 1 or 2. */

/*  If TRANS = 'T', solve the transposed system Z'*y = scale*b for y, */
/*  which is equivalent to solve for R and L in */

/*              A' * R  + D' * L   = scale *  C           (3) */
/*              R  * B' + L  * E'  = scale * -F */

/*  This case is used to compute an estimate of Dif[(A, D), (B, E)] = */
/*  sigma_min(Z) using reverse communicaton with DLACON. */

/*  DTGSY2 also (IJOB >= 1) contributes to the computation in STGSYL */
/*  of an upper bound on the separation between to matrix pairs. Then */
/*  the input (A, D), (B, E) are sub-pencils of the matrix pair in */
/*  DTGSYL. See STGSYL for details. */

/*  Arguments */
/*  ========= */

/*  TRANS   (input) CHARACTER */
/*          = 'N', solve the generalized Sylvester equation (1). */
/*          = 'T': solve the 'transposed' system (3). */

/*  IJOB    (input) INTEGER */
/*          Specifies what kind of functionality to be performed. */
/*          = 0: solve (1) only. */
/*          = 1: A contribution from this subsystem to a Frobenius */
/*               norm-based estimate of the separation between two matrix */
/*               pairs is computed. (look ahead strategy is used). */
/*          = 2: A contribution from this subsystem to a Frobenius */
/*               norm-based estimate of the separation between two matrix */
/*               pairs is computed. (DGECON on sub-systems is used.) */
/*          Not referenced if TRANS = 'T'. */

/*  M       (input) INTEGER */
/*          On entry, M specifies the order of A and D, and the row */
/*          dimension of C, F, R and L. */

/*  N       (input) INTEGER */
/*          On entry, N specifies the order of B and E, and the column */
/*          dimension of C, F, R and L. */

/*  A       (input) DOUBLE PRECISION array, dimension (LDA, M) */
/*          On entry, A contains an upper quasi triangular matrix. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the matrix A. LDA >= max(1, M). */

/*  B       (input) DOUBLE PRECISION array, dimension (LDB, N) */
/*          On entry, B contains an upper quasi triangular matrix. */

/*  LDB     (input) INTEGER */
/*          The leading dimension of the matrix B. LDB >= max(1, N). */

/*  C       (input/ output) DOUBLE PRECISION array, dimension (LDC, N) */
/*          On entry, C contains the right-hand-side of the first matrix */
/*          equation in (1). */
/*          On exit, if IJOB = 0, C has been overwritten by the */
/*          solution R. */

/*  LDC     (input) INTEGER */
/*          The leading dimension of the matrix C. LDC >= max(1, M). */

/*  D       (input) DOUBLE PRECISION array, dimension (LDD, M) */
/*          On entry, D contains an upper triangular matrix. */

/*  LDD     (input) INTEGER */
/*          The leading dimension of the matrix D. LDD >= max(1, M). */

/*  E       (input) DOUBLE PRECISION array, dimension (LDE, N) */
/*          On entry, E contains an upper triangular matrix. */

/*  LDE     (input) INTEGER */
/*          The leading dimension of the matrix E. LDE >= max(1, N). */

/*  F       (input/ output) DOUBLE PRECISION array, dimension (LDF, N) */
/*          On entry, F contains the right-hand-side of the second matrix */
/*          equation in (1). */
/*          On exit, if IJOB = 0, F has been overwritten by the */
/*          solution L. */

/*  LDF     (input) INTEGER */
/*          The leading dimension of the matrix F. LDF >= max(1, M). */

/*  SCALE   (output) DOUBLE PRECISION */
/*          On exit, 0 <= SCALE <= 1. If 0 < SCALE < 1, the solutions */
/*          R and L (C and F on entry) will hold the solutions to a */
/*          slightly perturbed system but the input matrices A, B, D and */
/*          E have not been changed. If SCALE = 0, R and L will hold the */
/*          solutions to the homogeneous system with C = F = 0. Normally, */
/*          SCALE = 1. */

/*  RDSUM   (input/output) DOUBLE PRECISION */
/*          On entry, the sum of squares of computed contributions to */
/*          the Dif-estimate under computation by DTGSYL, where the */
/*          scaling factor RDSCAL (see below) has been factored out. */
/*          On exit, the corresponding sum of squares updated with the */
/*          contributions from the current sub-system. */
/*          If TRANS = 'T' RDSUM is not touched. */
/*          NOTE: RDSUM only makes sense when DTGSY2 is called by STGSYL. */

/*  RDSCAL  (input/output) DOUBLE PRECISION */
/*          On entry, scaling factor used to prevent overflow in RDSUM. */
/*          On exit, RDSCAL is updated w.r.t. the current contributions */
/*          in RDSUM. */
/*          If TRANS = 'T', RDSCAL is not touched. */
/*          NOTE: RDSCAL only makes sense when DTGSY2 is called by */
/*                DTGSYL. */

/*  IWORK   (workspace) INTEGER array, dimension (M+N+2) */

/*  PQ      (output) INTEGER */
/*          On exit, the number of subsystems (of size 2-by-2, 4-by-4 and */
/*          8-by-8) solved by this routine. */

/*  INFO    (output) INTEGER */
/*          On exit, if INFO is set to */
/*            =0: Successful exit */
/*            <0: If INFO = -i, the i-th argument had an illegal value. */
/*            >0: The matrix pairs (A, D) and (B, E) have common or very */
/*                close eigenvalues. */

/*  Further Details */
/*  =============== */

/*  Based on contributions by */
/*     Bo Kagstrom and Peter Poromaa, Department of Computing Science, */
/*     Umea University, S-901 87 Umea, Sweden. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       INTEGER            LDZ >*/
/*<       PARAMETER          ( LDZ = 8 ) >*/
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            NOTRAN >*/
/*<    >*/
/*<       DOUBLE PRECISION   ALPHA, SCALOC >*/
/*     .. */
/*     .. Local Arrays .. */
/*<       INTEGER            IPIV( LDZ ), JPIV( LDZ ) >*/
/*<       DOUBLE PRECISION   RHS( LDZ ), Z( LDZ, LDZ ) >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. External Subroutines .. */
/*<    >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MAX >*/
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
    --iwork;

    /* Function Body */
    *info = 0;
/*<       IERR = 0 >*/
    ierr = 0;
/*<       NOTRAN = LSAME( TRANS, 'N' ) >*/
    notran = lsame_(trans, "N", (ftnlen)1, (ftnlen)1);
/*<       IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN >*/
    if (! notran && ! lsame_(trans, "T", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( ( IJOB.LT.0 ) .OR. ( IJOB.GT.2 ) ) THEN >*/
    } else if (*ijob < 0 || *ijob > 2) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( M.LE.0 ) THEN >*/
    } else if (*m <= 0) {
/*<          INFO = -3 >*/
        *info = -3;
/*<       ELSE IF( N.LE.0 ) THEN >*/
    } else if (*n <= 0) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( LDA.LT.MAX( 1, M ) ) THEN >*/
    } else if (*lda < max(1,*m)) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       ELSE IF( LDB.LT.MAX( 1, N ) ) THEN >*/
    } else if (*ldb < max(1,*n)) {
/*<          INFO = -8 >*/
        *info = -8;
/*<       ELSE IF( LDC.LT.MAX( 1, M ) ) THEN >*/
    } else if (*ldc < max(1,*m)) {
/*<          INFO = -10 >*/
        *info = -10;
/*<       ELSE IF( LDD.LT.MAX( 1, M ) ) THEN >*/
    } else if (*ldd < max(1,*m)) {
/*<          INFO = -12 >*/
        *info = -12;
/*<       ELSE IF( LDE.LT.MAX( 1, N ) ) THEN >*/
    } else if (*lde < max(1,*n)) {
/*<          INFO = -14 >*/
        *info = -14;
/*<       ELSE IF( LDF.LT.MAX( 1, M ) ) THEN >*/
    } else if (*ldf < max(1,*m)) {
/*<          INFO = -16 >*/
        *info = -16;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'DTGSY2', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("DTGSY2", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Determine block structure of A */

/*<       PQ = 0 >*/
    *pq = 0;
/*<       P = 0 >*/
    p = 0;
/*<       I = 1 >*/
    i__ = 1;
/*<    10 CONTINUE >*/
L10:
/*<    >*/
    if (i__ > *m) {
        goto L20;
    }
/*<       P = P + 1 >*/
    ++p;
/*<       IWORK( P ) = I >*/
    iwork[p] = i__;
/*<    >*/
    if (i__ == *m) {
        goto L20;
    }
/*<       IF( A( I+1, I ).NE.ZERO ) THEN >*/
    if (a[i__ + 1 + i__ * a_dim1] != 0.) {
/*<          I = I + 2 >*/
        i__ += 2;
/*<       ELSE >*/
    } else {
/*<          I = I + 1 >*/
        ++i__;
/*<       END IF >*/
    }
/*<       GO TO 10 >*/
    goto L10;
/*<    20 CONTINUE >*/
L20:
/*<       IWORK( P+1 ) = M + 1 >*/
    iwork[p + 1] = *m + 1;

/*     Determine block structure of B */

/*<       Q = P + 1 >*/
    q = p + 1;
/*<       J = 1 >*/
    j = 1;
/*<    30 CONTINUE >*/
L30:
/*<    >*/
    if (j > *n) {
        goto L40;
    }
/*<       Q = Q + 1 >*/
    ++q;
/*<       IWORK( Q ) = J >*/
    iwork[q] = j;
/*<    >*/
    if (j == *n) {
        goto L40;
    }
/*<       IF( B( J+1, J ).NE.ZERO ) THEN >*/
    if (b[j + 1 + j * b_dim1] != 0.) {
/*<          J = J + 2 >*/
        j += 2;
/*<       ELSE >*/
    } else {
/*<          J = J + 1 >*/
        ++j;
/*<       END IF >*/
    }
/*<       GO TO 30 >*/
    goto L30;
/*<    40 CONTINUE >*/
L40:
/*<       IWORK( Q+1 ) = N + 1 >*/
    iwork[q + 1] = *n + 1;
/*<       PQ = P*( Q-P-1 ) >*/
    *pq = p * (q - p - 1);

/*<       IF( NOTRAN ) THEN >*/
    if (notran) {

/*        Solve (I, J) - subsystem */
/*           A(I, I) * R(I, J) - L(I, J) * B(J, J) = C(I, J) */
/*           D(I, I) * R(I, J) - L(I, J) * E(J, J) = F(I, J) */
/*        for I = P, P - 1, ..., 1; J = 1, 2, ..., Q */

/*<          SCALE = ONE >*/
        *scale = 1.;
/*<          SCALOC = ONE >*/
        scaloc = 1.;
/*<          DO 120 J = P + 2, Q >*/
        i__1 = q;
        for (j = p + 2; j <= i__1; ++j) {
/*<             JS = IWORK( J ) >*/
            js = iwork[j];
/*<             JSP1 = JS + 1 >*/
            jsp1 = js + 1;
/*<             JE = IWORK( J+1 ) - 1 >*/
            je = iwork[j + 1] - 1;
/*<             NB = JE - JS + 1 >*/
            nb = je - js + 1;
/*<             DO 110 I = P, 1, -1 >*/
            for (i__ = p; i__ >= 1; --i__) {

/*<                IS = IWORK( I ) >*/
                is = iwork[i__];
/*<                ISP1 = IS + 1 >*/
                isp1 = is + 1;
/*<                IE = IWORK( I+1 ) - 1 >*/
                ie = iwork[i__ + 1] - 1;
/*<                MB = IE - IS + 1 >*/
                mb = ie - is + 1;
/*<                ZDIM = MB*NB*2 >*/
                zdim = mb * nb << 1;

/*<                IF( ( MB.EQ.1 ) .AND. ( NB.EQ.1 ) ) THEN >*/
                if (mb == 1 && nb == 1) {

/*                 Build a 2-by-2 system Z * x = RHS */

/*<                   Z( 1, 1 ) = A( IS, IS ) >*/
                    z__[0] = a[is + is * a_dim1];
/*<                   Z( 2, 1 ) = D( IS, IS ) >*/
                    z__[1] = d__[is + is * d_dim1];
/*<                   Z( 1, 2 ) = -B( JS, JS ) >*/
                    z__[8] = -b[js + js * b_dim1];
/*<                   Z( 2, 2 ) = -E( JS, JS ) >*/
                    z__[9] = -e[js + js * e_dim1];

/*                 Set up right hand side(s) */

/*<                   RHS( 1 ) = C( IS, JS ) >*/
                    rhs[0] = c__[is + js * c_dim1];
/*<                   RHS( 2 ) = F( IS, JS ) >*/
                    rhs[1] = f[is + js * f_dim1];

/*                 Solve Z * x = RHS */

/*<                   CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR ) >*/
                    dgetc2_(&zdim, z__, &c__8, ipiv, jpiv, &ierr);
/*<    >*/
                    if (ierr > 0) {
                        *info = ierr;
                    }

/*<                   IF( IJOB.EQ.0 ) THEN >*/
                    if (*ijob == 0) {
/*<    >*/
                        dgesc2_(&zdim, z__, &c__8, rhs, ipiv, jpiv, &scaloc);
/*<                      IF( SCALOC.NE.ONE ) THEN >*/
                        if (scaloc != 1.) {
/*<                         DO 50 K = 1, N >*/
                            i__2 = *n;
                            for (k = 1; k <= i__2; ++k) {
/*<                            CALL DSCAL( M, SCALOC, C( 1, K ), 1 ) >*/
                                dscal_(m, &scaloc, &c__[k * c_dim1 + 1], &
                                        c__1);
/*<                            CALL DSCAL( M, SCALOC, F( 1, K ), 1 ) >*/
                                dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
/*<    50                   CONTINUE >*/
/* L50: */
                            }
/*<                         SCALE = SCALE*SCALOC >*/
                            *scale *= scaloc;
/*<                      END IF >*/
                        }
/*<                   ELSE >*/
                    } else {
/*<    >*/
                        dlatdf_(ijob, &zdim, z__, &c__8, rhs, rdsum, rdscal,
                                ipiv, jpiv);
/*<                   END IF >*/
                    }

/*                 Unpack solution vector(s) */

/*<                   C( IS, JS ) = RHS( 1 ) >*/
                    c__[is + js * c_dim1] = rhs[0];
/*<                   F( IS, JS ) = RHS( 2 ) >*/
                    f[is + js * f_dim1] = rhs[1];

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

/*<                   IF( I.GT.1 ) THEN >*/
                    if (i__ > 1) {
/*<                      ALPHA = -RHS( 1 ) >*/
                        alpha = -rhs[0];
/*<    >*/
                        i__2 = is - 1;
                        daxpy_(&i__2, &alpha, &a[is * a_dim1 + 1], &c__1, &
                                c__[js * c_dim1 + 1], &c__1);
/*<    >*/
                        i__2 = is - 1;
                        daxpy_(&i__2, &alpha, &d__[is * d_dim1 + 1], &c__1, &
                                f[js * f_dim1 + 1], &c__1);
/*<                   END IF >*/
                    }
/*<                   IF( J.LT.Q ) THEN >*/
                    if (j < q) {
/*<    >*/
                        i__2 = *n - je;
                        daxpy_(&i__2, &rhs[1], &b[js + (je + 1) * b_dim1],
                                ldb, &c__[is + (je + 1) * c_dim1], ldc);
/*<    >*/
                        i__2 = *n - je;
                        daxpy_(&i__2, &rhs[1], &e[js + (je + 1) * e_dim1],
                                lde, &f[is + (je + 1) * f_dim1], ldf);
/*<                   END IF >*/
                    }

/*<                ELSE IF( ( MB.EQ.1 ) .AND. ( NB.EQ.2 ) ) THEN >*/
                } else if (mb == 1 && nb == 2) {

/*                 Build a 4-by-4 system Z * x = RHS */

/*<                   Z( 1, 1 ) = A( IS, IS ) >*/
                    z__[0] = a[is + is * a_dim1];
/*<                   Z( 2, 1 ) = ZERO >*/
                    z__[1] = 0.;
/*<                   Z( 3, 1 ) = D( IS, IS ) >*/
                    z__[2] = d__[is + is * d_dim1];
/*<                   Z( 4, 1 ) = ZERO >*/
                    z__[3] = 0.;

/*<                   Z( 1, 2 ) = ZERO >*/
                    z__[8] = 0.;
/*<                   Z( 2, 2 ) = A( IS, IS ) >*/
                    z__[9] = a[is + is * a_dim1];
/*<                   Z( 3, 2 ) = ZERO >*/
                    z__[10] = 0.;
/*<                   Z( 4, 2 ) = D( IS, IS ) >*/
                    z__[11] = d__[is + is * d_dim1];

/*<                   Z( 1, 3 ) = -B( JS, JS ) >*/
                    z__[16] = -b[js + js * b_dim1];
/*<                   Z( 2, 3 ) = -B( JS, JSP1 ) >*/
                    z__[17] = -b[js + jsp1 * b_dim1];
/*<                   Z( 3, 3 ) = -E( JS, JS ) >*/
                    z__[18] = -e[js + js * e_dim1];
/*<                   Z( 4, 3 ) = -E( JS, JSP1 ) >*/
                    z__[19] = -e[js + jsp1 * e_dim1];

/*<                   Z( 1, 4 ) = -B( JSP1, JS ) >*/
                    z__[24] = -b[jsp1 + js * b_dim1];
/*<                   Z( 2, 4 ) = -B( JSP1, JSP1 ) >*/
                    z__[25] = -b[jsp1 + jsp1 * b_dim1];
/*<                   Z( 3, 4 ) = ZERO >*/
                    z__[26] = 0.;
/*<                   Z( 4, 4 ) = -E( JSP1, JSP1 ) >*/
                    z__[27] = -e[jsp1 + jsp1 * e_dim1];

/*                 Set up right hand side(s) */

/*<                   RHS( 1 ) = C( IS, JS ) >*/
                    rhs[0] = c__[is + js * c_dim1];
/*<                   RHS( 2 ) = C( IS, JSP1 ) >*/
                    rhs[1] = c__[is + jsp1 * c_dim1];
/*<                   RHS( 3 ) = F( IS, JS ) >*/
                    rhs[2] = f[is + js * f_dim1];
/*<                   RHS( 4 ) = F( IS, JSP1 ) >*/
                    rhs[3] = f[is + jsp1 * f_dim1];

/*                 Solve Z * x = RHS */

/*<                   CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR ) >*/
                    dgetc2_(&zdim, z__, &c__8, ipiv, jpiv, &ierr);
/*<    >*/
                    if (ierr > 0) {
                        *info = ierr;
                    }

/*<                   IF( IJOB.EQ.0 ) THEN >*/
                    if (*ijob == 0) {
/*<    >*/
                        dgesc2_(&zdim, z__, &c__8, rhs, ipiv, jpiv, &scaloc);
/*<                      IF( SCALOC.NE.ONE ) THEN >*/
                        if (scaloc != 1.) {
/*<                         DO 60 K = 1, N >*/
                            i__2 = *n;
                            for (k = 1; k <= i__2; ++k) {
/*<                            CALL DSCAL( M, SCALOC, C( 1, K ), 1 ) >*/
                                dscal_(m, &scaloc, &c__[k * c_dim1 + 1], &
                                        c__1);
/*<                            CALL DSCAL( M, SCALOC, F( 1, K ), 1 ) >*/
                                dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
/*<    60                   CONTINUE >*/
/* L60: */
                            }
/*<                         SCALE = SCALE*SCALOC >*/
                            *scale *= scaloc;
/*<                      END IF >*/
                        }
/*<                   ELSE >*/
                    } else {
/*<    >*/
                        dlatdf_(ijob, &zdim, z__, &c__8, rhs, rdsum, rdscal,
                                ipiv, jpiv);
/*<                   END IF >*/
                    }

/*                 Unpack solution vector(s) */

/*<                   C( IS, JS ) = RHS( 1 ) >*/
                    c__[is + js * c_dim1] = rhs[0];
/*<                   C( IS, JSP1 ) = RHS( 2 ) >*/
                    c__[is + jsp1 * c_dim1] = rhs[1];
/*<                   F( IS, JS ) = RHS( 3 ) >*/
                    f[is + js * f_dim1] = rhs[2];
/*<                   F( IS, JSP1 ) = RHS( 4 ) >*/
                    f[is + jsp1 * f_dim1] = rhs[3];

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

/*<                   IF( I.GT.1 ) THEN >*/
                    if (i__ > 1) {
/*<    >*/
                        i__2 = is - 1;
                        dger_(&i__2, &nb, &c_b27, &a[is * a_dim1 + 1], &c__1,
                                rhs, &c__1, &c__[js * c_dim1 + 1], ldc);
/*<    >*/
                        i__2 = is - 1;
                        dger_(&i__2, &nb, &c_b27, &d__[is * d_dim1 + 1], &
                                c__1, rhs, &c__1, &f[js * f_dim1 + 1], ldf);
/*<                   END IF >*/
                    }
/*<                   IF( J.LT.Q ) THEN >*/
                    if (j < q) {
/*<    >*/
                        i__2 = *n - je;
                        daxpy_(&i__2, &rhs[2], &b[js + (je + 1) * b_dim1],
                                ldb, &c__[is + (je + 1) * c_dim1], ldc);
/*<    >*/
                        i__2 = *n - je;
                        daxpy_(&i__2, &rhs[2], &e[js + (je + 1) * e_dim1],
                                lde, &f[is + (je + 1) * f_dim1], ldf);
/*<    >*/
                        i__2 = *n - je;
                        daxpy_(&i__2, &rhs[3], &b[jsp1 + (je + 1) * b_dim1],
                                ldb, &c__[is + (je + 1) * c_dim1], ldc);
/*<    >*/
                        i__2 = *n - je;
                        daxpy_(&i__2, &rhs[3], &e[jsp1 + (je + 1) * e_dim1],
                                lde, &f[is + (je + 1) * f_dim1], ldf);
/*<                   END IF >*/
                    }

/*<                ELSE IF( ( MB.EQ.2 ) .AND. ( NB.EQ.1 ) ) THEN >*/
                } else if (mb == 2 && nb == 1) {

/*                 Build a 4-by-4 system Z * x = RHS */

/*<                   Z( 1, 1 ) = A( IS, IS ) >*/
                    z__[0] = a[is + is * a_dim1];
/*<                   Z( 2, 1 ) = A( ISP1, IS ) >*/
                    z__[1] = a[isp1 + is * a_dim1];
/*<                   Z( 3, 1 ) = D( IS, IS ) >*/
                    z__[2] = d__[is + is * d_dim1];
/*<                   Z( 4, 1 ) = ZERO >*/
                    z__[3] = 0.;

/*<                   Z( 1, 2 ) = A( IS, ISP1 ) >*/
                    z__[8] = a[is + isp1 * a_dim1];
/*<                   Z( 2, 2 ) = A( ISP1, ISP1 ) >*/
                    z__[9] = a[isp1 + isp1 * a_dim1];
/*<                   Z( 3, 2 ) = D( IS, ISP1 ) >*/
                    z__[10] = d__[is + isp1 * d_dim1];
/*<                   Z( 4, 2 ) = D( ISP1, ISP1 ) >*/
                    z__[11] = d__[isp1 + isp1 * d_dim1];

/*<                   Z( 1, 3 ) = -B( JS, JS ) >*/
                    z__[16] = -b[js + js * b_dim1];
/*<                   Z( 2, 3 ) = ZERO >*/
                    z__[17] = 0.;
/*<                   Z( 3, 3 ) = -E( JS, JS ) >*/
                    z__[18] = -e[js + js * e_dim1];
/*<                   Z( 4, 3 ) = ZERO >*/
                    z__[19] = 0.;

/*<                   Z( 1, 4 ) = ZERO >*/
                    z__[24] = 0.;
/*<                   Z( 2, 4 ) = -B( JS, JS ) >*/
                    z__[25] = -b[js + js * b_dim1];
/*<                   Z( 3, 4 ) = ZERO >*/
                    z__[26] = 0.;
/*<                   Z( 4, 4 ) = -E( JS, JS ) >*/
                    z__[27] = -e[js + js * e_dim1];

/*                 Set up right hand side(s) */

/*<                   RHS( 1 ) = C( IS, JS ) >*/
                    rhs[0] = c__[is + js * c_dim1];
/*<                   RHS( 2 ) = C( ISP1, JS ) >*/
                    rhs[1] = c__[isp1 + js * c_dim1];
/*<                   RHS( 3 ) = F( IS, JS ) >*/
                    rhs[2] = f[is + js * f_dim1];
/*<                   RHS( 4 ) = F( ISP1, JS ) >*/
                    rhs[3] = f[isp1 + js * f_dim1];

/*                 Solve Z * x = RHS */

/*<                   CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR ) >*/
                    dgetc2_(&zdim, z__, &c__8, ipiv, jpiv, &ierr);
/*<    >*/
                    if (ierr > 0) {
                        *info = ierr;
                    }
/*<                   IF( IJOB.EQ.0 ) THEN >*/
                    if (*ijob == 0) {
/*<    >*/
                        dgesc2_(&zdim, z__, &c__8, rhs, ipiv, jpiv, &scaloc);
/*<                      IF( SCALOC.NE.ONE ) THEN >*/
                        if (scaloc != 1.) {
/*<                         DO 70 K = 1, N >*/
                            i__2 = *n;
                            for (k = 1; k <= i__2; ++k) {
/*<                            CALL DSCAL( M, SCALOC, C( 1, K ), 1 ) >*/
                                dscal_(m, &scaloc, &c__[k * c_dim1 + 1], &
                                        c__1);
/*<                            CALL DSCAL( M, SCALOC, F( 1, K ), 1 ) >*/
                                dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
/*<    70                   CONTINUE >*/
/* L70: */
                            }
/*<                         SCALE = SCALE*SCALOC >*/
                            *scale *= scaloc;
/*<                      END IF >*/
                        }
/*<                   ELSE >*/
                    } else {
/*<    >*/
                        dlatdf_(ijob, &zdim, z__, &c__8, rhs, rdsum, rdscal,
                                ipiv, jpiv);
/*<                   END IF >*/
                    }

/*                 Unpack solution vector(s) */

/*<                   C( IS, JS ) = RHS( 1 ) >*/
                    c__[is + js * c_dim1] = rhs[0];
/*<                   C( ISP1, JS ) = RHS( 2 ) >*/
                    c__[isp1 + js * c_dim1] = rhs[1];
/*<                   F( IS, JS ) = RHS( 3 ) >*/
                    f[is + js * f_dim1] = rhs[2];
/*<                   F( ISP1, JS ) = RHS( 4 ) >*/
                    f[isp1 + js * f_dim1] = rhs[3];

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

/*<                   IF( I.GT.1 ) THEN >*/
                    if (i__ > 1) {
/*<    >*/
                        i__2 = is - 1;
                        dgemv_("N", &i__2, &mb, &c_b27, &a[is * a_dim1 + 1],
                                lda, rhs, &c__1, &c_b42, &c__[js * c_dim1 + 1]
                                , &c__1, (ftnlen)1);
/*<    >*/
                        i__2 = is - 1;
                        dgemv_("N", &i__2, &mb, &c_b27, &d__[is * d_dim1 + 1],
                                 ldd, rhs, &c__1, &c_b42, &f[js * f_dim1 + 1],
                                 &c__1, (ftnlen)1);
/*<                   END IF >*/
                    }
/*<                   IF( J.LT.Q ) THEN >*/
                    if (j < q) {
/*<    >*/
                        i__2 = *n - je;
                        dger_(&mb, &i__2, &c_b42, &rhs[2], &c__1, &b[js + (je
                                + 1) * b_dim1], ldb, &c__[is + (je + 1) *
                                c_dim1], ldc);
/*<    >*/
                        i__2 = *n - je;
                        dger_(&mb, &i__2, &c_b42, &rhs[2], &c__1, &e[js + (je
                                + 1) * e_dim1], ldb, &f[is + (je + 1) *
                                f_dim1], ldc);
/*<                   END IF >*/
                    }

/*<                ELSE IF( ( MB.EQ.2 ) .AND. ( NB.EQ.2 ) ) THEN >*/
                } else if (mb == 2 && nb == 2) {

/*                 Build an 8-by-8 system Z * x = RHS */

/*<                   CALL DCOPY( LDZ*LDZ, ZERO, 0, Z, 1 ) >*/
                    dcopy_(&c__64, &c_b54, &c__0, z__, &c__1);

/*<                   Z( 1, 1 ) = A( IS, IS ) >*/
                    z__[0] = a[is + is * a_dim1];
/*<                   Z( 2, 1 ) = A( ISP1, IS ) >*/
                    z__[1] = a[isp1 + is * a_dim1];
/*<                   Z( 5, 1 ) = D( IS, IS ) >*/
                    z__[4] = d__[is + is * d_dim1];

/*<                   Z( 1, 2 ) = A( IS, ISP1 ) >*/
                    z__[8] = a[is + isp1 * a_dim1];
/*<                   Z( 2, 2 ) = A( ISP1, ISP1 ) >*/
                    z__[9] = a[isp1 + isp1 * a_dim1];
/*<                   Z( 5, 2 ) = D( IS, ISP1 ) >*/
                    z__[12] = d__[is + isp1 * d_dim1];
/*<                   Z( 6, 2 ) = D( ISP1, ISP1 ) >*/
                    z__[13] = d__[isp1 + isp1 * d_dim1];

/*<                   Z( 3, 3 ) = A( IS, IS ) >*/
                    z__[18] = a[is + is * a_dim1];
/*<                   Z( 4, 3 ) = A( ISP1, IS ) >*/
                    z__[19] = a[isp1 + is * a_dim1];
/*<                   Z( 7, 3 ) = D( IS, IS ) >*/
                    z__[22] = d__[is + is * d_dim1];

/*<                   Z( 3, 4 ) = A( IS, ISP1 ) >*/
                    z__[26] = a[is + isp1 * a_dim1];
/*<                   Z( 4, 4 ) = A( ISP1, ISP1 ) >*/
                    z__[27] = a[isp1 + isp1 * a_dim1];
/*<                   Z( 7, 4 ) = D( IS, ISP1 ) >*/
                    z__[30] = d__[is + isp1 * d_dim1];
/*<                   Z( 8, 4 ) = D( ISP1, ISP1 ) >*/
                    z__[31] = d__[isp1 + isp1 * d_dim1];

/*<                   Z( 1, 5 ) = -B( JS, JS ) >*/
                    z__[32] = -b[js + js * b_dim1];
/*<                   Z( 3, 5 ) = -B( JS, JSP1 ) >*/
                    z__[34] = -b[js + jsp1 * b_dim1];
/*<                   Z( 5, 5 ) = -E( JS, JS ) >*/
                    z__[36] = -e[js + js * e_dim1];
/*<                   Z( 7, 5 ) = -E( JS, JSP1 ) >*/
                    z__[38] = -e[js + jsp1 * e_dim1];

/*<                   Z( 2, 6 ) = -B( JS, JS ) >*/
                    z__[41] = -b[js + js * b_dim1];
/*<                   Z( 4, 6 ) = -B( JS, JSP1 ) >*/
                    z__[43] = -b[js + jsp1 * b_dim1];
/*<                   Z( 6, 6 ) = -E( JS, JS ) >*/
                    z__[45] = -e[js + js * e_dim1];
/*<                   Z( 8, 6 ) = -E( JS, JSP1 ) >*/
                    z__[47] = -e[js + jsp1 * e_dim1];

/*<                   Z( 1, 7 ) = -B( JSP1, JS ) >*/
                    z__[48] = -b[jsp1 + js * b_dim1];
/*<                   Z( 3, 7 ) = -B( JSP1, JSP1 ) >*/
                    z__[50] = -b[jsp1 + jsp1 * b_dim1];
/*<                   Z( 7, 7 ) = -E( JSP1, JSP1 ) >*/
                    z__[54] = -e[jsp1 + jsp1 * e_dim1];

/*<                   Z( 2, 8 ) = -B( JSP1, JS ) >*/
                    z__[57] = -b[jsp1 + js * b_dim1];
/*<                   Z( 4, 8 ) = -B( JSP1, JSP1 ) >*/
                    z__[59] = -b[jsp1 + jsp1 * b_dim1];
/*<                   Z( 8, 8 ) = -E( JSP1, JSP1 ) >*/
                    z__[63] = -e[jsp1 + jsp1 * e_dim1];

/*                 Set up right hand side(s) */

/*<                   K = 1 >*/
                    k = 1;
/*<                   II = MB*NB + 1 >*/
                    ii = mb * nb + 1;
/*<                   DO 80 JJ = 0, NB - 1 >*/
                    i__2 = nb - 1;
                    for (jj = 0; jj <= i__2; ++jj) {
/*<                      CALL DCOPY( MB, C( IS, JS+JJ ), 1, RHS( K ), 1 ) >*/
                        dcopy_(&mb, &c__[is + (js + jj) * c_dim1], &c__1, &
                                rhs[k - 1], &c__1);
/*<                      CALL DCOPY( MB, F( IS, JS+JJ ), 1, RHS( II ), 1 ) >*/
                        dcopy_(&mb, &f[is + (js + jj) * f_dim1], &c__1, &rhs[
                                ii - 1], &c__1);
/*<                      K = K + MB >*/
                        k += mb;
/*<                      II = II + MB >*/
                        ii += mb;
/*<    80             CONTINUE >*/
/* L80: */
                    }

/*                 Solve Z * x = RHS */

/*<                   CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR ) >*/
                    dgetc2_(&zdim, z__, &c__8, ipiv, jpiv, &ierr);
/*<    >*/
                    if (ierr > 0) {
                        *info = ierr;
                    }
/*<                   IF( IJOB.EQ.0 ) THEN >*/
                    if (*ijob == 0) {
/*<    >*/
                        dgesc2_(&zdim, z__, &c__8, rhs, ipiv, jpiv, &scaloc);
/*<                      IF( SCALOC.NE.ONE ) THEN >*/
                        if (scaloc != 1.) {
/*<                         DO 90 K = 1, N >*/
                            i__2 = *n;
                            for (k = 1; k <= i__2; ++k) {
/*<                            CALL DSCAL( M, SCALOC, C( 1, K ), 1 ) >*/
                                dscal_(m, &scaloc, &c__[k * c_dim1 + 1], &
                                        c__1);
/*<                            CALL DSCAL( M, SCALOC, F( 1, K ), 1 ) >*/
                                dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
/*<    90                   CONTINUE >*/
/* L90: */
                            }
/*<                         SCALE = SCALE*SCALOC >*/
                            *scale *= scaloc;
/*<                      END IF >*/
                        }
/*<                   ELSE >*/
                    } else {
/*<    >*/
                        dlatdf_(ijob, &zdim, z__, &c__8, rhs, rdsum, rdscal,
                                ipiv, jpiv);
/*<                   END IF >*/
                    }

/*                 Unpack solution vector(s) */

/*<                   K = 1 >*/
                    k = 1;
/*<                   II = MB*NB + 1 >*/
                    ii = mb * nb + 1;
/*<                   DO 100 JJ = 0, NB - 1 >*/
                    i__2 = nb - 1;
                    for (jj = 0; jj <= i__2; ++jj) {
/*<                      CALL DCOPY( MB, RHS( K ), 1, C( IS, JS+JJ ), 1 ) >*/
                        dcopy_(&mb, &rhs[k - 1], &c__1, &c__[is + (js + jj) *
                                c_dim1], &c__1);
/*<                      CALL DCOPY( MB, RHS( II ), 1, F( IS, JS+JJ ), 1 ) >*/
                        dcopy_(&mb, &rhs[ii - 1], &c__1, &f[is + (js + jj) *
                                f_dim1], &c__1);
/*<                      K = K + MB >*/
                        k += mb;
/*<                      II = II + MB >*/
                        ii += mb;
/*<   100             CONTINUE >*/
/* L100: */
                    }

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

/*<                   IF( I.GT.1 ) THEN >*/
                    if (i__ > 1) {
/*<    >*/
                        i__2 = is - 1;
                        dgemm_("N", "N", &i__2, &nb, &mb, &c_b27, &a[is *
                                a_dim1 + 1], lda, rhs, &mb, &c_b42, &c__[js *
                                c_dim1 + 1], ldc, (ftnlen)1, (ftnlen)1);
/*<    >*/
                        i__2 = is - 1;
                        dgemm_("N", "N", &i__2, &nb, &mb, &c_b27, &d__[is *
                                d_dim1 + 1], ldd, rhs, &mb, &c_b42, &f[js *
                                f_dim1 + 1], ldf, (ftnlen)1, (ftnlen)1);
/*<                   END IF >*/
                    }
/*<                   IF( J.LT.Q ) THEN >*/
                    if (j < q) {
/*<                      K = MB*NB + 1 >*/
                        k = mb * nb + 1;
/*<    >*/
                        i__2 = *n - je;
                        dgemm_("N", "N", &mb, &i__2, &nb, &c_b42, &rhs[k - 1],
                                 &mb, &b[js + (je + 1) * b_dim1], ldb, &c_b42,
                                 &c__[is + (je + 1) * c_dim1], ldc, (ftnlen)1,
                                 (ftnlen)1);
/*<    >*/
                        i__2 = *n - je;
                        dgemm_("N", "N", &mb, &i__2, &nb, &c_b42, &rhs[k - 1],
                                 &mb, &e[js + (je + 1) * e_dim1], lde, &c_b42,
                                 &f[is + (je + 1) * f_dim1], ldf, (ftnlen)1, (
                                ftnlen)1);
/*<                   END IF >*/
                    }

/*<                END IF >*/
                }

/*<   110       CONTINUE >*/
/* L110: */
            }
/*<   120    CONTINUE >*/
/* L120: */
        }
/*<       ELSE >*/
    } else {

/*        Solve (I, J) - subsystem */
/*             A(I, I)' * R(I, J) + D(I, I)' * L(J, J)  =  C(I, J) */
/*             R(I, I)  * B(J, J) + L(I, J)  * E(J, J)  = -F(I, J) */
/*        for I = 1, 2, ..., P, J = Q, Q - 1, ..., 1 */

/*<          SCALE = ONE >*/
        *scale = 1.;
/*<          SCALOC = ONE >*/
        scaloc = 1.;
/*<          DO 200 I = 1, P >*/
        i__1 = p;
        for (i__ = 1; i__ <= i__1; ++i__) {

/*<             IS = IWORK( I ) >*/
            is = iwork[i__];
/*<             ISP1 = IS + 1 >*/
            isp1 = is + 1;
/*<             IE = IWORK( I+1 ) - 1 >*/
            ie = iwork[i__ + 1] - 1;
/*<             MB = IE - IS + 1 >*/
            mb = ie - is + 1;
/*<             DO 190 J = Q, P + 2, -1 >*/
            i__2 = p + 2;
            for (j = q; j >= i__2; --j) {

/*<                JS = IWORK( J ) >*/
                js = iwork[j];
/*<                JSP1 = JS + 1 >*/
                jsp1 = js + 1;
/*<                JE = IWORK( J+1 ) - 1 >*/
                je = iwork[j + 1] - 1;
/*<                NB = JE - JS + 1 >*/
                nb = je - js + 1;
/*<                ZDIM = MB*NB*2 >*/
                zdim = mb * nb << 1;
/*<                IF( ( MB.EQ.1 ) .AND. ( NB.EQ.1 ) ) THEN >*/
                if (mb == 1 && nb == 1) {

/*                 Build a 2-by-2 system Z' * x = RHS */

/*<                   Z( 1, 1 ) = A( IS, IS ) >*/
                    z__[0] = a[is + is * a_dim1];
/*<                   Z( 2, 1 ) = -B( JS, JS ) >*/
                    z__[1] = -b[js + js * b_dim1];
/*<                   Z( 1, 2 ) = D( IS, IS ) >*/
                    z__[8] = d__[is + is * d_dim1];
/*<                   Z( 2, 2 ) = -E( JS, JS ) >*/
                    z__[9] = -e[js + js * e_dim1];

/*                 Set up right hand side(s) */

/*<                   RHS( 1 ) = C( IS, JS ) >*/
                    rhs[0] = c__[is + js * c_dim1];
/*<                   RHS( 2 ) = F( IS, JS ) >*/
                    rhs[1] = f[is + js * f_dim1];

/*                 Solve Z' * x = RHS */

/*<                   CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR ) >*/
                    dgetc2_(&zdim, z__, &c__8, ipiv, jpiv, &ierr);
/*<    >*/
                    if (ierr > 0) {
                        *info = ierr;
                    }

/*<                   CALL DGESC2( ZDIM, Z, LDZ, RHS, IPIV, JPIV, SCALOC ) >*/
                    dgesc2_(&zdim, z__, &c__8, rhs, ipiv, jpiv, &scaloc);
/*<                   IF( SCALOC.NE.ONE ) THEN >*/
                    if (scaloc != 1.) {
/*<                      DO 130 K = 1, N >*/
                        i__3 = *n;
                        for (k = 1; k <= i__3; ++k) {
/*<                         CALL DSCAL( M, SCALOC, C( 1, K ), 1 ) >*/
                            dscal_(m, &scaloc, &c__[k * c_dim1 + 1], &c__1);
/*<                         CALL DSCAL( M, SCALOC, F( 1, K ), 1 ) >*/
                            dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
/*<   130                CONTINUE >*/
/* L130: */
                        }
/*<                      SCALE = SCALE*SCALOC >*/
                        *scale *= scaloc;
/*<                   END IF >*/
                    }

/*                 Unpack solution vector(s) */

/*<                   C( IS, JS ) = RHS( 1 ) >*/
                    c__[is + js * c_dim1] = rhs[0];
/*<                   F( IS, JS ) = RHS( 2 ) >*/
                    f[is + js * f_dim1] = rhs[1];

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

/*<                   IF( J.GT.P+2 ) THEN >*/
                    if (j > p + 2) {
/*<                      ALPHA = RHS( 1 ) >*/
                        alpha = rhs[0];
/*<    >*/
                        i__3 = js - 1;
                        daxpy_(&i__3, &alpha, &b[js * b_dim1 + 1], &c__1, &f[
                                is + f_dim1], ldf);
/*<                      ALPHA = RHS( 2 ) >*/
                        alpha = rhs[1];
/*<    >*/
                        i__3 = js - 1;
                        daxpy_(&i__3, &alpha, &e[js * e_dim1 + 1], &c__1, &f[
                                is + f_dim1], ldf);
/*<                   END IF >*/
                    }
/*<                   IF( I.LT.P ) THEN >*/
                    if (i__ < p) {
/*<                      ALPHA = -RHS( 1 ) >*/
                        alpha = -rhs[0];
/*<    >*/
                        i__3 = *m - ie;
                        daxpy_(&i__3, &alpha, &a[is + (ie + 1) * a_dim1], lda,
                                 &c__[ie + 1 + js * c_dim1], &c__1);
/*<                      ALPHA = -RHS( 2 ) >*/
                        alpha = -rhs[1];
/*<    >*/
                        i__3 = *m - ie;
                        daxpy_(&i__3, &alpha, &d__[is + (ie + 1) * d_dim1],
                                ldd, &c__[ie + 1 + js * c_dim1], &c__1);
/*<                   END IF >*/
                    }

/*<                ELSE IF( ( MB.EQ.1 ) .AND. ( NB.EQ.2 ) ) THEN >*/
                } else if (mb == 1 && nb == 2) {

/*                 Build a 4-by-4 system Z' * x = RHS */

/*<                   Z( 1, 1 ) = A( IS, IS ) >*/
                    z__[0] = a[is + is * a_dim1];
/*<                   Z( 2, 1 ) = ZERO >*/
                    z__[1] = 0.;
/*<                   Z( 3, 1 ) = -B( JS, JS ) >*/
                    z__[2] = -b[js + js * b_dim1];
/*<                   Z( 4, 1 ) = -B( JSP1, JS ) >*/
                    z__[3] = -b[jsp1 + js * b_dim1];

/*<                   Z( 1, 2 ) = ZERO >*/
                    z__[8] = 0.;
/*<                   Z( 2, 2 ) = A( IS, IS ) >*/
                    z__[9] = a[is + is * a_dim1];
/*<                   Z( 3, 2 ) = -B( JS, JSP1 ) >*/
                    z__[10] = -b[js + jsp1 * b_dim1];
/*<                   Z( 4, 2 ) = -B( JSP1, JSP1 ) >*/
                    z__[11] = -b[jsp1 + jsp1 * b_dim1];

/*<                   Z( 1, 3 ) = D( IS, IS ) >*/
                    z__[16] = d__[is + is * d_dim1];
/*<                   Z( 2, 3 ) = ZERO >*/
                    z__[17] = 0.;
/*<                   Z( 3, 3 ) = -E( JS, JS ) >*/
                    z__[18] = -e[js + js * e_dim1];
/*<                   Z( 4, 3 ) = ZERO >*/
                    z__[19] = 0.;

/*<                   Z( 1, 4 ) = ZERO >*/
                    z__[24] = 0.;
/*<                   Z( 2, 4 ) = D( IS, IS ) >*/
                    z__[25] = d__[is + is * d_dim1];
/*<                   Z( 3, 4 ) = -E( JS, JSP1 ) >*/
                    z__[26] = -e[js + jsp1 * e_dim1];
/*<                   Z( 4, 4 ) = -E( JSP1, JSP1 ) >*/
                    z__[27] = -e[jsp1 + jsp1 * e_dim1];

/*                 Set up right hand side(s) */

/*<                   RHS( 1 ) = C( IS, JS ) >*/
                    rhs[0] = c__[is + js * c_dim1];
/*<                   RHS( 2 ) = C( IS, JSP1 ) >*/
                    rhs[1] = c__[is + jsp1 * c_dim1];
/*<                   RHS( 3 ) = F( IS, JS ) >*/
                    rhs[2] = f[is + js * f_dim1];
/*<                   RHS( 4 ) = F( IS, JSP1 ) >*/
                    rhs[3] = f[is + jsp1 * f_dim1];

/*                 Solve Z' * x = RHS */

/*<                   CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR ) >*/
                    dgetc2_(&zdim, z__, &c__8, ipiv, jpiv, &ierr);
/*<    >*/
                    if (ierr > 0) {
                        *info = ierr;
                    }
/*<                   CALL DGESC2( ZDIM, Z, LDZ, RHS, IPIV, JPIV, SCALOC ) >*/
                    dgesc2_(&zdim, z__, &c__8, rhs, ipiv, jpiv, &scaloc);
/*<                   IF( SCALOC.NE.ONE ) THEN >*/
                    if (scaloc != 1.) {
/*<                      DO 140 K = 1, N >*/
                        i__3 = *n;
                        for (k = 1; k <= i__3; ++k) {
/*<                         CALL DSCAL( M, SCALOC, C( 1, K ), 1 ) >*/
                            dscal_(m, &scaloc, &c__[k * c_dim1 + 1], &c__1);
/*<                         CALL DSCAL( M, SCALOC, F( 1, K ), 1 ) >*/
                            dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
/*<   140                CONTINUE >*/
/* L140: */
                        }
/*<                      SCALE = SCALE*SCALOC >*/
                        *scale *= scaloc;
/*<                   END IF >*/
                    }

/*                 Unpack solution vector(s) */

/*<                   C( IS, JS ) = RHS( 1 ) >*/
                    c__[is + js * c_dim1] = rhs[0];
/*<                   C( IS, JSP1 ) = RHS( 2 ) >*/
                    c__[is + jsp1 * c_dim1] = rhs[1];
/*<                   F( IS, JS ) = RHS( 3 ) >*/
                    f[is + js * f_dim1] = rhs[2];
/*<                   F( IS, JSP1 ) = RHS( 4 ) >*/
                    f[is + jsp1 * f_dim1] = rhs[3];

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

/*<                   IF( J.GT.P+2 ) THEN >*/
                    if (j > p + 2) {
/*<    >*/
                        i__3 = js - 1;
                        daxpy_(&i__3, rhs, &b[js * b_dim1 + 1], &c__1, &f[is
                                + f_dim1], ldf);
/*<    >*/
                        i__3 = js - 1;
                        daxpy_(&i__3, &rhs[1], &b[jsp1 * b_dim1 + 1], &c__1, &
                                f[is + f_dim1], ldf);
/*<    >*/
                        i__3 = js - 1;
                        daxpy_(&i__3, &rhs[2], &e[js * e_dim1 + 1], &c__1, &f[
                                is + f_dim1], ldf);
/*<    >*/
                        i__3 = js - 1;
                        daxpy_(&i__3, &rhs[3], &e[jsp1 * e_dim1 + 1], &c__1, &
                                f[is + f_dim1], ldf);
/*<                   END IF >*/
                    }
/*<                   IF( I.LT.P ) THEN >*/
                    if (i__ < p) {
/*<    >*/
                        i__3 = *m - ie;
                        dger_(&i__3, &nb, &c_b27, &a[is + (ie + 1) * a_dim1],
                                lda, rhs, &c__1, &c__[ie + 1 + js * c_dim1],
                                ldc);
/*<    >*/
                        i__3 = *m - ie;
                        dger_(&i__3, &nb, &c_b27, &d__[is + (ie + 1) * d_dim1]
                                , ldd, &rhs[2], &c__1, &c__[ie + 1 + js *
                                c_dim1], ldc);
/*<                   END IF >*/
                    }

/*<                ELSE IF( ( MB.EQ.2 ) .AND. ( NB.EQ.1 ) ) THEN >*/
                } else if (mb == 2 && nb == 1) {

/*                 Build a 4-by-4 system Z' * x = RHS */

/*<                   Z( 1, 1 ) = A( IS, IS ) >*/
                    z__[0] = a[is + is * a_dim1];
/*<                   Z( 2, 1 ) = A( IS, ISP1 ) >*/
                    z__[1] = a[is + isp1 * a_dim1];
/*<                   Z( 3, 1 ) = -B( JS, JS ) >*/
                    z__[2] = -b[js + js * b_dim1];
/*<                   Z( 4, 1 ) = ZERO >*/
                    z__[3] = 0.;

/*<                   Z( 1, 2 ) = A( ISP1, IS ) >*/
                    z__[8] = a[isp1 + is * a_dim1];
/*<                   Z( 2, 2 ) = A( ISP1, ISP1 ) >*/
                    z__[9] = a[isp1 + isp1 * a_dim1];
/*<                   Z( 3, 2 ) = ZERO >*/
                    z__[10] = 0.;
/*<                   Z( 4, 2 ) = -B( JS, JS ) >*/
                    z__[11] = -b[js + js * b_dim1];

/*<                   Z( 1, 3 ) = D( IS, IS ) >*/
                    z__[16] = d__[is + is * d_dim1];
/*<                   Z( 2, 3 ) = D( IS, ISP1 ) >*/
                    z__[17] = d__[is + isp1 * d_dim1];
/*<                   Z( 3, 3 ) = -E( JS, JS ) >*/
                    z__[18] = -e[js + js * e_dim1];
/*<                   Z( 4, 3 ) = ZERO >*/
                    z__[19] = 0.;

/*<                   Z( 1, 4 ) = ZERO >*/
                    z__[24] = 0.;
/*<                   Z( 2, 4 ) = D( ISP1, ISP1 ) >*/
                    z__[25] = d__[isp1 + isp1 * d_dim1];
/*<                   Z( 3, 4 ) = ZERO >*/
                    z__[26] = 0.;
/*<                   Z( 4, 4 ) = -E( JS, JS ) >*/
                    z__[27] = -e[js + js * e_dim1];

/*                 Set up right hand side(s) */

/*<                   RHS( 1 ) = C( IS, JS ) >*/
                    rhs[0] = c__[is + js * c_dim1];
/*<                   RHS( 2 ) = C( ISP1, JS ) >*/
                    rhs[1] = c__[isp1 + js * c_dim1];
/*<                   RHS( 3 ) = F( IS, JS ) >*/
                    rhs[2] = f[is + js * f_dim1];
/*<                   RHS( 4 ) = F( ISP1, JS ) >*/
                    rhs[3] = f[isp1 + js * f_dim1];

/*                 Solve Z' * x = RHS */

/*<                   CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR ) >*/
                    dgetc2_(&zdim, z__, &c__8, ipiv, jpiv, &ierr);
/*<    >*/
                    if (ierr > 0) {
                        *info = ierr;
                    }

/*<                   CALL DGESC2( ZDIM, Z, LDZ, RHS, IPIV, JPIV, SCALOC ) >*/
                    dgesc2_(&zdim, z__, &c__8, rhs, ipiv, jpiv, &scaloc);
/*<                   IF( SCALOC.NE.ONE ) THEN >*/
                    if (scaloc != 1.) {
/*<                      DO 150 K = 1, N >*/
                        i__3 = *n;
                        for (k = 1; k <= i__3; ++k) {
/*<                         CALL DSCAL( M, SCALOC, C( 1, K ), 1 ) >*/
                            dscal_(m, &scaloc, &c__[k * c_dim1 + 1], &c__1);
/*<                         CALL DSCAL( M, SCALOC, F( 1, K ), 1 ) >*/
                            dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
/*<   150                CONTINUE >*/
/* L150: */
                        }
/*<                      SCALE = SCALE*SCALOC >*/
                        *scale *= scaloc;
/*<                   END IF >*/
                    }

/*                 Unpack solution vector(s) */

/*<                   C( IS, JS ) = RHS( 1 ) >*/
                    c__[is + js * c_dim1] = rhs[0];
/*<                   C( ISP1, JS ) = RHS( 2 ) >*/
                    c__[isp1 + js * c_dim1] = rhs[1];
/*<                   F( IS, JS ) = RHS( 3 ) >*/
                    f[is + js * f_dim1] = rhs[2];
/*<                   F( ISP1, JS ) = RHS( 4 ) >*/
                    f[isp1 + js * f_dim1] = rhs[3];

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

/*<                   IF( J.GT.P+2 ) THEN >*/
                    if (j > p + 2) {
/*<    >*/
                        i__3 = js - 1;
                        dger_(&mb, &i__3, &c_b42, rhs, &c__1, &b[js * b_dim1
                                + 1], &c__1, &f[is + f_dim1], ldf);
/*<    >*/
                        i__3 = js - 1;
                        dger_(&mb, &i__3, &c_b42, &rhs[2], &c__1, &e[js *
                                e_dim1 + 1], &c__1, &f[is + f_dim1], ldf);
/*<                   END IF >*/
                    }
/*<                   IF( I.LT.P ) THEN >*/
                    if (i__ < p) {
/*<    >*/
                        i__3 = *m - ie;
                        dgemv_("T", &mb, &i__3, &c_b27, &a[is + (ie + 1) *
                                a_dim1], lda, rhs, &c__1, &c_b42, &c__[ie + 1
                                + js * c_dim1], &c__1, (ftnlen)1);
/*<    >*/
                        i__3 = *m - ie;
                        dgemv_("T", &mb, &i__3, &c_b27, &d__[is + (ie + 1) *
                                d_dim1], ldd, &rhs[2], &c__1, &c_b42, &c__[ie
                                + 1 + js * c_dim1], &c__1, (ftnlen)1);
/*<                   END IF >*/
                    }

/*<                ELSE IF( ( MB.EQ.2 ) .AND. ( NB.EQ.2 ) ) THEN >*/
                } else if (mb == 2 && nb == 2) {

/*                 Build an 8-by-8 system Z' * x = RHS */

/*<                   CALL DCOPY( LDZ*LDZ, ZERO, 0, Z, 1 ) >*/
                    dcopy_(&c__64, &c_b54, &c__0, z__, &c__1);

/*<                   Z( 1, 1 ) = A( IS, IS ) >*/
                    z__[0] = a[is + is * a_dim1];
/*<                   Z( 2, 1 ) = A( IS, ISP1 ) >*/
                    z__[1] = a[is + isp1 * a_dim1];
/*<                   Z( 5, 1 ) = -B( JS, JS ) >*/
                    z__[4] = -b[js + js * b_dim1];
/*<                   Z( 7, 1 ) = -B( JSP1, JS ) >*/
                    z__[6] = -b[jsp1 + js * b_dim1];

/*<                   Z( 1, 2 ) = A( ISP1, IS ) >*/
                    z__[8] = a[isp1 + is * a_dim1];
/*<                   Z( 2, 2 ) = A( ISP1, ISP1 ) >*/
                    z__[9] = a[isp1 + isp1 * a_dim1];
/*<                   Z( 6, 2 ) = -B( JS, JS ) >*/
                    z__[13] = -b[js + js * b_dim1];
/*<                   Z( 8, 2 ) = -B( JSP1, JS ) >*/
                    z__[15] = -b[jsp1 + js * b_dim1];

/*<                   Z( 3, 3 ) = A( IS, IS ) >*/
                    z__[18] = a[is + is * a_dim1];
/*<                   Z( 4, 3 ) = A( IS, ISP1 ) >*/
                    z__[19] = a[is + isp1 * a_dim1];
/*<                   Z( 5, 3 ) = -B( JS, JSP1 ) >*/
                    z__[20] = -b[js + jsp1 * b_dim1];
/*<                   Z( 7, 3 ) = -B( JSP1, JSP1 ) >*/
                    z__[22] = -b[jsp1 + jsp1 * b_dim1];

/*<                   Z( 3, 4 ) = A( ISP1, IS ) >*/
                    z__[26] = a[isp1 + is * a_dim1];
/*<                   Z( 4, 4 ) = A( ISP1, ISP1 ) >*/
                    z__[27] = a[isp1 + isp1 * a_dim1];
/*<                   Z( 6, 4 ) = -B( JS, JSP1 ) >*/
                    z__[29] = -b[js + jsp1 * b_dim1];
/*<                   Z( 8, 4 ) = -B( JSP1, JSP1 ) >*/
                    z__[31] = -b[jsp1 + jsp1 * b_dim1];

/*<                   Z( 1, 5 ) = D( IS, IS ) >*/
                    z__[32] = d__[is + is * d_dim1];
/*<                   Z( 2, 5 ) = D( IS, ISP1 ) >*/
                    z__[33] = d__[is + isp1 * d_dim1];
/*<                   Z( 5, 5 ) = -E( JS, JS ) >*/
                    z__[36] = -e[js + js * e_dim1];

/*<                   Z( 2, 6 ) = D( ISP1, ISP1 ) >*/
                    z__[41] = d__[isp1 + isp1 * d_dim1];
/*<                   Z( 6, 6 ) = -E( JS, JS ) >*/
                    z__[45] = -e[js + js * e_dim1];

/*<                   Z( 3, 7 ) = D( IS, IS ) >*/
                    z__[50] = d__[is + is * d_dim1];
/*<                   Z( 4, 7 ) = D( IS, ISP1 ) >*/
                    z__[51] = d__[is + isp1 * d_dim1];
/*<                   Z( 5, 7 ) = -E( JS, JSP1 ) >*/
                    z__[52] = -e[js + jsp1 * e_dim1];
/*<                   Z( 7, 7 ) = -E( JSP1, JSP1 ) >*/
                    z__[54] = -e[jsp1 + jsp1 * e_dim1];

/*<                   Z( 4, 8 ) = D( ISP1, ISP1 ) >*/
                    z__[59] = d__[isp1 + isp1 * d_dim1];
/*<                   Z( 6, 8 ) = -E( JS, JSP1 ) >*/
                    z__[61] = -e[js + jsp1 * e_dim1];
/*<                   Z( 8, 8 ) = -E( JSP1, JSP1 ) >*/
                    z__[63] = -e[jsp1 + jsp1 * e_dim1];

/*                 Set up right hand side(s) */

/*<                   K = 1 >*/
                    k = 1;
/*<                   II = MB*NB + 1 >*/
                    ii = mb * nb + 1;
/*<                   DO 160 JJ = 0, NB - 1 >*/
                    i__3 = nb - 1;
                    for (jj = 0; jj <= i__3; ++jj) {
/*<                      CALL DCOPY( MB, C( IS, JS+JJ ), 1, RHS( K ), 1 ) >*/
                        dcopy_(&mb, &c__[is + (js + jj) * c_dim1], &c__1, &
                                rhs[k - 1], &c__1);
/*<                      CALL DCOPY( MB, F( IS, JS+JJ ), 1, RHS( II ), 1 ) >*/
                        dcopy_(&mb, &f[is + (js + jj) * f_dim1], &c__1, &rhs[
                                ii - 1], &c__1);
/*<                      K = K + MB >*/
                        k += mb;
/*<                      II = II + MB >*/
                        ii += mb;
/*<   160             CONTINUE >*/
/* L160: */
                    }


/*                 Solve Z' * x = RHS */

/*<                   CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR ) >*/
                    dgetc2_(&zdim, z__, &c__8, ipiv, jpiv, &ierr);
/*<    >*/
                    if (ierr > 0) {
                        *info = ierr;
                    }

/*<                   CALL DGESC2( ZDIM, Z, LDZ, RHS, IPIV, JPIV, SCALOC ) >*/
                    dgesc2_(&zdim, z__, &c__8, rhs, ipiv, jpiv, &scaloc);
/*<                   IF( SCALOC.NE.ONE ) THEN >*/
                    if (scaloc != 1.) {
/*<                      DO 170 K = 1, N >*/
                        i__3 = *n;
                        for (k = 1; k <= i__3; ++k) {
/*<                         CALL DSCAL( M, SCALOC, C( 1, K ), 1 ) >*/
                            dscal_(m, &scaloc, &c__[k * c_dim1 + 1], &c__1);
/*<                         CALL DSCAL( M, SCALOC, F( 1, K ), 1 ) >*/
                            dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
/*<   170                CONTINUE >*/
/* L170: */
                        }
/*<                      SCALE = SCALE*SCALOC >*/
                        *scale *= scaloc;
/*<                   END IF >*/
                    }

/*                 Unpack solution vector(s) */

/*<                   K = 1 >*/
                    k = 1;
/*<                   II = MB*NB + 1 >*/
                    ii = mb * nb + 1;
/*<                   DO 180 JJ = 0, NB - 1 >*/
                    i__3 = nb - 1;
                    for (jj = 0; jj <= i__3; ++jj) {
/*<                      CALL DCOPY( MB, RHS( K ), 1, C( IS, JS+JJ ), 1 ) >*/
                        dcopy_(&mb, &rhs[k - 1], &c__1, &c__[is + (js + jj) *
                                c_dim1], &c__1);
/*<                      CALL DCOPY( MB, RHS( II ), 1, F( IS, JS+JJ ), 1 ) >*/
                        dcopy_(&mb, &rhs[ii - 1], &c__1, &f[is + (js + jj) *
                                f_dim1], &c__1);
/*<                      K = K + MB >*/
                        k += mb;
/*<                      II = II + MB >*/
                        ii += mb;
/*<   180             CONTINUE >*/
/* L180: */
                    }

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

/*<                   IF( J.GT.P+2 ) THEN >*/
                    if (j > p + 2) {
/*<    >*/
                        i__3 = js - 1;
                        dgemm_("N", "T", &mb, &i__3, &nb, &c_b42, &c__[is +
                                js * c_dim1], ldc, &b[js * b_dim1 + 1], ldb, &
                                c_b42, &f[is + f_dim1], ldf, (ftnlen)1, (
                                ftnlen)1);
/*<    >*/
                        i__3 = js - 1;
                        dgemm_("N", "T", &mb, &i__3, &nb, &c_b42, &f[is + js *
                                 f_dim1], ldf, &e[js * e_dim1 + 1], lde, &
                                c_b42, &f[is + f_dim1], ldf, (ftnlen)1, (
                                ftnlen)1);
/*<                   END IF >*/
                    }
/*<                   IF( I.LT.P ) THEN >*/
                    if (i__ < p) {
/*<    >*/
                        i__3 = *m - ie;
                        dgemm_("T", "N", &i__3, &nb, &mb, &c_b27, &a[is + (ie
                                + 1) * a_dim1], lda, &c__[is + js * c_dim1],
                                ldc, &c_b42, &c__[ie + 1 + js * c_dim1], ldc,
                                (ftnlen)1, (ftnlen)1);
/*<    >*/
                        i__3 = *m - ie;
                        dgemm_("T", "N", &i__3, &nb, &mb, &c_b27, &d__[is + (
                                ie + 1) * d_dim1], ldd, &f[is + js * f_dim1],
                                ldf, &c_b42, &c__[ie + 1 + js * c_dim1], ldc,
                                (ftnlen)1, (ftnlen)1);
/*<                   END IF >*/
                    }

/*<                END IF >*/
                }

/*<   190       CONTINUE >*/
/* L190: */
            }
/*<   200    CONTINUE >*/
/* L200: */
        }

/*<       END IF >*/
    }
/*<       RETURN >*/
    return 0;

/*     End of DTGSY2 */

/*<       END >*/
} /* dtgsy2_ */

#ifdef __cplusplus
        }
#endif
