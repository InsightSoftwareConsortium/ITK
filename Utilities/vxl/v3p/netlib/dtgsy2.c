#include "f2c.h"
#include "netlib.h"

/* Table of constant values */
static integer c__8 = 8;
static integer c__1 = 1;
static doublereal c_b27 = -1.;
static doublereal c_b42 = 1.;
static integer c__64 = 64;
static doublereal c_b54 = 0.;
static integer c__0 = 0;

/* Subroutine */ void dtgsy2_(trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,
         ldd, e, lde, f, ldf, scale, rdsum, rdscal, iwork, pq, info)
char *trans;
integer *ijob, *m, *n;
doublereal *a;
integer *lda;
doublereal *b;
integer *ldb;
doublereal *c;
integer *ldc;
doublereal *d;
integer *ldd;
doublereal *e;
integer *lde;
doublereal *f;
integer *ldf;
doublereal *scale, *rdsum, *rdscal;
integer *iwork, *pq, *info;
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, d_dim1,
            d_offset, e_dim1, e_offset, f_dim1, f_offset, i__1, i__2, i__3;

    /* Local variables */
    static integer ierr, zdim, ipiv[8], jpiv[8], i, j, k, p, q;
    static doublereal alpha;
    static doublereal z[64] /* was [8][8] */;
    static integer ie, je, mb, nb, ii, jj, is, js;
    static doublereal scaloc;
    static logical notran;
    static doublereal rhs[8];
    static integer isp1, jsp1;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

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

    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    c_dim1 = *ldc;
    c_offset = 1 + c_dim1 * 1;
    c -= c_offset;
    d_dim1 = *ldd;
    d_offset = 1 + d_dim1 * 1;
    d -= d_offset;
    e_dim1 = *lde;
    e_offset = 1 + e_dim1 * 1;
    e -= e_offset;
    f_dim1 = *ldf;
    f_offset = 1 + f_dim1 * 1;
    f -= f_offset;
    --iwork;

/*     Decode and test input parameters */

    *info = 0;
    ierr = 0;
    notran = lsame_(trans, "N");
    if (! notran && ! lsame_(trans, "T")) {
        *info = -1;
    } else if (*ijob < 0 || *ijob > 2) {
        *info = -2;
    } else if (*m <= 0) {
        *info = -3;
    } else if (*n <= 0) {
        *info = -4;
    } else if (*lda < max(1,*m)) {
        *info = -5;
    } else if (*ldb < max(1,*n)) {
        *info = -8;
    } else if (*ldc < max(1,*m)) {
        *info = -10;
    } else if (*ldd < max(1,*m)) {
        *info = -12;
    } else if (*lde < max(1,*n)) {
        *info = -14;
    } else if (*ldf < max(1,*m)) {
        *info = -16;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("DTGSY2", &i__1);
        return;
    }

/*     Determine block structure of A */

    *pq = 0;
    p = 0;
    i = 1;
L10:
    if (i > *m) {
        goto L20;
    }
    ++p;
    iwork[p] = i;
    if (i == *m) {
        goto L20;
    }
    if (a[i + 1 + i * a_dim1] != 0.) {
        i += 2;
    } else {
        ++i;
    }
    goto L10;
L20:
    iwork[p + 1] = *m + 1;

/*     Determine block structure of B */

    q = p + 1;
    j = 1;
L30:
    if (j > *n) {
        goto L40;
    }
    ++q;
    iwork[q] = j;
    if (j == *n) {
        goto L40;
    }
    if (b[j + 1 + j * b_dim1] != 0.) {
        j += 2;
    } else {
        ++j;
    }
    goto L30;
L40:
    iwork[q + 1] = *n + 1;
    *pq = p * (q - p - 1);

    if (notran) {

/*        Solve (I, J) - subsystem */
/*           A(I, I) * R(I, J) - L(I, J) * B(J, J) = C(I, J) */
/*           D(I, I) * R(I, J) - L(I, J) * E(J, J) = F(I, J) */
/*        for I = P, P - 1, ..., 1; J = 1, 2, ..., Q */

        *scale = 1.;
        scaloc = 1.;
        i__1 = q;
        for (j = p + 2; j <= i__1; ++j) {
            js = iwork[j];
            jsp1 = js + 1;
            je = iwork[j + 1] - 1;
            nb = je - js + 1;
            for (i = p; i >= 1; --i) {

                is = iwork[i];
                isp1 = is + 1;
                ie = iwork[i + 1] - 1;
                mb = ie - is + 1;
                zdim = mb * nb << 1;

                if (mb == 1 && nb == 1) {

/*                 Build a 2-by-2 system Z * x = RHS */

                    z[0] = a[is + is * a_dim1];
                    z[1] = d[is + is * d_dim1];
                    z[8] = -b[js + js * b_dim1];
                    z[9] = -e[js + js * e_dim1];

/*                 Set up right hand side(s) */

                    rhs[0] = c[is + js * c_dim1];
                    rhs[1] = f[is + js * f_dim1];

/*                 Solve Z * x = RHS */

                    dgetc2_(&zdim, z, &c__8, ipiv, jpiv, &ierr);
                    if (ierr > 0) {
                        *info = ierr;
                    }

                    if (*ijob == 0) {
                        dgesc2_(&zdim, z, &c__8, rhs, ipiv, jpiv, &scaloc);
                        if (scaloc != 1.) {
                            i__2 = *n;
                            for (k = 1; k <= i__2; ++k) {
                                dscal_(m, &scaloc, &c[k * c_dim1 + 1], &c__1);
                                dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
                            }
                            *scale *= scaloc;
                        }
                    } else {
                        dlatdf_(ijob, &zdim, z, &c__8, rhs, rdsum, rdscal, ipiv, jpiv);
                    }

/*                 Unpack solution vector(s) */

                    c[is + js * c_dim1] = rhs[0];
                    f[is + js * f_dim1] = rhs[1];

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

                    if (i > 1) {
                        alpha = -rhs[0];
                        i__2 = is - 1;
                        daxpy_(&i__2, &alpha, &a[is * a_dim1 + 1], &c__1, &c[js * c_dim1 + 1], &c__1);
                        i__2 = is - 1;
                        daxpy_(&i__2, &alpha, &d[is * d_dim1 + 1], &c__1, &f[js * f_dim1 + 1], &c__1);
                    }
                    if (j < q) {
                        i__2 = *n - je;
                        daxpy_(&i__2, &rhs[1], &b[js + (je + 1) * b_dim1], ldb, &c[is + (je + 1) * c_dim1], ldc);
                        i__2 = *n - je;
                        daxpy_(&i__2, &rhs[1], &e[js + (je + 1) * e_dim1], lde, &f[is + (je + 1) * f_dim1], ldf);
                    }

                } else if (mb == 1 && nb == 2) {

/*                 Build a 4-by-4 system Z * x = RHS */

                    z[0] = a[is + is * a_dim1];
                    z[1] = 0.;
                    z[2] = d[is + is * d_dim1];
                    z[3] = 0.;

                    z[8] = 0.;
                    z[9] = a[is + is * a_dim1];
                    z[10] = 0.;
                    z[11] = d[is + is * d_dim1];

                    z[16] = -b[js + js * b_dim1];
                    z[17] = -b[js + jsp1 * b_dim1];
                    z[18] = -e[js + js * e_dim1];
                    z[19] = -e[js + jsp1 * e_dim1];

                    z[24] = -b[jsp1 + js * b_dim1];
                    z[25] = -b[jsp1 + jsp1 * b_dim1];
                    z[26] = 0.;
                    z[27] = -e[jsp1 + jsp1 * e_dim1];

/*                 Set up right hand side(s) */

                    rhs[0] = c[is + js * c_dim1];
                    rhs[1] = c[is + jsp1 * c_dim1];
                    rhs[2] = f[is + js * f_dim1];
                    rhs[3] = f[is + jsp1 * f_dim1];

/*                 Solve Z * x = RHS */

                    dgetc2_(&zdim, z, &c__8, ipiv, jpiv, &ierr);
                    if (ierr > 0) {
                        *info = ierr;
                    }

                    if (*ijob == 0) {
                        dgesc2_(&zdim, z, &c__8, rhs, ipiv, jpiv, &scaloc);
                        if (scaloc != 1.) {
                            i__2 = *n;
                            for (k = 1; k <= i__2; ++k) {
                                dscal_(m, &scaloc, &c[k * c_dim1 + 1], &c__1);
                                dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
                            }
                            *scale *= scaloc;
                        }
                    } else {
                        dlatdf_(ijob, &zdim, z, &c__8, rhs, rdsum, rdscal, ipiv, jpiv);
                    }

/*                 Unpack solution vector(s) */

                    c[is + js * c_dim1] = rhs[0];
                    c[is + jsp1 * c_dim1] = rhs[1];
                    f[is + js * f_dim1] = rhs[2];
                    f[is + jsp1 * f_dim1] = rhs[3];

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

                    if (i > 1) {
                        i__2 = is - 1;
                        dger_(&i__2, &nb, &c_b27, &a[is * a_dim1 + 1], &c__1, rhs, &c__1, &c[js * c_dim1 + 1], ldc);
                        i__2 = is - 1;
                        dger_(&i__2, &nb, &c_b27, &d[is * d_dim1 + 1], &c__1, rhs, &c__1, &f[js * f_dim1 + 1], ldf);
                    }
                    if (j < q) {
                        i__2 = *n - je;
                        daxpy_(&i__2, &rhs[2], &b[js + (je + 1) * b_dim1], ldb, &c[is + (je + 1) * c_dim1], ldc);
                        i__2 = *n - je;
                        daxpy_(&i__2, &rhs[2], &e[js + (je + 1) * e_dim1], lde, &f[is + (je + 1) * f_dim1], ldf);
                        i__2 = *n - je;
                        daxpy_(&i__2, &rhs[3], &b[jsp1 + (je + 1) * b_dim1], ldb, &c[is + (je + 1) * c_dim1], ldc);
                        i__2 = *n - je;
                        daxpy_(&i__2, &rhs[3], &e[jsp1 + (je + 1) * e_dim1], lde, &f[is + (je + 1) * f_dim1], ldf);
                    }

                } else if (mb == 2 && nb == 1) {

/*                 Build a 4-by-4 system Z * x = RHS */

                    z[0] = a[is + is * a_dim1];
                    z[1] = a[isp1 + is * a_dim1];
                    z[2] = d[is + is * d_dim1];
                    z[3] = 0.;

                    z[8] = a[is + isp1 * a_dim1];
                    z[9] = a[isp1 + isp1 * a_dim1];
                    z[10] = d[is + isp1 * d_dim1];
                    z[11] = d[isp1 + isp1 * d_dim1];

                    z[16] = -b[js + js * b_dim1];
                    z[17] = 0.;
                    z[18] = -e[js + js * e_dim1];
                    z[19] = 0.;

                    z[24] = 0.;
                    z[25] = -b[js + js * b_dim1];
                    z[26] = 0.;
                    z[27] = -e[js + js * e_dim1];

/*                 Set up right hand side(s) */

                    rhs[0] = c[is + js * c_dim1];
                    rhs[1] = c[isp1 + js * c_dim1];
                    rhs[2] = f[is + js * f_dim1];
                    rhs[3] = f[isp1 + js * f_dim1];

/*                 Solve Z * x = RHS */

                    dgetc2_(&zdim, z, &c__8, ipiv, jpiv, &ierr);
                    if (ierr > 0) {
                        *info = ierr;
                    }
                    if (*ijob == 0) {
                        dgesc2_(&zdim, z, &c__8, rhs, ipiv, jpiv, &scaloc);
                        if (scaloc != 1.) {
                            i__2 = *n;
                            for (k = 1; k <= i__2; ++k) {
                                dscal_(m, &scaloc, &c[k * c_dim1 + 1], &c__1);
                                dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
                            }
                            *scale *= scaloc;
                        }
                    } else {
                        dlatdf_(ijob, &zdim, z, &c__8, rhs, rdsum, rdscal, ipiv, jpiv);
                    }

/*                 Unpack solution vector(s) */

                    c[is + js * c_dim1] = rhs[0];
                    c[isp1 + js * c_dim1] = rhs[1];
                    f[is + js * f_dim1] = rhs[2];
                    f[isp1 + js * f_dim1] = rhs[3];

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

                    if (i > 1) {
                        i__2 = is - 1;
                        dgemv_("N", &i__2, &mb, &c_b27, &a[is * a_dim1 + 1], lda,
                               rhs, &c__1, &c_b42, &c[js * c_dim1 + 1], &c__1);
                        i__2 = is - 1;
                        dgemv_("N", &i__2, &mb, &c_b27, &d[is * d_dim1 + 1], ldd,
                               rhs, &c__1, &c_b42, &f[js * f_dim1 + 1], &c__1);
                    }
                    if (j < q) {
                        i__2 = *n - je;
                        dger_(&mb, &i__2, &c_b42, &rhs[2], &c__1,
                              &b[js + (je + 1) * b_dim1], ldb, &c[is + (je + 1) * c_dim1], ldc);
                        i__2 = *n - je;
                        dger_(&mb, &i__2, &c_b42, &rhs[2], &c__1,
                              &e[js + (je + 1) * e_dim1], ldb, &f[is + (je + 1) * f_dim1], ldc);
                    }

                } else if (mb == 2 && nb == 2) {

/*                 Build an 8-by-8 system Z * x = RHS */

                    dcopy_(&c__64, &c_b54, &c__0, z, &c__1);

                    z[0] = a[is + is * a_dim1];
                    z[1] = a[isp1 + is * a_dim1];
                    z[4] = d[is + is * d_dim1];

                    z[8] = a[is + isp1 * a_dim1];
                    z[9] = a[isp1 + isp1 * a_dim1];
                    z[12] = d[is + isp1 * d_dim1];
                    z[13] = d[isp1 + isp1 * d_dim1];

                    z[18] = a[is + is * a_dim1];
                    z[19] = a[isp1 + is * a_dim1];
                    z[22] = d[is + is * d_dim1];

                    z[26] = a[is + isp1 * a_dim1];
                    z[27] = a[isp1 + isp1 * a_dim1];
                    z[30] = d[is + isp1 * d_dim1];
                    z[31] = d[isp1 + isp1 * d_dim1];

                    z[32] = -b[js + js * b_dim1];
                    z[34] = -b[js + jsp1 * b_dim1];
                    z[36] = -e[js + js * e_dim1];
                    z[38] = -e[js + jsp1 * e_dim1];

                    z[41] = -b[js + js * b_dim1];
                    z[43] = -b[js + jsp1 * b_dim1];
                    z[45] = -e[js + js * e_dim1];
                    z[47] = -e[js + jsp1 * e_dim1];

                    z[48] = -b[jsp1 + js * b_dim1];
                    z[50] = -b[jsp1 + jsp1 * b_dim1];
                    z[54] = -e[jsp1 + jsp1 * e_dim1];

                    z[57] = -b[jsp1 + js * b_dim1];
                    z[59] = -b[jsp1 + jsp1 * b_dim1];
                    z[63] = -e[jsp1 + jsp1 * e_dim1];

/*                 Set up right hand side(s) */

                    k = 1;
                    ii = mb * nb + 1;
                    i__2 = nb - 1;
                    for (jj = 0; jj <= i__2; ++jj) {
                        dcopy_(&mb, &c[is + (js + jj) * c_dim1], &c__1, &rhs[k - 1], &c__1);
                        dcopy_(&mb, &f[is + (js + jj) * f_dim1], &c__1, &rhs[ii - 1], &c__1);
                        k += mb;
                        ii += mb;
                    }

/*                 Solve Z * x = RHS */

                    dgetc2_(&zdim, z, &c__8, ipiv, jpiv, &ierr);
                    if (ierr > 0) {
                        *info = ierr;
                    }
                    if (*ijob == 0) {
                        dgesc2_(&zdim, z, &c__8, rhs, ipiv, jpiv, &scaloc);
                        if (scaloc != 1.) {
                            i__2 = *n;
                            for (k = 1; k <= i__2; ++k) {
                                dscal_(m, &scaloc, &c[k * c_dim1 + 1], &c__1);
                                dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
                            }
                            *scale *= scaloc;
                        }
                    } else {
                        dlatdf_(ijob, &zdim, z, &c__8, rhs, rdsum, rdscal, ipiv, jpiv);
                    }

/*                 Unpack solution vector(s) */

                    k = 1;
                    ii = mb * nb + 1;
                    i__2 = nb - 1;
                    for (jj = 0; jj <= i__2; ++jj) {
                        dcopy_(&mb, &rhs[k - 1], &c__1, &c[is + (js + jj) * c_dim1], &c__1);
                        dcopy_(&mb, &rhs[ii - 1], &c__1, &f[is + (js + jj) * f_dim1], &c__1);
                        k += mb;
                        ii += mb;
                    }

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

                    if (i > 1) {
                        i__2 = is - 1;
                        dgemm_("N", "N", &i__2, &nb, &mb, &c_b27, &a[is * a_dim1 + 1], lda,
                               rhs, &mb, &c_b42, &c[js * c_dim1 + 1], ldc);
                        i__2 = is - 1;
                        dgemm_("N", "N", &i__2, &nb, &mb, &c_b27, &d[is * d_dim1 + 1], ldd,
                               rhs, &mb, &c_b42, &f[js * f_dim1 + 1], ldf);
                    }
                    if (j < q) {
                        k = mb * nb + 1;
                        i__2 = *n - je;
                        dgemm_("N", "N", &mb, &i__2, &nb, &c_b42, &rhs[k - 1], &mb,
                               &b[js + (je + 1) * b_dim1], ldb, &c_b42,
                               &c[is + (je + 1) * c_dim1], ldc);
                        i__2 = *n - je;
                        dgemm_("N", "N", &mb, &i__2, &nb, &c_b42, &rhs[k - 1], &mb,
                               &e[js + (je + 1) * e_dim1], lde, &c_b42,
                               &f[is + (je + 1) * f_dim1], ldf);
                    }
                }
            }
        }
    } else {

/*        Solve (I, J) - subsystem */
/*             A(I, I)' * R(I, J) + D(I, I)' * L(J, J)  =  C(I, J) */
/*             R(I, I)  * B(J, J) + L(I, J)  * E(J, J)  = -F(I, J) */
/*        for I = 1, 2, ..., P, J = Q, Q - 1, ..., 1 */

        *scale = 1.;
        scaloc = 1.;
        i__1 = p;
        for (i = 1; i <= i__1; ++i) {

            is = iwork[i];
            isp1 = is + 1;
            ie = iwork[i + 1] - 1;
            mb = ie - is + 1;
            i__2 = p + 2;
            for (j = q; j >= i__2; --j) {

                js = iwork[j];
                jsp1 = js + 1;
                je = iwork[j + 1] - 1;
                nb = je - js + 1;
                zdim = mb * nb << 1;
                if (mb == 1 && nb == 1) {

/*                 Build a 2-by-2 system Z' * x = RHS */

                    z[0] = a[is + is * a_dim1];
                    z[1] = -b[js + js * b_dim1];
                    z[8] = d[is + is * d_dim1];
                    z[9] = -e[js + js * e_dim1];

/*                 Set up right hand side(s) */

                    rhs[0] = c[is + js * c_dim1];
                    rhs[1] = f[is + js * f_dim1];

/*                 Solve Z' * x = RHS */

                    dgetc2_(&zdim, z, &c__8, ipiv, jpiv, &ierr);
                    if (ierr > 0) {
                        *info = ierr;
                    }

                    dgesc2_(&zdim, z, &c__8, rhs, ipiv, jpiv, &scaloc);
                    if (scaloc != 1.) {
                        i__3 = *n;
                        for (k = 1; k <= i__3; ++k) {
                            dscal_(m, &scaloc, &c[k * c_dim1 + 1], &c__1);
                            dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
                        }
                        *scale *= scaloc;
                    }

/*                 Unpack solution vector(s) */

                    c[is + js * c_dim1] = rhs[0];
                    f[is + js * f_dim1] = rhs[1];

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

                    if (j > p + 2) {
                        alpha = rhs[0];
                        i__3 = js - 1;
                        daxpy_(&i__3, &alpha, &b[js * b_dim1 + 1], &c__1, &f[is + f_dim1], ldf);
                        alpha = rhs[1];
                        i__3 = js - 1;
                        daxpy_(&i__3, &alpha, &e[js * e_dim1 + 1], &c__1, &f[is + f_dim1], ldf);
                    }
                    if (i < p) {
                        alpha = -rhs[0];
                        i__3 = *m - ie;
                        daxpy_(&i__3, &alpha, &a[is + (ie + 1) * a_dim1], lda, &c[ie + 1 + js * c_dim1], &c__1);
                        alpha = -rhs[1];
                        i__3 = *m - ie;
                        daxpy_(&i__3, &alpha, &d[is + (ie + 1) * d_dim1], ldd, &c[ie + 1 + js * c_dim1], &c__1);
                    }

                } else if (mb == 1 && nb == 2) {

/*                 Build a 4-by-4 system Z' * x = RHS */

                    z[0] = a[is + is * a_dim1];
                    z[1] = 0.;
                    z[2] = -b[js + js * b_dim1];
                    z[3] = -b[jsp1 + js * b_dim1];

                    z[8] = 0.;
                    z[9] = a[is + is * a_dim1];
                    z[10] = -b[js + jsp1 * b_dim1];
                    z[11] = -b[jsp1 + jsp1 * b_dim1];

                    z[16] = d[is + is * d_dim1];
                    z[17] = 0.;
                    z[18] = -e[js + js * e_dim1];
                    z[19] = 0.;

                    z[24] = 0.;
                    z[25] = d[is + is * d_dim1];
                    z[26] = -e[js + jsp1 * e_dim1];
                    z[27] = -e[jsp1 + jsp1 * e_dim1];

/*                 Set up right hand side(s) */

                    rhs[0] = c[is + js * c_dim1];
                    rhs[1] = c[is + jsp1 * c_dim1];
                    rhs[2] = f[is + js * f_dim1];
                    rhs[3] = f[is + jsp1 * f_dim1];

/*                 Solve Z' * x = RHS */

                    dgetc2_(&zdim, z, &c__8, ipiv, jpiv, &ierr);
                    if (ierr > 0) {
                        *info = ierr;
                    }
                    dgesc2_(&zdim, z, &c__8, rhs, ipiv, jpiv, &scaloc);
                    if (scaloc != 1.) {
                        i__3 = *n;
                        for (k = 1; k <= i__3; ++k) {
                            dscal_(m, &scaloc, &c[k * c_dim1 + 1], &c__1);
                            dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
                        }
                        *scale *= scaloc;
                    }

/*                 Unpack solution vector(s) */

                    c[is + js * c_dim1] = rhs[0];
                    c[is + jsp1 * c_dim1] = rhs[1];
                    f[is + js * f_dim1] = rhs[2];
                    f[is + jsp1 * f_dim1] = rhs[3];

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

                    if (j > p + 2) {
                        i__3 = js - 1;
                        daxpy_(&i__3, rhs, &b[js * b_dim1 + 1], &c__1, &f[is + f_dim1], ldf);
                        i__3 = js - 1;
                        daxpy_(&i__3, &rhs[1], &b[jsp1 * b_dim1 + 1], &c__1, &f[is + f_dim1], ldf);
                        i__3 = js - 1;
                        daxpy_(&i__3, &rhs[2], &e[js * e_dim1 + 1], &c__1, &f[is + f_dim1], ldf);
                        i__3 = js - 1;
                        daxpy_(&i__3, &rhs[3], &e[jsp1 * e_dim1 + 1], &c__1, &f[is + f_dim1], ldf);
                    }
                    if (i < p) {
                        i__3 = *m - ie;
                        dger_(&i__3, &nb, &c_b27, &a[is + (ie + 1) * a_dim1], lda, rhs,
                              &c__1, &c[ie + 1 + js * c_dim1], ldc);
                        i__3 = *m - ie;
                        dger_(&i__3, &nb, &c_b27, &d[is + (ie + 1) * d_dim1], ldd,
                              &rhs[2], &c__1, &c[ie + 1 + js * c_dim1], ldc);
                    }

                } else if (mb == 2 && nb == 1) {

/*                 Build a 4-by-4 system Z' * x = RHS */

                    z[0] = a[is + is * a_dim1];
                    z[1] = a[is + isp1 * a_dim1];
                    z[2] = -b[js + js * b_dim1];
                    z[3] = 0.;

                    z[8] = a[isp1 + is * a_dim1];
                    z[9] = a[isp1 + isp1 * a_dim1];
                    z[10] = 0.;
                    z[11] = -b[js + js * b_dim1];

                    z[16] = d[is + is * d_dim1];
                    z[17] = d[is + isp1 * d_dim1];
                    z[18] = -e[js + js * e_dim1];
                    z[19] = 0.;

                    z[24] = 0.;
                    z[25] = d[isp1 + isp1 * d_dim1];
                    z[26] = 0.;
                    z[27] = -e[js + js * e_dim1];

/*                 Set up right hand side(s) */

                    rhs[0] = c[is + js * c_dim1];
                    rhs[1] = c[isp1 + js * c_dim1];
                    rhs[2] = f[is + js * f_dim1];
                    rhs[3] = f[isp1 + js * f_dim1];

/*                 Solve Z' * x = RHS */

                    dgetc2_(&zdim, z, &c__8, ipiv, jpiv, &ierr);
                    if (ierr > 0) {
                        *info = ierr;
                    }

                    dgesc2_(&zdim, z, &c__8, rhs, ipiv, jpiv, &scaloc);
                    if (scaloc != 1.) {
                        i__3 = *n;
                        for (k = 1; k <= i__3; ++k) {
                            dscal_(m, &scaloc, &c[k * c_dim1 + 1], &c__1);
                            dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
                        }
                        *scale *= scaloc;
                    }

/*                 Unpack solution vector(s) */

                    c[is + js * c_dim1] = rhs[0];
                    c[isp1 + js * c_dim1] = rhs[1];
                    f[is + js * f_dim1] = rhs[2];
                    f[isp1 + js * f_dim1] = rhs[3];

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

                    if (j > p + 2) {
                        i__3 = js - 1;
                        dger_(&mb, &i__3, &c_b42, rhs, &c__1, &b[js * b_dim1 + 1], &c__1, &f[is + f_dim1], ldf);
                        i__3 = js - 1;
                        dger_(&mb, &i__3, &c_b42, &rhs[2], &c__1, &e[js * e_dim1 + 1], &c__1, &f[is + f_dim1], ldf);
                    }
                    if (i < p) {
                        i__3 = *m - ie;
                        dgemv_("T", &mb, &i__3, &c_b27, &a[is + (ie + 1) * a_dim1], lda,
                               rhs, &c__1, &c_b42, &c[ie + 1 + js * c_dim1], &c__1);
                        i__3 = *m - ie;
                        dgemv_("T", &mb, &i__3, &c_b27, &d[is + (ie + 1) * d_dim1], ldd,
                               &rhs[2], &c__1, &c_b42, &c[ie + 1 + js * c_dim1], &c__1);
                    }

                } else if (mb == 2 && nb == 2) {

/*                 Build an 8-by-8 system Z' * x = RHS */

                    dcopy_(&c__64, &c_b54, &c__0, z, &c__1);

                    z[0] = a[is + is * a_dim1];
                    z[1] = a[is + isp1 * a_dim1];
                    z[4] = -b[js + js * b_dim1];
                    z[6] = -b[jsp1 + js * b_dim1];

                    z[8] = a[isp1 + is * a_dim1];
                    z[9] = a[isp1 + isp1 * a_dim1];
                    z[13] = -b[js + js * b_dim1];
                    z[15] = -b[jsp1 + js * b_dim1];

                    z[18] = a[is + is * a_dim1];
                    z[19] = a[is + isp1 * a_dim1];
                    z[20] = -b[js + jsp1 * b_dim1];
                    z[22] = -b[jsp1 + jsp1 * b_dim1];

                    z[26] = a[isp1 + is * a_dim1];
                    z[27] = a[isp1 + isp1 * a_dim1];
                    z[29] = -b[js + jsp1 * b_dim1];
                    z[31] = -b[jsp1 + jsp1 * b_dim1];

                    z[32] = d[is + is * d_dim1];
                    z[33] = d[is + isp1 * d_dim1];
                    z[36] = -e[js + js * e_dim1];

                    z[41] = d[isp1 + isp1 * d_dim1];
                    z[45] = -e[js + js * e_dim1];

                    z[50] = d[is + is * d_dim1];
                    z[51] = d[is + isp1 * d_dim1];
                    z[52] = -e[js + jsp1 * e_dim1];
                    z[54] = -e[jsp1 + jsp1 * e_dim1];

                    z[59] = d[isp1 + isp1 * d_dim1];
                    z[61] = -e[js + jsp1 * e_dim1];
                    z[63] = -e[jsp1 + jsp1 * e_dim1];

/*                 Set up right hand side(s) */

                    k = 1;
                    ii = mb * nb + 1;
                    i__3 = nb - 1;
                    for (jj = 0; jj <= i__3; ++jj) {
                        dcopy_(&mb, &c[is + (js + jj) * c_dim1], &c__1, &rhs[k - 1], &c__1);
                        dcopy_(&mb, &f[is + (js + jj) * f_dim1], &c__1, &rhs[ii - 1], &c__1);
                        k += mb;
                        ii += mb;
                    }


/*                 Solve Z' * x = RHS */

                    dgetc2_(&zdim, z, &c__8, ipiv, jpiv, &ierr);
                    if (ierr > 0) {
                        *info = ierr;
                    }

                    dgesc2_(&zdim, z, &c__8, rhs, ipiv, jpiv, &scaloc);
                    if (scaloc != 1.) {
                        i__3 = *n;
                        for (k = 1; k <= i__3; ++k) {
                            dscal_(m, &scaloc, &c[k * c_dim1 + 1], &c__1);
                            dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
                        }
                        *scale *= scaloc;
                    }

/*                 Unpack solution vector(s) */

                    k = 1;
                    ii = mb * nb + 1;
                    i__3 = nb - 1;
                    for (jj = 0; jj <= i__3; ++jj) {
                        dcopy_(&mb, &rhs[k - 1], &c__1, &c[is + (js + jj) * c_dim1], &c__1);
                        dcopy_(&mb, &rhs[ii - 1], &c__1, &f[is + (js + jj) * f_dim1], &c__1);
                        k += mb;
                        ii += mb;
                    }

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

                    if (j > p + 2) {
                        i__3 = js - 1;
                        dgemm_("N", "T", &mb, &i__3, &nb, &c_b42, &c[is + js * c_dim1], ldc,
                               &b[js * b_dim1 + 1], ldb, &c_b42, &f[is + f_dim1], ldf);
                        i__3 = js - 1;
                        dgemm_("N", "T", &mb, &i__3, &nb, &c_b42, &f[is + js * f_dim1], ldf,
                               &e[js * e_dim1 + 1], lde, &c_b42, &f[is + f_dim1], ldf);
                    }
                    if (i < p) {
                        i__3 = *m - ie;
                        dgemm_("T", "N", &i__3, &nb, &mb, &c_b27, &a[is + (ie + 1) * a_dim1], lda,
                               &c[is + js * c_dim1], ldc, &c_b42, &c[ie + 1 + js * c_dim1], ldc);
                        i__3 = *m - ie;
                        dgemm_("T", "N", &i__3, &nb, &mb, &c_b27, &d[is + (ie + 1) * d_dim1], ldd,
                               &f[is + js * f_dim1], ldf, &c_b42, &c[ie + 1 + js * c_dim1], ldc);
                    }
                }
            }
        }
    }
} /* dtgsy2_ */
