/* lapack/double/dtgsyl.f -- translated by f2c (version 20050501).
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
static integer c_n1 = -1;
static integer c__5 = 5;
static doublereal c_b14 = 0.;
static integer c__0 = 0;
static integer c__1 = 1;
static doublereal c_b53 = -1.;
static doublereal c_b54 = 1.;

/*<    >*/
/* Subroutine */ int dtgsyl_(char *trans, integer *ijob, integer *m, integer *
        n, doublereal *a, integer *lda, doublereal *b, integer *ldb,
        doublereal *c__, integer *ldc, doublereal *d__, integer *ldd,
        doublereal *e, integer *lde, doublereal *f, integer *ldf, doublereal *
        scale, doublereal *dif, doublereal *work, integer *lwork, integer *
        iwork, integer *info, ftnlen trans_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, d_dim1,
            d_offset, e_dim1, e_offset, f_dim1, f_offset, i__1, i__2, i__3,
            i__4;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__, j, k, p, q, ie, je, mb, nb, is, js, pq;
    doublereal dsum;
    integer ppqq;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *), dgemm_(char *, char *, integer *, integer *, integer *
            , doublereal *, doublereal *, integer *, doublereal *, integer *,
            doublereal *, doublereal *, integer *, ftnlen, ftnlen);
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    integer ifunc, linfo;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *,
            doublereal *, integer *);
    integer lwmin;
    doublereal scale2=0;
    extern /* Subroutine */ int dtgsy2_(char *, integer *, integer *, integer
            *, doublereal *, integer *, doublereal *, integer *, doublereal *,
             integer *, doublereal *, integer *, doublereal *, integer *,
            doublereal *, integer *, doublereal *, doublereal *, doublereal *,
             integer *, integer *, integer *, ftnlen);
    doublereal dscale, scaloc;
    extern /* Subroutine */ int dlacpy_(char *, integer *, integer *,
            doublereal *, integer *, doublereal *, integer *, ftnlen),
            xerbla_(char *, integer *, ftnlen);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *,
            integer *, integer *, ftnlen, ftnlen);
    integer iround;
    logical notran;
    integer isolve;
    logical lquery;
    (void)trans_len;

/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          TRANS >*/
/*<    >*/
/*<       DOUBLE PRECISION   DIF, SCALE >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       INTEGER            IWORK( * ) >*/
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DTGSYL solves the generalized Sylvester equation: */

/*              A * R - L * B = scale * C                 (1) */
/*              D * R - L * E = scale * F */

/*  where R and L are unknown m-by-n matrices, (A, D), (B, E) and */
/*  (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n, */
/*  respectively, with real entries. (A, D) and (B, E) must be in */
/*  generalized (real) Schur canonical form, i.e. A, B are upper quasi */
/*  triangular and D, E are upper triangular. */

/*  The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output */
/*  scaling factor chosen to avoid overflow. */

/*  In matrix notation (1) is equivalent to solve  Zx = scale b, where */
/*  Z is defined as */

/*             Z = [ kron(In, A)  -kron(B', Im) ]         (2) */
/*                 [ kron(In, D)  -kron(E', Im) ]. */

/*  Here Ik is the identity matrix of size k and X' is the transpose of */
/*  X. kron(X, Y) is the Kronecker product between the matrices X and Y. */

/*  If TRANS = 'T', DTGSYL solves the transposed system Z'*y = scale*b, */
/*  which is equivalent to solve for R and L in */

/*              A' * R  + D' * L   = scale *  C           (3) */
/*              R  * B' + L  * E'  = scale * (-F) */

/*  This case (TRANS = 'T') is used to compute an one-norm-based estimate */
/*  of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D) */
/*  and (B,E), using DLACON. */

/*  If IJOB >= 1, DTGSYL computes a Frobenius norm-based estimate */
/*  of Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the */
/*  reciprocal of the smallest singular value of Z. See [1-2] for more */
/*  information. */

/*  This is a level 3 BLAS algorithm. */

/*  Arguments */
/*  ========= */

/*  TRANS   (input) CHARACTER*1 */
/*          = 'N', solve the generalized Sylvester equation (1). */
/*          = 'T', solve the 'transposed' system (3). */

/*  IJOB    (input) INTEGER */
/*          Specifies what kind of functionality to be performed. */
/*           =0: solve (1) only. */
/*           =1: The functionality of 0 and 3. */
/*           =2: The functionality of 0 and 4. */
/*           =3: Only an estimate of Dif[(A,D), (B,E)] is computed. */
/*               (look ahead strategy IJOB  = 1 is used). */
/*           =4: Only an estimate of Dif[(A,D), (B,E)] is computed. */
/*               ( DGECON on sub-systems is used ). */
/*          Not referenced if TRANS = 'T'. */

/*  M       (input) INTEGER */
/*          The order of the matrices A and D, and the row dimension of */
/*          the matrices C, F, R and L. */

/*  N       (input) INTEGER */
/*          The order of the matrices B and E, and the column dimension */
/*          of the matrices C, F, R and L. */

/*  A       (input) DOUBLE PRECISION array, dimension (LDA, M) */
/*          The upper quasi triangular matrix A. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A. LDA >= max(1, M). */

/*  B       (input) DOUBLE PRECISION array, dimension (LDB, N) */
/*          The upper quasi triangular matrix B. */

/*  LDB     (input) INTEGER */
/*          The leading dimension of the array B. LDB >= max(1, N). */

/*  C       (input/output) DOUBLE PRECISION array, dimension (LDC, N) */
/*          On entry, C contains the right-hand-side of the first matrix */
/*          equation in (1) or (3). */
/*          On exit, if IJOB = 0, 1 or 2, C has been overwritten by */
/*          the solution R. If IJOB = 3 or 4 and TRANS = 'N', C holds R, */
/*          the solution achieved during the computation of the */
/*          Dif-estimate. */

/*  LDC     (input) INTEGER */
/*          The leading dimension of the array C. LDC >= max(1, M). */

/*  D       (input) DOUBLE PRECISION array, dimension (LDD, M) */
/*          The upper triangular matrix D. */

/*  LDD     (input) INTEGER */
/*          The leading dimension of the array D. LDD >= max(1, M). */

/*  E       (input) DOUBLE PRECISION array, dimension (LDE, N) */
/*          The upper triangular matrix E. */

/*  LDE     (input) INTEGER */
/*          The leading dimension of the array E. LDE >= max(1, N). */

/*  F       (input/output) DOUBLE PRECISION array, dimension (LDF, N) */
/*          On entry, F contains the right-hand-side of the second matrix */
/*          equation in (1) or (3). */
/*          On exit, if IJOB = 0, 1 or 2, F has been overwritten by */
/*          the solution L. If IJOB = 3 or 4 and TRANS = 'N', F holds L, */
/*          the solution achieved during the computation of the */
/*          Dif-estimate. */

/*  LDF     (input) INTEGER */
/*          The leading dimension of the array F. LDF >= max(1, M). */

/*  DIF     (output) DOUBLE PRECISION */
/*          On exit DIF is the reciprocal of a lower bound of the */
/*          reciprocal of the Dif-function, i.e. DIF is an upper bound of */
/*          Dif[(A,D), (B,E)] = sigma_min(Z), where Z as in (2). */
/*          IF IJOB = 0 or TRANS = 'T', DIF is not touched. */

/*  SCALE   (output) DOUBLE PRECISION */
/*          On exit SCALE is the scaling factor in (1) or (3). */
/*          If 0 < SCALE < 1, C and F hold the solutions R and L, resp., */
/*          to a slightly perturbed system but the input matrices A, B, D */
/*          and E have not been changed. If SCALE = 0, C and F hold the */
/*          solutions R and L, respectively, to the homogeneous system */
/*          with C = F = 0. Normally, SCALE = 1. */

/*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK) */
/*          If IJOB = 0, WORK is not referenced.  Otherwise, */
/*          on exit, if INFO = 0, WORK(1) returns the optimal LWORK. */

/*  LWORK   (input) INTEGER */
/*          The dimension of the array WORK. LWORK > = 1. */
/*          If IJOB = 1 or 2 and TRANS = 'N', LWORK >= 2*M*N. */

/*          If LWORK = -1, then a workspace query is assumed; the routine */
/*          only calculates the optimal size of the WORK array, returns */
/*          this value as the first entry of the WORK array, and no error */
/*          message related to LWORK is issued by XERBLA. */

/*  IWORK   (workspace) INTEGER array, dimension (M+N+6) */

/*  INFO    (output) INTEGER */
/*            =0: successful exit */
/*            <0: If INFO = -i, the i-th argument had an illegal value. */
/*            >0: (A, D) and (B, E) have common or close eigenvalues. */

/*  Further Details */
/*  =============== */

/*  Based on contributions by */
/*     Bo Kagstrom and Peter Poromaa, Department of Computing Science, */
/*     Umea University, S-901 87 Umea, Sweden. */

/*  [1] B. Kagstrom and P. Poromaa, LAPACK-Style Algorithms and Software */
/*      for Solving the Generalized Sylvester Equation and Estimating the */
/*      Separation between Regular Matrix Pairs, Report UMINF - 93.23, */
/*      Department of Computing Science, Umea University, S-901 87 Umea, */
/*      Sweden, December 1993, Revised April 1994, Also as LAPACK Working */
/*      Note 75.  To appear in ACM Trans. on Math. Software, Vol 22, */
/*      No 1, 1996. */

/*  [2] B. Kagstrom, A Perturbation Analysis of the Generalized Sylvester */
/*      Equation (AR - LB, DR - LE ) = (C, F), SIAM J. Matrix Anal. */
/*      Appl., 15(4):1045-1060, 1994 */

/*  [3] B. Kagstrom and L. Westin, Generalized Schur Methods with */
/*      Condition Estimators for Solving the Generalized Sylvester */
/*      Equation, IEEE Transactions on Automatic Control, Vol. 34, No. 7, */
/*      July 1989, pp 745-751. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            LQUERY, NOTRAN >*/
/*<    >*/
/*<       DOUBLE PRECISION   DSCALE, DSUM, SCALE2, SCALOC >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       INTEGER            ILAENV >*/
/*<       EXTERNAL           LSAME, ILAENV >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DCOPY, DGEMM, DLACPY, DSCAL, DTGSY2, XERBLA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          DBLE, MAX, SQRT >*/
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
    --work;
    --iwork;

    /* Function Body */
    *info = 0;
/*<       NOTRAN = LSAME( TRANS, 'N' ) >*/
    notran = lsame_(trans, "N", (ftnlen)1, (ftnlen)1);
/*<       LQUERY = ( LWORK.EQ.-1 ) >*/
    lquery = *lwork == -1;

/*<       IF( ( IJOB.EQ.1 .OR. IJOB.EQ.2 ) .AND. NOTRAN ) THEN >*/
    if ((*ijob == 1 || *ijob == 2) && notran) {
/*<          LWMIN = MAX( 1, 2*M*N ) >*/
/* Computing MAX */
        i__1 = 1, i__2 = (*m << 1) * *n;
        lwmin = max(i__1,i__2);
/*<       ELSE >*/
    } else {
/*<          LWMIN = 1 >*/
        lwmin = 1;
/*<       END IF >*/
    }

/*<       IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN >*/
    if (! notran && ! lsame_(trans, "T", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( ( IJOB.LT.0 ) .OR. ( IJOB.GT.4 ) ) THEN >*/
    } else if (*ijob < 0 || *ijob > 4) {
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
/*<          INFO = -6 >*/
        *info = -6;
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
/*<       ELSE IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN >*/
    } else if (*lwork < lwmin && ! lquery) {
/*<          INFO = -20 >*/
        *info = -20;
/*<       END IF >*/
    }

/*<       IF( INFO.EQ.0 ) THEN >*/
    if (*info == 0) {
/*<          WORK( 1 ) = LWMIN >*/
        work[1] = (doublereal) lwmin;
/*<       END IF >*/
    }

/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'DTGSYL', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("DTGSYL", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       ELSE IF( LQUERY ) THEN >*/
    } else if (lquery) {
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Determine optimal block sizes MB and NB */

/*<       MB = ILAENV( 2, 'DTGSYL', TRANS, M, N, -1, -1 ) >*/
    mb = ilaenv_(&c__2, "DTGSYL", trans, m, n, &c_n1, &c_n1, (ftnlen)6, (
            ftnlen)1);
/*<       NB = ILAENV( 5, 'DTGSYL', TRANS, M, N, -1, -1 ) >*/
    nb = ilaenv_(&c__5, "DTGSYL", trans, m, n, &c_n1, &c_n1, (ftnlen)6, (
            ftnlen)1);

/*<       ISOLVE = 1 >*/
    isolve = 1;
/*<       IFUNC = 0 >*/
    ifunc = 0;
/*<       IF( IJOB.GE.3 .AND. NOTRAN ) THEN >*/
    if (*ijob >= 3 && notran) {
/*<          IFUNC = IJOB - 2 >*/
        ifunc = *ijob - 2;
/*<          DO 10 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             CALL DCOPY( M, ZERO, 0, C( 1, J ), 1 ) >*/
            dcopy_(m, &c_b14, &c__0, &c__[j * c_dim1 + 1], &c__1);
/*<             CALL DCOPY( M, ZERO, 0, F( 1, J ), 1 ) >*/
            dcopy_(m, &c_b14, &c__0, &f[j * f_dim1 + 1], &c__1);
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<       ELSE IF( IJOB.GE.1 .AND. NOTRAN ) THEN >*/
    } else if (*ijob >= 1 && notran) {
/*<          ISOLVE = 2 >*/
        isolve = 2;
/*<       END IF >*/
    }

/*<    >*/
    if ((mb <= 1 && nb <= 1) || (mb >= *m && nb >= *n)) {

/*<          DO 30 IROUND = 1, ISOLVE >*/
        i__1 = isolve;
        for (iround = 1; iround <= i__1; ++iround) {

/*           Use unblocked Level 2 solver */

/*<             DSCALE = ZERO >*/
            dscale = 0.;
/*<             DSUM = ONE >*/
            dsum = 1.;
/*<             PQ = 0 >*/
            pq = 0;
/*<    >*/
            dtgsy2_(trans, &ifunc, m, n, &a[a_offset], lda, &b[b_offset], ldb,
                     &c__[c_offset], ldc, &d__[d_offset], ldd, &e[e_offset],
                    lde, &f[f_offset], ldf, scale, &dsum, &dscale, &iwork[1],
                    &pq, info, (ftnlen)1);
/*<             IF( DSCALE.NE.ZERO ) THEN >*/
            if (dscale != 0.) {
/*<                IF( IJOB.EQ.1 .OR. IJOB.EQ.3 ) THEN >*/
                if (*ijob == 1 || *ijob == 3) {
/*<                   DIF = SQRT( DBLE( 2*M*N ) ) / ( DSCALE*SQRT( DSUM ) ) >*/
                    *dif = sqrt((doublereal) ((*m << 1) * *n)) / (dscale *
                            sqrt(dsum));
/*<                ELSE >*/
                } else {
/*<                   DIF = SQRT( DBLE( PQ ) ) / ( DSCALE*SQRT( DSUM ) ) >*/
                    *dif = sqrt((doublereal) pq) / (dscale * sqrt(dsum));
/*<                END IF >*/
                }
/*<             END IF >*/
            }

/*<             IF( ISOLVE.EQ.2 .AND. IROUND.EQ.1 ) THEN >*/
            if (isolve == 2 && iround == 1) {
/*<                IFUNC = IJOB >*/
                ifunc = *ijob;
/*<                SCALE2 = SCALE >*/
                scale2 = *scale;
/*<                CALL DLACPY( 'F', M, N, C, LDC, WORK, M ) >*/
                dlacpy_("F", m, n, &c__[c_offset], ldc, &work[1], m, (ftnlen)
                        1);
/*<                CALL DLACPY( 'F', M, N, F, LDF, WORK( M*N+1 ), M ) >*/
                dlacpy_("F", m, n, &f[f_offset], ldf, &work[*m * *n + 1], m, (
                        ftnlen)1);
/*<                DO 20 J = 1, N >*/
                i__2 = *n;
                for (j = 1; j <= i__2; ++j) {
/*<                   CALL DCOPY( M, ZERO, 0, C( 1, J ), 1 ) >*/
                    dcopy_(m, &c_b14, &c__0, &c__[j * c_dim1 + 1], &c__1);
/*<                   CALL DCOPY( M, ZERO, 0, F( 1, J ), 1 ) >*/
                    dcopy_(m, &c_b14, &c__0, &f[j * f_dim1 + 1], &c__1);
/*<    20          CONTINUE >*/
/* L20: */
                }
/*<             ELSE IF( ISOLVE.EQ.2 .AND. IROUND.EQ.2 ) THEN >*/
            } else if (isolve == 2 && iround == 2) {
/*<                CALL DLACPY( 'F', M, N, WORK, M, C, LDC ) >*/
                dlacpy_("F", m, n, &work[1], m, &c__[c_offset], ldc, (ftnlen)
                        1);
/*<                CALL DLACPY( 'F', M, N, WORK( M*N+1 ), M, F, LDF ) >*/
                dlacpy_("F", m, n, &work[*m * *n + 1], m, &f[f_offset], ldf, (
                        ftnlen)1);
/*<                SCALE = SCALE2 >*/
                *scale = scale2;
/*<             END IF >*/
            }
/*<    30    CONTINUE >*/
/* L30: */
        }

/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Determine block structure of A */

/*<       P = 0 >*/
    p = 0;
/*<       I = 1 >*/
    i__ = 1;
/*<    40 CONTINUE >*/
L40:
/*<    >*/
    if (i__ > *m) {
        goto L50;
    }
/*<       P = P + 1 >*/
    ++p;
/*<       IWORK( P ) = I >*/
    iwork[p] = i__;
/*<       I = I + MB >*/
    i__ += mb;
/*<    >*/
    if (i__ >= *m) {
        goto L50;
    }
/*<    >*/
    if (a[i__ + (i__ - 1) * a_dim1] != 0.) {
        ++i__;
    }
/*<       GO TO 40 >*/
    goto L40;
/*<    50 CONTINUE >*/
L50:

/*<       IWORK( P+1 ) = M + 1 >*/
    iwork[p + 1] = *m + 1;
/*<    >*/
    if (iwork[p] == iwork[p + 1]) {
        --p;
    }

/*     Determine block structure of B */

/*<       Q = P + 1 >*/
    q = p + 1;
/*<       J = 1 >*/
    j = 1;
/*<    60 CONTINUE >*/
L60:
/*<    >*/
    if (j > *n) {
        goto L70;
    }
/*<       Q = Q + 1 >*/
    ++q;
/*<       IWORK( Q ) = J >*/
    iwork[q] = j;
/*<       J = J + NB >*/
    j += nb;
/*<    >*/
    if (j >= *n) {
        goto L70;
    }
/*<    >*/
    if (b[j + (j - 1) * b_dim1] != 0.) {
        ++j;
    }
/*<       GO TO 60 >*/
    goto L60;
/*<    70 CONTINUE >*/
L70:

/*<       IWORK( Q+1 ) = N + 1 >*/
    iwork[q + 1] = *n + 1;
/*<    >*/
    if (iwork[q] == iwork[q + 1]) {
        --q;
    }

/*<       IF( NOTRAN ) THEN >*/
    if (notran) {

/*<          DO 150 IROUND = 1, ISOLVE >*/
        i__1 = isolve;
        for (iround = 1; iround <= i__1; ++iround) {

/*           Solve (I, J)-subsystem */
/*               A(I, I) * R(I, J) - L(I, J) * B(J, J) = C(I, J) */
/*               D(I, I) * R(I, J) - L(I, J) * E(J, J) = F(I, J) */
/*           for I = P, P - 1,..., 1; J = 1, 2,..., Q */

/*<             DSCALE = ZERO >*/
            dscale = 0.;
/*<             DSUM = ONE >*/
            dsum = 1.;
/*<             PQ = 0 >*/
            pq = 0;
/*<             SCALE = ONE >*/
            *scale = 1.;
/*<             DO 130 J = P + 2, Q >*/
            i__2 = q;
            for (j = p + 2; j <= i__2; ++j) {
/*<                JS = IWORK( J ) >*/
                js = iwork[j];
/*<                JE = IWORK( J+1 ) - 1 >*/
                je = iwork[j + 1] - 1;
/*<                NB = JE - JS + 1 >*/
                nb = je - js + 1;
/*<                DO 120 I = P, 1, -1 >*/
                for (i__ = p; i__ >= 1; --i__) {
/*<                   IS = IWORK( I ) >*/
                    is = iwork[i__];
/*<                   IE = IWORK( I+1 ) - 1 >*/
                    ie = iwork[i__ + 1] - 1;
/*<                   MB = IE - IS + 1 >*/
                    mb = ie - is + 1;
/*<                   PPQQ = 0 >*/
                    ppqq = 0;
/*<    >*/
                    dtgsy2_(trans, &ifunc, &mb, &nb, &a[is + is * a_dim1],
                            lda, &b[js + js * b_dim1], ldb, &c__[is + js *
                            c_dim1], ldc, &d__[is + is * d_dim1], ldd, &e[js
                            + js * e_dim1], lde, &f[is + js * f_dim1], ldf, &
                            scaloc, &dsum, &dscale, &iwork[q + 2], &ppqq, &
                            linfo, (ftnlen)1);
/*<    >*/
                    if (linfo > 0) {
                        *info = linfo;
                    }

/*<                   PQ = PQ + PPQQ >*/
                    pq += ppqq;
/*<                   IF( SCALOC.NE.ONE ) THEN >*/
                    if (scaloc != 1.) {
/*<                      DO 80 K = 1, JS - 1 >*/
                        i__3 = js - 1;
                        for (k = 1; k <= i__3; ++k) {
/*<                         CALL DSCAL( M, SCALOC, C( 1, K ), 1 ) >*/
                            dscal_(m, &scaloc, &c__[k * c_dim1 + 1], &c__1);
/*<                         CALL DSCAL( M, SCALOC, F( 1, K ), 1 ) >*/
                            dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
/*<    80                CONTINUE >*/
/* L80: */
                        }
/*<                      DO 90 K = JS, JE >*/
                        i__3 = je;
                        for (k = js; k <= i__3; ++k) {
/*<                         CALL DSCAL( IS-1, SCALOC, C( 1, K ), 1 ) >*/
                            i__4 = is - 1;
                            dscal_(&i__4, &scaloc, &c__[k * c_dim1 + 1], &
                                    c__1);
/*<                         CALL DSCAL( IS-1, SCALOC, F( 1, K ), 1 ) >*/
                            i__4 = is - 1;
                            dscal_(&i__4, &scaloc, &f[k * f_dim1 + 1], &c__1);
/*<    90                CONTINUE >*/
/* L90: */
                        }
/*<                      DO 100 K = JS, JE >*/
                        i__3 = je;
                        for (k = js; k <= i__3; ++k) {
/*<                         CALL DSCAL( M-IE, SCALOC, C( IE+1, K ), 1 ) >*/
                            i__4 = *m - ie;
                            dscal_(&i__4, &scaloc, &c__[ie + 1 + k * c_dim1],
                                    &c__1);
/*<                         CALL DSCAL( M-IE, SCALOC, F( IE+1, K ), 1 ) >*/
                            i__4 = *m - ie;
                            dscal_(&i__4, &scaloc, &f[ie + 1 + k * f_dim1], &
                                    c__1);
/*<   100                CONTINUE >*/
/* L100: */
                        }
/*<                      DO 110 K = JE + 1, N >*/
                        i__3 = *n;
                        for (k = je + 1; k <= i__3; ++k) {
/*<                         CALL DSCAL( M, SCALOC, C( 1, K ), 1 ) >*/
                            dscal_(m, &scaloc, &c__[k * c_dim1 + 1], &c__1);
/*<                         CALL DSCAL( M, SCALOC, F( 1, K ), 1 ) >*/
                            dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
/*<   110                CONTINUE >*/
/* L110: */
                        }
/*<                      SCALE = SCALE*SCALOC >*/
                        *scale *= scaloc;
/*<                   END IF >*/
                    }

/*                 Substitute R(I, J) and L(I, J) into remaining */
/*                 equation. */

/*<                   IF( I.GT.1 ) THEN >*/
                    if (i__ > 1) {
/*<    >*/
                        i__3 = is - 1;
                        dgemm_("N", "N", &i__3, &nb, &mb, &c_b53, &a[is *
                                a_dim1 + 1], lda, &c__[is + js * c_dim1], ldc,
                                 &c_b54, &c__[js * c_dim1 + 1], ldc, (ftnlen)
                                1, (ftnlen)1);
/*<    >*/
                        i__3 = is - 1;
                        dgemm_("N", "N", &i__3, &nb, &mb, &c_b53, &d__[is *
                                d_dim1 + 1], ldd, &c__[is + js * c_dim1], ldc,
                                 &c_b54, &f[js * f_dim1 + 1], ldf, (ftnlen)1,
                                (ftnlen)1);
/*<                   END IF >*/
                    }
/*<                   IF( J.LT.Q ) THEN >*/
                    if (j < q) {
/*<    >*/
                        i__3 = *n - je;
                        dgemm_("N", "N", &mb, &i__3, &nb, &c_b54, &f[is + js *
                                 f_dim1], ldf, &b[js + (je + 1) * b_dim1],
                                ldb, &c_b54, &c__[is + (je + 1) * c_dim1],
                                ldc, (ftnlen)1, (ftnlen)1);
/*<    >*/
                        i__3 = *n - je;
                        dgemm_("N", "N", &mb, &i__3, &nb, &c_b54, &f[is + js *
                                 f_dim1], ldf, &e[js + (je + 1) * e_dim1],
                                lde, &c_b54, &f[is + (je + 1) * f_dim1], ldf,
                                (ftnlen)1, (ftnlen)1);
/*<                   END IF >*/
                    }
/*<   120          CONTINUE >*/
/* L120: */
                }
/*<   130       CONTINUE >*/
/* L130: */
            }
/*<             IF( DSCALE.NE.ZERO ) THEN >*/
            if (dscale != 0.) {
/*<                IF( IJOB.EQ.1 .OR. IJOB.EQ.3 ) THEN >*/
                if (*ijob == 1 || *ijob == 3) {
/*<                   DIF = SQRT( DBLE( 2*M*N ) ) / ( DSCALE*SQRT( DSUM ) ) >*/
                    *dif = sqrt((doublereal) ((*m << 1) * *n)) / (dscale *
                            sqrt(dsum));
/*<                ELSE >*/
                } else {
/*<                   DIF = SQRT( DBLE( PQ ) ) / ( DSCALE*SQRT( DSUM ) ) >*/
                    *dif = sqrt((doublereal) pq) / (dscale * sqrt(dsum));
/*<                END IF >*/
                }
/*<             END IF >*/
            }
/*<             IF( ISOLVE.EQ.2 .AND. IROUND.EQ.1 ) THEN >*/
            if (isolve == 2 && iround == 1) {
/*<                IFUNC = IJOB >*/
                ifunc = *ijob;
/*<                SCALE2 = SCALE >*/
                scale2 = *scale;
/*<                CALL DLACPY( 'F', M, N, C, LDC, WORK, M ) >*/
                dlacpy_("F", m, n, &c__[c_offset], ldc, &work[1], m, (ftnlen)
                        1);
/*<                CALL DLACPY( 'F', M, N, F, LDF, WORK( M*N+1 ), M ) >*/
                dlacpy_("F", m, n, &f[f_offset], ldf, &work[*m * *n + 1], m, (
                        ftnlen)1);
/*<                DO 140 J = 1, N >*/
                i__2 = *n;
                for (j = 1; j <= i__2; ++j) {
/*<                   CALL DCOPY( M, ZERO, 0, C( 1, J ), 1 ) >*/
                    dcopy_(m, &c_b14, &c__0, &c__[j * c_dim1 + 1], &c__1);
/*<                   CALL DCOPY( M, ZERO, 0, F( 1, J ), 1 ) >*/
                    dcopy_(m, &c_b14, &c__0, &f[j * f_dim1 + 1], &c__1);
/*<   140          CONTINUE >*/
/* L140: */
                }
/*<             ELSE IF( ISOLVE.EQ.2 .AND. IROUND.EQ.2 ) THEN >*/
            } else if (isolve == 2 && iround == 2) {
/*<                CALL DLACPY( 'F', M, N, WORK, M, C, LDC ) >*/
                dlacpy_("F", m, n, &work[1], m, &c__[c_offset], ldc, (ftnlen)
                        1);
/*<                CALL DLACPY( 'F', M, N, WORK( M*N+1 ), M, F, LDF ) >*/
                dlacpy_("F", m, n, &work[*m * *n + 1], m, &f[f_offset], ldf, (
                        ftnlen)1);
/*<                SCALE = SCALE2 >*/
                *scale = scale2;
/*<             END IF >*/
            }
/*<   150    CONTINUE >*/
/* L150: */
        }

/*<       ELSE >*/
    } else {

/*        Solve transposed (I, J)-subsystem */
/*             A(I, I)' * R(I, J)  + D(I, I)' * L(I, J)  =  C(I, J) */
/*             R(I, J)  * B(J, J)' + L(I, J)  * E(J, J)' = -F(I, J) */
/*        for I = 1,2,..., P; J = Q, Q-1,..., 1 */

/*<          SCALE = ONE >*/
        *scale = 1.;
/*<          DO 210 I = 1, P >*/
        i__1 = p;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             IS = IWORK( I ) >*/
            is = iwork[i__];
/*<             IE = IWORK( I+1 ) - 1 >*/
            ie = iwork[i__ + 1] - 1;
/*<             MB = IE - IS + 1 >*/
            mb = ie - is + 1;
/*<             DO 200 J = Q, P + 2, -1 >*/
            i__2 = p + 2;
            for (j = q; j >= i__2; --j) {
/*<                JS = IWORK( J ) >*/
                js = iwork[j];
/*<                JE = IWORK( J+1 ) - 1 >*/
                je = iwork[j + 1] - 1;
/*<                NB = JE - JS + 1 >*/
                nb = je - js + 1;
/*<    >*/
                dtgsy2_(trans, &ifunc, &mb, &nb, &a[is + is * a_dim1], lda, &
                        b[js + js * b_dim1], ldb, &c__[is + js * c_dim1], ldc,
                         &d__[is + is * d_dim1], ldd, &e[js + js * e_dim1],
                        lde, &f[is + js * f_dim1], ldf, &scaloc, &dsum, &
                        dscale, &iwork[q + 2], &ppqq, &linfo, (ftnlen)1);
/*<    >*/
                if (linfo > 0) {
                    *info = linfo;
                }
/*<                IF( SCALOC.NE.ONE ) THEN >*/
                if (scaloc != 1.) {
/*<                   DO 160 K = 1, JS - 1 >*/
                    i__3 = js - 1;
                    for (k = 1; k <= i__3; ++k) {
/*<                      CALL DSCAL( M, SCALOC, C( 1, K ), 1 ) >*/
                        dscal_(m, &scaloc, &c__[k * c_dim1 + 1], &c__1);
/*<                      CALL DSCAL( M, SCALOC, F( 1, K ), 1 ) >*/
                        dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
/*<   160             CONTINUE >*/
/* L160: */
                    }
/*<                   DO 170 K = JS, JE >*/
                    i__3 = je;
                    for (k = js; k <= i__3; ++k) {
/*<                      CALL DSCAL( IS-1, SCALOC, C( 1, K ), 1 ) >*/
                        i__4 = is - 1;
                        dscal_(&i__4, &scaloc, &c__[k * c_dim1 + 1], &c__1);
/*<                      CALL DSCAL( IS-1, SCALOC, F( 1, K ), 1 ) >*/
                        i__4 = is - 1;
                        dscal_(&i__4, &scaloc, &f[k * f_dim1 + 1], &c__1);
/*<   170             CONTINUE >*/
/* L170: */
                    }
/*<                   DO 180 K = JS, JE >*/
                    i__3 = je;
                    for (k = js; k <= i__3; ++k) {
/*<                      CALL DSCAL( M-IE, SCALOC, C( IE+1, K ), 1 ) >*/
                        i__4 = *m - ie;
                        dscal_(&i__4, &scaloc, &c__[ie + 1 + k * c_dim1], &
                                c__1);
/*<                      CALL DSCAL( M-IE, SCALOC, F( IE+1, K ), 1 ) >*/
                        i__4 = *m - ie;
                        dscal_(&i__4, &scaloc, &f[ie + 1 + k * f_dim1], &c__1)
                                ;
/*<   180             CONTINUE >*/
/* L180: */
                    }
/*<                   DO 190 K = JE + 1, N >*/
                    i__3 = *n;
                    for (k = je + 1; k <= i__3; ++k) {
/*<                      CALL DSCAL( M, SCALOC, C( 1, K ), 1 ) >*/
                        dscal_(m, &scaloc, &c__[k * c_dim1 + 1], &c__1);
/*<                      CALL DSCAL( M, SCALOC, F( 1, K ), 1 ) >*/
                        dscal_(m, &scaloc, &f[k * f_dim1 + 1], &c__1);
/*<   190             CONTINUE >*/
/* L190: */
                    }
/*<                   SCALE = SCALE*SCALOC >*/
                    *scale *= scaloc;
/*<                END IF >*/
                }

/*              Substitute R(I, J) and L(I, J) into remaining equation. */

/*<                IF( J.GT.P+2 ) THEN >*/
                if (j > p + 2) {
/*<    >*/
                    i__3 = js - 1;
                    dgemm_("N", "T", &mb, &i__3, &nb, &c_b54, &c__[is + js *
                            c_dim1], ldc, &b[js * b_dim1 + 1], ldb, &c_b54, &
                            f[is + f_dim1], ldf, (ftnlen)1, (ftnlen)1);
/*<    >*/
                    i__3 = js - 1;
                    dgemm_("N", "T", &mb, &i__3, &nb, &c_b54, &f[is + js *
                            f_dim1], ldf, &e[js * e_dim1 + 1], lde, &c_b54, &
                            f[is + f_dim1], ldf, (ftnlen)1, (ftnlen)1);
/*<                END IF >*/
                }
/*<                IF( I.LT.P ) THEN >*/
                if (i__ < p) {
/*<    >*/
                    i__3 = *m - ie;
                    dgemm_("T", "N", &i__3, &nb, &mb, &c_b53, &a[is + (ie + 1)
                             * a_dim1], lda, &c__[is + js * c_dim1], ldc, &
                            c_b54, &c__[ie + 1 + js * c_dim1], ldc, (ftnlen)1,
                             (ftnlen)1);
/*<    >*/
                    i__3 = *m - ie;
                    dgemm_("T", "N", &i__3, &nb, &mb, &c_b53, &d__[is + (ie +
                            1) * d_dim1], ldd, &f[is + js * f_dim1], ldf, &
                            c_b54, &c__[ie + 1 + js * c_dim1], ldc, (ftnlen)1,
                             (ftnlen)1);
/*<                END IF >*/
                }
/*<   200       CONTINUE >*/
/* L200: */
            }
/*<   210    CONTINUE >*/
/* L210: */
        }

/*<       END IF >*/
    }

/*<       WORK( 1 ) = LWMIN >*/
    work[1] = (doublereal) lwmin;

/*<       RETURN >*/
    return 0;

/*     End of DTGSYL */

/*<       END >*/
} /* dtgsyl_ */

#ifdef __cplusplus
        }
#endif
