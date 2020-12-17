/* lapack/double/dhgeqz.f -- translated by f2c (version 20050501).
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

static doublereal c_b12 = 0.;
static doublereal c_b13 = 1.;
static integer c__1 = 1;
static integer c__3 = 3;

/*<    >*/
/* Subroutine */ int dhgeqz_(char *job, char *compq, char *compz, integer *n,
        integer *ilo, integer *ihi, doublereal *a, integer *lda, doublereal *
        b, integer *ldb, doublereal *alphar, doublereal *alphai, doublereal *
        beta, doublereal *q, integer *ldq, doublereal *z__, integer *ldz,
        doublereal *work, integer *lwork, integer *info, ftnlen job_len,
        ftnlen compq_len, ftnlen compz_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, q_dim1, q_offset, z_dim1,
            z_offset, i__1, i__2, i__3, i__4;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal c__;
    integer j;
    doublereal s, t, v[3], s1, s2, u1, u2, a11, a12, a21, a22, b11, b22, c12,
            c21;
    integer jc;
    doublereal an, bn, cl, cq, cr;
    integer in;
    doublereal u12, w11, w12, w21;
    integer jr;
    doublereal cz, w22, sl, wi, sr, vs, wr, b1a, b2a, a1i, a2i, b1i, b2i, a1r,
             a2r, b1r, b2r, wr2, ad11, ad12, ad21, ad22, c11i, c22i;
    integer jch;
    doublereal c11r, c22r, u12l;
    logical ilq=0;
    doublereal tau, sqi;
    logical ilz=0;
    doublereal ulp, sqr, szi, szr, ad11l, ad12l, ad21l, ad22l, ad32l, wabs,
            atol, btol, temp;
    extern /* Subroutine */ int drot_(integer *, doublereal *, integer *,
            doublereal *, integer *, doublereal *, doublereal *), dlag2_(
            doublereal *, integer *, doublereal *, integer *, doublereal *,
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *);
    doublereal temp2, s1inv, scale;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    integer iiter, ilast, jiter;
    doublereal anorm, bnorm;
    integer maxit;
    doublereal tempi, tempr;
    extern doublereal dlapy2_(doublereal *, doublereal *), dlapy3_(doublereal
            *, doublereal *, doublereal *);
    extern /* Subroutine */ int dlasv2_(doublereal *, doublereal *,
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *, doublereal *, doublereal *);
    logical ilazr2;
    doublereal ascale, bscale;
    extern doublereal dlamch_(char *, ftnlen);
    extern /* Subroutine */ int dlarfg_(integer *, doublereal *, doublereal *,
             integer *, doublereal *);
    extern doublereal dlanhs_(char *, integer *, doublereal *, integer *,
            doublereal *, ftnlen);
    extern /* Subroutine */ int dlaset_(char *, integer *, integer *,
            doublereal *, doublereal *, doublereal *, integer *, ftnlen);
    doublereal safmin;
    extern /* Subroutine */ int dlartg_(doublereal *, doublereal *,
            doublereal *, doublereal *, doublereal *);
    doublereal safmax;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    doublereal eshift;
    logical ilschr=0;
    integer icompq, ilastm, ischur;
    logical ilazro;
    integer icompz, ifirst, ifrstm, istart;
    logical ilpivt, lquery;
    (void)job_len;
    (void)compq_len;
    (void)compz_len;

/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          COMPQ, COMPZ, JOB >*/
/*<       INTEGER            IHI, ILO, INFO, LDA, LDB, LDQ, LDZ, LWORK, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DHGEQZ implements a single-/double-shift version of the QZ method for */
/*  finding the generalized eigenvalues */

/*  w(j)=(ALPHAR(j) + i*ALPHAI(j))/BETAR(j)   of the equation */

/*       det( A - w(i) B ) = 0 */

/*  In addition, the pair A,B may be reduced to generalized Schur form: */
/*  B is upper triangular, and A is block upper triangular, where the */
/*  diagonal blocks are either 1-by-1 or 2-by-2, the 2-by-2 blocks having */
/*  complex generalized eigenvalues (see the description of the argument */
/*  JOB.) */

/*  If JOB='S', then the pair (A,B) is simultaneously reduced to Schur */
/*  form by applying one orthogonal transformation (usually called Q) on */
/*  the left and another (usually called Z) on the right.  The 2-by-2 */
/*  upper-triangular diagonal blocks of B corresponding to 2-by-2 blocks */
/*  of A will be reduced to positive diagonal matrices.  (I.e., */
/*  if A(j+1,j) is non-zero, then B(j+1,j)=B(j,j+1)=0 and B(j,j) and */
/*  B(j+1,j+1) will be positive.) */

/*  If JOB='E', then at each iteration, the same transformations */
/*  are computed, but they are only applied to those parts of A and B */
/*  which are needed to compute ALPHAR, ALPHAI, and BETAR. */

/*  If JOB='S' and COMPQ and COMPZ are 'V' or 'I', then the orthogonal */
/*  transformations used to reduce (A,B) are accumulated into the arrays */
/*  Q and Z s.t.: */

/*       Q(in) A(in) Z(in)* = Q(out) A(out) Z(out)* */
/*       Q(in) B(in) Z(in)* = Q(out) B(out) Z(out)* */

/*  Ref: C.B. Moler & G.W. Stewart, "An Algorithm for Generalized Matrix */
/*       Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973), */
/*       pp. 241--256. */

/*  Arguments */
/*  ========= */

/*  JOB     (input) CHARACTER*1 */
/*          = 'E': compute only ALPHAR, ALPHAI, and BETA.  A and B will */
/*                 not necessarily be put into generalized Schur form. */
/*          = 'S': put A and B into generalized Schur form, as well */
/*                 as computing ALPHAR, ALPHAI, and BETA. */

/*  COMPQ   (input) CHARACTER*1 */
/*          = 'N': do not modify Q. */
/*          = 'V': multiply the array Q on the right by the transpose of */
/*                 the orthogonal transformation that is applied to the */
/*                 left side of A and B to reduce them to Schur form. */
/*          = 'I': like COMPQ='V', except that Q will be initialized to */
/*                 the identity first. */

/*  COMPZ   (input) CHARACTER*1 */
/*          = 'N': do not modify Z. */
/*          = 'V': multiply the array Z on the right by the orthogonal */
/*                 transformation that is applied to the right side of */
/*                 A and B to reduce them to Schur form. */
/*          = 'I': like COMPZ='V', except that Z will be initialized to */
/*                 the identity first. */

/*  N       (input) INTEGER */
/*          The order of the matrices A, B, Q, and Z.  N >= 0. */

/*  ILO     (input) INTEGER */
/*  IHI     (input) INTEGER */
/*          It is assumed that A is already upper triangular in rows and */
/*          columns 1:ILO-1 and IHI+1:N. */
/*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0. */

/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N) */
/*          On entry, the N-by-N upper Hessenberg matrix A.  Elements */
/*          below the subdiagonal must be zero. */
/*          If JOB='S', then on exit A and B will have been */
/*             simultaneously reduced to generalized Schur form. */
/*          If JOB='E', then on exit A will have been destroyed. */
/*             The diagonal blocks will be correct, but the off-diagonal */
/*             portion will be meaningless. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max( 1, N ). */

/*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, N) */
/*          On entry, the N-by-N upper triangular matrix B.  Elements */
/*          below the diagonal must be zero.  2-by-2 blocks in B */
/*          corresponding to 2-by-2 blocks in A will be reduced to */
/*          positive diagonal form.  (I.e., if A(j+1,j) is non-zero, */
/*          then B(j+1,j)=B(j,j+1)=0 and B(j,j) and B(j+1,j+1) will be */
/*          positive.) */
/*          If JOB='S', then on exit A and B will have been */
/*             simultaneously reduced to Schur form. */
/*          If JOB='E', then on exit B will have been destroyed. */
/*             Elements corresponding to diagonal blocks of A will be */
/*             correct, but the off-diagonal portion will be meaningless. */

/*  LDB     (input) INTEGER */
/*          The leading dimension of the array B.  LDB >= max( 1, N ). */

/*  ALPHAR  (output) DOUBLE PRECISION array, dimension (N) */
/*          ALPHAR(1:N) will be set to real parts of the diagonal */
/*          elements of A that would result from reducing A and B to */
/*          Schur form and then further reducing them both to triangular */
/*          form using unitary transformations s.t. the diagonal of B */
/*          was non-negative real.  Thus, if A(j,j) is in a 1-by-1 block */
/*          (i.e., A(j+1,j)=A(j,j+1)=0), then ALPHAR(j)=A(j,j). */
/*          Note that the (real or complex) values */
/*          (ALPHAR(j) + i*ALPHAI(j))/BETA(j), j=1,...,N, are the */
/*          generalized eigenvalues of the matrix pencil A - wB. */

/*  ALPHAI  (output) DOUBLE PRECISION array, dimension (N) */
/*          ALPHAI(1:N) will be set to imaginary parts of the diagonal */
/*          elements of A that would result from reducing A and B to */
/*          Schur form and then further reducing them both to triangular */
/*          form using unitary transformations s.t. the diagonal of B */
/*          was non-negative real.  Thus, if A(j,j) is in a 1-by-1 block */
/*          (i.e., A(j+1,j)=A(j,j+1)=0), then ALPHAR(j)=0. */
/*          Note that the (real or complex) values */
/*          (ALPHAR(j) + i*ALPHAI(j))/BETA(j), j=1,...,N, are the */
/*          generalized eigenvalues of the matrix pencil A - wB. */

/*  BETA    (output) DOUBLE PRECISION array, dimension (N) */
/*          BETA(1:N) will be set to the (real) diagonal elements of B */
/*          that would result from reducing A and B to Schur form and */
/*          then further reducing them both to triangular form using */
/*          unitary transformations s.t. the diagonal of B was */
/*          non-negative real.  Thus, if A(j,j) is in a 1-by-1 block */
/*          (i.e., A(j+1,j)=A(j,j+1)=0), then BETA(j)=B(j,j). */
/*          Note that the (real or complex) values */
/*          (ALPHAR(j) + i*ALPHAI(j))/BETA(j), j=1,...,N, are the */
/*          generalized eigenvalues of the matrix pencil A - wB. */
/*          (Note that BETA(1:N) will always be non-negative, and no */
/*          BETAI is necessary.) */

/*  Q       (input/output) DOUBLE PRECISION array, dimension (LDQ, N) */
/*          If COMPQ='N', then Q will not be referenced. */
/*          If COMPQ='V' or 'I', then the transpose of the orthogonal */
/*             transformations which are applied to A and B on the left */
/*             will be applied to the array Q on the right. */

/*  LDQ     (input) INTEGER */
/*          The leading dimension of the array Q.  LDQ >= 1. */
/*          If COMPQ='V' or 'I', then LDQ >= N. */

/*  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ, N) */
/*          If COMPZ='N', then Z will not be referenced. */
/*          If COMPZ='V' or 'I', then the orthogonal transformations */
/*             which are applied to A and B on the right will be applied */
/*             to the array Z on the right. */

/*  LDZ     (input) INTEGER */
/*          The leading dimension of the array Z.  LDZ >= 1. */
/*          If COMPZ='V' or 'I', then LDZ >= N. */

/*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK) */
/*          On exit, if INFO >= 0, WORK(1) returns the optimal LWORK. */

/*  LWORK   (input) INTEGER */
/*          The dimension of the array WORK.  LWORK >= max(1,N). */

/*          If LWORK = -1, then a workspace query is assumed; the routine */
/*          only calculates the optimal size of the WORK array, returns */
/*          this value as the first entry of the WORK array, and no error */
/*          message related to LWORK is issued by XERBLA. */

/*  INFO    (output) INTEGER */
/*          = 0: successful exit */
/*          < 0: if INFO = -i, the i-th argument had an illegal value */
/*          = 1,...,N: the QZ iteration did not converge.  (A,B) is not */
/*                     in Schur form, but ALPHAR(i), ALPHAI(i), and */
/*                     BETA(i), i=INFO+1,...,N should be correct. */
/*          = N+1,...,2*N: the shift calculation failed.  (A,B) is not */
/*                     in Schur form, but ALPHAR(i), ALPHAI(i), and */
/*                     BETA(i), i=INFO-N+1,...,N should be correct. */
/*          > 2*N:     various "impossible" errors. */

/*  Further Details */
/*  =============== */

/*  Iteration counters: */

/*  JITER  -- counts iterations. */
/*  IITER  -- counts iterations run since ILAST was last */
/*            changed.  This is therefore reset only when a 1-by-1 or */
/*            2-by-2 block deflates off the bottom. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*    $                     SAFETY = 1.0E+0 ) */
/*<       DOUBLE PRECISION   HALF, ZERO, ONE, SAFETY >*/
/*<    >*/
/*     .. */
/*     .. Local Scalars .. */
/*<    >*/
/*<    >*/
/*<    >*/
/*     .. */
/*     .. Local Arrays .. */
/*<       DOUBLE PRECISION   V( 3 ) >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       DOUBLE PRECISION   DLAMCH, DLANHS, DLAPY2, DLAPY3 >*/
/*<       EXTERNAL           LSAME, DLAMCH, DLANHS, DLAPY2, DLAPY3 >*/
/*     .. */
/*     .. External Subroutines .. */
/*<    >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, DBLE, MAX, MIN, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Decode JOB, COMPQ, COMPZ */

/*<       IF( LSAME( JOB, 'E' ) ) THEN >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1;
    b -= b_offset;
    --alphar;
    --alphai;
    --beta;
    q_dim1 = *ldq;
    q_offset = 1 + q_dim1;
    q -= q_offset;
    z_dim1 = *ldz;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --work;

    /* Function Body */
    if (lsame_(job, "E", (ftnlen)1, (ftnlen)1)) {
/*<          ILSCHR = .FALSE. >*/
        ilschr = FALSE_;
/*<          ISCHUR = 1 >*/
        ischur = 1;
/*<       ELSE IF( LSAME( JOB, 'S' ) ) THEN >*/
    } else if (lsame_(job, "S", (ftnlen)1, (ftnlen)1)) {
/*<          ILSCHR = .TRUE. >*/
        ilschr = TRUE_;
/*<          ISCHUR = 2 >*/
        ischur = 2;
/*<       ELSE >*/
    } else {
/*<          ISCHUR = 0 >*/
        ischur = 0;
/*<       END IF >*/
    }

/*<       IF( LSAME( COMPQ, 'N' ) ) THEN >*/
    if (lsame_(compq, "N", (ftnlen)1, (ftnlen)1)) {
/*<          ILQ = .FALSE. >*/
        ilq = FALSE_;
/*<          ICOMPQ = 1 >*/
        icompq = 1;
/*<       ELSE IF( LSAME( COMPQ, 'V' ) ) THEN >*/
    } else if (lsame_(compq, "V", (ftnlen)1, (ftnlen)1)) {
/*<          ILQ = .TRUE. >*/
        ilq = TRUE_;
/*<          ICOMPQ = 2 >*/
        icompq = 2;
/*<       ELSE IF( LSAME( COMPQ, 'I' ) ) THEN >*/
    } else if (lsame_(compq, "I", (ftnlen)1, (ftnlen)1)) {
/*<          ILQ = .TRUE. >*/
        ilq = TRUE_;
/*<          ICOMPQ = 3 >*/
        icompq = 3;
/*<       ELSE >*/
    } else {
/*<          ICOMPQ = 0 >*/
        icompq = 0;
/*<       END IF >*/
    }

/*<       IF( LSAME( COMPZ, 'N' ) ) THEN >*/
    if (lsame_(compz, "N", (ftnlen)1, (ftnlen)1)) {
/*<          ILZ = .FALSE. >*/
        ilz = FALSE_;
/*<          ICOMPZ = 1 >*/
        icompz = 1;
/*<       ELSE IF( LSAME( COMPZ, 'V' ) ) THEN >*/
    } else if (lsame_(compz, "V", (ftnlen)1, (ftnlen)1)) {
/*<          ILZ = .TRUE. >*/
        ilz = TRUE_;
/*<          ICOMPZ = 2 >*/
        icompz = 2;
/*<       ELSE IF( LSAME( COMPZ, 'I' ) ) THEN >*/
    } else if (lsame_(compz, "I", (ftnlen)1, (ftnlen)1)) {
/*<          ILZ = .TRUE. >*/
        ilz = TRUE_;
/*<          ICOMPZ = 3 >*/
        icompz = 3;
/*<       ELSE >*/
    } else {
/*<          ICOMPZ = 0 >*/
        icompz = 0;
/*<       END IF >*/
    }

/*     Check Argument Values */

/*<       INFO = 0 >*/
    *info = 0;
/*<       WORK( 1 ) = MAX( 1, N ) >*/
    work[1] = (doublereal) max(1,*n);
/*<       LQUERY = ( LWORK.EQ.-1 ) >*/
    lquery = *lwork == -1;
/*<       IF( ISCHUR.EQ.0 ) THEN >*/
    if (ischur == 0) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( ICOMPQ.EQ.0 ) THEN >*/
    } else if (icompq == 0) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( ICOMPZ.EQ.0 ) THEN >*/
    } else if (icompz == 0) {
/*<          INFO = -3 >*/
        *info = -3;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( ILO.LT.1 ) THEN >*/
    } else if (*ilo < 1) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       ELSE IF( IHI.GT.N .OR. IHI.LT.ILO-1 ) THEN >*/
    } else if (*ihi > *n || *ihi < *ilo - 1) {
/*<          INFO = -6 >*/
        *info = -6;
/*<       ELSE IF( LDA.LT.N ) THEN >*/
    } else if (*lda < *n) {
/*<          INFO = -8 >*/
        *info = -8;
/*<       ELSE IF( LDB.LT.N ) THEN >*/
    } else if (*ldb < *n) {
/*<          INFO = -10 >*/
        *info = -10;
/*<       ELSE IF( LDQ.LT.1 .OR. ( ILQ .AND. LDQ.LT.N ) ) THEN >*/
    } else if (*ldq < 1 || (ilq && *ldq < *n)) {
/*<          INFO = -15 >*/
        *info = -15;
/*<       ELSE IF( LDZ.LT.1 .OR. ( ILZ .AND. LDZ.LT.N ) ) THEN >*/
    } else if (*ldz < 1 || (ilz && *ldz < *n)) {
/*<          INFO = -17 >*/
        *info = -17;
/*<       ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN >*/
    } else if (*lwork < max(1,*n) && ! lquery) {
/*<          INFO = -19 >*/
        *info = -19;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'DHGEQZ', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("DHGEQZ", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       ELSE IF( LQUERY ) THEN >*/
    } else if (lquery) {
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<       IF( N.LE.0 ) THEN >*/
    if (*n <= 0) {
/*<          WORK( 1 ) = DBLE( 1 ) >*/
        work[1] = 1.;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Initialize Q and Z */

/*<    >*/
    if (icompq == 3) {
        dlaset_("Full", n, n, &c_b12, &c_b13, &q[q_offset], ldq, (ftnlen)4);
    }
/*<    >*/
    if (icompz == 3) {
        dlaset_("Full", n, n, &c_b12, &c_b13, &z__[z_offset], ldz, (ftnlen)4);
    }

/*     Machine Constants */

/*<       IN = IHI + 1 - ILO >*/
    in = *ihi + 1 - *ilo;
/*<       SAFMIN = DLAMCH( 'S' ) >*/
    safmin = dlamch_("S", (ftnlen)1);
/*<       SAFMAX = ONE / SAFMIN >*/
    safmax = 1. / safmin;
/*<       ULP = DLAMCH( 'E' )*DLAMCH( 'B' ) >*/
    ulp = dlamch_("E", (ftnlen)1) * dlamch_("B", (ftnlen)1);
/*<       ANORM = DLANHS( 'F', IN, A( ILO, ILO ), LDA, WORK ) >*/
    anorm = dlanhs_("F", &in, &a[*ilo + *ilo * a_dim1], lda, &work[1], (
            ftnlen)1);
/*<       BNORM = DLANHS( 'F', IN, B( ILO, ILO ), LDB, WORK ) >*/
    bnorm = dlanhs_("F", &in, &b[*ilo + *ilo * b_dim1], ldb, &work[1], (
            ftnlen)1);
/*<       ATOL = MAX( SAFMIN, ULP*ANORM ) >*/
/* Computing MAX */
    d__1 = safmin, d__2 = ulp * anorm;
    atol = max(d__1,d__2);
/*<       BTOL = MAX( SAFMIN, ULP*BNORM ) >*/
/* Computing MAX */
    d__1 = safmin, d__2 = ulp * bnorm;
    btol = max(d__1,d__2);
/*<       ASCALE = ONE / MAX( SAFMIN, ANORM ) >*/
    ascale = 1. / max(safmin,anorm);
/*<       BSCALE = ONE / MAX( SAFMIN, BNORM ) >*/
    bscale = 1. / max(safmin,bnorm);

/*     Set Eigenvalues IHI+1:N */

/*<       DO 30 J = IHI + 1, N >*/
    i__1 = *n;
    for (j = *ihi + 1; j <= i__1; ++j) {
/*<          IF( B( J, J ).LT.ZERO ) THEN >*/
        if (b[j + j * b_dim1] < 0.) {
/*<             IF( ILSCHR ) THEN >*/
            if (ilschr) {
/*<                DO 10 JR = 1, J >*/
                i__2 = j;
                for (jr = 1; jr <= i__2; ++jr) {
/*<                   A( JR, J ) = -A( JR, J ) >*/
                    a[jr + j * a_dim1] = -a[jr + j * a_dim1];
/*<                   B( JR, J ) = -B( JR, J ) >*/
                    b[jr + j * b_dim1] = -b[jr + j * b_dim1];
/*<    10          CONTINUE >*/
/* L10: */
                }
/*<             ELSE >*/
            } else {
/*<                A( J, J ) = -A( J, J ) >*/
                a[j + j * a_dim1] = -a[j + j * a_dim1];
/*<                B( J, J ) = -B( J, J ) >*/
                b[j + j * b_dim1] = -b[j + j * b_dim1];
/*<             END IF >*/
            }
/*<             IF( ILZ ) THEN >*/
            if (ilz) {
/*<                DO 20 JR = 1, N >*/
                i__2 = *n;
                for (jr = 1; jr <= i__2; ++jr) {
/*<                   Z( JR, J ) = -Z( JR, J ) >*/
                    z__[jr + j * z_dim1] = -z__[jr + j * z_dim1];
/*<    20          CONTINUE >*/
/* L20: */
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<          ALPHAR( J ) = A( J, J ) >*/
        alphar[j] = a[j + j * a_dim1];
/*<          ALPHAI( J ) = ZERO >*/
        alphai[j] = 0.;
/*<          BETA( J ) = B( J, J ) >*/
        beta[j] = b[j + j * b_dim1];
/*<    30 CONTINUE >*/
/* L30: */
    }

/*     If IHI < ILO, skip QZ steps */

/*<    >*/
    if (*ihi < *ilo) {
        goto L380;
    }

/*     MAIN QZ ITERATION LOOP */

/*     Initialize dynamic indices */

/*     Eigenvalues ILAST+1:N have been found. */
/*        Column operations modify rows IFRSTM:whatever. */
/*        Row operations modify columns whatever:ILASTM. */

/*     If only eigenvalues are being computed, then */
/*        IFRSTM is the row of the last splitting row above row ILAST; */
/*        this is always at least ILO. */
/*     IITER counts iterations since the last eigenvalue was found, */
/*        to tell when to use an extraordinary shift. */
/*     MAXIT is the maximum number of QZ sweeps allowed. */

/*<       ILAST = IHI >*/
    ilast = *ihi;
/*<       IF( ILSCHR ) THEN >*/
    if (ilschr) {
/*<          IFRSTM = 1 >*/
        ifrstm = 1;
/*<          ILASTM = N >*/
        ilastm = *n;
/*<       ELSE >*/
    } else {
/*<          IFRSTM = ILO >*/
        ifrstm = *ilo;
/*<          ILASTM = IHI >*/
        ilastm = *ihi;
/*<       END IF >*/
    }
/*<       IITER = 0 >*/
    iiter = 0;
/*<       ESHIFT = ZERO >*/
    eshift = 0.;
/*<       MAXIT = 30*( IHI-ILO+1 ) >*/
    maxit = (*ihi - *ilo + 1) * 30;

/*<       DO 360 JITER = 1, MAXIT >*/
    i__1 = maxit;
    for (jiter = 1; jiter <= i__1; ++jiter) {

/*        Split the matrix if possible. */

/*        Two tests: */
/*           1: A(j,j-1)=0  or  j=ILO */
/*           2: B(j,j)=0 */

/*<          IF( ILAST.EQ.ILO ) THEN >*/
        if (ilast == *ilo) {

/*           Special case: j=ILAST */

/*<             GO TO 80 >*/
            goto L80;
/*<          ELSE >*/
        } else {
/*<             IF( ABS( A( ILAST, ILAST-1 ) ).LE.ATOL ) THEN >*/
            if ((d__1 = a[ilast + (ilast - 1) * a_dim1], abs(d__1)) <= atol) {
/*<                A( ILAST, ILAST-1 ) = ZERO >*/
                a[ilast + (ilast - 1) * a_dim1] = 0.;
/*<                GO TO 80 >*/
                goto L80;
/*<             END IF >*/
            }
/*<          END IF >*/
        }

/*<          IF( ABS( B( ILAST, ILAST ) ).LE.BTOL ) THEN >*/
        if ((d__1 = b[ilast + ilast * b_dim1], abs(d__1)) <= btol) {
/*<             B( ILAST, ILAST ) = ZERO >*/
            b[ilast + ilast * b_dim1] = 0.;
/*<             GO TO 70 >*/
            goto L70;
/*<          END IF >*/
        }

/*        General case: j<ILAST */

/*<          DO 60 J = ILAST - 1, ILO, -1 >*/
        i__2 = *ilo;
        for (j = ilast - 1; j >= i__2; --j) {

/*           Test 1: for A(j,j-1)=0 or j=ILO */

/*<             IF( J.EQ.ILO ) THEN >*/
            if (j == *ilo) {
/*<                ILAZRO = .TRUE. >*/
                ilazro = TRUE_;
/*<             ELSE >*/
            } else {
/*<                IF( ABS( A( J, J-1 ) ).LE.ATOL ) THEN >*/
                if ((d__1 = a[j + (j - 1) * a_dim1], abs(d__1)) <= atol) {
/*<                   A( J, J-1 ) = ZERO >*/
                    a[j + (j - 1) * a_dim1] = 0.;
/*<                   ILAZRO = .TRUE. >*/
                    ilazro = TRUE_;
/*<                ELSE >*/
                } else {
/*<                   ILAZRO = .FALSE. >*/
                    ilazro = FALSE_;
/*<                END IF >*/
                }
/*<             END IF >*/
            }

/*           Test 2: for B(j,j)=0 */

/*<             IF( ABS( B( J, J ) ).LT.BTOL ) THEN >*/
            if ((d__1 = b[j + j * b_dim1], abs(d__1)) < btol) {
/*<                B( J, J ) = ZERO >*/
                b[j + j * b_dim1] = 0.;

/*              Test 1a: Check for 2 consecutive small subdiagonals in A */

/*<                ILAZR2 = .FALSE. >*/
                ilazr2 = FALSE_;
/*<                IF( .NOT.ILAZRO ) THEN >*/
                if (! ilazro) {
/*<                   TEMP = ABS( A( J, J-1 ) ) >*/
                    temp = (d__1 = a[j + (j - 1) * a_dim1], abs(d__1));
/*<                   TEMP2 = ABS( A( J, J ) ) >*/
                    temp2 = (d__1 = a[j + j * a_dim1], abs(d__1));
/*<                   TEMPR = MAX( TEMP, TEMP2 ) >*/
                    tempr = max(temp,temp2);
/*<                   IF( TEMPR.LT.ONE .AND. TEMPR.NE.ZERO ) THEN >*/
                    if (tempr < 1. && tempr != 0.) {
/*<                      TEMP = TEMP / TEMPR >*/
                        temp /= tempr;
/*<                      TEMP2 = TEMP2 / TEMPR >*/
                        temp2 /= tempr;
/*<                   END IF >*/
                    }
/*<    >*/
                    if (temp * (ascale * (d__1 = a[j + 1 + j * a_dim1], abs(
                            d__1))) <= temp2 * (ascale * atol)) {
                        ilazr2 = TRUE_;
                    }
/*<                END IF >*/
                }

/*              If both tests pass (1 & 2), i.e., the leading diagonal */
/*              element of B in the block is zero, split a 1x1 block off */
/*              at the top. (I.e., at the J-th row/column) The leading */
/*              diagonal element of the remainder can also be zero, so */
/*              this may have to be done repeatedly. */

/*<                IF( ILAZRO .OR. ILAZR2 ) THEN >*/
                if (ilazro || ilazr2) {
/*<                   DO 40 JCH = J, ILAST - 1 >*/
                    i__3 = ilast - 1;
                    for (jch = j; jch <= i__3; ++jch) {
/*<                      TEMP = A( JCH, JCH ) >*/
                        temp = a[jch + jch * a_dim1];
/*<    >*/
                        dlartg_(&temp, &a[jch + 1 + jch * a_dim1], &c__, &s, &
                                a[jch + jch * a_dim1]);
/*<                      A( JCH+1, JCH ) = ZERO >*/
                        a[jch + 1 + jch * a_dim1] = 0.;
/*<    >*/
                        i__4 = ilastm - jch;
                        drot_(&i__4, &a[jch + (jch + 1) * a_dim1], lda, &a[
                                jch + 1 + (jch + 1) * a_dim1], lda, &c__, &s);
/*<    >*/
                        i__4 = ilastm - jch;
                        drot_(&i__4, &b[jch + (jch + 1) * b_dim1], ldb, &b[
                                jch + 1 + (jch + 1) * b_dim1], ldb, &c__, &s);
/*<    >*/
                        if (ilq) {
                            drot_(n, &q[jch * q_dim1 + 1], &c__1, &q[(jch + 1)
                                     * q_dim1 + 1], &c__1, &c__, &s);
                        }
/*<    >*/
                        if (ilazr2) {
                            a[jch + (jch - 1) * a_dim1] *= c__;
                        }
/*<                      ILAZR2 = .FALSE. >*/
                        ilazr2 = FALSE_;
/*<                      IF( ABS( B( JCH+1, JCH+1 ) ).GE.BTOL ) THEN >*/
                        if ((d__1 = b[jch + 1 + (jch + 1) * b_dim1], abs(d__1)
                                ) >= btol) {
/*<                         IF( JCH+1.GE.ILAST ) THEN >*/
                            if (jch + 1 >= ilast) {
/*<                            GO TO 80 >*/
                                goto L80;
/*<                         ELSE >*/
                            } else {
/*<                            IFIRST = JCH + 1 >*/
                                ifirst = jch + 1;
/*<                            GO TO 110 >*/
                                goto L110;
/*<                         END IF >*/
                            }
/*<                      END IF >*/
                        }
/*<                      B( JCH+1, JCH+1 ) = ZERO >*/
                        b[jch + 1 + (jch + 1) * b_dim1] = 0.;
/*<    40             CONTINUE >*/
/* L40: */
                    }
/*<                   GO TO 70 >*/
                    goto L70;
/*<                ELSE >*/
                } else {

/*                 Only test 2 passed -- chase the zero to B(ILAST,ILAST) */
/*                 Then process as in the case B(ILAST,ILAST)=0 */

/*<                   DO 50 JCH = J, ILAST - 1 >*/
                    i__3 = ilast - 1;
                    for (jch = j; jch <= i__3; ++jch) {
/*<                      TEMP = B( JCH, JCH+1 ) >*/
                        temp = b[jch + (jch + 1) * b_dim1];
/*<    >*/
                        dlartg_(&temp, &b[jch + 1 + (jch + 1) * b_dim1], &c__,
                                 &s, &b[jch + (jch + 1) * b_dim1]);
/*<                      B( JCH+1, JCH+1 ) = ZERO >*/
                        b[jch + 1 + (jch + 1) * b_dim1] = 0.;
/*<    >*/
                        if (jch < ilastm - 1) {
                            i__4 = ilastm - jch - 1;
                            drot_(&i__4, &b[jch + (jch + 2) * b_dim1], ldb, &
                                    b[jch + 1 + (jch + 2) * b_dim1], ldb, &
                                    c__, &s);
                        }
/*<    >*/
                        i__4 = ilastm - jch + 2;
                        drot_(&i__4, &a[jch + (jch - 1) * a_dim1], lda, &a[
                                jch + 1 + (jch - 1) * a_dim1], lda, &c__, &s);
/*<    >*/
                        if (ilq) {
                            drot_(n, &q[jch * q_dim1 + 1], &c__1, &q[(jch + 1)
                                     * q_dim1 + 1], &c__1, &c__, &s);
                        }
/*<                      TEMP = A( JCH+1, JCH ) >*/
                        temp = a[jch + 1 + jch * a_dim1];
/*<    >*/
                        dlartg_(&temp, &a[jch + 1 + (jch - 1) * a_dim1], &c__,
                                 &s, &a[jch + 1 + jch * a_dim1]);
/*<                      A( JCH+1, JCH-1 ) = ZERO >*/
                        a[jch + 1 + (jch - 1) * a_dim1] = 0.;
/*<    >*/
                        i__4 = jch + 1 - ifrstm;
                        drot_(&i__4, &a[ifrstm + jch * a_dim1], &c__1, &a[
                                ifrstm + (jch - 1) * a_dim1], &c__1, &c__, &s)
                                ;
/*<    >*/
                        i__4 = jch - ifrstm;
                        drot_(&i__4, &b[ifrstm + jch * b_dim1], &c__1, &b[
                                ifrstm + (jch - 1) * b_dim1], &c__1, &c__, &s)
                                ;
/*<    >*/
                        if (ilz) {
                            drot_(n, &z__[jch * z_dim1 + 1], &c__1, &z__[(jch
                                    - 1) * z_dim1 + 1], &c__1, &c__, &s);
                        }
/*<    50             CONTINUE >*/
/* L50: */
                    }
/*<                   GO TO 70 >*/
                    goto L70;
/*<                END IF >*/
                }
/*<             ELSE IF( ILAZRO ) THEN >*/
            } else if (ilazro) {

/*              Only test 1 passed -- work on J:ILAST */

/*<                IFIRST = J >*/
                ifirst = j;
/*<                GO TO 110 >*/
                goto L110;
/*<             END IF >*/
            }

/*           Neither test passed -- try next J */

/*<    60    CONTINUE >*/
/* L60: */
        }

/*        (Drop-through is "impossible") */

/*<          INFO = N + 1 >*/
        *info = *n + 1;
/*<          GO TO 420 >*/
        goto L420;

/*        B(ILAST,ILAST)=0 -- clear A(ILAST,ILAST-1) to split off a */
/*        1x1 block. */

/*<    70    CONTINUE >*/
L70:
/*<          TEMP = A( ILAST, ILAST ) >*/
        temp = a[ilast + ilast * a_dim1];
/*<    >*/
        dlartg_(&temp, &a[ilast + (ilast - 1) * a_dim1], &c__, &s, &a[ilast +
                ilast * a_dim1]);
/*<          A( ILAST, ILAST-1 ) = ZERO >*/
        a[ilast + (ilast - 1) * a_dim1] = 0.;
/*<    >*/
        i__2 = ilast - ifrstm;
        drot_(&i__2, &a[ifrstm + ilast * a_dim1], &c__1, &a[ifrstm + (ilast -
                1) * a_dim1], &c__1, &c__, &s);
/*<    >*/
        i__2 = ilast - ifrstm;
        drot_(&i__2, &b[ifrstm + ilast * b_dim1], &c__1, &b[ifrstm + (ilast -
                1) * b_dim1], &c__1, &c__, &s);
/*<    >*/
        if (ilz) {
            drot_(n, &z__[ilast * z_dim1 + 1], &c__1, &z__[(ilast - 1) *
                    z_dim1 + 1], &c__1, &c__, &s);
        }

/*        A(ILAST,ILAST-1)=0 -- Standardize B, set ALPHAR, ALPHAI, */
/*                              and BETA */

/*<    80    CONTINUE >*/
L80:
/*<          IF( B( ILAST, ILAST ).LT.ZERO ) THEN >*/
        if (b[ilast + ilast * b_dim1] < 0.) {
/*<             IF( ILSCHR ) THEN >*/
            if (ilschr) {
/*<                DO 90 J = IFRSTM, ILAST >*/
                i__2 = ilast;
                for (j = ifrstm; j <= i__2; ++j) {
/*<                   A( J, ILAST ) = -A( J, ILAST ) >*/
                    a[j + ilast * a_dim1] = -a[j + ilast * a_dim1];
/*<                   B( J, ILAST ) = -B( J, ILAST ) >*/
                    b[j + ilast * b_dim1] = -b[j + ilast * b_dim1];
/*<    90          CONTINUE >*/
/* L90: */
                }
/*<             ELSE >*/
            } else {
/*<                A( ILAST, ILAST ) = -A( ILAST, ILAST ) >*/
                a[ilast + ilast * a_dim1] = -a[ilast + ilast * a_dim1];
/*<                B( ILAST, ILAST ) = -B( ILAST, ILAST ) >*/
                b[ilast + ilast * b_dim1] = -b[ilast + ilast * b_dim1];
/*<             END IF >*/
            }
/*<             IF( ILZ ) THEN >*/
            if (ilz) {
/*<                DO 100 J = 1, N >*/
                i__2 = *n;
                for (j = 1; j <= i__2; ++j) {
/*<                   Z( J, ILAST ) = -Z( J, ILAST ) >*/
                    z__[j + ilast * z_dim1] = -z__[j + ilast * z_dim1];
/*<   100          CONTINUE >*/
/* L100: */
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<          ALPHAR( ILAST ) = A( ILAST, ILAST ) >*/
        alphar[ilast] = a[ilast + ilast * a_dim1];
/*<          ALPHAI( ILAST ) = ZERO >*/
        alphai[ilast] = 0.;
/*<          BETA( ILAST ) = B( ILAST, ILAST ) >*/
        beta[ilast] = b[ilast + ilast * b_dim1];

/*        Go to next block -- exit if finished. */

/*<          ILAST = ILAST - 1 >*/
        --ilast;
/*<    >*/
        if (ilast < *ilo) {
            goto L380;
        }

/*        Reset counters */

/*<          IITER = 0 >*/
        iiter = 0;
/*<          ESHIFT = ZERO >*/
        eshift = 0.;
/*<          IF( .NOT.ILSCHR ) THEN >*/
        if (! ilschr) {
/*<             ILASTM = ILAST >*/
            ilastm = ilast;
/*<    >*/
            if (ifrstm > ilast) {
                ifrstm = *ilo;
            }
/*<          END IF >*/
        }
/*<          GO TO 350 >*/
        goto L350;

/*        QZ step */

/*        This iteration only involves rows/columns IFIRST:ILAST. We */
/*        assume IFIRST < ILAST, and that the diagonal of B is non-zero. */

/*<   110    CONTINUE >*/
L110:
/*<          IITER = IITER + 1 >*/
        ++iiter;
/*<          IF( .NOT.ILSCHR ) THEN >*/
        if (! ilschr) {
/*<             IFRSTM = IFIRST >*/
            ifrstm = ifirst;
/*<          END IF >*/
        }

/*        Compute single shifts. */

/*        At this point, IFIRST < ILAST, and the diagonal elements of */
/*        B(IFIRST:ILAST,IFIRST,ILAST) are larger than BTOL (in */
/*        magnitude) */

/*<          IF( ( IITER / 10 )*10.EQ.IITER ) THEN >*/
        if (iiter / 10 * 10 == iiter) {

/*           Exceptional shift.  Chosen for no particularly good reason. */
/*           (Single shift only.) */

/*<    >*/
            if ((doublereal) maxit * safmin * (d__1 = a[ilast - 1 + ilast *
                    a_dim1], abs(d__1)) < (d__2 = b[ilast - 1 + (ilast - 1) *
                    b_dim1], abs(d__2))) {
/*<    >*/
                eshift += a[ilast - 1 + ilast * a_dim1] / b[ilast - 1 + (
                        ilast - 1) * b_dim1];
/*<             ELSE >*/
            } else {
/*<                ESHIFT = ESHIFT + ONE / ( SAFMIN*DBLE( MAXIT ) ) >*/
                eshift += 1. / (safmin * (doublereal) maxit);
/*<             END IF >*/
            }
/*<             S1 = ONE >*/
            s1 = 1.;
/*<             WR = ESHIFT >*/
            wr = eshift;

/*<          ELSE >*/
        } else {

/*           Shifts based on the generalized eigenvalues of the */
/*           bottom-right 2x2 block of A and B. The first eigenvalue */
/*           returned by DLAG2 is the Wilkinson shift (AEP p.512), */

/*<    >*/
            d__1 = safmin * 100.;
            dlag2_(&a[ilast - 1 + (ilast - 1) * a_dim1], lda, &b[ilast - 1 + (
                    ilast - 1) * b_dim1], ldb, &d__1, &s1, &s2, &wr, &wr2, &
                    wi);

/*<             TEMP = MAX( S1, SAFMIN*MAX( ONE, ABS( WR ), ABS( WI ) ) ) >*/
/* Computing MAX */
/* Computing MAX */
            d__3 = 1., d__4 = abs(wr), d__3 = max(d__3,d__4), d__4 = abs(wi);
            d__1 = s1, d__2 = safmin * max(d__3,d__4);
            temp = max(d__1,d__2);
/*<    >*/
            if (wi != 0.) {
                goto L200;
            }
/*<          END IF >*/
        }

/*        Fiddle with shift to avoid overflow */

/*<          TEMP = MIN( ASCALE, ONE )*( HALF*SAFMAX ) >*/
        temp = min(ascale,1.) * (safmax * .5);
/*<          IF( S1.GT.TEMP ) THEN >*/
        if (s1 > temp) {
/*<             SCALE = TEMP / S1 >*/
            scale = temp / s1;
/*<          ELSE >*/
        } else {
/*<             SCALE = ONE >*/
            scale = 1.;
/*<          END IF >*/
        }

/*<          TEMP = MIN( BSCALE, ONE )*( HALF*SAFMAX ) >*/
        temp = min(bscale,1.) * (safmax * .5);
/*<    >*/
        if (abs(wr) > temp) {
/* Computing MIN */
            d__1 = scale, d__2 = temp / abs(wr);
            scale = min(d__1,d__2);
        }
/*<          S1 = SCALE*S1 >*/
        s1 = scale * s1;
/*<          WR = SCALE*WR >*/
        wr = scale * wr;

/*        Now check for two consecutive small subdiagonals. */

/*<          DO 120 J = ILAST - 1, IFIRST + 1, -1 >*/
        i__2 = ifirst + 1;
        for (j = ilast - 1; j >= i__2; --j) {
/*<             ISTART = J >*/
            istart = j;
/*<             TEMP = ABS( S1*A( J, J-1 ) ) >*/
            temp = (d__1 = s1 * a[j + (j - 1) * a_dim1], abs(d__1));
/*<             TEMP2 = ABS( S1*A( J, J )-WR*B( J, J ) ) >*/
            temp2 = (d__1 = s1 * a[j + j * a_dim1] - wr * b[j + j * b_dim1],
                    abs(d__1));
/*<             TEMPR = MAX( TEMP, TEMP2 ) >*/
            tempr = max(temp,temp2);
/*<             IF( TEMPR.LT.ONE .AND. TEMPR.NE.ZERO ) THEN >*/
            if (tempr < 1. && tempr != 0.) {
/*<                TEMP = TEMP / TEMPR >*/
                temp /= tempr;
/*<                TEMP2 = TEMP2 / TEMPR >*/
                temp2 /= tempr;
/*<             END IF >*/
            }
/*<    >*/
            if ((d__1 = ascale * a[j + 1 + j * a_dim1] * temp, abs(d__1)) <=
                    ascale * atol * temp2) {
                goto L130;
            }
/*<   120    CONTINUE >*/
/* L120: */
        }

/*<          ISTART = IFIRST >*/
        istart = ifirst;
/*<   130    CONTINUE >*/
L130:

/*        Do an implicit single-shift QZ sweep. */

/*        Initial Q */

/*<          TEMP = S1*A( ISTART, ISTART ) - WR*B( ISTART, ISTART ) >*/
        temp = s1 * a[istart + istart * a_dim1] - wr * b[istart + istart *
                b_dim1];
/*<          TEMP2 = S1*A( ISTART+1, ISTART ) >*/
        temp2 = s1 * a[istart + 1 + istart * a_dim1];
/*<          CALL DLARTG( TEMP, TEMP2, C, S, TEMPR ) >*/
        dlartg_(&temp, &temp2, &c__, &s, &tempr);

/*        Sweep */

/*<          DO 190 J = ISTART, ILAST - 1 >*/
        i__2 = ilast - 1;
        for (j = istart; j <= i__2; ++j) {
/*<             IF( J.GT.ISTART ) THEN >*/
            if (j > istart) {
/*<                TEMP = A( J, J-1 ) >*/
                temp = a[j + (j - 1) * a_dim1];
/*<                CALL DLARTG( TEMP, A( J+1, J-1 ), C, S, A( J, J-1 ) ) >*/
                dlartg_(&temp, &a[j + 1 + (j - 1) * a_dim1], &c__, &s, &a[j +
                        (j - 1) * a_dim1]);
/*<                A( J+1, J-1 ) = ZERO >*/
                a[j + 1 + (j - 1) * a_dim1] = 0.;
/*<             END IF >*/
            }

/*<             DO 140 JC = J, ILASTM >*/
            i__3 = ilastm;
            for (jc = j; jc <= i__3; ++jc) {
/*<                TEMP = C*A( J, JC ) + S*A( J+1, JC ) >*/
                temp = c__ * a[j + jc * a_dim1] + s * a[j + 1 + jc * a_dim1];
/*<                A( J+1, JC ) = -S*A( J, JC ) + C*A( J+1, JC ) >*/
                a[j + 1 + jc * a_dim1] = -s * a[j + jc * a_dim1] + c__ * a[j
                        + 1 + jc * a_dim1];
/*<                A( J, JC ) = TEMP >*/
                a[j + jc * a_dim1] = temp;
/*<                TEMP2 = C*B( J, JC ) + S*B( J+1, JC ) >*/
                temp2 = c__ * b[j + jc * b_dim1] + s * b[j + 1 + jc * b_dim1];
/*<                B( J+1, JC ) = -S*B( J, JC ) + C*B( J+1, JC ) >*/
                b[j + 1 + jc * b_dim1] = -s * b[j + jc * b_dim1] + c__ * b[j
                        + 1 + jc * b_dim1];
/*<                B( J, JC ) = TEMP2 >*/
                b[j + jc * b_dim1] = temp2;
/*<   140       CONTINUE >*/
/* L140: */
            }
/*<             IF( ILQ ) THEN >*/
            if (ilq) {
/*<                DO 150 JR = 1, N >*/
                i__3 = *n;
                for (jr = 1; jr <= i__3; ++jr) {
/*<                   TEMP = C*Q( JR, J ) + S*Q( JR, J+1 ) >*/
                    temp = c__ * q[jr + j * q_dim1] + s * q[jr + (j + 1) *
                            q_dim1];
/*<                   Q( JR, J+1 ) = -S*Q( JR, J ) + C*Q( JR, J+1 ) >*/
                    q[jr + (j + 1) * q_dim1] = -s * q[jr + j * q_dim1] + c__ *
                             q[jr + (j + 1) * q_dim1];
/*<                   Q( JR, J ) = TEMP >*/
                    q[jr + j * q_dim1] = temp;
/*<   150          CONTINUE >*/
/* L150: */
                }
/*<             END IF >*/
            }

/*<             TEMP = B( J+1, J+1 ) >*/
            temp = b[j + 1 + (j + 1) * b_dim1];
/*<             CALL DLARTG( TEMP, B( J+1, J ), C, S, B( J+1, J+1 ) ) >*/
            dlartg_(&temp, &b[j + 1 + j * b_dim1], &c__, &s, &b[j + 1 + (j +
                    1) * b_dim1]);
/*<             B( J+1, J ) = ZERO >*/
            b[j + 1 + j * b_dim1] = 0.;

/*<             DO 160 JR = IFRSTM, MIN( J+2, ILAST ) >*/
/* Computing MIN */
            i__4 = j + 2;
            i__3 = min(i__4,ilast);
            for (jr = ifrstm; jr <= i__3; ++jr) {
/*<                TEMP = C*A( JR, J+1 ) + S*A( JR, J ) >*/
                temp = c__ * a[jr + (j + 1) * a_dim1] + s * a[jr + j * a_dim1]
                        ;
/*<                A( JR, J ) = -S*A( JR, J+1 ) + C*A( JR, J ) >*/
                a[jr + j * a_dim1] = -s * a[jr + (j + 1) * a_dim1] + c__ * a[
                        jr + j * a_dim1];
/*<                A( JR, J+1 ) = TEMP >*/
                a[jr + (j + 1) * a_dim1] = temp;
/*<   160       CONTINUE >*/
/* L160: */
            }
/*<             DO 170 JR = IFRSTM, J >*/
            i__3 = j;
            for (jr = ifrstm; jr <= i__3; ++jr) {
/*<                TEMP = C*B( JR, J+1 ) + S*B( JR, J ) >*/
                temp = c__ * b[jr + (j + 1) * b_dim1] + s * b[jr + j * b_dim1]
                        ;
/*<                B( JR, J ) = -S*B( JR, J+1 ) + C*B( JR, J ) >*/
                b[jr + j * b_dim1] = -s * b[jr + (j + 1) * b_dim1] + c__ * b[
                        jr + j * b_dim1];
/*<                B( JR, J+1 ) = TEMP >*/
                b[jr + (j + 1) * b_dim1] = temp;
/*<   170       CONTINUE >*/
/* L170: */
            }
/*<             IF( ILZ ) THEN >*/
            if (ilz) {
/*<                DO 180 JR = 1, N >*/
                i__3 = *n;
                for (jr = 1; jr <= i__3; ++jr) {
/*<                   TEMP = C*Z( JR, J+1 ) + S*Z( JR, J ) >*/
                    temp = c__ * z__[jr + (j + 1) * z_dim1] + s * z__[jr + j *
                             z_dim1];
/*<                   Z( JR, J ) = -S*Z( JR, J+1 ) + C*Z( JR, J ) >*/
                    z__[jr + j * z_dim1] = -s * z__[jr + (j + 1) * z_dim1] +
                            c__ * z__[jr + j * z_dim1];
/*<                   Z( JR, J+1 ) = TEMP >*/
                    z__[jr + (j + 1) * z_dim1] = temp;
/*<   180          CONTINUE >*/
/* L180: */
                }
/*<             END IF >*/
            }
/*<   190    CONTINUE >*/
/* L190: */
        }

/*<          GO TO 350 >*/
        goto L350;

/*        Use Francis double-shift */

/*        Note: the Francis double-shift should work with real shifts, */
/*              but only if the block is at least 3x3. */
/*              This code may break if this point is reached with */
/*              a 2x2 block with real eigenvalues. */

/*<   200    CONTINUE >*/
L200:
/*<          IF( IFIRST+1.EQ.ILAST ) THEN >*/
        if (ifirst + 1 == ilast) {

/*           Special case -- 2x2 block with complex eigenvectors */

/*           Step 1: Standardize, that is, rotate so that */

/*                       ( B11  0  ) */
/*                   B = (         )  with B11 non-negative. */
/*                       (  0  B22 ) */

/*<    >*/
            dlasv2_(&b[ilast - 1 + (ilast - 1) * b_dim1], &b[ilast - 1 +
                    ilast * b_dim1], &b[ilast + ilast * b_dim1], &b22, &b11, &
                    sr, &cr, &sl, &cl);

/*<             IF( B11.LT.ZERO ) THEN >*/
            if (b11 < 0.) {
/*<                CR = -CR >*/
                cr = -cr;
/*<                SR = -SR >*/
                sr = -sr;
/*<                B11 = -B11 >*/
                b11 = -b11;
/*<                B22 = -B22 >*/
                b22 = -b22;
/*<             END IF >*/
            }

/*<    >*/
            i__2 = ilastm + 1 - ifirst;
            drot_(&i__2, &a[ilast - 1 + (ilast - 1) * a_dim1], lda, &a[ilast
                    + (ilast - 1) * a_dim1], lda, &cl, &sl);
/*<    >*/
            i__2 = ilast + 1 - ifrstm;
            drot_(&i__2, &a[ifrstm + (ilast - 1) * a_dim1], &c__1, &a[ifrstm
                    + ilast * a_dim1], &c__1, &cr, &sr);

/*<    >*/
            if (ilast < ilastm) {
                i__2 = ilastm - ilast;
                drot_(&i__2, &b[ilast - 1 + (ilast + 1) * b_dim1], ldb, &b[
                        ilast + (ilast + 1) * b_dim1], lda, &cl, &sl);
            }
/*<    >*/
            if (ifrstm < ilast - 1) {
                i__2 = ifirst - ifrstm;
                drot_(&i__2, &b[ifrstm + (ilast - 1) * b_dim1], &c__1, &b[
                        ifrstm + ilast * b_dim1], &c__1, &cr, &sr);
            }

/*<    >*/
            if (ilq) {
                drot_(n, &q[(ilast - 1) * q_dim1 + 1], &c__1, &q[ilast *
                        q_dim1 + 1], &c__1, &cl, &sl);
            }
/*<    >*/
            if (ilz) {
                drot_(n, &z__[(ilast - 1) * z_dim1 + 1], &c__1, &z__[ilast *
                        z_dim1 + 1], &c__1, &cr, &sr);
            }

/*<             B( ILAST-1, ILAST-1 ) = B11 >*/
            b[ilast - 1 + (ilast - 1) * b_dim1] = b11;
/*<             B( ILAST-1, ILAST ) = ZERO >*/
            b[ilast - 1 + ilast * b_dim1] = 0.;
/*<             B( ILAST, ILAST-1 ) = ZERO >*/
            b[ilast + (ilast - 1) * b_dim1] = 0.;
/*<             B( ILAST, ILAST ) = B22 >*/
            b[ilast + ilast * b_dim1] = b22;

/*           If B22 is negative, negate column ILAST */

/*<             IF( B22.LT.ZERO ) THEN >*/
            if (b22 < 0.) {
/*<                DO 210 J = IFRSTM, ILAST >*/
                i__2 = ilast;
                for (j = ifrstm; j <= i__2; ++j) {
/*<                   A( J, ILAST ) = -A( J, ILAST ) >*/
                    a[j + ilast * a_dim1] = -a[j + ilast * a_dim1];
/*<                   B( J, ILAST ) = -B( J, ILAST ) >*/
                    b[j + ilast * b_dim1] = -b[j + ilast * b_dim1];
/*<   210          CONTINUE >*/
/* L210: */
                }

/*<                IF( ILZ ) THEN >*/
                if (ilz) {
/*<                   DO 220 J = 1, N >*/
                    i__2 = *n;
                    for (j = 1; j <= i__2; ++j) {
/*<                      Z( J, ILAST ) = -Z( J, ILAST ) >*/
                        z__[j + ilast * z_dim1] = -z__[j + ilast * z_dim1];
/*<   220             CONTINUE >*/
/* L220: */
                    }
/*<                END IF >*/
                }
/*<             END IF >*/
            }

/*           Step 2: Compute ALPHAR, ALPHAI, and BETA (see refs.) */

/*           Recompute shift */

/*<    >*/
            d__1 = safmin * 100.;
            dlag2_(&a[ilast - 1 + (ilast - 1) * a_dim1], lda, &b[ilast - 1 + (
                    ilast - 1) * b_dim1], ldb, &d__1, &s1, &temp, &wr, &temp2,
                     &wi);

/*           If standardization has perturbed the shift onto real line, */
/*           do another (real single-shift) QR step. */

/*<    >*/
            if (wi == 0.) {
                goto L350;
            }
/*<             S1INV = ONE / S1 >*/
            s1inv = 1. / s1;

/*           Do EISPACK (QZVAL) computation of alpha and beta */

/*<             A11 = A( ILAST-1, ILAST-1 ) >*/
            a11 = a[ilast - 1 + (ilast - 1) * a_dim1];
/*<             A21 = A( ILAST, ILAST-1 ) >*/
            a21 = a[ilast + (ilast - 1) * a_dim1];
/*<             A12 = A( ILAST-1, ILAST ) >*/
            a12 = a[ilast - 1 + ilast * a_dim1];
/*<             A22 = A( ILAST, ILAST ) >*/
            a22 = a[ilast + ilast * a_dim1];

/*           Compute complex Givens rotation on right */
/*           (Assume some element of C = (sA - wB) > unfl ) */
/*                            __ */
/*           (sA - wB) ( CZ   -SZ ) */
/*                     ( SZ    CZ ) */

/*<             C11R = S1*A11 - WR*B11 >*/
            c11r = s1 * a11 - wr * b11;
/*<             C11I = -WI*B11 >*/
            c11i = -wi * b11;
/*<             C12 = S1*A12 >*/
            c12 = s1 * a12;
/*<             C21 = S1*A21 >*/
            c21 = s1 * a21;
/*<             C22R = S1*A22 - WR*B22 >*/
            c22r = s1 * a22 - wr * b22;
/*<             C22I = -WI*B22 >*/
            c22i = -wi * b22;

/*<    >*/
            if (abs(c11r) + abs(c11i) + abs(c12) > abs(c21) + abs(c22r) + abs(
                    c22i)) {
/*<                T = DLAPY3( C12, C11R, C11I ) >*/
                t = dlapy3_(&c12, &c11r, &c11i);
/*<                CZ = C12 / T >*/
                cz = c12 / t;
/*<                SZR = -C11R / T >*/
                szr = -c11r / t;
/*<                SZI = -C11I / T >*/
                szi = -c11i / t;
/*<             ELSE >*/
            } else {
/*<                CZ = DLAPY2( C22R, C22I ) >*/
                cz = dlapy2_(&c22r, &c22i);
/*<                IF( CZ.LE.SAFMIN ) THEN >*/
                if (cz <= safmin) {
/*<                   CZ = ZERO >*/
                    cz = 0.;
/*<                   SZR = ONE >*/
                    szr = 1.;
/*<                   SZI = ZERO >*/
                    szi = 0.;
/*<                ELSE >*/
                } else {
/*<                   TEMPR = C22R / CZ >*/
                    tempr = c22r / cz;
/*<                   TEMPI = C22I / CZ >*/
                    tempi = c22i / cz;
/*<                   T = DLAPY2( CZ, C21 ) >*/
                    t = dlapy2_(&cz, &c21);
/*<                   CZ = CZ / T >*/
                    cz /= t;
/*<                   SZR = -C21*TEMPR / T >*/
                    szr = -c21 * tempr / t;
/*<                   SZI = C21*TEMPI / T >*/
                    szi = c21 * tempi / t;
/*<                END IF >*/
                }
/*<             END IF >*/
            }

/*           Compute Givens rotation on left */

/*           (  CQ   SQ ) */
/*           (  __      )  A or B */
/*           ( -SQ   CQ ) */

/*<             AN = ABS( A11 ) + ABS( A12 ) + ABS( A21 ) + ABS( A22 ) >*/
            an = abs(a11) + abs(a12) + abs(a21) + abs(a22);
/*<             BN = ABS( B11 ) + ABS( B22 ) >*/
            bn = abs(b11) + abs(b22);
/*<             WABS = ABS( WR ) + ABS( WI ) >*/
            wabs = abs(wr) + abs(wi);
/*<             IF( S1*AN.GT.WABS*BN ) THEN >*/
            if (s1 * an > wabs * bn) {
/*<                CQ = CZ*B11 >*/
                cq = cz * b11;
/*<                SQR = SZR*B22 >*/
                sqr = szr * b22;
/*<                SQI = -SZI*B22 >*/
                sqi = -szi * b22;
/*<             ELSE >*/
            } else {
/*<                A1R = CZ*A11 + SZR*A12 >*/
                a1r = cz * a11 + szr * a12;
/*<                A1I = SZI*A12 >*/
                a1i = szi * a12;
/*<                A2R = CZ*A21 + SZR*A22 >*/
                a2r = cz * a21 + szr * a22;
/*<                A2I = SZI*A22 >*/
                a2i = szi * a22;
/*<                CQ = DLAPY2( A1R, A1I ) >*/
                cq = dlapy2_(&a1r, &a1i);
/*<                IF( CQ.LE.SAFMIN ) THEN >*/
                if (cq <= safmin) {
/*<                   CQ = ZERO >*/
                    cq = 0.;
/*<                   SQR = ONE >*/
                    sqr = 1.;
/*<                   SQI = ZERO >*/
                    sqi = 0.;
/*<                ELSE >*/
                } else {
/*<                   TEMPR = A1R / CQ >*/
                    tempr = a1r / cq;
/*<                   TEMPI = A1I / CQ >*/
                    tempi = a1i / cq;
/*<                   SQR = TEMPR*A2R + TEMPI*A2I >*/
                    sqr = tempr * a2r + tempi * a2i;
/*<                   SQI = TEMPI*A2R - TEMPR*A2I >*/
                    sqi = tempi * a2r - tempr * a2i;
/*<                END IF >*/
                }
/*<             END IF >*/
            }
/*<             T = DLAPY3( CQ, SQR, SQI ) >*/
            t = dlapy3_(&cq, &sqr, &sqi);
/*<             CQ = CQ / T >*/
            cq /= t;
/*<             SQR = SQR / T >*/
            sqr /= t;
/*<             SQI = SQI / T >*/
            sqi /= t;

/*           Compute diagonal elements of QBZ */

/*<             TEMPR = SQR*SZR - SQI*SZI >*/
            tempr = sqr * szr - sqi * szi;
/*<             TEMPI = SQR*SZI + SQI*SZR >*/
            tempi = sqr * szi + sqi * szr;
/*<             B1R = CQ*CZ*B11 + TEMPR*B22 >*/
            b1r = cq * cz * b11 + tempr * b22;
/*<             B1I = TEMPI*B22 >*/
            b1i = tempi * b22;
/*<             B1A = DLAPY2( B1R, B1I ) >*/
            b1a = dlapy2_(&b1r, &b1i);
/*<             B2R = CQ*CZ*B22 + TEMPR*B11 >*/
            b2r = cq * cz * b22 + tempr * b11;
/*<             B2I = -TEMPI*B11 >*/
            b2i = -tempi * b11;
/*<             B2A = DLAPY2( B2R, B2I ) >*/
            b2a = dlapy2_(&b2r, &b2i);

/*           Normalize so beta > 0, and Im( alpha1 ) > 0 */

/*<             BETA( ILAST-1 ) = B1A >*/
            beta[ilast - 1] = b1a;
/*<             BETA( ILAST ) = B2A >*/
            beta[ilast] = b2a;
/*<             ALPHAR( ILAST-1 ) = ( WR*B1A )*S1INV >*/
            alphar[ilast - 1] = wr * b1a * s1inv;
/*<             ALPHAI( ILAST-1 ) = ( WI*B1A )*S1INV >*/
            alphai[ilast - 1] = wi * b1a * s1inv;
/*<             ALPHAR( ILAST ) = ( WR*B2A )*S1INV >*/
            alphar[ilast] = wr * b2a * s1inv;
/*<             ALPHAI( ILAST ) = -( WI*B2A )*S1INV >*/
            alphai[ilast] = -(wi * b2a) * s1inv;

/*           Step 3: Go to next block -- exit if finished. */

/*<             ILAST = IFIRST - 1 >*/
            ilast = ifirst - 1;
/*<    >*/
            if (ilast < *ilo) {
                goto L380;
            }

/*           Reset counters */

/*<             IITER = 0 >*/
            iiter = 0;
/*<             ESHIFT = ZERO >*/
            eshift = 0.;
/*<             IF( .NOT.ILSCHR ) THEN >*/
            if (! ilschr) {
/*<                ILASTM = ILAST >*/
                ilastm = ilast;
/*<    >*/
                if (ifrstm > ilast) {
                    ifrstm = *ilo;
                }
/*<             END IF >*/
            }
/*<             GO TO 350 >*/
            goto L350;
/*<          ELSE >*/
        } else {

/*           Usual case: 3x3 or larger block, using Francis implicit */
/*                       double-shift */

/*                                    2 */
/*           Eigenvalue equation is  w  - c w + d = 0, */

/*                                         -1 2        -1 */
/*           so compute 1st column of  (A B  )  - c A B   + d */
/*           using the formula in QZIT (from EISPACK) */

/*           We assume that the block is at least 3x3 */

/*<    >*/
            ad11 = ascale * a[ilast - 1 + (ilast - 1) * a_dim1] / (bscale * b[
                    ilast - 1 + (ilast - 1) * b_dim1]);
/*<    >*/
            ad21 = ascale * a[ilast + (ilast - 1) * a_dim1] / (bscale * b[
                    ilast - 1 + (ilast - 1) * b_dim1]);
/*<    >*/
            ad12 = ascale * a[ilast - 1 + ilast * a_dim1] / (bscale * b[ilast
                    + ilast * b_dim1]);
/*<    >*/
            ad22 = ascale * a[ilast + ilast * a_dim1] / (bscale * b[ilast +
                    ilast * b_dim1]);
/*<             U12 = B( ILAST-1, ILAST ) / B( ILAST, ILAST ) >*/
            u12 = b[ilast - 1 + ilast * b_dim1] / b[ilast + ilast * b_dim1];
/*<    >*/
            ad11l = ascale * a[ifirst + ifirst * a_dim1] / (bscale * b[ifirst
                    + ifirst * b_dim1]);
/*<    >*/
            ad21l = ascale * a[ifirst + 1 + ifirst * a_dim1] / (bscale * b[
                    ifirst + ifirst * b_dim1]);
/*<    >*/
            ad12l = ascale * a[ifirst + (ifirst + 1) * a_dim1] / (bscale * b[
                    ifirst + 1 + (ifirst + 1) * b_dim1]);
/*<    >*/
            ad22l = ascale * a[ifirst + 1 + (ifirst + 1) * a_dim1] / (bscale *
                     b[ifirst + 1 + (ifirst + 1) * b_dim1]);
/*<    >*/
            ad32l = ascale * a[ifirst + 2 + (ifirst + 1) * a_dim1] / (bscale *
                     b[ifirst + 1 + (ifirst + 1) * b_dim1]);
/*<             U12L = B( IFIRST, IFIRST+1 ) / B( IFIRST+1, IFIRST+1 ) >*/
            u12l = b[ifirst + (ifirst + 1) * b_dim1] / b[ifirst + 1 + (ifirst
                    + 1) * b_dim1];

/*<    >*/
            v[0] = (ad11 - ad11l) * (ad22 - ad11l) - ad12 * ad21 + ad21 * u12
                    * ad11l + (ad12l - ad11l * u12l) * ad21l;
/*<    >*/
            v[1] = (ad22l - ad11l - ad21l * u12l - (ad11 - ad11l) - (ad22 -
                    ad11l) + ad21 * u12) * ad21l;
/*<             V( 3 ) = AD32L*AD21L >*/
            v[2] = ad32l * ad21l;

/*<             ISTART = IFIRST >*/
            istart = ifirst;

/*<             CALL DLARFG( 3, V( 1 ), V( 2 ), 1, TAU ) >*/
            dlarfg_(&c__3, v, &v[1], &c__1, &tau);
/*<             V( 1 ) = ONE >*/
            v[0] = 1.;

/*           Sweep */

/*<             DO 290 J = ISTART, ILAST - 2 >*/
            i__2 = ilast - 2;
            for (j = istart; j <= i__2; ++j) {

/*              All but last elements: use 3x3 Householder transforms. */

/*              Zero (j-1)st column of A */

/*<                IF( J.GT.ISTART ) THEN >*/
                if (j > istart) {
/*<                   V( 1 ) = A( J, J-1 ) >*/
                    v[0] = a[j + (j - 1) * a_dim1];
/*<                   V( 2 ) = A( J+1, J-1 ) >*/
                    v[1] = a[j + 1 + (j - 1) * a_dim1];
/*<                   V( 3 ) = A( J+2, J-1 ) >*/
                    v[2] = a[j + 2 + (j - 1) * a_dim1];

/*<                   CALL DLARFG( 3, A( J, J-1 ), V( 2 ), 1, TAU ) >*/
                    dlarfg_(&c__3, &a[j + (j - 1) * a_dim1], &v[1], &c__1, &
                            tau);
/*<                   V( 1 ) = ONE >*/
                    v[0] = 1.;
/*<                   A( J+1, J-1 ) = ZERO >*/
                    a[j + 1 + (j - 1) * a_dim1] = 0.;
/*<                   A( J+2, J-1 ) = ZERO >*/
                    a[j + 2 + (j - 1) * a_dim1] = 0.;
/*<                END IF >*/
                }

/*<                DO 230 JC = J, ILASTM >*/
                i__3 = ilastm;
                for (jc = j; jc <= i__3; ++jc) {
/*<    >*/
                    temp = tau * (a[j + jc * a_dim1] + v[1] * a[j + 1 + jc *
                            a_dim1] + v[2] * a[j + 2 + jc * a_dim1]);
/*<                   A( J, JC ) = A( J, JC ) - TEMP >*/
                    a[j + jc * a_dim1] -= temp;
/*<                   A( J+1, JC ) = A( J+1, JC ) - TEMP*V( 2 ) >*/
                    a[j + 1 + jc * a_dim1] -= temp * v[1];
/*<                   A( J+2, JC ) = A( J+2, JC ) - TEMP*V( 3 ) >*/
                    a[j + 2 + jc * a_dim1] -= temp * v[2];
/*<    >*/
                    temp2 = tau * (b[j + jc * b_dim1] + v[1] * b[j + 1 + jc *
                            b_dim1] + v[2] * b[j + 2 + jc * b_dim1]);
/*<                   B( J, JC ) = B( J, JC ) - TEMP2 >*/
                    b[j + jc * b_dim1] -= temp2;
/*<                   B( J+1, JC ) = B( J+1, JC ) - TEMP2*V( 2 ) >*/
                    b[j + 1 + jc * b_dim1] -= temp2 * v[1];
/*<                   B( J+2, JC ) = B( J+2, JC ) - TEMP2*V( 3 ) >*/
                    b[j + 2 + jc * b_dim1] -= temp2 * v[2];
/*<   230          CONTINUE >*/
/* L230: */
                }
/*<                IF( ILQ ) THEN >*/
                if (ilq) {
/*<                   DO 240 JR = 1, N >*/
                    i__3 = *n;
                    for (jr = 1; jr <= i__3; ++jr) {
/*<    >*/
                        temp = tau * (q[jr + j * q_dim1] + v[1] * q[jr + (j +
                                1) * q_dim1] + v[2] * q[jr + (j + 2) * q_dim1]
                                );
/*<                      Q( JR, J ) = Q( JR, J ) - TEMP >*/
                        q[jr + j * q_dim1] -= temp;
/*<                      Q( JR, J+1 ) = Q( JR, J+1 ) - TEMP*V( 2 ) >*/
                        q[jr + (j + 1) * q_dim1] -= temp * v[1];
/*<                      Q( JR, J+2 ) = Q( JR, J+2 ) - TEMP*V( 3 ) >*/
                        q[jr + (j + 2) * q_dim1] -= temp * v[2];
/*<   240             CONTINUE >*/
/* L240: */
                    }
/*<                END IF >*/
                }

/*              Zero j-th column of B (see DLAGBC for details) */

/*              Swap rows to pivot */

/*<                ILPIVT = .FALSE. >*/
                ilpivt = FALSE_;
/*<                TEMP = MAX( ABS( B( J+1, J+1 ) ), ABS( B( J+1, J+2 ) ) ) >*/
/* Computing MAX */
                d__3 = (d__1 = b[j + 1 + (j + 1) * b_dim1], abs(d__1)), d__4 =
                         (d__2 = b[j + 1 + (j + 2) * b_dim1], abs(d__2));
                temp = max(d__3,d__4);
/*<                TEMP2 = MAX( ABS( B( J+2, J+1 ) ), ABS( B( J+2, J+2 ) ) ) >*/
/* Computing MAX */
                d__3 = (d__1 = b[j + 2 + (j + 1) * b_dim1], abs(d__1)), d__4 =
                         (d__2 = b[j + 2 + (j + 2) * b_dim1], abs(d__2));
                temp2 = max(d__3,d__4);
/*<                IF( MAX( TEMP, TEMP2 ).LT.SAFMIN ) THEN >*/
                if (max(temp,temp2) < safmin) {
/*<                   SCALE = ZERO >*/
                    scale = 0.;
/*<                   U1 = ONE >*/
                    u1 = 1.;
/*<                   U2 = ZERO >*/
                    u2 = 0.;
/*<                   GO TO 250 >*/
                    goto L250;
/*<                ELSE IF( TEMP.GE.TEMP2 ) THEN >*/
                } else if (temp >= temp2) {
/*<                   W11 = B( J+1, J+1 ) >*/
                    w11 = b[j + 1 + (j + 1) * b_dim1];
/*<                   W21 = B( J+2, J+1 ) >*/
                    w21 = b[j + 2 + (j + 1) * b_dim1];
/*<                   W12 = B( J+1, J+2 ) >*/
                    w12 = b[j + 1 + (j + 2) * b_dim1];
/*<                   W22 = B( J+2, J+2 ) >*/
                    w22 = b[j + 2 + (j + 2) * b_dim1];
/*<                   U1 = B( J+1, J ) >*/
                    u1 = b[j + 1 + j * b_dim1];
/*<                   U2 = B( J+2, J ) >*/
                    u2 = b[j + 2 + j * b_dim1];
/*<                ELSE >*/
                } else {
/*<                   W21 = B( J+1, J+1 ) >*/
                    w21 = b[j + 1 + (j + 1) * b_dim1];
/*<                   W11 = B( J+2, J+1 ) >*/
                    w11 = b[j + 2 + (j + 1) * b_dim1];
/*<                   W22 = B( J+1, J+2 ) >*/
                    w22 = b[j + 1 + (j + 2) * b_dim1];
/*<                   W12 = B( J+2, J+2 ) >*/
                    w12 = b[j + 2 + (j + 2) * b_dim1];
/*<                   U2 = B( J+1, J ) >*/
                    u2 = b[j + 1 + j * b_dim1];
/*<                   U1 = B( J+2, J ) >*/
                    u1 = b[j + 2 + j * b_dim1];
/*<                END IF >*/
                }

/*              Swap columns if nec. */

/*<                IF( ABS( W12 ).GT.ABS( W11 ) ) THEN >*/
                if (abs(w12) > abs(w11)) {
/*<                   ILPIVT = .TRUE. >*/
                    ilpivt = TRUE_;
/*<                   TEMP = W12 >*/
                    temp = w12;
/*<                   TEMP2 = W22 >*/
                    temp2 = w22;
/*<                   W12 = W11 >*/
                    w12 = w11;
/*<                   W22 = W21 >*/
                    w22 = w21;
/*<                   W11 = TEMP >*/
                    w11 = temp;
/*<                   W21 = TEMP2 >*/
                    w21 = temp2;
/*<                END IF >*/
                }

/*              LU-factor */

/*<                TEMP = W21 / W11 >*/
                temp = w21 / w11;
/*<                U2 = U2 - TEMP*U1 >*/
                u2 -= temp * u1;
/*<                W22 = W22 - TEMP*W12 >*/
                w22 -= temp * w12;
/*<                W21 = ZERO >*/
//                w21 = 0.;

/*              Compute SCALE */

/*<                SCALE = ONE >*/
                scale = 1.;
/*<                IF( ABS( W22 ).LT.SAFMIN ) THEN >*/
                if (abs(w22) < safmin) {
/*<                   SCALE = ZERO >*/
                    scale = 0.;
/*<                   U2 = ONE >*/
                    u2 = 1.;
/*<                   U1 = -W12 / W11 >*/
                    u1 = -w12 / w11;
/*<                   GO TO 250 >*/
                    goto L250;
/*<                END IF >*/
                }
/*<    >*/
                if (abs(w22) < abs(u2)) {
                    scale = (d__1 = w22 / u2, abs(d__1));
                }
/*<    >*/
                if (abs(w11) < abs(u1)) {
/* Computing MIN */
                    d__2 = scale, d__3 = (d__1 = w11 / u1, abs(d__1));
                    scale = min(d__2,d__3);
                }

/*              Solve */

/*<                U2 = ( SCALE*U2 ) / W22 >*/
                u2 = scale * u2 / w22;
/*<                U1 = ( SCALE*U1-W12*U2 ) / W11 >*/
                u1 = (scale * u1 - w12 * u2) / w11;

/*<   250          CONTINUE >*/
L250:
/*<                IF( ILPIVT ) THEN >*/
                if (ilpivt) {
/*<                   TEMP = U2 >*/
                    temp = u2;
/*<                   U2 = U1 >*/
                    u2 = u1;
/*<                   U1 = TEMP >*/
                    u1 = temp;
/*<                END IF >*/
                }

/*              Compute Householder Vector */

/*<                T = SQRT( SCALE**2+U1**2+U2**2 ) >*/
/* Computing 2nd power */
                d__1 = scale;
/* Computing 2nd power */
                d__2 = u1;
/* Computing 2nd power */
                d__3 = u2;
                t = sqrt(d__1 * d__1 + d__2 * d__2 + d__3 * d__3);
/*<                TAU = ONE + SCALE / T >*/
                tau = scale / t + 1.;
/*<                VS = -ONE / ( SCALE+T ) >*/
                vs = -1. / (scale + t);
/*<                V( 1 ) = ONE >*/
                v[0] = 1.;
/*<                V( 2 ) = VS*U1 >*/
                v[1] = vs * u1;
/*<                V( 3 ) = VS*U2 >*/
                v[2] = vs * u2;

/*              Apply transformations from the right. */

/*<                DO 260 JR = IFRSTM, MIN( J+3, ILAST ) >*/
/* Computing MIN */
                i__4 = j + 3;
                i__3 = min(i__4,ilast);
                for (jr = ifrstm; jr <= i__3; ++jr) {
/*<    >*/
                    temp = tau * (a[jr + j * a_dim1] + v[1] * a[jr + (j + 1) *
                             a_dim1] + v[2] * a[jr + (j + 2) * a_dim1]);
/*<                   A( JR, J ) = A( JR, J ) - TEMP >*/
                    a[jr + j * a_dim1] -= temp;
/*<                   A( JR, J+1 ) = A( JR, J+1 ) - TEMP*V( 2 ) >*/
                    a[jr + (j + 1) * a_dim1] -= temp * v[1];
/*<                   A( JR, J+2 ) = A( JR, J+2 ) - TEMP*V( 3 ) >*/
                    a[jr + (j + 2) * a_dim1] -= temp * v[2];
/*<   260          CONTINUE >*/
/* L260: */
                }
/*<                DO 270 JR = IFRSTM, J + 2 >*/
                i__3 = j + 2;
                for (jr = ifrstm; jr <= i__3; ++jr) {
/*<    >*/
                    temp = tau * (b[jr + j * b_dim1] + v[1] * b[jr + (j + 1) *
                             b_dim1] + v[2] * b[jr + (j + 2) * b_dim1]);
/*<                   B( JR, J ) = B( JR, J ) - TEMP >*/
                    b[jr + j * b_dim1] -= temp;
/*<                   B( JR, J+1 ) = B( JR, J+1 ) - TEMP*V( 2 ) >*/
                    b[jr + (j + 1) * b_dim1] -= temp * v[1];
/*<                   B( JR, J+2 ) = B( JR, J+2 ) - TEMP*V( 3 ) >*/
                    b[jr + (j + 2) * b_dim1] -= temp * v[2];
/*<   270          CONTINUE >*/
/* L270: */
                }
/*<                IF( ILZ ) THEN >*/
                if (ilz) {
/*<                   DO 280 JR = 1, N >*/
                    i__3 = *n;
                    for (jr = 1; jr <= i__3; ++jr) {
/*<    >*/
                        temp = tau * (z__[jr + j * z_dim1] + v[1] * z__[jr + (
                                j + 1) * z_dim1] + v[2] * z__[jr + (j + 2) *
                                z_dim1]);
/*<                      Z( JR, J ) = Z( JR, J ) - TEMP >*/
                        z__[jr + j * z_dim1] -= temp;
/*<                      Z( JR, J+1 ) = Z( JR, J+1 ) - TEMP*V( 2 ) >*/
                        z__[jr + (j + 1) * z_dim1] -= temp * v[1];
/*<                      Z( JR, J+2 ) = Z( JR, J+2 ) - TEMP*V( 3 ) >*/
                        z__[jr + (j + 2) * z_dim1] -= temp * v[2];
/*<   280             CONTINUE >*/
/* L280: */
                    }
/*<                END IF >*/
                }
/*<                B( J+1, J ) = ZERO >*/
                b[j + 1 + j * b_dim1] = 0.;
/*<                B( J+2, J ) = ZERO >*/
                b[j + 2 + j * b_dim1] = 0.;
/*<   290       CONTINUE >*/
/* L290: */
            }

/*           Last elements: Use Givens rotations */

/*           Rotations from the left */

/*<             J = ILAST - 1 >*/
            j = ilast - 1;
/*<             TEMP = A( J, J-1 ) >*/
            temp = a[j + (j - 1) * a_dim1];
/*<             CALL DLARTG( TEMP, A( J+1, J-1 ), C, S, A( J, J-1 ) ) >*/
            dlartg_(&temp, &a[j + 1 + (j - 1) * a_dim1], &c__, &s, &a[j + (j
                    - 1) * a_dim1]);
/*<             A( J+1, J-1 ) = ZERO >*/
            a[j + 1 + (j - 1) * a_dim1] = 0.;

/*<             DO 300 JC = J, ILASTM >*/
            i__2 = ilastm;
            for (jc = j; jc <= i__2; ++jc) {
/*<                TEMP = C*A( J, JC ) + S*A( J+1, JC ) >*/
                temp = c__ * a[j + jc * a_dim1] + s * a[j + 1 + jc * a_dim1];
/*<                A( J+1, JC ) = -S*A( J, JC ) + C*A( J+1, JC ) >*/
                a[j + 1 + jc * a_dim1] = -s * a[j + jc * a_dim1] + c__ * a[j
                        + 1 + jc * a_dim1];
/*<                A( J, JC ) = TEMP >*/
                a[j + jc * a_dim1] = temp;
/*<                TEMP2 = C*B( J, JC ) + S*B( J+1, JC ) >*/
                temp2 = c__ * b[j + jc * b_dim1] + s * b[j + 1 + jc * b_dim1];
/*<                B( J+1, JC ) = -S*B( J, JC ) + C*B( J+1, JC ) >*/
                b[j + 1 + jc * b_dim1] = -s * b[j + jc * b_dim1] + c__ * b[j
                        + 1 + jc * b_dim1];
/*<                B( J, JC ) = TEMP2 >*/
                b[j + jc * b_dim1] = temp2;
/*<   300       CONTINUE >*/
/* L300: */
            }
/*<             IF( ILQ ) THEN >*/
            if (ilq) {
/*<                DO 310 JR = 1, N >*/
                i__2 = *n;
                for (jr = 1; jr <= i__2; ++jr) {
/*<                   TEMP = C*Q( JR, J ) + S*Q( JR, J+1 ) >*/
                    temp = c__ * q[jr + j * q_dim1] + s * q[jr + (j + 1) *
                            q_dim1];
/*<                   Q( JR, J+1 ) = -S*Q( JR, J ) + C*Q( JR, J+1 ) >*/
                    q[jr + (j + 1) * q_dim1] = -s * q[jr + j * q_dim1] + c__ *
                             q[jr + (j + 1) * q_dim1];
/*<                   Q( JR, J ) = TEMP >*/
                    q[jr + j * q_dim1] = temp;
/*<   310          CONTINUE >*/
/* L310: */
                }
/*<             END IF >*/
            }

/*           Rotations from the right. */

/*<             TEMP = B( J+1, J+1 ) >*/
            temp = b[j + 1 + (j + 1) * b_dim1];
/*<             CALL DLARTG( TEMP, B( J+1, J ), C, S, B( J+1, J+1 ) ) >*/
            dlartg_(&temp, &b[j + 1 + j * b_dim1], &c__, &s, &b[j + 1 + (j +
                    1) * b_dim1]);
/*<             B( J+1, J ) = ZERO >*/
            b[j + 1 + j * b_dim1] = 0.;

/*<             DO 320 JR = IFRSTM, ILAST >*/
            i__2 = ilast;
            for (jr = ifrstm; jr <= i__2; ++jr) {
/*<                TEMP = C*A( JR, J+1 ) + S*A( JR, J ) >*/
                temp = c__ * a[jr + (j + 1) * a_dim1] + s * a[jr + j * a_dim1]
                        ;
/*<                A( JR, J ) = -S*A( JR, J+1 ) + C*A( JR, J ) >*/
                a[jr + j * a_dim1] = -s * a[jr + (j + 1) * a_dim1] + c__ * a[
                        jr + j * a_dim1];
/*<                A( JR, J+1 ) = TEMP >*/
                a[jr + (j + 1) * a_dim1] = temp;
/*<   320       CONTINUE >*/
/* L320: */
            }
/*<             DO 330 JR = IFRSTM, ILAST - 1 >*/
            i__2 = ilast - 1;
            for (jr = ifrstm; jr <= i__2; ++jr) {
/*<                TEMP = C*B( JR, J+1 ) + S*B( JR, J ) >*/
                temp = c__ * b[jr + (j + 1) * b_dim1] + s * b[jr + j * b_dim1]
                        ;
/*<                B( JR, J ) = -S*B( JR, J+1 ) + C*B( JR, J ) >*/
                b[jr + j * b_dim1] = -s * b[jr + (j + 1) * b_dim1] + c__ * b[
                        jr + j * b_dim1];
/*<                B( JR, J+1 ) = TEMP >*/
                b[jr + (j + 1) * b_dim1] = temp;
/*<   330       CONTINUE >*/
/* L330: */
            }
/*<             IF( ILZ ) THEN >*/
            if (ilz) {
/*<                DO 340 JR = 1, N >*/
                i__2 = *n;
                for (jr = 1; jr <= i__2; ++jr) {
/*<                   TEMP = C*Z( JR, J+1 ) + S*Z( JR, J ) >*/
                    temp = c__ * z__[jr + (j + 1) * z_dim1] + s * z__[jr + j *
                             z_dim1];
/*<                   Z( JR, J ) = -S*Z( JR, J+1 ) + C*Z( JR, J ) >*/
                    z__[jr + j * z_dim1] = -s * z__[jr + (j + 1) * z_dim1] +
                            c__ * z__[jr + j * z_dim1];
/*<                   Z( JR, J+1 ) = TEMP >*/
                    z__[jr + (j + 1) * z_dim1] = temp;
/*<   340          CONTINUE >*/
/* L340: */
                }
/*<             END IF >*/
            }

/*           End of Double-Shift code */

/*<          END IF >*/
        }

/*<          GO TO 350 >*/
        goto L350;

/*        End of iteration loop */

/*<   350    CONTINUE >*/
L350:
/*<   360 CONTINUE >*/
/* L360: */
        ;
    }

/*     Drop-through = non-convergence */

/*<   370 CONTINUE >*/
/* L370: */
/*<       INFO = ILAST >*/
    *info = ilast;
/*<       GO TO 420 >*/
    goto L420;

/*     Successful completion of all QZ steps */

/*<   380 CONTINUE >*/
L380:

/*     Set Eigenvalues 1:ILO-1 */

/*<       DO 410 J = 1, ILO - 1 >*/
    i__1 = *ilo - 1;
    for (j = 1; j <= i__1; ++j) {
/*<          IF( B( J, J ).LT.ZERO ) THEN >*/
        if (b[j + j * b_dim1] < 0.) {
/*<             IF( ILSCHR ) THEN >*/
            if (ilschr) {
/*<                DO 390 JR = 1, J >*/
                i__2 = j;
                for (jr = 1; jr <= i__2; ++jr) {
/*<                   A( JR, J ) = -A( JR, J ) >*/
                    a[jr + j * a_dim1] = -a[jr + j * a_dim1];
/*<                   B( JR, J ) = -B( JR, J ) >*/
                    b[jr + j * b_dim1] = -b[jr + j * b_dim1];
/*<   390          CONTINUE >*/
/* L390: */
                }
/*<             ELSE >*/
            } else {
/*<                A( J, J ) = -A( J, J ) >*/
                a[j + j * a_dim1] = -a[j + j * a_dim1];
/*<                B( J, J ) = -B( J, J ) >*/
                b[j + j * b_dim1] = -b[j + j * b_dim1];
/*<             END IF >*/
            }
/*<             IF( ILZ ) THEN >*/
            if (ilz) {
/*<                DO 400 JR = 1, N >*/
                i__2 = *n;
                for (jr = 1; jr <= i__2; ++jr) {
/*<                   Z( JR, J ) = -Z( JR, J ) >*/
                    z__[jr + j * z_dim1] = -z__[jr + j * z_dim1];
/*<   400          CONTINUE >*/
/* L400: */
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<          ALPHAR( J ) = A( J, J ) >*/
        alphar[j] = a[j + j * a_dim1];
/*<          ALPHAI( J ) = ZERO >*/
        alphai[j] = 0.;
/*<          BETA( J ) = B( J, J ) >*/
        beta[j] = b[j + j * b_dim1];
/*<   410 CONTINUE >*/
/* L410: */
    }

/*     Normal Termination */

/*<       INFO = 0 >*/
    *info = 0;

/*     Exit (other than argument error) -- return optimal workspace size */

/*<   420 CONTINUE >*/
L420:
/*<       WORK( 1 ) = DBLE( N ) >*/
    work[1] = (doublereal) (*n);
/*<       RETURN >*/
    return 0;

/*     End of DHGEQZ */

/*<       END >*/
} /* dhgeqz_ */

#ifdef __cplusplus
        }
#endif
