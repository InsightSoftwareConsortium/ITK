/* lapack/double/dgges.f -- translated by f2c (version 20050501).
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
static integer c__0 = 0;
static integer c_n1 = -1;
static doublereal c_b33 = 0.;
static doublereal c_b34 = 1.;

/*<    >*/
/* Subroutine */ int dgges_(char *jobvsl, char *jobvsr, char *sort,
        logical (*delctg)(doublereal*,doublereal*,doublereal*),
        integer *n, doublereal *a, integer *lda, doublereal *b,
        integer *ldb, integer *sdim, doublereal *alphar, doublereal *alphai,
        doublereal *beta, doublereal *vsl, integer *ldvsl, doublereal *vsr,
        integer *ldvsr, doublereal *work, integer *lwork, logical *bwork,
        integer *info, ftnlen jobvsl_len, ftnlen jobvsr_len, ftnlen sort_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, vsl_dim1, vsl_offset,
            vsr_dim1, vsr_offset, i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__, ip;
    doublereal dif[2];
    integer ihi, ilo;
    doublereal eps, anrm, bnrm;
    integer idum[1], ierr, itau, iwrk;
    doublereal pvsl, pvsr;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    integer ileft, icols;
    logical cursl, ilvsl, ilvsr;
    integer irows;
    extern /* Subroutine */ int dlabad_(doublereal *, doublereal *), dggbak_(
            char *, char *, integer *, integer *, integer *, doublereal *,
            doublereal *, integer *, doublereal *, integer *, integer *,
            ftnlen, ftnlen), dggbal_(char *, integer *, doublereal *, integer
            *, doublereal *, integer *, integer *, integer *, doublereal *,
            doublereal *, doublereal *, integer *, ftnlen);
    logical lst2sl;
    extern doublereal dlamch_(char *, ftnlen), dlange_(char *, integer *,
            integer *, doublereal *, integer *, doublereal *, ftnlen);
    extern /* Subroutine */ int dgghrd_(char *, char *, integer *, integer *,
            integer *, doublereal *, integer *, doublereal *, integer *,
            doublereal *, integer *, doublereal *, integer *, integer *,
            ftnlen, ftnlen), dlascl_(char *, integer *, integer *, doublereal
            *, doublereal *, integer *, integer *, doublereal *, integer *,
            integer *, ftnlen);
    logical ilascl, ilbscl;
    extern /* Subroutine */ int dgeqrf_(integer *, integer *, doublereal *,
            integer *, doublereal *, doublereal *, integer *, integer *),
            dlacpy_(char *, integer *, integer *, doublereal *, integer *,
            doublereal *, integer *, ftnlen);
    doublereal safmin;
    extern /* Subroutine */ int dlaset_(char *, integer *, integer *,
            doublereal *, doublereal *, doublereal *, integer *, ftnlen);
    doublereal safmax;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    doublereal bignum;
    extern /* Subroutine */ int dhgeqz_(char *, char *, char *, integer *,
            integer *, integer *, doublereal *, integer *, doublereal *,
            integer *, doublereal *, doublereal *, doublereal *, doublereal *,
             integer *, doublereal *, integer *, doublereal *, integer *,
            integer *, ftnlen, ftnlen, ftnlen);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *,
            integer *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int dtgsen_(integer *, logical *, logical *,
            logical *, integer *, doublereal *, integer *, doublereal *,
            integer *, doublereal *, doublereal *, doublereal *, doublereal *,
             integer *, doublereal *, integer *, integer *, doublereal *,
            doublereal *, doublereal *, doublereal *, integer *, integer *,
            integer *, integer *);
    integer ijobvl, iright, ijobvr;
    extern /* Subroutine */ int dorgqr_(integer *, integer *, integer *,
            doublereal *, integer *, doublereal *, doublereal *, integer *,
            integer *);
    doublereal anrmto, bnrmto;
    logical lastsl;
    extern /* Subroutine */ int dormqr_(char *, char *, integer *, integer *,
            integer *, doublereal *, integer *, doublereal *, doublereal *,
            integer *, doublereal *, integer *, integer *, ftnlen, ftnlen);
    integer minwrk, maxwrk=0;
    doublereal smlnum;
    logical wantst, lquery;
    (void)jobvsl_len;
    (void)jobvsr_len;
    (void)sort_len;

/*  -- LAPACK driver routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          JOBVSL, JOBVSR, SORT >*/
/*<       INTEGER            INFO, LDA, LDB, LDVSL, LDVSR, LWORK, N, SDIM >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       LOGICAL            BWORK( * ) >*/
/*<    >*/
/*     .. */
/*     .. Function Arguments .. */
/*<       LOGICAL            DELCTG >*/
/*<       EXTERNAL           DELCTG >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DGGES computes for a pair of N-by-N real nonsymmetric matrices (A,B), */
/*  the generalized eigenvalues, the generalized real Schur form (S,T), */
/*  optionally, the left and/or right matrices of Schur vectors (VSL and */
/*  VSR). This gives the generalized Schur factorization */

/*           (A,B) = ( (VSL)*S*(VSR)**T, (VSL)*T*(VSR)**T ) */

/*  Optionally, it also orders the eigenvalues so that a selected cluster */
/*  of eigenvalues appears in the leading diagonal blocks of the upper */
/*  quasi-triangular matrix S and the upper triangular matrix T.The */
/*  leading columns of VSL and VSR then form an orthonormal basis for the */
/*  corresponding left and right eigenspaces (deflating subspaces). */

/*  (If only the generalized eigenvalues are needed, use the driver */
/*  DGGEV instead, which is faster.) */

/*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar w */
/*  or a ratio alpha/beta = w, such that  A - w*B is singular.  It is */
/*  usually represented as the pair (alpha,beta), as there is a */
/*  reasonable interpretation for beta=0 or both being zero. */

/*  A pair of matrices (S,T) is in generalized real Schur form if T is */
/*  upper triangular with non-negative diagonal and S is block upper */
/*  triangular with 1-by-1 and 2-by-2 blocks.  1-by-1 blocks correspond */
/*  to real generalized eigenvalues, while 2-by-2 blocks of S will be */
/*  "standardized" by making the corresponding elements of T have the */
/*  form: */
/*          [  a  0  ] */
/*          [  0  b  ] */

/*  and the pair of corresponding 2-by-2 blocks in S and T will have a */
/*  complex conjugate pair of generalized eigenvalues. */


/*  Arguments */
/*  ========= */

/*  JOBVSL  (input) CHARACTER*1 */
/*          = 'N':  do not compute the left Schur vectors; */
/*          = 'V':  compute the left Schur vectors. */

/*  JOBVSR  (input) CHARACTER*1 */
/*          = 'N':  do not compute the right Schur vectors; */
/*          = 'V':  compute the right Schur vectors. */

/*  SORT    (input) CHARACTER*1 */
/*          Specifies whether or not to order the eigenvalues on the */
/*          diagonal of the generalized Schur form. */
/*          = 'N':  Eigenvalues are not ordered; */
/*          = 'S':  Eigenvalues are ordered (see DELZTG); */

/*  DELZTG  (input) LOGICAL FUNCTION of three DOUBLE PRECISION arguments */
/*          DELZTG must be declared EXTERNAL in the calling subroutine. */
/*          If SORT = 'N', DELZTG is not referenced. */
/*          If SORT = 'S', DELZTG is used to select eigenvalues to sort */
/*          to the top left of the Schur form. */
/*          An eigenvalue (ALPHAR(j)+ALPHAI(j))/BETA(j) is selected if */
/*          DELZTG(ALPHAR(j),ALPHAI(j),BETA(j)) is true; i.e. if either */
/*          one of a complex conjugate pair of eigenvalues is selected, */
/*          then both complex eigenvalues are selected. */

/*          Note that in the ill-conditioned case, a selected complex */
/*          eigenvalue may no longer satisfy DELZTG(ALPHAR(j),ALPHAI(j), */
/*          BETA(j)) = .TRUE. after ordering. INFO is to be set to N+2 */
/*          in this case. */

/*  N       (input) INTEGER */
/*          The order of the matrices A, B, VSL, and VSR.  N >= 0. */

/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N) */
/*          On entry, the first of the pair of matrices. */
/*          On exit, A has been overwritten by its generalized Schur */
/*          form S. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of A.  LDA >= max(1,N). */

/*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, N) */
/*          On entry, the second of the pair of matrices. */
/*          On exit, B has been overwritten by its generalized Schur */
/*          form T. */

/*  LDB     (input) INTEGER */
/*          The leading dimension of B.  LDB >= max(1,N). */

/*  SDIM    (output) INTEGER */
/*          If SORT = 'N', SDIM = 0. */
/*          If SORT = 'S', SDIM = number of eigenvalues (after sorting) */
/*          for which DELZTG is true.  (Complex conjugate pairs for which */
/*          DELZTG is true for either eigenvalue count as 2.) */

/*  ALPHAR  (output) DOUBLE PRECISION array, dimension (N) */
/*  ALPHAI  (output) DOUBLE PRECISION array, dimension (N) */
/*  BETA    (output) DOUBLE PRECISION array, dimension (N) */
/*          On exit, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j=1,...,N, will */
/*          be the generalized eigenvalues.  ALPHAR(j) + ALPHAI(j)*i, */
/*          and  BETA(j),j=1,...,N are the diagonals of the complex Schur */
/*          form (S,T) that would result if the 2-by-2 diagonal blocks of */
/*          the real Schur form of (A,B) were further reduced to */
/*          triangular form using 2-by-2 complex unitary transformations. */
/*          If ALPHAI(j) is zero, then the j-th eigenvalue is real; if */
/*          positive, then the j-th and (j+1)-st eigenvalues are a */
/*          complex conjugate pair, with ALPHAI(j+1) negative. */

/*          Note: the quotients ALPHAR(j)/BETA(j) and ALPHAI(j)/BETA(j) */
/*          may easily over- or underflow, and BETA(j) may even be zero. */
/*          Thus, the user should avoid naively computing the ratio. */
/*          However, ALPHAR and ALPHAI will be always less than and */
/*          usually comparable with norm(A) in magnitude, and BETA always */
/*          less than and usually comparable with norm(B). */

/*  VSL     (output) DOUBLE PRECISION array, dimension (LDVSL,N) */
/*          If JOBVSL = 'V', VSL will contain the left Schur vectors. */
/*          Not referenced if JOBVSL = 'N'. */

/*  LDVSL   (input) INTEGER */
/*          The leading dimension of the matrix VSL. LDVSL >=1, and */
/*          if JOBVSL = 'V', LDVSL >= N. */

/*  VSR     (output) DOUBLE PRECISION array, dimension (LDVSR,N) */
/*          If JOBVSR = 'V', VSR will contain the right Schur vectors. */
/*          Not referenced if JOBVSR = 'N'. */

/*  LDVSR   (input) INTEGER */
/*          The leading dimension of the matrix VSR. LDVSR >= 1, and */
/*          if JOBVSR = 'V', LDVSR >= N. */

/*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK) */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK. */

/*  LWORK   (input) INTEGER */
/*          The dimension of the array WORK.  LWORK >= 8*N+16. */

/*          If LWORK = -1, then a workspace query is assumed; the routine */
/*          only calculates the optimal size of the WORK array, returns */
/*          this value as the first entry of the WORK array, and no error */
/*          message related to LWORK is issued by XERBLA. */

/*  BWORK   (workspace) LOGICAL array, dimension (N) */
/*          Not referenced if SORT = 'N'. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value. */
/*          = 1,...,N: */
/*                The QZ iteration failed.  (A,B) are not in Schur */
/*                form, but ALPHAR(j), ALPHAI(j), and BETA(j) should */
/*                be correct for j=INFO+1,...,N. */
/*          > N:  =N+1: other than QZ iteration failed in DHGEQZ. */
/*                =N+2: after reordering, roundoff changed values of */
/*                      some complex eigenvalues so that leading */
/*                      eigenvalues in the Generalized Schur form no */
/*                      longer satisfy DELZTG=.TRUE.  This could also */
/*                      be caused due to scaling. */
/*                =N+3: reordering failed in DTGSEN. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<    >*/
/*<    >*/
/*<    >*/
/*     .. */
/*     .. Local Arrays .. */
/*<       INTEGER            IDUM( 1 ) >*/
/*<       DOUBLE PRECISION   DIF( 2 ) >*/
/*     .. */
/*     .. External Subroutines .. */
/*<    >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       INTEGER            ILAENV >*/
/*<       DOUBLE PRECISION   DLAMCH, DLANGE >*/
/*<       EXTERNAL           LSAME, ILAENV, DLAMCH, DLANGE >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Decode the input arguments */

/*<       IF( LSAME( JOBVSL, 'N' ) ) THEN >*/
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
    vsl_dim1 = *ldvsl;
    vsl_offset = 1 + vsl_dim1;
    vsl -= vsl_offset;
    vsr_dim1 = *ldvsr;
    vsr_offset = 1 + vsr_dim1;
    vsr -= vsr_offset;
    --work;
    --bwork;

    /* Function Body */
    if (lsame_(jobvsl, "N", (ftnlen)1, (ftnlen)1)) {
/*<          IJOBVL = 1 >*/
        ijobvl = 1;
/*<          ILVSL = .FALSE. >*/
        ilvsl = FALSE_;
/*<       ELSE IF( LSAME( JOBVSL, 'V' ) ) THEN >*/
    } else if (lsame_(jobvsl, "V", (ftnlen)1, (ftnlen)1)) {
/*<          IJOBVL = 2 >*/
        ijobvl = 2;
/*<          ILVSL = .TRUE. >*/
        ilvsl = TRUE_;
/*<       ELSE >*/
    } else {
/*<          IJOBVL = -1 >*/
        ijobvl = -1;
/*<          ILVSL = .FALSE. >*/
        ilvsl = FALSE_;
/*<       END IF >*/
    }

/*<       IF( LSAME( JOBVSR, 'N' ) ) THEN >*/
    if (lsame_(jobvsr, "N", (ftnlen)1, (ftnlen)1)) {
/*<          IJOBVR = 1 >*/
        ijobvr = 1;
/*<          ILVSR = .FALSE. >*/
        ilvsr = FALSE_;
/*<       ELSE IF( LSAME( JOBVSR, 'V' ) ) THEN >*/
    } else if (lsame_(jobvsr, "V", (ftnlen)1, (ftnlen)1)) {
/*<          IJOBVR = 2 >*/
        ijobvr = 2;
/*<          ILVSR = .TRUE. >*/
        ilvsr = TRUE_;
/*<       ELSE >*/
    } else {
/*<          IJOBVR = -1 >*/
        ijobvr = -1;
/*<          ILVSR = .FALSE. >*/
        ilvsr = FALSE_;
/*<       END IF >*/
    }

/*<       WANTST = LSAME( SORT, 'S' ) >*/
    wantst = lsame_(sort, "S", (ftnlen)1, (ftnlen)1);

/*     Test the input arguments */

/*<       INFO = 0 >*/
    *info = 0;
/*<       LQUERY = ( LWORK.EQ.-1 ) >*/
    lquery = *lwork == -1;
/*<       IF( IJOBVL.LE.0 ) THEN >*/
    if (ijobvl <= 0) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( IJOBVR.LE.0 ) THEN >*/
    } else if (ijobvr <= 0) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( ( .NOT.WANTST ) .AND. ( .NOT.LSAME( SORT, 'N' ) ) ) THEN >*/
    } else if (! wantst && ! lsame_(sort, "N", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -3 >*/
        *info = -3;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       ELSE IF( LDA.LT.MAX( 1, N ) ) THEN >*/
    } else if (*lda < max(1,*n)) {
/*<          INFO = -7 >*/
        *info = -7;
/*<       ELSE IF( LDB.LT.MAX( 1, N ) ) THEN >*/
    } else if (*ldb < max(1,*n)) {
/*<          INFO = -9 >*/
        *info = -9;
/*<       ELSE IF( LDVSL.LT.1 .OR. ( ILVSL .AND. LDVSL.LT.N ) ) THEN >*/
    } else if (*ldvsl < 1 || (ilvsl && *ldvsl < *n)) {
/*<          INFO = -15 >*/
        *info = -15;
/*<       ELSE IF( LDVSR.LT.1 .OR. ( ILVSR .AND. LDVSR.LT.N ) ) THEN >*/
    } else if (*ldvsr < 1 || (ilvsr && *ldvsr < *n)) {
/*<          INFO = -17 >*/
        *info = -17;
/*<       END IF >*/
    }

/*     Compute workspace */
/*      (Note: Comments in the code beginning "Workspace:" describe the */
/*       minimal amount of workspace needed at that point in the code, */
/*       as well as the preferred amount for good performance. */
/*       NB refers to the optimal block size for the immediately */
/*       following subroutine, as returned by ILAENV.) */

/*<       MINWRK = 1 >*/
    minwrk = 1;
/*<       IF( INFO.EQ.0 .AND. ( LWORK.GE.1 .OR. LQUERY ) ) THEN >*/
    if (*info == 0 && (*lwork >= 1 || lquery)) {
/*<          MINWRK = 7*( N+1 ) + 16 >*/
        minwrk = (*n + 1) * 7 + 16;
/*<    >*/
        maxwrk = (*n + 1) * 7 + *n * ilaenv_(&c__1, "DGEQRF", " ", n, &c__1,
                n, &c__0, (ftnlen)6, (ftnlen)1) + 16;
/*<          IF( ILVSL ) THEN >*/
        if (ilvsl) {
/*<    >*/
/* Computing MAX */
            i__1 = maxwrk, i__2 = (*n + 1) * 7 + *n * ilaenv_(&c__1, "DORGQR",
                     " ", n, &c__1, n, &c_n1, (ftnlen)6, (ftnlen)1);
            maxwrk = max(i__1,i__2);
/*<          END IF >*/
        }
/*<          WORK( 1 ) = MAXWRK >*/
        work[1] = (doublereal) maxwrk;
/*<       END IF >*/
    }

/*<    >*/
    if (*lwork < minwrk && ! lquery) {
        *info = -19;
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'DGGES ', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("DGGES ", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       ELSE IF( LQUERY ) THEN >*/
    } else if (lquery) {
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<       IF( N.EQ.0 ) THEN >*/
    if (*n == 0) {
/*<          SDIM = 0 >*/
        *sdim = 0;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Get machine constants */

/*<       EPS = DLAMCH( 'P' ) >*/
    eps = dlamch_("P", (ftnlen)1);
/*<       SAFMIN = DLAMCH( 'S' ) >*/
    safmin = dlamch_("S", (ftnlen)1);
/*<       SAFMAX = ONE / SAFMIN >*/
    safmax = 1. / safmin;
/*<       CALL DLABAD( SAFMIN, SAFMAX ) >*/
    dlabad_(&safmin, &safmax);
/*<       SMLNUM = SQRT( SAFMIN ) / EPS >*/
    smlnum = sqrt(safmin) / eps;
/*<       BIGNUM = ONE / SMLNUM >*/
    bignum = 1. / smlnum;

/*     Scale A if max element outside range [SMLNUM,BIGNUM] */

/*<       ANRM = DLANGE( 'M', N, N, A, LDA, WORK ) >*/
    anrm = dlange_("M", n, n, &a[a_offset], lda, &work[1], (ftnlen)1);
/*<       ILASCL = .FALSE. >*/
    ilascl = FALSE_;
/*<       IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN >*/
    if (anrm > 0. && anrm < smlnum) {
/*<          ANRMTO = SMLNUM >*/
        anrmto = smlnum;
/*<          ILASCL = .TRUE. >*/
        ilascl = TRUE_;
/*<       ELSE IF( ANRM.GT.BIGNUM ) THEN >*/
    } else if (anrm > bignum) {
/*<          ANRMTO = BIGNUM >*/
        anrmto = bignum;
/*<          ILASCL = .TRUE. >*/
        ilascl = TRUE_;
/*<       END IF >*/
    }
/*<    >*/
    if (ilascl) {
        dlascl_("G", &c__0, &c__0, &anrm, &anrmto, n, n, &a[a_offset], lda, &
                ierr, (ftnlen)1);
    }

/*     Scale B if max element outside range [SMLNUM,BIGNUM] */

/*<       BNRM = DLANGE( 'M', N, N, B, LDB, WORK ) >*/
    bnrm = dlange_("M", n, n, &b[b_offset], ldb, &work[1], (ftnlen)1);
/*<       ILBSCL = .FALSE. >*/
    ilbscl = FALSE_;
/*<       IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN >*/
    if (bnrm > 0. && bnrm < smlnum) {
/*<          BNRMTO = SMLNUM >*/
        bnrmto = smlnum;
/*<          ILBSCL = .TRUE. >*/
        ilbscl = TRUE_;
/*<       ELSE IF( BNRM.GT.BIGNUM ) THEN >*/
    } else if (bnrm > bignum) {
/*<          BNRMTO = BIGNUM >*/
        bnrmto = bignum;
/*<          ILBSCL = .TRUE. >*/
        ilbscl = TRUE_;
/*<       END IF >*/
    }
/*<    >*/
    if (ilbscl) {
        dlascl_("G", &c__0, &c__0, &bnrm, &bnrmto, n, n, &b[b_offset], ldb, &
                ierr, (ftnlen)1);
    }

/*     Permute the matrix to make it more nearly triangular */
/*     (Workspace: need 6*N + 2*N space for storing balancing factors) */

/*<       ILEFT = 1 >*/
    ileft = 1;
/*<       IRIGHT = N + 1 >*/
    iright = *n + 1;
/*<       IWRK = IRIGHT + N >*/
    iwrk = iright + *n;
/*<    >*/
    dggbal_("P", n, &a[a_offset], lda, &b[b_offset], ldb, &ilo, &ihi, &work[
            ileft], &work[iright], &work[iwrk], &ierr, (ftnlen)1);

/*     Reduce B to triangular form (QR decomposition of B) */
/*     (Workspace: need N, prefer N*NB) */

/*<       IROWS = IHI + 1 - ILO >*/
    irows = ihi + 1 - ilo;
/*<       ICOLS = N + 1 - ILO >*/
    icols = *n + 1 - ilo;
/*<       ITAU = IWRK >*/
    itau = iwrk;
/*<       IWRK = ITAU + IROWS >*/
    iwrk = itau + irows;
/*<    >*/
    i__1 = *lwork + 1 - iwrk;
    dgeqrf_(&irows, &icols, &b[ilo + ilo * b_dim1], ldb, &work[itau], &work[
            iwrk], &i__1, &ierr);

/*     Apply the orthogonal transformation to matrix A */
/*     (Workspace: need N, prefer N*NB) */

/*<    >*/
    i__1 = *lwork + 1 - iwrk;
    dormqr_("L", "T", &irows, &icols, &irows, &b[ilo + ilo * b_dim1], ldb, &
            work[itau], &a[ilo + ilo * a_dim1], lda, &work[iwrk], &i__1, &
            ierr, (ftnlen)1, (ftnlen)1);

/*     Initialize VSL */
/*     (Workspace: need N, prefer N*NB) */

/*<       IF( ILVSL ) THEN >*/
    if (ilvsl) {
/*<          CALL DLASET( 'Full', N, N, ZERO, ONE, VSL, LDVSL ) >*/
        dlaset_("Full", n, n, &c_b33, &c_b34, &vsl[vsl_offset], ldvsl, (
                ftnlen)4);
/*<    >*/
        i__1 = irows - 1;
        i__2 = irows - 1;
        dlacpy_("L", &i__1, &i__2, &b[ilo + 1 + ilo * b_dim1], ldb, &vsl[ilo
                + 1 + ilo * vsl_dim1], ldvsl, (ftnlen)1);
/*<    >*/
        i__1 = *lwork + 1 - iwrk;
        dorgqr_(&irows, &irows, &irows, &vsl[ilo + ilo * vsl_dim1], ldvsl, &
                work[itau], &work[iwrk], &i__1, &ierr);
/*<       END IF >*/
    }

/*     Initialize VSR */

/*<    >*/
    if (ilvsr) {
        dlaset_("Full", n, n, &c_b33, &c_b34, &vsr[vsr_offset], ldvsr, (
                ftnlen)4);
    }

/*     Reduce to generalized Hessenberg form */
/*     (Workspace: none needed) */

/*<    >*/
    dgghrd_(jobvsl, jobvsr, n, &ilo, &ihi, &a[a_offset], lda, &b[b_offset],
            ldb, &vsl[vsl_offset], ldvsl, &vsr[vsr_offset], ldvsr, &ierr, (
            ftnlen)1, (ftnlen)1);

/*     Perform QZ algorithm, computing Schur vectors if desired */
/*     (Workspace: need N) */

/*<       IWRK = ITAU >*/
    iwrk = itau;
/*<    >*/
    i__1 = *lwork + 1 - iwrk;
    dhgeqz_("S", jobvsl, jobvsr, n, &ilo, &ihi, &a[a_offset], lda, &b[
            b_offset], ldb, &alphar[1], &alphai[1], &beta[1], &vsl[vsl_offset]
            , ldvsl, &vsr[vsr_offset], ldvsr, &work[iwrk], &i__1, &ierr, (
            ftnlen)1, (ftnlen)1, (ftnlen)1);
/*<       IF( IERR.NE.0 ) THEN >*/
    if (ierr != 0) {
/*<          IF( IERR.GT.0 .AND. IERR.LE.N ) THEN >*/
        if (ierr > 0 && ierr <= *n) {
/*<             INFO = IERR >*/
            *info = ierr;
/*<          ELSE IF( IERR.GT.N .AND. IERR.LE.2*N ) THEN >*/
        } else if (ierr > *n && ierr <= *n << 1) {
/*<             INFO = IERR - N >*/
            *info = ierr - *n;
/*<          ELSE >*/
        } else {
/*<             INFO = N + 1 >*/
            *info = *n + 1;
/*<          END IF >*/
        }
/*<          GO TO 50 >*/
        goto L50;
/*<       END IF >*/
    }

/*     Sort eigenvalues ALPHA/BETA if desired */
/*     (Workspace: need 4*N+16 ) */

/*<       SDIM = 0 >*/
    *sdim = 0;
/*<       IF( WANTST ) THEN >*/
    if (wantst) {

/*        Undo scaling on eigenvalues before DELZTGing */

/*<          IF( ILASCL ) THEN >*/
        if (ilascl) {
/*<    >*/
            dlascl_("G", &c__0, &c__0, &anrmto, &anrm, n, &c__1, &alphar[1],
                    n, &ierr, (ftnlen)1);
/*<    >*/
            dlascl_("G", &c__0, &c__0, &anrmto, &anrm, n, &c__1, &alphai[1],
                    n, &ierr, (ftnlen)1);
/*<          END IF >*/
        }
/*<    >*/
        if (ilbscl) {
            dlascl_("G", &c__0, &c__0, &bnrmto, &bnrm, n, &c__1, &beta[1], n,
                    &ierr, (ftnlen)1);
        }

/*        Select eigenvalues */

/*<          DO 10 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             BWORK( I ) = DELCTG( ALPHAR( I ), ALPHAI( I ), BETA( I ) ) >*/
            bwork[i__] = (*delctg)(&alphar[i__], &alphai[i__], &beta[i__]);
/*<    10    CONTINUE >*/
/* L10: */
        }

/*<    >*/
        i__1 = *lwork - iwrk + 1;
        dtgsen_(&c__0, &ilvsl, &ilvsr, &bwork[1], n, &a[a_offset], lda, &b[
                b_offset], ldb, &alphar[1], &alphai[1], &beta[1], &vsl[
                vsl_offset], ldvsl, &vsr[vsr_offset], ldvsr, sdim, &pvsl, &
                pvsr, dif, &work[iwrk], &i__1, idum, &c__1, &ierr);
/*<    >*/
        if (ierr == 1) {
            *info = *n + 3;
        }

/*<       END IF >*/
    }

/*     Apply back-permutation to VSL and VSR */
/*     (Workspace: none needed) */

/*<    >*/
    if (ilvsl) {
        dggbak_("P", "L", n, &ilo, &ihi, &work[ileft], &work[iright], n, &vsl[
                vsl_offset], ldvsl, &ierr, (ftnlen)1, (ftnlen)1);
    }

/*<    >*/
    if (ilvsr) {
        dggbak_("P", "R", n, &ilo, &ihi, &work[ileft], &work[iright], n, &vsr[
                vsr_offset], ldvsr, &ierr, (ftnlen)1, (ftnlen)1);
    }

/*     Check if unscaling would cause over/underflow, if so, rescale */
/*     (ALPHAR(I),ALPHAI(I),BETA(I)) so BETA(I) is on the order of */
/*     B(I,I) and ALPHAR(I) and ALPHAI(I) are on the order of A(I,I) */

/*<       IF( ILASCL ) THEN >*/
    if (ilascl) {
/*<          DO 20 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             IF( ALPHAI( I ).NE.ZERO ) THEN >*/
            if (alphai[i__] != 0.) {
/*<    >*/
                if (alphar[i__] / safmax > anrmto / anrm || safmin / alphar[
                        i__] > anrm / anrmto) {
/*<                   WORK( 1 ) = ABS( A( I, I ) / ALPHAR( I ) ) >*/
                    work[1] = (d__1 = a[i__ + i__ * a_dim1] / alphar[i__],
                            abs(d__1));
/*<                   BETA( I ) = BETA( I )*WORK( 1 ) >*/
                    beta[i__] *= work[1];
/*<                   ALPHAR( I ) = ALPHAR( I )*WORK( 1 ) >*/
                    alphar[i__] *= work[1];
/*<                   ALPHAI( I ) = ALPHAI( I )*WORK( 1 ) >*/
                    alphai[i__] *= work[1];
/*<    >*/
                } else if (alphai[i__] / safmax > anrmto / anrm || safmin /
                        alphai[i__] > anrm / anrmto) {
/*<                   WORK( 1 ) = ABS( A( I, I+1 ) / ALPHAI( I ) ) >*/
                    work[1] = (d__1 = a[i__ + (i__ + 1) * a_dim1] / alphai[
                            i__], abs(d__1));
/*<                   BETA( I ) = BETA( I )*WORK( 1 ) >*/
                    beta[i__] *= work[1];
/*<                   ALPHAR( I ) = ALPHAR( I )*WORK( 1 ) >*/
                    alphar[i__] *= work[1];
/*<                   ALPHAI( I ) = ALPHAI( I )*WORK( 1 ) >*/
                    alphai[i__] *= work[1];
/*<                END IF >*/
                }
/*<             END IF >*/
            }
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<       END IF >*/
    }

/*<       IF( ILBSCL ) THEN >*/
    if (ilbscl) {
/*<          DO 30 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             IF( ALPHAI( I ).NE.ZERO ) THEN >*/
            if (alphai[i__] != 0.) {
/*<    >*/
                if (beta[i__] / safmax > bnrmto / bnrm || safmin / beta[i__]
                        > bnrm / bnrmto) {
/*<                   WORK( 1 ) = ABS( B( I, I ) / BETA( I ) ) >*/
                    work[1] = (d__1 = b[i__ + i__ * b_dim1] / beta[i__], abs(
                            d__1));
/*<                   BETA( I ) = BETA( I )*WORK( 1 ) >*/
                    beta[i__] *= work[1];
/*<                   ALPHAR( I ) = ALPHAR( I )*WORK( 1 ) >*/
                    alphar[i__] *= work[1];
/*<                   ALPHAI( I ) = ALPHAI( I )*WORK( 1 ) >*/
                    alphai[i__] *= work[1];
/*<                END IF >*/
                }
/*<             END IF >*/
            }
/*<    30    CONTINUE >*/
/* L30: */
        }
/*<       END IF >*/
    }

/*     Undo scaling */

/*<       IF( ILASCL ) THEN >*/
    if (ilascl) {
/*<          CALL DLASCL( 'H', 0, 0, ANRMTO, ANRM, N, N, A, LDA, IERR ) >*/
        dlascl_("H", &c__0, &c__0, &anrmto, &anrm, n, n, &a[a_offset], lda, &
                ierr, (ftnlen)1);
/*<          CALL DLASCL( 'G', 0, 0, ANRMTO, ANRM, N, 1, ALPHAR, N, IERR ) >*/
        dlascl_("G", &c__0, &c__0, &anrmto, &anrm, n, &c__1, &alphar[1], n, &
                ierr, (ftnlen)1);
/*<          CALL DLASCL( 'G', 0, 0, ANRMTO, ANRM, N, 1, ALPHAI, N, IERR ) >*/
        dlascl_("G", &c__0, &c__0, &anrmto, &anrm, n, &c__1, &alphai[1], n, &
                ierr, (ftnlen)1);
/*<       END IF >*/
    }

/*<       IF( ILBSCL ) THEN >*/
    if (ilbscl) {
/*<          CALL DLASCL( 'U', 0, 0, BNRMTO, BNRM, N, N, B, LDB, IERR ) >*/
        dlascl_("U", &c__0, &c__0, &bnrmto, &bnrm, n, n, &b[b_offset], ldb, &
                ierr, (ftnlen)1);
/*<          CALL DLASCL( 'G', 0, 0, BNRMTO, BNRM, N, 1, BETA, N, IERR ) >*/
        dlascl_("G", &c__0, &c__0, &bnrmto, &bnrm, n, &c__1, &beta[1], n, &
                ierr, (ftnlen)1);
/*<       END IF >*/
    }

/*<       IF( WANTST ) THEN >*/
    if (wantst) {

/*        Check if reordering is correct */

/*<          LASTSL = .TRUE. >*/
        lastsl = TRUE_;
/*<          LST2SL = .TRUE. >*/
        lst2sl = TRUE_;
/*<          SDIM = 0 >*/
        *sdim = 0;
/*<          IP = 0 >*/
        ip = 0;
/*<          DO 40 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             CURSL = DELCTG( ALPHAR( I ), ALPHAI( I ), BETA( I ) ) >*/
            cursl = (*delctg)(&alphar[i__], &alphai[i__], &beta[i__]);
/*<             IF( ALPHAI( I ).EQ.ZERO ) THEN >*/
            if (alphai[i__] == 0.) {
/*<    >*/
                if (cursl) {
                    ++(*sdim);
                }
/*<                IP = 0 >*/
                ip = 0;
/*<    >*/
                if (cursl && ! lastsl) {
                    *info = *n + 2;
                }
/*<             ELSE >*/
            } else {
/*<                IF( IP.EQ.1 ) THEN >*/
                if (ip == 1) {

/*                 Last eigenvalue of conjugate pair */

/*<                   CURSL = CURSL .OR. LASTSL >*/
                    cursl = cursl || lastsl;
/*<                   LASTSL = CURSL >*/
                    lastsl = cursl;
/*<    >*/
                    if (cursl) {
                        *sdim += 2;
                    }
/*<                   IP = -1 >*/
                    ip = -1;
/*<    >*/
                    if (cursl && ! lst2sl) {
                        *info = *n + 2;
                    }
/*<                ELSE >*/
                } else {

/*                 First eigenvalue of conjugate pair */

/*<                   IP = 1 >*/
                    ip = 1;
/*<                END IF >*/
                }
/*<             END IF >*/
            }
/*<             LST2SL = LASTSL >*/
            lst2sl = lastsl;
/*<             LASTSL = CURSL >*/
            lastsl = cursl;
/*<    40    CONTINUE >*/
/* L40: */
        }

/*<       END IF >*/
    }

/*<    50 CONTINUE >*/
L50:

/*<       WORK( 1 ) = MAXWRK >*/
    work[1] = (doublereal) maxwrk;

/*<       RETURN >*/
    return 0;

/*     End of DGGES */

/*<       END >*/
} /* dgges_ */

#ifdef __cplusplus
        }
#endif
