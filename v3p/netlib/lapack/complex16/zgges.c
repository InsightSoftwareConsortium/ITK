/* lapack/complex16/zgges.f -- translated by f2c (version 20090411).
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

static doublecomplex c_b1 = {0.,0.};
static doublecomplex c_b2 = {1.,0.};
static integer c__1 = 1;
static integer c__0 = 0;
static integer c_n1 = -1;

/*<    >*/
/* Subroutine */ int zgges_(char *jobvsl, char *jobvsr, char *sort,
        logical (*selctg)(doublecomplex*,doublecomplex*),
        integer *n, doublecomplex *a, integer *lda, doublecomplex *b,
        integer *ldb, integer *sdim, doublecomplex *alpha, doublecomplex *
        beta, doublecomplex *vsl, integer *ldvsl, doublecomplex *vsr, integer
        *ldvsr, doublecomplex *work, integer *lwork, doublereal *rwork,
        logical *bwork, integer *info, ftnlen jobvsl_len, ftnlen jobvsr_len,
        ftnlen sort_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, vsl_dim1, vsl_offset,
            vsr_dim1, vsr_offset, i__1, i__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__;
    doublereal dif[2];
    integer ihi, ilo;
    doublereal eps, anrm, bnrm;
    integer idum[1], ierr, itau, iwrk;
    doublereal pvsl, pvsr;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    integer ileft, icols;
    logical cursl, ilvsl, ilvsr;
    integer irwrk, irows;
    extern /* Subroutine */ int dlabad_(doublereal *, doublereal *);
    extern doublereal dlamch_(char *, ftnlen);
    extern /* Subroutine */ int zggbak_(char *, char *, integer *, integer *,
            integer *, doublereal *, doublereal *, integer *, doublecomplex *,
             integer *, integer *, ftnlen, ftnlen), zggbal_(char *, integer *,
             doublecomplex *, integer *, doublecomplex *, integer *, integer *
            , integer *, doublereal *, doublereal *, doublereal *, integer *,
            ftnlen);
    logical ilascl, ilbscl;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *,
            integer *, integer *, ftnlen, ftnlen);
    extern doublereal zlange_(char *, integer *, integer *, doublecomplex *,
            integer *, doublereal *, ftnlen);
    doublereal bignum;
    integer ijobvl, iright;
    extern /* Subroutine */ int zgghrd_(char *, char *, integer *, integer *,
            integer *, doublecomplex *, integer *, doublecomplex *, integer *,
             doublecomplex *, integer *, doublecomplex *, integer *, integer *
            , ftnlen, ftnlen), zlascl_(char *, integer *, integer *,
            doublereal *, doublereal *, integer *, integer *, doublecomplex *,
             integer *, integer *, ftnlen);
    integer ijobvr;
    extern /* Subroutine */ int zgeqrf_(integer *, integer *, doublecomplex *,
             integer *, doublecomplex *, doublecomplex *, integer *, integer *
            );
    doublereal anrmto;
    integer lwkmin;
    logical lastsl;
    doublereal bnrmto;
    extern /* Subroutine */ int zlacpy_(char *, integer *, integer *,
            doublecomplex *, integer *, doublecomplex *, integer *, ftnlen),
            zlaset_(char *, integer *, integer *, doublecomplex *,
            doublecomplex *, doublecomplex *, integer *, ftnlen), zhgeqz_(
            char *, char *, char *, integer *, integer *, integer *,
            doublecomplex *, integer *, doublecomplex *, integer *,
            doublecomplex *, doublecomplex *, doublecomplex *, integer *,
            doublecomplex *, integer *, doublecomplex *, integer *,
            doublereal *, integer *, ftnlen, ftnlen, ftnlen), ztgsen_(integer
            *, logical *, logical *, logical *, integer *, doublecomplex *,
            integer *, doublecomplex *, integer *, doublecomplex *,
            doublecomplex *, doublecomplex *, integer *, doublecomplex *,
            integer *, integer *, doublereal *, doublereal *, doublereal *,
            doublecomplex *, integer *, integer *, integer *, integer *);
    doublereal smlnum;
    logical wantst, lquery;
    integer lwkopt;
    extern /* Subroutine */ int zungqr_(integer *, integer *, integer *,
            doublecomplex *, integer *, doublecomplex *, doublecomplex *,
            integer *, integer *), zunmqr_(char *, char *, integer *, integer
            *, integer *, doublecomplex *, integer *, doublecomplex *,
            doublecomplex *, integer *, doublecomplex *, integer *, integer *,
             ftnlen, ftnlen);
    (void)jobvsl_len;
    (void)jobvsr_len;
    (void)sort_len;

/*  -- LAPACK driver routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          JOBVSL, JOBVSR, SORT >*/
/*<       INTEGER            INFO, LDA, LDB, LDVSL, LDVSR, LWORK, N, SDIM >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       LOGICAL            BWORK( * ) >*/
/*<       DOUBLE PRECISION   RWORK( * ) >*/
/*<    >*/
/*     .. */
/*     .. Function Arguments .. */
/*<       LOGICAL            SELCTG >*/
/*<       EXTERNAL           SELCTG >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZGGES computes for a pair of N-by-N complex nonsymmetric matrices */
/*  (A,B), the generalized eigenvalues, the generalized complex Schur */
/*  form (S, T), and optionally left and/or right Schur vectors (VSL */
/*  and VSR). This gives the generalized Schur factorization */

/*          (A,B) = ( (VSL)*S*(VSR)**H, (VSL)*T*(VSR)**H ) */

/*  where (VSR)**H is the conjugate-transpose of VSR. */

/*  Optionally, it also orders the eigenvalues so that a selected cluster */
/*  of eigenvalues appears in the leading diagonal blocks of the upper */
/*  triangular matrix S and the upper triangular matrix T. The leading */
/*  columns of VSL and VSR then form an unitary basis for the */
/*  corresponding left and right eigenspaces (deflating subspaces). */

/*  (If only the generalized eigenvalues are needed, use the driver */
/*  ZGGEV instead, which is faster.) */

/*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar w */
/*  or a ratio alpha/beta = w, such that  A - w*B is singular.  It is */
/*  usually represented as the pair (alpha,beta), as there is a */
/*  reasonable interpretation for beta=0, and even for both being zero. */

/*  A pair of matrices (S,T) is in generalized complex Schur form if S */
/*  and T are upper triangular and, in addition, the diagonal elements */
/*  of T are non-negative real numbers. */

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
/*          = 'S':  Eigenvalues are ordered (see SELCTG). */

/*  SELCTG  (external procedure) LOGICAL FUNCTION of two COMPLEX*16 arguments */
/*          SELCTG must be declared EXTERNAL in the calling subroutine. */
/*          If SORT = 'N', SELCTG is not referenced. */
/*          If SORT = 'S', SELCTG is used to select eigenvalues to sort */
/*          to the top left of the Schur form. */
/*          An eigenvalue ALPHA(j)/BETA(j) is selected if */
/*          SELCTG(ALPHA(j),BETA(j)) is true. */

/*          Note that a selected complex eigenvalue may no longer satisfy */
/*          SELCTG(ALPHA(j),BETA(j)) = .TRUE. after ordering, since */
/*          ordering may change the value of complex eigenvalues */
/*          (especially if the eigenvalue is ill-conditioned), in this */
/*          case INFO is set to N+2 (See INFO below). */

/*  N       (input) INTEGER */
/*          The order of the matrices A, B, VSL, and VSR.  N >= 0. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA, N) */
/*          On entry, the first of the pair of matrices. */
/*          On exit, A has been overwritten by its generalized Schur */
/*          form S. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of A.  LDA >= max(1,N). */

/*  B       (input/output) COMPLEX*16 array, dimension (LDB, N) */
/*          On entry, the second of the pair of matrices. */
/*          On exit, B has been overwritten by its generalized Schur */
/*          form T. */

/*  LDB     (input) INTEGER */
/*          The leading dimension of B.  LDB >= max(1,N). */

/*  SDIM    (output) INTEGER */
/*          If SORT = 'N', SDIM = 0. */
/*          If SORT = 'S', SDIM = number of eigenvalues (after sorting) */
/*          for which SELCTG is true. */

/*  ALPHA   (output) COMPLEX*16 array, dimension (N) */
/*  BETA    (output) COMPLEX*16 array, dimension (N) */
/*          On exit,  ALPHA(j)/BETA(j), j=1,...,N, will be the */
/*          generalized eigenvalues.  ALPHA(j), j=1,...,N  and  BETA(j), */
/*          j=1,...,N  are the diagonals of the complex Schur form (A,B) */
/*          output by ZGGES. The  BETA(j) will be non-negative real. */

/*          Note: the quotients ALPHA(j)/BETA(j) may easily over- or */
/*          underflow, and BETA(j) may even be zero.  Thus, the user */
/*          should avoid naively computing the ratio alpha/beta. */
/*          However, ALPHA will be always less than and usually */
/*          comparable with norm(A) in magnitude, and BETA always less */
/*          than and usually comparable with norm(B). */

/*  VSL     (output) COMPLEX*16 array, dimension (LDVSL,N) */
/*          If JOBVSL = 'V', VSL will contain the left Schur vectors. */
/*          Not referenced if JOBVSL = 'N'. */

/*  LDVSL   (input) INTEGER */
/*          The leading dimension of the matrix VSL. LDVSL >= 1, and */
/*          if JOBVSL = 'V', LDVSL >= N. */

/*  VSR     (output) COMPLEX*16 array, dimension (LDVSR,N) */
/*          If JOBVSR = 'V', VSR will contain the right Schur vectors. */
/*          Not referenced if JOBVSR = 'N'. */

/*  LDVSR   (input) INTEGER */
/*          The leading dimension of the matrix VSR. LDVSR >= 1, and */
/*          if JOBVSR = 'V', LDVSR >= N. */

/*  WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK)) */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK. */

/*  LWORK   (input) INTEGER */
/*          The dimension of the array WORK.  LWORK >= max(1,2*N). */
/*          For good performance, LWORK must generally be larger. */

/*          If LWORK = -1, then a workspace query is assumed; the routine */
/*          only calculates the optimal size of the WORK array, returns */
/*          this value as the first entry of the WORK array, and no error */
/*          message related to LWORK is issued by XERBLA. */

/*  RWORK   (workspace) DOUBLE PRECISION array, dimension (8*N) */

/*  BWORK   (workspace) LOGICAL array, dimension (N) */
/*          Not referenced if SORT = 'N'. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value. */
/*          =1,...,N: */
/*                The QZ iteration failed.  (A,B) are not in Schur */
/*                form, but ALPHA(j) and BETA(j) should be correct for */
/*                j=INFO+1,...,N. */
/*          > N:  =N+1: other than QZ iteration failed in ZHGEQZ */
/*                =N+2: after reordering, roundoff changed values of */
/*                      some complex eigenvalues so that leading */
/*                      eigenvalues in the Generalized Schur form no */
/*                      longer satisfy SELCTG=.TRUE.  This could also */
/*                      be caused due to scaling. */
/*                =N+3: reordering falied in ZTGSEN. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 ) >*/
/*<       COMPLEX*16         CZERO, CONE >*/
/*<    >*/
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
/*<       DOUBLE PRECISION   DLAMCH, ZLANGE >*/
/*<       EXTERNAL           LSAME, ILAENV, DLAMCH, ZLANGE >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MAX, SQRT >*/
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
    --alpha;
    --beta;
    vsl_dim1 = *ldvsl;
    vsl_offset = 1 + vsl_dim1;
    vsl -= vsl_offset;
    vsr_dim1 = *ldvsr;
    vsr_offset = 1 + vsr_dim1;
    vsr -= vsr_offset;
    --work;
    --rwork;
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
/*<          INFO = -14 >*/
        *info = -14;
/*<       ELSE IF( LDVSR.LT.1 .OR. ( ILVSR .AND. LDVSR.LT.N ) ) THEN >*/
    } else if (*ldvsr < 1 || (ilvsr && *ldvsr < *n)) {
/*<          INFO = -16 >*/
        *info = -16;
/*<       END IF >*/
    }

/*     Compute workspace */
/*      (Note: Comments in the code beginning "Workspace:" describe the */
/*       minimal amount of workspace needed at that point in the code, */
/*       as well as the preferred amount for good performance. */
/*       NB refers to the optimal block size for the immediately */
/*       following subroutine, as returned by ILAENV.) */

/*<       IF( INFO.EQ.0 ) THEN >*/
    if (*info == 0) {
/*<          LWKMIN = MAX( 1, 2*N ) >*/
/* Computing MAX */
        i__1 = 1, i__2 = *n << 1;
        lwkmin = max(i__1,i__2);
/*<          LWKOPT = MAX( 1, N + N*ILAENV( 1, 'ZGEQRF', ' ', N, 1, N, 0 ) ) >*/
/* Computing MAX */
        i__1 = 1, i__2 = *n + *n * ilaenv_(&c__1, "ZGEQRF", " ", n, &c__1, n,
                &c__0, (ftnlen)6, (ftnlen)1);
        lwkopt = max(i__1,i__2);
/*<    >*/
/* Computing MAX */
        i__1 = lwkopt, i__2 = *n + *n * ilaenv_(&c__1, "ZUNMQR", " ", n, &
                c__1, n, &c_n1, (ftnlen)6, (ftnlen)1);
        lwkopt = max(i__1,i__2);
/*<          IF( ILVSL ) THEN >*/
        if (ilvsl) {
/*<    >*/
/* Computing MAX */
            i__1 = lwkopt, i__2 = *n + *n * ilaenv_(&c__1, "ZUNGQR", " ", n, &
                    c__1, n, &c_n1, (ftnlen)6, (ftnlen)1);
            lwkopt = max(i__1,i__2);
/*<          END IF >*/
        }
/*<          WORK( 1 ) = LWKOPT >*/
        work[1].r = (doublereal) lwkopt, work[1].i = 0.;

/*<    >*/
        if (*lwork < lwkmin && ! lquery) {
            *info = -18;
        }
/*<       END IF >*/
    }

/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZGGES ', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZGGES ", &i__1, (ftnlen)6);
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
/*<       SMLNUM = DLAMCH( 'S' ) >*/
    smlnum = dlamch_("S", (ftnlen)1);
/*<       BIGNUM = ONE / SMLNUM >*/
    bignum = 1. / smlnum;
/*<       CALL DLABAD( SMLNUM, BIGNUM ) >*/
    dlabad_(&smlnum, &bignum);
/*<       SMLNUM = SQRT( SMLNUM ) / EPS >*/
    smlnum = sqrt(smlnum) / eps;
/*<       BIGNUM = ONE / SMLNUM >*/
    bignum = 1. / smlnum;

/*     Scale A if max element outside range [SMLNUM,BIGNUM] */

/*<       ANRM = ZLANGE( 'M', N, N, A, LDA, RWORK ) >*/
    anrm = zlange_("M", n, n, &a[a_offset], lda, &rwork[1], (ftnlen)1);
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
        zlascl_("G", &c__0, &c__0, &anrm, &anrmto, n, n, &a[a_offset], lda, &
                ierr, (ftnlen)1);
    }

/*     Scale B if max element outside range [SMLNUM,BIGNUM] */

/*<       BNRM = ZLANGE( 'M', N, N, B, LDB, RWORK ) >*/
    bnrm = zlange_("M", n, n, &b[b_offset], ldb, &rwork[1], (ftnlen)1);
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
        zlascl_("G", &c__0, &c__0, &bnrm, &bnrmto, n, n, &b[b_offset], ldb, &
                ierr, (ftnlen)1);
    }

/*     Permute the matrix to make it more nearly triangular */
/*     (Real Workspace: need 6*N) */

/*<       ILEFT = 1 >*/
    ileft = 1;
/*<       IRIGHT = N + 1 >*/
    iright = *n + 1;
/*<       IRWRK = IRIGHT + N >*/
    irwrk = iright + *n;
/*<    >*/
    zggbal_("P", n, &a[a_offset], lda, &b[b_offset], ldb, &ilo, &ihi, &rwork[
            ileft], &rwork[iright], &rwork[irwrk], &ierr, (ftnlen)1);

/*     Reduce B to triangular form (QR decomposition of B) */
/*     (Complex Workspace: need N, prefer N*NB) */

/*<       IROWS = IHI + 1 - ILO >*/
    irows = ihi + 1 - ilo;
/*<       ICOLS = N + 1 - ILO >*/
    icols = *n + 1 - ilo;
/*<       ITAU = 1 >*/
    itau = 1;
/*<       IWRK = ITAU + IROWS >*/
    iwrk = itau + irows;
/*<    >*/
    i__1 = *lwork + 1 - iwrk;
    zgeqrf_(&irows, &icols, &b[ilo + ilo * b_dim1], ldb, &work[itau], &work[
            iwrk], &i__1, &ierr);

/*     Apply the orthogonal transformation to matrix A */
/*     (Complex Workspace: need N, prefer N*NB) */

/*<    >*/
    i__1 = *lwork + 1 - iwrk;
    zunmqr_("L", "C", &irows, &icols, &irows, &b[ilo + ilo * b_dim1], ldb, &
            work[itau], &a[ilo + ilo * a_dim1], lda, &work[iwrk], &i__1, &
            ierr, (ftnlen)1, (ftnlen)1);

/*     Initialize VSL */
/*     (Complex Workspace: need N, prefer N*NB) */

/*<       IF( ILVSL ) THEN >*/
    if (ilvsl) {
/*<          CALL ZLASET( 'Full', N, N, CZERO, CONE, VSL, LDVSL ) >*/
        zlaset_("Full", n, n, &c_b1, &c_b2, &vsl[vsl_offset], ldvsl, (ftnlen)
                4);
/*<          IF( IROWS.GT.1 ) THEN >*/
        if (irows > 1) {
/*<    >*/
            i__1 = irows - 1;
            i__2 = irows - 1;
            zlacpy_("L", &i__1, &i__2, &b[ilo + 1 + ilo * b_dim1], ldb, &vsl[
                    ilo + 1 + ilo * vsl_dim1], ldvsl, (ftnlen)1);
/*<          END IF >*/
        }
/*<    >*/
        i__1 = *lwork + 1 - iwrk;
        zungqr_(&irows, &irows, &irows, &vsl[ilo + ilo * vsl_dim1], ldvsl, &
                work[itau], &work[iwrk], &i__1, &ierr);
/*<       END IF >*/
    }

/*     Initialize VSR */

/*<    >*/
    if (ilvsr) {
        zlaset_("Full", n, n, &c_b1, &c_b2, &vsr[vsr_offset], ldvsr, (ftnlen)
                4);
    }

/*     Reduce to generalized Hessenberg form */
/*     (Workspace: none needed) */

/*<    >*/
    zgghrd_(jobvsl, jobvsr, n, &ilo, &ihi, &a[a_offset], lda, &b[b_offset],
            ldb, &vsl[vsl_offset], ldvsl, &vsr[vsr_offset], ldvsr, &ierr, (
            ftnlen)1, (ftnlen)1);

/*<       SDIM = 0 >*/
    *sdim = 0;

/*     Perform QZ algorithm, computing Schur vectors if desired */
/*     (Complex Workspace: need N) */
/*     (Real Workspace: need N) */

/*<       IWRK = ITAU >*/
    iwrk = itau;
/*<    >*/
    i__1 = *lwork + 1 - iwrk;
    zhgeqz_("S", jobvsl, jobvsr, n, &ilo, &ihi, &a[a_offset], lda, &b[
            b_offset], ldb, &alpha[1], &beta[1], &vsl[vsl_offset], ldvsl, &
            vsr[vsr_offset], ldvsr, &work[iwrk], &i__1, &rwork[irwrk], &ierr,
            (ftnlen)1, (ftnlen)1, (ftnlen)1);
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
/*<          GO TO 30 >*/
        goto L30;
/*<       END IF >*/
    }

/*     Sort eigenvalues ALPHA/BETA if desired */
/*     (Workspace: none needed) */

/*<       IF( WANTST ) THEN >*/
    if (wantst) {

/*        Undo scaling on eigenvalues before selecting */

/*<    >*/
        if (ilascl) {
            zlascl_("G", &c__0, &c__0, &anrm, &anrmto, n, &c__1, &alpha[1], n,
                     &ierr, (ftnlen)1);
        }
/*<    >*/
        if (ilbscl) {
            zlascl_("G", &c__0, &c__0, &bnrm, &bnrmto, n, &c__1, &beta[1], n,
                    &ierr, (ftnlen)1);
        }

/*        Select eigenvalues */

/*<          DO 10 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             BWORK( I ) = SELCTG( ALPHA( I ), BETA( I ) ) >*/
            bwork[i__] = (*selctg)(&alpha[i__], &beta[i__]);
/*<    10    CONTINUE >*/
/* L10: */
        }

/*<    >*/
        i__1 = *lwork - iwrk + 1;
        ztgsen_(&c__0, &ilvsl, &ilvsr, &bwork[1], n, &a[a_offset], lda, &b[
                b_offset], ldb, &alpha[1], &beta[1], &vsl[vsl_offset], ldvsl,
                &vsr[vsr_offset], ldvsr, sdim, &pvsl, &pvsr, dif, &work[iwrk],
                &i__1, idum, &c__1, &ierr);
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
        zggbak_("P", "L", n, &ilo, &ihi, &rwork[ileft], &rwork[iright], n, &
                vsl[vsl_offset], ldvsl, &ierr, (ftnlen)1, (ftnlen)1);
    }
/*<    >*/
    if (ilvsr) {
        zggbak_("P", "R", n, &ilo, &ihi, &rwork[ileft], &rwork[iright], n, &
                vsr[vsr_offset], ldvsr, &ierr, (ftnlen)1, (ftnlen)1);
    }

/*     Undo scaling */

/*<       IF( ILASCL ) THEN >*/
    if (ilascl) {
/*<          CALL ZLASCL( 'U', 0, 0, ANRMTO, ANRM, N, N, A, LDA, IERR ) >*/
        zlascl_("U", &c__0, &c__0, &anrmto, &anrm, n, n, &a[a_offset], lda, &
                ierr, (ftnlen)1);
/*<          CALL ZLASCL( 'G', 0, 0, ANRMTO, ANRM, N, 1, ALPHA, N, IERR ) >*/
        zlascl_("G", &c__0, &c__0, &anrmto, &anrm, n, &c__1, &alpha[1], n, &
                ierr, (ftnlen)1);
/*<       END IF >*/
    }

/*<       IF( ILBSCL ) THEN >*/
    if (ilbscl) {
/*<          CALL ZLASCL( 'U', 0, 0, BNRMTO, BNRM, N, N, B, LDB, IERR ) >*/
        zlascl_("U", &c__0, &c__0, &bnrmto, &bnrm, n, n, &b[b_offset], ldb, &
                ierr, (ftnlen)1);
/*<          CALL ZLASCL( 'G', 0, 0, BNRMTO, BNRM, N, 1, BETA, N, IERR ) >*/
        zlascl_("G", &c__0, &c__0, &bnrmto, &bnrm, n, &c__1, &beta[1], n, &
                ierr, (ftnlen)1);
/*<       END IF >*/
    }

/*<       IF( WANTST ) THEN >*/
    if (wantst) {

/*        Check if reordering is correct */

/*<          LASTSL = .TRUE. >*/
        lastsl = TRUE_;
/*<          SDIM = 0 >*/
        *sdim = 0;
/*<          DO 20 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             CURSL = SELCTG( ALPHA( I ), BETA( I ) ) >*/
            cursl = (*selctg)(&alpha[i__], &beta[i__]);
/*<    >*/
            if (cursl) {
                ++(*sdim);
            }
/*<    >*/
            if (cursl && ! lastsl) {
                *info = *n + 2;
            }
/*<             LASTSL = CURSL >*/
            lastsl = cursl;
/*<    20    CONTINUE >*/
/* L20: */
        }

/*<       END IF >*/
    }

/*<    30 CONTINUE >*/
L30:

/*<       WORK( 1 ) = LWKOPT >*/
    work[1].r = (doublereal) lwkopt, work[1].i = 0.;

/*<       RETURN >*/
    return 0;

/*     End of ZGGES */

/*<       END >*/
} /* zgges_ */

#ifdef __cplusplus
        }
#endif
