/* lapack/complex16/zgees.f -- translated by f2c (version 20090411).
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

/*<    >*/
/* Subroutine */ int zgees_(char *jobvs, char *sort,
        logical (*select)(doublecomplex*), integer *n,
        doublecomplex *a, integer *lda, integer *sdim, doublecomplex *w,
        doublecomplex *vs, integer *ldvs, doublecomplex *work, integer *lwork,
        doublereal *rwork, logical *bwork, integer *info, ftnlen jobvs_len,
        ftnlen sort_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, vs_dim1, vs_offset, i__1, i__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__;
    doublereal s;
    integer ihi, ilo;
    doublereal dum[1], eps, sep;
    integer ibal;
    doublereal anrm;
    integer ierr, itau, iwrk, icond, ieval;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int zcopy_(integer *, doublecomplex *, integer *,
            doublecomplex *, integer *), dlabad_(doublereal *, doublereal *);
    logical scalea;
    extern doublereal dlamch_(char *, ftnlen);
    doublereal cscale;
    extern /* Subroutine */ int zgebak_(char *, char *, integer *, integer *,
            integer *, doublereal *, integer *, doublecomplex *, integer *,
            integer *, ftnlen, ftnlen), zgebal_(char *, integer *,
            doublecomplex *, integer *, integer *, integer *, doublereal *,
            integer *, ftnlen), xerbla_(char *, integer *, ftnlen);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *,
            integer *, integer *, ftnlen, ftnlen);
    extern doublereal zlange_(char *, integer *, integer *, doublecomplex *,
            integer *, doublereal *, ftnlen);
    doublereal bignum;
    extern /* Subroutine */ int zgehrd_(integer *, integer *, integer *,
            doublecomplex *, integer *, doublecomplex *, doublecomplex *,
            integer *, integer *), zlascl_(char *, integer *, integer *,
            doublereal *, doublereal *, integer *, integer *, doublecomplex *,
             integer *, integer *, ftnlen), zlacpy_(char *, integer *,
            integer *, doublecomplex *, integer *, doublecomplex *, integer *,
             ftnlen);
    integer minwrk, maxwrk;
    doublereal smlnum;
    extern /* Subroutine */ int zhseqr_(char *, char *, integer *, integer *,
            integer *, doublecomplex *, integer *, doublecomplex *,
            doublecomplex *, integer *, doublecomplex *, integer *, integer *,
             ftnlen, ftnlen);
    integer hswork;
    extern /* Subroutine */ int zunghr_(integer *, integer *, integer *,
            doublecomplex *, integer *, doublecomplex *, doublecomplex *,
            integer *, integer *);
    logical wantst, lquery, wantvs;
    extern /* Subroutine */ int ztrsen_(char *, char *, logical *, integer *,
            doublecomplex *, integer *, doublecomplex *, integer *,
            doublecomplex *, integer *, doublereal *, doublereal *,
            doublecomplex *, integer *, integer *, ftnlen, ftnlen);
    (void)jobvs_len;
    (void)sort_len;

/*  -- LAPACK driver routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          JOBVS, SORT >*/
/*<       INTEGER            INFO, LDA, LDVS, LWORK, N, SDIM >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       LOGICAL            BWORK( * ) >*/
/*<       DOUBLE PRECISION   RWORK( * ) >*/
/*<       COMPLEX*16         A( LDA, * ), VS( LDVS, * ), W( * ), WORK( * ) >*/
/*     .. */
/*     .. Function Arguments .. */
/*<       LOGICAL            SELECT >*/
/*<       EXTERNAL           SELECT >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZGEES computes for an N-by-N complex nonsymmetric matrix A, the */
/*  eigenvalues, the Schur form T, and, optionally, the matrix of Schur */
/*  vectors Z.  This gives the Schur factorization A = Z*T*(Z**H). */

/*  Optionally, it also orders the eigenvalues on the diagonal of the */
/*  Schur form so that selected eigenvalues are at the top left. */
/*  The leading columns of Z then form an orthonormal basis for the */
/*  invariant subspace corresponding to the selected eigenvalues. */

/*  A complex matrix is in Schur form if it is upper triangular. */

/*  Arguments */
/*  ========= */

/*  JOBVS   (input) CHARACTER*1 */
/*          = 'N': Schur vectors are not computed; */
/*          = 'V': Schur vectors are computed. */

/*  SORT    (input) CHARACTER*1 */
/*          Specifies whether or not to order the eigenvalues on the */
/*          diagonal of the Schur form. */
/*          = 'N': Eigenvalues are not ordered: */
/*          = 'S': Eigenvalues are ordered (see SELECT). */

/*  SELECT  (external procedure) LOGICAL FUNCTION of one COMPLEX*16 argument */
/*          SELECT must be declared EXTERNAL in the calling subroutine. */
/*          If SORT = 'S', SELECT is used to select eigenvalues to order */
/*          to the top left of the Schur form. */
/*          IF SORT = 'N', SELECT is not referenced. */
/*          The eigenvalue W(j) is selected if SELECT(W(j)) is true. */

/*  N       (input) INTEGER */
/*          The order of the matrix A. N >= 0. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N) */
/*          On entry, the N-by-N matrix A. */
/*          On exit, A has been overwritten by its Schur form T. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,N). */

/*  SDIM    (output) INTEGER */
/*          If SORT = 'N', SDIM = 0. */
/*          If SORT = 'S', SDIM = number of eigenvalues for which */
/*                         SELECT is true. */

/*  W       (output) COMPLEX*16 array, dimension (N) */
/*          W contains the computed eigenvalues, in the same order that */
/*          they appear on the diagonal of the output Schur form T. */

/*  VS      (output) COMPLEX*16 array, dimension (LDVS,N) */
/*          If JOBVS = 'V', VS contains the unitary matrix Z of Schur */
/*          vectors. */
/*          If JOBVS = 'N', VS is not referenced. */

/*  LDVS    (input) INTEGER */
/*          The leading dimension of the array VS.  LDVS >= 1; if */
/*          JOBVS = 'V', LDVS >= N. */

/*  WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK)) */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK. */

/*  LWORK   (input) INTEGER */
/*          The dimension of the array WORK.  LWORK >= max(1,2*N). */
/*          For good performance, LWORK must generally be larger. */

/*          If LWORK = -1, then a workspace query is assumed; the routine */
/*          only calculates the optimal size of the WORK array, returns */
/*          this value as the first entry of the WORK array, and no error */
/*          message related to LWORK is issued by XERBLA. */

/*  RWORK   (workspace) DOUBLE PRECISION array, dimension (N) */

/*  BWORK   (workspace) LOGICAL array, dimension (N) */
/*          Not referenced if SORT = 'N'. */

/*  INFO    (output) INTEGER */
/*          = 0: successful exit */
/*          < 0: if INFO = -i, the i-th argument had an illegal value. */
/*          > 0: if INFO = i, and i is */
/*               <= N:  the QR algorithm failed to compute all the */
/*                      eigenvalues; elements 1:ILO-1 and i+1:N of W */
/*                      contain those eigenvalues which have converged; */
/*                      if JOBVS = 'V', VS contains the matrix which */
/*                      reduces A to its partially converged Schur form. */
/*               = N+1: the eigenvalues could not be reordered because */
/*                      some eigenvalues were too close to separate (the */
/*                      problem is very ill-conditioned); */
/*               = N+2: after reordering, roundoff changed values of */
/*                      some complex eigenvalues so that leading */
/*                      eigenvalues in the Schur form no longer satisfy */
/*                      SELECT = .TRUE..  This could also be caused by */
/*                      underflow due to scaling. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            LQUERY, SCALEA, WANTST, WANTVS >*/
/*<    >*/
/*<       DOUBLE PRECISION   ANRM, BIGNUM, CSCALE, EPS, S, SEP, SMLNUM >*/
/*     .. */
/*     .. Local Arrays .. */
/*<       DOUBLE PRECISION   DUM( 1 ) >*/
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

/*     Test the input arguments */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --w;
    vs_dim1 = *ldvs;
    vs_offset = 1 + vs_dim1;
    vs -= vs_offset;
    --work;
    --rwork;
    --bwork;

    /* Function Body */
    *info = 0;
/*<       LQUERY = ( LWORK.EQ.-1 ) >*/
    lquery = *lwork == -1;
/*<       WANTVS = LSAME( JOBVS, 'V' ) >*/
    wantvs = lsame_(jobvs, "V", (ftnlen)1, (ftnlen)1);
/*<       WANTST = LSAME( SORT, 'S' ) >*/
    wantst = lsame_(sort, "S", (ftnlen)1, (ftnlen)1);
/*<       IF( ( .NOT.WANTVS ) .AND. ( .NOT.LSAME( JOBVS, 'N' ) ) ) THEN >*/
    if (! wantvs && ! lsame_(jobvs, "N", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( ( .NOT.WANTST ) .AND. ( .NOT.LSAME( SORT, 'N' ) ) ) THEN >*/
    } else if (! wantst && ! lsame_(sort, "N", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( LDA.LT.MAX( 1, N ) ) THEN >*/
    } else if (*lda < max(1,*n)) {
/*<          INFO = -6 >*/
        *info = -6;
/*<       ELSE IF( LDVS.LT.1 .OR. ( WANTVS .AND. LDVS.LT.N ) ) THEN >*/
    } else if (*ldvs < 1 || (wantvs && *ldvs < *n)) {
/*<          INFO = -10 >*/
        *info = -10;
/*<       END IF >*/
    }

/*     Compute workspace */
/*      (Note: Comments in the code beginning "Workspace:" describe the */
/*       minimal amount of workspace needed at that point in the code, */
/*       as well as the preferred amount for good performance. */
/*       CWorkspace refers to complex workspace, and RWorkspace to real */
/*       workspace. NB refers to the optimal block size for the */
/*       immediately following subroutine, as returned by ILAENV. */
/*       HSWORK refers to the workspace preferred by ZHSEQR, as */
/*       calculated below. HSWORK is computed assuming ILO=1 and IHI=N, */
/*       the worst case.) */

/*<       IF( INFO.EQ.0 ) THEN >*/
    if (*info == 0) {
/*<          IF( N.EQ.0 ) THEN >*/
        if (*n == 0) {
/*<             MINWRK = 1 >*/
            minwrk = 1;
/*<             MAXWRK = 1 >*/
            maxwrk = 1;
/*<          ELSE >*/
        } else {
/*<             MAXWRK = N + N*ILAENV( 1, 'ZGEHRD', ' ', N, 1, N, 0 ) >*/
            maxwrk = *n + *n * ilaenv_(&c__1, "ZGEHRD", " ", n, &c__1, n, &
                    c__0, (ftnlen)6, (ftnlen)1);
/*<             MINWRK = 2*N >*/
            minwrk = *n << 1;

/*<    >*/
            zhseqr_("S", jobvs, n, &c__1, n, &a[a_offset], lda, &w[1], &vs[
                    vs_offset], ldvs, &work[1], &c_n1, &ieval, (ftnlen)1, (
                    ftnlen)1);
/*<             HSWORK = WORK( 1 ) >*/
            hswork = (integer) work[1].r;

/*<             IF( .NOT.WANTVS ) THEN >*/
            if (! wantvs) {
/*<                MAXWRK = MAX( MAXWRK, HSWORK ) >*/
                maxwrk = max(maxwrk,hswork);
/*<             ELSE >*/
            } else {
/*<    >*/
/* Computing MAX */
                i__1 = maxwrk, i__2 = *n + (*n - 1) * ilaenv_(&c__1, "ZUNGHR",
                         " ", n, &c__1, n, &c_n1, (ftnlen)6, (ftnlen)1);
                maxwrk = max(i__1,i__2);
/*<                MAXWRK = MAX( MAXWRK, HSWORK ) >*/
                maxwrk = max(maxwrk,hswork);
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<          WORK( 1 ) = MAXWRK >*/
        work[1].r = (doublereal) maxwrk, work[1].i = 0.;

/*<          IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN >*/
        if (*lwork < minwrk && ! lquery) {
/*<             INFO = -12 >*/
            *info = -12;
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZGEES ', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZGEES ", &i__1, (ftnlen)6);
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

/*<       ANRM = ZLANGE( 'M', N, N, A, LDA, DUM ) >*/
    anrm = zlange_("M", n, n, &a[a_offset], lda, dum, (ftnlen)1);
/*<       SCALEA = .FALSE. >*/
    scalea = FALSE_;
/*<       IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN >*/
    if (anrm > 0. && anrm < smlnum) {
/*<          SCALEA = .TRUE. >*/
        scalea = TRUE_;
/*<          CSCALE = SMLNUM >*/
        cscale = smlnum;
/*<       ELSE IF( ANRM.GT.BIGNUM ) THEN >*/
    } else if (anrm > bignum) {
/*<          SCALEA = .TRUE. >*/
        scalea = TRUE_;
/*<          CSCALE = BIGNUM >*/
        cscale = bignum;
/*<       END IF >*/
    }
/*<    >*/
    if (scalea) {
        zlascl_("G", &c__0, &c__0, &anrm, &cscale, n, n, &a[a_offset], lda, &
                ierr, (ftnlen)1);
    }

/*     Permute the matrix to make it more nearly triangular */
/*     (CWorkspace: none) */
/*     (RWorkspace: need N) */

/*<       IBAL = 1 >*/
    ibal = 1;
/*<       CALL ZGEBAL( 'P', N, A, LDA, ILO, IHI, RWORK( IBAL ), IERR ) >*/
    zgebal_("P", n, &a[a_offset], lda, &ilo, &ihi, &rwork[ibal], &ierr, (
            ftnlen)1);

/*     Reduce to upper Hessenberg form */
/*     (CWorkspace: need 2*N, prefer N+N*NB) */
/*     (RWorkspace: none) */

/*<       ITAU = 1 >*/
    itau = 1;
/*<       IWRK = N + ITAU >*/
    iwrk = *n + itau;
/*<    >*/
    i__1 = *lwork - iwrk + 1;
    zgehrd_(n, &ilo, &ihi, &a[a_offset], lda, &work[itau], &work[iwrk], &i__1,
             &ierr);

/*<       IF( WANTVS ) THEN >*/
    if (wantvs) {

/*        Copy Householder vectors to VS */

/*<          CALL ZLACPY( 'L', N, N, A, LDA, VS, LDVS ) >*/
        zlacpy_("L", n, n, &a[a_offset], lda, &vs[vs_offset], ldvs, (ftnlen)1)
                ;

/*        Generate unitary matrix in VS */
/*        (CWorkspace: need 2*N-1, prefer N+(N-1)*NB) */
/*        (RWorkspace: none) */

/*<    >*/
        i__1 = *lwork - iwrk + 1;
        zunghr_(n, &ilo, &ihi, &vs[vs_offset], ldvs, &work[itau], &work[iwrk],
                 &i__1, &ierr);
/*<       END IF >*/
    }

/*<       SDIM = 0 >*/
    *sdim = 0;

/*     Perform QR iteration, accumulating Schur vectors in VS if desired */
/*     (CWorkspace: need 1, prefer HSWORK (see comments) ) */
/*     (RWorkspace: none) */

/*<       IWRK = ITAU >*/
    iwrk = itau;
/*<    >*/
    i__1 = *lwork - iwrk + 1;
    zhseqr_("S", jobvs, n, &ilo, &ihi, &a[a_offset], lda, &w[1], &vs[
            vs_offset], ldvs, &work[iwrk], &i__1, &ieval, (ftnlen)1, (ftnlen)
            1);
/*<    >*/
    if (ieval > 0) {
        *info = ieval;
    }

/*     Sort eigenvalues if desired */

/*<       IF( WANTST .AND. INFO.EQ.0 ) THEN >*/
    if (wantst && *info == 0) {
/*<    >*/
        if (scalea) {
            zlascl_("G", &c__0, &c__0, &cscale, &anrm, n, &c__1, &w[1], n, &
                    ierr, (ftnlen)1);
        }
/*<          DO 10 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             BWORK( I ) = SELECT( W( I ) ) >*/
            bwork[i__] = (*select)(&w[i__]);
/*<    10    CONTINUE >*/
/* L10: */
        }

/*        Reorder eigenvalues and transform Schur vectors */
/*        (CWorkspace: none) */
/*        (RWorkspace: none) */

/*<    >*/
        i__1 = *lwork - iwrk + 1;
        ztrsen_("N", jobvs, &bwork[1], n, &a[a_offset], lda, &vs[vs_offset],
                ldvs, &w[1], sdim, &s, &sep, &work[iwrk], &i__1, &icond, (
                ftnlen)1, (ftnlen)1);
/*<       END IF >*/
    }

/*<       IF( WANTVS ) THEN >*/
    if (wantvs) {

/*        Undo balancing */
/*        (CWorkspace: none) */
/*        (RWorkspace: need N) */

/*<    >*/
        zgebak_("P", "R", n, &ilo, &ihi, &rwork[ibal], n, &vs[vs_offset],
                ldvs, &ierr, (ftnlen)1, (ftnlen)1);
/*<       END IF >*/
    }

/*<       IF( SCALEA ) THEN >*/
    if (scalea) {

/*        Undo scaling for the Schur form of A */

/*<          CALL ZLASCL( 'U', 0, 0, CSCALE, ANRM, N, N, A, LDA, IERR ) >*/
        zlascl_("U", &c__0, &c__0, &cscale, &anrm, n, n, &a[a_offset], lda, &
                ierr, (ftnlen)1);
/*<          CALL ZCOPY( N, A, LDA+1, W, 1 ) >*/
        i__1 = *lda + 1;
        zcopy_(n, &a[a_offset], &i__1, &w[1], &c__1);
/*<       END IF >*/
    }

/*<       WORK( 1 ) = MAXWRK >*/
    work[1].r = (doublereal) maxwrk, work[1].i = 0.;
/*<       RETURN >*/
    return 0;

/*     End of ZGEES */

/*<       END >*/
} /* zgees_ */

#ifdef __cplusplus
        }
#endif
