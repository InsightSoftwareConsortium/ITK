/* lapack/complex16/zgeev.f -- translated by f2c (version 20050501).
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
static integer c__8 = 8;
static integer c_n1 = -1;
static integer c__4 = 4;

/*<    >*/
/* Subroutine */ int zgeev_(char *jobvl, char *jobvr, integer *n,
        doublecomplex *a, integer *lda, doublecomplex *w, doublecomplex *vl,
        integer *ldvl, doublecomplex *vr, integer *ldvr, doublecomplex *work,
        integer *lwork, doublereal *rwork, integer *info, ftnlen jobvl_len,
        ftnlen jobvr_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, vl_dim1, vl_offset, vr_dim1, vr_offset, i__1,
            i__2, i__3, i__4;
    doublereal d__1, d__2;
    doublecomplex z__1, z__2;

    /* Builtin functions */
    double sqrt(doublereal), d_imag(doublecomplex *);
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    integer i__, k, ihi;
    doublereal scl;
    integer ilo;
    doublereal dum[1], eps;
    doublecomplex tmp;
    integer ibal;
    char side[1];
    integer maxb;
    doublereal anrm;
    integer ierr, itau, iwrk, nout;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int zscal_(integer *, doublecomplex *,
            doublecomplex *, integer *), dlabad_(doublereal *, doublereal *);
    extern doublereal dznrm2_(integer *, doublecomplex *, integer *);
    logical scalea;
    extern doublereal dlamch_(char *, ftnlen);
    doublereal cscale;
    extern /* Subroutine */ int zgebak_(char *, char *, integer *, integer *,
            integer *, doublereal *, integer *, doublecomplex *, integer *,
            integer *, ftnlen, ftnlen), zgebal_(char *, integer *,
            doublecomplex *, integer *, integer *, integer *, doublereal *,
            integer *, ftnlen);
    extern integer idamax_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *,
            integer *, integer *, ftnlen, ftnlen);
    logical select[1];
    extern /* Subroutine */ int zdscal_(integer *, doublereal *,
            doublecomplex *, integer *);
    doublereal bignum;
    extern doublereal zlange_(char *, integer *, integer *, doublecomplex *,
            integer *, doublereal *, ftnlen);
    extern /* Subroutine */ int zgehrd_(integer *, integer *, integer *,
            doublecomplex *, integer *, doublecomplex *, doublecomplex *,
            integer *, integer *), zlascl_(char *, integer *, integer *,
            doublereal *, doublereal *, integer *, integer *, doublecomplex *,
             integer *, integer *, ftnlen), zlacpy_(char *, integer *,
            integer *, doublecomplex *, integer *, doublecomplex *, integer *,
             ftnlen);
    integer minwrk, maxwrk=0;
    logical wantvl;
    doublereal smlnum;
    integer hswork, irwork=0;
    extern /* Subroutine */ int zhseqr_(char *, char *, integer *, integer *,
            integer *, doublecomplex *, integer *, doublecomplex *,
            doublecomplex *, integer *, doublecomplex *, integer *, integer *,
             ftnlen, ftnlen), ztrevc_(char *, char *, logical *, integer *,
            doublecomplex *, integer *, doublecomplex *, integer *,
            doublecomplex *, integer *, integer *, integer *, doublecomplex *,
             doublereal *, integer *, ftnlen, ftnlen);
    logical lquery, wantvr;
    extern /* Subroutine */ int zunghr_(integer *, integer *, integer *,
            doublecomplex *, integer *, doublecomplex *, doublecomplex *,
            integer *, integer *);
    (void)jobvl_len;
    (void)jobvr_len;

/*  -- LAPACK driver routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          JOBVL, JOBVR >*/
/*<       INTEGER            INFO, LDA, LDVL, LDVR, LWORK, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   RWORK( * ) >*/
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZGEEV computes for an N-by-N complex nonsymmetric matrix A, the */
/*  eigenvalues and, optionally, the left and/or right eigenvectors. */

/*  The right eigenvector v(j) of A satisfies */
/*                   A * v(j) = lambda(j) * v(j) */
/*  where lambda(j) is its eigenvalue. */
/*  The left eigenvector u(j) of A satisfies */
/*                u(j)**H * A = lambda(j) * u(j)**H */
/*  where u(j)**H denotes the conjugate transpose of u(j). */

/*  The computed eigenvectors are normalized to have Euclidean norm */
/*  equal to 1 and largest component real. */

/*  Arguments */
/*  ========= */

/*  JOBVL   (input) CHARACTER*1 */
/*          = 'N': left eigenvectors of A are not computed; */
/*          = 'V': left eigenvectors of are computed. */

/*  JOBVR   (input) CHARACTER*1 */
/*          = 'N': right eigenvectors of A are not computed; */
/*          = 'V': right eigenvectors of A are computed. */

/*  N       (input) INTEGER */
/*          The order of the matrix A. N >= 0. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N) */
/*          On entry, the N-by-N matrix A. */
/*          On exit, A has been overwritten. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,N). */

/*  W       (output) COMPLEX*16 array, dimension (N) */
/*          W contains the computed eigenvalues. */

/*  VL      (output) COMPLEX*16 array, dimension (LDVL,N) */
/*          If JOBVL = 'V', the left eigenvectors u(j) are stored one */
/*          after another in the columns of VL, in the same order */
/*          as their eigenvalues. */
/*          If JOBVL = 'N', VL is not referenced. */
/*          u(j) = VL(:,j), the j-th column of VL. */

/*  LDVL    (input) INTEGER */
/*          The leading dimension of the array VL.  LDVL >= 1; if */
/*          JOBVL = 'V', LDVL >= N. */

/*  VR      (output) COMPLEX*16 array, dimension (LDVR,N) */
/*          If JOBVR = 'V', the right eigenvectors v(j) are stored one */
/*          after another in the columns of VR, in the same order */
/*          as their eigenvalues. */
/*          If JOBVR = 'N', VR is not referenced. */
/*          v(j) = VR(:,j), the j-th column of VR. */

/*  LDVR    (input) INTEGER */
/*          The leading dimension of the array VR.  LDVR >= 1; if */
/*          JOBVR = 'V', LDVR >= N. */

/*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK) */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK. */

/*  LWORK   (input) INTEGER */
/*          The dimension of the array WORK.  LWORK >= max(1,2*N). */
/*          For good performance, LWORK must generally be larger. */

/*          If LWORK = -1, then a workspace query is assumed; the routine */
/*          only calculates the optimal size of the WORK array, returns */
/*          this value as the first entry of the WORK array, and no error */
/*          message related to LWORK is issued by XERBLA. */

/*  RWORK   (workspace) DOUBLE PRECISION array, dimension (2*N) */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value. */
/*          > 0:  if INFO = i, the QR algorithm failed to compute all the */
/*                eigenvalues, and no eigenvectors have been computed; */
/*                elements and i+1:N of W contain eigenvalues which have */
/*                converged. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            LQUERY, SCALEA, WANTVL, WANTVR >*/
/*<       CHARACTER          SIDE >*/
/*<    >*/
/*<       DOUBLE PRECISION   ANRM, BIGNUM, CSCALE, EPS, SCL, SMLNUM >*/
/*<       COMPLEX*16         TMP >*/
/*     .. */
/*     .. Local Arrays .. */
/*<       LOGICAL            SELECT( 1 ) >*/
/*<       DOUBLE PRECISION   DUM( 1 ) >*/
/*     .. */
/*     .. External Subroutines .. */
/*<    >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       INTEGER            IDAMAX, ILAENV >*/
/*<       DOUBLE PRECISION   DLAMCH, DZNRM2, ZLANGE >*/
/*<       EXTERNAL           LSAME, IDAMAX, ILAENV, DLAMCH, DZNRM2, ZLANGE >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          DBLE, DCMPLX, DCONJG, DIMAG, MAX, MIN, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input arguments */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --w;
    vl_dim1 = *ldvl;
    vl_offset = 1 + vl_dim1;
    vl -= vl_offset;
    vr_dim1 = *ldvr;
    vr_offset = 1 + vr_dim1;
    vr -= vr_offset;
    --work;
    --rwork;

    /* Function Body */
    *info = 0;
/*<       LQUERY = ( LWORK.EQ.-1 ) >*/
    lquery = *lwork == -1;
/*<       WANTVL = LSAME( JOBVL, 'V' ) >*/
    wantvl = lsame_(jobvl, "V", (ftnlen)1, (ftnlen)1);
/*<       WANTVR = LSAME( JOBVR, 'V' ) >*/
    wantvr = lsame_(jobvr, "V", (ftnlen)1, (ftnlen)1);
/*<       IF( ( .NOT.WANTVL ) .AND. ( .NOT.LSAME( JOBVL, 'N' ) ) ) THEN >*/
    if (! wantvl && ! lsame_(jobvl, "N", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( ( .NOT.WANTVR ) .AND. ( .NOT.LSAME( JOBVR, 'N' ) ) ) THEN >*/
    } else if (! wantvr && ! lsame_(jobvr, "N", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -3 >*/
        *info = -3;
/*<       ELSE IF( LDA.LT.MAX( 1, N ) ) THEN >*/
    } else if (*lda < max(1,*n)) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       ELSE IF( LDVL.LT.1 .OR. ( WANTVL .AND. LDVL.LT.N ) ) THEN >*/
    } else if (*ldvl < 1 || (wantvl && *ldvl < *n)) {
/*<          INFO = -8 >*/
        *info = -8;
/*<       ELSE IF( LDVR.LT.1 .OR. ( WANTVR .AND. LDVR.LT.N ) ) THEN >*/
    } else if (*ldvr < 1 || (wantvr && *ldvr < *n)) {
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

/*<       MINWRK = 1 >*/
    minwrk = 1;
/*<       IF( INFO.EQ.0 .AND. ( LWORK.GE.1 .OR. LQUERY ) ) THEN >*/
    if (*info == 0 && (*lwork >= 1 || lquery)) {
/*<          MAXWRK = N + N*ILAENV( 1, 'ZGEHRD', ' ', N, 1, N, 0 ) >*/
        maxwrk = *n + *n * ilaenv_(&c__1, "ZGEHRD", " ", n, &c__1, n, &c__0, (
                ftnlen)6, (ftnlen)1);
/*<          IF( ( .NOT.WANTVL ) .AND. ( .NOT.WANTVR ) ) THEN >*/
        if (! wantvl && ! wantvr) {
/*<             MINWRK = MAX( 1, 2*N ) >*/
/* Computing MAX */
            i__1 = 1, i__2 = *n << 1;
            minwrk = max(i__1,i__2);
/*<             MAXB = MAX( ILAENV( 8, 'ZHSEQR', 'EN', N, 1, N, -1 ), 2 ) >*/
/* Computing MAX */
            i__1 = ilaenv_(&c__8, "ZHSEQR", "EN", n, &c__1, n, &c_n1, (ftnlen)
                    6, (ftnlen)2);
            maxb = max(i__1,2);
/*<    >*/
/* Computing MIN */
/* Computing MAX */
            i__3 = 2, i__4 = ilaenv_(&c__4, "ZHSEQR", "EN", n, &c__1, n, &
                    c_n1, (ftnlen)6, (ftnlen)2);
            i__1 = min(maxb,*n), i__2 = max(i__3,i__4);
            k = min(i__1,i__2);
/*<             HSWORK = MAX( K*( K+2 ), 2*N ) >*/
/* Computing MAX */
            i__1 = k * (k + 2), i__2 = *n << 1;
            hswork = max(i__1,i__2);
/*<             MAXWRK = MAX( MAXWRK, HSWORK ) >*/
            maxwrk = max(maxwrk,hswork);
/*<          ELSE >*/
        } else {
/*<             MINWRK = MAX( 1, 2*N ) >*/
/* Computing MAX */
            i__1 = 1, i__2 = *n << 1;
            minwrk = max(i__1,i__2);
/*<    >*/
/* Computing MAX */
            i__1 = maxwrk, i__2 = *n + (*n - 1) * ilaenv_(&c__1, "ZUNGHR",
                    " ", n, &c__1, n, &c_n1, (ftnlen)6, (ftnlen)1);
            maxwrk = max(i__1,i__2);
/*<             MAXB = MAX( ILAENV( 8, 'ZHSEQR', 'SV', N, 1, N, -1 ), 2 ) >*/
/* Computing MAX */
            i__1 = ilaenv_(&c__8, "ZHSEQR", "SV", n, &c__1, n, &c_n1, (ftnlen)
                    6, (ftnlen)2);
            maxb = max(i__1,2);
/*<    >*/
/* Computing MIN */
/* Computing MAX */
            i__3 = 2, i__4 = ilaenv_(&c__4, "ZHSEQR", "SV", n, &c__1, n, &
                    c_n1, (ftnlen)6, (ftnlen)2);
            i__1 = min(maxb,*n), i__2 = max(i__3,i__4);
            k = min(i__1,i__2);
/*<             HSWORK = MAX( K*( K+2 ), 2*N ) >*/
/* Computing MAX */
            i__1 = k * (k + 2), i__2 = *n << 1;
            hswork = max(i__1,i__2);
/*<             MAXWRK = MAX( MAXWRK, HSWORK, 2*N ) >*/
/* Computing MAX */
            i__1 = max(maxwrk,hswork), i__2 = *n << 1;
            maxwrk = max(i__1,i__2);
/*<          END IF >*/
        }
/*<          WORK( 1 ) = MAXWRK >*/
        work[1].r = (doublereal) maxwrk, work[1].i = 0.;
/*<       END IF >*/
    }
/*<       IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN >*/
    if (*lwork < minwrk && ! lquery) {
/*<          INFO = -12 >*/
        *info = -12;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZGEEV ', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZGEEV ", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       ELSE IF( LQUERY ) THEN >*/
    } else if (lquery) {
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<    >*/
    if (*n == 0) {
        return 0;
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

/*     Balance the matrix */
/*     (CWorkspace: none) */
/*     (RWorkspace: need N) */

/*<       IBAL = 1 >*/
    ibal = 1;
/*<       CALL ZGEBAL( 'B', N, A, LDA, ILO, IHI, RWORK( IBAL ), IERR ) >*/
    zgebal_("B", n, &a[a_offset], lda, &ilo, &ihi, &rwork[ibal], &ierr, (
            ftnlen)1);

/*     Reduce to upper Hessenberg form */
/*     (CWorkspace: need 2*N, prefer N+N*NB) */
/*     (RWorkspace: none) */

/*<       ITAU = 1 >*/
    itau = 1;
/*<       IWRK = ITAU + N >*/
    iwrk = itau + *n;
/*<    >*/
    i__1 = *lwork - iwrk + 1;
    zgehrd_(n, &ilo, &ihi, &a[a_offset], lda, &work[itau], &work[iwrk], &i__1,
             &ierr);

/*<       IF( WANTVL ) THEN >*/
    if (wantvl) {

/*        Want left eigenvectors */
/*        Copy Householder vectors to VL */

/*<          SIDE = 'L' >*/
        *(unsigned char *)side = 'L';
/*<          CALL ZLACPY( 'L', N, N, A, LDA, VL, LDVL ) >*/
        zlacpy_("L", n, n, &a[a_offset], lda, &vl[vl_offset], ldvl, (ftnlen)1)
                ;

/*        Generate unitary matrix in VL */
/*        (CWorkspace: need 2*N-1, prefer N+(N-1)*NB) */
/*        (RWorkspace: none) */

/*<    >*/
        i__1 = *lwork - iwrk + 1;
        zunghr_(n, &ilo, &ihi, &vl[vl_offset], ldvl, &work[itau], &work[iwrk],
                 &i__1, &ierr);

/*        Perform QR iteration, accumulating Schur vectors in VL */
/*        (CWorkspace: need 1, prefer HSWORK (see comments) ) */
/*        (RWorkspace: none) */

/*<          IWRK = ITAU >*/
        iwrk = itau;
/*<    >*/
        i__1 = *lwork - iwrk + 1;
        zhseqr_("S", "V", n, &ilo, &ihi, &a[a_offset], lda, &w[1], &vl[
                vl_offset], ldvl, &work[iwrk], &i__1, info, (ftnlen)1, (
                ftnlen)1);

/*<          IF( WANTVR ) THEN >*/
        if (wantvr) {

/*           Want left and right eigenvectors */
/*           Copy Schur vectors to VR */

/*<             SIDE = 'B' >*/
            *(unsigned char *)side = 'B';
/*<             CALL ZLACPY( 'F', N, N, VL, LDVL, VR, LDVR ) >*/
            zlacpy_("F", n, n, &vl[vl_offset], ldvl, &vr[vr_offset], ldvr, (
                    ftnlen)1);
/*<          END IF >*/
        }

/*<       ELSE IF( WANTVR ) THEN >*/
    } else if (wantvr) {

/*        Want right eigenvectors */
/*        Copy Householder vectors to VR */

/*<          SIDE = 'R' >*/
        *(unsigned char *)side = 'R';
/*<          CALL ZLACPY( 'L', N, N, A, LDA, VR, LDVR ) >*/
        zlacpy_("L", n, n, &a[a_offset], lda, &vr[vr_offset], ldvr, (ftnlen)1)
                ;

/*        Generate unitary matrix in VR */
/*        (CWorkspace: need 2*N-1, prefer N+(N-1)*NB) */
/*        (RWorkspace: none) */

/*<    >*/
        i__1 = *lwork - iwrk + 1;
        zunghr_(n, &ilo, &ihi, &vr[vr_offset], ldvr, &work[itau], &work[iwrk],
                 &i__1, &ierr);

/*        Perform QR iteration, accumulating Schur vectors in VR */
/*        (CWorkspace: need 1, prefer HSWORK (see comments) ) */
/*        (RWorkspace: none) */

/*<          IWRK = ITAU >*/
        iwrk = itau;
/*<    >*/
        i__1 = *lwork - iwrk + 1;
        zhseqr_("S", "V", n, &ilo, &ihi, &a[a_offset], lda, &w[1], &vr[
                vr_offset], ldvr, &work[iwrk], &i__1, info, (ftnlen)1, (
                ftnlen)1);

/*<       ELSE >*/
    } else {

/*        Compute eigenvalues only */
/*        (CWorkspace: need 1, prefer HSWORK (see comments) ) */
/*        (RWorkspace: none) */

/*<          IWRK = ITAU >*/
        iwrk = itau;
/*<    >*/
        i__1 = *lwork - iwrk + 1;
        zhseqr_("E", "N", n, &ilo, &ihi, &a[a_offset], lda, &w[1], &vr[
                vr_offset], ldvr, &work[iwrk], &i__1, info, (ftnlen)1, (
                ftnlen)1);
/*<       END IF >*/
    }

/*     If INFO > 0 from ZHSEQR, then quit */

/*<    >*/
    if (*info > 0) {
        goto L50;
    }

/*<       IF( WANTVL .OR. WANTVR ) THEN >*/
    if (wantvl || wantvr) {

/*        Compute left and/or right eigenvectors */
/*        (CWorkspace: need 2*N) */
/*        (RWorkspace: need 2*N) */

/*<          IRWORK = IBAL + N >*/
        irwork = ibal + *n;
/*<    >*/
        ztrevc_(side, "B", select, n, &a[a_offset], lda, &vl[vl_offset], ldvl,
                 &vr[vr_offset], ldvr, n, &nout, &work[iwrk], &rwork[irwork],
                &ierr, (ftnlen)1, (ftnlen)1);
/*<       END IF >*/
    }

/*<       IF( WANTVL ) THEN >*/
    if (wantvl) {

/*        Undo balancing of left eigenvectors */
/*        (CWorkspace: none) */
/*        (RWorkspace: need N) */

/*<    >*/
        zgebak_("B", "L", n, &ilo, &ihi, &rwork[ibal], n, &vl[vl_offset],
                ldvl, &ierr, (ftnlen)1, (ftnlen)1);

/*        Normalize left eigenvectors and make largest component real */

/*<          DO 20 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             SCL = ONE / DZNRM2( N, VL( 1, I ), 1 ) >*/
            scl = 1. / dznrm2_(n, &vl[i__ * vl_dim1 + 1], &c__1);
/*<             CALL ZDSCAL( N, SCL, VL( 1, I ), 1 ) >*/
            zdscal_(n, &scl, &vl[i__ * vl_dim1 + 1], &c__1);
/*<             DO 10 K = 1, N >*/
            i__2 = *n;
            for (k = 1; k <= i__2; ++k) {
/*<    >*/
                i__3 = k + i__ * vl_dim1;
/* Computing 2nd power */
                d__1 = vl[i__3].r;
/* Computing 2nd power */
                d__2 = d_imag(&vl[k + i__ * vl_dim1]);
                rwork[irwork + k - 1] = d__1 * d__1 + d__2 * d__2;
/*<    10       CONTINUE >*/
/* L10: */
            }
/*<             K = IDAMAX( N, RWORK( IRWORK ), 1 ) >*/
            k = idamax_(n, &rwork[irwork], &c__1);
/*<             TMP = DCONJG( VL( K, I ) ) / SQRT( RWORK( IRWORK+K-1 ) ) >*/
            d_cnjg(&z__2, &vl[k + i__ * vl_dim1]);
            d__1 = sqrt(rwork[irwork + k - 1]);
            z__1.r = z__2.r / d__1, z__1.i = z__2.i / d__1;
            tmp.r = z__1.r, tmp.i = z__1.i;
/*<             CALL ZSCAL( N, TMP, VL( 1, I ), 1 ) >*/
            zscal_(n, &tmp, &vl[i__ * vl_dim1 + 1], &c__1);
/*<             VL( K, I ) = DCMPLX( DBLE( VL( K, I ) ), ZERO ) >*/
            i__2 = k + i__ * vl_dim1;
            i__3 = k + i__ * vl_dim1;
            d__1 = vl[i__3].r;
            z__1.r = d__1, z__1.i = 0.;
            vl[i__2].r = z__1.r, vl[i__2].i = z__1.i;
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<       END IF >*/
    }

/*<       IF( WANTVR ) THEN >*/
    if (wantvr) {

/*        Undo balancing of right eigenvectors */
/*        (CWorkspace: none) */
/*        (RWorkspace: need N) */

/*<    >*/
        zgebak_("B", "R", n, &ilo, &ihi, &rwork[ibal], n, &vr[vr_offset],
                ldvr, &ierr, (ftnlen)1, (ftnlen)1);

/*        Normalize right eigenvectors and make largest component real */

/*<          DO 40 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             SCL = ONE / DZNRM2( N, VR( 1, I ), 1 ) >*/
            scl = 1. / dznrm2_(n, &vr[i__ * vr_dim1 + 1], &c__1);
/*<             CALL ZDSCAL( N, SCL, VR( 1, I ), 1 ) >*/
            zdscal_(n, &scl, &vr[i__ * vr_dim1 + 1], &c__1);
/*<             DO 30 K = 1, N >*/
            i__2 = *n;
            for (k = 1; k <= i__2; ++k) {
/*<    >*/
                i__3 = k + i__ * vr_dim1;
/* Computing 2nd power */
                d__1 = vr[i__3].r;
/* Computing 2nd power */
                d__2 = d_imag(&vr[k + i__ * vr_dim1]);
                rwork[irwork + k - 1] = d__1 * d__1 + d__2 * d__2;
/*<    30       CONTINUE >*/
/* L30: */
            }
/*<             K = IDAMAX( N, RWORK( IRWORK ), 1 ) >*/
            k = idamax_(n, &rwork[irwork], &c__1);
/*<             TMP = DCONJG( VR( K, I ) ) / SQRT( RWORK( IRWORK+K-1 ) ) >*/
            d_cnjg(&z__2, &vr[k + i__ * vr_dim1]);
            d__1 = sqrt(rwork[irwork + k - 1]);
            z__1.r = z__2.r / d__1, z__1.i = z__2.i / d__1;
            tmp.r = z__1.r, tmp.i = z__1.i;
/*<             CALL ZSCAL( N, TMP, VR( 1, I ), 1 ) >*/
            zscal_(n, &tmp, &vr[i__ * vr_dim1 + 1], &c__1);
/*<             VR( K, I ) = DCMPLX( DBLE( VR( K, I ) ), ZERO ) >*/
            i__2 = k + i__ * vr_dim1;
            i__3 = k + i__ * vr_dim1;
            d__1 = vr[i__3].r;
            z__1.r = d__1, z__1.i = 0.;
            vr[i__2].r = z__1.r, vr[i__2].i = z__1.i;
/*<    40    CONTINUE >*/
/* L40: */
        }
/*<       END IF >*/
    }

/*     Undo scaling if necessary */

/*<    50 CONTINUE >*/
L50:
/*<       IF( SCALEA ) THEN >*/
    if (scalea) {
/*<    >*/
        i__1 = *n - *info;
/* Computing MAX */
        i__3 = *n - *info;
        i__2 = max(i__3,1);
        zlascl_("G", &c__0, &c__0, &cscale, &anrm, &i__1, &c__1, &w[*info + 1]
                , &i__2, &ierr, (ftnlen)1);
/*<          IF( INFO.GT.0 ) THEN >*/
        if (*info > 0) {
/*<             CALL ZLASCL( 'G', 0, 0, CSCALE, ANRM, ILO-1, 1, W, N, IERR ) >*/
            i__1 = ilo - 1;
            zlascl_("G", &c__0, &c__0, &cscale, &anrm, &i__1, &c__1, &w[1], n,
                     &ierr, (ftnlen)1);
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       WORK( 1 ) = MAXWRK >*/
    work[1].r = (doublereal) maxwrk, work[1].i = 0.;
/*<       RETURN >*/
    return 0;

/*     End of ZGEEV */

/*<       END >*/
} /* zgeev_ */

#ifdef __cplusplus
        }
#endif
