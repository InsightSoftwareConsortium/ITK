#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Table of constant values */
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__8 = 8;
static integer c_n1 = -1;
static integer c__4 = 4;

/* Subroutine */ void zgeev_(jobvl, jobvr, n, a, lda, w, vl, ldvl, vr, ldvr, work, lwork, rwork, info)
const char *jobvl, *jobvr;
const integer *n;
doublecomplex *a;
const integer *lda;
doublecomplex *w, *vl;
const integer *ldvl;
doublecomplex *vr;
const integer *ldvr;
doublecomplex *work;
integer *lwork;
doublereal *rwork;
integer *info;
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Local variables */
    static integer ibal;
    static char side[1];
    static integer maxb;
    static doublereal anrm;
    static integer ierr, itau, iwrk, nout, i, k;
    static logical scalea;
    static doublereal cscale;
    static logical select[1];
    static doublereal bignum;
    static integer minwrk, maxwrk;
    static logical wantvl;
    static doublereal smlnum;
    static integer hswork, irwork;
    static logical wantvr;
    static integer ihi;
    static doublereal scl;
    static integer ilo;
    static doublereal dum[1], eps;
    static doublecomplex tmp;


/*  -- LAPACK driver routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZGEEV computes for an N-by-N complex nonsymmetric matrix A, the       */
/*  eigenvalues and, optionally, the left and/or right eigenvectors.      */
/*                                                                        */
/*  The right eigenvector v(j) of A satisfies                             */
/*                   A * v(j) = lambda(j) * v(j)                          */
/*  where lambda(j) is its eigenvalue.                                    */
/*  The left eigenvector u(j) of A satisfies                              */
/*                u(j)**H * A = lambda(j) * u(j)**H                       */
/*  where u(j)**H denotes the conjugate transpose of u(j).                */
/*                                                                        */
/*  The computed eigenvectors are normalized to have Euclidean norm       */
/*  equal to 1 and largest component real.                                */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  JOBVL   (input) CHARACTER*1                                           */
/*          = 'N': left eigenvectors of A are not computed;               */
/*          = 'V': left eigenvectors of are computed.                     */
/*                                                                        */
/*  JOBVR   (input) CHARACTER*1                                           */
/*          = 'N': right eigenvectors of A are not computed;              */
/*          = 'V': right eigenvectors of A are computed.                  */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the matrix A. N >= 0.                            */
/*                                                                        */
/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)            */
/*          On entry, the N-by-N matrix A.                                */
/*          On exit, A has been overwritten.                              */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A.  LDA >= max(1,N).       */
/*                                                                        */
/*  W       (output) COMPLEX*16 array, dimension (N)                      */
/*          W contains the computed eigenvalues.                          */
/*                                                                        */
/*  VL      (output) COMPLEX*16 array, dimension (LDVL,N)                 */
/*          If JOBVL = 'V', the left eigenvectors u(j) are stored one     */
/*          after another in the columns of VL, in the same order         */
/*          as their eigenvalues.                                         */
/*          If JOBVL = 'N', VL is not referenced.                         */
/*          u(j) = VL(:,j), the j-th column of VL.                        */
/*                                                                        */
/*  LDVL    (input) INTEGER                                               */
/*          The leading dimension of the array VL.  LDVL >= 1; if         */
/*          JOBVL = 'V', LDVL >= N.                                       */
/*                                                                        */
/*  VR      (output) COMPLEX*16 array, dimension (LDVR,N)                 */
/*          If JOBVR = 'V', the right eigenvectors v(j) are stored one    */
/*          after another in the columns of VR, in the same order         */
/*          as their eigenvalues.                                         */
/*          If JOBVR = 'N', VR is not referenced.                         */
/*          v(j) = VR(:,j), the j-th column of VR.                        */
/*                                                                        */
/*  LDVR    (input) INTEGER                                               */
/*          The leading dimension of the array VR.  LDVR >= 1; if         */
/*          JOBVR = 'V', LDVR >= N.                                       */
/*                                                                        */
/*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)        */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.      */
/*                                                                        */
/*  LWORK   (input) INTEGER                                               */
/*          The dimension of the array WORK.  LWORK >= max(1,2*N).        */
/*          For good performance, LWORK must generally be larger.         */
/*                                                                        */
/*  RWORK   (workspace) DOUBLE PRECISION array, dimension (2*N)           */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0:  successful exit                                         */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value.   */
/*          > 0:  if INFO = i, the QR algorithm failed to compute all the */
/*                eigenvalues, and no eigenvectors have been computed;    */
/*                elements and i+1:N of W contain eigenvalues which have  */
/*                converged.                                              */
/*                                                                        */
/*  ===================================================================== */

    *info = 0;
    wantvl = lsame_(jobvl, "V");
    wantvr = lsame_(jobvr, "V");
    if (! wantvl && ! lsame_(jobvl, "N")) {
        *info = -1;
    } else if (! wantvr && ! lsame_(jobvr, "N")) {
        *info = -2;
    } else if (*n < 0) {
        *info = -3;
    } else if (*lda < max(1,*n)) {
        *info = -5;
    } else if (*ldvl < 1 || (wantvl && *ldvl < *n)) {
        *info = -8;
    } else if (*ldvr < 1 || (wantvr && *ldvr < *n)) {
        *info = -10;
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

    minwrk = 1;
    if (*info == 0 && *lwork >= 1) {
        maxwrk = *n + *n * ilaenv_(&c__1, "ZGEHRD", " ", n, &c__1, n, &c__0);
        if (! wantvl && ! wantvr) {
            minwrk = max(1,(*n<<1));
            maxb = ilaenv_(&c__8, "ZHSEQR", "EN", n, &c__1, n, &c_n1);
            maxb = max(maxb,2);
            k = ilaenv_(&c__4, "ZHSEQR", "EN", n, &c__1, n, &c_n1);
            k = min(min(maxb,*n),max(2,k));
            hswork = max(k * (k + 2),(*n<<1));
            maxwrk = max(maxwrk,hswork);
        } else {
            minwrk = max(1,(*n<<1));
            i__1 = *n + (*n - 1) * ilaenv_(&c__1, "ZUNGHR", " ", n, &c__1, n, &c_n1);
            maxwrk = max(maxwrk,i__1);
            maxb = ilaenv_(&c__8, "ZHSEQR", "SV", n, &c__1, n, &c_n1);
            maxb = max(maxb,2);
            k = ilaenv_(&c__4, "ZHSEQR", "SV", n, &c__1, n, &c_n1);
            k = min(min(maxb,*n),max(2,k));
            hswork = max(k * (k + 2),(*n<<1));
            maxwrk = max(max(maxwrk,hswork),(*n<<1));
        }
        work[0].r = (doublereal) maxwrk, work[0].i = 0.;
    }
    if (*lwork < minwrk) {
        *info = -12;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("ZGEEV ", &i__1);
        return;
    }

/*     Quick return if possible */

    if (*n == 0) {
        return;
    }

/*     Get machine constants */

    eps = dlamch_("P");
    smlnum = dlamch_("S");
    bignum = 1. / smlnum;
    dlabad_(&smlnum, &bignum);
    smlnum = sqrt(smlnum) / eps;
    bignum = 1. / smlnum;

/*     Scale A if max element outside range [SMLNUM,BIGNUM] */

    anrm = zlange_("M", n, n, a, lda, dum);
    scalea = FALSE_;
    if (anrm > 0. && anrm < smlnum) {
        scalea = TRUE_;
        cscale = smlnum;
    } else if (anrm > bignum) {
        scalea = TRUE_;
        cscale = bignum;
    }
    if (scalea) {
        zlascl_("G", &c__0, &c__0, &anrm, &cscale, n, n, a, lda, &ierr);
    }

/*     Balance the matrix */
/*     (CWorkspace: none) */
/*     (RWorkspace: need N) */

    ibal = 0;
    zgebal_("B", n, a, lda, &ilo, &ihi, &rwork[ibal], &ierr);

/*     Reduce to upper Hessenberg form */
/*     (CWorkspace: need 2*N, prefer N+N*NB) */
/*     (RWorkspace: none) */

    itau = 0;
    iwrk = itau + *n;
    i__1 = *lwork - iwrk;
    zgehrd_(n, &ilo, &ihi, a, lda, &work[itau], &work[iwrk], &i__1, &ierr);

    if (wantvl) {

/*        Want left eigenvectors */
/*        Copy Householder vectors to VL */

        *side = 'L';
        zlacpy_("L", n, n, a, lda, vl, ldvl);

/*        Generate unitary matrix in VL */
/*        (CWorkspace: need 2*N-1, prefer N+(N-1)*NB) */
/*        (RWorkspace: none) */

        i__1 = *lwork - iwrk;
        zunghr_(n, &ilo, &ihi, vl, ldvl, &work[itau], &work[iwrk], &i__1, &ierr);

/*        Perform QR iteration, accumulating Schur vectors in VL */
/*        (CWorkspace: need 1, prefer HSWORK (see comments) ) */
/*        (RWorkspace: none) */

        iwrk = itau;
        i__1 = *lwork - iwrk;
        zhseqr_("S", "V", n, &ilo, &ihi, a, lda, w, vl, ldvl, &work[iwrk], &i__1, info);

        if (wantvr) {

/*           Want left and right eigenvectors */
/*           Copy Schur vectors to VR */

            *side = 'B';
            zlacpy_("F", n, n, vl, ldvl, vr, ldvr);
        }

    } else if (wantvr) {

/*        Want right eigenvectors */
/*        Copy Householder vectors to VR */

        *side = 'R';
        zlacpy_("L", n, n, a, lda, vr, ldvr);

/*        Generate unitary matrix in VR */
/*        (CWorkspace: need 2*N-1, prefer N+(N-1)*NB) */
/*        (RWorkspace: none) */

        i__1 = *lwork - iwrk;
        zunghr_(n, &ilo, &ihi, vr, ldvr, &work[itau], &work[iwrk], &i__1, &ierr);

/*        Perform QR iteration, accumulating Schur vectors in VR */
/*        (CWorkspace: need 1, prefer HSWORK (see comments) ) */
/*        (RWorkspace: none) */

        iwrk = itau;
        i__1 = *lwork - iwrk;
        zhseqr_("S", "V", n, &ilo, &ihi, a, lda, w, vr, ldvr, &work[iwrk], &i__1, info);

    } else {

/*        Compute eigenvalues only */
/*        (CWorkspace: need 1, prefer HSWORK (see comments) ) */
/*        (RWorkspace: none) */

        iwrk = itau;
        i__1 = *lwork - iwrk;
        zhseqr_("E", "N", n, &ilo, &ihi, a, lda, w, vr, ldvr, &work[iwrk], &i__1, info);
    }

/*     If INFO > 0 from ZHSEQR, then quit */

    if (*info > 0) {
        goto L50;
    }

    if (wantvl || wantvr) {

/*        Compute left and/or right eigenvectors */
/*        (CWorkspace: need 2*N) */
/*        (RWorkspace: need 2*N) */

        irwork = ibal + *n;
        ztrevc_(side, "B", select, n, a, lda, vl, ldvl, vr, ldvr, n, &nout, &work[iwrk], &rwork[irwork], &ierr);
    }

    if (wantvl) {

/*        Undo balancing of left eigenvectors */
/*        (CWorkspace: none) */
/*        (RWorkspace: need N) */

        zgebak_("B", "L", n, &ilo, &ihi, &rwork[ibal], n, vl, ldvl, &ierr);

/*        Normalize left eigenvectors and make largest component real */

        for (i = 0; i < *n; ++i) {
            scl = 1. / dznrm2_(n, &vl[i * *ldvl], &c__1);
            zdscal_(n, &scl, &vl[i * *ldvl], &c__1);
            for (k = 0; k < *n; ++k) {
                i__1 = k + i * *ldvl; /* index [k,i] */
                rwork[irwork + k] = vl[i__1].r * vl[i__1].r + vl[i__1].i * vl[i__1].i;
            }
            k = idamax_(n, &rwork[irwork], &c__1) - 1;
            d_cnjg(&tmp, &vl[k + i * *ldvl]);
            d__1 = sqrt(rwork[irwork + k]);
            tmp.r /= d__1, tmp.i /= d__1;
            zscal_(n, &tmp, &vl[i * *ldvl], &c__1);
            vl[k + i * *ldvl].i = 0.;
        }
    }

    if (wantvr) {

/*        Undo balancing of right eigenvectors */
/*        (CWorkspace: none) */
/*        (RWorkspace: need N) */

        zgebak_("B", "R", n, &ilo, &ihi, &rwork[ibal], n, vr, ldvr, &ierr);

/*        Normalize right eigenvectors and make largest component real */

        for (i = 0; i < *n; ++i) {
            scl = 1. / dznrm2_(n, &vr[i * *ldvr], &c__1);
            zdscal_(n, &scl, &vr[i * *ldvr], &c__1);
            for (k = 0; k < *n; ++k) {
                i__1 = k + i * *ldvr; /* index [k,i] */
                rwork[irwork + k] = vr[i__1].r * vr[i__1].r + vr[i__1].i * vr[i__1].i;
            }
            k = idamax_(n, &rwork[irwork], &c__1) - 1;
            d_cnjg(&tmp, &vr[k + i * *ldvr]);
            d__1 = sqrt(rwork[irwork + k]);
            tmp.r /= d__1, tmp.i /= d__1;
            zscal_(n, &tmp, &vr[i * *ldvr], &c__1);
            vr[k + i * *ldvr].i = 0.;
        }
    }

/*     Undo scaling if necessary */

L50:
    if (scalea) {
        i__1 = *n - *info;
        i__2 = max(i__1, 1);
        zlascl_("G", &c__0, &c__0, &cscale, &anrm, &i__1, &c__1, &w[*info], &i__2, &ierr);
        if (*info > 0) {
            i__1 = ilo - 1;
            zlascl_("G", &c__0, &c__0, &cscale, &anrm, &i__1, &c__1, w, n, &ierr);
        }
    }

    work[0].r = (doublereal) maxwrk, work[0].i = 0.;

} /* zgeev_ */
