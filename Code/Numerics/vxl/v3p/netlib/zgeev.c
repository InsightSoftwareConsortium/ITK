/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;
static integer c__8 = 8;
static integer c_n1 = -1;
static integer c__4 = 4;

/* Subroutine */ int zgeev_(jobvl, jobvr, n, a, lda, w, vl, ldvl, vr, ldvr,
        work, lwork, rwork, info, jobvl_len, jobvr_len)
char *jobvl, *jobvr;
integer *n;
doublecomplex *a;
integer *lda;
doublecomplex *w, *vl;
integer *ldvl;
doublecomplex *vr;
integer *ldvr;
doublecomplex *work;
integer *lwork;
doublereal *rwork;
integer *info;
ftnlen jobvl_len;
ftnlen jobvr_len;
{
    /* System generated locals */
    integer a_dim1, a_offset, vl_dim1, vl_offset, vr_dim1, vr_offset, i__1,
            i__2, i__3, i__4;
    doublereal d__1, d__2;
    doublecomplex z__1, z__2;

    /* Builtin functions */
    double sqrt(), d_imag();
    void d_cnjg();

    /* Local variables */
    static integer ibal;
    static char side[1];
    static integer maxb;
    static doublereal anrm;
    static integer ierr, itau, iwrk, nout, i, k;
    extern logical lsame_();
    extern /* Subroutine */ int zscal_(), dlabad_();
    extern doublereal dznrm2_();
    static logical scalea;
    extern doublereal dlamch_();
    static doublereal cscale;
    extern /* Subroutine */ int zgebak_(), zgebal_();
    extern integer idamax_();
    extern /* Subroutine */ int xerbla_();
    extern integer ilaenv_();
    static logical select[1];
    extern /* Subroutine */ int zdscal_();
    static doublereal bignum;
    extern doublereal zlange_();
    extern /* Subroutine */ int zgehrd_(), zlascl_(), zlacpy_();
    static integer minwrk, maxwrk;
    static logical wantvl;
    static doublereal smlnum;
    static integer hswork, irwork;
    extern /* Subroutine */ int zhseqr_(), ztrevc_();
    static logical wantvr;
    extern /* Subroutine */ int zunghr_();
    static integer ihi;
    static doublereal scl;
    static integer ilo;
    static doublereal dum[1], eps;
    static doublecomplex tmp;


/*  -- LAPACK driver routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
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

/*  RWORK   (workspace) DOUBLE PRECISION array, dimension (2*N) */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value. */
/*          > 0:  if INFO = i, the QR algorithm failed to compute all the
*/
/*                eigenvalues, and no eigenvectors have been computed; */
/*                elements and i+1:N of W contain eigenvalues which have
*/
/*                converged. */

/*  =====================================================================
*/

/*     .. Parameters .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Local Arrays .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input arguments */

    /* Parameter adjustments */
    --rwork;
    --work;
    vr_dim1 = *ldvr;
    vr_offset = vr_dim1 + 1;
    vr -= vr_offset;
    vl_dim1 = *ldvl;
    vl_offset = vl_dim1 + 1;
    vl -= vl_offset;
    --w;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    *info = 0;
    wantvl = lsame_(jobvl, "V", 1L, 1L);
    wantvr = lsame_(jobvr, "V", 1L, 1L);
    if (! wantvl && ! lsame_(jobvl, "N", 1L, 1L)) {
        *info = -1;
    } else if (! wantvr && ! lsame_(jobvr, "N", 1L, 1L)) {
        *info = -2;
    } else if (*n < 0) {
        *info = -3;
    } else if (*lda < max(1,*n)) {
        *info = -5;
    } else if (*ldvl < 1 || wantvl && *ldvl < *n) {
        *info = -8;
    } else if (*ldvr < 1 || wantvr && *ldvr < *n) {
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
        maxwrk = *n + *n * ilaenv_(&c__1, "ZGEHRD", " ", n, &c__1, n, &c__0,
                6L, 1L);
        if (! wantvl && ! wantvr) {
/* Computing MAX */
            i__1 = 1, i__2 = *n << 1;
            minwrk = max(i__1,i__2);
/* Computing MAX */
            i__1 = ilaenv_(&c__8, "ZHSEQR", "EN", n, &c__1, n, &c_n1, 6L, 2L);
            maxb = max(i__1,2);
/* Computing MIN */
/* Computing MAX */
            i__3 = 2, i__4 = ilaenv_(&c__4, "ZHSEQR", "EN", n, &c__1, n, &
                    c_n1, 6L, 2L);
            i__1 = min(maxb,*n), i__2 = max(i__3,i__4);
            k = min(i__1,i__2);
/* Computing MAX */
            i__1 = k * (k + 2), i__2 = *n << 1;
            hswork = max(i__1,i__2);
            maxwrk = max(maxwrk,hswork);
        } else {
/* Computing MAX */
            i__1 = 1, i__2 = *n << 1;
            minwrk = max(i__1,i__2);
/* Computing MAX */
            i__1 = maxwrk, i__2 = *n + (*n - 1) * ilaenv_(&c__1, "ZUNGHR",
                    " ", n, &c__1, n, &c_n1, 6L, 1L);
            maxwrk = max(i__1,i__2);
/* Computing MAX */
            i__1 = ilaenv_(&c__8, "ZHSEQR", "SV", n, &c__1, n, &c_n1, 6L, 2L);
            maxb = max(i__1,2);
/* Computing MIN */
/* Computing MAX */
            i__3 = 2, i__4 = ilaenv_(&c__4, "ZHSEQR", "SV", n, &c__1, n, &
                    c_n1, 6L, 2L);
            i__1 = min(maxb,*n), i__2 = max(i__3,i__4);
            k = min(i__1,i__2);
/* Computing MAX */
            i__1 = k * (k + 2), i__2 = *n << 1;
            hswork = max(i__1,i__2);
/* Computing MAX */
            i__1 = max(maxwrk,hswork), i__2 = *n << 1;
            maxwrk = max(i__1,i__2);
        }
        work[1].r = (doublereal) maxwrk, work[1].i = 0.;
    }
    if (*lwork < minwrk) {
        *info = -12;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("ZGEEV ", &i__1, 6L);
        return 0;
    }

/*     Quick return if possible */

    if (*n == 0) {
        return 0;
    }

/*     Get machine constants */

    eps = dlamch_("P", 1L);
    smlnum = dlamch_("S", 1L);
    bignum = 1. / smlnum;
    dlabad_(&smlnum, &bignum);
    smlnum = sqrt(smlnum) / eps;
    bignum = 1. / smlnum;

/*     Scale A if max element outside range [SMLNUM,BIGNUM] */

    anrm = zlange_("M", n, n, &a[a_offset], lda, dum, 1L);
    scalea = FALSE_;
    if (anrm > 0. && anrm < smlnum) {
        scalea = TRUE_;
        cscale = smlnum;
    } else if (anrm > bignum) {
        scalea = TRUE_;
        cscale = bignum;
    }
    if (scalea) {
        zlascl_("G", &c__0, &c__0, &anrm, &cscale, n, n, &a[a_offset], lda, &
                ierr, 1L);
    }

/*     Balance the matrix */
/*     (CWorkspace: none) */
/*     (RWorkspace: need N) */

    ibal = 1;
    zgebal_("B", n, &a[a_offset], lda, &ilo, &ihi, &rwork[ibal], &ierr, 1L);

/*     Reduce to upper Hessenberg form */
/*     (CWorkspace: need 2*N, prefer N+N*NB) */
/*     (RWorkspace: none) */

    itau = 1;
    iwrk = itau + *n;
    i__1 = *lwork - iwrk + 1;
    zgehrd_(n, &ilo, &ihi, &a[a_offset], lda, &work[itau], &work[iwrk], &i__1,
             &ierr);

    if (wantvl) {

/*        Want left eigenvectors */
/*        Copy Householder vectors to VL */

        *side = 'L';
        zlacpy_("L", n, n, &a[a_offset], lda, &vl[vl_offset], ldvl, 1L);

/*        Generate unitary matrix in VL */
/*        (CWorkspace: need 2*N-1, prefer N+(N-1)*NB) */
/*        (RWorkspace: none) */

        i__1 = *lwork - iwrk + 1;
        zunghr_(n, &ilo, &ihi, &vl[vl_offset], ldvl, &work[itau], &work[iwrk],
                 &i__1, &ierr);

/*        Perform QR iteration, accumulating Schur vectors in VL */
/*        (CWorkspace: need 1, prefer HSWORK (see comments) ) */
/*        (RWorkspace: none) */

        iwrk = itau;
        i__1 = *lwork - iwrk + 1;
        zhseqr_("S", "V", n, &ilo, &ihi, &a[a_offset], lda, &w[1], &vl[
                vl_offset], ldvl, &work[iwrk], &i__1, info, 1L, 1L);

        if (wantvr) {

/*           Want left and right eigenvectors */
/*           Copy Schur vectors to VR */

            *side = 'B';
            zlacpy_("F", n, n, &vl[vl_offset], ldvl, &vr[vr_offset], ldvr, 1L)
                    ;
        }

    } else if (wantvr) {

/*        Want right eigenvectors */
/*        Copy Householder vectors to VR */

        *side = 'R';
        zlacpy_("L", n, n, &a[a_offset], lda, &vr[vr_offset], ldvr, 1L);

/*        Generate unitary matrix in VR */
/*        (CWorkspace: need 2*N-1, prefer N+(N-1)*NB) */
/*        (RWorkspace: none) */

        i__1 = *lwork - iwrk + 1;
        zunghr_(n, &ilo, &ihi, &vr[vr_offset], ldvr, &work[itau], &work[iwrk],
                 &i__1, &ierr);

/*        Perform QR iteration, accumulating Schur vectors in VR */
/*        (CWorkspace: need 1, prefer HSWORK (see comments) ) */
/*        (RWorkspace: none) */

        iwrk = itau;
        i__1 = *lwork - iwrk + 1;
        zhseqr_("S", "V", n, &ilo, &ihi, &a[a_offset], lda, &w[1], &vr[
                vr_offset], ldvr, &work[iwrk], &i__1, info, 1L, 1L);

    } else {

/*        Compute eigenvalues only */
/*        (CWorkspace: need 1, prefer HSWORK (see comments) ) */
/*        (RWorkspace: none) */

        iwrk = itau;
        i__1 = *lwork - iwrk + 1;
        zhseqr_("E", "N", n, &ilo, &ihi, &a[a_offset], lda, &w[1], &vr[
                vr_offset], ldvr, &work[iwrk], &i__1, info, 1L, 1L);
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
        ztrevc_(side, "B", select, n, &a[a_offset], lda, &vl[vl_offset], ldvl,
                 &vr[vr_offset], ldvr, n, &nout, &work[iwrk], &rwork[irwork],
                &ierr, 1L, 1L);
    }

    if (wantvl) {

/*        Undo balancing of left eigenvectors */
/*        (CWorkspace: none) */
/*        (RWorkspace: need N) */

        zgebak_("B", "L", n, &ilo, &ihi, &rwork[ibal], n, &vl[vl_offset],
                ldvl, &ierr, 1L, 1L);

/*        Normalize left eigenvectors and make largest component real
*/

        i__1 = *n;
        for (i = 1; i <= i__1; ++i) {
            scl = 1. / dznrm2_(n, &vl[i * vl_dim1 + 1], &c__1);
            zdscal_(n, &scl, &vl[i * vl_dim1 + 1], &c__1);
            i__2 = *n;
            for (k = 1; k <= i__2; ++k) {
                i__3 = k + i * vl_dim1;
/* Computing 2nd power */
                d__1 = vl[i__3].r;
/* Computing 2nd power */
                d__2 = d_imag(&vl[k + i * vl_dim1]);
                rwork[irwork + k - 1] = d__1 * d__1 + d__2 * d__2;
/* L10: */
            }
            k = idamax_(n, &rwork[irwork], &c__1);
            d_cnjg(&z__2, &vl[k + i * vl_dim1]);
            d__1 = sqrt(rwork[irwork + k - 1]);
            z__1.r = z__2.r / d__1, z__1.i = z__2.i / d__1;
            tmp.r = z__1.r, tmp.i = z__1.i;
            zscal_(n, &tmp, &vl[i * vl_dim1 + 1], &c__1);
            i__2 = k + i * vl_dim1;
            i__3 = k + i * vl_dim1;
            d__1 = vl[i__3].r;
            z__1.r = d__1, z__1.i = 0.;
            vl[i__2].r = z__1.r, vl[i__2].i = z__1.i;
/* L20: */
        }
    }

    if (wantvr) {

/*        Undo balancing of right eigenvectors */
/*        (CWorkspace: none) */
/*        (RWorkspace: need N) */

        zgebak_("B", "R", n, &ilo, &ihi, &rwork[ibal], n, &vr[vr_offset],
                ldvr, &ierr, 1L, 1L);

/*        Normalize right eigenvectors and make largest component real
 */

        i__1 = *n;
        for (i = 1; i <= i__1; ++i) {
            scl = 1. / dznrm2_(n, &vr[i * vr_dim1 + 1], &c__1);
            zdscal_(n, &scl, &vr[i * vr_dim1 + 1], &c__1);
            i__2 = *n;
            for (k = 1; k <= i__2; ++k) {
                i__3 = k + i * vr_dim1;
/* Computing 2nd power */
                d__1 = vr[i__3].r;
/* Computing 2nd power */
                d__2 = d_imag(&vr[k + i * vr_dim1]);
                rwork[irwork + k - 1] = d__1 * d__1 + d__2 * d__2;
/* L30: */
            }
            k = idamax_(n, &rwork[irwork], &c__1);
            d_cnjg(&z__2, &vr[k + i * vr_dim1]);
            d__1 = sqrt(rwork[irwork + k - 1]);
            z__1.r = z__2.r / d__1, z__1.i = z__2.i / d__1;
            tmp.r = z__1.r, tmp.i = z__1.i;
            zscal_(n, &tmp, &vr[i * vr_dim1 + 1], &c__1);
            i__2 = k + i * vr_dim1;
            i__3 = k + i * vr_dim1;
            d__1 = vr[i__3].r;
            z__1.r = d__1, z__1.i = 0.;
            vr[i__2].r = z__1.r, vr[i__2].i = z__1.i;
/* L40: */
        }
    }

/*     Undo scaling if necessary */

L50:
    if (scalea) {
        i__1 = *n - *info;
/* Computing MAX */
        i__3 = *n - *info;
        i__2 = max(i__3,1);
        zlascl_("G", &c__0, &c__0, &cscale, &anrm, &i__1, &c__1, &w[*info + 1]
                , &i__2, &ierr, 1L);
        if (*info > 0) {
            i__1 = ilo - 1;
            zlascl_("G", &c__0, &c__0, &cscale, &anrm, &i__1, &c__1, &w[1], n,
                     &ierr, 1L);
        }
    }

    work[1].r = (doublereal) maxwrk, work[1].i = 0.;
    return 0;

/*     End of ZGEEV */

} /* zgeev_ */

