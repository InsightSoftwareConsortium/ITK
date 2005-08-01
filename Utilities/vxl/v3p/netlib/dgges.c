#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Table of constant values */
static integer c__1 = 1;
static integer c__0 = 0;
static integer c_n1 = -1;
static doublereal c_b33 = 0.;
static doublereal c_b34 = 1.;

/* Subroutine */
void dgges_(jobvsl, jobvsr, sort, delctg, n, a, lda, b, ldb,
            sdim, alphar, alphai, beta, vsl, ldvsl, vsr, ldvsr, work, lwork, bwork, info)
const char *jobvsl, *jobvsr, *sort;
logical (*delctg) (doublereal*,doublereal*,doublereal*);
integer *n;
doublereal *a;
integer *lda;
doublereal *b;
integer *ldb, *sdim;
doublereal *alphar, *alphai, *beta, *vsl;
integer *ldvsl;
doublereal *vsr;
integer *ldvsr;
doublereal *work;
integer *lwork;
logical *bwork;
integer *info;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static doublereal anrm, bnrm;
    static integer idum[1], ierr, itau, iwrk;
    static doublereal pvsl, pvsr;
    static integer i;
    static integer ileft, icols;
    static logical cursl, ilvsl, ilvsr;
    static integer irows;
    static logical lst2sl;
    static integer ip;
    static logical ilascl, ilbscl;
    static doublereal safmin;
    static doublereal safmax;
    static doublereal bignum;
    static integer ijobvl, iright, ijobvr;
    static doublereal anrmto, bnrmto;
    static logical lastsl;
    static integer minwrk, maxwrk;
    static doublereal smlnum;
    static logical wantst, lquery;
    static doublereal dif[2];
    static integer ihi, ilo;
    static doublereal eps;


/*  -- LAPACK driver routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DGGES computes for a pair of N-by-N real nonsymmetric matrices (A,B), */
/*  the generalized eigenvalues, the generalized real Schur form (S,T),   */
/*  optionally, the left and/or right matrices of Schur vectors (VSL and  */
/*  VSR). This gives the generalized Schur factorization                  */
/*                                                                        */
/*           (A,B) = ( (VSL)*S*(VSR)**T, (VSL)*T*(VSR)**T )               */
/*                                                                        */
/*  Optionally, it also orders the eigenvalues so that a selected cluster */
/*  of eigenvalues appears in the leading diagonal blocks of the upper    */
/*  quasi-triangular matrix S and the upper triangular matrix T.The       */
/*  leading columns of VSL and VSR then form an orthonormal basis for the */
/*  corresponding left and right eigenspaces (deflating subspaces).       */
/*                                                                        */
/*  (If only the generalized eigenvalues are needed, use the driver       */
/*  DGGEV instead, which is faster.)                                      */
/*                                                                        */
/*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar w   */
/*  or a ratio alpha/beta = w, such that  A - w*B is singular.  It is     */
/*  usually represented as the pair (alpha,beta), as there is a           */
/*  reasonable interpretation for beta=0 or both being zero.              */
/*                                                                        */
/*  A pair of matrices (S,T) is in generalized real Schur form if T is    */
/*  upper triangular with non-negative diagonal and S is block upper      */
/*  triangular with 1-by-1 and 2-by-2 blocks.  1-by-1 blocks correspond   */
/*  to real generalized eigenvalues, while 2-by-2 blocks of S will be     */
/*  "standardized" by making the corresponding elements of T have the     */
/*  form:                                                                 */
/*          [  a  0  ]                                                    */
/*          [  0  b  ]                                                    */
/*                                                                        */
/*  and the pair of corresponding 2-by-2 blocks in S and T will have a    */
/*  complex conjugate pair of generalized eigenvalues.                    */
/*                                                                        */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  JOBVSL  (input) CHARACTER*1                                           */
/*          = 'N':  do not compute the left Schur vectors;                */
/*          = 'V':  compute the left Schur vectors.                       */
/*                                                                        */
/*  JOBVSR  (input) CHARACTER*1                                           */
/*          = 'N':  do not compute the right Schur vectors;               */
/*          = 'V':  compute the right Schur vectors.                      */
/*                                                                        */
/*  SORT    (input) CHARACTER*1                                           */
/*          Specifies whether or not to order the eigenvalues on the      */
/*          diagonal of the generalized Schur form.                       */
/*          = 'N':  Eigenvalues are not ordered;                          */
/*          = 'S':  Eigenvalues are ordered (see DELZTG);                 */
/*                                                                        */
/*  DELZTG  (input) LOGICAL FUNCTION of three DOUBLE PRECISION arguments  */
/*          DELZTG must be declared EXTERNAL in the calling subroutine.   */
/*          If SORT = 'N', DELZTG is not referenced.                      */
/*          If SORT = 'S', DELZTG is used to select eigenvalues to sort   */
/*          to the top left of the Schur form.                            */
/*          An eigenvalue (ALPHAR(j)+ALPHAI(j))/BETA(j) is selected if    */
/*          DELZTG(ALPHAR(j),ALPHAI(j),BETA(j)) is true; i.e. if either   */
/*          one of a complex conjugate pair of eigenvalues is selected,   */
/*          then both complex eigenvalues are selected.                   */
/*                                                                        */
/*          Note that in the ill-conditioned case, a selected complex     */
/*          eigenvalue may no longer satisfy DELZTG(ALPHAR(j),ALPHAI(j),  */
/*          BETA(j)) = .TRUE. after ordering. INFO is to be set to N+2    */
/*          in this case.                                                 */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the matrices A, B, VSL, and VSR.  N >= 0.        */
/*                                                                        */
/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)     */
/*          On entry, the first of the pair of matrices.                  */
/*          On exit, A has been overwritten by its generalized Schur      */
/*          form S.                                                       */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of A.  LDA >= max(1,N).                 */
/*                                                                        */
/*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)     */
/*          On entry, the second of the pair of matrices.                 */
/*          On exit, B has been overwritten by its generalized Schur      */
/*          form T.                                                       */
/*                                                                        */
/*  LDB     (input) INTEGER                                               */
/*          The leading dimension of B.  LDB >= max(1,N).                 */
/*                                                                        */
/*  SDIM    (output) INTEGER                                              */
/*          If SORT = 'N', SDIM = 0.                                      */
/*          If SORT = 'S', SDIM = number of eigenvalues (after sorting)   */
/*          for which DELZTG is true.  (Complex conjugate pairs for which */
/*          DELZTG is true for either eigenvalue count as 2.)             */
/*                                                                        */
/*  ALPHAR  (output) DOUBLE PRECISION array, dimension (N)                */
/*  ALPHAI  (output) DOUBLE PRECISION array, dimension (N)                */
/*  BETA    (output) DOUBLE PRECISION array, dimension (N)                */
/*          On exit, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j=1,...,N, will   */
/*          be the generalized eigenvalues.  ALPHAR(j) + ALPHAI(j)*i,     */
/*          and  BETA(j),j=1,...,N are the diagonals of the complex Schur */
/*          form (S,T) that would result if the 2-by-2 diagonal blocks of */
/*          the real Schur form of (A,B) were further reduced to          */
/*          triangular form using 2-by-2 complex unitary transformations. */
/*          If ALPHAI(j) is zero, then the j-th eigenvalue is real; if    */
/*          positive, then the j-th and (j+1)-st eigenvalues are a        */
/*          complex conjugate pair, with ALPHAI(j+1) negative.            */
/*                                                                        */
/*          Note: the quotients ALPHAR(j)/BETA(j) and ALPHAI(j)/BETA(j)   */
/*          may easily over- or underflow, and BETA(j) may even be zero.  */
/*          Thus, the user should avoid naively computing the ratio.      */
/*          However, ALPHAR and ALPHAI will be always less than and       */
/*          usually comparable with norm(A) in magnitude, and BETA always */
/*          less than and usually comparable with norm(B).                */
/*                                                                        */
/*  VSL     (output) DOUBLE PRECISION array, dimension (LDVSL,N)          */
/*          If JOBVSL = 'V', VSL will contain the left Schur vectors.     */
/*          Not referenced if JOBVSL = 'N'.                               */
/*                                                                        */
/*  LDVSL   (input) INTEGER                                               */
/*          The leading dimension of the matrix VSL. LDVSL >=1, and       */
/*          if JOBVSL = 'V', LDVSL >= N.                                  */
/*                                                                        */
/*  VSR     (output) DOUBLE PRECISION array, dimension (LDVSR,N)          */
/*          If JOBVSR = 'V', VSR will contain the right Schur vectors.    */
/*          Not referenced if JOBVSR = 'N'.                               */
/*                                                                        */
/*  LDVSR   (input) INTEGER                                               */
/*          The leading dimension of the matrix VSR. LDVSR >= 1, and      */
/*          if JOBVSR = 'V', LDVSR >= N.                                  */
/*                                                                        */
/*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)  */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.      */
/*                                                                        */
/*  LWORK   (input) INTEGER                                               */
/*          The dimension of the array WORK.  LWORK >= 8*N+16.            */
/*                                                                        */
/*          If LWORK = -1, then a workspace query is assumed; the routine */
/*          only calculates the optimal size of the WORK array, returns   */
/*          this value as the first entry of the WORK array, and no error */
/*          message related to LWORK is issued by XERBLA.                 */
/*                                                                        */
/*  BWORK   (workspace) LOGICAL array, dimension (N)                      */
/*          Not referenced if SORT = 'N'.                                 */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0:  successful exit                                         */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value.   */
/*          = 1,...,N:                                                    */
/*                The QZ iteration failed.  (A,B) are not in Schur        */
/*                form, but ALPHAR(j), ALPHAI(j), and BETA(j) should      */
/*                be correct for j=INFO+1,...,N.                          */
/*          > N:  =N+1: other than QZ iteration failed in DHGEQZ.         */
/*                =N+2: after reordering, roundoff changed values of      */
/*                      some complex eigenvalues so that leading          */
/*                      eigenvalues in the Generalized Schur form no      */
/*                      longer satisfy DELZTG=.TRUE.  This could also     */
/*                      be caused due to scaling.                         */
/*                =N+3: reordering failed in DTGSEN.                      */
/*                                                                        */
/*  ===================================================================== */

/*     Decode the input arguments */

    if (lsame_(jobvsl, "N")) {
        ijobvl = 1;
        ilvsl = FALSE_;
    } else if (lsame_(jobvsl, "V")) {
        ijobvl = 2;
        ilvsl = TRUE_;
    } else {
        ijobvl = -1;
        ilvsl = FALSE_;
    }

    if (lsame_(jobvsr, "N")) {
        ijobvr = 1;
        ilvsr = FALSE_;
    } else if (lsame_(jobvsr, "V")) {
        ijobvr = 2;
        ilvsr = TRUE_;
    } else {
        ijobvr = -1;
        ilvsr = FALSE_;
    }

    wantst = lsame_(sort, "S");

/*     Test the input arguments */

    *info = 0;
    lquery = *lwork == -1;
    if (ijobvl <= 0) {
        *info = -1;
    } else if (ijobvr <= 0) {
        *info = -2;
    } else if (! wantst && ! lsame_(sort, "N")) {
        *info = -3;
    } else if (*n < 0) {
        *info = -5;
    } else if (*lda < max(1,*n)) {
        *info = -7;
    } else if (*ldb < max(1,*n)) {
        *info = -9;
    } else if (*ldvsl < 1 || (ilvsl && *ldvsl < *n)) {
        *info = -15;
    } else if (*ldvsr < 1 || (ilvsr && *ldvsr < *n)) {
        *info = -17;
    }

/*     Compute workspace */
/*      (Note: Comments in the code beginning "Workspace:" describe the */
/*       minimal amount of workspace needed at that point in the code, */
/*       as well as the preferred amount for good performance. */
/*       NB refers to the optimal block size for the immediately */
/*       following subroutine, as returned by ILAENV.) */

    minwrk = 1;
    if (*info == 0 && (*lwork >= 1 || lquery)) {
        minwrk = (*n + 1) * 7 + 16;
        maxwrk = (*n + 1) * 7 + *n * ilaenv_(&c__1, "DGEQRF", " ", n, &c__1, n, &c__0) + 16;
        if (ilvsl) {
            i__1 = (*n + 1) * 7 + *n * ilaenv_(&c__1, "DORGQR", " ", n, &c__1, n, &c_n1);
            maxwrk = max(maxwrk,i__1);
        }
        work[0] = (doublereal) maxwrk;
    }

    if (*lwork < minwrk && ! lquery) {
        *info = -19;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("DGGES ", &i__1);
        return;
    } else if (lquery) {
        return;
    }

/*     Quick return if possible */

    if (*n == 0) {
        *sdim = 0;
        return;
    }

/*     Get machine constants */

    eps = dlamch_("P");
    safmin = dlamch_("S");
    safmax = 1. / safmin;
    dlabad_(&safmin, &safmax);
    smlnum = sqrt(safmin) / eps;
    bignum = 1. / smlnum;

/*     Scale A if max element outside range [SMLNUM,BIGNUM] */

    anrm = dlange_("M", n, n, a, lda, work);
    ilascl = FALSE_;
    if (anrm > 0. && anrm < smlnum) {
        anrmto = smlnum;
        ilascl = TRUE_;
    } else if (anrm > bignum) {
        anrmto = bignum;
        ilascl = TRUE_;
    }
    if (ilascl) {
        dlascl_("G", &c__0, &c__0, &anrm, &anrmto, n, n, a, lda, &ierr);
    }

/*     Scale B if max element outside range [SMLNUM,BIGNUM] */

    bnrm = dlange_("M", n, n, b, ldb, work);
    ilbscl = FALSE_;
    if (bnrm > 0. && bnrm < smlnum) {
        bnrmto = smlnum;
        ilbscl = TRUE_;
    } else if (bnrm > bignum) {
        bnrmto = bignum;
        ilbscl = TRUE_;
    }
    if (ilbscl) {
        dlascl_("G", &c__0, &c__0, &bnrm, &bnrmto, n, n, b, ldb, &ierr);
    }

/*     Permute the matrix to make it more nearly triangular */
/*     (Workspace: need 6*N + 2*N space for storing balancing factors) */

    ileft = 0;
    iright = *n;
    iwrk = iright + *n;
    dggbal_("P", n, a, lda, b, ldb, &ilo, &ihi, &work[ileft], &work[iright], &work[iwrk], &ierr);
    --ilo;

/*     Reduce B to triangular form (QR decomposition of B) */
/*     (Workspace: need N, prefer N*NB) */

    irows = ihi - ilo;
    icols = *n - ilo;
    itau = iwrk;
    iwrk = itau + irows;
    i__1 = *lwork - iwrk;
    dgeqrf_(&irows, &icols, &b[ilo + ilo * *ldb], ldb, &work[itau], &work[iwrk], &i__1, &ierr);

/*     Apply the orthogonal transformation to matrix A */
/*     (Workspace: need N, prefer N*NB) */

    i__1 = *lwork - iwrk;
    dormqr_("L", "T", &irows, &icols, &irows, &b[ilo + ilo * *ldb], ldb, &work[itau],
            &a[ilo + ilo * *lda], lda, &work[iwrk], &i__1, &ierr);

/*     Initialize VSL */
/*     (Workspace: need N, prefer N*NB) */

    if (ilvsl) {
        dlaset_("Full", n, n, &c_b33, &c_b34, vsl, ldvsl);
        i__1 = irows - 1;
        dlacpy_("L", &i__1, &i__1, &b[ilo + 1 + ilo * *ldb], ldb, &vsl[ilo + 1 + ilo * *ldvsl], ldvsl);
        i__1 = *lwork - iwrk;
        dorgqr_(&irows, &irows, &irows, &vsl[ilo + ilo * *ldvsl], ldvsl, &work[itau], &work[iwrk], &i__1, &ierr);
    }

/*     Initialize VSR */

    if (ilvsr) {
        dlaset_("Full", n, n, &c_b33, &c_b34, vsr, ldvsr);
    }

/*     Reduce to generalized Hessenberg form */
/*     (Workspace: none needed) */

    ++ilo;
    dgghrd_(jobvsl, jobvsr, n, &ilo, &ihi, a, lda, b, ldb, vsl, ldvsl, vsr, ldvsr, &ierr);

/*     Perform QZ algorithm, computing Schur vectors if desired */
/*     (Workspace: need N) */

    iwrk = itau;
    i__1 = *lwork - iwrk;
    dhgeqz_("S", jobvsl, jobvsr, n, &ilo, &ihi, a, lda, b, ldb, alphar, alphai, beta,
            vsl, ldvsl, vsr, ldvsr, &work[iwrk], &i__1, &ierr);
    if (ierr != 0) {
        if (ierr > 0 && ierr <= *n) {
            *info = ierr;
        } else if (ierr > *n && ierr <= *n << 1) {
            *info = ierr - *n;
        } else {
            *info = *n + 1;
        }
        work[0] = (doublereal) maxwrk;
        return;
    }

/*     Sort eigenvalues ALPHA/BETA if desired */
/*     (Workspace: need 4*N+16 ) */

    *sdim = 0;
    if (wantst) {

/*        Undo scaling on eigenvalues before DELZTGing */

        if (ilascl) {
            dlascl_("G", &c__0, &c__0, &anrmto, &anrm, n, &c__1, alphar, n, &ierr);
            dlascl_("G", &c__0, &c__0, &anrmto, &anrm, n, &c__1, alphai, n, &ierr);
        }
        if (ilbscl) {
            dlascl_("G", &c__0, &c__0, &bnrmto, &bnrm, n, &c__1, beta, n, &ierr);
        }

/*        Select eigenvalues */

        for (i = 0; i < *n; ++i) {
            bwork[i] = (*delctg)(&alphar[i], &alphai[i], &beta[i]);
        }

        i__1 = *lwork - iwrk;
        dtgsen_(&c__0, &ilvsl, &ilvsr, bwork, n, a, lda, b, ldb, alphar, alphai, beta,
                vsl, ldvsl, vsr, ldvsr, sdim, &pvsl, &pvsr, dif, &work[iwrk], &i__1, idum, &c__1, &ierr);
        if (ierr == 1) {
            *info = *n + 3;
        }
    }

/*     Apply back-permutation to VSL and VSR */
/*     (Workspace: none needed) */

    if (ilvsl) {
        dggbak_("P", "L", n, &ilo, &ihi, &work[ileft], &work[iright], n, vsl, ldvsl, &ierr);
    }

    if (ilvsr) {
        dggbak_("P", "R", n, &ilo, &ihi, &work[ileft], &work[iright], n, vsr, ldvsr, &ierr);
    }

/*     Check if unscaling would cause over/underflow, if so, rescale */
/*     (ALPHAR(I),ALPHAI(I),BETA(I)) so BETA(I) is on the order of */
/*     B(I,I) and ALPHAR(I) and ALPHAI(I) are on the order of A(I,I) */

    if (ilascl) {
        for (i = 0; i < *n; ++i) {
            if (alphai[i] != 0.) {
                if (alphar[i] / safmax > anrmto / anrm || safmin / alphar[i] > anrm / anrmto) {
                    work[0] = abs(a[i + i * *lda] / alphar[i]);
                    beta[i] *= work[0];
                    alphar[i] *= work[0];
                    alphai[i] *= work[0];
                } else if (alphai[i] / safmax > anrmto / anrm || safmin / alphai[i] > anrm / anrmto) {
                    work[0] = abs(a[i + (i+1) * *lda] / alphai[i]);
                    beta[i] *= work[0];
                    alphar[i] *= work[0];
                    alphai[i] *= work[0];
                }
            }
        }
    }

    if (ilbscl) {
        for (i = 0; i < *n; ++i) {
            if (alphai[i] != 0.) {
                if (beta[i] / safmax > bnrmto / bnrm || safmin / beta[i] > bnrm / bnrmto) {
                    work[0] = abs(b[i + i * *ldb] / beta[i]);
                    beta[i] *= work[0];
                    alphar[i] *= work[0];
                    alphai[i] *= work[0];
                }
            }
        }
    }

/*     Undo scaling */

    if (ilascl) {
        dlascl_("H", &c__0, &c__0, &anrmto, &anrm, n, n, a, lda, &ierr);
        dlascl_("G", &c__0, &c__0, &anrmto, &anrm, n, &c__1, alphar, n, &ierr);
        dlascl_("G", &c__0, &c__0, &anrmto, &anrm, n, &c__1, alphai, n, &ierr);
    }

    if (ilbscl) {
        dlascl_("U", &c__0, &c__0, &bnrmto, &bnrm, n, n, b, ldb, &ierr);
        dlascl_("G", &c__0, &c__0, &bnrmto, &bnrm, n, &c__1, beta, n, &ierr);
    }

    if (wantst) {

/*        Check if reordering is correct */

        lastsl = TRUE_;
        lst2sl = TRUE_;
        *sdim = 0;
        ip = 0;
        for (i = 0; i < *n; ++i) {
            cursl = (*delctg)(&alphar[i], &alphai[i], &beta[i]);
            if (alphai[i] == 0.) {
                if (cursl) {
                    ++(*sdim);
                }
                ip = 0;
                if (cursl && ! lastsl) {
                    *info = *n + 2;
                }
            } else {
                if (ip == 1) {

/*                 Last eigenvalue of conjugate pair */

                    cursl = cursl || lastsl;
                    lastsl = cursl;
                    if (cursl) {
                        *sdim += 2;
                    }
                    ip = -1;
                    if (cursl && ! lst2sl) {
                        *info = *n + 2;
                    }
                } else {

/*                 First eigenvalue of conjugate pair */

                    ip = 1;
                }
            }
            lst2sl = lastsl;
            lastsl = cursl;
        }
    }
    work[0] = (doublereal) maxwrk;

} /* dgges_ */
