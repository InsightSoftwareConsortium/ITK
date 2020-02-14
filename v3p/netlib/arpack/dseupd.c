/* arpack/dseupd.f -- translated by f2c (version 20090411).
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

/* Common Block Declarations */

/*Extern struct { */
/*  integer logfil, ndigit, mgetv0, msaupd, msaup2, msaitr, mseigt, msapps, */
/*          msgets, mseupd, mnaupd, mnaup2, mnaitr, mneigh, mnapps, mngets, */
/*          mneupd, mcaupd, mcaup2, mcaitr, mceigh, mcapps, mcgets, mceupd; */
/*} debug_; */

/*#define debug_1 debug_ */

/* Table of constant values */

static doublereal c_b21 = .66666666666666663;
static integer c__1 = 1;
static logical c_true = TRUE_;
static doublereal c_b103 = 1.;

/* \BeginDoc */

/* \Name: dseupd */

/* \Description: */

/*  This subroutine returns the converged approximations to eigenvalues */
/*  of A*z = lambda*B*z and (optionally): */

/*      (1) the corresponding approximate eigenvectors, */

/*      (2) an orthonormal (Lanczos) basis for the associated approximate */
/*          invariant subspace, */

/*      (3) Both. */

/*  There is negligible additional cost to obtain eigenvectors.  An orthonormal */
/*  (Lanczos) basis is always computed.  There is an additional storage cost */
/*  of n*nev if both are requested (in this case a separate array Z must be */
/*  supplied). */

/*  These quantities are obtained from the Lanczos factorization computed */
/*  by DSAUPD for the linear operator OP prescribed by the MODE selection */
/*  (see IPARAM(7) in DSAUPD documentation.)  DSAUPD must be called before */
/*  this routine is called. These approximate eigenvalues and vectors are */
/*  commonly called Ritz values and Ritz vectors respectively.  They are */
/*  referred to as such in the comments that follow.   The computed orthonormal */
/*  basis for the invariant subspace corresponding to these Ritz values is */
/*  referred to as a Lanczos basis. */

/*  See documentation in the header of the subroutine DSAUPD for a definition */
/*  of OP as well as other terms and the relation of computed Ritz values */
/*  and vectors of OP with respect to the given problem  A*z = lambda*B*z. */

/*  The approximate eigenvalues of the original problem are returned in */
/*  ascending algebraic order.  The user may elect to call this routine */
/*  once for each desired Ritz vector and store it peripherally if desired. */
/*  There is also the option of computing a selected set of these vectors */
/*  with a single call. */

/* \Usage: */
/*  call dseupd */
/*     ( RVEC, HOWMNY, SELECT, D, Z, LDZ, SIGMA, BMAT, N, WHICH, NEV, TOL, */
/*       RESID, NCV, V, LDV, IPARAM, IPNTR, WORKD, WORKL, LWORKL, INFO ) */

/*  RVEC    LOGICAL  (INPUT) */
/*          Specifies whether Ritz vectors corresponding to the Ritz value */
/*          approximations to the eigenproblem A*z = lambda*B*z are computed. */

/*             RVEC = .FALSE.     Compute Ritz values only. */

/*             RVEC = .TRUE.      Compute Ritz vectors. */

/*  HOWMNY  Character*1  (INPUT) */
/*          Specifies how many Ritz vectors are wanted and the form of Z */
/*          the matrix of Ritz vectors. See remark 1 below. */
/*          = 'A': compute NEV Ritz vectors; */
/*          = 'S': compute some of the Ritz vectors, specified */
/*                 by the logical array SELECT. */

/*  SELECT  Logical array of dimension NEV.  (INPUT) */
/*          If HOWMNY = 'S', SELECT specifies the Ritz vectors to be */
/*          computed. To select the Ritz vector corresponding to a */
/*          Ritz value D(j), SELECT(j) must be set to .TRUE.. */
/*          If HOWMNY = 'A' , SELECT is not referenced. */

/*  D       Double precision array of dimension NEV.  (OUTPUT) */
/*          On exit, D contains the Ritz value approximations to the */
/*          eigenvalues of A*z = lambda*B*z. The values are returned */
/*          in ascending order. If IPARAM(7) = 3,4,5 then D represents */
/*          the Ritz values of OP computed by dsaupd transformed to */
/*          those of the original eigensystem A*z = lambda*B*z. If */
/*          IPARAM(7) = 1,2 then the Ritz values of OP are the same */
/*          as the those of A*z = lambda*B*z. */

/*  Z       Double precision N by NEV array if HOWMNY = 'A'.  (OUTPUT) */
/*          On exit, Z contains the B-orthonormal Ritz vectors of the */
/*          eigensystem A*z = lambda*B*z corresponding to the Ritz */
/*          value approximations. */
/*          If  RVEC = .FALSE. then Z is not referenced. */
/*          NOTE: The array Z may be set equal to first NEV columns of the */
/*          Arnoldi/Lanczos basis array V computed by DSAUPD. */

/*  LDZ     Integer.  (INPUT) */
/*          The leading dimension of the array Z.  If Ritz vectors are */
/*          desired, then  LDZ .ge.  max( 1, N ).  In any case,  LDZ .ge. 1. */

/*  SIGMA   Double precision  (INPUT) */
/*          If IPARAM(7) = 3,4,5 represents the shift. Not referenced if */
/*          IPARAM(7) = 1 or 2. */


/*  **** The remaining arguments MUST be the same as for the   **** */
/*  **** call to DNAUPD that was just completed.               **** */

/*  NOTE: The remaining arguments */

/*           BMAT, N, WHICH, NEV, TOL, RESID, NCV, V, LDV, IPARAM, IPNTR, */
/*           WORKD, WORKL, LWORKL, INFO */

/*         must be passed directly to DSEUPD following the last call */
/*         to DSAUPD.  These arguments MUST NOT BE MODIFIED between */
/*         the the last call to DSAUPD and the call to DSEUPD. */

/*  Two of these parameters (WORKL, INFO) are also output parameters: */

/*  WORKL   Double precision work array of length LWORKL.  (OUTPUT/WORKSPACE) */
/*          WORKL(1:4*ncv) contains information obtained in */
/*          dsaupd.  They are not changed by dseupd. */
/*          WORKL(4*ncv+1:ncv*ncv+8*ncv) holds the */
/*          untransformed Ritz values, the computed error estimates, */
/*          and the associated eigenvector matrix of H. */

/*          Note: IPNTR(8:10) contains the pointer into WORKL for addresses */
/*          of the above information computed by dseupd. */
/*          ------------------------------------------------------------- */
/*          IPNTR(8): pointer to the NCV RITZ values of the original system. */
/*          IPNTR(9): pointer to the NCV corresponding error bounds. */
/*          IPNTR(10): pointer to the NCV by NCV matrix of eigenvectors */
/*                     of the tridiagonal matrix T. Only referenced by */
/*                     dseupd if RVEC = .TRUE. See Remarks. */
/*          ------------------------------------------------------------- */

/*  INFO    Integer.  (OUTPUT) */
/*          Error flag on output. */
/*          =  0: Normal exit. */
/*          = -1: N must be positive. */
/*          = -2: NEV must be positive. */
/*          = -3: NCV must be greater than NEV and less than or equal to N. */
/*          = -5: WHICH must be one of 'LM', 'SM', 'LA', 'SA' or 'BE'. */
/*          = -6: BMAT must be one of 'I' or 'G'. */
/*          = -7: Length of private work WORKL array is not sufficient. */
/*          = -8: Error return from trid. eigenvalue calculation; */
/*                Information error from LAPACK routine dsteqr. */
/*          = -9: Starting vector is zero. */
/*          = -10: IPARAM(7) must be 1,2,3,4,5. */
/*          = -11: IPARAM(7) = 1 and BMAT = 'G' are incompatible. */
/*          = -12: NEV and WHICH = 'BE' are incompatible. */
/*          = -14: DSAUPD did not find any eigenvalues to sufficient */
/*                 accuracy. */
/*          = -15: HOWMNY must be one of 'A' or 'S' if RVEC = .true. */
/*          = -16: HOWMNY = 'S' not yet implemented */

/* \BeginLib */

/* \References: */
/*  1. D.C. Sorensen, "Implicit Application of Polynomial Filters in */
/*     a k-Step Arnoldi Method", SIAM J. Matr. Anal. Apps., 13 (1992), */
/*     pp 357-385. */
/*  2. R.B. Lehoucq, "Analysis and Implementation of an Implicitly */
/*     Restarted Arnoldi Iteration", Rice University Technical Report */
/*     TR95-13, Department of Computational and Applied Mathematics. */
/*  3. B.N. Parlett, "The Symmetric Eigenvalue Problem". Prentice-Hall, */
/*     1980. */
/*  4. B.N. Parlett, B. Nour-Omid, "Towards a Black Box Lanczos Program", */
/*     Computer Physics Communications, 53 (1989), pp 169-179. */
/*  5. B. Nour-Omid, B.N. Parlett, T. Ericson, P.S. Jensen, "How to */
/*     Implement the Spectral Transformation", Math. Comp., 48 (1987), */
/*     pp 663-673. */
/*  6. R.G. Grimes, J.G. Lewis and H.D. Simon, "A Shifted Block Lanczos */
/*     Algorithm for Solving Sparse Symmetric Generalized Eigenproblems", */
/*     SIAM J. Matr. Anal. Apps.,  January (1993). */
/*  7. L. Reichel, W.B. Gragg, "Algorithm 686: FORTRAN Subroutines */
/*     for Updating the QR decomposition", ACM TOMS, December 1990, */
/*     Volume 16 Number 4, pp 369-377. */

/* \Remarks */
/*  1. The converged Ritz values are always returned in increasing */
/*     (algebraic) order. */

/*  2. Currently only HOWMNY = 'A' is implemented. It is included at this */
/*     stage for the user who wants to incorporate it. */

/* \Routines called: */
/*     dsesrt  ARPACK routine that sorts an array X, and applies the */
/*             corresponding permutation to a matrix A. */
/*     dsortr  dsortr  ARPACK sorting routine. */
/*     dgeqr2  LAPACK routine that computes the QR factorization of */
/*             a matrix. */
/*     dlacpy  LAPACK matrix copy routine. */
/*     dlamch  LAPACK routine that determines machine constants. */
/*     dorm2r  LAPACK routine that applies an orthogonal matrix in */
/*             factored form. */
/*     dsteqr  LAPACK routine that computes eigenvalues and eigenvectors */
/*             of a tridiagonal matrix. */
/*     dger    Level 2 BLAS rank one update to a matrix. */
/*     dcopy   Level 1 BLAS that copies one vector to another . */
/*     dnrm2   Level 1 BLAS that computes the norm of a vector. */
/*     dscal   Level 1 BLAS that scales a vector. */
/*     dswap   Level 1 BLAS that swaps the contents of two vectors. */
/* \Authors */
/*     Danny Sorensen               Phuong Vu */
/*     Richard Lehoucq              CRPC / Rice University */
/*     Chao Yang                    Houston, Texas */
/*     Dept. of Computational & */
/*     Applied Mathematics */
/*     Rice University */
/*     Houston, Texas */

/* \Revision history: */
/*     12/15/93: Version ' 2.1' */

/* \SCCS Information: @(#) */
/* FILE: seupd.F   SID: 2.7   DATE OF SID: 8/27/96   RELEASE: 2 */

/* \EndLib */

/* ----------------------------------------------------------------------- */
/*<        >*/
/* Subroutine */ int dseupd_(logical *rvec, char *howmny, logical *select,
        doublereal *d__, doublereal *z__, integer *ldz, doublereal *sigma,
        char *bmat, integer *n, char *which, integer *nev, doublereal *tol,
        doublereal *resid, integer *ncv, doublereal *v, integer *ldv, integer
        *iparam, integer *ipntr, doublereal *workd, doublereal *workl,
        integer *lworkl, integer *info, ftnlen howmny_len, ftnlen bmat_len,
        ftnlen which_len)
{
    /* System generated locals */
    integer v_dim1, v_offset, z_dim1, z_offset, i__1;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double pow_dd(doublereal *, doublereal *);

    /* Local variables */
    integer j, k, ih, iq, iw;
/*  doublereal kv[2]; */
    integer ibd, ihb, ihd, ldh, ilg, ldq, ism, irz;
    extern /* Subroutine */ int dger_(integer *, integer *, doublereal *,
            doublereal *, integer *, doublereal *, integer *, doublereal *,
            integer *);
    integer mode;
    doublereal eps23;
    integer ierr;
    doublereal temp;
    integer next;
    char type__[6];
    integer ritz;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *);
    logical reord;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *,
            doublereal *, integer *);
    integer nconv;
    doublereal rnorm;
    extern /* Subroutine */ int dgeqr2_(integer *, integer *, doublereal *,
            integer *, doublereal *, doublereal *, integer *);
    doublereal bnorm2;
    extern /* Subroutine */ int dorm2r_(char *, char *, integer *, integer *,
            integer *, doublereal *, integer *, doublereal *, doublereal *,
            integer *, doublereal *, integer *, ftnlen, ftnlen);
    doublereal thres1, thres2;
    extern doublereal dlamch_(char *, ftnlen);
    integer bounds, /* msglvl, */ ktrord;
    extern /* Subroutine */ int dlacpy_(char *, integer *, integer *,
            doublereal *, integer *, doublereal *, integer *, ftnlen),
            dsesrt_(char *, logical *, integer *, doublereal *, integer *,
            doublereal *, integer *, ftnlen), dsteqr_(char *, integer *,
            doublereal *, doublereal *, doublereal *, integer *, doublereal *,
             integer *, ftnlen), dsortr_(char *, logical *, integer *,
            doublereal *, doublereal *, ftnlen);
    doublereal tempbnd;
    integer leftptr, rghtptr;


/*     %----------------------------------------------------% */
/*     | Include files for debugging and timing information | */
/*     %----------------------------------------------------% */

/*<       include   'debug.h' >*/
/*<       include   'stat.h' >*/

/* \SCCS Information: @(#) */
/* FILE: debug.h   SID: 2.3   DATE OF SID: 11/16/95   RELEASE: 2 */

/*     %---------------------------------% */
/*     | See debug.doc for documentation | */
/*     %---------------------------------% */
/*<        >*/
/*<       character  bmat, howmny, which*2 >*/

/*     %------------------% */
/*     | Scalar Arguments | */
/*     %------------------% */

/*     %--------------------------------% */
/*     | See stat.doc for documentation | */
/*     %--------------------------------% */

/* \SCCS Information: @(#) */
/* FILE: stat.h   SID: 2.2   DATE OF SID: 11/16/95   RELEASE: 2 */

/*<       save       t0, t1, t2, t3, t4, t5 >*/

/*<       integer    nopx, nbx, nrorth, nitref, nrstrt >*/
/*<        >*/
/*<        >*/
/*<       logical    rvec, select(ncv) >*/
/*<       integer    info, ldz, ldv, lworkl, n, ncv, nev >*/
/*<        >*/

/*     %-----------------% */
/*     | Array Arguments | */
/*     %-----------------% */

/*<       integer    iparam(7), ipntr(11) >*/
/*<        >*/

/*     %------------% */
/*     | Parameters | */
/*     %------------% */

/*<        >*/
/*<       parameter (one = 1.0D+0, zero = 0.0D+0) >*/

/*     %---------------% */
/*     | Local Scalars | */
/*     %---------------% */

/*<       character  type*6 >*/
/*<        >*/
/*<        >*/
/*<       logical    reord >*/

/*     %--------------% */
/*     | Local Arrays | */
/*     %--------------% */

/*<        >*/

/*     %----------------------% */
/*     | External Subroutines | */
/*     %----------------------% */

/*<        >*/

/*     %--------------------% */
/*     | External Functions | */
/*     %--------------------% */

/*<        >*/
/*<       external   dnrm2, dlamch >*/

/*     %---------------------% */
/*     | Intrinsic Functions | */
/*     %---------------------% */

/*<       intrinsic    min >*/

/*     %-----------------------% */
/*     | Executable Statements | */
/*     %-----------------------% */

/*     %------------------------% */
/*     | Set default parameters | */
/*     %------------------------% */

/*<       msglvl = mseupd >*/
    /* Parameter adjustments */
    --workd;
    --resid;
    z_dim1 = *ldz;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --d__;
    --select;
    v_dim1 = *ldv;
    v_offset = 1 + v_dim1;
    v -= v_offset;
    --iparam;
    --ipntr;
    --workl;

    /* Function Body */
/*  msglvl = debug_1.mseupd; */
/*<       mode = iparam(7) >*/
    mode = iparam[7];
/*<       nconv = iparam(5) >*/
    nconv = iparam[5];
/*<       info = 0 >*/
    *info = 0;

/*     %--------------% */
/*     | Quick return | */
/*     %--------------% */

/*<       if (nconv .eq. 0) go to 9000 >*/
    if (nconv == 0) {
        goto L9000;
    }
/*<       ierr = 0 >*/
    ierr = 0;

/*<       if (nconv .le. 0)                        ierr = -14  >*/
    if (nconv <= 0) {
        ierr = -14;
    }
/*<       if (n .le. 0)                            ierr = -1 >*/
    if (*n <= 0) {
        ierr = -1;
    }
/*<       if (nev .le. 0)                          ierr = -2 >*/
    if (*nev <= 0) {
        ierr = -2;
    }
/*<       if (ncv .le. nev .or.  ncv .gt. n)       ierr = -3 >*/
    if (*ncv <= *nev || *ncv > *n) {
        ierr = -3;
    }
/*<        >*/
    if (s_cmp(which, "LM", (ftnlen)2, (ftnlen)2) != 0 && s_cmp(which, "SM", (
            ftnlen)2, (ftnlen)2) != 0 && s_cmp(which, "LA", (ftnlen)2, (
            ftnlen)2) != 0 && s_cmp(which, "SA", (ftnlen)2, (ftnlen)2) != 0 &&
             s_cmp(which, "BE", (ftnlen)2, (ftnlen)2) != 0) {
        ierr = -5;
    }
/*<       if (bmat .ne. 'I' .and. bmat .ne. 'G')   ierr = -6 >*/
    if (*(unsigned char *)bmat != 'I' && *(unsigned char *)bmat != 'G') {
        ierr = -6;
    }
/*<        >*/
    if (*(unsigned char *)howmny != 'A' && *(unsigned char *)howmny != 'P' &&
            *(unsigned char *)howmny != 'S' && *rvec) {
        ierr = -15;
    }
/*<       if (rvec .and. howmny .eq. 'S')           ierr = -16 >*/
    if (*rvec && *(unsigned char *)howmny == 'S') {
        ierr = -16;
    }

/*<       if (rvec .and. lworkl .lt. ncv**2+8*ncv) ierr = -7 >*/
/* Computing 2nd power */
    i__1 = *ncv;
    if (*rvec && *lworkl < i__1 * i__1 + (*ncv << 3)) {
        ierr = -7;
    }

/*<       if (mode .eq. 1 .or. mode .eq. 2) then >*/
    if (mode == 1 || mode == 2) {
/*<          type = 'REGULR' >*/
        s_copy(type__, "REGULR", (ftnlen)6, (ftnlen)6);
/*<       else if (mode .eq. 3 ) then >*/
    } else if (mode == 3) {
/*<          type = 'SHIFTI' >*/
        s_copy(type__, "SHIFTI", (ftnlen)6, (ftnlen)6);
/*<       else if (mode .eq. 4 ) then >*/
    } else if (mode == 4) {
/*<          type = 'BUCKLE' >*/
        s_copy(type__, "BUCKLE", (ftnlen)6, (ftnlen)6);
/*<       else if (mode .eq. 5 ) then >*/
    } else if (mode == 5) {
/*<          type = 'CAYLEY' >*/
        s_copy(type__, "CAYLEY", (ftnlen)6, (ftnlen)6);
/*<       else  >*/
    } else {
/*<                                                ierr = -10 >*/
        ierr = -10;
/*<       end if >*/
    }
/*<       if (mode .eq. 1 .and. bmat .eq. 'G')     ierr = -11 >*/
    if (mode == 1 && *(unsigned char *)bmat == 'G') {
        ierr = -11;
    }
/*<       if (nev .eq. 1 .and. which .eq. 'BE')    ierr = -12 >*/
    if (*nev == 1 && s_cmp(which, "BE", (ftnlen)2, (ftnlen)2) == 0) {
        ierr = -12;
    }

/*     %------------% */
/*     | Error Exit | */
/*     %------------% */

/*<       if (ierr .ne. 0) then >*/
    if (ierr != 0) {
/*<          info = ierr >*/
        *info = ierr;
/*<          go to 9000 >*/
        goto L9000;
/*<       end if >*/
    }

/*     %-------------------------------------------------------% */
/*     | Pointer into WORKL for address of H, RITZ, BOUNDS, Q  | */
/*     | etc... and the remaining workspace.                   | */
/*     | Also update pointer to be used on output.             | */
/*     | Memory is laid out as follows:                        | */
/*     | workl(1:2*ncv) := generated tridiagonal matrix H      | */
/*     |       The subdiagonal is stored in workl(2:ncv).      | */
/*     |       The dead spot is workl(1) but upon exiting      | */
/*     |       dsaupd stores the B-norm of the last residual   | */
/*     |       vector in workl(1). We use this !!!             | */
/*     | workl(2*ncv+1:2*ncv+ncv) := ritz values               | */
/*     |       The wanted values are in the first NCONV spots. | */
/*     | workl(3*ncv+1:3*ncv+ncv) := computed Ritz estimates   | */
/*     |       The wanted values are in the first NCONV spots. | */
/*     | NOTE: workl(1:4*ncv) is set by dsaupd and is not      | */
/*     |       modified by dseupd.                             | */
/*     %-------------------------------------------------------% */

/*     %-------------------------------------------------------% */
/*     | The following is used and set by dseupd.              | */
/*     | workl(4*ncv+1:4*ncv+ncv) := used as workspace during  | */
/*     |       computation of the eigenvectors of H. Stores    | */
/*     |       the diagonal of H. Upon EXIT contains the NCV   | */
/*     |       Ritz values of the original system. The first   | */
/*     |       NCONV spots have the wanted values. If MODE =   | */
/*     |       1 or 2 then will equal workl(2*ncv+1:3*ncv).    | */
/*     | workl(5*ncv+1:5*ncv+ncv) := used as workspace during  | */
/*     |       computation of the eigenvectors of H. Stores    | */
/*     |       the subdiagonal of H. Upon EXIT contains the    | */
/*     |       NCV corresponding Ritz estimates of the         | */
/*     |       original system. The first NCONV spots have the | */
/*     |       wanted values. If MODE = 1,2 then will equal    | */
/*     |       workl(3*ncv+1:4*ncv).                           | */
/*     | workl(6*ncv+1:6*ncv+ncv*ncv) := orthogonal Q that is  | */
/*     |       the eigenvector matrix for H as returned by     | */
/*     |       dsteqr. Not referenced if RVEC = .False.        | */
/*     |       Ordering follows that of workl(4*ncv+1:5*ncv)   | */
/*     | workl(6*ncv+ncv*ncv+1:6*ncv+ncv*ncv+2*ncv) :=         | */
/*     |       Workspace. Needed by dsteqr and by dseupd.      | */
/*     | GRAND total of NCV*(NCV+8) locations.                 | */
/*     %-------------------------------------------------------% */


/*<       ih     = ipntr(5) >*/
    ih = ipntr[5];
/*<       ritz   = ipntr(6) >*/
    ritz = ipntr[6];
/*<       bounds = ipntr(7) >*/
    bounds = ipntr[7];
/*<       ldh    = ncv >*/
    ldh = *ncv;
/*<       ldq    = ncv >*/
    ldq = *ncv;
/*<       ihd    = bounds + ldh >*/
    ihd = bounds + ldh;
/*<       ihb    = ihd    + ldh >*/
    ihb = ihd + ldh;
/*<       iq     = ihb    + ldh >*/
    iq = ihb + ldh;
/*<       iw     = iq     + ldh*ncv >*/
    iw = iq + ldh * *ncv;
/*<       next   = iw     + 2*ncv >*/
    next = iw + (*ncv << 1);
/*<       ipntr(4)  = next >*/
    ipntr[4] = next;
/*<       ipntr(8)  = ihd >*/
    ipntr[8] = ihd;
/*<       ipntr(9)  = ihb >*/
    ipntr[9] = ihb;
/*<       ipntr(10) = iq >*/
    ipntr[10] = iq;

/*     %----------------------------------------% */
/*     | irz points to the Ritz values computed | */
/*     |     by _seigt before exiting _saup2.   | */
/*     | ibd points to the Ritz estimates       | */
/*     |     computed by _seigt before exiting  | */
/*     |     _saup2.                            | */
/*     %----------------------------------------% */

/*<       irz = ipntr(11)+ncv >*/
    irz = ipntr[11] + *ncv;
/*<       ibd = irz+ncv >*/
    ibd = irz + *ncv;


/*     %---------------------------------% */
/*     | Set machine dependent constant. | */
/*     %---------------------------------% */

/*<       eps23 = dlamch('Epsilon-Machine')  >*/
    eps23 = dlamch_("Epsilon-Machine", (ftnlen)15);
/*<       eps23 = eps23**(2.0D+0 / 3.0D+0) >*/
    eps23 = pow_dd(&eps23, &c_b21);

/*     %---------------------------------------% */
/*     | RNORM is B-norm of the RESID(1:N).    | */
/*     | BNORM2 is the 2 norm of B*RESID(1:N). | */
/*     | Upon exit of dsaupd WORKD(1:N) has    | */
/*     | B*RESID(1:N).                         | */
/*     %---------------------------------------% */

/*<       rnorm = workl(ih) >*/
    rnorm = workl[ih];
/*<       if (bmat .eq. 'I') then >*/
    if (*(unsigned char *)bmat == 'I') {
/*<          bnorm2 = rnorm >*/
        bnorm2 = rnorm;
/*<       else if (bmat .eq. 'G') then >*/
    } else if (*(unsigned char *)bmat == 'G') {
/*<          bnorm2 = dnrm2(n, workd, 1) >*/
        bnorm2 = dnrm2_(n, &workd[1], &c__1);
/*<       end if >*/
    }

/*<       if (rvec) then >*/
    if (*rvec) {

/*        %------------------------------------------------% */
/*        | Get the converged Ritz value on the boundary.  | */
/*        | This value will be used to dermine whether we  | */
/*        | need to reorder the eigenvalues and            | */
/*        | eigenvectors comupted by _steqr, and is        | */
/*        | referred to as the "threshold" value.          | */
/*        |                                                | */
/*        | A Ritz value gamma is said to be a wanted      | */
/*        | one, if                                        | */
/*        | abs(gamma) .ge. threshold, when WHICH = 'LM';  | */
/*        | abs(gamma) .le. threshold, when WHICH = 'SM';  | */
/*        | gamma      .ge. threshold, when WHICH = 'LA';  | */
/*        | gamma      .le. threshold, when WHICH = 'SA';  | */
/*        | gamma .le. thres1 .or. gamma .ge. thres2       | */
/*        |                            when WHICH = 'BE';  | */
/*        |                                                | */
/*        | Note: converged Ritz values and associated     | */
/*        | Ritz estimates have been placed in the first   | */
/*        | NCONV locations in workl(ritz) and             | */
/*        | workl(bounds) respectively. They have been     | */
/*        | sorted (in _saup2) according to the WHICH      | */
/*        | selection criterion. (Except in the case       | */
/*        | WHICH = 'BE', they are sorted in an increasing | */
/*        | order.)                                        | */
/*        %------------------------------------------------% */

/*<        >*/
        if (s_cmp(which, "LM", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(which,
                "SM", (ftnlen)2, (ftnlen)2) == 0 || s_cmp(which, "LA", (
                ftnlen)2, (ftnlen)2) == 0 || s_cmp(which, "SA", (ftnlen)2, (
                ftnlen)2) == 0) {

/*<              thres1 = workl(ritz) >*/
            thres1 = workl[ritz];

/*             if (msglvl .gt. 2) then */
/*                call dvout(logfil, 1, thres1, ndigit, */
/*     &          '_seupd: Threshold eigenvalue used for re-ordering') */
/*             end if */

/*<          else if (which .eq. 'BE') then >*/
        } else if (s_cmp(which, "BE", (ftnlen)2, (ftnlen)2) == 0) {

/*            %------------------------------------------------% */
/*            | Ritz values returned from _saup2 have been     | */
/*            | sorted in increasing order.  Thus two          | */
/*            | "threshold" values (one for the small end, one | */
/*            | for the large end) are in the middle.          | */
/*            %------------------------------------------------% */

/*<              ism = max(nev,nconv) / 2 >*/
            ism = max(*nev,nconv) / 2;
/*<              ilg = ism + 1 >*/
            ilg = ism + 1;
/*<              thres1 = workl(ism) >*/
            thres1 = workl[ism];
/*<              thres2 = workl(ilg)  >*/
            thres2 = workl[ilg];

/*<              if (msglvl .gt. 2) then >*/
/*          if (msglvl > 2) { */
/*<                 kv(1) = thres1 >*/
/*              kv[0] = thres1; */
/*<                 kv(2) = thres2 >*/
/*              kv[1] = thres2; */
/*                call dvout(logfil, 2, kv, ndigit, */
/*     &          '_seupd: Threshold eigenvalues used for re-ordering') */
/*<              end if >*/
/*          } */

/*<          end if >*/
        }

/*        %----------------------------------------------------------% */
/*        | Check to see if all converged Ritz values appear within  | */
/*        | the first NCONV diagonal elements returned from _seigt.  | */
/*        | This is done in the following way:                       | */
/*        |                                                          | */
/*        | 1) For each Ritz value obtained from _seigt, compare it  | */
/*        |    with the threshold Ritz value computed above to       | */
/*        |    determine whether it is a wanted one.                 | */
/*        |                                                          | */
/*        | 2) If it is wanted, then check the corresponding Ritz    | */
/*        |    estimate to see if it has converged.  If it has, set  | */
/*        |    corresponding entry in the logical array SELECT to    | */
/*        |    .TRUE..                                               | */
/*        |                                                          | */
/*        | If SELECT(j) = .TRUE. and j > NCONV, then there is a     | */
/*        | converged Ritz value that does not appear at the top of  | */
/*        | the diagonal matrix computed by _seigt in _saup2.        | */
/*        | Reordering is needed.                                    | */
/*        %----------------------------------------------------------% */

/*<          reord = .false. >*/
        reord = FALSE_;
/*<          ktrord = 0 >*/
        ktrord = 0;
/*<          do 10 j = 0, ncv-1 >*/
        i__1 = *ncv - 1;
        for (j = 0; j <= i__1; ++j) {
/*<             select(j+1) = .false. >*/
            select[j + 1] = FALSE_;
/*<             if (which .eq. 'LM') then >*/
            if (s_cmp(which, "LM", (ftnlen)2, (ftnlen)2) == 0) {
/*<                if (abs(workl(irz+j)) .ge. abs(thres1)) then >*/
                if ((d__1 = workl[irz + j], abs(d__1)) >= abs(thres1)) {
/*<                    tempbnd = max( eps23, abs(workl(irz+j)) ) >*/
/* Computing MAX */
                    d__2 = eps23, d__3 = (d__1 = workl[irz + j], abs(d__1));
                    tempbnd = max(d__2,d__3);
/*<                    if (workl(ibd+j) .le. tol*tempbnd) then >*/
                    if (workl[ibd + j] <= *tol * tempbnd) {
/*<                       select(j+1) = .true. >*/
                        select[j + 1] = TRUE_;
/*<                    end if >*/
                    }
/*<                end if >*/
                }
/*<             else if (which .eq. 'SM') then >*/
            } else if (s_cmp(which, "SM", (ftnlen)2, (ftnlen)2) == 0) {
/*<                if (abs(workl(irz+j)) .le. abs(thres1)) then >*/
                if ((d__1 = workl[irz + j], abs(d__1)) <= abs(thres1)) {
/*<                    tempbnd = max( eps23, abs(workl(irz+j)) ) >*/
/* Computing MAX */
                    d__2 = eps23, d__3 = (d__1 = workl[irz + j], abs(d__1));
                    tempbnd = max(d__2,d__3);
/*<                    if (workl(ibd+j) .le. tol*tempbnd) then >*/
                    if (workl[ibd + j] <= *tol * tempbnd) {
/*<                       select(j+1) = .true. >*/
                        select[j + 1] = TRUE_;
/*<                    end if >*/
                    }
/*<                end if >*/
                }
/*<             else if (which .eq. 'LA') then >*/
            } else if (s_cmp(which, "LA", (ftnlen)2, (ftnlen)2) == 0) {
/*<                if (workl(irz+j) .ge. thres1) then >*/
                if (workl[irz + j] >= thres1) {
/*<                   tempbnd = max( eps23, abs(workl(irz+j)) ) >*/
/* Computing MAX */
                    d__2 = eps23, d__3 = (d__1 = workl[irz + j], abs(d__1));
                    tempbnd = max(d__2,d__3);
/*<                   if (workl(ibd+j) .le. tol*tempbnd) then >*/
                    if (workl[ibd + j] <= *tol * tempbnd) {
/*<                      select(j+1) = .true. >*/
                        select[j + 1] = TRUE_;
/*<                   end if >*/
                    }
/*<                end if >*/
                }
/*<             else if (which .eq. 'SA') then >*/
            } else if (s_cmp(which, "SA", (ftnlen)2, (ftnlen)2) == 0) {
/*<                if (workl(irz+j) .le. thres1) then >*/
                if (workl[irz + j] <= thres1) {
/*<                   tempbnd = max( eps23, abs(workl(irz+j)) ) >*/
/* Computing MAX */
                    d__2 = eps23, d__3 = (d__1 = workl[irz + j], abs(d__1));
                    tempbnd = max(d__2,d__3);
/*<                   if (workl(ibd+j) .le. tol*tempbnd) then >*/
                    if (workl[ibd + j] <= *tol * tempbnd) {
/*<                      select(j+1) = .true. >*/
                        select[j + 1] = TRUE_;
/*<                   end if >*/
                    }
/*<                end if >*/
                }
/*<             else if (which .eq. 'BE') then >*/
            } else if (s_cmp(which, "BE", (ftnlen)2, (ftnlen)2) == 0) {
/*<        >*/
                if (workl[irz + j] <= thres1 || workl[irz + j] >= thres2) {
/*<                   tempbnd = max( eps23, abs(workl(irz+j)) ) >*/
/* Computing MAX */
                    d__2 = eps23, d__3 = (d__1 = workl[irz + j], abs(d__1));
                    tempbnd = max(d__2,d__3);
/*<                   if (workl(ibd+j) .le. tol*tempbnd) then >*/
                    if (workl[ibd + j] <= *tol * tempbnd) {
/*<                      select(j+1) = .true. >*/
                        select[j + 1] = TRUE_;
/*<                   end if >*/
                    }
/*<                end if >*/
                }
/*<             end if >*/
            }
/*<             if (j+1 .gt. nconv ) reord = select(j+1) .or. reord >*/
            if (j + 1 > nconv) {
                reord = select[j + 1] || reord;
            }
/*<             if (select(j+1)) ktrord = ktrord + 1 >*/
            if (select[j + 1]) {
                ++ktrord;
            }
/*<  10      continue >*/
/* L10: */
        }
/*        %-------------------------------------------% */
/*        | If KTRORD .ne. NCONV, something is wrong. | */
/*        %-------------------------------------------% */

/*         if (msglvl .gt. 2) then */
/*             call ivout(logfil, 1, ktrord, ndigit, */
/*     &            '_seupd: Number of specified eigenvalues') */
/*             call ivout(logfil, 1, nconv, ndigit, */
/*     &            '_seupd: Number of "converged" eigenvalues') */
/*         end if */

/*        %-----------------------------------------------------------% */
/*        | Call LAPACK routine _steqr to compute the eigenvalues and | */
/*        | eigenvectors of the final symmetric tridiagonal matrix H. | */
/*        | Initialize the eigenvector matrix Q to the identity.      | */
/*        %-----------------------------------------------------------% */

/*<          call dcopy (ncv-1, workl(ih+1), 1, workl(ihb), 1) >*/
        i__1 = *ncv - 1;
        dcopy_(&i__1, &workl[ih + 1], &c__1, &workl[ihb], &c__1);
/*<          call dcopy (ncv, workl(ih+ldh), 1, workl(ihd), 1) >*/
        dcopy_(ncv, &workl[ih + ldh], &c__1, &workl[ihd], &c__1);

/*<        >*/
        dsteqr_("Identity", ncv, &workl[ihd], &workl[ihb], &workl[iq], &ldq, &
                workl[iw], &ierr, (ftnlen)8);

/*<          if (ierr .ne. 0) then >*/
        if (ierr != 0) {
/*<             info = -8 >*/
            *info = -8;
/*<             go to 9000 >*/
            goto L9000;
/*<          end if >*/
        }

/*<          if (msglvl .gt. 1) then >*/
/*      if (msglvl > 1) { */
/*<             call dcopy (ncv, workl(iq+ncv-1), ldq, workl(iw), 1) >*/
/*          dcopy_(ncv, &workl[iq + *ncv - 1], &ldq, &workl[iw], &c__1); */
/*            call dvout (logfil, ncv, workl(ihd), ndigit, */
/*     &          '_seupd: NCV Ritz values of the final H matrix') */
/*            call dvout (logfil, ncv, workl(iw), ndigit, */
/*     &           '_seupd: last row of the eigenvector matrix for H') */
/*<          end if >*/
/*      } */

/*<          if (reord) then >*/
        if (reord) {

/*           %---------------------------------------------% */
/*           | Reordered the eigenvalues and eigenvectors  | */
/*           | computed by _steqr so that the "converged"  | */
/*           | eigenvalues appear in the first NCONV       | */
/*           | positions of workl(ihd), and the associated | */
/*           | eigenvectors appear in the first NCONV      | */
/*           | columns.                                    | */
/*           %---------------------------------------------% */

/*<             leftptr = 1 >*/
            leftptr = 1;
/*<             rghtptr = ncv >*/
            rghtptr = *ncv;

/*<             if (ncv .eq. 1) go to 30 >*/
            if (*ncv == 1) {
                goto L30;
            }

/*<  20         if (select(leftptr)) then >*/
L20:
            if (select[leftptr]) {

/*              %-------------------------------------------% */
/*              | Search, from the left, for the first Ritz | */
/*              | value that has not converged.             | */
/*              %-------------------------------------------% */

/*<                leftptr = leftptr + 1 >*/
                ++leftptr;

/*<             else if ( .not. select(rghtptr)) then >*/
            } else if (! select[rghtptr]) {

/*              %----------------------------------------------% */
/*              | Search, from the right, the first Ritz value | */
/*              | that has converged.                          | */
/*              %----------------------------------------------% */

/*<                rghtptr = rghtptr - 1 >*/
                --rghtptr;

/*<             else >*/
            } else {

/*              %----------------------------------------------% */
/*              | Swap the Ritz value on the left that has not | */
/*              | converged with the Ritz value on the right   | */
/*              | that has converged.  Swap the associated     | */
/*              | eigenvector of the tridiagonal matrix H as   | */
/*              | well.                                        | */
/*              %----------------------------------------------% */

/*<                temp = workl(ihd+leftptr-1) >*/
                temp = workl[ihd + leftptr - 1];
/*<                workl(ihd+leftptr-1) = workl(ihd+rghtptr-1) >*/
                workl[ihd + leftptr - 1] = workl[ihd + rghtptr - 1];
/*<                workl(ihd+rghtptr-1) = temp >*/
                workl[ihd + rghtptr - 1] = temp;
/*<        >*/
                dcopy_(ncv, &workl[iq + *ncv * (leftptr - 1)], &c__1, &workl[
                        iw], &c__1);
/*<        >*/
                dcopy_(ncv, &workl[iq + *ncv * (rghtptr - 1)], &c__1, &workl[
                        iq + *ncv * (leftptr - 1)], &c__1);
/*<        >*/
                dcopy_(ncv, &workl[iw], &c__1, &workl[iq + *ncv * (rghtptr -
                        1)], &c__1);
/*<                leftptr = leftptr + 1 >*/
                ++leftptr;
/*<                rghtptr = rghtptr - 1 >*/
                --rghtptr;

/*<             end if >*/
            }

/*<             if (leftptr .lt. rghtptr) go to 20 >*/
            if (leftptr < rghtptr) {
                goto L20;
            }

/*<  30      end if >*/
L30:
            ;
        }

/*         if (msglvl .gt. 2) then */
/*             call dvout (logfil, ncv, workl(ihd), ndigit, */
/*     &       '_seupd: The eigenvalues of H--reordered') */
/*         end if */

/*        %----------------------------------------% */
/*        | Load the converged Ritz values into D. | */
/*        %----------------------------------------% */

/*<          call dcopy(nconv, workl(ihd), 1, d, 1) >*/
        dcopy_(&nconv, &workl[ihd], &c__1, &d__[1], &c__1);

/*<       else >*/
    } else {

/*        %-----------------------------------------------------% */
/*        | Ritz vectors not required. Load Ritz values into D. | */
/*        %-----------------------------------------------------% */

/*<          call dcopy (nconv, workl(ritz), 1, d, 1) >*/
        dcopy_(&nconv, &workl[ritz], &c__1, &d__[1], &c__1);
/*<          call dcopy (ncv, workl(ritz), 1, workl(ihd), 1) >*/
        dcopy_(ncv, &workl[ritz], &c__1, &workl[ihd], &c__1);

/*<       end if >*/
    }

/*     %------------------------------------------------------------------% */
/*     | Transform the Ritz values and possibly vectors and corresponding | */
/*     | Ritz estimates of OP to those of A*x=lambda*B*x. The Ritz values | */
/*     | (and corresponding data) are returned in ascending order.        | */
/*     %------------------------------------------------------------------% */

/*<       if (type .eq. 'REGULR') then >*/
    if (s_cmp(type__, "REGULR", (ftnlen)6, (ftnlen)6) == 0) {

/*        %---------------------------------------------------------% */
/*        | Ascending sort of wanted Ritz values, vectors and error | */
/*        | bounds. Not necessary if only Ritz values are desired.  | */
/*        %---------------------------------------------------------% */

/*<          if (rvec) then >*/
        if (*rvec) {
/*<             call dsesrt ('LA', rvec , nconv, d, ncv, workl(iq), ldq) >*/
            dsesrt_("LA", rvec, &nconv, &d__[1], ncv, &workl[iq], &ldq, (
                    ftnlen)2);
/*<          else >*/
        } else {
/*<             call dcopy (ncv, workl(bounds), 1, workl(ihb), 1) >*/
            dcopy_(ncv, &workl[bounds], &c__1, &workl[ihb], &c__1);
/*<          end if >*/
        }

/*<       else  >*/
    } else {

/*        %-------------------------------------------------------------% */
/*        | *  Make a copy of all the Ritz values.                      | */
/*        | *  Transform the Ritz values back to the original system.   | */
/*        |    For TYPE = 'SHIFTI' the transformation is                | */
/*        |             lambda = 1/theta + sigma                        | */
/*        |    For TYPE = 'BUCKLE' the transformation is                | */
/*        |             lambda = sigma * theta / ( theta - 1 )          | */
/*        |    For TYPE = 'CAYLEY' the transformation is                | */
/*        |             lambda = sigma * (theta + 1) / (theta - 1 )     | */
/*        |    where the theta are the Ritz values returned by dsaupd.  | */
/*        | NOTES:                                                      | */
/*        | *The Ritz vectors are not affected by the transformation.   | */
/*        |  They are only reordered.                                   | */
/*        %-------------------------------------------------------------% */

/*<          call dcopy (ncv, workl(ihd), 1, workl(iw), 1) >*/
        dcopy_(ncv, &workl[ihd], &c__1, &workl[iw], &c__1);
/*<          if (type .eq. 'SHIFTI') then  >*/
        if (s_cmp(type__, "SHIFTI", (ftnlen)6, (ftnlen)6) == 0) {
/*<             do 40 k=1, ncv >*/
            i__1 = *ncv;
            for (k = 1; k <= i__1; ++k) {
/*<                workl(ihd+k-1) = one / workl(ihd+k-1) + sigma >*/
                workl[ihd + k - 1] = 1. / workl[ihd + k - 1] + *sigma;
/*<   40        continue >*/
/* L40: */
            }
/*<          else if (type .eq. 'BUCKLE') then >*/
        } else if (s_cmp(type__, "BUCKLE", (ftnlen)6, (ftnlen)6) == 0) {
/*<             do 50 k=1, ncv >*/
            i__1 = *ncv;
            for (k = 1; k <= i__1; ++k) {
/*<        >*/
                workl[ihd + k - 1] = *sigma * workl[ihd + k - 1] / (workl[ihd
                        + k - 1] - 1.);
/*<   50        continue >*/
/* L50: */
            }
/*<          else if (type .eq. 'CAYLEY') then >*/
        } else if (s_cmp(type__, "CAYLEY", (ftnlen)6, (ftnlen)6) == 0) {
/*<             do 60 k=1, ncv >*/
            i__1 = *ncv;
            for (k = 1; k <= i__1; ++k) {
/*<        >*/
                workl[ihd + k - 1] = *sigma * (workl[ihd + k - 1] + 1.) / (
                        workl[ihd + k - 1] - 1.);
/*<   60        continue >*/
/* L60: */
            }
/*<          end if >*/
        }

/*        %-------------------------------------------------------------% */
/*        | *  Store the wanted NCONV lambda values into D.             | */
/*        | *  Sort the NCONV wanted lambda in WORKL(IHD:IHD+NCONV-1)   | */
/*        |    into ascending order and apply sort to the NCONV theta   | */
/*        |    values in the transformed system. We'll need this to     | */
/*        |    compute Ritz estimates in the original system.           | */
/*        | *  Finally sort the lambda's into ascending order and apply | */
/*        |    to Ritz vectors if wanted. Else just sort lambda's into  | */
/*        |    ascending order.                                         | */
/*        | NOTES:                                                      | */
/*        | *workl(iw:iw+ncv-1) contain the theta ordered so that they  | */
/*        |  match the ordering of the lambda. We'll use them again for | */
/*        |  Ritz vector purification.                                  | */
/*        %-------------------------------------------------------------% */

/*<          call dcopy (nconv, workl(ihd), 1, d, 1) >*/
        dcopy_(&nconv, &workl[ihd], &c__1, &d__[1], &c__1);
/*<          call dsortr ('LA', .true., nconv, workl(ihd), workl(iw)) >*/
        dsortr_("LA", &c_true, &nconv, &workl[ihd], &workl[iw], (ftnlen)2);
/*<          if (rvec) then >*/
        if (*rvec) {
/*<             call dsesrt ('LA', rvec , nconv, d, ncv, workl(iq), ldq) >*/
            dsesrt_("LA", rvec, &nconv, &d__[1], ncv, &workl[iq], &ldq, (
                    ftnlen)2);
/*<          else >*/
        } else {
/*<             call dcopy (ncv, workl(bounds), 1, workl(ihb), 1) >*/
            dcopy_(ncv, &workl[bounds], &c__1, &workl[ihb], &c__1);
/*<             call dscal (ncv, bnorm2/rnorm, workl(ihb), 1) >*/
            d__1 = bnorm2 / rnorm;
            dscal_(ncv, &d__1, &workl[ihb], &c__1);
/*<             call dsortr ('LA', .true., nconv, d, workl(ihb)) >*/
            dsortr_("LA", &c_true, &nconv, &d__[1], &workl[ihb], (ftnlen)2);
/*<          end if >*/
        }

/*<       end if  >*/
    }

/*     %------------------------------------------------% */
/*     | Compute the Ritz vectors. Transform the wanted | */
/*     | eigenvectors of the symmetric tridiagonal H by | */
/*     | the Lanczos basis matrix V.                    | */
/*     %------------------------------------------------% */

/*<       if (rvec .and. howmny .eq. 'A') then >*/
    if (*rvec && *(unsigned char *)howmny == 'A') {

/*        %----------------------------------------------------------% */
/*        | Compute the QR factorization of the matrix representing  | */
/*        | the wanted invariant subspace located in the first NCONV | */
/*        | columns of workl(iq,ldq).                                | */
/*        %----------------------------------------------------------% */

/*<        >*/
        dgeqr2_(ncv, &nconv, &workl[iq], &ldq, &workl[iw + *ncv], &workl[ihb],
                 &ierr);


/*        %--------------------------------------------------------% */
/*        | * Postmultiply V by Q.                                 | */
/*        | * Copy the first NCONV columns of VQ into Z.           | */
/*        | The N by NCONV matrix Z is now a matrix representation | */
/*        | of the approximate invariant subspace associated with  | */
/*        | the Ritz values in workl(ihd).                         | */
/*        %--------------------------------------------------------% */

/*<        >*/
        dorm2r_("Right", "Notranspose", n, ncv, &nconv, &workl[iq], &ldq, &
                workl[iw + *ncv], &v[v_offset], ldv, &workd[*n + 1], &ierr, (
                ftnlen)5, (ftnlen)11);
/*<          call dlacpy ('All', n, nconv, v, ldv, z, ldz) >*/
        dlacpy_("All", n, &nconv, &v[v_offset], ldv, &z__[z_offset], ldz, (
                ftnlen)3);

/*        %-----------------------------------------------------% */
/*        | In order to compute the Ritz estimates for the Ritz | */
/*        | values in both systems, need the last row of the    | */
/*        | eigenvector matrix. Remember, it's in factored form | */
/*        %-----------------------------------------------------% */

/*<          do 65 j = 1, ncv-1 >*/
        i__1 = *ncv - 1;
        for (j = 1; j <= i__1; ++j) {
/*<             workl(ihb+j-1) = zero  >*/
            workl[ihb + j - 1] = 0.;
/*<   65     continue >*/
/* L65: */
        }
/*<          workl(ihb+ncv-1) = one >*/
        workl[ihb + *ncv - 1] = 1.;
/*<        >*/
        dorm2r_("Left", "Transpose", ncv, &c__1, &nconv, &workl[iq], &ldq, &
                workl[iw + *ncv], &workl[ihb], ncv, &temp, &ierr, (ftnlen)4, (
                ftnlen)9);

/*<       else if (rvec .and. howmny .eq. 'S') then >*/
    } else if (*rvec && *(unsigned char *)howmny == 'S') {

/*     Not yet implemented. See remark 2 above. */

/*<       end if >*/
    }

/*<       if (type .eq. 'REGULR' .and. rvec) then >*/
    if (s_cmp(type__, "REGULR", (ftnlen)6, (ftnlen)6) == 0 && *rvec) {

/*<             do 70 j=1, ncv >*/
        i__1 = *ncv;
        for (j = 1; j <= i__1; ++j) {
/*<                workl(ihb+j-1) = rnorm * abs( workl(ihb+j-1) ) >*/
            workl[ihb + j - 1] = rnorm * (d__1 = workl[ihb + j - 1], abs(d__1)
                    );
/*<  70         continue >*/
/* L70: */
        }

/*<       else if (type .ne. 'REGULR' .and. rvec) then >*/
    } else if (s_cmp(type__, "REGULR", (ftnlen)6, (ftnlen)6) != 0 && *rvec) {

/*        %-------------------------------------------------% */
/*        | *  Determine Ritz estimates of the theta.       | */
/*        |    If RVEC = .true. then compute Ritz estimates | */
/*        |               of the theta.                     | */
/*        |    If RVEC = .false. then copy Ritz estimates   | */
/*        |              as computed by dsaupd.             | */
/*        | *  Determine Ritz estimates of the lambda.      | */
/*        %-------------------------------------------------% */

/*<          call dscal (ncv, bnorm2, workl(ihb), 1) >*/
        dscal_(ncv, &bnorm2, &workl[ihb], &c__1);
/*<          if (type .eq. 'SHIFTI') then  >*/
        if (s_cmp(type__, "SHIFTI", (ftnlen)6, (ftnlen)6) == 0) {

/*<             do 80 k=1, ncv >*/
            i__1 = *ncv;
            for (k = 1; k <= i__1; ++k) {
/*<                workl(ihb+k-1) = abs( workl(ihb+k-1) ) / workl(iw+k-1)**2 >*/
/* Computing 2nd power */
                d__2 = workl[iw + k - 1];
                workl[ihb + k - 1] = (d__1 = workl[ihb + k - 1], abs(d__1)) /
                        (d__2 * d__2);
/*<  80         continue >*/
/* L80: */
            }

/*<          else if (type .eq. 'BUCKLE') then >*/
        } else if (s_cmp(type__, "BUCKLE", (ftnlen)6, (ftnlen)6) == 0) {

/*<             do 90 k=1, ncv >*/
            i__1 = *ncv;
            for (k = 1; k <= i__1; ++k) {
/*<        >*/
/* Computing 2nd power */
                d__2 = workl[iw + k - 1] - 1.;
                workl[ihb + k - 1] = *sigma * (d__1 = workl[ihb + k - 1], abs(
                        d__1)) / (d__2 * d__2);
/*<  90         continue >*/
/* L90: */
            }

/*<          else if (type .eq. 'CAYLEY') then >*/
        } else if (s_cmp(type__, "CAYLEY", (ftnlen)6, (ftnlen)6) == 0) {

/*<             do 100 k=1, ncv >*/
            i__1 = *ncv;
            for (k = 1; k <= i__1; ++k) {
/*<        >*/
                workl[ihb + k - 1] = (d__1 = workl[ihb + k - 1] / workl[iw +
                        k - 1] * (workl[iw + k - 1] - 1.), abs(d__1));
/*<  100        continue >*/
/* L100: */
            }

/*<          end if >*/
        }

/*<       end if >*/
    }

/*      if (type .ne. 'REGULR' .and. msglvl .gt. 1) then */
/*         call dvout (logfil, nconv, d, ndigit, */
/*     &          '_seupd: Untransformed converged Ritz values') */
/*         call dvout (logfil, nconv, workl(ihb), ndigit, */
/*     &     '_seupd: Ritz estimates of the untransformed Ritz values') */
/*      else if (msglvl .gt. 1) then */
/*         call dvout (logfil, nconv, d, ndigit, */
/*     &          '_seupd: Converged Ritz values') */
/*         call dvout (logfil, nconv, workl(ihb), ndigit, */
/*     &     '_seupd: Associated Ritz estimates') */
/*      end if */

/*     %-------------------------------------------------% */
/*     | Ritz vector purification step. Formally perform | */
/*     | one of inverse subspace iteration. Only used    | */
/*     | for MODE = 3,4,5. See reference 7               | */
/*     %-------------------------------------------------% */

/*<       if (rvec .and. (type .eq. 'SHIFTI' .or. type .eq. 'CAYLEY')) then >*/
    if (*rvec && (s_cmp(type__, "SHIFTI", (ftnlen)6, (ftnlen)6) == 0 || s_cmp(
            type__, "CAYLEY", (ftnlen)6, (ftnlen)6) == 0)) {

/*<          do 110 k=0, nconv-1 >*/
        i__1 = nconv - 1;
        for (k = 0; k <= i__1; ++k) {
/*<             workl(iw+k) = workl(iq+k*ldq+ncv-1) / workl(iw+k) >*/
            workl[iw + k] = workl[iq + k * ldq + *ncv - 1] / workl[iw + k];
/*<  110     continue >*/
/* L110: */
        }

/*<       else if (rvec .and. type .eq. 'BUCKLE') then >*/
    } else if (*rvec && s_cmp(type__, "BUCKLE", (ftnlen)6, (ftnlen)6) == 0) {

/*<          do 120 k=0, nconv-1 >*/
        i__1 = nconv - 1;
        for (k = 0; k <= i__1; ++k) {
/*<             workl(iw+k) = workl(iq+k*ldq+ncv-1) / (workl(iw+k)-one) >*/
            workl[iw + k] = workl[iq + k * ldq + *ncv - 1] / (workl[iw + k] -
                    1.);
/*<  120     continue >*/
/* L120: */
        }

/*<       end if  >*/
    }

/*<        >*/
    if (s_cmp(type__, "REGULR", (ftnlen)6, (ftnlen)6) != 0) {
        dger_(n, &nconv, &c_b103, &resid[1], &c__1, &workl[iw], &c__1, &z__[
                z_offset], ldz);
    }

/*<  9000 continue >*/
L9000:

/*<       return >*/
    return 0;

/*     %---------------% */
/*     | End of dseupd | */
/*     %---------------% */

/*<       end >*/
} /* dseupd_ */

#ifdef __cplusplus
        }
#endif
