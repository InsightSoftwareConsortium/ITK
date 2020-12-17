/* arpack/dsaup2.f -- translated by f2c (version 20090411).
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

/*Extern struct { */
/*  integer nopx, nbx, nrorth, nitref, nrstrt; */
/*  real tsaupd, tsaup2, tsaitr, tseigt, tsgets, tsapps, tsconv, tnaupd, */
/*          tnaup2, tnaitr, tneigh, tngets, tnapps, tnconv, tcaupd, tcaup2, */
/*          tcaitr, tceigh, tcgets, tcapps, tcconv, tmvopx, tmvbx, tgetv0, */
/*          titref, trvec; */
/*} timing_; */

/*#define timing_1 timing_ */

/* Table of constant values */

static doublereal c_b3 = .66666666666666663;
static integer c__1 = 1;
static integer c__0 = 0;
static logical c_true = TRUE_;

/* ----------------------------------------------------------------------- */
/* \BeginDoc */

/* \Name: dsaup2 */

/* \Description: */
/*  Intermediate level interface called by dsaupd. */

/* \Usage: */
/*  call dsaup2 */
/*     ( IDO, BMAT, N, WHICH, NEV, NP, TOL, RESID, MODE, IUPD, */
/*       ISHIFT, MXITER, V, LDV, H, LDH, RITZ, BOUNDS, Q, LDQ, WORKL, */
/*       IPNTR, WORKD, INFO ) */

/* \Arguments */

/*  IDO, BMAT, N, WHICH, NEV, TOL, RESID: same as defined in dsaupd. */
/*  MODE, ISHIFT, MXITER: see the definition of IPARAM in dsaupd. */

/*  NP      Integer.  (INPUT/OUTPUT) */
/*          Contains the number of implicit shifts to apply during */
/*          each Arnoldi/Lanczos iteration. */
/*          If ISHIFT=1, NP is adjusted dynamically at each iteration */
/*          to accelerate convergence and prevent stagnation. */
/*          This is also roughly equal to the number of matrix-vector */
/*          products (involving the operator OP) per Arnoldi iteration. */
/*          The logic for adjusting is contained within the current */
/*          subroutine. */
/*          If ISHIFT=0, NP is the number of shifts the user needs */
/*          to provide via reverse comunication. 0 < NP < NCV-NEV. */
/*          NP may be less than NCV-NEV since a leading block of the current */
/*          upper Tridiagonal matrix has split off and contains "unwanted" */
/*          Ritz values. */
/*          Upon termination of the IRA iteration, NP contains the number */
/*          of "converged" wanted Ritz values. */

/*  IUPD    Integer.  (INPUT) */
/*          IUPD .EQ. 0: use explicit restart instead implicit update. */
/*          IUPD .NE. 0: use implicit update. */

/*  V       Double precision N by (NEV+NP) array.  (INPUT/OUTPUT) */
/*          The Lanczos basis vectors. */

/*  LDV     Integer.  (INPUT) */
/*          Leading dimension of V exactly as declared in the calling */
/*          program. */

/*  H       Double precision (NEV+NP) by 2 array.  (OUTPUT) */
/*          H is used to store the generated symmetric tridiagonal matrix */
/*          The subdiagonal is stored in the first column of H starting */
/*          at H(2,1).  The main diagonal is stored in the second column */
/*          of H starting at H(1,2). If dsaup2 converges store the */
/*          B-norm of the final residual vector in H(1,1). */

/*  LDH     Integer.  (INPUT) */
/*          Leading dimension of H exactly as declared in the calling */
/*          program. */

/*  RITZ    Double precision array of length NEV+NP.  (OUTPUT) */
/*          RITZ(1:NEV) contains the computed Ritz values of OP. */

/*  BOUNDS  Double precision array of length NEV+NP.  (OUTPUT) */
/*          BOUNDS(1:NEV) contain the error bounds corresponding to RITZ. */

/*  Q       Double precision (NEV+NP) by (NEV+NP) array.  (WORKSPACE) */
/*          Private (replicated) work array used to accumulate the */
/*          rotation in the shift application step. */

/*  LDQ     Integer.  (INPUT) */
/*          Leading dimension of Q exactly as declared in the calling */
/*          program. */

/*  WORKL   Double precision array of length at least 3*(NEV+NP).  (INPUT/WORKSPACE) */
/*          Private (replicated) array on each PE or array allocated on */
/*          the front end.  It is used in the computation of the */
/*          tridiagonal eigenvalue problem, the calculation and */
/*          application of the shifts and convergence checking. */
/*          If ISHIFT .EQ. O and IDO .EQ. 3, the first NP locations */
/*          of WORKL are used in reverse communication to hold the user */
/*          supplied shifts. */

/*  IPNTR   Integer array of length 3.  (OUTPUT) */
/*          Pointer to mark the starting locations in the WORKD for */
/*          vectors used by the Lanczos iteration. */
/*          ------------------------------------------------------------- */
/*          IPNTR(1): pointer to the current operand vector X. */
/*          IPNTR(2): pointer to the current result vector Y. */
/*          IPNTR(3): pointer to the vector B * X when used in one of */
/*                    the spectral transformation modes.  X is the current */
/*                    operand. */
/*          ------------------------------------------------------------- */

/*  WORKD   Double precision work array of length 3*N.  (REVERSE COMMUNICATION) */
/*          Distributed array to be used in the basic Lanczos iteration */
/*          for reverse communication.  The user should not use WORKD */
/*          as temporary workspace during the iteration !!!!!!!!!! */
/*          See Data Distribution Note in dsaupd. */

/*  INFO    Integer.  (INPUT/OUTPUT) */
/*          If INFO .EQ. 0, a randomly initial residual vector is used. */
/*          If INFO .NE. 0, RESID contains the initial residual vector, */
/*                          possibly from a previous run. */
/*          Error flag on output. */
/*          =     0: Normal return. */
/*          =     1: All possible eigenvalues of OP has been found. */
/*                   NP returns the size of the invariant subspace */
/*                   spanning the operator OP. */
/*          =     2: No shifts could be applied. */
/*          =    -8: Error return from trid. eigenvalue calculation; */
/*                   This should never happen. */
/*          =    -9: Starting vector is zero. */
/*          = -9999: Could not build an Lanczos factorization. */
/*                   Size that was built in returned in NP. */

/* \EndDoc */

/* ----------------------------------------------------------------------- */

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

/* \Routines called: */
/*     dgetv0  ARPACK initial vector generation routine. */
/*     dsaitr  ARPACK Lanczos factorization routine. */
/*     dsapps  ARPACK application of implicit shifts routine. */
/*     dsconv  ARPACK convergence of Ritz values routine. */
/*     dseigt  ARPACK compute Ritz values and error bounds routine. */
/*     dsgets  ARPACK reorder Ritz values and error bounds routine. */
/*     dsortr  ARPACK sorting routine. */
/*     second  ARPACK utility routine for timing. */
/*     dlamch  LAPACK routine that determines machine constants. */
/*     dcopy   Level 1 BLAS that copies one vector to another. */
/*     ddot    Level 1 BLAS that computes the scalar product of two vectors. */
/*     dnrm2   Level 1 BLAS that computes the norm of a vector. */
/*     dscal   Level 1 BLAS that scales a vector. */
/*     dswap   Level 1 BLAS that swaps two vectors. */

/* \Author */
/*     Danny Sorensen               Phuong Vu */
/*     Richard Lehoucq              CRPC / Rice University */
/*     Dept. of Computational &     Houston, Texas */
/*     Applied Mathematics */
/*     Rice University */
/*     Houston, Texas */

/* \Revision history: */
/*     12/15/93: Version ' 2.4' */
/*     xx/xx/95: Version ' 2.4'.  (R.B. Lehoucq) */

/* \SCCS Information: @(#) */
/* FILE: saup2.F   SID: 2.6   DATE OF SID: 8/16/96   RELEASE: 2 */

/* \EndLib */

/* ----------------------------------------------------------------------- */

/*<        >*/
/* Subroutine */ int dsaup2_(integer *ido, char *bmat, integer *n, char *
        which, integer *nev, integer *np, doublereal *tol, doublereal *resid,
        integer *mode, integer *iupd, integer *ishift, integer *mxiter,
        doublereal *v, integer *ldv, doublereal *h__, integer *ldh,
        doublereal *ritz, doublereal *bounds, doublereal *q, integer *ldq,
        doublereal *workl, integer *ipntr, doublereal *workd, integer *info,
        ftnlen bmat_len, ftnlen which_len)
{
    /* System generated locals */
    integer h_dim1, h_offset, q_dim1, q_offset, v_dim1, v_offset, i__1, i__2,
            i__3;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *);
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double sqrt(doublereal);

    /* Local variables */
    integer j;
/*  static real t0, t1, t2, t3; */
/*  integer kp[3]; */
    static integer np0, nev0;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *,
            integer *);
    static doublereal eps23;
    integer ierr;
    static integer iter;
    doublereal temp;
    integer nevd2;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    static logical getv0;
    integer nevm2;
    static logical cnorm;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *,
            doublereal *, integer *), dswap_(integer *, doublereal *, integer
            *, doublereal *, integer *);
    static integer nconv;
    static logical initv;
    static doublereal rnorm;
    extern /* Subroutine */ int dgetv0_(integer *, char *, integer *, logical
            *, integer *, integer *, doublereal *, integer *, doublereal *,
            doublereal *, integer *, doublereal *, integer *, ftnlen);
    extern doublereal dlamch_(char *, ftnlen);
    integer nevbef;
    extern /* Subroutine */ int second_(real *);
    static logical update;
    char wprime[2];
    static logical ushift;
    static integer kplusp /*, msglvl */;
    integer nptemp;
    extern /* Subroutine */ int dsaitr_(integer *, char *, integer *, integer
            *, integer *, integer *, doublereal *, doublereal *, doublereal *,
             integer *, doublereal *, integer *, integer *, doublereal *,
            integer *, ftnlen), dsconv_(integer *, doublereal *, doublereal *,
             doublereal *, integer *), dseigt_(doublereal *, integer *,
            doublereal *, integer *, doublereal *, doublereal *, doublereal *,
             integer *), dsgets_(integer *, char *, integer *, integer *,
            doublereal *, doublereal *, doublereal *, ftnlen), dsapps_(
            integer *, integer *, integer *, doublereal *, doublereal *,
            integer *, doublereal *, integer *, doublereal *, doublereal *,
            integer *, doublereal *), dsortr_(char *, logical *, integer *,
            doublereal *, doublereal *, ftnlen);


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
/*<       character  bmat*1, which*2 >*/

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
/*<        >*/
/*<        >*/

/*     %-----------------% */
/*     | Array Arguments | */
/*     %-----------------% */

/*<       integer    ipntr(3) >*/
/*<        >*/

/*     %------------% */
/*     | Parameters | */
/*     %------------% */

/*<        >*/
/*<       parameter (one = 1.0D+0, zero = 0.0D+0) >*/

/*     %---------------% */
/*     | Local Scalars | */
/*     %---------------% */

/*<       character  wprime*2 >*/
/*<       logical    cnorm, getv0, initv, update, ushift >*/
/*<        >*/
/*<        >*/
/*<        >*/

/*     %----------------------% */
/*     | External Subroutines | */
/*     %----------------------% */

/*<        >*/

/*     %--------------------% */
/*     | External Functions | */
/*     %--------------------% */

/*<        >*/
/*<       external   ddot, dnrm2, dlamch >*/

/*     %---------------------% */
/*     | Intrinsic Functions | */
/*     %---------------------% */

/*<       intrinsic    min >*/

/*     %-----------------------% */
/*     | Executable Statements | */
/*     %-----------------------% */

/*<       if (ido .eq. 0) then >*/
    /* Parameter adjustments */
    --workd;
    --resid;
    --workl;
    --bounds;
    --ritz;
    v_dim1 = *ldv;
    v_offset = 1 + v_dim1;
    v -= v_offset;
    h_dim1 = *ldh;
    h_offset = 1 + h_dim1;
    h__ -= h_offset;
    q_dim1 = *ldq;
    q_offset = 1 + q_dim1;
    q -= q_offset;
    --ipntr;

    /* Function Body */
    if (*ido == 0) {

/*        %-------------------------------% */
/*        | Initialize timing statistics  | */
/*        | & message level for debugging | */
/*        %-------------------------------% */

/*<          call second (t0) >*/
/*      second_(&t0); */
/*<          msglvl = msaup2 >*/
/*      msglvl = debug_1.msaup2; */

/*        %---------------------------------% */
/*        | Set machine dependent constant. | */
/*        %---------------------------------% */

/*<          eps23 = dlamch('Epsilon-Machine') >*/
        eps23 = dlamch_("Epsilon-Machine", (ftnlen)15);
/*<          eps23 = eps23**(2.0D+0/3.0D+0) >*/
        eps23 = pow_dd(&eps23, &c_b3);

/*        %-------------------------------------% */
/*        | nev0 and np0 are integer variables  | */
/*        | hold the initial values of NEV & NP | */
/*        %-------------------------------------% */

/*<          nev0   = nev >*/
        nev0 = *nev;
/*<          np0    = np >*/
        np0 = *np;

/*        %-------------------------------------% */
/*        | kplusp is the bound on the largest  | */
/*        |        Lanczos factorization built. | */
/*        | nconv is the current number of      | */
/*        |        "converged" eigenvlues.      | */
/*        | iter is the counter on the current  | */
/*        |      iteration step.                | */
/*        %-------------------------------------% */

/*<          kplusp = nev0 + np0 >*/
        kplusp = nev0 + np0;
/*<          nconv  = 0 >*/
        nconv = 0;
/*<          iter   = 0 >*/
        iter = 0;

/*        %--------------------------------------------% */
/*        | Set flags for computing the first NEV steps | */
/*        | of the Lanczos factorization.              | */
/*        %--------------------------------------------% */

/*<          getv0    = .true. >*/
        getv0 = TRUE_;
/*<          update   = .false. >*/
        update = FALSE_;
/*<          ushift   = .false. >*/
        ushift = FALSE_;
/*<          cnorm    = .false. >*/
        cnorm = FALSE_;

/*<          if (info .ne. 0) then >*/
        if (*info != 0) {

/*        %--------------------------------------------% */
/*        | User provides the initial residual vector. | */
/*        %--------------------------------------------% */

/*<             initv = .true. >*/
            initv = TRUE_;
/*<             info  = 0 >*/
            *info = 0;
/*<          else >*/
        } else {
/*<             initv = .false. >*/
            initv = FALSE_;
/*<          end if >*/
        }
/*<       end if >*/
    }

/*     %---------------------------------------------% */
/*     | Get a possibly random starting vector and   | */
/*     | force it into the range of the operator OP. | */
/*     %---------------------------------------------% */

/*<    10 continue >*/
/* L10: */

/*<       if (getv0) then >*/
    if (getv0) {
/*<        >*/
        dgetv0_(ido, bmat, &c__1, &initv, n, &c__1, &v[v_offset], ldv, &resid[
                1], &rnorm, &ipntr[1], &workd[1], info, (ftnlen)1);

/*<          if (ido .ne. 99) go to 9000 >*/
        if (*ido != 99) {
            goto L9000;
        }

/*<          if (rnorm .eq. zero) then >*/
        if (rnorm == 0.) {

/*           %-----------------------------------------% */
/*           | The initial vector is zero. Error exit. | */
/*           %-----------------------------------------% */

/*<             info = -9 >*/
            *info = -9;
/*<             go to 1200 >*/
            goto L1200;
/*<          end if >*/
        }
/*<          getv0 = .false. >*/
        getv0 = FALSE_;
/*<          ido  = 0 >*/
        *ido = 0;
/*<       end if >*/
    }

/*     %------------------------------------------------------------% */
/*     | Back from reverse communication: continue with update step | */
/*     %------------------------------------------------------------% */

/*<       if (update) go to 20 >*/
    if (update) {
        goto L20;
    }

/*     %-------------------------------------------% */
/*     | Back from computing user specified shifts | */
/*     %-------------------------------------------% */

/*<       if (ushift) go to 50 >*/
    if (ushift) {
        goto L50;
    }

/*     %-------------------------------------% */
/*     | Back from computing residual norm   | */
/*     | at the end of the current iteration | */
/*     %-------------------------------------% */

/*<       if (cnorm)  go to 100 >*/
    if (cnorm) {
        goto L100;
    }

/*     %----------------------------------------------------------% */
/*     | Compute the first NEV steps of the Lanczos factorization | */
/*     %----------------------------------------------------------% */

/*<        >*/
    dsaitr_(ido, bmat, n, &c__0, &nev0, mode, &resid[1], &rnorm, &v[v_offset],
             ldv, &h__[h_offset], ldh, &ipntr[1], &workd[1], info, (ftnlen)1);

/*     %---------------------------------------------------% */
/*     | ido .ne. 99 implies use of reverse communication  | */
/*     | to compute operations involving OP and possibly B | */
/*     %---------------------------------------------------% */

/*<       if (ido .ne. 99) go to 9000 >*/
    if (*ido != 99) {
        goto L9000;
    }

/*<       if (info .gt. 0) then >*/
    if (*info > 0) {

/*        %-----------------------------------------------------% */
/*        | dsaitr was unable to build an Lanczos factorization | */
/*        | of length NEV0. INFO is returned with the size of   | */
/*        | the factorization built. Exit main loop.            | */
/*        %-----------------------------------------------------% */

/*<          np   = info >*/
        *np = *info;
/*<          mxiter = iter >*/
        *mxiter = iter;
/*<          info = -9999 >*/
        *info = -9999;
/*<          go to 1200 >*/
        goto L1200;
/*<       end if >*/
    }

/*     %--------------------------------------------------------------% */
/*     |                                                              | */
/*     |           M A I N  LANCZOS  I T E R A T I O N  L O O P       | */
/*     |           Each iteration implicitly restarts the Lanczos     | */
/*     |           factorization in place.                            | */
/*     |                                                              | */
/*     %--------------------------------------------------------------% */

/*<  1000 continue >*/
L1000:

/*<          iter = iter + 1 >*/
    ++iter;

/*         if (msglvl .gt. 0) then */
/*            call ivout (logfil, 1, iter, ndigit, */
/*     &           '_saup2: **** Start of major iteration number ****') */
/*         end if */
/*         if (msglvl .gt. 1) then */
/*            call ivout (logfil, 1, nev, ndigit, */
/*     &     '_saup2: The length of the current Lanczos factorization') */
/*            call ivout (logfil, 1, np, ndigit, */
/*     &           '_saup2: Extend the Lanczos factorization by') */
/*         end if */

/*        %------------------------------------------------------------% */
/*        | Compute NP additional steps of the Lanczos factorization. | */
/*        %------------------------------------------------------------% */

/*<          ido = 0 >*/
    *ido = 0;
/*<    20    continue >*/
L20:
/*<          update = .true. >*/
    update = TRUE_;

/*<        >*/
    dsaitr_(ido, bmat, n, nev, np, mode, &resid[1], &rnorm, &v[v_offset], ldv,
             &h__[h_offset], ldh, &ipntr[1], &workd[1], info, (ftnlen)1);

/*        %---------------------------------------------------% */
/*        | ido .ne. 99 implies use of reverse communication  | */
/*        | to compute operations involving OP and possibly B | */
/*        %---------------------------------------------------% */

/*<          if (ido .ne. 99) go to 9000 >*/
    if (*ido != 99) {
        goto L9000;
    }

/*<          if (info .gt. 0) then >*/
    if (*info > 0) {

/*           %-----------------------------------------------------% */
/*           | dsaitr was unable to build an Lanczos factorization | */
/*           | of length NEV0+NP0. INFO is returned with the size  | */
/*           | of the factorization built. Exit main loop.         | */
/*           %-----------------------------------------------------% */

/*<             np = info >*/
        *np = *info;
/*<             mxiter = iter >*/
        *mxiter = iter;
/*<             info = -9999 >*/
        *info = -9999;
/*<             go to 1200 >*/
        goto L1200;
/*<          end if >*/
    }
/*<          update = .false. >*/
    update = FALSE_;

/*         if (msglvl .gt. 1) then */
/*            call dvout (logfil, 1, rnorm, ndigit, */
/*     &           '_saup2: Current B-norm of residual for factorization') */
/*         end if */

/*        %--------------------------------------------------------% */
/*        | Compute the eigenvalues and corresponding error bounds | */
/*        | of the current symmetric tridiagonal matrix.           | */
/*        %--------------------------------------------------------% */

/*<          call dseigt (rnorm, kplusp, h, ldh, ritz, bounds, workl, ierr) >*/
    dseigt_(&rnorm, &kplusp, &h__[h_offset], ldh, &ritz[1], &bounds[1], &
            workl[1], &ierr);

/*<          if (ierr .ne. 0) then >*/
    if (ierr != 0) {
/*<             info = -8 >*/
        *info = -8;
/*<             go to 1200 >*/
        goto L1200;
/*<          end if >*/
    }

/*        %----------------------------------------------------% */
/*        | Make a copy of eigenvalues and corresponding error | */
/*        | bounds obtained from _seigt.                       | */
/*        %----------------------------------------------------% */

/*<          call dcopy(kplusp, ritz, 1, workl(kplusp+1), 1) >*/
    dcopy_(&kplusp, &ritz[1], &c__1, &workl[kplusp + 1], &c__1);
/*<          call dcopy(kplusp, bounds, 1, workl(2*kplusp+1), 1) >*/
    dcopy_(&kplusp, &bounds[1], &c__1, &workl[(kplusp << 1) + 1], &c__1);

/*        %---------------------------------------------------% */
/*        | Select the wanted Ritz values and their bounds    | */
/*        | to be used in the convergence test.               | */
/*        | The selection is based on the requested number of | */
/*        | eigenvalues instead of the current NEV and NP to  | */
/*        | prevent possible misconvergence.                  | */
/*        | * Wanted Ritz values := RITZ(NP+1:NEV+NP)         | */
/*        | * Shifts := RITZ(1:NP) := WORKL(1:NP)             | */
/*        %---------------------------------------------------% */

/*<          nev = nev0 >*/
    *nev = nev0;
/*<          np = np0 >*/
    *np = np0;
/*<          call dsgets (ishift, which, nev, np, ritz, bounds, workl) >*/
    dsgets_(ishift, which, nev, np, &ritz[1], &bounds[1], &workl[1], (ftnlen)
            2);

/*        %-------------------% */
/*        | Convergence test. | */
/*        %-------------------% */

/*<          call dcopy (nev, bounds(np+1), 1, workl(np+1), 1) >*/
    dcopy_(nev, &bounds[*np + 1], &c__1, &workl[*np + 1], &c__1);
/*<          call dsconv (nev, ritz(np+1), workl(np+1), tol, nconv) >*/
    dsconv_(nev, &ritz[*np + 1], &workl[*np + 1], tol, &nconv);

/*<          if (msglvl .gt. 2) then >*/
/*  if (msglvl > 2) { */
/*<             kp(1) = nev >*/
/*      kp[0] = *nev; */
/*<             kp(2) = np >*/
/*      kp[1] = *np; */
/*<             kp(3) = nconv >*/
/*      kp[2] = nconv; */
/*            call ivout (logfil, 3, kp, ndigit, */
/*     &                  '_saup2: NEV, NP, NCONV are') */
/*            call dvout (logfil, kplusp, ritz, ndigit, */
/*     &           '_saup2: The eigenvalues of H') */
/*            call dvout (logfil, kplusp, bounds, ndigit, */
/*     &          '_saup2: Ritz estimates of the current NCV Ritz values') */
/*<          end if >*/
/*  } */

/*        %---------------------------------------------------------% */
/*        | Count the number of unwanted Ritz values that have zero | */
/*        | Ritz estimates. If any Ritz estimates are equal to zero | */
/*        | then a leading block of H of order equal to at least    | */
/*        | the number of Ritz values with zero Ritz estimates has  | */
/*        | split off. None of these Ritz values may be removed by  | */
/*        | shifting. Decrease NP the number of shifts to apply. If | */
/*        | no shifts may be applied, then prepare to exit          | */
/*        %---------------------------------------------------------% */

/*<          nptemp = np >*/
    nptemp = *np;
/*<          do 30 j=1, nptemp >*/
    i__1 = nptemp;
    for (j = 1; j <= i__1; ++j) {
/*<             if (bounds(j) .eq. zero) then >*/
        if (bounds[j] == 0.) {
/*<                np = np - 1 >*/
            --(*np);
/*<                nev = nev + 1 >*/
            ++(*nev);
/*<             end if >*/
        }
/*<  30      continue >*/
/* L30: */
    }

/*<        >*/
    if (nconv >= nev0 || iter > *mxiter || *np == 0) {

/*           %------------------------------------------------% */
/*           | Prepare to exit. Put the converged Ritz values | */
/*           | and corresponding bounds in RITZ(1:NCONV) and  | */
/*           | BOUNDS(1:NCONV) respectively. Then sort. Be    | */
/*           | careful when NCONV > NP since we don't want to | */
/*           | swap overlapping locations.                    | */
/*           %------------------------------------------------% */

/*<             if (which .eq. 'BE') then >*/
        if (s_cmp(which, "BE", (ftnlen)2, (ftnlen)2) == 0) {

/*              %-----------------------------------------------------% */
/*              | Both ends of the spectrum are requested.            | */
/*              | Sort the eigenvalues into algebraically decreasing  | */
/*              | order first then swap low end of the spectrum next  | */
/*              | to high end in appropriate locations.               | */
/*              | NOTE: when np < floor(nev/2) be careful not to swap | */
/*              | overlapping locations.                              | */
/*              %-----------------------------------------------------% */

/*<                wprime = 'SA' >*/
            s_copy(wprime, "SA", (ftnlen)2, (ftnlen)2);
/*<                call dsortr (wprime, .true., kplusp, ritz, bounds) >*/
            dsortr_(wprime, &c_true, &kplusp, &ritz[1], &bounds[1], (ftnlen)2)
                    ;
/*<                nevd2 = nev / 2 >*/
            nevd2 = *nev / 2;
/*<                nevm2 = nev - nevd2  >*/
            nevm2 = *nev - nevd2;
/*<                if ( nev .gt. 1 ) then >*/
            if (*nev > 1) {
/*<        >*/
                i__1 = min(nevd2,*np);
/* Computing MAX */
                i__2 = kplusp - nevd2 + 1, i__3 = kplusp - *np + 1;
                dswap_(&i__1, &ritz[nevm2 + 1], &c__1, &ritz[max(i__2,i__3)],
                        &c__1);
/*<        >*/
                i__1 = min(nevd2,*np);
/* Computing MAX */
                i__2 = kplusp - nevd2 + 1, i__3 = kplusp - *np;
                dswap_(&i__1, &bounds[nevm2 + 1], &c__1, &bounds[max(i__2,
                        i__3) + 1], &c__1);
/*<                end if >*/
            }

/*<             else >*/
        } else {

/*              %--------------------------------------------------% */
/*              | LM, SM, LA, SA case.                             | */
/*              | Sort the eigenvalues of H into the an order that | */
/*              | is opposite to WHICH, and apply the resulting    | */
/*              | order to BOUNDS.  The eigenvalues are sorted so  | */
/*              | that the wanted part are always within the first | */
/*              | NEV locations.                                   | */
/*              %--------------------------------------------------% */

/*<                if (which .eq. 'LM') wprime = 'SM' >*/
            if (s_cmp(which, "LM", (ftnlen)2, (ftnlen)2) == 0) {
                s_copy(wprime, "SM", (ftnlen)2, (ftnlen)2);
            }
/*<                if (which .eq. 'SM') wprime = 'LM' >*/
            if (s_cmp(which, "SM", (ftnlen)2, (ftnlen)2) == 0) {
                s_copy(wprime, "LM", (ftnlen)2, (ftnlen)2);
            }
/*<                if (which .eq. 'LA') wprime = 'SA' >*/
            if (s_cmp(which, "LA", (ftnlen)2, (ftnlen)2) == 0) {
                s_copy(wprime, "SA", (ftnlen)2, (ftnlen)2);
            }
/*<                if (which .eq. 'SA') wprime = 'LA' >*/
            if (s_cmp(which, "SA", (ftnlen)2, (ftnlen)2) == 0) {
                s_copy(wprime, "LA", (ftnlen)2, (ftnlen)2);
            }

/*<                call dsortr (wprime, .true., kplusp, ritz, bounds) >*/
            dsortr_(wprime, &c_true, &kplusp, &ritz[1], &bounds[1], (ftnlen)2)
                    ;

/*<             end if >*/
        }

/*           %--------------------------------------------------% */
/*           | Scale the Ritz estimate of each Ritz value       | */
/*           | by 1 / max(eps23,magnitude of the Ritz value).   | */
/*           %--------------------------------------------------% */

/*<             do 35 j = 1, nev0 >*/
        i__1 = nev0;
        for (j = 1; j <= i__1; ++j) {
/*<                temp = max( eps23, abs(ritz(j)) ) >*/
/* Computing MAX */
            d__2 = eps23, d__3 = (d__1 = ritz[j], abs(d__1));
            temp = max(d__2,d__3);
/*<                bounds(j) = bounds(j)/temp >*/
            bounds[j] /= temp;
/*<  35         continue >*/
/* L35: */
        }

/*           %----------------------------------------------------% */
/*           | Sort the Ritz values according to the scaled Ritz  | */
/*           | esitmates.  This will push all the converged ones  | */
/*           | towards the front of ritzr, ritzi, bounds          | */
/*           | (in the case when NCONV < NEV.)                    | */
/*           %----------------------------------------------------% */

/*<             wprime = 'LA' >*/
        s_copy(wprime, "LA", (ftnlen)2, (ftnlen)2);
/*<             call dsortr(wprime, .true., nev0, bounds, ritz) >*/
        dsortr_(wprime, &c_true, &nev0, &bounds[1], &ritz[1], (ftnlen)2);

/*           %----------------------------------------------% */
/*           | Scale the Ritz estimate back to its original | */
/*           | value.                                       | */
/*           %----------------------------------------------% */

/*<             do 40 j = 1, nev0 >*/
        i__1 = nev0;
        for (j = 1; j <= i__1; ++j) {
/*<                 temp = max( eps23, abs(ritz(j)) ) >*/
/* Computing MAX */
            d__2 = eps23, d__3 = (d__1 = ritz[j], abs(d__1));
            temp = max(d__2,d__3);
/*<                 bounds(j) = bounds(j)*temp >*/
            bounds[j] *= temp;
/*<  40         continue >*/
/* L40: */
        }

/*           %--------------------------------------------------% */
/*           | Sort the "converged" Ritz values again so that   | */
/*           | the "threshold" values and their associated Ritz | */
/*           | estimates appear at the appropriate position in  | */
/*           | ritz and bound.                                  | */
/*           %--------------------------------------------------% */

/*<             if (which .eq. 'BE') then >*/
        if (s_cmp(which, "BE", (ftnlen)2, (ftnlen)2) == 0) {

/*              %------------------------------------------------% */
/*              | Sort the "converged" Ritz values in increasing | */
/*              | order.  The "threshold" values are in the      | */
/*              | middle.                                        | */
/*              %------------------------------------------------% */

/*<                wprime = 'LA' >*/
            s_copy(wprime, "LA", (ftnlen)2, (ftnlen)2);
/*<                call dsortr(wprime, .true., nconv, ritz, bounds) >*/
            dsortr_(wprime, &c_true, &nconv, &ritz[1], &bounds[1], (ftnlen)2);

/*<             else >*/
        } else {

/*              %----------------------------------------------% */
/*              | In LM, SM, LA, SA case, sort the "converged" | */
/*              | Ritz values according to WHICH so that the   | */
/*              | "threshold" value appears at the front of    | */
/*              | ritz.                                        | */
/*              %----------------------------------------------% */
/*<                call dsortr(which, .true., nconv, ritz, bounds) >*/
            dsortr_(which, &c_true, &nconv, &ritz[1], &bounds[1], (ftnlen)2);

/*<             end if >*/
        }

/*           %------------------------------------------% */
/*           |  Use h( 1,1 ) as storage to communicate  | */
/*           |  rnorm to _seupd if needed               | */
/*           %------------------------------------------% */

/*<             h(1,1) = rnorm >*/
        h__[h_dim1 + 1] = rnorm;

/*            if (msglvl .gt. 1) then */
/*               call dvout (logfil, kplusp, ritz, ndigit, */
/*     &            '_saup2: Sorted Ritz values.') */
/*               call dvout (logfil, kplusp, bounds, ndigit, */
/*     &            '_saup2: Sorted ritz estimates.') */
/*            end if */

/*           %------------------------------------% */
/*           | Max iterations have been exceeded. | */
/*           %------------------------------------% */

/*<             if (iter .gt. mxiter .and. nconv .lt. nev) info = 1 >*/
        if (iter > *mxiter && nconv < *nev) {
            *info = 1;
        }

/*           %---------------------% */
/*           | No shifts to apply. | */
/*           %---------------------% */

/*<             if (np .eq. 0 .and. nconv .lt. nev0) info = 2 >*/
        if (*np == 0 && nconv < nev0) {
            *info = 2;
        }

/*<             np = nconv >*/
        *np = nconv;
/*<             go to 1100 >*/
        goto L1100;

/*<          else if (nconv .lt. nev .and. ishift .eq. 1) then >*/
    } else if (nconv < *nev && *ishift == 1) {

/*           %---------------------------------------------------% */
/*           | Do not have all the requested eigenvalues yet.    | */
/*           | To prevent possible stagnation, adjust the number | */
/*           | of Ritz values and the shifts.                    | */
/*           %---------------------------------------------------% */

/*<             nevbef = nev >*/
        nevbef = *nev;
/*<             nev = nev + min (nconv, np/2) >*/
/* Computing MIN */
        i__1 = nconv, i__2 = *np / 2;
        *nev += min(i__1,i__2);
/*<             if (nev .eq. 1 .and. kplusp .ge. 6) then >*/
        if (*nev == 1 && kplusp >= 6) {
/*<                nev = kplusp / 2 >*/
            *nev = kplusp / 2;
/*<             else if (nev .eq. 1 .and. kplusp .gt. 2) then >*/
        } else if (*nev == 1 && kplusp > 2) {
/*<                nev = 2 >*/
            *nev = 2;
/*<             end if >*/
        }
/*<             np  = kplusp - nev >*/
        *np = kplusp - *nev;

/*           %---------------------------------------% */
/*           | If the size of NEV was just increased | */
/*           | resort the eigenvalues.               | */
/*           %---------------------------------------% */

/*<        >*/
        if (nevbef < *nev) {
            dsgets_(ishift, which, nev, np, &ritz[1], &bounds[1], &workl[1], (
                    ftnlen)2);
        }

/*<          end if >*/
    }

/*<          if (msglvl .gt. 0) then >*/
/*  if (msglvl > 0) { */
/*            call ivout (logfil, 1, nconv, ndigit, */
/*     &           '_saup2: no. of "converged" Ritz values at this iter.') */
/*<             if (msglvl .gt. 1) then >*/
/*      if (msglvl > 1) { */
/*<                kp(1) = nev >*/
/*          kp[0] = *nev; */
/*<                kp(2) = np >*/
/*          kp[1] = *np; */
/*               call ivout (logfil, 2, kp, ndigit, */
/*     &              '_saup2: NEV and NP are') */
/*               call dvout (logfil, nev, ritz(np+1), ndigit, */
/*     &              '_saup2: "wanted" Ritz values.') */
/*               call dvout (logfil, nev, bounds(np+1), ndigit, */
/*     &              '_saup2: Ritz estimates of the "wanted" values ') */
/*<             end if >*/
/*      } */
/*<          end if >*/
/*  } */

/*<          if (ishift .eq. 0) then >*/
    if (*ishift == 0) {

/*           %-----------------------------------------------------% */
/*           | User specified shifts: reverse communication to     | */
/*           | compute the shifts. They are returned in the first  | */
/*           | NP locations of WORKL.                              | */
/*           %-----------------------------------------------------% */

/*<             ushift = .true. >*/
        ushift = TRUE_;
/*<             ido = 3 >*/
        *ido = 3;
/*<             go to 9000 >*/
        goto L9000;
/*<          end if >*/
    }

/*<    50    continue >*/
L50:

/*        %------------------------------------% */
/*        | Back from reverse communication;   | */
/*        | User specified shifts are returned | */
/*        | in WORKL(1:*NP)                   | */
/*        %------------------------------------% */

/*<          ushift = .false. >*/
    ushift = FALSE_;


/*        %---------------------------------------------------------% */
/*        | Move the NP shifts to the first NP locations of RITZ to | */
/*        | free up WORKL.  This is for the non-exact shift case;   | */
/*        | in the exact shift case, dsgets already handles this.   | */
/*        %---------------------------------------------------------% */

/*<          if (ishift .eq. 0) call dcopy (np, workl, 1, ritz, 1) >*/
    if (*ishift == 0) {
        dcopy_(np, &workl[1], &c__1, &ritz[1], &c__1);
    }

/*         if (msglvl .gt. 2) then */
/*            call ivout (logfil, 1, np, ndigit, */
/*     &                  '_saup2: The number of shifts to apply ') */
/*            call dvout (logfil, np, workl, ndigit, */
/*     &                  '_saup2: shifts selected') */
/*            if (ishift .eq. 1) then */
/*               call dvout (logfil, np, bounds, ndigit, */
/*     &                  '_saup2: corresponding Ritz estimates') */
/*             end if */
/*         end if */

/*        %---------------------------------------------------------% */
/*        | Apply the NP0 implicit shifts by QR bulge chasing.      | */
/*        | Each shift is applied to the entire tridiagonal matrix. | */
/*        | The first 2*N locations of WORKD are used as workspace. | */
/*        | After dsapps is done, we have a Lanczos                 | */
/*        | factorization of length NEV.                            | */
/*        %---------------------------------------------------------% */

/*<        >*/
    dsapps_(n, nev, np, &ritz[1], &v[v_offset], ldv, &h__[h_offset], ldh, &
            resid[1], &q[q_offset], ldq, &workd[1]);

/*        %---------------------------------------------% */
/*        | Compute the B-norm of the updated residual. | */
/*        | Keep B*RESID in WORKD(1:N) to be used in    | */
/*        | the first step of the next call to dsaitr.  | */
/*        %---------------------------------------------% */

/*<          cnorm = .true. >*/
    cnorm = TRUE_;
/*<          call second (t2) >*/
/*  second_(&t2); */
/*<          if (bmat .eq. 'G') then >*/
    if (*(unsigned char *)bmat == 'G') {
/*<             nbx = nbx + 1 >*/
/*      ++timing_1.nbx; */
/*<             call dcopy (n, resid, 1, workd(n+1), 1) >*/
        dcopy_(n, &resid[1], &c__1, &workd[*n + 1], &c__1);
/*<             ipntr(1) = n + 1 >*/
        ipntr[1] = *n + 1;
/*<             ipntr(2) = 1 >*/
        ipntr[2] = 1;
/*<             ido = 2 >*/
        *ido = 2;

/*           %----------------------------------% */
/*           | Exit in order to compute B*RESID | */
/*           %----------------------------------% */

/*<             go to 9000 >*/
        goto L9000;
/*<          else if (bmat .eq. 'I') then >*/
    } else if (*(unsigned char *)bmat == 'I') {
/*<             call dcopy (n, resid, 1, workd, 1) >*/
        dcopy_(n, &resid[1], &c__1, &workd[1], &c__1);
/*<          end if >*/
    }

/*<   100    continue >*/
L100:

/*        %----------------------------------% */
/*        | Back from reverse communication; | */
/*        | WORKD(1:N) := B*RESID            | */
/*        %----------------------------------% */

/*<          if (bmat .eq. 'G') then >*/
    if (*(unsigned char *)bmat == 'G') {
/*<             call second (t3) >*/
/*      second_(&t3); */
/*<             tmvbx = tmvbx + (t3 - t2) >*/
/*      timing_1.tmvbx += t3 - t2; */
/*<          end if >*/
    }

/*<          if (bmat .eq. 'G') then          >*/
    if (*(unsigned char *)bmat == 'G') {
/*<             rnorm = ddot (n, resid, 1, workd, 1) >*/
        rnorm = ddot_(n, &resid[1], &c__1, &workd[1], &c__1);
/*<             rnorm = sqrt(abs(rnorm)) >*/
        rnorm = sqrt((abs(rnorm)));
/*<          else if (bmat .eq. 'I') then >*/
    } else if (*(unsigned char *)bmat == 'I') {
/*<             rnorm = dnrm2(n, resid, 1) >*/
        rnorm = dnrm2_(n, &resid[1], &c__1);
/*<          end if >*/
    }
/*<          cnorm = .false. >*/
    cnorm = FALSE_;
/*<   130    continue >*/
/* L130: */

/*         if (msglvl .gt. 2) then */
/*            call dvout (logfil, 1, rnorm, ndigit, */
/*     &      '_saup2: B-norm of residual for NEV factorization') */
/*            call dvout (logfil, nev, h(1,2), ndigit, */
/*     &           '_saup2: main diagonal of compressed H matrix') */
/*            call dvout (logfil, nev-1, h(2,1), ndigit, */
/*     &           '_saup2: subdiagonal of compressed H matrix') */
/*         end if */

/*<       go to 1000 >*/
    goto L1000;

/*     %---------------------------------------------------------------% */
/*     |                                                               | */
/*     |  E N D     O F     M A I N     I T E R A T I O N     L O O P  | */
/*     |                                                               | */
/*     %---------------------------------------------------------------% */

/*<  1100 continue >*/
L1100:

/*<       mxiter = iter >*/
    *mxiter = iter;
/*<       nev = nconv >*/
    *nev = nconv;

/*<  1200 continue >*/
L1200:
/*<       ido = 99 >*/
    *ido = 99;

/*     %------------% */
/*     | Error exit | */
/*     %------------% */

/*<       call second (t1) >*/
/*  second_(&t1); */
/*<       tsaup2 = t1 - t0 >*/
/*  timing_1.tsaup2 = t1 - t0; */

/*<  9000 continue >*/
L9000:
/*<       return >*/
    return 0;

/*     %---------------% */
/*     | End of dsaup2 | */
/*     %---------------% */

/*<       end >*/
} /* dsaup2_ */

#ifdef __cplusplus
        }
#endif
