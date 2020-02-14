/* arpack/dgetv0.f -- translated by f2c (version 20090411).
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

static integer c__1 = 1;
static doublereal c_b24 = 1.;
static doublereal c_b26 = 0.;
static doublereal c_b29 = -1.;

/* ----------------------------------------------------------------------- */
/* \BeginDoc */

/* \Name: dgetv0 */

/* \Description: */
/*  Generate a random initial residual vector for the Arnoldi process. */
/*  Force the residual vector to be in the range of the operator OP. */

/* \Usage: */
/*  call dgetv0 */
/*     ( IDO, BMAT, ITRY, INITV, N, J, V, LDV, RESID, RNORM, */
/*       IPNTR, WORKD, IERR ) */

/* \Arguments */
/*  IDO     Integer.  (INPUT/OUTPUT) */
/*          Reverse communication flag.  IDO must be zero on the first */
/*          call to dgetv0. */
/*          ------------------------------------------------------------- */
/*          IDO =  0: first call to the reverse communication interface */
/*          IDO = -1: compute  Y = OP * X  where */
/*                    IPNTR(1) is the pointer into WORKD for X, */
/*                    IPNTR(2) is the pointer into WORKD for Y. */
/*                    This is for the initialization phase to force the */
/*                    starting vector into the range of OP. */
/*          IDO =  2: compute  Y = B * X  where */
/*                    IPNTR(1) is the pointer into WORKD for X, */
/*                    IPNTR(2) is the pointer into WORKD for Y. */
/*          IDO = 99: done */
/*          ------------------------------------------------------------- */

/*  BMAT    Character*1.  (INPUT) */
/*          BMAT specifies the type of the matrix B in the (generalized) */
/*          eigenvalue problem A*x = lambda*B*x. */
/*          B = 'I' -> standard eigenvalue problem A*x = lambda*x */
/*          B = 'G' -> generalized eigenvalue problem A*x = lambda*B*x */

/*  ITRY    Integer.  (INPUT) */
/*          ITRY counts the number of times that dgetv0 is called. */
/*          It should be set to 1 on the initial call to dgetv0. */

/*  INITV   Logical variable.  (INPUT) */
/*          .TRUE.  => the initial residual vector is given in RESID. */
/*          .FALSE. => generate a random initial residual vector. */

/*  N       Integer.  (INPUT) */
/*          Dimension of the problem. */

/*  J       Integer.  (INPUT) */
/*          Index of the residual vector to be generated, with respect to */
/*          the Arnoldi process.  J > 1 in case of a "restart". */

/*  V       Double precision N by J array.  (INPUT) */
/*          The first J-1 columns of V contain the current Arnoldi basis */
/*          if this is a "restart". */

/*  LDV     Integer.  (INPUT) */
/*          Leading dimension of V exactly as declared in the calling */
/*          program. */

/*  RESID   Double precision array of length N.  (INPUT/OUTPUT) */
/*          Initial residual vector to be generated.  If RESID is */
/*          provided, force RESID into the range of the operator OP. */

/*  RNORM   Double precision scalar.  (OUTPUT) */
/*          B-norm of the generated residual. */

/*  IPNTR   Integer array of length 3.  (OUTPUT) */

/*  WORKD   Double precision work array of length 2*N.  (REVERSE COMMUNICATION). */
/*          On exit, WORK(1:N) = B*RESID to be used in SSAITR. */

/*  IERR    Integer.  (OUTPUT) */
/*          =  0: Normal exit. */
/*          = -1: Cannot generate a nontrivial restarted residual vector */
/*                in the range of the operator OP. */

/* \EndDoc */

/* ----------------------------------------------------------------------- */

/* \BeginLib */

/* \Local variables: */
/*     xxxxxx  real */

/* \References: */
/*  1. D.C. Sorensen, "Implicit Application of Polynomial Filters in */
/*     a k-Step Arnoldi Method", SIAM J. Matr. Anal. Apps., 13 (1992), */
/*     pp 357-385. */
/*  2. R.B. Lehoucq, "Analysis and Implementation of an Implicitly */
/*     Restarted Arnoldi Iteration", Rice University Technical Report */
/*     TR95-13, Department of Computational and Applied Mathematics. */

/* \Routines called: */
/*     second  ARPACK utility routine for timing. */
/*     dlarnv  LAPACK routine for generating a random vector. */
/*     dgemv   Level 2 BLAS routine for matrix vector multiplication. */
/*     dcopy   Level 1 BLAS that copies one vector to another. */
/*     ddot    Level 1 BLAS that computes the scalar product of two vectors. */
/*     dnrm2   Level 1 BLAS that computes the norm of a vector. */

/* \Author */
/*     Danny Sorensen               Phuong Vu */
/*     Richard Lehoucq              CRPC / Rice University */
/*     Dept. of Computational &     Houston, Texas */
/*     Applied Mathematics */
/*     Rice University */
/*     Houston, Texas */

/* \SCCS Information: @(#) */
/* FILE: getv0.F   SID: 2.6   DATE OF SID: 8/27/96   RELEASE: 2 */

/* \EndLib */

/* ----------------------------------------------------------------------- */

/*<        >*/
/* Subroutine */ int dgetv0_(integer *ido, char *bmat, integer *itry, logical
        *initv, integer *n, integer *j, doublereal *v, integer *ldv,
        doublereal *resid, doublereal *rnorm, integer *ipntr, doublereal *
        workd, integer *ierr, ftnlen bmat_len)
{
    /* Initialized data */

    static logical inits = TRUE_;

    /* System generated locals */
    integer v_dim1, v_offset, i__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
/*  static real t0, t1, t2, t3; */
    integer jj;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *,
            integer *);
    static integer iter;
    static logical orth;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    static integer iseed[4];
    extern /* Subroutine */ int dgemv_(char *, integer *, integer *,
            doublereal *, doublereal *, integer *, doublereal *, integer *,
            doublereal *, doublereal *, integer *, ftnlen);
    integer idist;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *,
            doublereal *, integer *);
    static logical first;
    static doublereal rnorm0;
/*  static integer msglvl; */
    extern /* Subroutine */ int dlarnv_(integer *, integer *, integer *,
            doublereal *);


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
/*<       character  bmat*1 >*/

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
/*<       logical    initv >*/
/*<       integer    ido, ierr, itry, j, ldv, n >*/
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

/*     %------------------------% */
/*     | Local Scalars & Arrays | */
/*     %------------------------% */

/*<       logical    first, inits, orth >*/
/*<       integer    idist, iseed(4), iter, msglvl, jj >*/
/*<        >*/
/*<       save       first, iseed, inits, iter, msglvl, orth, rnorm0 >*/

/*     %----------------------% */
/*     | External Subroutines | */
/*     %----------------------% */

/*<       external   dlarnv, dcopy, dgemv, second >*/

/*     %--------------------% */
/*     | External Functions | */
/*     %--------------------% */

/*<        >*/
/*<       external   ddot, dnrm2 >*/

/*     %---------------------% */
/*     | Intrinsic Functions | */
/*     %---------------------% */

/*<       intrinsic    abs, sqrt >*/

/*     %-----------------% */
/*     | Data Statements | */
/*     %-----------------% */

/*<       data       inits /.true./ >*/
    /* Parameter adjustments */
    --workd;
    --resid;
    v_dim1 = *ldv;
    v_offset = 1 + v_dim1;
    v -= v_offset;
    --ipntr;

    /* Function Body */

/*     %-----------------------% */
/*     | Executable Statements | */
/*     %-----------------------% */


/*     %-----------------------------------% */
/*     | Initialize the seed of the LAPACK | */
/*     | random number generator           | */
/*     %-----------------------------------% */

/*<       if (inits) then >*/
    if (inits) {
/*<           iseed(1) = 1 >*/
        iseed[0] = 1;
/*<           iseed(2) = 3 >*/
        iseed[1] = 3;
/*<           iseed(3) = 5 >*/
        iseed[2] = 5;
/*<           iseed(4) = 7 >*/
        iseed[3] = 7;
/*<           inits = .false. >*/
        inits = FALSE_;
/*<       end if >*/
    }

/*<       if (ido .eq.  0) then >*/
    if (*ido == 0) {

/*        %-------------------------------% */
/*        | Initialize timing statistics  | */
/*        | & message level for debugging | */
/*        %-------------------------------% */

/*<          call second (t0) >*/
/*      second_(&t0); */
/*<          msglvl = mgetv0 >*/
/*      msglvl = debug_1.mgetv0; */

/*<          ierr   = 0 >*/
        *ierr = 0;
/*<          iter   = 0 >*/
        iter = 0;
/*<          first  = .FALSE. >*/
        first = FALSE_;
/*<          orth   = .FALSE. >*/
        orth = FALSE_;

/*        %-----------------------------------------------------% */
/*        | Possibly generate a random starting vector in RESID | */
/*        | Use a LAPACK random number generator used by the    | */
/*        | matrix generation routines.                         | */
/*        |    idist = 1: uniform (0,1)  distribution;          | */
/*        |    idist = 2: uniform (-1,1) distribution;          | */
/*        |    idist = 3: normal  (0,1)  distribution;          | */
/*        %-----------------------------------------------------% */

/*<          if (.not.initv) then >*/
        if (! (*initv)) {
/*<             idist = 2 >*/
            idist = 2;
/*<             call dlarnv (idist, iseed, n, resid) >*/
            dlarnv_(&idist, iseed, n, &resid[1]);
/*<          end if >*/
        }

/*        %----------------------------------------------------------% */
/*        | Force the starting vector into the range of OP to handle | */
/*        | the generalized problem when B is possibly (singular).   | */
/*        %----------------------------------------------------------% */

/*<          call second (t2) >*/
/*      second_(&t2); */
/*<          if (bmat .eq. 'G') then >*/
        if (*(unsigned char *)bmat == 'G') {
/*<             nopx = nopx + 1 >*/
/*          ++timing_1.nopx; */
/*<             ipntr(1) = 1 >*/
            ipntr[1] = 1;
/*<             ipntr(2) = n + 1 >*/
            ipntr[2] = *n + 1;
/*<             call dcopy (n, resid, 1, workd, 1) >*/
            dcopy_(n, &resid[1], &c__1, &workd[1], &c__1);
/*<             ido = -1 >*/
            *ido = -1;
/*<             go to 9000 >*/
            goto L9000;
/*<          end if >*/
        }
/*<       end if >*/
    }

/*     %-----------------------------------------% */
/*     | Back from computing OP*(initial-vector) | */
/*     %-----------------------------------------% */

/*<       if (first) go to 20 >*/
    if (first) {
        goto L20;
    }

/*     %-----------------------------------------------% */
/*     | Back from computing B*(orthogonalized-vector) | */
/*     %-----------------------------------------------% */

/*<       if (orth)  go to 40 >*/
    if (orth) {
        goto L40;
    }

/*<       if (bmat .eq. 'G') then >*/
    if (*(unsigned char *)bmat == 'G') {
/*<          call second (t3) >*/
/*      second_(&t3); */
/*<          tmvopx = tmvopx + (t3 - t2) >*/
/*      timing_1.tmvopx += t3 - t2; */
/*<       end if >*/
    }

/*     %------------------------------------------------------% */
/*     | Starting vector is now in the range of OP; r = OP*r; | */
/*     | Compute B-norm of starting vector.                   | */
/*     %------------------------------------------------------% */

/*<       call second (t2) >*/
/*  second_(&t2); */
/*<       first = .TRUE. >*/
    first = TRUE_;
/*<       if (bmat .eq. 'G') then >*/
    if (*(unsigned char *)bmat == 'G') {
/*<          nbx = nbx + 1 >*/
/*      ++timing_1.nbx; */
/*<          call dcopy (n, workd(n+1), 1, resid, 1) >*/
        dcopy_(n, &workd[*n + 1], &c__1, &resid[1], &c__1);
/*<          ipntr(1) = n + 1 >*/
        ipntr[1] = *n + 1;
/*<          ipntr(2) = 1 >*/
        ipntr[2] = 1;
/*<          ido = 2 >*/
        *ido = 2;
/*<          go to 9000 >*/
        goto L9000;
/*<       else if (bmat .eq. 'I') then >*/
    } else if (*(unsigned char *)bmat == 'I') {
/*<          call dcopy (n, resid, 1, workd, 1) >*/
        dcopy_(n, &resid[1], &c__1, &workd[1], &c__1);
/*<       end if >*/
    }

/*<    20 continue >*/
L20:

/*<       if (bmat .eq. 'G') then >*/
    if (*(unsigned char *)bmat == 'G') {
/*<          call second (t3) >*/
/*      second_(&t3); */
/*<          tmvbx = tmvbx + (t3 - t2) >*/
/*      timing_1.tmvbx += t3 - t2; */
/*<       end if >*/
    }

/*<       first = .FALSE. >*/
    first = FALSE_;
/*<       if (bmat .eq. 'G') then >*/
    if (*(unsigned char *)bmat == 'G') {
/*<           rnorm0 = ddot (n, resid, 1, workd, 1) >*/
        rnorm0 = ddot_(n, &resid[1], &c__1, &workd[1], &c__1);
/*<           rnorm0 = sqrt(abs(rnorm0)) >*/
        rnorm0 = sqrt((abs(rnorm0)));
/*<       else if (bmat .eq. 'I') then >*/
    } else if (*(unsigned char *)bmat == 'I') {
/*<            rnorm0 = dnrm2(n, resid, 1) >*/
        rnorm0 = dnrm2_(n, &resid[1], &c__1);
/*<       end if >*/
    }
/*<       rnorm  = rnorm0 >*/
    *rnorm = rnorm0;

/*     %---------------------------------------------% */
/*     | Exit if this is the very first Arnoldi step | */
/*     %---------------------------------------------% */

/*<       if (j .eq. 1) go to 50 >*/
    if (*j == 1) {
        goto L50;
    }

/*     %---------------------------------------------------------------- */
/*     | Otherwise need to B-orthogonalize the starting vector against | */
/*     | the current Arnoldi basis using Gram-Schmidt with iter. ref.  | */
/*     | This is the case where an invariant subspace is encountered   | */
/*     | in the middle of the Arnoldi factorization.                   | */
/*     |                                                               | */
/*     |       s = V^{T}*B*r;   r = r - V*s;                           | */
/*     |                                                               | */
/*     | Stopping criteria used for iter. ref. is discussed in         | */
/*     | Parlett's book, page 107 and in Gragg & Reichel TOMS paper.   | */
/*     %---------------------------------------------------------------% */

/*<       orth = .TRUE. >*/
    orth = TRUE_;
/*<    30 continue >*/
L30:

/*<        >*/
    i__1 = *j - 1;
    dgemv_("T", n, &i__1, &c_b24, &v[v_offset], ldv, &workd[1], &c__1, &c_b26,
             &workd[*n + 1], &c__1, (ftnlen)1);
/*<        >*/
    i__1 = *j - 1;
    dgemv_("N", n, &i__1, &c_b29, &v[v_offset], ldv, &workd[*n + 1], &c__1, &
            c_b24, &resid[1], &c__1, (ftnlen)1);

/*     %----------------------------------------------------------% */
/*     | Compute the B-norm of the orthogonalized starting vector | */
/*     %----------------------------------------------------------% */

/*<       call second (t2) >*/
/*  second_(&t2); */
/*<       if (bmat .eq. 'G') then >*/
    if (*(unsigned char *)bmat == 'G') {
/*<          nbx = nbx + 1 >*/
/*      ++timing_1.nbx; */
/*<          call dcopy (n, resid, 1, workd(n+1), 1) >*/
        dcopy_(n, &resid[1], &c__1, &workd[*n + 1], &c__1);
/*<          ipntr(1) = n + 1 >*/
        ipntr[1] = *n + 1;
/*<          ipntr(2) = 1 >*/
        ipntr[2] = 1;
/*<          ido = 2 >*/
        *ido = 2;
/*<          go to 9000 >*/
        goto L9000;
/*<       else if (bmat .eq. 'I') then >*/
    } else if (*(unsigned char *)bmat == 'I') {
/*<          call dcopy (n, resid, 1, workd, 1) >*/
        dcopy_(n, &resid[1], &c__1, &workd[1], &c__1);
/*<       end if >*/
    }

/*<    40 continue >*/
L40:

/*<       if (bmat .eq. 'G') then >*/
    if (*(unsigned char *)bmat == 'G') {
/*<          call second (t3) >*/
/*      second_(&t3); */
/*<          tmvbx = tmvbx + (t3 - t2) >*/
/*      timing_1.tmvbx += t3 - t2; */
/*<       end if >*/
    }

/*<       if (bmat .eq. 'G') then >*/
    if (*(unsigned char *)bmat == 'G') {
/*<          rnorm = ddot (n, resid, 1, workd, 1) >*/
        *rnorm = ddot_(n, &resid[1], &c__1, &workd[1], &c__1);
/*<          rnorm = sqrt(abs(rnorm)) >*/
        *rnorm = sqrt((abs(*rnorm)));
/*<       else if (bmat .eq. 'I') then >*/
    } else if (*(unsigned char *)bmat == 'I') {
/*<          rnorm = dnrm2(n, resid, 1) >*/
        *rnorm = dnrm2_(n, &resid[1], &c__1);
/*<       end if >*/
    }

/*     %--------------------------------------% */
/*     | Check for further orthogonalization. | */
/*     %--------------------------------------% */

/*      if (msglvl .gt. 2) then */
/*          call dvout (logfil, 1, rnorm0, ndigit, */
/*     &                '_getv0: re-orthonalization ; rnorm0 is') */
/*          call dvout (logfil, 1, rnorm, ndigit, */
/*     &                '_getv0: re-orthonalization ; rnorm is') */
/*      end if */

/*<       if (rnorm .gt. 0.717*rnorm0) go to 50 >*/
    if (*rnorm > rnorm0 * (float).717) {
        goto L50;
    }

/*<       iter = iter + 1 >*/
    ++iter;
/*<       if (iter .le. 1) then >*/
    if (iter <= 1) {

/*        %-----------------------------------% */
/*        | Perform iterative refinement step | */
/*        %-----------------------------------% */

/*<          rnorm0 = rnorm >*/
        rnorm0 = *rnorm;
/*<          go to 30 >*/
        goto L30;
/*<       else >*/
    } else {

/*        %------------------------------------% */
/*        | Iterative refinement step "failed" | */
/*        %------------------------------------% */

/*<          do 45 jj = 1, n >*/
        i__1 = *n;
        for (jj = 1; jj <= i__1; ++jj) {
/*<             resid(jj) = zero >*/
            resid[jj] = 0.;
/*<    45    continue >*/
/* L45: */
        }
/*<          rnorm = zero >*/
        *rnorm = 0.;
/*<          ierr = -1 >*/
        *ierr = -1;
/*<       end if >*/
    }

/*<    50 continue >*/
L50:

/*      if (msglvl .gt. 0) then */
/*         call dvout (logfil, 1, rnorm, ndigit, */
/*     &        '_getv0: B-norm of initial / restarted starting vector') */
/*      end if */
/*      if (msglvl .gt. 2) then */
/*         call dvout (logfil, n, resid, ndigit, */
/*     &        '_getv0: initial / restarted starting vector') */
/*      end if */
/*<       ido = 99 >*/
    *ido = 99;

/*<       call second (t1) >*/
/*  second_(&t1); */
/*<       tgetv0 = tgetv0 + (t1 - t0) >*/
/*  timing_1.tgetv0 += t1 - t0; */

/*<  9000 continue >*/
L9000:
/*<       return >*/
    return 0;

/*     %---------------% */
/*     | End of dgetv0 | */
/*     %---------------% */

/*<       end >*/
} /* dgetv0_ */

#ifdef __cplusplus
        }
#endif
