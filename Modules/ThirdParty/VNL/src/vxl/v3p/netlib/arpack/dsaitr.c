/* arpack/dsaitr.f -- translated by f2c (version 20090411).
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

static logical c_false = FALSE_;
static integer c__1 = 1;
static doublereal c_b18 = 1.;
static doublereal c_b43 = 0.;
static doublereal c_b51 = -1.;

/* ----------------------------------------------------------------------- */
/* \BeginDoc */

/* \Name: dsaitr */

/* \Description: */
/*  Reverse communication interface for applying NP additional steps to */
/*  a K step symmetric Arnoldi factorization. */

/*  Input:  OP*V_{k}  -  V_{k}*H = r_{k}*e_{k}^T */

/*          with (V_{k}^T)*B*V_{k} = I, (V_{k}^T)*B*r_{k} = 0. */

/*  Output: OP*V_{k+p}  -  V_{k+p}*H = r_{k+p}*e_{k+p}^T */

/*          with (V_{k+p}^T)*B*V_{k+p} = I, (V_{k+p}^T)*B*r_{k+p} = 0. */

/*  where OP and B are as in dsaupd.  The B-norm of r_{k+p} is also */
/*  computed and returned. */

/* \Usage: */
/*  call dsaitr */
/*     ( IDO, BMAT, N, K, NP, MODE, RESID, RNORM, V, LDV, H, LDH, */
/*       IPNTR, WORKD, INFO ) */

/* \Arguments */
/*  IDO     Integer.  (INPUT/OUTPUT) */
/*          Reverse communication flag. */
/*          ------------------------------------------------------------- */
/*          IDO =  0: first call to the reverse communication interface */
/*          IDO = -1: compute  Y = OP * X  where */
/*                    IPNTR(1) is the pointer into WORK for X, */
/*                    IPNTR(2) is the pointer into WORK for Y. */
/*                    This is for the restart phase to force the new */
/*                    starting vector into the range of OP. */
/*          IDO =  1: compute  Y = OP * X  where */
/*                    IPNTR(1) is the pointer into WORK for X, */
/*                    IPNTR(2) is the pointer into WORK for Y, */
/*                    IPNTR(3) is the pointer into WORK for B * X. */
/*          IDO =  2: compute  Y = B * X  where */
/*                    IPNTR(1) is the pointer into WORK for X, */
/*                    IPNTR(2) is the pointer into WORK for Y. */
/*          IDO = 99: done */
/*          ------------------------------------------------------------- */
/*          When the routine is used in the "shift-and-invert" mode, the */
/*          vector B * Q is already available and does not need to be */
/*          recomputed in forming OP * Q. */

/*  BMAT    Character*1.  (INPUT) */
/*          BMAT specifies the type of matrix B that defines the */
/*          semi-inner product for the operator OP.  See dsaupd. */
/*          B = 'I' -> standard eigenvalue problem A*x = lambda*x */
/*          B = 'G' -> generalized eigenvalue problem A*x = lambda*M*x */

/*  N       Integer.  (INPUT) */
/*          Dimension of the eigenproblem. */

/*  K       Integer.  (INPUT) */
/*          Current order of H and the number of columns of V. */

/*  NP      Integer.  (INPUT) */
/*          Number of additional Arnoldi steps to take. */

/*  MODE    Integer.  (INPUT) */
/*          Signifies which form for "OP". If MODE=2 then */
/*          a reduction in the number of B matrix vector multiplies */
/*          is possible since the B-norm of OP*x is equivalent to */
/*          the inv(B)-norm of A*x. */

/*  RESID   Double precision array of length N.  (INPUT/OUTPUT) */
/*          On INPUT:  RESID contains the residual vector r_{k}. */
/*          On OUTPUT: RESID contains the residual vector r_{k+p}. */

/*  RNORM   Double precision scalar.  (INPUT/OUTPUT) */
/*          On INPUT the B-norm of r_{k}. */
/*          On OUTPUT the B-norm of the updated residual r_{k+p}. */

/*  V       Double precision N by K+NP array.  (INPUT/OUTPUT) */
/*          On INPUT:  V contains the Arnoldi vectors in the first K */
/*          columns. */
/*          On OUTPUT: V contains the new NP Arnoldi vectors in the next */
/*          NP columns.  The first K columns are unchanged. */

/*  LDV     Integer.  (INPUT) */
/*          Leading dimension of V exactly as declared in the calling */
/*          program. */

/*  H       Double precision (K+NP) by 2 array.  (INPUT/OUTPUT) */
/*          H is used to store the generated symmetric tridiagonal matrix */
/*          with the subdiagonal in the first column starting at H(2,1) */
/*          and the main diagonal in the second column. */

/*  LDH     Integer.  (INPUT) */
/*          Leading dimension of H exactly as declared in the calling */
/*          program. */

/*  IPNTR   Integer array of length 3.  (OUTPUT) */
/*          Pointer to mark the starting locations in the WORK for */
/*          vectors used by the Arnoldi iteration. */
/*          ------------------------------------------------------------- */
/*          IPNTR(1): pointer to the current operand vector X. */
/*          IPNTR(2): pointer to the current result vector Y. */
/*          IPNTR(3): pointer to the vector B * X when used in the */
/*                    shift-and-invert mode.  X is the current operand. */
/*          ------------------------------------------------------------- */

/*  WORKD   Double precision work array of length 3*N.  (REVERSE COMMUNICATION) */
/*          Distributed array to be used in the basic Arnoldi iteration */
/*          for reverse communication.  The calling program should not */
/*          use WORKD as temporary workspace during the iteration !!!!!! */
/*          On INPUT, WORKD(1:N) = B*RESID where RESID is associated */
/*          with the K step Arnoldi factorization. Used to save some */
/*          computation at the first step. */
/*          On OUTPUT, WORKD(1:N) = B*RESID where RESID is associated */
/*          with the K+NP step Arnoldi factorization. */

/*  INFO    Integer.  (OUTPUT) */
/*          = 0: Normal exit. */
/*          > 0: Size of an invariant subspace of OP is found that is */
/*               less than K + NP. */

/* \EndDoc */

/* ----------------------------------------------------------------------- */

/* \BeginLib */

/* \Local variables: */
/*     xxxxxx  real */

/* \Routines called: */
/*     dgetv0  ARPACK routine to generate the initial vector. */
/*     ivout   ARPACK utility routine that prints integers. */
/*     dmout   ARPACK utility routine that prints matrices. */
/*     dlamch  LAPACK routine that determines machine constants. */
/*     dlascl  LAPACK routine for careful scaling of a matrix. */
/*     dgemv   Level 2 BLAS routine for matrix vector multiplication. */
/*     daxpy   Level 1 BLAS that computes a vector triad. */
/*     dscal   Level 1 BLAS that scales a vector. */
/*     dcopy   Level 1 BLAS that copies one vector to another . */
/*     ddot    Level 1 BLAS that computes the scalar product of two vectors. */
/*     dnrm2   Level 1 BLAS that computes the norm of a vector. */

/* \Author */
/*     Danny Sorensen               Phuong Vu */
/*     Richard Lehoucq              CRPC / Rice University */
/*     Dept. of Computational &     Houston, Texas */
/*     Applied Mathematics */
/*     Rice University */
/*     Houston, Texas */

/* \Revision history: */
/*     xx/xx/93: Version ' 2.4' */

/* \SCCS Information: @(#) */
/* FILE: saitr.F   SID: 2.6   DATE OF SID: 8/28/96   RELEASE: 2 */

/* \Remarks */
/*  The algorithm implemented is: */

/*  restart = .false. */
/*  Given V_{k} = [v_{1}, ..., v_{k}], r_{k}; */
/*  r_{k} contains the initial residual vector even for k = 0; */
/*  Also assume that rnorm = || B*r_{k} || and B*r_{k} are already */
/*  computed by the calling program. */

/*  betaj = rnorm ; p_{k+1} = B*r_{k} ; */
/*  For  j = k+1, ..., k+np  Do */
/*     1) if ( betaj < tol ) stop or restart depending on j. */
/*        if ( restart ) generate a new starting vector. */
/*     2) v_{j} = r(j-1)/betaj;  V_{j} = [V_{j-1}, v_{j}]; */
/*        p_{j} = p_{j}/betaj */
/*     3) r_{j} = OP*v_{j} where OP is defined as in dsaupd */
/*        For shift-invert mode p_{j} = B*v_{j} is already available. */
/*        wnorm = || OP*v_{j} || */
/*     4) Compute the j-th step residual vector. */
/*        w_{j} =  V_{j}^T * B * OP * v_{j} */
/*        r_{j} =  OP*v_{j} - V_{j} * w_{j} */
/*        alphaj <- j-th component of w_{j} */
/*        rnorm = || r_{j} || */
/*        betaj+1 = rnorm */
/*        If (rnorm > 0.717*wnorm) accept step and go back to 1) */
/*     5) Re-orthogonalization step: */
/*        s = V_{j}'*B*r_{j} */
/*        r_{j} = r_{j} - V_{j}*s;  rnorm1 = || r_{j} || */
/*        alphaj = alphaj + s_{j}; */
/*     6) Iterative refinement step: */
/*        If (rnorm1 > 0.717*rnorm) then */
/*           rnorm = rnorm1 */
/*           accept step and go back to 1) */
/*        Else */
/*           rnorm = rnorm1 */
/*           If this is the first time in step 6), go to 5) */
/*           Else r_{j} lies in the span of V_{j} numerically. */
/*              Set r_{j} = 0 and rnorm = 0; go to 1) */
/*        EndIf */
/*  End Do */

/* \EndLib */

/* ----------------------------------------------------------------------- */

/*<        >*/
/* Subroutine */ int dsaitr_(integer *ido, char *bmat, integer *n, integer *k,
         integer *np, integer *mode, doublereal *resid, doublereal *rnorm,
        doublereal *v, integer *ldv, doublereal *h__, integer *ldh, integer *
        ipntr, doublereal *workd, integer *info, ftnlen bmat_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer h_dim1, h_offset, v_dim1, v_offset, i__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__;
    static integer j;
/*  static real t0, t1, t2, t3, t4, t5; */
    integer jj;
    static integer ipj, irj, ivj;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *,
            integer *);
    static integer ierr, iter, itry;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    doublereal temp1;
    static logical orth1, orth2, step3, step4;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *), dgemv_(char *, integer *, integer *, doublereal *,
            doublereal *, integer *, doublereal *, integer *, doublereal *,
            doublereal *, integer *, ftnlen);
    integer infol;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *,
            doublereal *, integer *);
/*  doublereal xtemp[2]; */
    static doublereal wnorm;
    extern /* Subroutine */ int dgetv0_(integer *, char *, integer *, logical
            *, integer *, integer *, doublereal *, integer *, doublereal *,
            doublereal *, integer *, doublereal *, integer *, ftnlen);
    static doublereal rnorm1;
    extern doublereal dlamch_(char *, ftnlen);
    extern /* Subroutine */ int dlascl_(char *, integer *, integer *,
            doublereal *, doublereal *, integer *, integer *, doublereal *,
            integer *, integer *, ftnlen), second_(real *);
    static doublereal safmin;
    static logical rstart;
/*  static integer msglvl; */


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
/*<       integer    ido, info, k, ldh, ldv, n, mode, np >*/
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

/*<       logical    first, orth1, orth2, rstart, step3, step4 >*/
/*<        >*/
/*<        >*/
/*<        >*/

/*     %-----------------------% */
/*     | Local Array Arguments | */
/*     %-----------------------% */

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

/*     %-----------------% */
/*     | Data statements | */
/*     %-----------------% */

/*<       data      first / .true. / >*/
    /* Parameter adjustments */
    --workd;
    --resid;
    v_dim1 = *ldv;
    v_offset = 1 + v_dim1;
    v -= v_offset;
    h_dim1 = *ldh;
    h_offset = 1 + h_dim1;
    h__ -= h_offset;
    --ipntr;

    /* Function Body */

/*     %-----------------------% */
/*     | Executable Statements | */
/*     %-----------------------% */

/*<       if (first) then >*/
    if (first) {
/*<          first = .false. >*/
        first = FALSE_;

/*        %--------------------------------% */
/*        | safmin = safe minimum is such  | */
/*        | that 1/sfmin does not overflow | */
/*        %--------------------------------% */

/*<          safmin = dlamch('safmin') >*/
        safmin = dlamch_("safmin", (ftnlen)6);
/*<       end if >*/
    }

/*<       if (ido .eq. 0) then >*/
    if (*ido == 0) {

/*        %-------------------------------% */
/*        | Initialize timing statistics  | */
/*        | & message level for debugging | */
/*        %-------------------------------% */

/*<          call second (t0) >*/
/*      second_(&t0); */
/*<          msglvl = msaitr >*/
/*      msglvl = debug_1.msaitr; */

/*        %------------------------------% */
/*        | Initial call to this routine | */
/*        %------------------------------% */

/*<          info   = 0 >*/
        *info = 0;
/*<          step3  = .false. >*/
        step3 = FALSE_;
/*<          step4  = .false. >*/
        step4 = FALSE_;
/*<          rstart = .false. >*/
        rstart = FALSE_;
/*<          orth1  = .false. >*/
        orth1 = FALSE_;
/*<          orth2  = .false. >*/
        orth2 = FALSE_;

/*        %--------------------------------% */
/*        | Pointer to the current step of | */
/*        | the factorization to build     | */
/*        %--------------------------------% */

/*<          j      = k + 1 >*/
        j = *k + 1;

/*        %------------------------------------------% */
/*        | Pointers used for reverse communication  | */
/*        | when using WORKD.                        | */
/*        %------------------------------------------% */

/*<          ipj    = 1 >*/
        ipj = 1;
/*<          irj    = ipj   + n >*/
        irj = ipj + *n;
/*<          ivj    = irj   + n >*/
        ivj = irj + *n;
/*<       end if >*/
    }

/*     %-------------------------------------------------% */
/*     | When in reverse communication mode one of:      | */
/*     | STEP3, STEP4, ORTH1, ORTH2, RSTART              | */
/*     | will be .true.                                  | */
/*     | STEP3: return from computing OP*v_{j}.          | */
/*     | STEP4: return from computing B-norm of OP*v_{j} | */
/*     | ORTH1: return from computing B-norm of r_{j+1}  | */
/*     | ORTH2: return from computing B-norm of          | */
/*     |        correction to the residual vector.       | */
/*     | RSTART: return from OP computations needed by   | */
/*     |         dgetv0.                                 | */
/*     %-------------------------------------------------% */

/*<       if (step3)  go to 50 >*/
    if (step3) {
        goto L50;
    }
/*<       if (step4)  go to 60 >*/
    if (step4) {
        goto L60;
    }
/*<       if (orth1)  go to 70 >*/
    if (orth1) {
        goto L70;
    }
/*<       if (orth2)  go to 90 >*/
    if (orth2) {
        goto L90;
    }
/*<       if (rstart) go to 30 >*/
    if (rstart) {
        goto L30;
    }

/*     %------------------------------% */
/*     | Else this is the first step. | */
/*     %------------------------------% */

/*     %--------------------------------------------------------------% */
/*     |                                                              | */
/*     |        A R N O L D I     I T E R A T I O N     L O O P       | */
/*     |                                                              | */
/*     | Note:  B*r_{j-1} is already in WORKD(1:N)=WORKD(IPJ:IPJ+N-1) | */
/*     %--------------------------------------------------------------% */

/*<  1000 continue >*/
L1000:

/*         if (msglvl .gt. 2) then */
/*            call ivout (logfil, 1, j, ndigit, */
/*     &                  '_saitr: generating Arnoldi vector no.') */
/*            call dvout (logfil, 1, rnorm, ndigit, */
/*     &                  '_saitr: B-norm of the current residual =') */
/*         end if */

/*        %---------------------------------------------------------% */
/*        | Check for exact zero. Equivalent to determining whether | */
/*        | a j-step Arnoldi factorization is present.              | */
/*        %---------------------------------------------------------% */

/*<          if (rnorm .gt. zero) go to 40 >*/
    if (*rnorm > 0.) {
        goto L40;
    }

/*           %---------------------------------------------------% */
/*           | Invariant subspace found, generate a new starting | */
/*           | vector which is orthogonal to the current Arnoldi | */
/*           | basis and continue the iteration.                 | */
/*           %---------------------------------------------------% */

/*            if (msglvl .gt. 0) then */
/*               call ivout (logfil, 1, j, ndigit, */
/*     &                     '_saitr: ****** restart at step ******') */
/*            end if */

/*           %---------------------------------------------% */
/*           | ITRY is the loop variable that controls the | */
/*           | maximum amount of times that a restart is   | */
/*           | attempted. NRSTRT is used by stat.h         | */
/*           %---------------------------------------------% */

/*<             nrstrt = nrstrt + 1 >*/
/*  ++timing_1.nrstrt; */
/*<             itry   = 1 >*/
    itry = 1;
/*<    20       continue >*/
L20:
/*<             rstart = .true. >*/
    rstart = TRUE_;
/*<             ido    = 0 >*/
    *ido = 0;
/*<    30       continue >*/
L30:

/*           %--------------------------------------% */
/*           | If in reverse communication mode and | */
/*           | RSTART = .true. flow returns here.   | */
/*           %--------------------------------------% */

/*<        >*/
    dgetv0_(ido, bmat, &itry, &c_false, n, &j, &v[v_offset], ldv, &resid[1],
            rnorm, &ipntr[1], &workd[1], &ierr, (ftnlen)1);
/*<             if (ido .ne. 99) go to 9000 >*/
    if (*ido != 99) {
        goto L9000;
    }
/*<             if (ierr .lt. 0) then >*/
    if (ierr < 0) {
/*<                itry = itry + 1 >*/
        ++itry;
/*<                if (itry .le. 3) go to 20 >*/
        if (itry <= 3) {
            goto L20;
        }

/*              %------------------------------------------------% */
/*              | Give up after several restart attempts.        | */
/*              | Set INFO to the size of the invariant subspace | */
/*              | which spans OP and exit.                       | */
/*              %------------------------------------------------% */

/*<                info = j - 1 >*/
        *info = j - 1;
/*<                call second (t1) >*/
/*      second_(&t1); */
/*<                tsaitr = tsaitr + (t1 - t0) >*/
/*      timing_1.tsaitr += t1 - t0; */
/*<                ido = 99 >*/
        *ido = 99;
/*<                go to 9000 >*/
        goto L9000;
/*<             end if >*/
    }

/*<    40    continue >*/
L40:

/*        %---------------------------------------------------------% */
/*        | STEP 2:  v_{j} = r_{j-1}/rnorm and p_{j} = p_{j}/rnorm  | */
/*        | Note that p_{j} = B*r_{j-1}. In order to avoid overflow | */
/*        | when reciprocating a small RNORM, test against lower    | */
/*        | machine bound.                                          | */
/*        %---------------------------------------------------------% */

/*<          call dcopy (n, resid, 1, v(1,j), 1) >*/
    dcopy_(n, &resid[1], &c__1, &v[j * v_dim1 + 1], &c__1);
/*<          if (rnorm .ge. safmin) then >*/
    if (*rnorm >= safmin) {
/*<              temp1 = one / rnorm >*/
        temp1 = 1. / *rnorm;
/*<              call dscal (n, temp1, v(1,j), 1) >*/
        dscal_(n, &temp1, &v[j * v_dim1 + 1], &c__1);
/*<              call dscal (n, temp1, workd(ipj), 1) >*/
        dscal_(n, &temp1, &workd[ipj], &c__1);
/*<          else >*/
    } else {

/*            %-----------------------------------------% */
/*            | To scale both v_{j} and p_{j} carefully | */
/*            | use LAPACK routine SLASCL               | */
/*            %-----------------------------------------% */

/*<        >*/
        dlascl_("General", &i__, &i__, rnorm, &c_b18, n, &c__1, &v[j * v_dim1
                + 1], n, &infol, (ftnlen)7);
/*<        >*/
        dlascl_("General", &i__, &i__, rnorm, &c_b18, n, &c__1, &workd[ipj],
                n, &infol, (ftnlen)7);
/*<          end if >*/
    }

/*        %------------------------------------------------------% */
/*        | STEP 3:  r_{j} = OP*v_{j}; Note that p_{j} = B*v_{j} | */
/*        | Note that this is not quite yet r_{j}. See STEP 4    | */
/*        %------------------------------------------------------% */

/*<          step3 = .true. >*/
    step3 = TRUE_;
/*<          nopx  = nopx + 1 >*/
/*  ++timing_1.nopx; */
/*<          call second (t2) >*/
/*  second_(&t2); */
/*<          call dcopy (n, v(1,j), 1, workd(ivj), 1) >*/
    dcopy_(n, &v[j * v_dim1 + 1], &c__1, &workd[ivj], &c__1);
/*<          ipntr(1) = ivj >*/
    ipntr[1] = ivj;
/*<          ipntr(2) = irj >*/
    ipntr[2] = irj;
/*<          ipntr(3) = ipj >*/
    ipntr[3] = ipj;
/*<          ido = 1 >*/
    *ido = 1;

/*        %-----------------------------------% */
/*        | Exit in order to compute OP*v_{j} | */
/*        %-----------------------------------% */

/*<          go to 9000 >*/
    goto L9000;
/*<    50    continue >*/
L50:

/*        %-----------------------------------% */
/*        | Back from reverse communication;  | */
/*        | WORKD(IRJ:IRJ+N-1) := OP*v_{j}.   | */
/*        %-----------------------------------% */

/*<          call second (t3) >*/
/*  second_(&t3); */
/*<          tmvopx = tmvopx + (t3 - t2) >*/
/*  timing_1.tmvopx += t3 - t2; */

/*<          step3 = .false. >*/
    step3 = FALSE_;

/*        %------------------------------------------% */
/*        | Put another copy of OP*v_{j} into RESID. | */
/*        %------------------------------------------% */

/*<          call dcopy (n, workd(irj), 1, resid, 1) >*/
    dcopy_(n, &workd[irj], &c__1, &resid[1], &c__1);

/*        %-------------------------------------------% */
/*        | STEP 4:  Finish extending the symmetric   | */
/*        |          Arnoldi to length j. If MODE = 2 | */
/*        |          then B*OP = B*inv(B)*A = A and   | */
/*        |          we don't need to compute B*OP.   | */
/*        | NOTE: If MODE = 2 WORKD(IVJ:IVJ+N-1) is   | */
/*        | assumed to have A*v_{j}.                  | */
/*        %-------------------------------------------% */

/*<          if (mode .eq. 2) go to 65 >*/
    if (*mode == 2) {
        goto L65;
    }
/*<          call second (t2) >*/
/*  second_(&t2); */
/*<          if (bmat .eq. 'G') then >*/
    if (*(unsigned char *)bmat == 'G') {
/*<             nbx = nbx + 1 >*/
/*      ++timing_1.nbx; */
/*<             step4 = .true. >*/
        step4 = TRUE_;
/*<             ipntr(1) = irj >*/
        ipntr[1] = irj;
/*<             ipntr(2) = ipj >*/
        ipntr[2] = ipj;
/*<             ido = 2 >*/
        *ido = 2;

/*           %-------------------------------------% */
/*           | Exit in order to compute B*OP*v_{j} | */
/*           %-------------------------------------% */

/*<             go to 9000 >*/
        goto L9000;
/*<          else if (bmat .eq. 'I') then >*/
    } else if (*(unsigned char *)bmat == 'I') {
/*<               call dcopy(n, resid, 1 , workd(ipj), 1) >*/
        dcopy_(n, &resid[1], &c__1, &workd[ipj], &c__1);
/*<          end if >*/
    }
/*<    60    continue >*/
L60:

/*        %-----------------------------------% */
/*        | Back from reverse communication;  | */
/*        | WORKD(IPJ:IPJ+N-1) := B*OP*v_{j}. | */
/*        %-----------------------------------% */

/*<          if (bmat .eq. 'G') then >*/
    if (*(unsigned char *)bmat == 'G') {
/*<             call second (t3) >*/
/*      second_(&t3); */
/*<             tmvbx = tmvbx + (t3 - t2) >*/
/*      timing_1.tmvbx += t3 - t2; */
/*<          end if  >*/
    }

/*<          step4 = .false. >*/
    step4 = FALSE_;

/*        %-------------------------------------% */
/*        | The following is needed for STEP 5. | */
/*        | Compute the B-norm of OP*v_{j}.     | */
/*        %-------------------------------------% */

/*<    65    continue >*/
L65:
/*<          if (mode .eq. 2) then >*/
    if (*mode == 2) {

/*           %----------------------------------% */
/*           | Note that the B-norm of OP*v_{j} | */
/*           | is the inv(B)-norm of A*v_{j}.   | */
/*           %----------------------------------% */

/*<             wnorm = ddot (n, resid, 1, workd(ivj), 1) >*/
        wnorm = ddot_(n, &resid[1], &c__1, &workd[ivj], &c__1);
/*<             wnorm = sqrt(abs(wnorm)) >*/
        wnorm = sqrt((abs(wnorm)));
/*<          else if (bmat .eq. 'G') then          >*/
    } else if (*(unsigned char *)bmat == 'G') {
/*<             wnorm = ddot (n, resid, 1, workd(ipj), 1) >*/
        wnorm = ddot_(n, &resid[1], &c__1, &workd[ipj], &c__1);
/*<             wnorm = sqrt(abs(wnorm)) >*/
        wnorm = sqrt((abs(wnorm)));
/*<          else if (bmat .eq. 'I') then >*/
    } else if (*(unsigned char *)bmat == 'I') {
/*<             wnorm = dnrm2(n, resid, 1) >*/
        wnorm = dnrm2_(n, &resid[1], &c__1);
/*<          end if >*/
    }

/*        %-----------------------------------------% */
/*        | Compute the j-th residual corresponding | */
/*        | to the j step factorization.            | */
/*        | Use Classical Gram Schmidt and compute: | */
/*        | w_{j} <-  V_{j}^T * B * OP * v_{j}      | */
/*        | r_{j} <-  OP*v_{j} - V_{j} * w_{j}      | */
/*        %-----------------------------------------% */


/*        %------------------------------------------% */
/*        | Compute the j Fourier coefficients w_{j} | */
/*        | WORKD(IPJ:IPJ+N-1) contains B*OP*v_{j}.  | */
/*        %------------------------------------------% */

/*<          if (mode .ne. 2 ) then >*/
    if (*mode != 2) {
/*<        >*/
        dgemv_("T", n, &j, &c_b18, &v[v_offset], ldv, &workd[ipj], &c__1, &
                c_b43, &workd[irj], &c__1, (ftnlen)1);
/*<          else if (mode .eq. 2) then >*/
    } else if (*mode == 2) {
/*<        >*/
        dgemv_("T", n, &j, &c_b18, &v[v_offset], ldv, &workd[ivj], &c__1, &
                c_b43, &workd[irj], &c__1, (ftnlen)1);
/*<          end if >*/
    }

/*        %--------------------------------------% */
/*        | Orthgonalize r_{j} against V_{j}.    | */
/*        | RESID contains OP*v_{j}. See STEP 3. | */
/*        %--------------------------------------% */

/*<        >*/
    dgemv_("N", n, &j, &c_b51, &v[v_offset], ldv, &workd[irj], &c__1, &c_b18,
            &resid[1], &c__1, (ftnlen)1);

/*        %--------------------------------------% */
/*        | Extend H to have j rows and columns. | */
/*        %--------------------------------------% */

/*<          h(j,2) = workd(irj + j - 1) >*/
    h__[j + (h_dim1 << 1)] = workd[irj + j - 1];
/*<          if (j .eq. 1  .or.  rstart) then >*/
    if (j == 1 || rstart) {
/*<             h(j,1) = zero >*/
        h__[j + h_dim1] = 0.;
/*<          else >*/
    } else {
/*<             h(j,1) = rnorm >*/
        h__[j + h_dim1] = *rnorm;
/*<          end if >*/
    }
/*<          call second (t4) >*/
/*  second_(&t4); */

/*<          orth1 = .true. >*/
    orth1 = TRUE_;
/*<          iter  = 0 >*/
    iter = 0;

/*<          call second (t2) >*/
/*  second_(&t2); */
/*<          if (bmat .eq. 'G') then >*/
    if (*(unsigned char *)bmat == 'G') {
/*<             nbx = nbx + 1 >*/
/*      ++timing_1.nbx; */
/*<             call dcopy (n, resid, 1, workd(irj), 1) >*/
        dcopy_(n, &resid[1], &c__1, &workd[irj], &c__1);
/*<             ipntr(1) = irj >*/
        ipntr[1] = irj;
/*<             ipntr(2) = ipj >*/
        ipntr[2] = ipj;
/*<             ido = 2 >*/
        *ido = 2;

/*           %----------------------------------% */
/*           | Exit in order to compute B*r_{j} | */
/*           %----------------------------------% */

/*<             go to 9000 >*/
        goto L9000;
/*<          else if (bmat .eq. 'I') then >*/
    } else if (*(unsigned char *)bmat == 'I') {
/*<             call dcopy (n, resid, 1, workd(ipj), 1) >*/
        dcopy_(n, &resid[1], &c__1, &workd[ipj], &c__1);
/*<          end if >*/
    }
/*<    70    continue >*/
L70:

/*        %---------------------------------------------------% */
/*        | Back from reverse communication if ORTH1 = .true. | */
/*        | WORKD(IPJ:IPJ+N-1) := B*r_{j}.                    | */
/*        %---------------------------------------------------% */

/*<          if (bmat .eq. 'G') then >*/
    if (*(unsigned char *)bmat == 'G') {
/*<             call second (t3) >*/
/*      second_(&t3); */
/*<             tmvbx = tmvbx + (t3 - t2) >*/
/*      timing_1.tmvbx += t3 - t2; */
/*<          end if >*/
    }

/*<          orth1 = .false. >*/
    orth1 = FALSE_;

/*        %------------------------------% */
/*        | Compute the B-norm of r_{j}. | */
/*        %------------------------------% */

/*<          if (bmat .eq. 'G') then          >*/
    if (*(unsigned char *)bmat == 'G') {
/*<             rnorm = ddot (n, resid, 1, workd(ipj), 1) >*/
        *rnorm = ddot_(n, &resid[1], &c__1, &workd[ipj], &c__1);
/*<             rnorm = sqrt(abs(rnorm)) >*/
        *rnorm = sqrt((abs(*rnorm)));
/*<          else if (bmat .eq. 'I') then >*/
    } else if (*(unsigned char *)bmat == 'I') {
/*<             rnorm = dnrm2(n, resid, 1) >*/
        *rnorm = dnrm2_(n, &resid[1], &c__1);
/*<          end if >*/
    }

/*        %-----------------------------------------------------------% */
/*        | STEP 5: Re-orthogonalization / Iterative refinement phase | */
/*        | Maximum NITER_ITREF tries.                                | */
/*        |                                                           | */
/*        |          s      = V_{j}^T * B * r_{j}                     | */
/*        |          r_{j}  = r_{j} - V_{j}*s                         | */
/*        |          alphaj = alphaj + s_{j}                          | */
/*        |                                                           | */
/*        | The stopping criteria used for iterative refinement is    | */
/*        | discussed in Parlett's book SEP, page 107 and in Gragg &  | */
/*        | Reichel ACM TOMS paper; Algorithm 686, Dec. 1990.         | */
/*        | Determine if we need to correct the residual. The goal is | */
/*        | to enforce ||v(:,1:j)^T * r_{j}|| .le. eps * || r_{j} ||  | */
/*        %-----------------------------------------------------------% */

/*<          if (rnorm .gt. 0.717*wnorm) go to 100 >*/
    if (*rnorm > wnorm * (float).717) {
        goto L100;
    }
/*<          nrorth = nrorth + 1 >*/
/*  ++timing_1.nrorth; */

/*        %---------------------------------------------------% */
/*        | Enter the Iterative refinement phase. If further  | */
/*        | refinement is necessary, loop back here. The loop | */
/*        | variable is ITER. Perform a step of Classical     | */
/*        | Gram-Schmidt using all the Arnoldi vectors V_{j}  | */
/*        %---------------------------------------------------% */

/*<    80    continue >*/
L80:

/*<          if (msglvl .gt. 2) then >*/
/*  if (msglvl > 2) { */
/*<             xtemp(1) = wnorm >*/
/*      xtemp[0] = wnorm; */
/*<             xtemp(2) = rnorm >*/
/*      xtemp[1] = *rnorm; */
/*            call dvout (logfil, 2, xtemp, ndigit, */
/*     &           '_saitr: re-orthonalization ; wnorm and rnorm are') */
/*<          end if >*/
/*  } */

/*        %----------------------------------------------------% */
/*        | Compute V_{j}^T * B * r_{j}.                       | */
/*        | WORKD(IRJ:IRJ+J-1) = v(:,1:J)'*WORKD(IPJ:IPJ+N-1). | */
/*        %----------------------------------------------------% */

/*<        >*/
    dgemv_("T", n, &j, &c_b18, &v[v_offset], ldv, &workd[ipj], &c__1, &c_b43,
            &workd[irj], &c__1, (ftnlen)1);

/*        %----------------------------------------------% */
/*        | Compute the correction to the residual:      | */
/*        | r_{j} = r_{j} - V_{j} * WORKD(IRJ:IRJ+J-1).  | */
/*        | The correction to H is v(:,1:J)*H(1:J,1:J) + | */
/*        | v(:,1:J)*WORKD(IRJ:IRJ+J-1)*e'_j, but only   | */
/*        | H(j,j) is updated.                           | */
/*        %----------------------------------------------% */

/*<        >*/
    dgemv_("N", n, &j, &c_b51, &v[v_offset], ldv, &workd[irj], &c__1, &c_b18,
            &resid[1], &c__1, (ftnlen)1);

/*<          if (j .eq. 1  .or.  rstart) h(j,1) = zero >*/
    if (j == 1 || rstart) {
        h__[j + h_dim1] = 0.;
    }
/*<          h(j,2) = h(j,2) + workd(irj + j - 1) >*/
    h__[j + (h_dim1 << 1)] += workd[irj + j - 1];

/*<          orth2 = .true. >*/
    orth2 = TRUE_;
/*<          call second (t2) >*/
/*  second_(&t2); */
/*<          if (bmat .eq. 'G') then >*/
    if (*(unsigned char *)bmat == 'G') {
/*<             nbx = nbx + 1 >*/
/*      ++timing_1.nbx; */
/*<             call dcopy (n, resid, 1, workd(irj), 1) >*/
        dcopy_(n, &resid[1], &c__1, &workd[irj], &c__1);
/*<             ipntr(1) = irj >*/
        ipntr[1] = irj;
/*<             ipntr(2) = ipj >*/
        ipntr[2] = ipj;
/*<             ido = 2 >*/
        *ido = 2;

/*           %-----------------------------------% */
/*           | Exit in order to compute B*r_{j}. | */
/*           | r_{j} is the corrected residual.  | */
/*           %-----------------------------------% */

/*<             go to 9000 >*/
        goto L9000;
/*<          else if (bmat .eq. 'I') then >*/
    } else if (*(unsigned char *)bmat == 'I') {
/*<             call dcopy (n, resid, 1, workd(ipj), 1) >*/
        dcopy_(n, &resid[1], &c__1, &workd[ipj], &c__1);
/*<          end if >*/
    }
/*<    90    continue >*/
L90:

/*        %---------------------------------------------------% */
/*        | Back from reverse communication if ORTH2 = .true. | */
/*        %---------------------------------------------------% */

/*<          if (bmat .eq. 'G') then >*/
    if (*(unsigned char *)bmat == 'G') {
/*<             call second (t3) >*/
/*      second_(&t3); */
/*<             tmvbx = tmvbx + (t3 - t2) >*/
/*      timing_1.tmvbx += t3 - t2; */
/*<          end if >*/
    }

/*        %-----------------------------------------------------% */
/*        | Compute the B-norm of the corrected residual r_{j}. | */
/*        %-----------------------------------------------------% */

/*<          if (bmat .eq. 'G') then          >*/
    if (*(unsigned char *)bmat == 'G') {
/*<              rnorm1 = ddot (n, resid, 1, workd(ipj), 1) >*/
        rnorm1 = ddot_(n, &resid[1], &c__1, &workd[ipj], &c__1);
/*<              rnorm1 = sqrt(abs(rnorm1)) >*/
        rnorm1 = sqrt((abs(rnorm1)));
/*<          else if (bmat .eq. 'I') then >*/
    } else if (*(unsigned char *)bmat == 'I') {
/*<              rnorm1 = dnrm2(n, resid, 1) >*/
        rnorm1 = dnrm2_(n, &resid[1], &c__1);
/*<          end if >*/
    }

/*         if (msglvl .gt. 0 .and. iter .gt. 0) then */
/*            call ivout (logfil, 1, j, ndigit, */
/*     &           '_saitr: Iterative refinement for Arnoldi residual') */
/*            if (msglvl .gt. 2) then */
/*                xtemp(1) = rnorm */
/*                xtemp(2) = rnorm1 */
/*                call dvout (logfil, 2, xtemp, ndigit, */
/*     &           '_saitr: iterative refinement ; rnorm and rnorm1 are') */
/*            end if */
/*         end if */

/*        %-----------------------------------------% */
/*        | Determine if we need to perform another | */
/*        | step of re-orthogonalization.           | */
/*        %-----------------------------------------% */

/*<          if (rnorm1 .gt. 0.717*rnorm) then >*/
    if (rnorm1 > *rnorm * (float).717) {

/*           %--------------------------------% */
/*           | No need for further refinement | */
/*           %--------------------------------% */

/*<             rnorm = rnorm1 >*/
        *rnorm = rnorm1;

/*<          else >*/
    } else {

/*           %-------------------------------------------% */
/*           | Another step of iterative refinement step | */
/*           | is required. NITREF is used by stat.h     | */
/*           %-------------------------------------------% */

/*<             nitref = nitref + 1 >*/
/*      ++timing_1.nitref; */
/*<             rnorm  = rnorm1 >*/
        *rnorm = rnorm1;
/*<             iter   = iter + 1 >*/
        ++iter;
/*<             if (iter .le. 1) go to 80 >*/
        if (iter <= 1) {
            goto L80;
        }

/*           %-------------------------------------------------% */
/*           | Otherwise RESID is numerically in the span of V | */
/*           %-------------------------------------------------% */

/*<             do 95 jj = 1, n >*/
        i__1 = *n;
        for (jj = 1; jj <= i__1; ++jj) {
/*<                resid(jj) = zero >*/
            resid[jj] = 0.;
/*<   95        continue >*/
/* L95: */
        }
/*<             rnorm = zero >*/
        *rnorm = 0.;
/*<          end if >*/
    }

/*        %----------------------------------------------% */
/*        | Branch here directly if iterative refinement | */
/*        | wasn't necessary or after at most NITER_REF  | */
/*        | steps of iterative refinement.               | */
/*        %----------------------------------------------% */

/*<   100    continue >*/
L100:

/*<          rstart = .false. >*/
    rstart = FALSE_;
/*<          orth2  = .false. >*/
    orth2 = FALSE_;

/*<          call second (t5) >*/
/*  second_(&t5); */
/*<          titref = titref + (t5 - t4) >*/
/*  timing_1.titref += t5 - t4; */

/*        %----------------------------------------------------------% */
/*        | Make sure the last off-diagonal element is non negative  | */
/*        | If not perform a similarity transformation on H(1:j,1:j) | */
/*        | and scale v(:,j) by -1.                                  | */
/*        %----------------------------------------------------------% */

/*<          if (h(j,1) .lt. zero) then >*/
    if (h__[j + h_dim1] < 0.) {
/*<             h(j,1) = -h(j,1) >*/
        h__[j + h_dim1] = -h__[j + h_dim1];
/*<             if ( j .lt. k+np) then  >*/
        if (j < *k + *np) {
/*<                call dscal(n, -one, v(1,j+1), 1) >*/
            dscal_(n, &c_b51, &v[(j + 1) * v_dim1 + 1], &c__1);
/*<             else >*/
        } else {
/*<                call dscal(n, -one, resid, 1) >*/
            dscal_(n, &c_b51, &resid[1], &c__1);
/*<             end if >*/
        }
/*<          end if >*/
    }

/*        %------------------------------------% */
/*        | STEP 6: Update  j = j+1;  Continue | */
/*        %------------------------------------% */

/*<          j = j + 1 >*/
    ++j;
/*<          if (j .gt. k+np) then >*/
    if (j > *k + *np) {
/*<             call second (t1) >*/
/*      second_(&t1); */
/*<             tsaitr = tsaitr + (t1 - t0) >*/
/*      timing_1.tsaitr += t1 - t0; */
/*<             ido = 99 >*/
        *ido = 99;

/*            if (msglvl .gt. 1) then */
/*               call dvout (logfil, k+np, h(1,2), ndigit, */
/*     &         '_saitr: main diagonal of matrix H of step K+NP.') */
/*               if (k+np .gt. 1) then */
/*               call dvout (logfil, k+np-1, h(2,1), ndigit, */
/*     &         '_saitr: sub diagonal of matrix H of step K+NP.') */
/*               end if */
/*            end if */

/*<             go to 9000 >*/
        goto L9000;
/*<          end if >*/
    }

/*        %--------------------------------------------------------% */
/*        | Loop back to extend the factorization by another step. | */
/*        %--------------------------------------------------------% */

/*<       go to 1000 >*/
    goto L1000;

/*     %---------------------------------------------------------------% */
/*     |                                                               | */
/*     |  E N D     O F     M A I N     I T E R A T I O N     L O O P  | */
/*     |                                                               | */
/*     %---------------------------------------------------------------% */

/*<  9000 continue >*/
L9000:
/*<       return >*/
    return 0;

/*     %---------------% */
/*     | End of dsaitr | */
/*     %---------------% */

/*<       end >*/
} /* dsaitr_ */

#ifdef __cplusplus
        }
#endif
