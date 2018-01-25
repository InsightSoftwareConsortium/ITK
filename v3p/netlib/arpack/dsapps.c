/* arpack/dsapps.f -- translated by f2c (version 20090411).
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

static doublereal c_b4 = 0.;
static doublereal c_b5 = 1.;
static doublereal c_b14 = -1.;
static integer c__1 = 1;

/* ----------------------------------------------------------------------- */
/* \BeginDoc */

/* \Name: dsapps */

/* \Description: */
/*  Given the Arnoldi factorization */

/*     A*V_{k} - V_{k}*H_{k} = r_{k+p}*e_{k+p}^T, */

/*  apply NP shifts implicitly resulting in */

/*     A*(V_{k}*Q) - (V_{k}*Q)*(Q^T* H_{k}*Q) = r_{k+p}*e_{k+p}^T * Q */

/*  where Q is an orthogonal matrix of order KEV+NP. Q is the product of */
/*  rotations resulting from the NP bulge chasing sweeps.  The updated Arnoldi */
/*  factorization becomes: */

/*     A*VNEW_{k} - VNEW_{k}*HNEW_{k} = rnew_{k}*e_{k}^T. */

/* \Usage: */
/*  call dsapps */
/*     ( N, KEV, NP, SHIFT, V, LDV, H, LDH, RESID, Q, LDQ, WORKD ) */

/* \Arguments */
/*  N       Integer.  (INPUT) */
/*          Problem size, i.e. dimension of matrix A. */

/*  KEV     Integer.  (INPUT) */
/*          INPUT: KEV+NP is the size of the input matrix H. */
/*          OUTPUT: KEV is the size of the updated matrix HNEW. */

/*  NP      Integer.  (INPUT) */
/*          Number of implicit shifts to be applied. */

/*  SHIFT   Double precision array of length NP.  (INPUT) */
/*          The shifts to be applied. */

/*  V       Double precision N by (KEV+NP) array.  (INPUT/OUTPUT) */
/*          INPUT: V contains the current KEV+NP Arnoldi vectors. */
/*          OUTPUT: VNEW = V(1:n,1:KEV); the updated Arnoldi vectors */
/*          are in the first KEV columns of V. */

/*  LDV     Integer.  (INPUT) */
/*          Leading dimension of V exactly as declared in the calling */
/*          program. */

/*  H       Double precision (KEV+NP) by 2 array.  (INPUT/OUTPUT) */
/*          INPUT: H contains the symmetric tridiagonal matrix of the */
/*          Arnoldi factorization with the subdiagonal in the 1st column */
/*          starting at H(2,1) and the main diagonal in the 2nd column. */
/*          OUTPUT: H contains the updated tridiagonal matrix in the */
/*          KEV leading submatrix. */

/*  LDH     Integer.  (INPUT) */
/*          Leading dimension of H exactly as declared in the calling */
/*          program. */

/*  RESID   Double precision array of length (N).  (INPUT/OUTPUT) */
/*          INPUT: RESID contains the the residual vector r_{k+p}. */
/*          OUTPUT: RESID is the updated residual vector rnew_{k}. */

/*  Q       Double precision KEV+NP by KEV+NP work array.  (WORKSPACE) */
/*          Work array used to accumulate the rotations during the bulge */
/*          chase sweep. */

/*  LDQ     Integer.  (INPUT) */
/*          Leading dimension of Q exactly as declared in the calling */
/*          program. */

/*  WORKD   Double precision work array of length 2*N.  (WORKSPACE) */
/*          Distributed array used in the application of the accumulated */
/*          orthogonal matrix Q. */

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
/*     dlamch  LAPACK routine that determines machine constants. */
/*     dlartg  LAPACK Givens rotation construction routine. */
/*     dlacpy  LAPACK matrix copy routine. */
/*     dlaset  LAPACK matrix initialization routine. */
/*     dgemv   Level 2 BLAS routine for matrix vector multiplication. */
/*     daxpy   Level 1 BLAS that computes a vector triad. */
/*     dcopy   Level 1 BLAS that copies one vector to another. */
/*     dscal   Level 1 BLAS that scales a vector. */

/* \Author */
/*     Danny Sorensen               Phuong Vu */
/*     Richard Lehoucq              CRPC / Rice University */
/*     Dept. of Computational &     Houston, Texas */
/*     Applied Mathematics */
/*     Rice University */
/*     Houston, Texas */

/* \Revision history: */
/*     12/16/93: Version ' 2.1' */

/* \SCCS Information: @(#) */
/* FILE: sapps.F   SID: 2.5   DATE OF SID: 4/19/96   RELEASE: 2 */

/* \Remarks */
/*  1. In this version, each shift is applied to all the subblocks of */
/*     the tridiagonal matrix H and not just to the submatrix that it */
/*     comes from. This routine assumes that the subdiagonal elements */
/*     of H that are stored in h(1:kev+np,1) are nonegative upon input */
/*     and enforce this condition upon output. This version incorporates */
/*     deflation. See code for documentation. */

/* \EndLib */

/* ----------------------------------------------------------------------- */

/*<        >*/
/* Subroutine */ int dsapps_(integer *n, integer *kev, integer *np,
        doublereal *shift, doublereal *v, integer *ldv, doublereal *h__,
        integer *ldh, doublereal *resid, doublereal *q, integer *ldq,
        doublereal *workd)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer h_dim1, h_offset, q_dim1, q_offset, v_dim1, v_offset, i__1, i__2,
            i__3, i__4;
    doublereal d__1, d__2;

    /* Local variables */
    doublereal c__, f, g;
    integer i__, j;
    doublereal r__, s, a1, a2, a3, a4;
/*  static real t0, t1; */
    integer jj;
    doublereal big;
    integer iend, itop;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *), dgemv_(char *, integer *, integer *, doublereal *,
            doublereal *, integer *, doublereal *, integer *, doublereal *,
            doublereal *, integer *, ftnlen), dcopy_(integer *, doublereal *,
            integer *, doublereal *, integer *), daxpy_(integer *, doublereal
            *, doublereal *, integer *, doublereal *, integer *);
    extern doublereal dlamch_(char *, ftnlen);
    extern /* Subroutine */ int second_(real *);
    static doublereal epsmch;
    integer istart, kplusp /*, msglvl */;
    extern /* Subroutine */ int dlacpy_(char *, integer *, integer *,
            doublereal *, integer *, doublereal *, integer *, ftnlen),
            dlartg_(doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *), dlaset_(char *, integer *, integer *, doublereal *,
             doublereal *, doublereal *, integer *, ftnlen);


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
/*<       integer    kev, ldh, ldq, ldv, n, np >*/

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

/*     %-----------------% */
/*     | Array Arguments | */
/*     %-----------------% */

/*<        >*/

/*     %------------% */
/*     | Parameters | */
/*     %------------% */

/*<        >*/
/*<       parameter (one = 1.0D+0, zero = 0.0D+0) >*/

/*     %---------------% */
/*     | Local Scalars | */
/*     %---------------% */

/*<       integer    i, iend, istart, itop, j, jj, kplusp, msglvl >*/
/*<       logical    first >*/
/*<        >*/
/*<       save       epsmch, first >*/


/*     %----------------------% */
/*     | External Subroutines | */
/*     %----------------------% */

/*<        >*/

/*     %--------------------% */
/*     | External Functions | */
/*     %--------------------% */

/*<        >*/
/*<       external   dlamch >*/

/*     %----------------------% */
/*     | Intrinsics Functions | */
/*     %----------------------% */

/*<       intrinsic  abs >*/

/*     %----------------% */
/*     | Data statments | */
/*     %----------------% */

/*<       data       first / .true. / >*/
    /* Parameter adjustments */
    --workd;
    --resid;
    --shift;
    v_dim1 = *ldv;
    v_offset = 1 + v_dim1;
    v -= v_offset;
    h_dim1 = *ldh;
    h_offset = 1 + h_dim1;
    h__ -= h_offset;
    q_dim1 = *ldq;
    q_offset = 1 + q_dim1;
    q -= q_offset;

    /* Function Body */

/*     %-----------------------% */
/*     | Executable Statements | */
/*     %-----------------------% */

/*<       if (first) then >*/
    if (first) {
/*<          epsmch = dlamch('Epsilon-Machine') >*/
        epsmch = dlamch_("Epsilon-Machine", (ftnlen)15);
/*<          first = .false. >*/
        first = FALSE_;
/*<       end if >*/
    }
/*<       itop = 1 >*/
    itop = 1;

/*     %-------------------------------% */
/*     | Initialize timing statistics  | */
/*     | & message level for debugging | */
/*     %-------------------------------% */

/*<       call second (t0) >*/
/*  second_(&t0); */
/*<       msglvl = msapps >*/
/*  msglvl = debug_1.msapps; */

/*<       kplusp = kev + np  >*/
    kplusp = *kev + *np;

/*     %----------------------------------------------% */
/*     | Initialize Q to the identity matrix of order | */
/*     | kplusp used to accumulate the rotations.     | */
/*     %----------------------------------------------% */

/*<       call dlaset ('All', kplusp, kplusp, zero, one, q, ldq) >*/
    dlaset_("All", &kplusp, &kplusp, &c_b4, &c_b5, &q[q_offset], ldq, (ftnlen)
            3);

/*     %----------------------------------------------% */
/*     | Quick return if there are no shifts to apply | */
/*     %----------------------------------------------% */

/*<       if (np .eq. 0) go to 9000 >*/
    if (*np == 0) {
        goto L9000;
    }

/*     %----------------------------------------------------------% */
/*     | Apply the np shifts implicitly. Apply each shift to the  | */
/*     | whole matrix and not just to the submatrix from which it | */
/*     | comes.                                                   | */
/*     %----------------------------------------------------------% */

/*<       do 90 jj = 1, np >*/
    i__1 = *np;
    for (jj = 1; jj <= i__1; ++jj) {

/*<          istart = itop >*/
        istart = itop;

/*        %----------------------------------------------------------% */
/*        | Check for splitting and deflation. Currently we consider | */
/*        | an off-diagonal element h(i+1,1) negligible if           | */
/*        |         h(i+1,1) .le. epsmch*( |h(i,2)| + |h(i+1,2)| )   | */
/*        | for i=1:KEV+NP-1.                                        | */
/*        | If above condition tests true then we set h(i+1,1) = 0.  | */
/*        | Note that h(1:KEV+NP,1) are assumed to be non negative.  | */
/*        %----------------------------------------------------------% */

/*<    20    continue >*/
L20:

/*        %------------------------------------------------% */
/*        | The following loop exits early if we encounter | */
/*        | a negligible off diagonal element.             | */
/*        %------------------------------------------------% */

/*<          do 30 i = istart, kplusp-1 >*/
        i__2 = kplusp - 1;
        for (i__ = istart; i__ <= i__2; ++i__) {
/*<             big   = abs(h(i,2)) + abs(h(i+1,2)) >*/
            big = (d__1 = h__[i__ + (h_dim1 << 1)], abs(d__1)) + (d__2 = h__[
                    i__ + 1 + (h_dim1 << 1)], abs(d__2));
/*<             if (h(i+1,1) .le. epsmch*big) then >*/
            if (h__[i__ + 1 + h_dim1] <= epsmch * big) {
/*               if (msglvl .gt. 0) then */
/*                  call ivout (logfil, 1, i, ndigit, */
/*     &                 '_sapps: deflation at row/column no.') */
/*                  call ivout (logfil, 1, jj, ndigit, */
/*     &                 '_sapps: occurred before shift number.') */
/*                  call dvout (logfil, 1, h(i+1,1), ndigit, */
/*     &                 '_sapps: the corresponding off diagonal element') */
/*               end if */
/*<                h(i+1,1) = zero >*/
                h__[i__ + 1 + h_dim1] = 0.;
/*<                iend = i >*/
                iend = i__;
/*<                go to 40 >*/
                goto L40;
/*<             end if >*/
            }
/*<    30    continue >*/
/* L30: */
        }
/*<          iend = kplusp >*/
        iend = kplusp;
/*<    40    continue >*/
L40:

/*<          if (istart .lt. iend) then >*/
        if (istart < iend) {

/*           %--------------------------------------------------------% */
/*           | Construct the plane rotation G'(istart,istart+1,theta) | */
/*           | that attempts to drive h(istart+1,1) to zero.          | */
/*           %--------------------------------------------------------% */

/*<              f = h(istart,2) - shift(jj) >*/
            f = h__[istart + (h_dim1 << 1)] - shift[jj];
/*<              g = h(istart+1,1) >*/
            g = h__[istart + 1 + h_dim1];
/*<              call dlartg (f, g, c, s, r) >*/
            dlartg_(&f, &g, &c__, &s, &r__);

/*            %-------------------------------------------------------% */
/*            | Apply rotation to the left and right of H;            | */
/*            | H <- G' * H * G,  where G = G(istart,istart+1,theta). | */
/*            | This will create a "bulge".                           | */
/*            %-------------------------------------------------------% */

/*<              a1 = c*h(istart,2)   + s*h(istart+1,1) >*/
            a1 = c__ * h__[istart + (h_dim1 << 1)] + s * h__[istart + 1 +
                    h_dim1];
/*<              a2 = c*h(istart+1,1) + s*h(istart+1,2) >*/
            a2 = c__ * h__[istart + 1 + h_dim1] + s * h__[istart + 1 + (
                    h_dim1 << 1)];
/*<              a4 = c*h(istart+1,2) - s*h(istart+1,1) >*/
            a4 = c__ * h__[istart + 1 + (h_dim1 << 1)] - s * h__[istart + 1 +
                    h_dim1];
/*<              a3 = c*h(istart+1,1) - s*h(istart,2)  >*/
            a3 = c__ * h__[istart + 1 + h_dim1] - s * h__[istart + (h_dim1 <<
                    1)];
/*<              h(istart,2)   = c*a1 + s*a2 >*/
            h__[istart + (h_dim1 << 1)] = c__ * a1 + s * a2;
/*<              h(istart+1,2) = c*a4 - s*a3 >*/
            h__[istart + 1 + (h_dim1 << 1)] = c__ * a4 - s * a3;
/*<              h(istart+1,1) = c*a3 + s*a4 >*/
            h__[istart + 1 + h_dim1] = c__ * a3 + s * a4;

/*            %----------------------------------------------------% */
/*            | Accumulate the rotation in the matrix Q;  Q <- Q*G | */
/*            %----------------------------------------------------% */

/*<              do 60 j = 1, min(istart+jj,kplusp) >*/
/* Computing MIN */
            i__3 = istart + jj;
            i__2 = min(i__3,kplusp);
            for (j = 1; j <= i__2; ++j) {
/*<                 a1            =   c*q(j,istart) + s*q(j,istart+1) >*/
                a1 = c__ * q[j + istart * q_dim1] + s * q[j + (istart + 1) *
                        q_dim1];
/*<                 q(j,istart+1) = - s*q(j,istart) + c*q(j,istart+1) >*/
                q[j + (istart + 1) * q_dim1] = -s * q[j + istart * q_dim1] +
                        c__ * q[j + (istart + 1) * q_dim1];
/*<                 q(j,istart)   = a1 >*/
                q[j + istart * q_dim1] = a1;
/*<    60        continue >*/
/* L60: */
            }


/*            %----------------------------------------------% */
/*            | The following loop chases the bulge created. | */
/*            | Note that the previous rotation may also be  | */
/*            | done within the following loop. But it is    | */
/*            | kept separate to make the distinction among  | */
/*            | the bulge chasing sweeps and the first plane | */
/*            | rotation designed to drive h(istart+1,1) to  | */
/*            | zero.                                        | */
/*            %----------------------------------------------% */

/*<              do 70 i = istart+1, iend-1 >*/
            i__2 = iend - 1;
            for (i__ = istart + 1; i__ <= i__2; ++i__) {

/*               %----------------------------------------------% */
/*               | Construct the plane rotation G'(i,i+1,theta) | */
/*               | that zeros the i-th bulge that was created   | */
/*               | by G(i-1,i,theta). g represents the bulge.   | */
/*               %----------------------------------------------% */

/*<                 f = h(i,1) >*/
                f = h__[i__ + h_dim1];
/*<                 g = s*h(i+1,1) >*/
                g = s * h__[i__ + 1 + h_dim1];

/*               %----------------------------------% */
/*               | Final update with G(i-1,i,theta) | */
/*               %----------------------------------% */

/*<                 h(i+1,1) = c*h(i+1,1) >*/
                h__[i__ + 1 + h_dim1] = c__ * h__[i__ + 1 + h_dim1];
/*<                 call dlartg (f, g, c, s, r) >*/
                dlartg_(&f, &g, &c__, &s, &r__);

/*               %-------------------------------------------% */
/*               | The following ensures that h(1:iend-1,1), | */
/*               | the first iend-2 off diagonal of elements | */
/*               | H, remain non negative.                   | */
/*               %-------------------------------------------% */

/*<                 if (r .lt. zero) then >*/
                if (r__ < 0.) {
/*<                    r = -r >*/
                    r__ = -r__;
/*<                    c = -c >*/
                    c__ = -c__;
/*<                    s = -s >*/
                    s = -s;
/*<                 end if >*/
                }

/*               %--------------------------------------------% */
/*               | Apply rotation to the left and right of H; | */
/*               | H <- G * H * G',  where G = G(i,i+1,theta) | */
/*               %--------------------------------------------% */

/*<                 h(i,1) = r >*/
                h__[i__ + h_dim1] = r__;

/*<                 a1 = c*h(i,2)   + s*h(i+1,1) >*/
                a1 = c__ * h__[i__ + (h_dim1 << 1)] + s * h__[i__ + 1 +
                        h_dim1];
/*<                 a2 = c*h(i+1,1) + s*h(i+1,2) >*/
                a2 = c__ * h__[i__ + 1 + h_dim1] + s * h__[i__ + 1 + (h_dim1
                        << 1)];
/*<                 a3 = c*h(i+1,1) - s*h(i,2) >*/
                a3 = c__ * h__[i__ + 1 + h_dim1] - s * h__[i__ + (h_dim1 << 1)
                        ];
/*<                 a4 = c*h(i+1,2) - s*h(i+1,1) >*/
                a4 = c__ * h__[i__ + 1 + (h_dim1 << 1)] - s * h__[i__ + 1 +
                        h_dim1];

/*<                 h(i,2)   = c*a1 + s*a2 >*/
                h__[i__ + (h_dim1 << 1)] = c__ * a1 + s * a2;
/*<                 h(i+1,2) = c*a4 - s*a3 >*/
                h__[i__ + 1 + (h_dim1 << 1)] = c__ * a4 - s * a3;
/*<                 h(i+1,1) = c*a3 + s*a4 >*/
                h__[i__ + 1 + h_dim1] = c__ * a3 + s * a4;

/*               %----------------------------------------------------% */
/*               | Accumulate the rotation in the matrix Q;  Q <- Q*G | */
/*               %----------------------------------------------------% */

/*<                 do 50 j = 1, min( j+jj, kplusp ) >*/
/* Computing MIN */
                i__4 = j + jj;
                i__3 = min(i__4,kplusp);
                for (j = 1; j <= i__3; ++j) {
/*<                    a1       =   c*q(j,i) + s*q(j,i+1) >*/
                    a1 = c__ * q[j + i__ * q_dim1] + s * q[j + (i__ + 1) *
                            q_dim1];
/*<                    q(j,i+1) = - s*q(j,i) + c*q(j,i+1) >*/
                    q[j + (i__ + 1) * q_dim1] = -s * q[j + i__ * q_dim1] +
                            c__ * q[j + (i__ + 1) * q_dim1];
/*<                    q(j,i)   = a1 >*/
                    q[j + i__ * q_dim1] = a1;
/*<    50           continue >*/
/* L50: */
                }

/*<    70        continue >*/
/* L70: */
            }

/*<          end if >*/
        }

/*        %--------------------------% */
/*        | Update the block pointer | */
/*        %--------------------------% */

/*<          istart = iend + 1 >*/
        istart = iend + 1;

/*        %------------------------------------------% */
/*        | Make sure that h(iend,1) is non-negative | */
/*        | If not then set h(iend,1) <-- -h(iend,1) | */
/*        | and negate the last column of Q.         | */
/*        | We have effectively carried out a        | */
/*        | similarity on transformation H           | */
/*        %------------------------------------------% */

/*<          if (h(iend,1) .lt. zero) then >*/
        if (h__[iend + h_dim1] < 0.) {
/*<              h(iend,1) = -h(iend,1) >*/
            h__[iend + h_dim1] = -h__[iend + h_dim1];
/*<              call dscal(kplusp, -one, q(1,iend), 1) >*/
            dscal_(&kplusp, &c_b14, &q[iend * q_dim1 + 1], &c__1);
/*<          end if >*/
        }

/*        %--------------------------------------------------------% */
/*        | Apply the same shift to the next block if there is any | */
/*        %--------------------------------------------------------% */

/*<          if (iend .lt. kplusp) go to 20 >*/
        if (iend < kplusp) {
            goto L20;
        }

/*        %-----------------------------------------------------% */
/*        | Check if we can increase the the start of the block | */
/*        %-----------------------------------------------------% */

/*<          do 80 i = itop, kplusp-1 >*/
        i__2 = kplusp - 1;
        for (i__ = itop; i__ <= i__2; ++i__) {
/*<             if (h(i+1,1) .gt. zero) go to 90 >*/
            if (h__[i__ + 1 + h_dim1] > 0.) {
                goto L90;
            }
/*<             itop  = itop + 1 >*/
            ++itop;
/*<    80    continue >*/
/* L80: */
        }

/*        %-----------------------------------% */
/*        | Finished applying the jj-th shift | */
/*        %-----------------------------------% */

/*<    90 continue >*/
L90:
        ;
    }

/*     %------------------------------------------% */
/*     | All shifts have been applied. Check for  | */
/*     | more possible deflation that might occur | */
/*     | after the last shift is applied.         | */
/*     %------------------------------------------% */

/*<       do 100 i = itop, kplusp-1 >*/
    i__1 = kplusp - 1;
    for (i__ = itop; i__ <= i__1; ++i__) {
/*<          big   = abs(h(i,2)) + abs(h(i+1,2)) >*/
        big = (d__1 = h__[i__ + (h_dim1 << 1)], abs(d__1)) + (d__2 = h__[i__
                + 1 + (h_dim1 << 1)], abs(d__2));
/*<          if (h(i+1,1) .le. epsmch*big) then >*/
        if (h__[i__ + 1 + h_dim1] <= epsmch * big) {
/*            if (msglvl .gt. 0) then */
/*               call ivout (logfil, 1, i, ndigit, */
/*     &              '_sapps: deflation at row/column no.') */
/*               call dvout (logfil, 1, h(i+1,1), ndigit, */
/*     &              '_sapps: the corresponding off diagonal element') */
/*            end if */
/*<             h(i+1,1) = zero >*/
            h__[i__ + 1 + h_dim1] = 0.;
/*<          end if >*/
        }
/*<  100  continue >*/
/* L100: */
    }

/*     %-------------------------------------------------% */
/*     | Compute the (kev+1)-st column of (V*Q) and      | */
/*     | temporarily store the result in WORKD(N+1:2*N). | */
/*     | This is not necessary if h(kev+1,1) = 0.         | */
/*     %-------------------------------------------------% */

/*<        >*/
    if (h__[*kev + 1 + h_dim1] > 0.) {
        dgemv_("N", n, &kplusp, &c_b5, &v[v_offset], ldv, &q[(*kev + 1) *
                q_dim1 + 1], &c__1, &c_b4, &workd[*n + 1], &c__1, (ftnlen)1);
    }

/*     %-------------------------------------------------------% */
/*     | Compute column 1 to kev of (V*Q) in backward order    | */
/*     | taking advantage that Q is an upper triangular matrix | */
/*     | with lower bandwidth np.                              | */
/*     | Place results in v(:,kplusp-kev:kplusp) temporarily.  | */
/*     %-------------------------------------------------------% */

/*<       do 130 i = 1, kev >*/
    i__1 = *kev;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<        >*/
        i__2 = kplusp - i__ + 1;
        dgemv_("N", n, &i__2, &c_b5, &v[v_offset], ldv, &q[(*kev - i__ + 1) *
                q_dim1 + 1], &c__1, &c_b4, &workd[1], &c__1, (ftnlen)1);
/*<          call dcopy (n, workd, 1, v(1,kplusp-i+1), 1) >*/
        dcopy_(n, &workd[1], &c__1, &v[(kplusp - i__ + 1) * v_dim1 + 1], &
                c__1);
/*<   130 continue >*/
/* L130: */
    }

/*     %-------------------------------------------------% */
/*     |  Move v(:,kplusp-kev+1:kplusp) into v(:,1:kev). | */
/*     %-------------------------------------------------% */

/*<       call dlacpy ('All', n, kev, v(1,np+1), ldv, v, ldv) >*/
    dlacpy_("All", n, kev, &v[(*np + 1) * v_dim1 + 1], ldv, &v[v_offset], ldv,
             (ftnlen)3);

/*     %--------------------------------------------% */
/*     | Copy the (kev+1)-st column of (V*Q) in the | */
/*     | appropriate place if h(kev+1,1) .ne. zero. | */
/*     %--------------------------------------------% */

/*<        >*/
    if (h__[*kev + 1 + h_dim1] > 0.) {
        dcopy_(n, &workd[*n + 1], &c__1, &v[(*kev + 1) * v_dim1 + 1], &c__1);
    }

/*     %-------------------------------------% */
/*     | Update the residual vector:         | */
/*     |    r <- sigmak*r + betak*v(:,kev+1) | */
/*     | where                               | */
/*     |    sigmak = (e_{kev+p}'*Q)*e_{kev}  | */
/*     |    betak = e_{kev+1}'*H*e_{kev}     | */
/*     %-------------------------------------% */

/*<       call dscal (n, q(kplusp,kev), resid, 1) >*/
    dscal_(n, &q[kplusp + *kev * q_dim1], &resid[1], &c__1);
/*<        >*/
    if (h__[*kev + 1 + h_dim1] > 0.) {
        daxpy_(n, &h__[*kev + 1 + h_dim1], &v[(*kev + 1) * v_dim1 + 1], &c__1,
                 &resid[1], &c__1);
    }

/*      if (msglvl .gt. 1) then */
/*         call dvout (logfil, 1, q(kplusp,kev), ndigit, */
/*     &      '_sapps: sigmak of the updated residual vector') */
/*         call dvout (logfil, 1, h(kev+1,1), ndigit, */
/*     &      '_sapps: betak of the updated residual vector') */
/*         call dvout (logfil, kev, h(1,2), ndigit, */
/*     &      '_sapps: updated main diagonal of H for next iteration') */
/*         if (kev .gt. 1) then */
/*         call dvout (logfil, kev-1, h(2,1), ndigit, */
/*     &      '_sapps: updated sub diagonal of H for next iteration') */
/*         end if */
/*      end if */

/*<       call second (t1) >*/
/*  second_(&t1); */
/*<       tsapps = tsapps + (t1 - t0) >*/
/*  timing_1.tsapps += t1 - t0; */

/*<  9000 continue  >*/
L9000:
/*<       return >*/
    return 0;

/*     %---------------% */
/*     | End of dsapps | */
/*     %---------------% */

/*<       end >*/
} /* dsapps_ */

#ifdef __cplusplus
        }
#endif
