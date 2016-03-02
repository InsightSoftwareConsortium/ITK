/* arpack/dsconv.f -- translated by f2c (version 20090411).
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
/*  integer nopx, nbx, nrorth, nitref, nrstrt; */
/*  real tsaupd, tsaup2, tsaitr, tseigt, tsgets, tsapps, tsconv, tnaupd, */
/*          tnaup2, tnaitr, tneigh, tngets, tnapps, tnconv, tcaupd, tcaup2, */
/*          tcaitr, tceigh, tcgets, tcapps, tcconv, tmvopx, tmvbx, tgetv0, */
/*          titref, trvec; */
/*} timing_; */

/*#define timing_1 timing_ */

/* Table of constant values */

static doublereal c_b3 = .66666666666666663;

/* ----------------------------------------------------------------------- */
/* \BeginDoc */

/* \Name: dsconv */

/* \Description: */
/*  Convergence testing for the symmetric Arnoldi eigenvalue routine. */

/* \Usage: */
/*  call dsconv */
/*     ( N, RITZ, BOUNDS, TOL, NCONV ) */

/* \Arguments */
/*  N       Integer.  (INPUT) */
/*          Number of Ritz values to check for convergence. */

/*  RITZ    Double precision array of length N.  (INPUT) */
/*          The Ritz values to be checked for convergence. */

/*  BOUNDS  Double precision array of length N.  (INPUT) */
/*          Ritz estimates associated with the Ritz values in RITZ. */

/*  TOL     Double precision scalar.  (INPUT) */
/*          Desired relative accuracy for a Ritz value to be considered */
/*          "converged". */

/*  NCONV   Integer scalar.  (OUTPUT) */
/*          Number of "converged" Ritz values. */

/* \EndDoc */

/* ----------------------------------------------------------------------- */

/* \BeginLib */

/* \Routines called: */
/*     second  ARPACK utility routine for timing. */
/*     dlamch  LAPACK routine that determines machine constants. */

/* \Author */
/*     Danny Sorensen               Phuong Vu */
/*     Richard Lehoucq              CRPC / Rice University */
/*     Dept. of Computational &     Houston, Texas */
/*     Applied Mathematics */
/*     Rice University */
/*     Houston, Texas */

/* \SCCS Information: @(#) */
/* FILE: sconv.F   SID: 2.4   DATE OF SID: 4/19/96   RELEASE: 2 */

/* \Remarks */
/*     1. Starting with version 2.4, this routine no longer uses the */
/*        Parlett strategy using the gap conditions. */

/* \EndLib */

/* ----------------------------------------------------------------------- */

/*<       subroutine dsconv (n, ritz, bounds, tol, nconv) >*/
/* Subroutine */ int dsconv_(integer *n, doublereal *ritz, doublereal *bounds,
         doublereal *tol, integer *nconv)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *);

    /* Local variables */
    integer i__;
/*  static real t0, t1; */
    doublereal eps23, temp;
    extern doublereal dlamch_(char *, ftnlen);
    extern /* Subroutine */ int second_(real *);


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
/*<       integer    n, nconv >*/

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

/*     %-----------------% */
/*     | Array Arguments | */
/*     %-----------------% */

/*<        >*/

/*     %---------------% */
/*     | Local Scalars | */
/*     %---------------% */

/*<       integer    i >*/
/*<        >*/

/*     %-------------------% */
/*     | External routines | */
/*     %-------------------% */

/*<        >*/
/*<       external   dlamch >*/
/*     %---------------------% */
/*     | Intrinsic Functions | */
/*     %---------------------% */

/*<       intrinsic    abs >*/

/*     %-----------------------% */
/*     | Executable Statements | */
/*     %-----------------------% */

/*<       call second (t0) >*/
    /* Parameter adjustments */
    --bounds;
    --ritz;

    /* Function Body */
/*  second_(&t0); */

/*<       eps23 = dlamch('Epsilon-Machine')  >*/
    eps23 = dlamch_("Epsilon-Machine", (ftnlen)15);
/*<       eps23 = eps23**(2.0D+0 / 3.0D+0) >*/
    eps23 = pow_dd(&eps23, &c_b3);

/*<       nconv  = 0 >*/
    *nconv = 0;
/*<       do 10 i = 1, n >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        %-----------------------------------------------------% */
/*        | The i-th Ritz value is considered "converged"       | */
/*        | when: bounds(i) .le. TOL*max(eps23, abs(ritz(i)))   | */
/*        %-----------------------------------------------------% */

/*<          temp = max( eps23, abs(ritz(i)) ) >*/
/* Computing MAX */
        d__2 = eps23, d__3 = (d__1 = ritz[i__], abs(d__1));
        temp = max(d__2,d__3);
/*<          if ( bounds(i) .le. tol*temp ) then >*/
        if (bounds[i__] <= *tol * temp) {
/*<             nconv = nconv + 1 >*/
            ++(*nconv);
/*<          end if >*/
        }

/*<    10 continue >*/
/* L10: */
    }

/*<       call second (t1) >*/
/*  second_(&t1); */
/*<       tsconv = tsconv + (t1 - t0) >*/
/*  timing_1.tsconv += t1 - t0; */

/*<       return >*/
    return 0;

/*     %---------------% */
/*     | End of dsconv | */
/*     %---------------% */

/*<       end >*/
} /* dsconv_ */

#ifdef __cplusplus
        }
#endif
