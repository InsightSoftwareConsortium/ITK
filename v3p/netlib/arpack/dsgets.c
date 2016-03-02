/* arpack/dsgets.f -- translated by f2c (version 20090411).
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

static logical c_true = TRUE_;
static integer c__1 = 1;

/* ----------------------------------------------------------------------- */
/* \BeginDoc */

/* \Name: dsgets */

/* \Description: */
/*  Given the eigenvalues of the symmetric tridiagonal matrix H, */
/*  computes the NP shifts AMU that are zeros of the polynomial of */
/*  degree NP which filters out components of the unwanted eigenvectors */
/*  corresponding to the AMU's based on some given criteria. */

/*  NOTE: This is called even in the case of user specified shifts in */
/*  order to sort the eigenvalues, and error bounds of H for later use. */

/* \Usage: */
/*  call dsgets */
/*     ( ISHIFT, WHICH, KEV, NP, RITZ, BOUNDS, SHIFTS ) */

/* \Arguments */
/*  ISHIFT  Integer.  (INPUT) */
/*          Method for selecting the implicit shifts at each iteration. */
/*          ISHIFT = 0: user specified shifts */
/*          ISHIFT = 1: exact shift with respect to the matrix H. */

/*  WHICH   Character*2.  (INPUT) */
/*          Shift selection criteria. */
/*          'LM' -> KEV eigenvalues of largest magnitude are retained. */
/*          'SM' -> KEV eigenvalues of smallest magnitude are retained. */
/*          'LA' -> KEV eigenvalues of largest value are retained. */
/*          'SA' -> KEV eigenvalues of smallest value are retained. */
/*          'BE' -> KEV eigenvalues, half from each end of the spectrum. */
/*                  If KEV is odd, compute one more from the high end. */

/*  KEV      Integer.  (INPUT) */
/*          KEV+NP is the size of the matrix H. */

/*  NP      Integer.  (INPUT) */
/*          Number of implicit shifts to be computed. */

/*  RITZ    Double precision array of length KEV+NP.  (INPUT/OUTPUT) */
/*          On INPUT, RITZ contains the eigenvalues of H. */
/*          On OUTPUT, RITZ are sorted so that the unwanted eigenvalues */
/*          are in the first NP locations and the wanted part is in */
/*          the last KEV locations.  When exact shifts are selected, the */
/*          unwanted part corresponds to the shifts to be applied. */

/*  BOUNDS  Double precision array of length KEV+NP.  (INPUT/OUTPUT) */
/*          Error bounds corresponding to the ordering in RITZ. */

/*  SHIFTS  Double precision array of length NP.  (INPUT/OUTPUT) */
/*          On INPUT:  contains the user specified shifts if ISHIFT = 0. */
/*          On OUTPUT: contains the shifts sorted into decreasing order */
/*          of magnitude with respect to the Ritz estimates contained in */
/*          BOUNDS. If ISHIFT = 0, SHIFTS is not modified on exit. */

/* \EndDoc */

/* ----------------------------------------------------------------------- */

/* \BeginLib */

/* \Local variables: */
/*     xxxxxx  real */

/* \Routines called: */
/*     dsortr  ARPACK utility sorting routine. */
/*     second  ARPACK utility routine for timing. */
/*     dcopy   Level 1 BLAS that copies one vector to another. */
/*     dswap   Level 1 BLAS that swaps the contents of two vectors. */

/* \Author */
/*     Danny Sorensen               Phuong Vu */
/*     Richard Lehoucq              CRPC / Rice University */
/*     Dept. of Computational &     Houston, Texas */
/*     Applied Mathematics */
/*     Rice University */
/*     Houston, Texas */

/* \Revision history: */
/*     xx/xx/93: Version ' 2.1' */

/* \SCCS Information: @(#) */
/* FILE: sgets.F   SID: 2.4   DATE OF SID: 4/19/96   RELEASE: 2 */

/* \Remarks */

/* \EndLib */

/* ----------------------------------------------------------------------- */

/*<       subroutine dsgets ( ishift, which, kev, np, ritz, bounds, shifts ) >*/
/* Subroutine */ int dsgets_(integer *ishift, char *which, integer *kev,
        integer *np, doublereal *ritz, doublereal *bounds, doublereal *shifts,
         ftnlen which_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
/*  static real t0, t1; */
    integer kevd2;
    extern /* Subroutine */ int dswap_(integer *, doublereal *, integer *,
            doublereal *, integer *), dcopy_(integer *, doublereal *, integer
            *, doublereal *, integer *), second_(real *);
/*  integer msglvl; */
    extern /* Subroutine */ int dsortr_(char *, logical *, integer *,
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
/*<       character*2 which >*/

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
/*<       integer    ishift, kev, np >*/

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

/*<       integer    kevd2, msglvl >*/

/*     %----------------------% */
/*     | External Subroutines | */
/*     %----------------------% */

/*<       external   dswap, dcopy, dsortr, second >*/

/*     %---------------------% */
/*     | Intrinsic Functions | */
/*     %---------------------% */

/*<       intrinsic    max, min >*/

/*     %-----------------------% */
/*     | Executable Statements | */
/*     %-----------------------% */

/*     %-------------------------------% */
/*     | Initialize timing statistics  | */
/*     | & message level for debugging | */
/*     %-------------------------------% */

/*<       call second (t0) >*/
    /* Parameter adjustments */
    --shifts;
    --bounds;
    --ritz;

    /* Function Body */
/*  second_(&t0); */
/*<       msglvl = msgets >*/
/*  msglvl = debug_1.msgets; */

/*<       if (which .eq. 'BE') then >*/
    if (s_cmp(which, "BE", (ftnlen)2, (ftnlen)2) == 0) {

/*        %-----------------------------------------------------% */
/*        | Both ends of the spectrum are requested.            | */
/*        | Sort the eigenvalues into algebraically increasing  | */
/*        | order first then swap high end of the spectrum next | */
/*        | to low end in appropriate locations.                | */
/*        | NOTE: when np < floor(kev/2) be careful not to swap | */
/*        | overlapping locations.                              | */
/*        %-----------------------------------------------------% */

/*<          call dsortr ('LA', .true., kev+np, ritz, bounds) >*/
        i__1 = *kev + *np;
        dsortr_("LA", &c_true, &i__1, &ritz[1], &bounds[1], (ftnlen)2);
/*<          kevd2 = kev / 2  >*/
        kevd2 = *kev / 2;
/*<          if ( kev .gt. 1 ) then >*/
        if (*kev > 1) {
/*<        >*/
            i__1 = min(kevd2,*np);
            dswap_(&i__1, &ritz[1], &c__1, &ritz[max(kevd2,*np) + 1], &c__1);
/*<        >*/
            i__1 = min(kevd2,*np);
            dswap_(&i__1, &bounds[1], &c__1, &bounds[max(kevd2,*np) + 1], &
                    c__1);
/*<          end if >*/
        }

/*<       else >*/
    } else {

/*        %----------------------------------------------------% */
/*        | LM, SM, LA, SA case.                               | */
/*        | Sort the eigenvalues of H into the desired order   | */
/*        | and apply the resulting order to BOUNDS.           | */
/*        | The eigenvalues are sorted so that the wanted part | */
/*        | are always in the last KEV locations.               | */
/*        %----------------------------------------------------% */

/*<          call dsortr (which, .true., kev+np, ritz, bounds) >*/
        i__1 = *kev + *np;
        dsortr_(which, &c_true, &i__1, &ritz[1], &bounds[1], (ftnlen)2);
/*<       end if >*/
    }

/*<       if (ishift .eq. 1 .and. np .gt. 0) then >*/
    if (*ishift == 1 && *np > 0) {

/*        %-------------------------------------------------------% */
/*        | Sort the unwanted Ritz values used as shifts so that  | */
/*        | the ones with largest Ritz estimates are first.       | */
/*        | This will tend to minimize the effects of the         | */
/*        | forward instability of the iteration when the shifts  | */
/*        | are applied in subroutine dsapps.                     | */
/*        %-------------------------------------------------------% */

/*<          call dsortr ('SM', .true., np, bounds, ritz) >*/
        dsortr_("SM", &c_true, np, &bounds[1], &ritz[1], (ftnlen)2);
/*<          call dcopy (np, ritz, 1, shifts, 1) >*/
        dcopy_(np, &ritz[1], &c__1, &shifts[1], &c__1);
/*<       end if >*/
    }

/*<       call second (t1) >*/
/*  second_(&t1); */
/*<       tsgets = tsgets + (t1 - t0) >*/
/*  timing_1.tsgets += t1 - t0; */

/*      if (msglvl .gt. 0) then */
/*         call ivout (logfil, 1, kev, ndigit, '_sgets: KEV is') */
/*         call ivout (logfil, 1, np, ndigit, '_sgets: NP is') */
/*         call dvout (logfil, kev+np, ritz, ndigit, */
/*     &        '_sgets: Eigenvalues of current H matrix') */
/*         call dvout (logfil, kev+np, bounds, ndigit, */
/*     &        '_sgets: Associated Ritz estimates') */
/*      end if */

/*<       return >*/
    return 0;

/*     %---------------% */
/*     | End of dsgets | */
/*     %---------------% */

/*<       end >*/
} /* dsgets_ */

#ifdef __cplusplus
        }
#endif
