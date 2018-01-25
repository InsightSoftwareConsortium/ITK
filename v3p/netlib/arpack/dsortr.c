/* arpack/dsortr.f -- translated by f2c (version 20090411).
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

/* ----------------------------------------------------------------------- */
/* \BeginDoc */

/* \Name: dsortr */

/* \Description: */
/*  Sort the array X1 in the order specified by WHICH and optionally */
/*  applies the permutation to the array X2. */

/* \Usage: */
/*  call dsortr */
/*     ( WHICH, APPLY, N, X1, X2 ) */

/* \Arguments */
/*  WHICH   Character*2.  (Input) */
/*          'LM' -> X1 is sorted into increasing order of magnitude. */
/*          'SM' -> X1 is sorted into decreasing order of magnitude. */
/*          'LA' -> X1 is sorted into increasing order of algebraic. */
/*          'SA' -> X1 is sorted into decreasing order of algebraic. */

/*  APPLY   Logical.  (Input) */
/*          APPLY = .TRUE.  -> apply the sorted order to X2. */
/*          APPLY = .FALSE. -> do not apply the sorted order to X2. */

/*  N       Integer.  (INPUT) */
/*          Size of the arrays. */

/*  X1      Double precision array of length N.  (INPUT/OUTPUT) */
/*          The array to be sorted. */

/*  X2      Double precision array of length N.  (INPUT/OUTPUT) */
/*          Only referenced if APPLY = .TRUE. */

/* \EndDoc */

/* ----------------------------------------------------------------------- */

/* \BeginLib */

/* \Author */
/*     Danny Sorensen               Phuong Vu */
/*     Richard Lehoucq              CRPC / Rice University */
/*     Dept. of Computational &     Houston, Texas */
/*     Applied Mathematics */
/*     Rice University */
/*     Houston, Texas */

/* \Revision history: */
/*     12/16/93: Version ' 2.1'. */
/*               Adapted from the sort routine in LANSO. */

/* \SCCS Information: @(#) */
/* FILE: sortr.F   SID: 2.3   DATE OF SID: 4/19/96   RELEASE: 2 */

/* \EndLib */

/* ----------------------------------------------------------------------- */

/*<       subroutine dsortr (which, apply, n, x1, x2) >*/
/* Subroutine */ int dsortr_(char *which, logical *apply, integer *n,
        doublereal *x1, doublereal *x2, ftnlen which_len)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__, j, igap;
    doublereal temp;


/*     %------------------% */
/*     | Scalar Arguments | */
/*     %------------------% */

/*<       character*2 which >*/
/*<       logical    apply >*/
/*<       integer    n >*/

/*     %-----------------% */
/*     | Array Arguments | */
/*     %-----------------% */

/*<        >*/

/*     %---------------% */
/*     | Local Scalars | */
/*     %---------------% */

/*<       integer    i, igap, j >*/
/*<        >*/

/*     %-----------------------% */
/*     | Executable Statements | */
/*     %-----------------------% */

/*<       igap = n / 2 >*/
    igap = *n / 2;

/*<       if (which .eq. 'SA') then >*/
    if (s_cmp(which, "SA", (ftnlen)2, (ftnlen)2) == 0) {

/*        X1 is sorted into decreasing order of algebraic. */

/*<    10    continue >*/
L10:
/*<          if (igap .eq. 0) go to 9000 >*/
        if (igap == 0) {
            goto L9000;
        }
/*<          do 30 i = igap, n-1 >*/
        i__1 = *n - 1;
        for (i__ = igap; i__ <= i__1; ++i__) {
/*<             j = i-igap >*/
            j = i__ - igap;
/*<    20       continue >*/
L20:

/*<             if (j.lt.0) go to 30 >*/
            if (j < 0) {
                goto L30;
            }

/*<             if (x1(j).lt.x1(j+igap)) then >*/
            if (x1[j] < x1[j + igap]) {
/*<                temp = x1(j) >*/
                temp = x1[j];
/*<                x1(j) = x1(j+igap) >*/
                x1[j] = x1[j + igap];
/*<                x1(j+igap) = temp >*/
                x1[j + igap] = temp;
/*<                if (apply) then >*/
                if (*apply) {
/*<                   temp = x2(j) >*/
                    temp = x2[j];
/*<                   x2(j) = x2(j+igap) >*/
                    x2[j] = x2[j + igap];
/*<                   x2(j+igap) = temp >*/
                    x2[j + igap] = temp;
/*<                end if >*/
                }
/*<             else >*/
            } else {
/*<                go to 30 >*/
                goto L30;
/*<             endif >*/
            }
/*<             j = j-igap >*/
            j -= igap;
/*<             go to 20 >*/
            goto L20;
/*<    30    continue >*/
L30:
            ;
        }
/*<          igap = igap / 2 >*/
        igap /= 2;
/*<          go to 10 >*/
        goto L10;

/*<       else if (which .eq. 'SM') then >*/
    } else if (s_cmp(which, "SM", (ftnlen)2, (ftnlen)2) == 0) {

/*        X1 is sorted into decreasing order of magnitude. */

/*<    40    continue >*/
L40:
/*<          if (igap .eq. 0) go to 9000 >*/
        if (igap == 0) {
            goto L9000;
        }
/*<          do 60 i = igap, n-1 >*/
        i__1 = *n - 1;
        for (i__ = igap; i__ <= i__1; ++i__) {
/*<             j = i-igap >*/
            j = i__ - igap;
/*<    50       continue >*/
L50:

/*<             if (j.lt.0) go to 60 >*/
            if (j < 0) {
                goto L60;
            }

/*<             if (abs(x1(j)).lt.abs(x1(j+igap))) then >*/
            if ((d__1 = x1[j], abs(d__1)) < (d__2 = x1[j + igap], abs(d__2)))
                    {
/*<                temp = x1(j) >*/
                temp = x1[j];
/*<                x1(j) = x1(j+igap) >*/
                x1[j] = x1[j + igap];
/*<                x1(j+igap) = temp >*/
                x1[j + igap] = temp;
/*<                if (apply) then >*/
                if (*apply) {
/*<                   temp = x2(j) >*/
                    temp = x2[j];
/*<                   x2(j) = x2(j+igap) >*/
                    x2[j] = x2[j + igap];
/*<                   x2(j+igap) = temp >*/
                    x2[j + igap] = temp;
/*<                end if >*/
                }
/*<             else >*/
            } else {
/*<                go to 60 >*/
                goto L60;
/*<             endif >*/
            }
/*<             j = j-igap >*/
            j -= igap;
/*<             go to 50 >*/
            goto L50;
/*<    60    continue >*/
L60:
            ;
        }
/*<          igap = igap / 2 >*/
        igap /= 2;
/*<          go to 40 >*/
        goto L40;

/*<       else if (which .eq. 'LA') then >*/
    } else if (s_cmp(which, "LA", (ftnlen)2, (ftnlen)2) == 0) {

/*        X1 is sorted into increasing order of algebraic. */

/*<    70    continue >*/
L70:
/*<          if (igap .eq. 0) go to 9000 >*/
        if (igap == 0) {
            goto L9000;
        }
/*<          do 90 i = igap, n-1 >*/
        i__1 = *n - 1;
        for (i__ = igap; i__ <= i__1; ++i__) {
/*<             j = i-igap >*/
            j = i__ - igap;
/*<    80       continue >*/
L80:

/*<             if (j.lt.0) go to 90 >*/
            if (j < 0) {
                goto L90;
            }

/*<             if (x1(j).gt.x1(j+igap)) then >*/
            if (x1[j] > x1[j + igap]) {
/*<                temp = x1(j) >*/
                temp = x1[j];
/*<                x1(j) = x1(j+igap) >*/
                x1[j] = x1[j + igap];
/*<                x1(j+igap) = temp >*/
                x1[j + igap] = temp;
/*<                if (apply) then >*/
                if (*apply) {
/*<                   temp = x2(j) >*/
                    temp = x2[j];
/*<                   x2(j) = x2(j+igap) >*/
                    x2[j] = x2[j + igap];
/*<                   x2(j+igap) = temp >*/
                    x2[j + igap] = temp;
/*<                end if >*/
                }
/*<             else >*/
            } else {
/*<                go to 90 >*/
                goto L90;
/*<             endif >*/
            }
/*<             j = j-igap >*/
            j -= igap;
/*<             go to 80 >*/
            goto L80;
/*<    90    continue >*/
L90:
            ;
        }
/*<          igap = igap / 2 >*/
        igap /= 2;
/*<          go to 70 >*/
        goto L70;

/*<       else if (which .eq. 'LM') then >*/
    } else if (s_cmp(which, "LM", (ftnlen)2, (ftnlen)2) == 0) {

/*        X1 is sorted into increasing order of magnitude. */

/*<   100    continue >*/
L100:
/*<          if (igap .eq. 0) go to 9000 >*/
        if (igap == 0) {
            goto L9000;
        }
/*<          do 120 i = igap, n-1 >*/
        i__1 = *n - 1;
        for (i__ = igap; i__ <= i__1; ++i__) {
/*<             j = i-igap >*/
            j = i__ - igap;
/*<   110       continue >*/
L110:

/*<             if (j.lt.0) go to 120 >*/
            if (j < 0) {
                goto L120;
            }

/*<             if (abs(x1(j)).gt.abs(x1(j+igap))) then >*/
            if ((d__1 = x1[j], abs(d__1)) > (d__2 = x1[j + igap], abs(d__2)))
                    {
/*<                temp = x1(j) >*/
                temp = x1[j];
/*<                x1(j) = x1(j+igap) >*/
                x1[j] = x1[j + igap];
/*<                x1(j+igap) = temp >*/
                x1[j + igap] = temp;
/*<                if (apply) then >*/
                if (*apply) {
/*<                   temp = x2(j) >*/
                    temp = x2[j];
/*<                   x2(j) = x2(j+igap) >*/
                    x2[j] = x2[j + igap];
/*<                   x2(j+igap) = temp >*/
                    x2[j + igap] = temp;
/*<                end if >*/
                }
/*<             else >*/
            } else {
/*<                go to 120 >*/
                goto L120;
/*<             endif >*/
            }
/*<             j = j-igap >*/
            j -= igap;
/*<             go to 110 >*/
            goto L110;
/*<   120    continue >*/
L120:
            ;
        }
/*<          igap = igap / 2 >*/
        igap /= 2;
/*<          go to 100 >*/
        goto L100;
/*<       end if >*/
    }

/*<  9000 continue >*/
L9000:
/*<       return >*/
    return 0;

/*     %---------------% */
/*     | End of dsortr | */
/*     %---------------% */

/*<       end >*/
} /* dsortr_ */

#ifdef __cplusplus
        }
#endif
