/* dgamlm.f -- translated by f2c (version 20041007).
   You must link the resulting object file with libf2c:
        on Microsoft Windows system, link with libf2c.lib;
        on Linux or Unix systems, link with .../path/to/libf2c.a -lm
        or, if you install libf2c.a in a standard place, with -lf2c -lm
        -- in that order, at the end of the command line, as in
                cc *.o -lf2c -lm
        Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

                http://www.netlib.org/f2c/libf2c.zip
*/

#define V3P_NETLIB_SRC
#include "v3p_netlib.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;

/* DECK DGAMLM */
/* Subroutine */ int dgamlm_(doublereal *xmin, doublereal *xmax)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double log(doublereal);

    /* Local variables */
    integer i__;
    doublereal xln, xold;
    extern doublereal d1mach_(integer *);
    doublereal alnbig, alnsml;
    extern /* Subroutine */ int xermsg_(const char *, const char *, const char *, integer *,
            integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  DGAMLM */
/* ***PURPOSE  Compute the minimum and maximum bounds for the argument in */
/*            the Gamma function. */
/* ***LIBRARY   SLATEC (FNLIB) */
/* ***CATEGORY  C7A, R2 */
/* ***TYPE      DOUBLE PRECISION (GAMLIM-S, DGAMLM-D) */
/* ***KEYWORDS  COMPLETE GAMMA FUNCTION, FNLIB, LIMITS, SPECIAL FUNCTIONS */
/* ***AUTHOR  Fullerton, W., (LANL) */
/* ***DESCRIPTION */

/* Calculate the minimum and maximum legal bounds for X in gamma(X). */
/* XMIN and XMAX are not the only bounds, but they are the only non- */
/* trivial ones to calculate. */

/*             Output Arguments -- */
/* XMIN   double precision minimum legal value of X in gamma(X).  Any */
/*        smaller value of X might result in underflow. */
/* XMAX   double precision maximum legal value of X in gamma(X).  Any */
/*        larger value of X might cause overflow. */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  D1MACH, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   770601  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/* ***END PROLOGUE  DGAMLM */
/* ***FIRST EXECUTABLE STATEMENT  DGAMLM */
    alnsml = log(d1mach_(&c__1));
    *xmin = -alnsml;
    for (i__ = 1; i__ <= 10; ++i__) {
        xold = *xmin;
        xln = log(*xmin);
        *xmin -= *xmin * ((*xmin + .5) * xln - *xmin - .2258 + alnsml) / (*
                xmin * xln + .5);
        if ((d__1 = *xmin - xold, abs(d__1)) < .005) {
            goto L20;
        }
/* L10: */
    }
    xermsg_("SLATEC", "DGAMLM", "UNABLE TO FIND XMIN", &c__1, &c__2, (ftnlen)
            6, (ftnlen)6, (ftnlen)19);

L20:
    *xmin = -(*xmin) + .01;

    alnbig = log(d1mach_(&c__2));
    *xmax = alnbig;
    for (i__ = 1; i__ <= 10; ++i__) {
        xold = *xmax;
        xln = log(*xmax);
        *xmax -= *xmax * ((*xmax - .5) * xln - *xmax + .9189 - alnbig) / (*
                xmax * xln - .5);
        if ((d__1 = *xmax - xold, abs(d__1)) < .005) {
            goto L40;
        }
/* L30: */
    }
    xermsg_("SLATEC", "DGAMLM", "UNABLE TO FIND XMAX", &c__2, &c__2, (ftnlen)
            6, (ftnlen)6, (ftnlen)19);

L40:
    *xmax += -.01;
/* Computing MAX */
    d__1 = *xmin, d__2 = -(*xmax) + 1.;
    *xmin = max(d__1,d__2);

    return 0;
} /* dgamlm_ */

