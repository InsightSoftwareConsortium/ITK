/* dlngam.f -- translated by f2c (version 20041007).
   You must link the resulting object file with libf2c:
        on Microsoft Windows system, link with libf2c.lib;
        on Linux or Unix systems, link with .../path/to/libf2c.a -lm
        or, if you install libf2c.a in a standard place, with -lf2c -lm
        -- in that order, at the end of the command line, as in
                cc *.o -lf2c -lm
        Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

                http://www.netlib.org/f2c/libf2c.zip
*/


/** This routine has been editted to be thread safe **/

#define V3P_NETLIB_SRC
#include "v3p_netlib.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__4 = 4;
static integer c__3 = 3;
static integer c__1 = 1;

/* DECK DLNGAM */
doublereal dlngam_(doublereal *x)
{
    /* Initialized data */

    static doublereal sq2pil = .91893853320467274178032973640562;
    static doublereal sqpi2l = .225791352644727432363097614947441;
    static doublereal pi = 3.1415926535897932384626433832795;
    // static logical first = TRUE_;

    /* System generated locals */
    doublereal ret_val = 0.0, d__1, d__2;

    /* Builtin functions */
    double log(doublereal), sqrt(doublereal), sin(doublereal), d_int(
            doublereal *);

    /* Local variables */
    doublereal y, temp;
    /* static */ doublereal xmax, dxrel;
    extern doublereal d1mach_(integer *), d9lgmc_(doublereal *), dgamma_(
            doublereal *);
    extern /* Subroutine */ int xermsg_(const char *, const char *, const char *, integer *,
            integer *, ftnlen, ftnlen, ftnlen);
    doublereal sinpiy;

/* ***BEGIN PROLOGUE  DLNGAM */
/* ***PURPOSE  Compute the logarithm of the absolute value of the Gamma */
/*            function. */
/* ***LIBRARY   SLATEC (FNLIB) */
/* ***CATEGORY  C7A */
/* ***TYPE      DOUBLE PRECISION (ALNGAM-S, DLNGAM-D, CLNGAM-C) */
/* ***KEYWORDS  ABSOLUTE VALUE, COMPLETE GAMMA FUNCTION, FNLIB, LOGARITHM, */
/*             SPECIAL FUNCTIONS */
/* ***AUTHOR  Fullerton, W., (LANL) */
/* ***DESCRIPTION */

/* DLNGAM(X) calculates the double precision logarithm of the */
/* absolute value of the Gamma function for double precision */
/* argument X. */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  D1MACH, D9LGMC, DGAMMA, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   770601  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   900727  Added EXTERNAL statement.  (WRB) */
/* ***END PROLOGUE  DLNGAM */
/* ***FIRST EXECUTABLE STATEMENT  DLNGAM */

    // d1mach has been made thread safe, so there is no need for the
    // statics in determining these values
//     if (first) {
//      temp = 1. / log(d1mach_(&c__2));
//      xmax = temp * d1mach_(&c__2);
//      dxrel = sqrt(d1mach_(&c__4));
//     }
//     first = FALSE_;
    temp = 1. / log(d1mach_(&c__2));
    xmax = temp * d1mach_(&c__2);
    dxrel = sqrt(d1mach_(&c__4));

    y = abs(*x);
    if (y > 10.) {
        goto L20;
    }

/* LOG (ABS (DGAMMA(X)) ) FOR ABS(X) .LE. 10.0 */

    ret_val = log((d__1 = dgamma_(x), abs(d__1)));
    return ret_val;

/* LOG ( ABS (DGAMMA(X)) ) FOR ABS(X) .GT. 10.0 */

L20:
    if (y > xmax) {
        xermsg_("SLATEC", "DLNGAM", "ABS(X) SO BIG DLNGAM OVERFLOWS", &c__2, &
                c__2, (ftnlen)6, (ftnlen)6, (ftnlen)30);
    }

    if (*x > 0.) {
        ret_val = sq2pil + (*x - .5) * log(*x) - *x + d9lgmc_(&y);
    }
    if (*x > 0.) {
        return ret_val;
    }

    sinpiy = (d__1 = sin(pi * y), abs(d__1));
    if (sinpiy == 0.) {
        xermsg_("SLATEC", "DLNGAM", "X IS A NEGATIVE INTEGER", &c__3, &c__2, (
                ftnlen)6, (ftnlen)6, (ftnlen)23);
    }

    d__2 = *x - .5;
    if ((d__1 = (*x - d_int(&d__2)) / *x, abs(d__1)) < dxrel) {
        xermsg_("SLATEC", "DLNGAM", "ANSWER LT HALF PRECISION BECAUSE X TOO "
                "NEAR NEGATIVE INTEGER", &c__1, &c__1, (ftnlen)6, (ftnlen)6, (
                ftnlen)60);
    }

    ret_val = sqpi2l + (*x - .5) * log(y) - *x - log(sinpiy) - d9lgmc_(&y);
    return ret_val;

} /* dlngam_ */

