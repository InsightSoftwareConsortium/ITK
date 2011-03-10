/* d9lgic.f -- translated by f2c (version 20041007).
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

static integer c__3 = 3;
static integer c__1 = 1;
static integer c__2 = 2;

/* DECK D9LGIC */
doublereal d9lgic_(doublereal *a, doublereal *x, doublereal *alx)
{
    /* Initialized data */

    /* static */ doublereal eps = 0.;

    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double log(doublereal);

    /* Local variables */
    integer k;
    doublereal p, r__, s, t, fk, xma, xpa;
    extern doublereal d1mach_(integer *);
    extern /* Subroutine */ int xermsg_(const char *, const char *, const char *, integer *,
            integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  D9LGIC */
/* ***SUBSIDIARY */
/* ***PURPOSE  Compute the log complementary incomplete Gamma function */
/*            for large X and for A .LE. X. */
/* ***LIBRARY   SLATEC (FNLIB) */
/* ***CATEGORY  C7E */
/* ***TYPE      DOUBLE PRECISION (R9LGIC-S, D9LGIC-D) */
/* ***KEYWORDS  COMPLEMENTARY INCOMPLETE GAMMA FUNCTION, FNLIB, LARGE X, */
/*             LOGARITHM, SPECIAL FUNCTIONS */
/* ***AUTHOR  Fullerton, W., (LANL) */
/* ***DESCRIPTION */

/* Compute the log complementary incomplete gamma function for large X */
/* and for A .LE. X. */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  D1MACH, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   770701  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   900720  Routine changed from user-callable to subsidiary.  (WRB) */
/* ***END PROLOGUE  D9LGIC */
/* ***FIRST EXECUTABLE STATEMENT  D9LGIC */

    // d1mach has been made thread safe, so there is no need for the
    // statics in determining eps
//     if (eps == 0.) {
//      eps = .5 * d1mach_(&c__3);
//     }
    eps = .5 * d1mach_(&c__3);

    xpa = *x + 1. - *a;
    xma = *x - 1. - *a;

    r__ = 0.;
    p = 1.;
    s = p;
    for (k = 1; k <= 300; ++k) {
        fk = (doublereal) k;
        t = fk * (*a - fk) * (r__ + 1.);
        r__ = -t / ((xma + fk * 2.) * (xpa + fk * 2.) + t);
        p = r__ * p;
        s += p;
        if (abs(p) < eps * s) {
            goto L20;
        }
/* L10: */
    }
    xermsg_("SLATEC", "D9LGIC", "NO CONVERGENCE IN 300 TERMS OF CONTINUED FR"
            "ACTION", &c__1, &c__2, (ftnlen)6, (ftnlen)6, (ftnlen)49);

L20:
    ret_val = *a * *alx - *x + log(s / xpa);

    return ret_val;
} /* d9lgic_ */

