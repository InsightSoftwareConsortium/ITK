/* d9lgit.f -- translated by f2c (version 20041007).
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
static integer c__4 = 4;
static integer c__2 = 2;
static integer c__1 = 1;

/* DECK D9LGIT */
doublereal d9lgit_(doublereal *a, doublereal *x, doublereal *algap1)
{
    /* Initialized data */

    // static logical first = TRUE_;

    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double sqrt(doublereal), log(doublereal);

    /* Local variables */
    integer k;
    doublereal p, r__, s, t, fk, ax, a1x;
    /* static */ doublereal eps;
    doublereal hstar;
    /* static */ doublereal sqeps;
    extern doublereal d1mach_(integer *);
    extern /* Subroutine */ int xermsg_(const char *, const char *, const char *, integer *,
            integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  D9LGIT */
/* ***SUBSIDIARY */
/* ***PURPOSE  Compute the logarithm of Tricomi's incomplete Gamma */
/*            function with Perron's continued fraction for large X and */
/*            A .GE. X. */
/* ***LIBRARY   SLATEC (FNLIB) */
/* ***CATEGORY  C7E */
/* ***TYPE      DOUBLE PRECISION (R9LGIT-S, D9LGIT-D) */
/* ***KEYWORDS  FNLIB, INCOMPLETE GAMMA FUNCTION, LOGARITHM, */
/*             PERRON'S CONTINUED FRACTION, SPECIAL FUNCTIONS, TRICOMI */
/* ***AUTHOR  Fullerton, W., (LANL) */
/* ***DESCRIPTION */

/* Compute the log of Tricomi's incomplete gamma function with Perron's */
/* continued fraction for large X and for A .GE. X. */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  D1MACH, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   770701  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   900720  Routine changed from user-callable to subsidiary.  (WRB) */
/* ***END PROLOGUE  D9LGIT */
/* ***FIRST EXECUTABLE STATEMENT  D9LGIT */

    // d1mach has been made thread safe, so there is no need for the
    // statics in determining eps and sqeps
//     if (first) {
//      eps = d1mach_(&c__3) * .5;
//      sqeps = sqrt(d1mach_(&c__4));
//     }
//     first = FALSE_;
    eps = d1mach_(&c__3) * .5;
    sqeps = sqrt(d1mach_(&c__4));

    if (*x <= 0. || *a < *x) {
        xermsg_("SLATEC", "D9LGIT", "X SHOULD BE GT 0.0 AND LE A", &c__2, &
                c__2, (ftnlen)6, (ftnlen)6, (ftnlen)27);
    }

    ax = *a + *x;
    a1x = ax + 1.;
    r__ = 0.;
    p = 1.;
    s = p;
    for (k = 1; k <= 200; ++k) {
        fk = (doublereal) k;
        t = (*a + fk) * *x * (r__ + 1.);
        r__ = t / ((ax + fk) * (a1x + fk) - t);
        p = r__ * p;
        s += p;
        if (abs(p) < eps * s) {
            goto L30;
        }
/* L20: */
    }
    xermsg_("SLATEC", "D9LGIT", "NO CONVERGENCE IN 200 TERMS OF CONTINUED FR"
            "ACTION", &c__3, &c__2, (ftnlen)6, (ftnlen)6, (ftnlen)49);

L30:
    hstar = 1. - *x * s / a1x;
    if (hstar < sqeps) {
        xermsg_("SLATEC", "D9LGIT", "RESULT LESS THAN HALF PRECISION", &c__1,
                &c__1, (ftnlen)6, (ftnlen)6, (ftnlen)31);
    }

    ret_val = -(*x) - *algap1 - log(hstar);
    return ret_val;

} /* d9lgit_ */

