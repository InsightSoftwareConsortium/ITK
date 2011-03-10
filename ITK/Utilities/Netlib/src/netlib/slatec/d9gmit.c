/* d9gmit.f -- translated by f2c (version 20041007).
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
static doublereal c_b19 = 1.;

/* DECK D9GMIT */
doublereal d9gmit_(doublereal *a, doublereal *x, doublereal *algap1,
        doublereal *sgngam, doublereal *alx)
{
    /* Initialized data */

    // static logical first = TRUE_;

    /* System generated locals */
    integer i__1;
    doublereal ret_val, d__1;

    /* Builtin functions */
    double log(doublereal), d_sign(doublereal *, doublereal *), exp(
            doublereal);

    /* Local variables */
    integer k, m;
    doublereal s, t, ae;
    integer ma;
    doublereal fk, te;
    /* static */ doublereal bot, eps;
    doublereal alg2, algs = 0.0, aeps, sgng2;
    extern doublereal d1mach_(integer *), dlngam_(doublereal *);
    extern /* Subroutine */ int xermsg_(const char *, const char *, const char *, integer *,
            integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  D9GMIT */
/* ***SUBSIDIARY */
/* ***PURPOSE  Compute Tricomi's incomplete Gamma function for small */
/*            arguments. */
/* ***LIBRARY   SLATEC (FNLIB) */
/* ***CATEGORY  C7E */
/* ***TYPE      DOUBLE PRECISION (R9GMIT-S, D9GMIT-D) */
/* ***KEYWORDS  COMPLEMENTARY INCOMPLETE GAMMA FUNCTION, FNLIB, SMALL X, */
/*             SPECIAL FUNCTIONS, TRICOMI */
/* ***AUTHOR  Fullerton, W., (LANL) */
/* ***DESCRIPTION */

/* Compute Tricomi's incomplete gamma function for small X. */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  D1MACH, DLNGAM, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   770701  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890911  Removed unnecessary intrinsics.  (WRB) */
/*   890911  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   900720  Routine changed from user-callable to subsidiary.  (WRB) */
/* ***END PROLOGUE  D9GMIT */
/* ***FIRST EXECUTABLE STATEMENT  D9GMIT */

    // d1mach has been made thread safe, so there is no need for the
    // statics in determining eps and bot
//     if (first) {
//      eps = d1mach_(&c__3) * .5;
//      bot = log(d1mach_(&c__1));
//     }
//     first = FALSE_;
    eps = d1mach_(&c__3) * .5;
    bot = log(d1mach_(&c__1));

    if (*x <= 0.) {
        xermsg_("SLATEC", "D9GMIT", "X SHOULD BE GT 0", &c__1, &c__2, (ftnlen)
                6, (ftnlen)6, (ftnlen)16);
    }

    ma = (integer) (*a + .5);
    if (*a < 0.) {
        ma = (integer) (*a - .5);
    }
    aeps = *a - ma;

    ae = *a;
    if (*a < -.5) {
        ae = aeps;
    }

    t = 1.;
    te = ae;
    s = t;
    for (k = 1; k <= 200; ++k) {
        fk = (doublereal) k;
        te = -(*x) * te / fk;
        t = te / (ae + fk);
        s += t;
        if (abs(t) < eps * abs(s)) {
            goto L30;
        }
/* L20: */
    }
    xermsg_("SLATEC", "D9GMIT", "NO CONVERGENCE IN 200 TERMS OF TAYLOR-S SER"
            "IES", &c__2, &c__2, (ftnlen)6, (ftnlen)6, (ftnlen)46);

L30:
    if (*a >= -.5) {
        algs = -(*algap1) + log(s);
    }
    if (*a >= -.5) {
        goto L60;
    }

    d__1 = aeps + 1.;
    algs = -dlngam_(&d__1) + log(s);
    s = 1.;
    m = -ma - 1;
    if (m == 0) {
        goto L50;
    }
    t = 1.;
    i__1 = m;
    for (k = 1; k <= i__1; ++k) {
        t = *x * t / (aeps - (m + 1 - k));
        s += t;
        if (abs(t) < eps * abs(s)) {
            goto L50;
        }
/* L40: */
    }

L50:
    ret_val = 0.;
    algs = -ma * log(*x) + algs;
    if (s == 0. || aeps == 0.) {
        goto L60;
    }

    sgng2 = *sgngam * d_sign(&c_b19, &s);
    alg2 = -(*x) - *algap1 + log((abs(s)));

    if (alg2 > bot) {
        ret_val = sgng2 * exp(alg2);
    }
    if (algs > bot) {
        ret_val += exp(algs);
    }
    return ret_val;

L60:
    ret_val = exp(algs);
    return ret_val;

} /* d9gmit_ */

