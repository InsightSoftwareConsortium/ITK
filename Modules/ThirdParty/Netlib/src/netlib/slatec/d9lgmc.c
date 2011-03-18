/* d9lgmc.f -- translated by f2c (version 20041007).
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

static integer c__15 = 15;
static integer c__3 = 3;
static integer c__1 = 1;
static integer c__2 = 2;

/* DECK D9LGMC */
doublereal d9lgmc_(doublereal *x)
{
    /* Initialized data */

    static doublereal algmcs[15] = { .1666389480451863247205729650822,
            -1.384948176067563840732986059135e-5,
            9.810825646924729426157171547487e-9,
            -1.809129475572494194263306266719e-11,
            6.221098041892605227126015543416e-14,
            -3.399615005417721944303330599666e-16,
            2.683181998482698748957538846666e-18,
            -2.868042435334643284144622399999e-20,
            3.962837061046434803679306666666e-22,
            -6.831888753985766870111999999999e-24,
            1.429227355942498147573333333333e-25,
            -3.547598158101070547199999999999e-27,1.025680058010470912e-28,
            -3.401102254316748799999999999999e-30,
            1.276642195630062933333333333333e-31 };
    // static logical first = TRUE_;

    /* System generated locals */
    real r__1;
    doublereal ret_val, d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), log(doublereal), exp(doublereal);

    /* Local variables */
    /* static */ doublereal xbig, xmax;
    /* static */ integer nalgm;
    extern doublereal d1mach_(integer *), dcsevl_(doublereal *, doublereal *,
            integer *);
    extern integer initds_(doublereal *, integer *, real *);
    extern /* Subroutine */ int xermsg_(const char *, const char *, const char *, integer *,
            integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  D9LGMC */
/* ***SUBSIDIARY */
/* ***PURPOSE  Compute the log Gamma correction factor so that */
/*            LOG(DGAMMA(X)) = LOG(SQRT(2*PI)) + (X-5.)*LOG(X) - X */
/*            + D9LGMC(X). */
/* ***LIBRARY   SLATEC (FNLIB) */
/* ***CATEGORY  C7E */
/* ***TYPE      DOUBLE PRECISION (R9LGMC-S, D9LGMC-D, C9LGMC-C) */
/* ***KEYWORDS  COMPLETE GAMMA FUNCTION, CORRECTION TERM, FNLIB, */
/*             LOG GAMMA, LOGARITHM, SPECIAL FUNCTIONS */
/* ***AUTHOR  Fullerton, W., (LANL) */
/* ***DESCRIPTION */

/* Compute the log gamma correction factor for X .GE. 10. so that */
/* LOG (DGAMMA(X)) = LOG(SQRT(2*PI)) + (X-.5)*LOG(X) - X + D9lGMC(X) */

/* Series for ALGM       on the interval  0.          to  1.00000E-02 */
/*                                        with weighted error   1.28E-31 */
/*                                         log weighted error  30.89 */
/*                               significant figures required  29.81 */
/*                                    decimal places required  31.48 */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  D1MACH, DCSEVL, INITDS, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   770601  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   900720  Routine changed from user-callable to subsidiary.  (WRB) */
/* ***END PROLOGUE  D9LGMC */
/* ***FIRST EXECUTABLE STATEMENT  D9LGMC */

    // d1mach has been made thread safe, so there is no need for the
    // statics in determining these values
//     if (first) {
//      r__1 = (real) d1mach_(&c__3);
//      nalgm = initds_(algmcs, &c__15, &r__1);
//      xbig = 1. / sqrt(d1mach_(&c__3));
// /* Computing MIN */
//      d__1 = log(d1mach_(&c__2) / 12.), d__2 = -log(d1mach_(&c__1) * 12.);
//      xmax = exp((min(d__1,d__2)));
//     }
//     first = FALSE_;
    r__1 = (real) d1mach_(&c__3);
    nalgm = initds_(algmcs, &c__15, &r__1);
    xbig = 1. / sqrt(d1mach_(&c__3));
    /* Computing MIN */
    d__1 = log(d1mach_(&c__2) / 12.), d__2 = -log(d1mach_(&c__1) * 12.);
    xmax = exp((min(d__1,d__2)));

    if (*x < 10.) {
        xermsg_("SLATEC", "D9LGMC", "X MUST BE GE 10", &c__1, &c__2, (ftnlen)
                6, (ftnlen)6, (ftnlen)15);
    }
    if (*x >= xmax) {
        goto L20;
    }

    ret_val = 1. / (*x * 12.);
    if (*x < xbig) {
/* Computing 2nd power */
        d__2 = 10. / *x;
        d__1 = d__2 * d__2 * 2. - 1.;
        ret_val = dcsevl_(&d__1, algmcs, &nalgm) / *x;
    }
    return ret_val;

L20:
    ret_val = 0.;
    xermsg_("SLATEC", "D9LGMC", "X SO BIG D9LGMC UNDERFLOWS", &c__2, &c__1, (
            ftnlen)6, (ftnlen)6, (ftnlen)26);
    return ret_val;

} /* d9lgmc_ */

