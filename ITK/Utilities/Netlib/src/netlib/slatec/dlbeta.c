/* dlbeta.f -- translated by f2c (version 20041007).
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

/* DECK DLBETA */
doublereal dlbeta_(doublereal *a, doublereal *b)
{
    /* Initialized data */

    static doublereal sq2pil = .91893853320467274178032973640562;

    /* System generated locals */
    doublereal ret_val, d__1;

    /* Builtin functions */
    double log(doublereal);

    /* Local variables */
    doublereal p, q, corr;
    extern doublereal d9lgmc_(doublereal *), dgamma_(doublereal *), dlngam_(
            doublereal *), dlnrel_(doublereal *);
    extern /* Subroutine */ int xermsg_(const char *, const char *, const char *, integer *,
            integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  DLBETA */
/* ***PURPOSE  Compute the natural logarithm of the complete Beta */
/*            function. */
/* ***LIBRARY   SLATEC (FNLIB) */
/* ***CATEGORY  C7B */
/* ***TYPE      DOUBLE PRECISION (ALBETA-S, DLBETA-D, CLBETA-C) */
/* ***KEYWORDS  FNLIB, LOGARITHM OF THE COMPLETE BETA FUNCTION, */
/*             SPECIAL FUNCTIONS */
/* ***AUTHOR  Fullerton, W., (LANL) */
/* ***DESCRIPTION */

/* DLBETA(A,B) calculates the double precision natural logarithm of */
/* the complete beta function for double precision arguments */
/* A and B. */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  D9LGMC, DGAMMA, DLNGAM, DLNREL, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   770701  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   900727  Added EXTERNAL statement.  (WRB) */
/* ***END PROLOGUE  DLBETA */
/* ***FIRST EXECUTABLE STATEMENT  DLBETA */
    p = min(*a,*b);
    q = max(*a,*b);

    if (p <= 0.) {
        xermsg_("SLATEC", "DLBETA", "BOTH ARGUMENTS MUST BE GT ZERO", &c__1, &
                c__2, (ftnlen)6, (ftnlen)6, (ftnlen)30);
    }

    if (p >= 10.) {
        goto L30;
    }
    if (q >= 10.) {
        goto L20;
    }

/* P AND Q ARE SMALL. */

    d__1 = p + q;
    ret_val = log(dgamma_(&p) * (dgamma_(&q) / dgamma_(&d__1)));
    return ret_val;

/* P IS SMALL, BUT Q IS BIG. */

L20:
    d__1 = p + q;
    corr = d9lgmc_(&q) - d9lgmc_(&d__1);
    d__1 = -p / (p + q);
    ret_val = dlngam_(&p) + corr + p - p * log(p + q) + (q - .5) * dlnrel_(&
            d__1);
    return ret_val;

/* P AND Q ARE BIG. */

L30:
    d__1 = p + q;
    corr = d9lgmc_(&p) + d9lgmc_(&q) - d9lgmc_(&d__1);
    d__1 = -p / (p + q);
    ret_val = log(q) * -.5 + sq2pil + corr + (p - .5) * log(p / (p + q)) + q *
             dlnrel_(&d__1);
    return ret_val;

} /* dlbeta_ */

