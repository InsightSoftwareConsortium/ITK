/* dlnrel.f -- translated by f2c (version 20041007).
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
static integer c__43 = 43;
static integer c__4 = 4;
static integer c__2 = 2;
static integer c__1 = 1;

/* DECK DLNREL */
doublereal dlnrel_(doublereal *x)
{
    /* Initialized data */

    static doublereal alnrcs[43] = { 1.0378693562743769800686267719098,
            -.13364301504908918098766041553133,
            .01940824913552056335792619937475,
            -.0030107551127535777690376537776592,
            4.8694614797154850090456366509137e-4,
            -8.1054881893175356066809943008622e-5,
            1.3778847799559524782938251496059e-5,
            -2.3802210894358970251369992914935e-6,
            4.1640416213865183476391859901989e-7,
            -7.3595828378075994984266837031998e-8,
            1.3117611876241674949152294345011e-8,
            -2.3546709317742425136696092330175e-9,
            4.2522773276034997775638052962567e-10,
            -7.71908941348407968261081074933e-11,
            1.4075746481359069909215356472191e-11,
            -2.5769072058024680627537078627584e-12,
            4.7342406666294421849154395005938e-13,
            -8.7249012674742641745301263292675e-14,
            1.6124614902740551465739833119115e-14,
            -2.9875652015665773006710792416815e-15,
            5.5480701209082887983041321697279e-16,
            -1.0324619158271569595141333961932e-16,
            1.9250239203049851177878503244868e-17,
            -3.5955073465265150011189707844266e-18,
            6.7264542537876857892194574226773e-19,
            -1.2602624168735219252082425637546e-19,
            2.3644884408606210044916158955519e-20,
            -4.4419377050807936898878389179733e-21,
            8.3546594464034259016241293994666e-22,
            -1.5731559416479562574899253521066e-22,
            2.9653128740247422686154369706666e-23,
            -5.5949583481815947292156013226666e-24,
            1.0566354268835681048187284138666e-24,
            -1.9972483680670204548314999466666e-25,
            3.7782977818839361421049855999999e-26,
            -7.1531586889081740345038165333333e-27,
            1.3552488463674213646502024533333e-27,
            -2.5694673048487567430079829333333e-28,
            4.8747756066216949076459519999999e-29,
            -9.2542112530849715321132373333333e-30,
            1.757859784176023923326976e-30,
            -3.3410026677731010351377066666666e-31,
            6.3533936180236187354180266666666e-32 };
    // static logical first = TRUE_;

    /* System generated locals */
    real r__1;
    doublereal ret_val = 0., d__1;

    /* Builtin functions */
    double sqrt(doublereal), log(doublereal);

    /* Local variables */
    /* static */ doublereal xmin;
    extern doublereal d1mach_(integer *), dcsevl_(doublereal *, doublereal *,
            integer *);
    /* static */ integer nlnrel;
    extern integer initds_(doublereal *, integer *, real *);
    extern /* Subroutine */ int xermsg_(const char *, const char *, const char *, integer *,
            integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  DLNREL */
/* ***PURPOSE  Evaluate ln(1+X) accurate in the sense of relative error. */
/* ***LIBRARY   SLATEC (FNLIB) */
/* ***CATEGORY  C4B */
/* ***TYPE      DOUBLE PRECISION (ALNREL-S, DLNREL-D, CLNREL-C) */
/* ***KEYWORDS  ELEMENTARY FUNCTIONS, FNLIB, LOGARITHM */
/* ***AUTHOR  Fullerton, W., (LANL) */
/* ***DESCRIPTION */

/* DLNREL(X) calculates the double precision natural logarithm of */
/* (1.0+X) for double precision argument X.  This routine should */
/* be used when X is small and accurate to calculate the logarithm */
/* accurately (in the relative error sense) in the neighborhood */
/* of 1.0. */

/* Series for ALNR       on the interval -3.75000E-01 to  3.75000E-01 */
/*                                        with weighted error   6.35E-32 */
/*                                         log weighted error  31.20 */
/*                               significant figures required  30.93 */
/*                                    decimal places required  32.01 */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  D1MACH, DCSEVL, INITDS, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   770601  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/* ***END PROLOGUE  DLNREL */
/* ***FIRST EXECUTABLE STATEMENT  DLNREL */

    // d1mach has been made thread safe, so there is no need for the
    // statics in determining these values
//     if (first) {
//      r__1 = (real) d1mach_(&c__3) * .1f;
//      nlnrel = initds_(alnrcs, &c__43, &r__1);
//      xmin = sqrt(d1mach_(&c__4)) - 1.;
//     }
//     first = FALSE_;
    r__1 = (real) d1mach_(&c__3) * .1f;
    nlnrel = initds_(alnrcs, &c__43, &r__1);
    xmin = sqrt(d1mach_(&c__4)) - 1.;

    if (*x <= -1.) {
        xermsg_("SLATEC", "DLNREL", "X IS LE -1", &c__2, &c__2, (ftnlen)6, (
                ftnlen)6, (ftnlen)10);
    }
    if (*x < xmin) {
        xermsg_("SLATEC", "DLNREL", "ANSWER LT HALF PRECISION BECAUSE X TOO "
                "NEAR -1", &c__1, &c__1, (ftnlen)6, (ftnlen)6, (ftnlen)46);
    }

    if (abs(*x) <= .375) {
        d__1 = *x / .375;
        ret_val = *x * (1. - *x * dcsevl_(&d__1, alnrcs, &nlnrel));
    }

    if (abs(*x) > .375) {
        ret_val = log(*x + 1.);
    }

    return ret_val;
} /* dlnrel_ */

