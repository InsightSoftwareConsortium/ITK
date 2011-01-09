/* dgamit.f -- translated by f2c (version 20041007).
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
static integer c__1 = 1;
static integer c__2 = 2;
static doublereal c_b10 = 1.;

/* DECK DGAMIT */
doublereal dgamit_(doublereal *a, doublereal *x)
{
    /* Initialized data */

    // static logical first = TRUE_;

    /* System generated locals */
    doublereal ret_val, d__1, d__2;

    /* Builtin functions */
    double log(doublereal), sqrt(doublereal), d_sign(doublereal *, doublereal
            *), d_int(doublereal *), exp(doublereal);

    /* Local variables */
    doublereal h__, t, sga, alx;
    /* static */ doublereal bot;
    doublereal alng, aeps;
    extern doublereal dgamr_(doublereal *);
    doublereal ainta;
    /* static */ doublereal sqeps;
    extern doublereal d1mach_(integer *);
    doublereal algap1;
    extern doublereal d9lgic_(doublereal *, doublereal *, doublereal *),
            d9lgit_(doublereal *, doublereal *, doublereal *), d9gmit_(
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *), dlngam_(doublereal *);
    extern /* Subroutine */ int dlgams_(doublereal *, doublereal *,
            doublereal *);
    doublereal sgngam;
    /* static */ doublereal alneps;
//    extern /* Subroutine */ int xerclr_(void);
    extern /* Subroutine */ int xermsg_(const char *, const char *, const char *,
             integer *, integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  DGAMIT */
/* ***PURPOSE  Calculate Tricomi's form of the incomplete Gamma function. */
/* ***LIBRARY   SLATEC (FNLIB) */
/* ***CATEGORY  C7E */
/* ***TYPE      DOUBLE PRECISION (GAMIT-S, DGAMIT-D) */
/* ***KEYWORDS  COMPLEMENTARY INCOMPLETE GAMMA FUNCTION, FNLIB, */
/*             SPECIAL FUNCTIONS, TRICOMI */
/* ***AUTHOR  Fullerton, W., (LANL) */
/* ***DESCRIPTION */

/*   Evaluate Tricomi's incomplete Gamma function defined by */

/*   DGAMIT = X**(-A)/GAMMA(A) * integral from 0 to X of EXP(-T) * */
/*              T**(A-1.) */

/*   for A .GT. 0.0 and by analytic continuation for A .LE. 0.0. */
/*   GAMMA(X) is the complete gamma function of X. */

/*   DGAMIT is evaluated for arbitrary real values of A and for non- */
/*   negative values of X (even though DGAMIT is defined for X .LT. */
/*   0.0), except that for X = 0 and A .LE. 0.0, DGAMIT is infinite, */
/*   which is a fatal error. */

/*   The function and both arguments are DOUBLE PRECISION. */

/*   A slight deterioration of 2 or 3 digits accuracy will occur when */
/*   DGAMIT is very large or very small in absolute value, because log- */
/*   arithmic variables are used.  Also, if the parameter  A  is very */
/*   close to a negative integer (but not a negative integer), there is */
/*   a loss of accuracy, which is reported if the result is less than */
/*   half machine precision. */

/* ***REFERENCES  W. Gautschi, A computational procedure for incomplete */
/*                 gamma functions, ACM Transactions on Mathematical */
/*                 Software 5, 4 (December 1979), pp. 466-481. */
/*               W. Gautschi, Incomplete gamma functions, Algorithm 542, */
/*                 ACM Transactions on Mathematical Software 5, 4 */
/*                 (December 1979), pp. 482-489. */
/* ***ROUTINES CALLED  D1MACH, D9GMIT, D9LGIC, D9LGIT, DGAMR, DLGAMS, */
/*                    DLNGAM, XERCLR, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   770701  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   920528  DESCRIPTION and REFERENCES sections revised.  (WRB) */
/* ***END PROLOGUE  DGAMIT */
/* ***FIRST EXECUTABLE STATEMENT  DGAMIT */

    // d1mach has been made thread safe, so there is no need for the
    // statics in determining these values
//     if (first) {
//      alneps = -log(d1mach_(&c__3));
//      sqeps = sqrt(d1mach_(&c__4));
//      bot = log(d1mach_(&c__1));
//     }
//     first = FALSE_;
    alneps = -log(d1mach_(&c__3));
    sqeps = sqrt(d1mach_(&c__4));
    bot = log(d1mach_(&c__1));

    if (*x < 0.) {
        xermsg_("SLATEC", "DGAMIT", "X IS NEGATIVE", &c__2, &c__2, (ftnlen)6,
                (ftnlen)6, (ftnlen)13);
    }

    if (*x != 0.) {
        alx = log(*x);
    }
    sga = 1.;
    if (*a != 0.) {
        sga = d_sign(&c_b10, a);
    }
    d__1 = *a + sga * .5;
    ainta = d_int(&d__1);
    aeps = *a - ainta;

    if (*x > 0.) {
        goto L20;
    }
    ret_val = 0.;
    if (ainta > 0. || aeps != 0.) {
        d__1 = *a + 1.;
        ret_val = dgamr_(&d__1);
    }
    return ret_val;

L20:
    if (*x > 1.) {
        goto L30;
    }
    if (*a >= -.5 || aeps != 0.) {
        d__1 = *a + 1.;
        dlgams_(&d__1, &algap1, &sgngam);
    }
    ret_val = d9gmit_(a, x, &algap1, &sgngam, &alx);
    return ret_val;

L30:
    if (*a < *x) {
        goto L40;
    }
    d__2 = *a + 1.;
    d__1 = dlngam_(&d__2);
    t = d9lgit_(a, x, &d__1);
    if (t < bot) {
//      xerclr_();
    }
    ret_val = exp(t);
    return ret_val;

L40:
    alng = d9lgic_(a, x, &alx);

/* EVALUATE DGAMIT IN TERMS OF LOG (DGAMIC (A, X)) */

    h__ = 1.;
    if (aeps == 0. && ainta <= 0.) {
        goto L50;
    }

    d__1 = *a + 1.;
    dlgams_(&d__1, &algap1, &sgngam);
    t = log((abs(*a))) + alng - algap1;
    if (t > alneps) {
        goto L60;
    }

    if (t > -alneps) {
        h__ = 1. - sga * sgngam * exp(t);
    }
    if (abs(h__) > sqeps) {
        goto L50;
    }

//    xerclr_();
    xermsg_("SLATEC", "DGAMIT", "RESULT LT HALF PRECISION", &c__1, &c__1, (
            ftnlen)6, (ftnlen)6, (ftnlen)24);

L50:
    t = -(*a) * alx + log((abs(h__)));
    if (t < bot) {
//      xerclr_();
    }
    d__1 = exp(t);
    ret_val = d_sign(&d__1, &h__);
    return ret_val;

L60:
    t -= *a * alx;
    if (t < bot) {
//      xerclr_();
    }
    ret_val = -sga * sgngam * exp(t);
    return ret_val;

} /* dgamit_ */

