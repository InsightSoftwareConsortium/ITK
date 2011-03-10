/* dcsevl.f -- translated by f2c (version 20041007).
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

static integer c__4 = 4;
static integer c__2 = 2;
static integer c__3 = 3;
static integer c__1 = 1;

/* DECK DCSEVL */
doublereal dcsevl_(doublereal *x, doublereal *cs, integer *n)
{
    /* Initialized data */

    // static logical first = TRUE_;

    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    integer i__;
    doublereal b0, b1, b2;
    integer ni;
    doublereal twox;
    /* static */ doublereal onepl;
    extern doublereal d1mach_(integer *);
    extern /* Subroutine */ int xermsg_(const char *, const char *, const char *, integer *,
            integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  DCSEVL */
/* ***PURPOSE  Evaluate a Chebyshev series. */
/* ***LIBRARY   SLATEC (FNLIB) */
/* ***CATEGORY  C3A2 */
/* ***TYPE      DOUBLE PRECISION (CSEVL-S, DCSEVL-D) */
/* ***KEYWORDS  CHEBYSHEV SERIES, FNLIB, SPECIAL FUNCTIONS */
/* ***AUTHOR  Fullerton, W., (LANL) */
/* ***DESCRIPTION */

/*  Evaluate the N-term Chebyshev series CS at X.  Adapted from */
/*  a method presented in the paper by Broucke referenced below. */

/*       Input Arguments -- */
/*  X    value at which the series is to be evaluated. */
/*  CS   array of N terms of a Chebyshev series.  In evaluating */
/*       CS, only half the first coefficient is summed. */
/*  N    number of terms in array CS. */

/* ***REFERENCES  R. Broucke, Ten subroutines for the manipulation of */
/*                 Chebyshev series, Algorithm 446, Communications of */
/*                 the A.C.M. 16, (1973) pp. 254-256. */
/*               L. Fox and I. B. Parker, Chebyshev Polynomials in */
/*                 Numerical Analysis, Oxford University Press, 1968, */
/*                 page 56. */
/* ***ROUTINES CALLED  D1MACH, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   770401  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   900329  Prologued revised extensively and code rewritten to allow */
/*           X to be slightly outside interval (-1,+1).  (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  DCSEVL */
    /* Parameter adjustments */
    --cs;

    /* Function Body */
/* ***FIRST EXECUTABLE STATEMENT  DCSEVL */

    // d1mach has been made thread safe, so there is no need for the
    // statics in determining these values
//     if (first) {
//      onepl = 1. + d1mach_(&c__4);
//     }
//     first = FALSE_;
    onepl = 1. + d1mach_(&c__4);

    if (*n < 1) {
        xermsg_("SLATEC", "DCSEVL", "NUMBER OF TERMS .LE. 0", &c__2, &c__2, (
                ftnlen)6, (ftnlen)6, (ftnlen)22);
    }
    if (*n > 1000) {
        xermsg_("SLATEC", "DCSEVL", "NUMBER OF TERMS .GT. 1000", &c__3, &c__2,
                 (ftnlen)6, (ftnlen)6, (ftnlen)25);
    }
    if (abs(*x) > onepl) {
        xermsg_("SLATEC", "DCSEVL", "X OUTSIDE THE INTERVAL (-1,+1)", &c__1, &
                c__1, (ftnlen)6, (ftnlen)6, (ftnlen)30);
    }

    b2 = 0.;
    b1 = 0.;
    b0 = 0.;
    twox = *x * 2.;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
        b2 = b1;
        b1 = b0;
        ni = *n + 1 - i__;
        b0 = twox * b1 - b2 + cs[ni];
/* L10: */
    }

    ret_val = (b0 - b2) * .5;

    return ret_val;
} /* dcsevl_ */

