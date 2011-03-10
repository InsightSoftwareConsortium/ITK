/* dgami.f -- translated by f2c (version 20041007).
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

/* DECK DGAMI */
doublereal dgami_(doublereal *a, doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double log(doublereal), exp(doublereal);

    /* Local variables */
    extern doublereal dlngam_(doublereal *), dgamit_(doublereal *, doublereal
            *);
    doublereal factor;
    extern /* Subroutine */ int xermsg_(const char *, const char *, const char *, integer *,
            integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  DGAMI */
/* ***PURPOSE  Evaluate the incomplete Gamma function. */
/* ***LIBRARY   SLATEC (FNLIB) */
/* ***CATEGORY  C7E */
/* ***TYPE      DOUBLE PRECISION (GAMI-S, DGAMI-D) */
/* ***KEYWORDS  FNLIB, INCOMPLETE GAMMA FUNCTION, SPECIAL FUNCTIONS */
/* ***AUTHOR  Fullerton, W., (LANL) */
/* ***DESCRIPTION */

/* Evaluate the incomplete gamma function defined by */

/* DGAMI = integral from T = 0 to X of EXP(-T) * T**(A-1.0) . */

/* DGAMI is evaluated for positive values of A and non-negative values */
/* of X.  A slight deterioration of 2 or 3 digits accuracy will occur */
/* when DGAMI is very large or very small, because logarithmic variables */
/* are used.  The function and both arguments are double precision. */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  DGAMIT, DLNGAM, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   770701  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/* ***END PROLOGUE  DGAMI */
/* ***FIRST EXECUTABLE STATEMENT  DGAMI */
    if (*a <= 0.) {
        xermsg_("SLATEC", "DGAMI", "A MUST BE GT ZERO", &c__1, &c__2, (ftnlen)
                6, (ftnlen)5, (ftnlen)17);
    }
    if (*x < 0.) {
        xermsg_("SLATEC", "DGAMI", "X MUST BE GE ZERO", &c__2, &c__2, (ftnlen)
                6, (ftnlen)5, (ftnlen)17);
    }

    ret_val = 0.;
    if (*x == 0.) {
        return ret_val;
    }

/* THE ONLY ERROR POSSIBLE IN THE EXPRESSION BELOW IS A FATAL OVERFLOW. */
    factor = exp(dlngam_(a) + *a * log(*x));

    ret_val = factor * dgamit_(a, x);

    return ret_val;
} /* dgami_ */

