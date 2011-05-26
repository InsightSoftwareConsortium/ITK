/* initds.f -- translated by f2c (version 20041007).
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

static integer c__2 = 2;
static integer c__1 = 1;

/* DECK INITDS */
integer initds_(doublereal *os, integer *nos, real *eta)
{
    /* System generated locals */
    integer ret_val, i__1;
    real r__1;

    /* Local variables */
    integer i__ = 0, ii;
    real err;
    extern /* Subroutine */ int xermsg_(const char *, const char *, const char *, integer *,
            integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  INITDS */
/* ***PURPOSE  Determine the number of terms needed in an orthogonal */
/*            polynomial series so that it meets a specified accuracy. */
/* ***LIBRARY   SLATEC (FNLIB) */
/* ***CATEGORY  C3A2 */
/* ***TYPE      DOUBLE PRECISION (INITS-S, INITDS-D) */
/* ***KEYWORDS  CHEBYSHEV, FNLIB, INITIALIZE, ORTHOGONAL POLYNOMIAL, */
/*             ORTHOGONAL SERIES, SPECIAL FUNCTIONS */
/* ***AUTHOR  Fullerton, W., (LANL) */
/* ***DESCRIPTION */

/*  Initialize the orthogonal series, represented by the array OS, so */
/*  that INITDS is the number of terms needed to insure the error is no */
/*  larger than ETA.  Ordinarily, ETA will be chosen to be one-tenth */
/*  machine precision. */

/*             Input Arguments -- */
/*   OS     double precision array of NOS coefficients in an orthogonal */
/*          series. */
/*   NOS    number of coefficients in OS. */
/*   ETA    single precision scalar containing requested accuracy of */
/*          series. */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   770601  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   891115  Modified error message.  (WRB) */
/*   891115  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/* ***END PROLOGUE  INITDS */
/* ***FIRST EXECUTABLE STATEMENT  INITDS */
    /* Parameter adjustments */
    --os;

    /* Function Body */
    if (*nos < 1) {
        xermsg_("SLATEC", "INITDS", "Number of coefficients is less than 1", &
                c__2, &c__1, (ftnlen)6, (ftnlen)6, (ftnlen)37);
    }

    err = 0.f;
    i__1 = *nos;
    for (ii = 1; ii <= i__1; ++ii) {
        i__ = *nos + 1 - ii;
        err += (r__1 = (real) os[i__], (real) dabs(r__1));
        if (err > *eta) {
            goto L20;
        }
/* L10: */
    }

L20:
    if (i__ == *nos) {
        xermsg_("SLATEC", "INITDS", "Chebyshev series too short for specifie"
                "d accuracy", &c__1, &c__1, (ftnlen)6, (ftnlen)6, (ftnlen)49);
    }
    ret_val = i__;

    return ret_val;
} /* initds_ */

