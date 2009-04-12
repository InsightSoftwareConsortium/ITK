/* dlgams.f -- translated by f2c (version 20041007).
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

static doublereal c_b2 = 2.;

/* DECK DLGAMS */
/* Subroutine */ int dlgams_(doublereal *x, doublereal *dlgam, doublereal *
        sgngam)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double d_int(doublereal *), d_mod(doublereal *, doublereal *);

    /* Local variables */
    integer int__;
    extern doublereal dlngam_(doublereal *);

/* ***BEGIN PROLOGUE  DLGAMS */
/* ***PURPOSE  Compute the logarithm of the absolute value of the Gamma */
/*            function. */
/* ***LIBRARY   SLATEC (FNLIB) */
/* ***CATEGORY  C7A */
/* ***TYPE      DOUBLE PRECISION (ALGAMS-S, DLGAMS-D) */
/* ***KEYWORDS  ABSOLUTE VALUE OF THE LOGARITHM OF THE GAMMA FUNCTION, */
/*             FNLIB, SPECIAL FUNCTIONS */
/* ***AUTHOR  Fullerton, W., (LANL) */
/* ***DESCRIPTION */

/* DLGAMS(X,DLGAM,SGNGAM) calculates the double precision natural */
/* logarithm of the absolute value of the Gamma function for */
/* double precision argument X and stores the result in double */
/* precision argument DLGAM. */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  DLNGAM */
/* ***REVISION HISTORY  (YYMMDD) */
/*   770701  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/* ***END PROLOGUE  DLGAMS */
/* ***FIRST EXECUTABLE STATEMENT  DLGAMS */
    *dlgam = dlngam_(x);
    *sgngam = 1.;
    if (*x > 0.) {
        return 0;
    }

    d__1 = -d_int(x);
    int__ = (integer) (d_mod(&d__1, &c_b2) + .1);
    if (int__ == 0) {
        *sgngam = -1.;
    }

    return 0;
} /* dlgams_ */

