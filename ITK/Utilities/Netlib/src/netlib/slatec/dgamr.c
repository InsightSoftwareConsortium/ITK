/* dgamr.f -- translated by f2c (version 20041007).
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

//static integer c__1 = 1;

/* DECK DGAMR */
doublereal dgamr_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double d_int(doublereal *), exp(doublereal);

    /* Local variables */
    doublereal alngx;
//    integer irold;
//    extern /* Subroutine */ int xgetf_(integer *);
    doublereal sgngx;
//    extern /* Subroutine */ int xsetf_(integer *);
    extern doublereal dgamma_(doublereal *);
    extern /* Subroutine */ int dlgams_(doublereal *, doublereal *,
            doublereal *), xerclr_(void);

/* ***BEGIN PROLOGUE  DGAMR */
/* ***PURPOSE  Compute the reciprocal of the Gamma function. */
/* ***LIBRARY   SLATEC (FNLIB) */
/* ***CATEGORY  C7A */
/* ***TYPE      DOUBLE PRECISION (GAMR-S, DGAMR-D, CGAMR-C) */
/* ***KEYWORDS  FNLIB, RECIPROCAL GAMMA FUNCTION, SPECIAL FUNCTIONS */
/* ***AUTHOR  Fullerton, W., (LANL) */
/* ***DESCRIPTION */

/* DGAMR(X) calculates the double precision reciprocal of the */
/* complete Gamma function for double precision argument X. */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  DGAMMA, DLGAMS, XERCLR, XGETF, XSETF */
/* ***REVISION HISTORY  (YYMMDD) */
/*   770701  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900727  Added EXTERNAL statement.  (WRB) */
/* ***END PROLOGUE  DGAMR */
/* ***FIRST EXECUTABLE STATEMENT  DGAMR */
    ret_val = 0.;
    if (*x <= 0. && d_int(x) == *x) {
        return ret_val;
    }

//    xgetf_(&irold);
//    xsetf_(&c__1);
    if (abs(*x) > 10.) {
        goto L10;
    }
    ret_val = 1. / dgamma_(x);
//    xerclr_();
//    xsetf_(&irold);
    return ret_val;

L10:
    dlgams_(x, &alngx, &sgngx);
//    xerclr_();
//    xsetf_(&irold);
    ret_val = sgngx * exp(-alngx);
    return ret_val;

} /* dgamr_ */

