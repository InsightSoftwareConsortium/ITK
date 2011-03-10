/* dbetai.f -- translated by f2c (version 20041007).
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

/* DECK DBETAI */
doublereal dbetai_(doublereal *x, doublereal *pin, doublereal *qin)
{
    /* Initialized data */

    // static logical first = TRUE_;

    /* System generated locals */
    integer i__1;
    doublereal ret_val, d__1;

    /* Builtin functions */
    double log(doublereal), d_int(doublereal *), exp(doublereal);

    /* Local variables */
    doublereal c__;
    integer i__, n;
    doublereal p, q, y, p1;
    integer ib;
    doublereal xb, xi, ps;
    /* static */ doublereal eps, sml;
    doublereal term;
    extern doublereal d1mach_(integer *), dlbeta_(doublereal *, doublereal *);
    /* static */ doublereal alneps, alnsml;
    doublereal finsum;
    extern /* Subroutine */ int xermsg_(const char *, const char *, const char *, integer *,
            integer *, ftnlen, ftnlen, ftnlen);

/* ***BEGIN PROLOGUE  DBETAI */
/* ***PURPOSE  Calculate the incomplete Beta function. */
/* ***LIBRARY   SLATEC (FNLIB) */
/* ***CATEGORY  C7F */
/* ***TYPE      DOUBLE PRECISION (BETAI-S, DBETAI-D) */
/* ***KEYWORDS  FNLIB, INCOMPLETE BETA FUNCTION, SPECIAL FUNCTIONS */
/* ***AUTHOR  Fullerton, W., (LANL) */
/* ***DESCRIPTION */

/*   DBETAI calculates the DOUBLE PRECISION incomplete beta function. */

/*   The incomplete beta function ratio is the probability that a */
/*   random variable from a beta distribution having parameters PIN and */
/*   QIN will be less than or equal to X. */

/*     -- Input Arguments -- All arguments are DOUBLE PRECISION. */
/*   X      upper limit of integration.  X must be in (0,1) inclusive. */
/*   PIN    first beta distribution parameter.  PIN must be .GT. 0.0. */
/*   QIN    second beta distribution parameter.  QIN must be .GT. 0.0. */

/* ***REFERENCES  Nancy E. Bosten and E. L. Battiste, Remark on Algorithm */
/*                 179, Communications of the ACM 17, 3 (March 1974), */
/*                 pp. 156. */
/* ***ROUTINES CALLED  D1MACH, DLBETA, XERMSG */
/* ***REVISION HISTORY  (YYMMDD) */
/*   770701  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890911  Removed unnecessary intrinsics.  (WRB) */
/*   890911  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) */
/*   920528  DESCRIPTION and REFERENCES sections revised.  (WRB) */
/* ***END PROLOGUE  DBETAI */
/* ***FIRST EXECUTABLE STATEMENT  DBETAI */

    // d1mach has been made thread safe, so there is no need for the
    // statics in determining these values
//     if (first) {
//      eps = d1mach_(&c__3);
//      alneps = log(eps);
//      sml = d1mach_(&c__1);
//      alnsml = log(sml);
//     }
//     first = FALSE_;
    eps = d1mach_(&c__3);
    alneps = log(eps);
    sml = d1mach_(&c__1);
    alnsml = log(sml);

    if (*x < 0. || *x > 1.) {
        xermsg_("SLATEC", "DBETAI", "X IS NOT IN THE RANGE (0,1)", &c__1, &
                c__2, (ftnlen)6, (ftnlen)6, (ftnlen)27);
    }
    if (*pin <= 0. || *qin <= 0.) {
        xermsg_("SLATEC", "DBETAI", "P AND/OR Q IS LE ZERO", &c__2, &c__2, (
                ftnlen)6, (ftnlen)6, (ftnlen)21);
    }

    y = *x;
    p = *pin;
    q = *qin;
    if (q <= p && *x < .8) {
        goto L20;
    }
    if (*x < .2) {
        goto L20;
    }
    y = 1. - y;
    p = *qin;
    q = *pin;

L20:
    if ((p + q) * y / (p + 1.) < eps) {
        goto L80;
    }

/* EVALUATE THE INFINITE SUM FIRST.  TERM WILL EQUAL */
/* Y**P/BETA(PS,P) * (1.-PS)-SUB-I * Y**I / FAC(I) . */

    ps = q - d_int(&q);
    if (ps == 0.) {
        ps = 1.;
    }
    xb = p * log(y) - dlbeta_(&ps, &p) - log(p);
    ret_val = 0.;
    if (xb < alnsml) {
        goto L40;
    }

    ret_val = exp(xb);
    term = ret_val * p;
    if (ps == 1.) {
        goto L40;
    }
/* Computing MAX */
    d__1 = alneps / log(y);
    n = (integer) max(d__1,4.);
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
        xi = (doublereal) i__;
        term = term * (xi - ps) * y / xi;
        ret_val += term / (p + xi);
/* L30: */
    }

/* NOW EVALUATE THE FINITE SUM, MAYBE. */

L40:
    if (q <= 1.) {
        goto L70;
    }

    xb = p * log(y) + q * log(1. - y) - dlbeta_(&p, &q) - log(q);
/* Computing MAX */
    d__1 = xb / alnsml;
    ib = (integer) max(d__1,0.);
    term = exp(xb - ib * alnsml);
    c__ = 1. / (1. - y);
    p1 = q * c__ / (p + q - 1.);

    finsum = 0.;
    n = (integer) q;
    if (q == (doublereal) n) {
        --n;
    }
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
        if (p1 <= 1. && term / eps <= finsum) {
            goto L60;
        }
        xi = (doublereal) i__;
        term = (q - xi + 1.) * c__ * term / (p + q - xi);

        if (term > 1.) {
            --ib;
        }
        if (term > 1.) {
            term *= sml;
        }

        if (ib == 0) {
            finsum += term;
        }
/* L50: */
    }

L60:
    ret_val += finsum;
L70:
    if (y != *x || p != *pin) {
        ret_val = 1. - ret_val;
    }
/* Computing MAX */
    d__1 = min(ret_val,1.);
    ret_val = max(d__1,0.);
    return ret_val;

L80:
    ret_val = 0.;
    xb = p * log((max(y,sml))) - log(p) - dlbeta_(&p, &q);
    if (xb > alnsml && y != 0.) {
        ret_val = exp(xb);
    }
    if (y != *x || p != *pin) {
        ret_val = 1. - ret_val;
    }

    return ret_val;
} /* dbetai_ */

