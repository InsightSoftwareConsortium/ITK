/* camsun.f -- translated by f2c (version 19950102).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"
#include <stdio.h>

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int chscdf_(x, nu, cdf)
real *x;
integer *nu;
real *cdf;
{
    /* Initialized data */

    static integer nucut = 1000;
    static doublereal b43 = 17.;
    static doublereal pi = 3.14159265358979;
    static doublereal dpower = .33333333333333;
    static doublereal b11 = .33333333333333;
    static doublereal b21 = -.02777777777778;
    static doublereal b31 = -6.1728395061e-4;
    static doublereal b32 = -13.;
    static doublereal b41 = 1.8004115226e-4;
    static doublereal b42 = 6.;

    /* Error strings */
    static char fmt_15[] = "(***** FATAL ERROR--THE SECOND INPUT ARGUMENT TO THE CHSCDF SUBROUTINE IS NON-POSITIVE *****)";
    static char fmt_47[] = "(***** THE VALUE OF THE ARGUMENT IS %d *****\002)";
    static char fmt_4[] = "(***** NON-FATAL DIAGNOSTIC--THE FIRST  INPUT ARGUMENT TO THE CHSCDF SUBROUTINE IS NEGATIVE *****)";
    static char fmt_46[] = "(***** THE VALUE OF THE ARGUMENT IS %f *****\002)";
    static char fmt_99[] = "(*****INTERNAL ERROR IN CHSCDF SUBROUTINE -- IMPOSSIBLE BRANCH CONDITION AT BRANCH POINT %d)";

    /* System generated locals */
    integer i__1;
    real r__1, r__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(), exp(), pow(), log();

    /* Local variables */
    static real cdfn, danu;
    static integer imin, imax;
    static doublereal term, term0, term1, term2, term3, term4;
    static integer i;
    static doublereal dcdfn, dfact;
    static real amean, u, z;
    static integer ibran;
    static real spchi;
    static doublereal d1, d2, d3, ai;
    static real sd;
    static doublereal dw, dx;
    static integer ievodd;
    extern /* Subroutine */ int norcdf_();
    static doublereal chi;
    static real anu;
    static doublereal dnu;
    static integer ipr;
    static doublereal sum;

/*     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION */
/*              FUNCTION VALUE FOR THE CHI-SQUARED DISTRIBUTION */
/*              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU. */
/*              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X. */
/*              THE PROBABILITY DENSITY FUNCTION IS GIVEN */
/*              IN THE REFERENCES BELOW. */
/*     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT */
/*                                WHICH THE CUMULATIVE DISTRIBUTION */
/*                                FUNCTION IS TO BE EVALUATED. */
/*                                X SHOULD BE NON-NEGATIVE. */
/*                     --NU     = THE INTEGER NUMBER OF DEGREES */
/*                                OF FREEDOM. */
/*                                NU SHOULD BE POSITIVE. */
/*     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE */
/*                                DISTRIBUTION FUNCTION VALUE. */
/*     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION */
/*             FUNCTION VALUE CDF FOR THE CHI-SQUARED DISTRIBUTION */
/*             WITH DEGREES OF FREEDOM PARAMETER = NU. */
/*     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. */
/*     RESTRICTIONS--X SHOULD BE NON-NEGATIVE. */
/*                 --NU SHOULD BE A POSITIVE INTEGER VARIABLE. */
/*     OTHER DATAPAC   SUBROUTINES NEEDED--NORCDF. */
/*     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DEXP. */
/*     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION. */
/*     LANGUAGE--ANSI FORTRAN. */
/*     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS */
/*                 SERIES 55, 1964, PAGE 941, FORMULAE 26.4.4 AND 26.4.5. 
*/
/*               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE */
/*                 DISTRIBUTIONS--1, 1970, PAGE 176, */
/*                 FORMULA 28, AND PAGE 180, FORMULA 33.1. */
/*               --OWEN, HANDBOOK OF STATISTICAL TABLES, */
/*                 1962, PAGES 50-55. */
/*               --PEARSON AND HARTLEY, BIOMETRIKA TABLES */
/*                 FOR STATISTICIANS, VOLUME 1, 1954, */
/*                 PAGES 122-131. */
/*     WRITTEN BY--JAMES J. FILLIBEN */
/*                 STATISTICAL ENGINEERING LABORATORY (205.03) */
/*                 NATIONAL BUREAU OF STANDARDS */
/*                 WASHINGTON, D. C. 20234 */
/*                 PHONE:  301-921-2315 */
/*     ORIGINAL VERSION--JUNE      1972. */
/*     UPDATED         --MAY       1974. */
/*     UPDATED         --SEPTEMBER 1975. */
/*     UPDATED         --NOVEMBER  1975. */
/*     UPDATED         --OCTOBER   1976. */

/* --------------------------------------------------------------------- 
*/


    ipr = 6;

/*     CHECK THE INPUT ARGUMENTS FOR ERRORS */

    if (*nu <= 0) {
	goto L50;
    }
    if (*x < (float)0.) {
	goto L55;
    }
    goto L90;
L50:

    fprintf(stderr,fmt_15);
    fprintf(stderr,fmt_47,*nu);

    *cdf = (float)0.;
    return 0;
L55:
    fprintf(stderr,fmt_4);
    fprintf(stderr,fmt_46,*x);

    *cdf = (float)0.;
    return 0;
L90:

/* -----START POINT----------------------------------------------------- 
*/

    dx = *x;
    anu = (real) (*nu);
    dnu = (doublereal) (*nu);

/*     IF X IS NON-POSITIVE, SET CDF = 0.0 AND RETURN. */
/*     IF NU IS SMALLER THAN 10 AND X IS MORE THAN 200 */
/*     STANDARD DEVIATIONS BELOW THE MEAN, */
/*     SET CDF = 0.0 AND RETURN. */
/*     IF NU IS 10 OR LARGER AND X IS MORE THAN 100 */
/*     STANDARD DEVIATIONS BELOW THE MEAN, */
/*     SET CDF = 0.0 AND RETURN. */
/*     IF NU IS SMALLER THAN 10 AND X IS MORE THAN 200 */
/*     STANDARD DEVIATIONS ABOVE THE MEAN, */
/*     SET CDF = 1.0 AND RETURN. */
/*     IF NU IS 10 OR LARGER AND X IS MORE THAN 100 */
/*     STANDARD DEVIATIONS ABOVE THE MEAN, */
/*     SET CDF = 1.0 AND RETURN. */

    if (*x <= (float)0.) {
	goto L105;
    }
    amean = anu;
    sd = sqrt(anu * (float)2.);
    z = (*x - amean) / sd;
    if (*nu < 10 && z < (float)-200.) {
	goto L105;
    }
    if (*nu >= 10 && z < (float)-100.) {
	goto L105;
    }
    if (*nu < 10 && z > (float)200.) {
	goto L107;
    }
    if (*nu >= 10 && z > (float)100.) {
	goto L107;
    }
    goto L109;
L105:
    *cdf = (float)0.;
    return 0;
L107:
    *cdf = (float)1.;
    return 0;
L109:

/*     DISTINGUISH BETWEEN 3 SEPARATE REGIONS */
/*     OF THE (X,NU) SPACE. */
/*     BRANCH TO THE PROPER COMPUTATIONAL METHOD */
/*     DEPENDING ON THE REGION. */
/*     NUCUT HAS THE VALUE 1000. */

    if (*nu < nucut) {
	goto L1000;
    }
    if (*nu >= nucut && *x <= anu) {
	goto L2000;
    }
    if (*nu >= nucut && *x > anu) {
	goto L3000;
    }
    ibran = 1;
    fprintf(stderr,fmt_99,ibran);
    return 0;

/*     TREAT THE SMALL AND MODERATE DEGREES OF FREEDOM CASE */
/*     (THAT IS, WHEN NU IS SMALLER THAN 1000). */
/*     METHOD UTILIZED--EXACT FINITE SUM */
/*     (SEE AMS 55, PAGE 941, FORMULAE 26.4.4 AND 26.4.5). */

L1000:
    chi = sqrt(dx);
    ievodd = *nu - (*nu / 2 << 1);
    if (ievodd == 0) {
	goto L120;
    }

    sum = 0.;
    term = (float)1. / chi;
    imin = 1;
    imax = *nu - 1;
    goto L130;

L120:
    sum = 1.;
    term = 1.;
    imin = 2;
    imax = *nu - 2;

L130:
    if (imin > imax) {
	goto L160;
    }
    i__1 = imax;
    for (i = imin; i <= i__1; i += 2) {
	ai = (doublereal) i;
	term *= dx / ai;
	sum += term;
/* L100: */
    }
L160:

    sum *= exp(-dx / 2.);
    if (ievodd == 0) {
	goto L170;
    }
    sum = sqrt(2. / pi) * sum;
    spchi = chi;
    norcdf_(&spchi, &cdfn);
    dcdfn = cdfn;
    sum += (1. - dcdfn) * 2.;
L170:
    *cdf = 1. - sum;
    return 0;

/*     TREAT THE CASE WHEN NU IS LARGE */
/*     (THAT IS, WHEN NU IS EQUAL TO OR GREATER THAN 1000) */
/*     AND X IS LESS THAN OR EQUAL TO NU. */
/*     METHOD UTILIZED--WILSON-HILFERTY APPROXIMATION */
/*     (SEE JOHNSON AND KOTZ, VOLUME 1, PAGE 176, FORMULA 28). */

L2000:
    dfact = dnu * 4.5;
    d__1 = dx / dnu;
    u = (pow(d__1, dpower) - 1. + 1. / dfact) * sqrt(dfact);
    norcdf_(&u, &cdfn);
    *cdf = cdfn;
    return 0;

/*     TREAT THE CASE WHEN NU IS LARGE */
/*     (THAT IS, WHEN NU IS EQUAL TO OR GREATER THAN 1000) */
/*     AND X IS LARGER THAN NU. */
/*     METHOD UTILIZED--HILL'S ASYMPTOTIC EXPANSION */
/*     (SEE JOHNSON AND KOTZ, VOLUME 1, PAGE 180, FORMULA 33.1). */

L3000:
    dw = sqrt(dx - dnu - dnu * log(dx / dnu));
    danu = sqrt(2. / dnu);
    d1 = dw;
/* Computing 2nd power */
    d__1 = dw;
    d2 = d__1 * d__1;
/* Computing 3rd power */
    d__1 = dw, d__2 = d__1;
    d3 = d__2 * (d__1 * d__1);
    term0 = dw;
    term1 = b11 * danu;
/* Computing 2nd power */
    r__1 = danu;
    term2 = b21 * d1 * (r__1 * r__1);
/* Computing 3rd power */
    r__1 = danu, r__2 = r__1;
    term3 = b31 * (d2 + b32) * (r__2 * (r__1 * r__1));
/* Computing 4th power */
    r__1 = danu, r__1 *= r__1;
    term4 = b41 * (b42 * d3 + b43 * d1) * (r__1 * r__1);
    u = term0 + term1 + term2 + term3 + term4;
    norcdf_(&u, &cdfn);
    *cdf = cdfn;
    return 0;

} /* chscdf_ */

/* NORCDF */
/* Subroutine */ int norcdf_(x, cdf)
real *x, *cdf;
{
    /* Initialized data */

    static real b1 = (float).31938153;
    static real b2 = (float)-.356563782;
    static real b3 = (float)1.781477937;
    static real b4 = (float)-1.821255978;
    static real b5 = (float)1.330274429;
    static real p = (float).2316419;

    /* System generated locals */
    real r__1, r__2, r__3, r__4, r__5, r__6;

    /* Builtin functions */
    double exp();

    /* Local variables */
    static real t, z;
    static integer ipr;


/*     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION */
/*              FUNCTION VALUE FOR THE NORMAL (GAUSSIAN) */
/*              DISTRIBUTION WITH MEAN = 0 AND STANDARD DEVIATION = 1. */
/*              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS */
/*              THE PROBABILITY DENSITY FUNCTION */
/*              F(X) = (1/SQRT(2*PI))*EXP(-X*X/2). */
/*     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT */
/*                                WHICH THE CUMULATIVE DISTRIBUTION */
/*                                FUNCTION IS TO BE EVALUATED. */
/*     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE */
/*                                DISTRIBUTION FUNCTION VALUE. */
/*     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION */
/*             FUNCTION VALUE CDF. */
/*     PRINTING--NONE. */
/*     RESTRICTIONS--NONE. */
/*     OTHER DATAPAC   SUBROUTINES NEEDED--NONE. */
/*     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP. */
/*     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION. */
/*     LANGUAGE--ANSI FORTRAN. */
/*     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS */
/*                 SERIES 55, 1964, PAGE 932, FORMULA 26.2.17. */
/*               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE */
/*                 DISTRIBUTIONS--1, 1970, PAGES 40-111. */
/*     WRITTEN BY--JAMES J. FILLIBEN */
/*                 STATISTICAL ENGINEERING LABORATORY (205.03) */
/*                 NATIONAL BUREAU OF STANDARDS */
/*                 WASHINGTON, D. C. 20234 */
/*                 PHONE:  301-921-2315 */
/*     ORIGINAL VERSION--JUNE      1972. */
/*     UPDATED         --SEPTEMBER 1975. */
/*     UPDATED         --NOVEMBER  1975. */

/* --------------------------------------------------------------------- 
*/


    ipr = 6;

/*     CHECK THE INPUT ARGUMENTS FOR ERRORS. */
/*     NO INPUT ARGUMENT ERRORS POSSIBLE */
/*     FOR THIS DISTRIBUTION. */

/* -----START POINT----------------------------------------------------- 
*/

    z = *x;
    if (*x < (float)0.) {
	z = -(doublereal)z;
    }
    t = (float)1. / (p * z + (float)1.);
/* Computing 2nd power */
    r__1 = t;
/* Computing 3rd power */
    r__2 = t, r__3 = r__2;
/* Computing 4th power */
    r__4 = t, r__4 *= r__4;
/* Computing 5th power */
    r__5 = t, r__6 = r__5, r__5 *= r__5;
    *cdf = (float)1. - exp(z * (float)-.5 * z) * (float).39894228040143 * (b1 
	    * t + b2 * (r__1 * r__1) + b3 * (r__3 * (r__2 * r__2)) + b4 * (
	    r__4 * r__4) + b5 * (r__6 * (r__5 * r__5)));
    if (*x < (float)0.) {
	*cdf = (float)1. - *cdf;
    }

    return 0;
} /* norcdf_ */

