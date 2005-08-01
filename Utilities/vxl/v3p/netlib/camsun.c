#include "f2c.h"
#include "netlib.h"
#include <stdio.h>
extern double sqrt(double), exp(double), pow(double,double), log(double); /* #include <math.h> */

static void norcdf_(real *x, real *cdf);
static void dnorcdf_(doublereal *x, doublereal *cdf);

/* Subroutine */ void chscdf_(x, nu, cdf)
const real *x;
const integer *nu;
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

    /* Local variables */
    static real cdfn;
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
    static doublereal chi;
    static real anu;
    static doublereal danu, danu2, dnu;
    static doublereal sum;

/*     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION     */
/*              FUNCTION VALUE FOR THE CHI-SQUARED DISTRIBUTION          */
/*              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.          */
/*              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X.     */
/*              THE PROBABILITY DENSITY FUNCTION IS GIVEN                */
/*              IN THE REFERENCES BELOW.                                 */
/*     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT          */
/*                                WHICH THE CUMULATIVE DISTRIBUTION      */
/*                                FUNCTION IS TO BE EVALUATED.           */
/*                                X SHOULD BE NON-NEGATIVE.              */
/*                     --NU     = THE INTEGER NUMBER OF DEGREES          */
/*                                OF FREEDOM.                            */
/*                                NU SHOULD BE POSITIVE.                 */
/*     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE        */
/*                                DISTRIBUTION FUNCTION VALUE.           */
/*     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION              */
/*             FUNCTION VALUE CDF FOR THE CHI-SQUARED DISTRIBUTION       */
/*             WITH DEGREES OF FREEDOM PARAMETER = NU.                   */
/*     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.   */
/*     RESTRICTIONS--X SHOULD BE NON-NEGATIVE.                           */
/*                 --NU SHOULD BE A POSITIVE INTEGER VARIABLE.           */
/*     OTHER DATAPAC   SUBROUTINES NEEDED--NORCDF.                       */
/*     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DEXP.                  */
/*     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.                    */
/*     LANGUAGE--ANSI FORTRAN.                                           */
/*     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS      */
/*                 SERIES 55, 1964, PAGE 941, FORMULAE 26.4.4 AND 26.4.5.*/
/*               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE               */
/*                 DISTRIBUTIONS--1, 1970, PAGE 176,                     */
/*                 FORMULA 28, AND PAGE 180, FORMULA 33.1.               */
/*               --OWEN, HANDBOOK OF STATISTICAL TABLES,                 */
/*                 1962, PAGES 50-55.                                    */
/*               --PEARSON AND HARTLEY, BIOMETRIKA TABLES                */
/*                 FOR STATISTICIANS, VOLUME 1, 1954,                    */
/*                 PAGES 122-131.                                        */
/*     WRITTEN BY--JAMES J. FILLIBEN                                     */
/*                 STATISTICAL ENGINEERING LABORATORY (205.03)           */
/*                 NATIONAL BUREAU OF STANDARDS                          */
/*                 WASHINGTON, D. C. 20234                               */
/*                 PHONE:  301-921-2315                                  */
/*     ORIGINAL VERSION--JUNE      1972.                                 */
/*     UPDATED         --MAY       1974.                                 */
/*     UPDATED         --SEPTEMBER 1975.                                 */
/*     UPDATED         --NOVEMBER  1975.                                 */
/*     UPDATED         --OCTOBER   1976.                                 */
/*                                                                       */
/* --------------------------------------------------------------------- */

/*     CHECK THE INPUT ARGUMENTS FOR ERRORS */

    if (*nu <= 0) {
        fprintf(stderr,fmt_15);
        fprintf(stderr,fmt_47,*nu);
        *cdf = 0.f;
        return;
    }
    if (*x < 0.f) {
        fprintf(stderr,fmt_4);
        fprintf(stderr,fmt_46,*x);
        *cdf = 0.f;
        return;
    }

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

    if (*x <= 0.f) {
        *cdf = 0.f;
        return;
    }
    amean = anu;
    sd = sqrtf(anu * 2.f);
    z = (*x - amean) / sd;
    if (*nu < 10 && z < -200.f) {
        *cdf = 0.f;
        return;
    }
    if (*nu >= 10 && z < -100.f) {
        *cdf = 0.f;
        return;
    }
    if (*nu < 10 && z > 200.f) {
        *cdf = 1.f;
        return;
    }
    if (*nu >= 10 && z > 100.f) {
        *cdf = 1.f;
        return;
    }

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
    return;

/*     TREAT THE SMALL AND MODERATE DEGREES OF FREEDOM CASE */
/*     (THAT IS, WHEN NU IS SMALLER THAN 1000). */
/*     METHOD UTILIZED--EXACT FINITE SUM */
/*     (SEE AMS 55, PAGE 941, FORMULAE 26.4.4 AND 26.4.5). */

L1000:
    chi = sqrt(dx);
    ievodd = *nu - (*nu / 2 << 1);
    if (ievodd == 0) {
        sum = 1.;
        term = 1.;
        imin = 2;
        imax = *nu - 2;
    }
    else {
        sum = 0.;
        term = 1.f / chi;
        imin = 1;
        imax = *nu - 1;
    }
    if (imin <= imax)
    for (i = imin; i <= imax; i += 2) {
        ai = (doublereal) i;
        term *= dx / ai;
        sum += term;
    }

    sum *= exp(-dx / 2.);
    if (ievodd != 0) {
        sum *= sqrt(2. / pi);
        spchi = (float)chi;
        norcdf_(&spchi, &cdfn);
        dcdfn = cdfn;
        sum += (1. - dcdfn) * 2.;
    }
    *cdf = 1.f - (float)sum;
    return;

/*     TREAT THE CASE WHEN NU IS LARGE */
/*     (THAT IS, WHEN NU IS EQUAL TO OR GREATER THAN 1000) */
/*     AND X IS LESS THAN OR EQUAL TO NU. */
/*     METHOD UTILIZED--WILSON-HILFERTY APPROXIMATION */
/*     (SEE JOHNSON AND KOTZ, VOLUME 1, PAGE 176, FORMULA 28). */

L2000:
    dfact = dnu * 4.5;
    u = (float)(dx / dnu);
    u = (float)((pow(u, dpower) - 1.0 + 1.0 / dfact) * sqrt(dfact));
    norcdf_(&u, &cdfn);
    *cdf = cdfn;
    return;

/*     TREAT THE CASE WHEN NU IS LARGE */
/*     (THAT IS, WHEN NU IS EQUAL TO OR GREATER THAN 1000) */
/*     AND X IS LARGER THAN NU. */
/*     METHOD UTILIZED--HILL'S ASYMPTOTIC EXPANSION */
/*     (SEE JOHNSON AND KOTZ, VOLUME 1, PAGE 180, FORMULA 33.1). */

L3000:
    dw = sqrt(dx - dnu - dnu * log(dx / dnu));
    danu = sqrt(2.0 / dnu);
    d1 = dw;
    d2 = dw * dw;
    d3 = dw * d2;
    term0 = dw;
    term1 = b11 * danu;
    danu2 = danu * danu;
    term2 = b21 * d1 * danu2;
    term3 = b31 * (d2 + b32) * danu * danu2;
    term4 = b41 * (b42 * d3 + b43 * d1) * danu2 * danu2;
    u = (float)(term0 + term1 + term2 + term3 + term4);
    norcdf_(&u, &cdfn);
    *cdf = cdfn;
} /* chscdf_ */

/* Subroutine */
static void norcdf_(x, cdf)
real *x, *cdf;
{
    /* Initialized data */
    static real b1 = .31938153f;
    static real b2 = -.356563782f;
    static real b3 = 1.781477937f;
    static real b4 = -1.821255978f;
    static real b5 = 1.330274429f;
    static real p = .2316419f;

    /* System generated locals */
    real tt;

    /* Local variables */
    static real t, z;

/*     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION     */
/*              FUNCTION VALUE FOR THE NORMAL (GAUSSIAN)                 */
/*              DISTRIBUTION WITH MEAN = 0 AND STANDARD DEVIATION = 1.   */
/*              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS           */
/*              THE PROBABILITY DENSITY FUNCTION                         */
/*              F(X) = (1/SQRT(2*PI))*EXP(-X*X/2).                       */
/*     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT          */
/*                                WHICH THE CUMULATIVE DISTRIBUTION      */
/*                                FUNCTION IS TO BE EVALUATED.           */
/*     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE        */
/*                                DISTRIBUTION FUNCTION VALUE.           */
/*     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION              */
/*             FUNCTION VALUE CDF.                                       */
/*     PRINTING--NONE.                                                   */
/*     RESTRICTIONS--NONE.                                               */
/*     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.                         */
/*     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.                          */
/*     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.                    */
/*     LANGUAGE--ANSI FORTRAN.                                           */
/*     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS      */
/*                 SERIES 55, 1964, PAGE 932, FORMULA 26.2.17.           */
/*               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE               */
/*                 DISTRIBUTIONS--1, 1970, PAGES 40-111.                 */
/*     WRITTEN BY--JAMES J. FILLIBEN                                     */
/*                 STATISTICAL ENGINEERING LABORATORY (205.03)           */
/*                 NATIONAL BUREAU OF STANDARDS                          */
/*                 WASHINGTON, D. C. 20234                               */
/*                 PHONE:  301-921-2315                                  */
/*     ORIGINAL VERSION--JUNE      1972.                                 */
/*     UPDATED         --SEPTEMBER 1975.                                 */
/*     UPDATED         --NOVEMBER  1975.                                 */
/*                                                                       */
/* --------------------------------------------------------------------- */

/*     CHECK THE INPUT ARGUMENTS FOR ERRORS. */
/*     NO INPUT ARGUMENT ERRORS POSSIBLE FOR THIS DISTRIBUTION. */

    z = *x;
    if (z < 0.f)
        z = -z;
    t = 1.f / (p * z + 1.f);
    tt = t*t;
    *cdf = 1.f - (float)exp(z * -.5f * z) * .39894228040143f * (b1 * t + b2 * tt + b3 * t*tt + b4 * tt*tt + b5 * t*tt*tt);
    if (*x < 0.f) {
        *cdf = 1.f - *cdf;
    }
} /* norcdf_ */

/* Subroutine */ void dchscdf_(x, nu, cdf)
doublereal *x;
integer *nu;
doublereal *cdf;
{
    /* Initialized data */
    static integer nucut = 1000;
    static doublereal b43 = 17.;
    static doublereal pi = 3.14159265358979;
    static doublereal dpower = .333333333333333333;
    static doublereal b11 = .333333333333333333;
    static doublereal b21 = -.027777777777777778;
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
    doublereal danu2;

    /* Local variables */
    static doublereal cdfn, danu;
    static integer imin, imax;
    static doublereal term, term0, term1, term2, term3, term4;
    static integer i;
    static doublereal dcdfn, dfact;
    static doublereal amean, u, z;
    static integer ibran;
    static doublereal spchi;
    static doublereal d1, d2, d3, ai;
    static doublereal sd;
    static doublereal dw, dx;
    static integer ievodd;
    static doublereal chi;
    static doublereal anu;
    static doublereal dnu;
    static doublereal sum;

/*     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION     */
/*              FUNCTION VALUE FOR THE CHI-SQUARED DISTRIBUTION          */
/*              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.          */
/*              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X.     */
/*              THE PROBABILITY DENSITY FUNCTION IS GIVEN                */
/*              IN THE REFERENCES BELOW.                                 */
/*     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT          */
/*                                WHICH THE CUMULATIVE DISTRIBUTION      */
/*                                FUNCTION IS TO BE EVALUATED.           */
/*                                X SHOULD BE NON-NEGATIVE.              */
/*                     --NU     = THE INTEGER NUMBER OF DEGREES          */
/*                                OF FREEDOM.                            */
/*                                NU SHOULD BE POSITIVE.                 */
/*     OUTPUT ARGUMENTS--CDF    = THE DOUBLE PRECISION CUMULATIVE        */
/*                                DISTRIBUTION FUNCTION VALUE.           */
/*     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION              */
/*             FUNCTION VALUE CDF FOR THE CHI-SQUARED DISTRIBUTION       */
/*             WITH DEGREES OF FREEDOM PARAMETER = NU.                   */
/*     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.   */
/*     RESTRICTIONS--X SHOULD BE NON-NEGATIVE.                           */
/*                 --NU SHOULD BE A POSITIVE INTEGER VARIABLE.           */
/*     OTHER DATAPAC   SUBROUTINES NEEDED--NORCDF.                       */
/*     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DEXP.                  */
/*     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.                    */
/*     LANGUAGE--ANSI FORTRAN.                                           */
/*     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS      */
/*                 SERIES 55, 1964, PAGE 941, FORMULAE 26.4.4 AND 26.4.5.*/
/*               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE               */
/*                 DISTRIBUTIONS--1, 1970, PAGE 176,                     */
/*                 FORMULA 28, AND PAGE 180, FORMULA 33.1.               */
/*               --OWEN, HANDBOOK OF STATISTICAL TABLES,                 */
/*                 1962, PAGES 50-55.                                    */
/*               --PEARSON AND HARTLEY, BIOMETRIKA TABLES                */
/*                 FOR STATISTICIANS, VOLUME 1, 1954,                    */
/*                 PAGES 122-131.                                        */
/*     WRITTEN BY--JAMES J. FILLIBEN                                     */
/*                 STATISTICAL ENGINEERING LABORATORY (205.03)           */
/*                 NATIONAL BUREAU OF STANDARDS                          */
/*                 WASHINGTON, D. C. 20234                               */
/*                 PHONE:  301-921-2315                                  */
/*     ORIGINAL VERSION--JUNE      1972.                                 */
/*     UPDATED         --MAY       1974.                                 */
/*     UPDATED         --SEPTEMBER 1975.                                 */
/*     UPDATED         --NOVEMBER  1975.                                 */
/*     UPDATED         --OCTOBER   1976.                                 */
/*                                                                       */
/* --------------------------------------------------------------------- */

/*     CHECK THE INPUT ARGUMENTS FOR ERRORS */

    if (*nu <= 0) {
        fprintf(stderr,fmt_15);
        fprintf(stderr,fmt_47,*nu);
        *cdf = 0.0;
        return;
    }
    if (*x < 0.0) {
        fprintf(stderr,fmt_4);
        fprintf(stderr,fmt_46,*x);
        *cdf = 0.0;
        return;
    }

    dx = *x;
    anu = (doublereal) (*nu);
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

    if (*x <= 0.0) {
        *cdf = 0.0;
        return;
    }
    amean = anu;
    sd = sqrt(anu * 2.0);
    z = (*x - amean) / sd;
    if (*nu < 10 && z < -200.0) {
        *cdf = 0.0;
        return;
    }
    if (*nu >= 10 && z < -100.0) {
        *cdf = 0.0;
        return;
    }
    if (*nu < 10 && z > 200.0) {
        *cdf = 1.0;
        return;
    }
    if (*nu >= 10 && z > 100.0) {
        *cdf = 1.0;
        return;
    }

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
    return;

/*     TREAT THE SMALL AND MODERATE DEGREES OF FREEDOM CASE */
/*     (THAT IS, WHEN NU IS SMALLER THAN 1000). */
/*     METHOD UTILIZED--EXACT FINITE SUM */
/*     (SEE AMS 55, PAGE 941, FORMULAE 26.4.4 AND 26.4.5). */

L1000:
    chi = sqrt(dx);
    ievodd = *nu - (*nu / 2 << 1);
    if (ievodd == 0) {
        sum = 1.;
        term = 1.;
        imin = 2;
        imax = *nu - 2;
    }
    else {
        sum = 0.;
        term = 1.0 / chi;
        imin = 1;
        imax = *nu - 1;
    }
    if (imin <= imax)
    for (i = imin; i <= imax; i += 2) {
        ai = (doublereal) i;
        term *= dx / ai;
        sum += term;
    }

    sum *= exp(-dx / 2.);
    if (ievodd != 0) {
        sum *= sqrt(2. / pi);
        spchi = chi;
        dnorcdf_(&spchi, &cdfn);
        dcdfn = cdfn;
        sum += (1. - dcdfn) * 2.;
    }
    *cdf = 1.0 - sum;
    if (*cdf < 0.0) *cdf = 0.0;
    return;

/*     TREAT THE CASE WHEN NU IS LARGE */
/*     (THAT IS, WHEN NU IS EQUAL TO OR GREATER THAN 1000) */
/*     AND X IS LESS THAN OR EQUAL TO NU. */
/*     METHOD UTILIZED--WILSON-HILFERTY APPROXIMATION */
/*     (SEE JOHNSON AND KOTZ, VOLUME 1, PAGE 176, FORMULA 28). */

L2000:
    dfact = dnu * 4.5;
    u = dx / dnu;
    u = (pow(u, dpower) - 1.0 + 1.0 / dfact) * sqrt(dfact);
    dnorcdf_(&u, &cdfn);
    *cdf = cdfn;
    return;

/*     TREAT THE CASE WHEN NU IS LARGE */
/*     (THAT IS, WHEN NU IS EQUAL TO OR GREATER THAN 1000) */
/*     AND X IS LARGER THAN NU. */
/*     METHOD UTILIZED--HILL'S ASYMPTOTIC EXPANSION */
/*     (SEE JOHNSON AND KOTZ, VOLUME 1, PAGE 180, FORMULA 33.1). */

L3000:
    dw = sqrt(dx - dnu - dnu * log(dx / dnu));
    danu = sqrt(2.0 / dnu);
    d1 = dw;
    d2 = dw * dw;
    d3 = dw * d2;
    term0 = dw;
    term1 = b11 * danu;
    danu2 = danu * danu;
    term2 = b21 * d1 * danu2;
    term3 = b31 * (d2 + b32) * danu * danu2;
    term4 = b41 * (b42 * d3 + b43 * d1) * danu2 * danu2;
    u = term0 + term1 + term2 + term3 + term4;
    dnorcdf_(&u, &cdfn);
    *cdf = cdfn;
} /* dchscdf_ */

/* Subroutine */
static void dnorcdf_(x, cdf)
doublereal *x, *cdf;
{
    /* Initialized data */
    static doublereal b1 = .31938153;
    static doublereal b2 = -.356563782;
    static doublereal b3 = 1.781477937;
    static doublereal b4 = -1.821255978;
    static doublereal b5 = 1.330274429;
    static doublereal p = .2316419;

    /* System generated locals */
    doublereal tt;

    /* Local variables */
    static doublereal t, z;


/*     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION     */
/*              FUNCTION VALUE FOR THE NORMAL (GAUSSIAN)                 */
/*              DISTRIBUTION WITH MEAN = 0 AND STANDARD DEVIATION = 1.   */
/*              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS           */
/*              THE PROBABILITY DENSITY FUNCTION                         */
/*              F(X) = (1/SQRT(2*PI))*EXP(-X*X/2).                       */
/*     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT          */
/*                                WHICH THE CUMULATIVE DISTRIBUTION      */
/*                                FUNCTION IS TO BE EVALUATED.           */
/*     OUTPUT ARGUMENTS--CDF    = THE DOUBLE PRECISION CUMULATIVE        */
/*                                DISTRIBUTION FUNCTION VALUE.           */
/*     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION              */
/*             FUNCTION VALUE CDF.                                       */
/*     PRINTING--NONE.                                                   */
/*     RESTRICTIONS--NONE.                                               */
/*     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.                         */
/*     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.                          */
/*     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.                    */
/*     LANGUAGE--ANSI FORTRAN.                                           */
/*     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS      */
/*                 SERIES 55, 1964, PAGE 932, FORMULA 26.2.17.           */
/*               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE               */
/*                 DISTRIBUTIONS--1, 1970, PAGES 40-111.                 */
/*     WRITTEN BY--JAMES J. FILLIBEN                                     */
/*                 STATISTICAL ENGINEERING LABORATORY (205.03)           */
/*                 NATIONAL BUREAU OF STANDARDS                          */
/*                 WASHINGTON, D. C. 20234                               */
/*                 PHONE:  301-921-2315                                  */
/*     ORIGINAL VERSION--JUNE      1972.                                 */
/*     UPDATED         --SEPTEMBER 1975.                                 */
/*     UPDATED         --NOVEMBER  1975.                                 */
/*                                                                       */
/* --------------------------------------------------------------------- */

/*     CHECK THE INPUT ARGUMENTS FOR ERRORS. */
/*     NO INPUT ARGUMENT ERRORS POSSIBLE FOR THIS DISTRIBUTION. */

    z = *x;
    if (z < 0.0)
        z = -z;
    t = 1.0 / (p * z + 1.0);
    tt = t*t;
    *cdf = 1.0 - exp(z * -.5 * z) * .39894228040143 * (b1 * t + b2 * tt + b3 * t*tt + b4 * tt*tt + b5 * t*tt*tt);
    if (*x < 0.0)
        *cdf = 1.0 - *cdf;
    if (*cdf < 0.0)
        *cdf = 0.0;
} /* dnorcdf_ */
