#include "f2c.h"
#include "netlib.h"
extern double atan(double), sqrt(double); /* #include <math.h> */

doublereal urand_(iy)
integer *iy;
{
    /* Initialized data */

    static integer m2 = 0;
    static integer itwo = 2;

    /* Local variables */
    static integer m;
    static real s;
    static doublereal halfm;
    static integer ia, ic, mic;

/*      URAND IS A UNIFORM RANDOM NUMBER GENERATOR BASED  ON  THEORY  AND */
/*  SUGGESTIONS  GIVEN  IN  D.E. KNUTH (1969),  VOL  2.   THE INTEGER  IY */
/*  SHOULD BE INITIALIZED TO AN ARBITRARY INTEGER PRIOR TO THE FIRST CALL */
/*  TO URAND.  THE CALLING PROGRAM SHOULD  NOT  ALTER  THE  VALUE  OF  IY */
/*  BETWEEN  SUBSEQUENT CALLS TO URAND.  VALUES OF URAND WILL BE RETURNED */
/*  IN THE INTERVAL (0,1). */

    if (m2 != 0) {
        goto L20;
    }

/*  IF FIRST ENTRY, COMPUTE MACHINE INTEGER WORD LENGTH */

    m = 1;
    do {
        m2 = m;
        m = itwo * m2;
    } while (m > m2);

    halfm = (doublereal) m2;

/*  COMPUTE MULTIPLIER AND INCREMENT FOR LINEAR CONGRUENTIAL METHOD */

    ia = ((integer) (halfm * atan(1.) / 8.) << 3) + 5;
    ic = ((integer) (halfm * (.5 - sqrt(3.) / 6.)) << 1) + 1;
    mic = m2 - ic + m2;

/*  S IS THE SCALE FACTOR FOR CONVERTING TO FLOATING POINT */

    s = .5f / m2;

/*  COMPUTE NEXT RANDOM NUMBER */

L20:
    *iy *= ia;

/*  THE FOLLOWING STATEMENT IS FOR COMPUTERS WHICH DO NOT ALLOW */
/*  INTEGER OVERFLOW ON ADDITION */

    if (*iy > mic) {
        *iy = *iy - m2 - m2;
    }

    *iy += ic;

/*  THE FOLLOWING STATEMENT IS FOR COMPUTERS WHERE THE */
/*  WORD LENGTH FOR ADDITION IS GREATER THAN FOR MULTIPLICATION */

    if (*iy / 2 > m2) {
        *iy = *iy - m2 - m2;
    }

/*  THE FOLLOWING STATEMENT IS FOR COMPUTERS WHERE INTEGER */
/*  OVERFLOW AFFECTS THE SIGN BIT */

    if (*iy < 0) {
        *iy = *iy + m2 + m2;
    }
    return (real) (*iy) * s;
} /* urand_ */
