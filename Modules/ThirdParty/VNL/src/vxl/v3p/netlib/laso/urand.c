/* laso/urand.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
        on Microsoft Windows system, link with libf2c.lib;
        on Linux or Unix systems, link with .../path/to/libf2c.a -lm
        or, if you install libf2c.a in a standard place, with -lf2c -lm
        -- in that order, at the end of the command line, as in
                cc *.o -lf2c -lm
        Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

                http://www.netlib.org/f2c/libf2c.zip
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "v3p_netlib.h"

/*<       REAL FUNCTION URAND(IY) >*/
doublereal urand_(integer *iy)
{
    /* Initialized data */

    static integer m2 = 0; /* constant */
    //UNUSED VAR static integer itwo = 2; /* constant */

    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double atan(doublereal), sqrt(doublereal);

    /* Local variables */
    real s=0;
    integer ia=0, ic=0, mic=0;
    doublereal halfm;

/*<       INTEGER  IY >*/

/*      URAND IS A UNIFORM RANDOM NUMBER GENERATOR BASED  ON  THEORY  AND */
/*  SUGGESTIONS  GIVEN  IN  D.E. KNUTH (1969),  VOL  2.   THE INTEGER  IY */
/*  SHOULD BE INITIALIZED TO AN ARBITRARY INTEGER PRIOR TO THE FIRST CALL */
/*  TO URAND.  THE CALLING PROGRAM SHOULD  NOT  ALTER  THE  VALUE  OF  IY */
/*  BETWEEN  SUBSEQUENT CALLS TO URAND.  VALUES OF URAND WILL BE RETURNED */
/*  IN THE INTERVAL (0,1). */

/*<       INTEGER  IA,IC,ITWO,M2,M,MIC >*/
/*<       DOUBLE PRECISION  HALFM >*/
/*<       REAL  S >*/
/*<       DOUBLE PRECISION  DATAN,DSQRT >*/
/*<       DATA M2/0/,ITWO/2/ >*/
/*<       IF (M2 .NE. 0) GO TO 20 >*/
    if (m2 != 0) {
        goto L20;
    }

/*  IF FIRST ENTRY, COMPUTE MACHINE INTEGER WORD LENGTH */
/*<       M = 1 >*/
/*<    10 M2 = M >*/
/*<       M = ITWO*M2 >*/
/*<       IF (M .GT. M2) GO TO 10 >*/
/* Just use bit shifting to prevent signed integer overflow in original f2c code */
    m2 = ((integer) 1) << ( sizeof(integer) * 8 - 2);

/*<       HALFM = M2 >*/
    halfm = (doublereal) m2;

/*  COMPUTE MULTIPLIER AND INCREMENT FOR LINEAR CONGRUENTIAL METHOD */

/*<       IA = 8*IDINT(HALFM*DATAN(1.D0)/8.D0) + 5 >*/
    ia = ((integer) (halfm * atan(1.) / 8.) << 3) + 5;
/*<       IC = 2*IDINT(HALFM*(0.5D0-DSQRT(3.D0)/6.D0)) + 1 >*/
    ic = ((integer) (halfm * (.5 - sqrt(3.) / 6.)) << 1) + 1;
/*<       MIC = (M2 - IC) + M2 >*/
    mic = m2 - ic + m2;

/*  S IS THE SCALE FACTOR FOR CONVERTING TO FLOATING POINT */

/*<       S = 0.5/HALFM >*/
    s = (float).5 / halfm;

/*  COMPUTE NEXT RANDOM NUMBER */

/*<    20 IY = IY*IA >*/
L20:
    *iy *= ia;

/*  THE FOLLOWING STATEMENT IS FOR COMPUTERS WHICH DO NOT ALLOW */
/*  INTEGER OVERFLOW ON ADDITION */

/*<       IF (IY .GT. MIC) IY = (IY - M2) - M2 >*/
    if (*iy > mic) {
        *iy = *iy - m2 - m2;
    }

/*<       IY = IY + IC >*/
    *iy += ic;

/*  THE FOLLOWING STATEMENT IS FOR COMPUTERS WHERE THE */
/*  WORD LENGTH FOR ADDITION IS GREATER THAN FOR MULTIPLICATION */

/*<       IF (IY/2 .GT. M2) IY = (IY - M2) - M2 >*/
    if (*iy / 2 > m2) {
        *iy = *iy - m2 - m2;
    }

/*  THE FOLLOWING STATEMENT IS FOR COMPUTERS WHERE INTEGER */
/*  OVERFLOW AFFECTS THE SIGN BIT */

/*<       IF (IY .LT. 0) IY = (IY + M2) + M2 >*/
    if (*iy < 0) {
        *iy = *iy + m2 + m2;
    }
/*<       URAND = FLOAT(IY)*S >*/
    ret_val = (real) (*iy) * s;
/*<       RETURN >*/
    return ret_val;
/*<       END >*/
} /* urand_ */

#ifdef __cplusplus
        }
#endif
