/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"


/* *********************************************************************** */

/* Subroutine */ int slaran_(n, x)
integer *n;
real *x;
{
    /* Initialized data */

    static integer iurand = 0;

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i;
    extern doublereal urand_();


/*  THIS SUBROUTINE SETS THE VECTOR X TO RANDOM NUMBERS */

/*  FORMAL PARAMETERS */


/*  LOCAL VARIABLES */


/*  FUNCTIONS CALLED */


/*  SUBROUTINES CALLED */

/*     NONE */

/*  INITIALIZE SEED */

    /* Parameter adjustments */
    --x;

    /* Function Body */

    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        x[i] = urand_(&iurand) - (float).5;
/* L10: */
    }
    return 0;
} /* slaran_ */

