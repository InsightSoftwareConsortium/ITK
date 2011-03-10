/* laso/dlaran.f -- translated by f2c (version 20050501).
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


/* *********************************************************************** */

/*<       SUBROUTINE DLARAN(N, X) >*/
/* Subroutine */ int dlaran_(integer *n, doublereal *x)
{
    /* Initialized data */

    static integer iurand = 0; /* constant */

    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__;
    extern doublereal urand_(integer *);


/*  THIS SUBROUTINE SETS THE VECTOR X TO RANDOM NUMBERS */

/*  FORMAL PARAMETERS */

/*<       INTEGER N >*/
/*<       DOUBLE PRECISION X(N) >*/

/*  LOCAL VARIABLES */

/*<       INTEGER I, IURAND >*/

/*  FUNCTIONS CALLED */

/*<       REAL URAND >*/
/*<       DOUBLE PRECISION DBLE  >*/

/*  SUBROUTINES CALLED */

/*     NONE */

/*  INITIALIZE SEED */

/*<       DATA IURAND /0/ >*/
    /* Parameter adjustments */
    --x;

    /* Function Body */

/*<       DO 10 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          X(I) = DBLE(URAND(IURAND)) - 0.5D0 >*/
        x[i__] = (doublereal) urand_(&iurand) - .5;
/*<    10 CONTINUE >*/
/* L10: */
    }
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* dlaran_ */

#ifdef __cplusplus
        }
#endif
