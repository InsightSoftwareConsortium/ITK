/* lbfgs-example.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "../f2c.h"
#include "../netlib.h"
#include <stdio.h>
/* Common Block Declarations */

Extern struct {
    integer mp, lp;
    doublereal gtol, stpmin, stpmax;
} lb3_;

#define lb3_1 lb3_


/*     *********************** */
/*     SIMPLE DRIVER FOR LBFGS */
/*     *********************** */

/*     Example of driver for LBFGS routine, using a */
/*     simple test problem. The solution point is at */
/*     X=(1,...,1) and the optimal function value of 0. */

/*                          JORGE NOCEDAL */
/*                        *** July 1990 *** */

/* Main program */ int main(void)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Local variables */
    static doublereal diag[2000], xtol;
    static doublereal f, g[2000];
    static integer j, m, n, iflag, icall;
    static doublereal w[30014], x[2000];
    static doublereal t1, t2;
    static logical diagco;
    static integer iprint[2];
    static doublereal eps;


/*     The driver for LBFGS must always declare LB2 as EXTERNAL */


    lb3_1.lp = 1;
    n = 100;
    m = 5;
    iprint[0] = 1;
    iprint[1] = 0;

/*     We do not wish to provide the diagonal matrices Hk0, and */
/*     therefore set DIAGCO to FALSE. */

    diagco = FALSE_;
    eps = 1e-5;
    xtol = 1e-16;
    icall = 0;
    iflag = 0;
    i__1 = n;
    for (j = 1; j <= i__1; j += 2) {
        x[j - 1] = -1.2;
        x[j] = 1.;
/* L10: */
    }

L20:
    f = 0.;
    i__1 = n;
    for (j = 1; j <= i__1; j += 2) {
        t1 = 1. - x[j - 1];
/* Computing 2nd power */
        d__1 = x[j - 1];
        t2 = (x[j] - d__1 * d__1) * 10.;
        g[j] = t2 * 20.;
        g[j - 1] = (x[j - 1] * g[j] + t1) * -2.;
/* Computing 2nd power */
        d__1 = t1;
/* Computing 2nd power */
        d__2 = t2;
        f = f + d__1 * d__1 + d__2 * d__2;
/* L30: */
    }
    lbfgs_(&n, &m, x, &f, g, &diagco, diag, iprint, &eps, &xtol, w, &iflag);
    printf("f = %7g\n", f);
    if (iflag <= 0) {
        goto L50;
    }
    ++icall;
/*     We allow at most 2000 evaluations of F and G */
    if (icall > 2000) {
        goto L50;
    }
    goto L20;
L50:
    printf("f = %7g\n", f);
    printf("iterations = %16d\n", icall);

    return 0;
} /* main */

/* Main program alias */ int sdrive_ (void); int sdrive_() { return main (); }
