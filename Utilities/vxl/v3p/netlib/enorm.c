#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

doublereal enorm_(n, x)
const integer *n;
const doublereal *x;
{
    /* Initialized data */
    static doublereal rdwarf = 3.834e-20;
    static doublereal rgiant = 1.304e19;

    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal xabs, x1max, x3max;
    static integer i;
    static doublereal s1, s2, s3, agiant;

/*     **********                                                       */
/*                                                                      */
/*     function enorm                                                   */
/*                                                                      */
/*     given an n-vector x, this function calculates the                */
/*     euclidean norm of x.                                             */
/*                                                                      */
/*     the euclidean norm is computed by accumulating the sum of        */
/*     squares in three different sums. the sums of squares for the     */
/*     small and large components are scaled so that no overflows       */
/*     occur. non-destructive underflows are permitted. underflows      */
/*     and overflows do not occur in the computation of the unscaled    */
/*     sum of squares for the intermediate components.                  */
/*     the definitions of small, intermediate and large components      */
/*     depend on two constants, rdwarf and rgiant. the main             */
/*     restrictions on these constants are that rdwarf**2 not           */
/*     underflow and rgiant**2 not overflow. the constants              */
/*     given here are suitable for every known computer.                */
/*                                                                      */
/*     the function statement is                                        */
/*                                                                      */
/*       double precision function enorm(n,x)                           */
/*                                                                      */
/*     where                                                            */
/*                                                                      */
/*       n is a positive integer input variable.                        */
/*                                                                      */
/*       x is an input array of length n.                               */
/*                                                                      */
/*     argonne national laboratory. minpack project. march 1980.        */
/*     burton s. garbow, kenneth e. hillstrom, jorge j. more            */
/*                                                                      */
/*     **********                                                       */

    s1 = 0.; s2 = 0.; s3 = 0.;
    x1max = 0.; x3max = 0.;
    agiant = rgiant / (*n);
    for (i = 0; i < *n; ++i) {
        xabs = abs(x[i]);
        if (xabs > rdwarf && xabs < agiant) {
/*           sum for intermediate components. */
            s2 += xabs * xabs;
        }
        else if (xabs <= rdwarf) {
/*              sum for small components. */
            if (xabs <= x3max) {
                if (xabs != 0.) {
                    d__1 = xabs / x3max;
                    s3 += d__1 * d__1;
                }
            }
            else {
                d__1 = x3max / xabs;
                s3 = 1. + s3 * d__1 * d__1;
                x3max = xabs;
            }
        }
/*              sum for large components. */
        else if (xabs <= x1max) {
            d__1 = xabs / x1max;
            s1 += d__1 * d__1;
        }
        else {
            d__1 = x1max / xabs;
            s1 = 1. + s1 * d__1 * d__1;
            x1max = xabs;
        }
    }

/*     calculation of norm. */

    if (s1 != 0.)
        return x1max * sqrt(s1 + s2 / x1max / x1max);
    else if (s2 == 0.)
        return x3max * sqrt(s3);
    else if (s2 >= x3max)
        return sqrt(s2 * (1. + x3max / s2 * (x3max * s3)));
    else
        return sqrt(x3max * (s2 / x3max + x3max * s3));

} /* enorm_ */
