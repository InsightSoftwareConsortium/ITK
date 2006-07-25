/* minpack/enorm.f -- translated by f2c (version 20050501).
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

/*<       double precision function enorm(n,x) >*/
doublereal enorm_(integer *n, doublereal *x)
{
    /* Initialized data */

    static doublereal one = 1.; /* constant */
    static doublereal zero = 0.; /* constant */
    static doublereal rdwarf = 3.834e-20; /* constant */
    static doublereal rgiant = 1.304e19; /* constant */

    /* System generated locals */
    integer i__1;
    doublereal ret_val=0, d__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__;
    doublereal s1, s2, s3, xabs, x1max, x3max, agiant, floatn;

/*<       integer n >*/
/*<       double precision x(n) >*/
/*     ********** */

/*     function enorm */

/*     given an n-vector x, this function calculates the */
/*     euclidean norm of x. */

/*     the euclidean norm is computed by accumulating the sum of */
/*     squares in three different sums. the sums of squares for the */
/*     small and large components are scaled so that no overflows */
/*     occur. non-destructive underflows are permitted. underflows */
/*     and overflows do not occur in the computation of the unscaled */
/*     sum of squares for the intermediate components. */
/*     the definitions of small, intermediate and large components */
/*     depend on two constants, rdwarf and rgiant. the main */
/*     restrictions on these constants are that rdwarf**2 not */
/*     underflow and rgiant**2 not overflow. the constants */
/*     given here are suitable for every known computer. */

/*     the function statement is */

/*       double precision function enorm(n,x) */

/*     where */

/*       n is a positive integer input variable. */

/*       x is an input array of length n. */

/*     subprograms called */

/*       fortran-supplied ... dabs,dsqrt */

/*     argonne national laboratory. minpack project. march 1980. */
/*     burton s. garbow, kenneth e. hillstrom, jorge j. more */

/*     ********** */
/*<       integer i >*/
/*<    >*/
/*<       data one,zero,rdwarf,rgiant /1.0d0,0.0d0,3.834d-20,1.304d19/ >*/
    /* Parameter adjustments */
    --x;

    /* Function Body */
/*<       s1 = zero >*/
    s1 = zero;
/*<       s2 = zero >*/
    s2 = zero;
/*<       s3 = zero >*/
    s3 = zero;
/*<       x1max = zero >*/
    x1max = zero;
/*<       x3max = zero >*/
    x3max = zero;
/*<       floatn = n >*/
    floatn = (doublereal) (*n);
/*<       agiant = rgiant/floatn >*/
    agiant = rgiant / floatn;
/*<       do 90 i = 1, n >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          xabs = dabs(x(i)) >*/
        xabs = (d__1 = x[i__], abs(d__1));
/*<          if (xabs .gt. rdwarf .and. xabs .lt. agiant) go to 70 >*/
        if (xabs > rdwarf && xabs < agiant) {
            goto L70;
        }
/*<             if (xabs .le. rdwarf) go to 30 >*/
        if (xabs <= rdwarf) {
            goto L30;
        }

/*              sum for large components. */

/*<                if (xabs .le. x1max) go to 10 >*/
        if (xabs <= x1max) {
            goto L10;
        }
/*<                   s1 = one + s1*(x1max/xabs)**2 >*/
/* Computing 2nd power */
        d__1 = x1max / xabs;
        s1 = one + s1 * (d__1 * d__1);
/*<                   x1max = xabs >*/
        x1max = xabs;
/*<                   go to 20 >*/
        goto L20;
/*<    10          continue >*/
L10:
/*<                   s1 = s1 + (xabs/x1max)**2 >*/
/* Computing 2nd power */
        d__1 = xabs / x1max;
        s1 += d__1 * d__1;
/*<    20          continue >*/
L20:
/*<                go to 60 >*/
        goto L60;
/*<    30       continue >*/
L30:

/*              sum for small components. */

/*<                if (xabs .le. x3max) go to 40 >*/
        if (xabs <= x3max) {
            goto L40;
        }
/*<                   s3 = one + s3*(x3max/xabs)**2 >*/
/* Computing 2nd power */
        d__1 = x3max / xabs;
        s3 = one + s3 * (d__1 * d__1);
/*<                   x3max = xabs >*/
        x3max = xabs;
/*<                   go to 50 >*/
        goto L50;
/*<    40          continue >*/
L40:
/*<                   if (xabs .ne. zero) s3 = s3 + (xabs/x3max)**2 >*/
        if (xabs != zero) {
/* Computing 2nd power */
            d__1 = xabs / x3max;
            s3 += d__1 * d__1;
        }
/*<    50          continue >*/
L50:
/*<    60       continue >*/
L60:
/*<             go to 80 >*/
        goto L80;
/*<    70    continue >*/
L70:

/*           sum for intermediate components. */

/*<             s2 = s2 + xabs**2 >*/
/* Computing 2nd power */
        d__1 = xabs;
        s2 += d__1 * d__1;
/*<    80    continue >*/
L80:
/*<    90    continue >*/
/* L90: */
        ;
    }

/*     calculation of norm. */

/*<       if (s1 .eq. zero) go to 100 >*/
    if (s1 == zero) {
        goto L100;
    }
/*<          enorm = x1max*dsqrt(s1+(s2/x1max)/x1max) >*/
    ret_val = x1max * sqrt(s1 + s2 / x1max / x1max);
/*<          go to 130 >*/
    goto L130;
/*<   100 continue >*/
L100:
/*<          if (s2 .eq. zero) go to 110 >*/
    if (s2 == zero) {
        goto L110;
    }
/*<    >*/
    if (s2 >= x3max) {
        ret_val = sqrt(s2 * (one + x3max / s2 * (x3max * s3)));
    }
/*<    >*/
    if (s2 < x3max) {
        ret_val = sqrt(x3max * (s2 / x3max + x3max * s3));
    }
/*<             go to 120 >*/
    goto L120;
/*<   110    continue >*/
L110:
/*<             enorm = x3max*dsqrt(s3) >*/
    ret_val = x3max * sqrt(s3);
/*<   120    continue >*/
L120:
/*<   130 continue >*/
L130:
/*<       return >*/
    return ret_val;

/*     last card of function enorm. */

/*<       end >*/
} /* enorm_ */

#ifdef __cplusplus
        }
#endif
