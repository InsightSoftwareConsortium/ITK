/* eispack/epslon.f -- translated by f2c (version 20050501).
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

/*<       double precision function epslon (x) >*/
doublereal epslon_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val, d__1;

    /* Local variables */
    doublereal a, b, c__, eps;

/*<       double precision x >*/

/*     estimate unit roundoff in quantities of size x. */

/*<       double precision a,b,c,eps >*/

/*     this program should function properly on all systems */
/*     satisfying the following two assumptions, */
/*        1.  the base used in representing floating point */
/*            numbers is not a power of three. */
/*        2.  the quantity  a  in statement 10 is represented to */
/*            the accuracy used in floating point variables */
/*            that are stored in memory. */
/*     the statement number 10 and the go to 10 are intended to */
/*     force optimizing compilers to generate code satisfying */
/*     assumption 2. */
/*     under these assumptions, it should be true that, */
/*            a  is not exactly equal to four-thirds, */
/*            b  has a zero for its last bit or digit, */
/*            c  is not exactly equal to one, */
/*            eps  measures the separation of 1.0 from */
/*                 the next larger floating point number. */
/*     the developers of eispack would appreciate being informed */
/*     about any systems where these assumptions do not hold. */

/*     this version dated 4/6/83. */

/*<       a = 4.0d0/3.0d0 >*/
    a = 1.3333333333333333;
/*<    10 b = a - 1.0d0 >*/
L10:
    b = a - 1.;
/*<       c = b + b + b >*/
    c__ = b + b + b;
/*<       eps = dabs(c-1.0d0) >*/
    eps = (d__1 = c__ - 1., abs(d__1));
/*<       if (eps .eq. 0.0d0) go to 10 >*/
    if (eps == 0.) {
        goto L10;
    }
/*<       epslon = eps*dabs(x) >*/
    ret_val = eps * abs(*x);
/*<       return >*/
    return ret_val;
/*<       end >*/
} /* epslon_ */

#ifdef __cplusplus
        }
#endif
