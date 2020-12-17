/* eispack/pythag.f -- translated by f2c (version 20050501).
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

/*<       double precision function pythag(a,b) >*/
doublereal pythag_(doublereal *a, doublereal *b)
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2, d__3;

    /* Local variables */
    doublereal p, r__, s, t, u;

/*<       double precision a,b >*/

/*     finds dsqrt(a**2+b**2) without overflow or destructive underflow */

/*<       double precision p,r,s,t,u >*/
/*<       p = dmax1(dabs(a),dabs(b)) >*/
/* Computing MAX */
    d__1 = abs(*a), d__2 = abs(*b);
    p = max(d__1,d__2);
/*<       if (p .eq. 0.0d0) go to 20 >*/
    if (p == 0.) {
        goto L20;
    }
/*<       r = (dmin1(dabs(a),dabs(b))/p)**2 >*/
/* Computing MIN */
    d__2 = abs(*a), d__3 = abs(*b);
/* Computing 2nd power */
    d__1 = min(d__2,d__3) / p;
    r__ = d__1 * d__1;
/*<    10 continue >*/
L10:
/*<          t = 4.0d0 + r >*/
    t = r__ + 4.;
/*<          if (t .eq. 4.0d0) go to 20 >*/
    if (t == 4.) {
        goto L20;
    }
/*<          s = r/t >*/
    s = r__ / t;
/*<          u = 1.0d0 + 2.0d0*s >*/
    u = s * 2. + 1.;
/*<          p = u*p >*/
    p = u * p;
/*<          r = (s/u)**2 * r >*/
/* Computing 2nd power */
    d__1 = s / u;
    r__ = d__1 * d__1 * r__;
/*<       go to 10 >*/
    goto L10;
/*<    20 pythag = p >*/
L20:
    ret_val = p;
/*<       return >*/
    return ret_val;
/*<       end >*/
} /* pythag_ */

#ifdef __cplusplus
        }
#endif
