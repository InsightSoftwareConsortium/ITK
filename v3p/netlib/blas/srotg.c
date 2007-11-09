/* blas/srotg.f -- translated by f2c (version 20050501).
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

/* Table of constant values */

static real c_b4 = (float)1.;

/*<       subroutine srotg(sa,sb,c,s) >*/
/* Subroutine */ int srotg_(real *sa, real *sb, real *c__, real *s)
{
    /* System generated locals */
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    real r__, z__, roe, scale;


/*     construct givens plane rotation. */
/*     jack dongarra, linpack, 3/11/78. */

/*<       real sa,sb,c,s,roe,scale,r,z >*/

/*<       roe = sb >*/
    roe = *sb;
/*<       if( abs(sa) .gt. abs(sb) ) roe = sa >*/
    if (dabs(*sa) > dabs(*sb)) {
        roe = *sa;
    }
/*<       scale = abs(sa) + abs(sb) >*/
    scale = dabs(*sa) + dabs(*sb);
/*<       if( scale .ne. 0.0 ) go to 10 >*/
    if (scale != (float)0.) {
        goto L10;
    }
/*<          c = 1.0 >*/
    *c__ = (float)1.;
/*<          s = 0.0 >*/
    *s = (float)0.;
/*<          r = 0.0 >*/
    r__ = (float)0.;
/*<          z = 0.0 >*/
    z__ = (float)0.;
/*<          go to 20 >*/
    goto L20;
/*<    10 r = scale*sqrt((sa/scale)**2 + (sb/scale)**2) >*/
L10:
/* Computing 2nd power */
    r__1 = *sa / scale;
/* Computing 2nd power */
    r__2 = *sb / scale;
    r__ = scale * sqrt(r__1 * r__1 + r__2 * r__2);
/*<       r = sign(1.0,roe)*r >*/
    r__ = r_sign(&c_b4, &roe) * r__;
/*<       c = sa/r >*/
    *c__ = *sa / r__;
/*<       s = sb/r >*/
    *s = *sb / r__;
/*<       z = 1.0 >*/
    z__ = (float)1.;
/*<       if( abs(sa) .gt. abs(sb) ) z = s >*/
    if (dabs(*sa) > dabs(*sb)) {
        z__ = *s;
    }
/*<       if( abs(sb) .ge. abs(sa) .and. c .ne. 0.0 ) z = 1.0/c >*/
    if (dabs(*sb) >= dabs(*sa) && *c__ != (float)0.) {
        z__ = (float)1. / *c__;
    }
/*<    20 sa = r >*/
L20:
    *sa = r__;
/*<       sb = z >*/
    *sb = z__;
/*<       return >*/
    return 0;
/*<       end >*/
} /* srotg_ */

#ifdef __cplusplus
        }
#endif
