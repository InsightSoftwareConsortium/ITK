/* srotg.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static real c_b4 = (float)1.;

/* Subroutine */ int srotg_(sa, sb, c, s)
real *sa, *sb, *c, *s;
{
    /* System generated locals */
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(), r_sign();

    /* Local variables */
    static real r, scale, z, roe;


/*     construct givens plane rotation. */
/*     jack dongarra, linpack, 3/11/78. */


    roe = *sb;
    if (dabs(*sa) > dabs(*sb)) {
	roe = *sa;
    }
    scale = dabs(*sa) + dabs(*sb);
    if (scale != (float)0.) {
	goto L10;
    }
    *c = (float)1.;
    *s = (float)0.;
    r = (float)0.;
    z = (float)0.;
    goto L20;
L10:
/* Computing 2nd power */
    r__1 = *sa / scale;
/* Computing 2nd power */
    r__2 = *sb / scale;
    r = scale * sqrt(r__1 * r__1 + r__2 * r__2);
    r = r_sign(&c_b4, &roe) * r;
    *c = *sa / r;
    *s = *sb / r;
    z = (float)1.;
    if (dabs(*sa) > dabs(*sb)) {
	z = *s;
    }
    if (dabs(*sb) >= dabs(*sa) && *c != (float)0.) {
	z = (float)1. / *c;
    }
L20:
    *sa = r;
    *sb = z;
    return 0;
} /* srotg_ */

