/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"


/* *********************************************************************** */

/* Subroutine */ int slager_(n, nband, nstart, a, tmin, tmax)
integer *n, *nband, *nstart;
real *a, *tmin, *tmax;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    real r__1, r__2;

    /* Local variables */
    static real temp;
    static integer i, k, l, m;


/*  THIS SUBROUTINE COMPUTES BOUNDS ON THE SPECTRUM OF A BY */
/*  EXAMINING THE GERSCHGORIN CIRCLES. ONLY THE NEWLY CREATED */
/*  CIRCLES ARE EXAMINED */

/*  FORMAL PARAMETERS */


/*  LOCAL VARIABLES */


/*  FUNCTIONS CALLED */


    /* Parameter adjustments */
    a_dim1 = *nband;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (k = *nstart; k <= i__1; ++k) {
        temp = (float)0.;
        i__2 = *nband;
        for (i = 2; i <= i__2; ++i) {
            temp += (r__1 = a[i + k * a_dim1], dabs(r__1));
/* L10: */
        }
/* L20: */
        l = min(k,*nband);
        if (l == 1) {
            goto L40;
        }
        i__2 = l;
        for (i = 2; i <= i__2; ++i) {
            m = k - i + 1;
            temp += (r__1 = a[i + m * a_dim1], dabs(r__1));
/* L30: */
        }
L40:
/* Computing MIN */
        r__1 = *tmin, r__2 = a[k * a_dim1 + 1] - temp;
        *tmin = dmin(r__1,r__2);
/* Computing MAX */
        r__1 = *tmax, r__2 = a[k * a_dim1 + 1] + temp;
        *tmax = dmax(r__1,r__2);
/* L50: */
    }
    return 0;
} /* slager_ */

