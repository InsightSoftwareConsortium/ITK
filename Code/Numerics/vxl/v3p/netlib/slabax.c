/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;


/* *********************************************************************** */

/* Subroutine */ int slabax_(n, nband, a, x, y)
integer *n, *nband;
real *a, *x, *y;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    static real zero[1];
    static integer i, k, l, m;
    extern /* Subroutine */ int scopy_();


/*  THIS SUBROUTINE SETS Y = A*X */
/*  WHERE X AND Y ARE VECTORS OF LENGTH N */
/*  AND A IS AN  N X NBAND  SYMMETRIC BAND MATRIX */

/*  FORMAL PARAMETERS */


/*  LOCAL VARIABLES */


/*  FUNCTIONS CALLED */


/*  SUBROUTINES CALLED */

/*     SCOPY */

    /* Parameter adjustments */
    --y;
    --x;
    a_dim1 = *nband;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    zero[0] = (float)0.;
    scopy_(n, zero, &c__0, &y[1], &c__1);
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
        y[k] += a[k * a_dim1 + 1] * x[k];
/* Computing MIN */
        i__2 = *n - k + 1;
        m = min(i__2,*nband);
        if (m < 2) {
            goto L20;
        }
        i__2 = m;
        for (i = 2; i <= i__2; ++i) {
            l = k + i - 1;
            y[l] += a[i + k * a_dim1] * x[k];
            y[k] += a[i + k * a_dim1] * x[l];
/* L10: */
        }
L20:
        ;
    }
    return 0;
} /* slabax_ */

