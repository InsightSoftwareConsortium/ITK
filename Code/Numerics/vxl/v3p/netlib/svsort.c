/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;


/* ------------------------------------------------------------------- */

/* Subroutine */ int svsort_(num, val, res, iflag, v, nmvec, n, vec)
integer *num;
real *val, *res;
integer *iflag;
real *v;
integer *nmvec, *n;
real *vec;
{
    /* System generated locals */
    integer vec_dim1, vec_offset, i__1, i__2;

    /* Local variables */
    static real temp;
    static integer i, k, m;
    extern /* Subroutine */ int sswap_();


/*  THIS SUBROUTINE SORTS THE EIGENVALUES (VAL) IN ASCENDING ORDER */
/*  WHILE CONCURRENTLY SWAPPING THE RESIDUALS AND VECTORS. */
    /* Parameter adjustments */
    vec_dim1 = *nmvec;
    vec_offset = vec_dim1 + 1;
    vec -= vec_offset;
    --v;
    --res;
    --val;

    /* Function Body */
    if (*num <= 1) {
	return 0;
    }
    i__1 = *num;
    for (i = 2; i <= i__1; ++i) {
	m = *num - i + 1;
	i__2 = m;
	for (k = 1; k <= i__2; ++k) {
	    if (val[k] <= val[k + 1]) {
		goto L10;
	    }
	    temp = val[k];
	    val[k] = val[k + 1];
	    val[k + 1] = temp;
	    temp = res[k];
	    res[k] = res[k + 1];
	    res[k + 1] = temp;
	    sswap_(n, &vec[k * vec_dim1 + 1], &c__1, &vec[(k + 1) * vec_dim1 
		    + 1], &c__1);
	    if (*iflag == 0) {
		goto L10;
	    }
	    temp = v[k];
	    v[k] = v[k + 1];
	    v[k + 1] = temp;
L10:
	    ;
	}
/* L20: */
    }
    return 0;
} /* svsort_ */

