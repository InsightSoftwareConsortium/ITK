/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;


/* ------------------------------------------------------------------ */

/* Subroutine */ int smvpc_(nblock, bet, maxj, j, s, number, resnrm, orthcf, 
	rv)
integer *nblock;
real *bet;
integer *maxj, *j;
real *s;
integer *number;
real *resnrm, *orthcf, *rv;
{
    /* System generated locals */
    integer bet_dim1, bet_offset, s_dim1, s_offset, i__1, i__2;
    real r__1, r__2, r__3;

    /* Local variables */
    extern doublereal sdot_(), snrm2_();
    static integer i, k, m;



/* THIS SUBROUTINE COMPUTES THE NORM AND THE SMALLEST ELEMENT */
/* (IN ABSOLUTE VALUE) OF THE VECTOR BET*SJI, WHERE SJI */
/* IS AN NBLOCK VECTOR OF THE LAST NBLOCK ELEMENTS OF THE ITH */
/* EIGENVECTOR OF T.  THESE QUANTITIES ARE THE RESIDUAL NORM */
/* AND THE ORTHOGONALITY COEFFICIENT RESPECTIVELY FOR THE */
/* CORRESPONDING RITZ PAIR.  THE ORTHOGONALITY COEFFICIENT IS */
/* NORMALIZED TO ACCOUNT FOR THE LOCAL REORTHOGONALIZATION. */


    /* Parameter adjustments */
    --rv;
    --orthcf;
    --resnrm;
    s_dim1 = *maxj;
    s_offset = s_dim1 + 1;
    s -= s_offset;
    bet_dim1 = *nblock;
    bet_offset = bet_dim1 + 1;
    bet -= bet_offset;

    /* Function Body */
    m = *j - *nblock + 1;
    i__1 = *number;
    for (i = 1; i <= i__1; ++i) {
	i__2 = *nblock;
	for (k = 1; k <= i__2; ++k) {
	    rv[k] = sdot_(nblock, &s[m + i * s_dim1], &c__1, &bet[k + 
		    bet_dim1], nblock);
	    if (k == 1) {
		orthcf[i] = (r__1 = rv[k], dabs(r__1));
	    }
/* Computing MIN */
	    r__2 = orthcf[i], r__3 = (r__1 = rv[k], dabs(r__1));
	    orthcf[i] = dmin(r__2,r__3);
/* L10: */
	}
	resnrm[i] = snrm2_(nblock, &rv[1], &c__1);
/* L20: */
    }
    return 0;
} /* smvpc_ */

