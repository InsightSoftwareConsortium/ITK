/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int slaeig_(n, nband, nl, nr, a, eigval, lde, eigvec, bound, 
	atemp, d, vtemp, eps, tmin, tmax)
integer *n, *nband, *nl, *nr;
real *a, *eigval;
integer *lde;
real *eigvec, *bound, *atemp, *d, *vtemp, *eps, *tmin, *tmax;
{
    /* System generated locals */
    integer a_dim1, a_offset, eigvec_dim1, eigvec_offset, i__1;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static real atol;
    static integer nval, i, m;
    static real artol;
    extern /* Subroutine */ int slabcm_();


/*  THIS IS A SPECIALIZED VERSION OF THE SUBROUTINE BNDEIG TAILORED */
/*  SPECIFICALLY FOR USE BY THE LASO PACKAGE. */


/*  LOCAL VARIABLES */


/*  FUNCTIONS CALLED */


/*  SUBROUTINES CALLED */

/*     SLABCM, SLABFC, SLAGER, SCOPY */

/*  SET PARAMETERS */

    /* Parameter adjustments */
    --vtemp;
    --d;
    --atemp;
    bound -= 3;
    eigvec_dim1 = *lde;
    eigvec_offset = eigvec_dim1 + 1;
    eigvec -= eigvec_offset;
    --eigval;
    a_dim1 = *nband;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
/* Computing MAX */
    r__1 = *tmax, r__2 = -(doublereal)(*tmin);
    atol = (real) (*n) * *eps * dmax(r__1,r__2);
    artol = atol / sqrt(*eps);
    nval = *nr - *nl + 1;

/*   CHECK FOR SPECIAL CASE OF N = 1 */

    if (*n != 1) {
	goto L30;
    }
    eigval[1] = a[a_dim1 + 1];
    eigvec[eigvec_dim1 + 1] = (float)1.;
    return 0;

/*   SET UP INITIAL EIGENVALUE BOUNDS */

L30:
    m = nval + 1;
    i__1 = m;
    for (i = 2; i <= i__1; ++i) {
	bound[(i << 1) + 1] = *tmin;
	bound[(i << 1) + 2] = *tmax;
/* L50: */
    }
    bound[4] = *tmax;
    bound[(nval + 2 << 1) + 1] = *tmin;
    if (*nl == 1) {
	bound[4] = *tmin;
    }
    if (*nr == *n) {
	bound[(nval + 2 << 1) + 1] = *tmax;
    }

/* L60: */
    slabcm_(n, nband, nl, nr, &a[a_offset], &eigval[1], lde, &eigvec[
	    eigvec_offset], &atol, &artol, &bound[3], &atemp[1], &d[1], &
	    vtemp[1]);
    return 0;
} /* slaeig_ */

