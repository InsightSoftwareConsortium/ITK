/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;


/* ------------------------------------------------------------------ */

/* Subroutine */ int snppla_(op, iovect, n, nperm, nop, nmval, val, nmvec, 
	vec, nblock, h, hv, p, q, bound, d, delta, small, raritz, eps)
/* Subroutine */ int (*op) (), (*iovect) ();
integer *n, *nperm, *nop, *nmval;
real *val;
integer *nmvec;
real *vec;
integer *nblock;
real *h, *hv, *p, *q, *bound, *d, *delta;
logical *small, *raritz;
real *eps;
{
    /* System generated locals */
    integer val_dim1, val_offset, vec_dim1, vec_offset, h_dim1, h_offset, 
	    hv_dim1, hv_offset, p_dim1, p_offset, q_dim1, q_offset, i__1, 
	    i__2, i__3, i__4;
    real r__1;

    /* Local variables */
    static real hmin, hmax, temp;
    extern doublereal sdot_();
    static real zero[1];
    extern doublereal snrm2_();
    static integer i, j, k, l, m;
    extern /* Subroutine */ int scopy_(), saxpy_();
    static integer jj, kk;
    extern /* Subroutine */ int slaeig_(), slager_();



/* THIS SUBROUTINE POST PROCESSES THE EIGENVECTORS.  BLOCK MATRIX */
/* VECTOR PRODUCTS ARE USED TO MINIMIZED THE NUMBER OF CALLS TO OP. */


/* IF RARITZ IS .TRUE.  A FINAL RAYLEIGH-RITZ PROCEDURE IS APPLIED */
/* TO THE EIGENVECTORS. */

    /* Parameter adjustments */
    --d;
    --bound;
    q_dim1 = *n;
    q_offset = q_dim1 + 1;
    q -= q_offset;
    p_dim1 = *n;
    p_offset = p_dim1 + 1;
    p -= p_offset;
    hv_dim1 = *nperm;
    hv_offset = hv_dim1 + 1;
    hv -= hv_offset;
    h_dim1 = *nperm;
    h_offset = h_dim1 + 1;
    h -= h_offset;
    vec_dim1 = *nmvec;
    vec_offset = vec_dim1 + 1;
    vec -= vec_offset;
    val_dim1 = *nmval;
    val_offset = val_dim1 + 1;
    val -= val_offset;

    /* Function Body */
    zero[0] = (float)0.;
    if (! (*raritz)) {
	goto L190;
    }

/* ------------------------------------------------------------------ */

/* THIS CONSTRUCTS H=Q*AQ, WHERE THE COLUMNS OF Q ARE THE */
/* APPROXIMATE EIGENVECTORS.  TEMP = -1 IS USED WHEN SMALL IS */
/* FALSE TO AVOID HAVING TO RESORT THE EIGENVALUES AND EIGENVECTORS */
/* COMPUTED BY SLAEIG. */

    i__1 = *nperm * *nperm;
    scopy_(&i__1, zero, &c__0, &h[h_offset], &c__1);
    temp = (float)-1.;
    if (*small) {
	temp = (float)1.;
    }
    m = *nperm % *nblock;
    if (m == 0) {
	goto L40;
    }
    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
	scopy_(n, &vec[i * vec_dim1 + 1], &c__1, &p[i * p_dim1 + 1], &c__1);
/* L10: */
    }
    (*iovect)(n, &m, &p[p_offset], &m, &c__0);
    (*op)(n, &m, &p[p_offset], &q[q_offset]);
    ++(*nop);
    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
	i__2 = *nperm;
	for (j = i; j <= i__2; ++j) {
	    jj = j - i + 1;
	    h[jj + i * h_dim1] = temp * sdot_(n, &vec[j * vec_dim1 + 1], &
		    c__1, &q[i * q_dim1 + 1], &c__1);
/* L20: */
	}
/* L30: */
    }
    if (*nperm < *nblock) {
	goto L90;
    }
L40:
    m += *nblock;
    i__1 = *nperm;
    i__2 = *nblock;
    for (i = m; i__2 < 0 ? i >= i__1 : i <= i__1; i += i__2) {
	i__3 = *nblock;
	for (j = 1; j <= i__3; ++j) {
	    l = i - *nblock + j;
	    scopy_(n, &vec[l * vec_dim1 + 1], &c__1, &p[j * p_dim1 + 1], &
		    c__1);
/* L50: */
	}
	(*iovect)(n, nblock, &p[p_offset], &i, &c__0);
	(*op)(n, nblock, &p[p_offset], &q[q_offset]);
	++(*nop);
	i__3 = *nblock;
	for (j = 1; j <= i__3; ++j) {
	    l = i - *nblock + j;
	    i__4 = *nperm;
	    for (k = l; k <= i__4; ++k) {
		kk = k - l + 1;
		h[kk + l * h_dim1] = temp * sdot_(n, &vec[k * vec_dim1 + 1], &
			c__1, &q[j * q_dim1 + 1], &c__1);
/* L60: */
	    }
/* L70: */
	}
/* L80: */
    }

/* THIS COMPUTES THE SPECTRAL DECOMPOSITION OF H. */

L90:
    hmin = h[h_dim1 + 1];
    hmax = h[h_dim1 + 1];
    slager_(nperm, nperm, &c__1, &h[h_offset], &hmin, &hmax);
    slaeig_(nperm, nperm, &c__1, nperm, &h[h_offset], &val[val_offset], nperm,
	     &hv[hv_offset], &bound[1], &p[p_offset], &d[1], &q[q_offset], 
	    eps, &hmin, &hmax);

/* THIS COMPUTES THE RITZ VECTORS--THE COLUMNS OF */
/* Y = QS WHERE S IS THE MATRIX OF EIGENVECTORS OF H. */

    i__2 = *nperm;
    for (i = 1; i <= i__2; ++i) {
	scopy_(n, zero, &c__0, &vec[i * vec_dim1 + 1], &c__1);
/* L120: */
    }
    m = *nperm % *nblock;
    if (m == 0) {
	goto L150;
    }
    (*iovect)(n, &m, &p[p_offset], &m, &c__1);
    i__2 = m;
    for (i = 1; i <= i__2; ++i) {
	i__1 = *nperm;
	for (j = 1; j <= i__1; ++j) {
	    saxpy_(n, &hv[i + j * hv_dim1], &p[i * p_dim1 + 1], &c__1, &vec[j 
		    * vec_dim1 + 1], &c__1);
/* L130: */
	}
/* L140: */
    }
    if (*nperm < *nblock) {
	goto L190;
    }
L150:
    m += *nblock;
    i__2 = *nperm;
    i__1 = *nblock;
    for (i = m; i__1 < 0 ? i >= i__2 : i <= i__2; i += i__1) {
	(*iovect)(n, nblock, &p[p_offset], &i, &c__1);
	i__3 = *nblock;
	for (j = 1; j <= i__3; ++j) {
	    l = i - *nblock + j;
	    i__4 = *nperm;
	    for (k = 1; k <= i__4; ++k) {
		saxpy_(n, &hv[l + k * hv_dim1], &p[j * p_dim1 + 1], &c__1, &
			vec[k * vec_dim1 + 1], &c__1);
/* L160: */
	    }
/* L170: */
	}
/* L180: */
    }

/* ------------------------------------------------------------------ */

/* THIS SECTION COMPUTES THE RAYLEIGH QUOTIENTS (IN VAL(*,1)) */
/* AND RESIDUAL NORMS (IN VAL(*,2)) OF THE EIGENVECTORS. */

L190:
    if (! (*small)) {
	*delta = -(doublereal)(*delta);
    }
    m = *nperm % *nblock;
    if (m == 0) {
	goto L220;
    }
    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
	scopy_(n, &vec[i * vec_dim1 + 1], &c__1, &p[i * p_dim1 + 1], &c__1);
/* L200: */
    }
    (*op)(n, &m, &p[p_offset], &q[q_offset]);
    ++(*nop);
    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
	val[i + val_dim1] = sdot_(n, &p[i * p_dim1 + 1], &c__1, &q[i * q_dim1 
		+ 1], &c__1);
	r__1 = -(doublereal)val[i + val_dim1];
	saxpy_(n, &r__1, &p[i * p_dim1 + 1], &c__1, &q[i * q_dim1 + 1], &c__1)
		;
	val[i + (val_dim1 << 1)] = snrm2_(n, &q[i * q_dim1 + 1], &c__1);
/* L210: */
    }
    if (*nperm < *nblock) {
	goto L260;
    }
L220:
    ++m;
    i__1 = *nperm;
    i__2 = *nblock;
    for (i = m; i__2 < 0 ? i >= i__1 : i <= i__1; i += i__2) {
	i__3 = *nblock;
	for (j = 1; j <= i__3; ++j) {
	    l = i - 1 + j;
	    scopy_(n, &vec[l * vec_dim1 + 1], &c__1, &p[j * p_dim1 + 1], &
		    c__1);
/* L230: */
	}
	(*op)(n, nblock, &p[p_offset], &q[q_offset]);
	++(*nop);
	i__3 = *nblock;
	for (j = 1; j <= i__3; ++j) {
	    l = i - 1 + j;
	    val[l + val_dim1] = sdot_(n, &p[j * p_dim1 + 1], &c__1, &q[j * 
		    q_dim1 + 1], &c__1);
	    r__1 = -(doublereal)val[l + val_dim1];
	    saxpy_(n, &r__1, &p[j * p_dim1 + 1], &c__1, &q[j * q_dim1 + 1], &
		    c__1);
	    val[l + (val_dim1 << 1)] = snrm2_(n, &q[j * q_dim1 + 1], &c__1);
/* L240: */
	}
/* L250: */
    }

/* THIS COMPUTES THE ACCURACY ESTIMATES.  FOR CONSISTENCY WITH SILASO */
/* A DO LOOP IS NOT USED. */

L260:
    i = 0;
L270:
    ++i;
    if (i > *nperm) {
	return 0;
    }
    temp = *delta - val[i + val_dim1];
    if (! (*small)) {
	temp = -(doublereal)temp;
    }
    val[i + (val_dim1 << 2)] = (float)0.;
    if (temp > (float)0.) {
	val[i + (val_dim1 << 2)] = val[i + (val_dim1 << 1)] / temp;
    }
    val[i + val_dim1 * 3] = val[i + (val_dim1 << 2)] * val[i + (val_dim1 << 1)
	    ];
    goto L270;

} /* snppla_ */

