/* tred2.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int tred2_(nm, n, a, d, e, z)
integer *nm, *n;
doublereal *a, *d, *e, *z;
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(), d_sign();

    /* Local variables */
    static doublereal f, g, h;
    static integer i, j, k, l;
    static doublereal scale, hh;
    static integer ii, jp1;



/*     this subroutine is a translation of the algol procedure tred2, */
/*     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). */

/*     this subroutine reduces a real symmetric matrix to a */
/*     symmetric tridiagonal matrix using and accumulating */
/*     orthogonal similarity transformations. */

/*     on input */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement. */

/*        n is the order of the matrix. */

/*        a contains the real symmetric input matrix.  only the */
/*          lower triangle of the matrix need be supplied. */

/*     on output */

/*        d contains the diagonal elements of the tridiagonal matrix. */

/*        e contains the subdiagonal elements of the tridiagonal */
/*          matrix in its last n-1 positions.  e(1) is set to zero. */

/*        z contains the orthogonal transformation matrix */
/*          produced in the reduction. */

/*        a and z may coincide.  if distinct, a is unaltered. */

/*     questions and comments should be directed to burton s. garbow, */
/*     mathematics and computer science div, argonne national laboratory 
*/

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --e;
    --d;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {

	i__2 = *n;
	for (j = i; j <= i__2; ++j) {
/* L80: */
	    z[j + i * z_dim1] = a[j + i * a_dim1];
	}

	d[i] = a[*n + i * a_dim1];
/* L100: */
    }

    if (*n == 1) {
	goto L510;
    }
/*     .......... for i=n step -1 until 2 do -- .......... */
    i__1 = *n;
    for (ii = 2; ii <= i__1; ++ii) {
	i = *n + 2 - ii;
	l = i - 1;
	h = 0.;
	scale = 0.;
	if (l < 2) {
	    goto L130;
	}
/*     .......... scale row (algol tol then not needed) .......... */
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L120: */
	    scale += (d__1 = d[k], abs(d__1));
	}

	if (scale != 0.) {
	    goto L140;
	}
L130:
	e[i] = d[l];

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    d[j] = z[l + j * z_dim1];
	    z[i + j * z_dim1] = 0.;
	    z[j + i * z_dim1] = 0.;
/* L135: */
	}

	goto L290;

L140:
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    d[k] /= scale;
	    h += d[k] * d[k];
/* L150: */
	}

	f = d[l];
	d__1 = sqrt(h);
	g = -d_sign(&d__1, &f);
	e[i] = scale * g;
	h -= f * g;
	d[l] = f - g;
/*     .......... form a*u .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L170: */
	    e[j] = 0.;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d[j];
	    z[j + i * z_dim1] = f;
	    g = e[j] + z[j + j * z_dim1] * f;
	    jp1 = j + 1;
	    if (l < jp1) {
		goto L220;
	    }

	    i__3 = l;
	    for (k = jp1; k <= i__3; ++k) {
		g += z[k + j * z_dim1] * d[k];
		e[k] += z[k + j * z_dim1] * f;
/* L200: */
	    }

L220:
	    e[j] = g;
/* L240: */
	}
/*     .......... form p .......... */
	f = 0.;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    e[j] /= h;
	    f += e[j] * d[j];
/* L245: */
	}

	hh = f / (h + h);
/*     .......... form q .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L250: */
	    e[j] -= hh * d[j];
	}
/*     .......... form reduced a .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d[j];
	    g = e[j];

	    i__3 = l;
	    for (k = j; k <= i__3; ++k) {
/* L260: */
		z[k + j * z_dim1] = z[k + j * z_dim1] - f * e[k] - g * d[k];
	    }

	    d[j] = z[l + j * z_dim1];
	    z[i + j * z_dim1] = 0.;
/* L280: */
	}

L290:
	d[i] = h;
/* L300: */
    }
/*     .......... accumulation of transformation matrices .......... */
    i__1 = *n;
    for (i = 2; i <= i__1; ++i) {
	l = i - 1;
	z[*n + l * z_dim1] = z[l + l * z_dim1];
	z[l + l * z_dim1] = 1.;
	h = d[i];
	if (h == 0.) {
	    goto L380;
	}

	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L330: */
	    d[k] = z[k + i * z_dim1] / h;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    g = 0.;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
/* L340: */
		g += z[k + i * z_dim1] * z[k + j * z_dim1];
	    }

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		z[k + j * z_dim1] -= g * d[k];
/* L360: */
	    }
	}

L380:
	i__3 = l;
	for (k = 1; k <= i__3; ++k) {
/* L400: */
	    z[k + i * z_dim1] = 0.;
	}

/* L500: */
    }

L510:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	d[i] = z[*n + i * z_dim1];
	z[*n + i * z_dim1] = 0.;
/* L520: */
    }

    z[*n + *n * z_dim1] = 1.;
    e[1] = 0.;
    return 0;
} /* tred2_ */

