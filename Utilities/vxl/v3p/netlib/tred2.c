#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Subroutine */ void tred2_(nm, n, a, d, e, z)
const integer *nm, *n;
const doublereal *a;
doublereal *d, *e, *z;
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal f, g, h;
    static integer i, j, k, l;
    static doublereal scale, hh;

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
/*     mathematics and computer science div, argonne national laboratory */

/*     this version dated august 1983. */

/*     ------------------------------------------------------------------ */

    for (i = 0; i < *n; ++i) {
        for (j = i; j < *n; ++j) {
            z[j + i * *nm] = a[j + i * *nm];
        }

        d[i] = a[*n-1 + i * *nm];
    }

    for (i = *n-1; i > 0; --i) {
        l = i - 1;
        h = 0.;
        scale = 0.;
/*     .......... scale row (algol tol then not needed) .......... */
        for (k = 0; k <= l; ++k) {
            scale += abs(d[k]);
        }
        if (scale == 0.) {
            e[i] = d[l];
            for (j = 0; j <= l; ++j) {
                d[j] = z[l + j * *nm];
                z[i + j * *nm] = 0.;
                z[j + i * *nm] = 0.;
            }
            goto L290;
        }
        for (k = 0; k <= l; ++k) {
            d[k] /= scale;
            h += d[k] * d[k];
        }

        f = d[l];
        d__1 = sqrt(h);
        g = -d_sign(&d__1, &f);
        e[i] = scale * g;
        h -= f * g;
        d[l] = f - g;
/*     .......... form a*u .......... */
        for (j = 0; j <= l; ++j) {
            e[j] = 0.;
        }

        for (j = 0; j <= l; ++j) {
            f = d[j];
            z[j + i * *nm] = f;
            g = e[j] + z[j + j * *nm] * f;

            for (k = j+1; k <= l; ++k) {
                g += z[k + j * *nm] * d[k];
                e[k] += z[k + j * *nm] * f;
            }

            e[j] = g;
        }
/*     .......... form p .......... */
        f = 0.;

        for (j = 0; j <= l; ++j) {
            e[j] /= h;
            f += e[j] * d[j];
        }

        hh = f / (h + h);
/*     .......... form q .......... */
        for (j = 0; j <= l; ++j) {
            e[j] -= hh * d[j];
        }
/*     .......... form reduced a .......... */
        for (j = 0; j <= l; ++j) {
            f = d[j];
            g = e[j];

            for (k = j; k <= l; ++k) {
                z[k + j * *nm] = z[k + j * *nm] - f * e[k] - g * d[k];
            }

            d[j] = z[l + j * *nm];
            z[i + j * *nm] = 0.;
        }
L290:
        d[i] = h;
    }
/*     .......... accumulation of transformation matrices .......... */
    for (i = 1; i < *n; ++i) {
        l = i - 1;
        z[*n-1 + l * *nm] = z[l + l * *nm];
        z[l + l * *nm] = 1.;
        h = d[i];
        if (h == 0.) {
            goto L380;
        }

        for (k = 0; k <= l; ++k) {
            d[k] = z[k + i * *nm] / h;
        }

        for (j = 0; j <= l; ++j) {
            g = 0.;

            for (k = 0; k <= l; ++k) {
                g += z[k + i * *nm] * z[k + j * *nm];
            }

            for (k = 0; k <= l; ++k) {
                z[k + j * *nm] -= g * d[k];
            }
        }
L380:
        for (k = 0; k <= l; ++k) {
            z[k + i * *nm] = 0.;
        }
    }
    for (i = 0; i < *n; ++i) {
        d[i] = z[*n-1 + i * *nm];
        z[*n-1 + i * *nm] = 0.;
    }

    z[*n-1 + (*n-1) * *nm] = 1.;
    e[0] = 0.;
} /* tred2_ */
