#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Subroutine */ void tred1_(nm, n, a, d, e, e2)
const integer *nm, *n;
doublereal *a, *d, *e, *e2;
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal f, g, h;
    static integer i, j, k, l;
    static doublereal scale;

/*     this subroutine is a translation of the algol procedure tred1,     */
/*     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson.    */
/*     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).    */

/*     this subroutine reduces a real symmetric matrix                    */
/*     to a symmetric tridiagonal matrix using                            */
/*     orthogonal similarity transformations.                             */
/*                                                                        */
/*     on input                                                           */
/*                                                                        */
/*        nm must be set to the row dimension of two-dimensional          */
/*          array parameters as declared in the calling program           */
/*          dimension statement.                                          */
/*                                                                        */
/*        n is the order of the matrix.                                   */
/*                                                                        */
/*        a contains the real symmetric input matrix.  only the           */
/*          lower triangle of the matrix need be supplied.                */
/*                                                                        */
/*     on output                                                          */
/*                                                                        */
/*        a contains information about the orthogonal trans-              */
/*          formations used in the reduction in its strict lower          */
/*          triangle.  the full upper triangle of a is unaltered.         */
/*                                                                        */
/*        d contains the diagonal elements of the tridiagonal matrix.     */
/*                                                                        */
/*        e contains the subdiagonal elements of the tridiagonal          */
/*          matrix in its last n-1 positions.  e(1) is set to zero.       */
/*                                                                        */
/*        e2 contains the squares of the corresponding elements of e.     */
/*          e2 may coincide with e if the squares are not needed.         */
/*                                                                        */
/*     questions and comments should be directed to burton s. garbow,     */
/*     mathematics and computer science div, argonne national laboratory  */
/*                                                                        */
/*     this version dated august 1983.                                    */
/*                                                                        */
/*     ------------------------------------------------------------------ */

    for (i = 0; i < *n; ++i) {
        d[i] = a[*n-1 + i * *nm];
        a[*n-1 + i * *nm] = a[i + i * *nm];
    }
    for (i = *n-1; i >= 0; --i) {
        l = i - 1;
        h = 0.;
        scale = 0.;
/*     .......... scale row (algol tol then not needed) .......... */
        for (k = 0; k <= l; ++k) {
            scale += abs(d[k]);
        }
        if (scale == 0.) {
            for (j = 0; j <= l; ++j) {
                d[j] = a[l + j * *nm];
                a[l + j * *nm] = a[i + j * *nm];
                a[i + j * *nm] = 0.;
            }
            e[i] = 0.;
            e2[i] = 0.;
            continue;
        }
        for (k = 0; k <= l; ++k) {
            d[k] /= scale;
            h += d[k] * d[k];
        }

        e2[i] = scale * scale * h;
        f = d[l];
        d__1 = sqrt(h);
        g = -d_sign(&d__1, &f);
        e[i] = scale * g;
        h -= f * g;
        d[l] = f - g;
        if (l == 0) {
            goto L285;
        }
/*     .......... form a*u .......... */
        for (j = 0; j <= l; ++j) {
            e[j] = 0.;
        }

        for (j = 0; j <= l; ++j) {
            f = d[j];
            g = e[j] + a[j + j * *nm] * f;

            for (k = j+1; k <= l; ++k) {
                g += a[k + j * *nm] * d[k];
                e[k] += a[k + j * *nm] * f;
            }
            e[j] = g;
        }
/*     .......... form p .......... */
        f = 0.;

        for (j = 0; j <= l; ++j) {
            e[j] /= h;
            f += e[j] * d[j];
        }

        h = f / (h + h);
/*     .......... form q .......... */
        for (j = 0; j <= l; ++j) {
            e[j] -= h * d[j];
        }
/*     .......... form reduced a .......... */
        for (j = 0; j <= l; ++j) {
            f = d[j];
            g = e[j];

            for (k = j; k <= l; ++k) {
                a[k + j * *nm] = a[k + j * *nm] - f * e[k] - g * d[k];
            }
        }
L285:
        for (j = 0; j <= l; ++j) {
            f = d[j];
            d[j] = a[l + j * *nm];
            a[l + j * *nm] = a[i + j * *nm];
            a[i + j * *nm] = f * scale;
        }
    }
} /* tred1_ */
