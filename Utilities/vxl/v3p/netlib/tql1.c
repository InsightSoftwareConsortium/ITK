#include "f2c.h"
#include "netlib.h"

/* Table of constant values */
static doublereal c_b10 = 1.;

/* Subroutine */ void tql1_(n, d, e, ierr)
const integer *n;
doublereal *d, *e;
integer *ierr;
{
    /* Local variables */
    static doublereal c, f, g, h;
    static integer i, j, l, m;
    static doublereal p, r, s, c2, c3;
    static doublereal s2;
    static doublereal dl1, el1;
    static doublereal tst1, tst2;

/*     this subroutine is a translation of the algol procedure tql1,      */
/*     num. math. 11, 293-306(1968) by bowdler, martin, reinsch, and      */
/*     wilkinson.                                                         */
/*     handbook for auto. comp., vol.ii-linear algebra, 227-240(1971).    */
/*                                                                        */
/*     this subroutine finds the eigenvalues of a symmetric               */
/*     tridiagonal matrix by the ql method.                               */
/*                                                                        */
/*     on input                                                           */
/*                                                                        */
/*        n is the order of the matrix.                                   */
/*                                                                        */
/*        d contains the diagonal elements of the input matrix.           */
/*                                                                        */
/*        e contains the subdiagonal elements of the input matrix         */
/*          in its last n-1 positions.  e(1) is arbitrary.                */
/*                                                                        */
/*      on output                                                         */
/*                                                                        */
/*        d contains the eigenvalues in ascending order.  if an           */
/*          error exit is made, the eigenvalues are correct and           */
/*          ordered for indices 1,2,...ierr-1, but may not be             */
/*          the smallest eigenvalues.                                     */
/*                                                                        */
/*        e has been destroyed.                                           */
/*                                                                        */
/*        ierr is set to                                                  */
/*          zero       for normal return,                                 */
/*          j          if the j-th eigenvalue has not been                */
/*                     determined after 30 iterations.                    */
/*                                                                        */
/*     calls pythag for  sqrt(a*a + b*b) .                                */
/*                                                                        */
/*     questions and comments should be directed to burton s. garbow,     */
/*     mathematics and computer science div, argonne national laboratory  */
/*                                                                        */
/*     this version dated august 1983.                                    */
/*                                                                        */
/*     ------------------------------------------------------------------ */

    *ierr = 0;
    if (*n == 1) {
         return;
    }

    for (i = 1; i < *n; ++i) {
        e[i-1] = e[i];
    }

    f = 0.;
    tst1 = 0.;
    e[*n-1] = 0.;

    for (l = 0; l < *n; ++l) {
        j = 0;
        h = abs(d[l]) + abs(e[l]);
        if (tst1 < h) {
            tst1 = h;
        }
/*     .......... look for small sub-diagonal element .......... */
        for (m = l; m < *n; ++m) {
            tst2 = tst1 + abs(e[m]);
            if (tst2 == tst1) {
                break;
            }
/*     .......... e(n) is always zero, so there is no exit */
/*                through the bottom of the loop .......... */
        }

        if (m == l) {
            goto L210;
        }
L130:
        if (j == 30) {
/*     .......... set error -- no convergence to an */
/*                eigenvalue after 30 iterations .......... */
            *ierr = l+1;
            return;
        }
        ++j;
/*     .......... form shift .......... */
        g = d[l];
        p = (d[l+1] - g) / (e[l] * 2.);
        r = pythag_(&p, &c_b10);
        d[l] = e[l] / (p + d_sign(&r, &p));
        d[l+1] = e[l] * (p + d_sign(&r, &p));
        dl1 = d[l+1];
        h = g - d[l];

        for (i = l+2; i < *n; ++i) {
            d[i] -= h;
        }

        f += h;
/*     .......... ql transformation .......... */
        p = d[m];
        c = 1.;
        c2 = c;
        el1 = e[l+1];
        s = 0.;
        for (i = m-1; i >= l; --i) {
            c3 = c2;
            c2 = c;
            s2 = s;
            g = c * e[i];
            h = c * p;
            r = pythag_(&p, &e[i]);
            e[i+1] = s * r;
            s = e[i] / r;
            c = p / r;
            p = c * d[i] - s * g;
            d[i+1] = h + s * (c * g + s * d[i]);
        }

        p = -s * s2 * c3 * el1 * e[l] / dl1;
        e[l] = s * p;
        d[l] = c * p;
        tst2 = tst1 + abs(e[l]);
        if (tst2 > tst1) {
            goto L130;
        }
L210:
        p = d[l] + f;
/*     .......... order eigenvalues .......... */
        for (i = l; i > 0; --i) {
            if (p >= d[i-1])
                break;
            d[i] = d[i-1];
        }
        d[i] = p;
    }
} /* tql1_ */
