#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Subroutine */ void qrsolv_(n, r, ldr, ipvt, diag, qtb, x, sdiag, wa)
const integer *n;
doublereal *r;
const integer *ldr, *ipvt;
const doublereal *diag, *qtb;
doublereal *x, *sdiag, *wa;
{
    /* Local variables */
    static doublereal temp;
    static integer i, j, k, l;
    static doublereal cotan;
    static integer nsing;
    static doublereal qtbpj;
    static doublereal tan_, cos_, sin_, sum;

/*     **********                                                     */
/*                                                                    */
/*     subroutine qrsolv                                              */
/*                                                                    */
/*     given an m by n matrix a, an n by n diagonal matrix d,         */
/*     and an m-vector b, the problem is to determine an x which      */
/*     solves the system                                              */
/*                                                                    */
/*           a*x = b ,     d*x = 0 ,                                  */
/*                                                                    */
/*     in the least squares sense.                                    */
/*                                                                    */
/*     this subroutine completes the solution of the problem          */
/*     if it is provided with the necessary information from the      */
/*     qr factorization, with column pivoting, of a. that is, if      */
/*     a*p = q*r, where p is a permutation matrix, q has orthogonal   */
/*     columns, and r is an upper triangular matrix with diagonal     */
/*     elements of nonincreasing magnitude, then qrsolv expects       */
/*     the full upper triangle of r, the permutation matrix p,        */
/*     and the first n components of (q transpose)*b. the system      */
/*     a*x = b, d*x = 0, is then equivalent to                        */
/*                                                                    */
/*                  t       t                                         */
/*           r*z = q *b ,  p *d*p*z = 0 ,                             */
/*                                                                    */
/*     where x = p*z. if this system does not have full rank,         */
/*     then a least squares solution is obtained. on output qrsolv    */
/*     also provides an upper triangular matrix s such that           */
/*                                                                    */
/*            t   t               t                                   */
/*           p *(a *a + d*d)*p = s *s .                               */
/*                                                                    */
/*     s is computed within qrsolv and may be of separate interest.   */
/*                                                                    */
/*     the subroutine statement is                                    */
/*                                                                    */
/*       subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa)          */
/*                                                                    */
/*     where                                                          */
/*                                                                    */
/*       n is a positive integer input variable set to the order of r.*/
/*                                                                    */
/*       r is an n by n array. on input the full upper triangle       */
/*         must contain the full upper triangle of the matrix r.      */
/*         on output the full upper triangle is unaltered, and the    */
/*         strict lower triangle contains the strict upper triangle   */
/*         (transposed) of the upper triangular matrix s.             */
/*                                                                    */
/*       ldr is a positive integer input variable not less than n     */
/*         which specifies the leading dimension of the array r.      */
/*                                                                    */
/*       ipvt is an integer input array of length n which defines the */
/*         permutation matrix p such that a*p = q*r. column j of p    */
/*         is column ipvt(j) of the identity matrix.                  */
/*                                                                    */
/*       diag is an input array of length n which must contain the    */
/*         diagonal elements of the matrix d.                         */
/*                                                                    */
/*       qtb is an input array of length n which must contain the first */
/*         n elements of the vector (q transpose)*b.                  */
/*                                                                    */
/*       x is an output array of length n which contains the least    */
/*         squares solution of the system a*x = b, d*x = 0.           */
/*                                                                    */
/*       sdiag is an output array of length n which contains the      */
/*         diagonal elements of the upper triangular matrix s.        */
/*                                                                    */
/*       wa is a work array of length n.                              */
/*                                                                    */
/*     argonne national laboratory. minpack project. march 1980.      */
/*     burton s. garbow, kenneth e. hillstrom, jorge j. more          */
/*                                                                    */
/*     **********                                                     */
/*                                                                    */
/*     copy r and (q transpose)*b to preserve input and initialize s. */
/*     in particular, save the diagonal elements of r in x.           */

    for (j = 0; j < *n; ++j) {
        for (i = j; i < *n; ++i) {
            r[i + j * *ldr] = r[j + i * *ldr];
        }
        x[j] = r[j + j * *ldr];
        wa[j] = qtb[j];
    }

/*     eliminate the diagonal matrix d using a givens rotation. */

    for (j = 0; j < *n; ++j) {

/*        prepare the row of d to be eliminated, locating the */
/*        diagonal element using p from the qr factorization. */

        l = ipvt[j] - 1;
        if (diag[l] == 0.) {
            goto L90;
        }
        for (k = j; k < *n; ++k) {
            sdiag[k] = 0.;
        }
        sdiag[j] = diag[l];

/*        the transformations to eliminate the row of d */
/*        modify only a single element of (q transpose)*b */
/*        beyond the first n, which is initially zero. */

        qtbpj = 0.;
        for (k = j; k < *n; ++k) {

/*           determine a givens rotation which eliminates the */
/*           appropriate element in the current row of d. */

            if (sdiag[k] == 0.)
                continue;

            if (abs(r[k + k * *ldr]) < abs(sdiag[k])) {
                cotan = r[k + k * *ldr] / sdiag[k];
                sin_ = .5 / sqrt(.25 + .25 * (cotan * cotan));
                cos_ = sin_ * cotan;
            } else {
                tan_ = sdiag[k] / r[k + k * *ldr];
                cos_ = .5 / sqrt(.25 + .25 * (tan_ * tan_));
                sin_ = cos_ * tan_;
            }

/*           compute the modified diagonal element of r and */
/*           the modified element of ((q transpose)*b,0). */

            r[k + k * *ldr] = cos_ * r[k + k * *ldr] + sin_ * sdiag[k];
            temp = cos_ * wa[k] + sin_ * qtbpj;
            qtbpj = -sin_ * wa[k] + cos_ * qtbpj;
            wa[k] = temp;

/*           accumulate the transformation in the row of s. */

            for (i = k+1; i < *n; ++i) {
                temp = cos_ * r[i + k * *ldr] + sin_ * sdiag[i];
                sdiag[i] = -sin_ * r[i + k * *ldr] + cos_ * sdiag[i];
                r[i + k * *ldr] = temp;
            }
        }
L90:

/*        store the diagonal element of s and restore */
/*        the corresponding diagonal element of r. */

        sdiag[j] = r[j + j * *ldr];
        r[j + j * *ldr] = x[j];
    }

/*     solve the triangular system for z. if the system is */
/*     singular, then obtain a least squares solution. */

    nsing = *n;
    for (j = 0; j < *n; ++j) {
        if (sdiag[j] == 0. && nsing == *n) {
            nsing = j;
        }
        if (nsing < *n) {
            wa[j] = 0.;
        }
    }
    for (k = 0; k < nsing; ++k) {
        j = nsing - k - 1;
        sum = 0.;
        for (i = j+1; i < nsing; ++i) {
            sum += r[i + j * *ldr] * wa[i];
        }
        wa[j] = (wa[j] - sum) / sdiag[j];
    }

/*     permute the components of z back to components of x. */

    for (j = 0; j < *n; ++j) {
        l = ipvt[j] - 1;
        x[l] = wa[j];
    }
} /* qrsolv_ */
