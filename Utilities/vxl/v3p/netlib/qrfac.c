#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Modified by Peter Vanroose, June 2003: manual optimisation and clean-up */

/* Table of constant values */
static integer c__1 = 1;

/* Subroutine */ void qrfac_(m, n, a, lda, pivot, ipvt, lipvt, rdiag, acnorm, wa)
integer *m, *n;
doublereal *a;
integer *lda;
logical *pivot;
integer *ipvt, *lipvt;
doublereal *rdiag, *acnorm, *wa;
{
    /* Initialized data */
    static doublereal one = 1.;
    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    static integer kmax;
    static doublereal temp;
    static integer i, j, k, minmn;
    static doublereal epsmch;
    static doublereal ajnorm;
    static integer jp1;
    static doublereal sum;
    static integer failure = 44;

/*     **********                                                       */
/*                                                                      */
/*     subroutine qrfac                                                 */
/*                                                                      */
/*     this subroutine uses householder transformations with column     */
/*     pivoting (optional) to compute a qr factorization of the         */
/*     m by n matrix a. that is, qrfac determines an orthogonal         */
/*     matrix q, a permutation matrix p, and an upper trapezoidal       */
/*     matrix r with diagonal elements of nonincreasing magnitude,      */
/*     such that a*p = q*r. the householder transformation for          */
/*     column k, k = 1,2,...,min(m,n), is of the form                   */
/*                                                                      */
/*                           t                                          */
/*           i - (1/u(k))*u*u                                           */
/*                                                                      */
/*     where u has zeros in the first k-1 positions. the form of        */
/*     this transformation and the method of pivoting first             */
/*     appeared in the corresponding linpack subroutine.                */
/*                                                                      */
/*     the subroutine statement is                                      */
/*                                                                      */
/*       subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)   */
/*                                                                      */
/*     where                                                            */
/*                                                                      */
/*       m is a positive integer input variable set to the number       */
/*         of rows of a.                                                */
/*                                                                      */
/*       n is a positive integer input variable set to the number       */
/*         of columns of a.                                             */
/*                                                                      */
/*       a is an m by n array. on input a contains the matrix for       */
/*         which the qr factorization is to be computed. on output      */
/*         the strict upper trapezoidal part of a contains the strict   */
/*         upper trapezoidal part of r, and the lower trapezoidal       */
/*         part of a contains a factored form of q (the non-trivial     */
/*         elements of the u vectors described above).                  */
/*                                                                      */
/*       lda is a positive integer input variable not less than m       */
/*         which specifies the leading dimension of the array a.        */
/*                                                                      */
/*       pivot is a logical input variable. if pivot is set true,       */
/*         then column pivoting is enforced. if pivot is set false,     */
/*         then no column pivoting is done.                             */
/*                                                                      */
/*       ipvt is an integer output array of length lipvt. ipvt          */
/*         defines the permutation matrix p such that a*p = q*r.        */
/*         column j of p is column ipvt(j) of the identity matrix.      */
/*         if pivot is false, ipvt is not referenced.                   */
/*                                                                      */
/*       lipvt is a positive integer input variable. if pivot is false, */
/*         then lipvt may be as small as 1. if pivot is true, then      */
/*         lipvt must be at least n.                                    */
/*                                                                      */
/*       rdiag is an output array of length n which contains the        */
/*         diagonal elements of r.                                      */
/*                                                                      */
/*       acnorm is an output array of length n which contains the       */
/*         norms of the corresponding columns of the input matrix a.    */
/*         if this information is not needed, then acnorm can coincide  */
/*         with rdiag.                                                  */
/*                                                                      */
/*       wa is a work array of length n. if pivot is false, then wa     */
/*         can coincide with rdiag.                                     */

/*     argonne national laboratory. minpack project. march 1980.        */
/*     burton s. garbow, kenneth e. hillstrom, jorge j. more            */
/*                                                                      */
/*     **********                                                       */

    /* epsmch is the machine precision. */
    epsmch = dpmpar_(&c__1);

    /* verify that the size of ipvt is at least n */
    if (lipvt < n)
        exit_(&failure);

    /* compute the initial column norms and initialize several arrays. */
    for (j = 0; j < *n; ++j) {
        acnorm[j] = enorm_(m, &a[j* *lda]);
        rdiag[j] = acnorm[j];
        wa[j] = rdiag[j];
        if (*pivot) {
            ipvt[j] = j+1;
        }
    }

    /* reduce a to r with householder transformations. */
    minmn = min(*m,*n);
    for (j = 0; j < minmn; ++j) {
        if (! (*pivot)) {
            goto L40;
        }

        /* bring the column of largest norm into the pivot position. */
        kmax = j;
        for (k = j; k < *n; ++k) {
            if (rdiag[k] > rdiag[kmax]) {
                kmax = k;
            }
        }
        if (kmax == j) {
            goto L40;
        }
        for (i = 0; i < *m; ++i) {
            temp = a[i + j* *lda];
            a[i + j* *lda] = a[i + kmax* *lda];
            a[i + kmax* *lda] = temp;
        }
        rdiag[kmax] = rdiag[j];
        wa[kmax] = wa[j];
        k = ipvt[j];
        ipvt[j] = ipvt[kmax];
        ipvt[kmax] = k;
L40:

        /* compute the householder transformation to reduce the */
        /* j-th column of a to a multiple of the j-th unit vector. */
        i__1 = *m - j;
        ajnorm = enorm_(&i__1, &a[j + j* *lda]);
        if (ajnorm == zero) {
            goto L100;
        }
        if (a[j + j* *lda] < zero) {
            ajnorm = -ajnorm;
        }
        for (i = j; i < *m; ++i) {
            a[i + j* *lda] /= ajnorm;
        }
        a[j + j* *lda] += one;

        /* apply the transformation to the remaining columns */
        /* and update the norms. */
        jp1 = j+1;
        for (k = jp1; k < *n; ++k) {
            sum = zero;
            for (i = j; i < *m; ++i) {
                sum += a[i + j* *lda] * a[i + k* *lda];
            }
            temp = sum / a[j + j* *lda];
            for (i = j; i < *m; ++i) {
                a[i + k* *lda] -= temp * a[i + j* *lda];
            }
            if (! (*pivot) || rdiag[k] == zero) {
                continue;
            }
            temp = a[j + k* *lda] / rdiag[k];
            rdiag[k] *= sqrt((max(zero, one - temp * temp)));
            d__1 = rdiag[k] / wa[k];
            if (d__1 * d__1 > 20*epsmch) {
                continue;
            }
            i__1 = *m - jp1;
            rdiag[k] = enorm_(&i__1, &a[jp1 + k* *lda]);
            wa[k] = rdiag[k];
        }
L100:
        rdiag[j] = -ajnorm;
    }
} /* qrfac_ */
