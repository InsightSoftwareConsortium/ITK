#include "f2c.h"
#include "netlib.h"

/* Subroutine */ void dlaswp_(n, a, lda, k1, k2, ipiv, incx)
integer *n;
doublereal *a;
integer *lda, *k1, *k2, *ipiv, *incx;
{
    /* Local variables */
    static doublereal temp;
    static integer i, j, k, i1, i2, n32, ip, ix, ix0, inc;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DLASWP performs a series of row interchanges on the matrix A.         */
/*  One row interchange is initiated for each of rows K1 through K2 of A. */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The number of columns of the matrix A.                        */
/*                                                                        */
/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)      */
/*          On entry, the matrix of column dimension N to which the row   */
/*          interchanges will be applied.                                 */
/*          On exit, the permuted matrix.                                 */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A.                         */
/*                                                                        */
/*  K1      (input) INTEGER                                               */
/*          The first element of IPIV for which a row interchange will    */
/*          be done.                                                      */
/*                                                                        */
/*  K2      (input) INTEGER                                               */
/*          The last element of IPIV for which a row interchange will     */
/*          be done.                                                      */
/*                                                                        */
/*  IPIV    (input) INTEGER array, dimension (M*abs(INCX))                */
/*          The vector of pivot indices.  Only the elements in positions  */
/*          K1 through K2 of IPIV are accessed.                           */
/*          IPIV(K) = L implies rows K and L are to be interchanged.      */
/*                                                                        */
/*  INCX    (input) INTEGER                                               */
/*          The increment between successive values of IPIV.  If IPIV     */
/*          is negative, the pivots are applied in reverse order.         */
/*                                                                        */
/*  Further Details                                                       */
/*  ===============                                                       */
/*                                                                        */
/*  Modified by                                                           */
/*   R. C. Whaley, Computer Science Dept., Univ. of Tenn., Knoxville, USA */
/*                                                                        */
/* =====================================================================  */

/*     Interchange row I with row IPIV(I) for each of rows K1 through K2. */

    if (*incx > 0) {
        i1 = *k1 - 1;
        ix0 = i1;
        i2 = *k2 - 1;
        inc = 1;
    } else if (*incx < 0) {
        i1 = *k2 - 1;
        ix0 = - i1 * *incx;
        i2 = *k1 - 1;
        inc = -1;
    } else {
        return;
    }

    n32 = *n / 32 << 5;
    if (n32 != 0) {
        for (j = 0; j < n32; j += 32) {
            ix = ix0;
            for (i = i1; inc < 0 ? i >= i2 : i <= i2; i += inc) {
                ip = ipiv[ix] - 1;
                if (ip != i) {
                    for (k = j; k < j + 32; ++k) {
                        temp = a[i + k * *lda];
                        a[i + k * *lda] = a[ip + k * *lda];
                        a[ip + k * *lda] = temp;
                    }
                }
                ix += *incx;
            }
        }
    }
    if (n32 != *n) {
        ++n32;
        ix = ix0;
        for (i = i1; inc < 0 ? i >= i2 : i <= i2; i += inc) {
            ip = ipiv[ix] - 1;
            if (ip != i) {
                for (k = n32-1; k < *n; ++k) {
                    temp = a[i + k * *lda];
                    a[i + k * *lda] = a[ip + k * *lda];
                    a[ip + k * *lda] = temp;
                }
            }
            ix += *incx;
        }
    }
} /* dlaswp_ */
