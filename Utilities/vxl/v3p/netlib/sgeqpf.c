#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Table of constant values */
static integer c__1 = 1;

/* Subroutine */ void sgeqpf_(integer *m, integer *n, real *a, integer *lda,
        integer *jpvt, real *tau, real *work, integer *info)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static real temp, temp2;
    static integer i, j;
    static integer itemp;
    static integer ma, mn;
    static real aii;
    static integer pvt;

/*  -- LAPACK test routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     March 31, 1993 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  SGEQPF computes a QR factorization with column pivoting of a          */
/*  real M-by-N matrix A: A*P = Q*R.                                      */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  M       (input) INTEGER                                               */
/*          The number of rows of the matrix A. M >= 0.                   */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The number of columns of the matrix A. N >= 0                 */
/*                                                                        */
/*  A       (input/output) REAL array, dimension (LDA,N)                  */
/*          On entry, the M-by-N matrix A.                                */
/*          On exit, the upper triangle of the array contains the         */
/*          min(M,N)-by-N upper triangular matrix R; the elements         */
/*          below the diagonal, together with the array TAU,              */
/*          represent the orthogonal matrix Q as a product of             */
/*          min(m,n) elementary reflectors.                               */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A. LDA >= max(1,M).        */
/*                                                                        */
/*  JPVT    (input/output) INTEGER array, dimension (N)                   */
/*          On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted */
/*          to the front of A*P (a leading column); if JPVT(i) = 0,       */
/*          the i-th column of A is a free column.                        */
/*          On exit, if JPVT(i) = k, then the i-th column of A*P          */
/*          was the k-th column of A.                                     */
/*                                                                        */
/*  TAU     (output) REAL array, dimension (min(M,N))                     */
/*          The scalar factors of the elementary reflectors.              */
/*                                                                        */
/*  WORK    (workspace) REAL array, dimension (3*N)                       */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0:  successful exit                                         */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value    */
/*                                                                        */
/*  Further Details                                                       */
/*  ===============                                                       */
/*                                                                        */
/*  The matrix Q is represented as a product of elementary reflectors     */
/*                                                                        */
/*     Q = H(1) H(2) . . . H(n)                                           */
/*                                                                        */
/*  Each H(i) has the form                                                */
/*                                                                        */
/*     H = I - tau * v * v'                                               */
/*                                                                        */
/*  where tau is a real scalar, and v is a real vector with               */
/*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i).  */
/*                                                                        */
/*  The matrix P is represented in jpvt as follows: If                    */
/*     jpvt(j) = i                                                        */
/*  then the jth column of P is the ith canonical unit vector.            */
/*                                                                        */
/*  ===================================================================== */

/*     Test the input arguments */

    *info = 0;
    if (*m < 0) {
        *info = -1;
    } else if (*n < 0) {
        *info = -2;
    } else if (*lda < max(1,*m)) {
        *info = -4;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("SGEQPF", &i__1);
        return;
    }

    mn = min(*m,*n);

/*     Move initial columns up front */

    itemp = 0;
    for (i = 0; i < *n; ++i) {
        if (jpvt[i] != 0) {
            if (i != itemp) {
                sswap_(m, &a[i * *lda], &c__1, &a[itemp * *lda], &c__1);
                jpvt[i] = jpvt[itemp];
                jpvt[itemp] = i+1;
            } else {
                jpvt[i] = i+1;
            }
            ++itemp;
        } else {
            jpvt[i] = i+1;
        }
    }
    --itemp;

/*     Compute the QR factorization and update remaining columns */

    if (itemp >= 0) {
        ma = min(itemp+1,*m);
        sgeqr2_(m, &ma, a, lda, tau, work, info);
        if (ma < *n) {
            i__1 = *n - ma;
            sorm2r_("Left", "Transpose", m, &i__1, &ma, a, lda, tau, &a[ma * *lda], lda, work, info);
        }
    }

    if (itemp < mn-1) {

/*        Initialize partial column norms. The first n elements of */
/*        work store the exact column norms. */

        for (i = itemp + 1; i < *n; ++i) {
            i__1 = *m - itemp - 1;
            work[i] = snrm2_(&i__1, &a[itemp + 1 + i * *lda], &c__1);
            work[*n + i] = work[i];
        }

/*        Compute factorization */

        for (i = itemp + 1; i < mn; ++i) {

/*           Determine ith pivot column and swap if necessary */

            i__1 = *n - i;
            pvt = i - 1 + isamax_(&i__1, &work[i], &c__1);

            if (pvt != i) {
                sswap_(m, &a[pvt * *lda], &c__1, &a[i * *lda], &c__1);
                itemp = jpvt[pvt];
                jpvt[pvt] = jpvt[i];
                jpvt[i] = itemp;
                work[pvt] = work[i];
                work[*n + pvt] = work[*n + i];
            }

/*           Generate elementary reflector H(i) */

            if (i < *m - 1) {
                i__1 = *m - i;
                slarfg_(&i__1, &a[i + i * *lda], &a[i + 1 + i * *lda], &c__1, &tau[i]);
            } else {
                i__1 = *m - 1;
                slarfg_(&c__1, &a[i__1 + i__1 * *lda], &a[i__1 + i__1 * *lda], &c__1, &tau[i__1]);
            }

            if (i < *n - 1) {

/*              Apply H(i) to A(i:m,i+1:n) from the left */

                aii = a[i + i * *lda];
                a[i + i * *lda] = 1.f;
                i__1 = *m - i;
                i__2 = *n - i - 1;
                slarf_("LEFT", &i__1, &i__2, &a[i + i * *lda], &c__1, &tau[i],
                       &a[i + (i + 1) * *lda], lda, &work[*n << 1]);
                a[i + i * *lda] = aii;
            }

/*           Update partial column norms */

            for (j = i + 1; j < *n; ++j) {
                if (work[j] != 0.f) {
                    temp = abs(a[i + j * *lda]) / work[j];
                    temp = 1.f - temp * temp;
                    temp = max(temp,0.f);
                    temp2 = work[j] / work[*n + j];
                    temp2 = temp * .05f * (temp2 * temp2) + 1.f;
                    if (temp2 == 1.f) {
                        if (*m - i > 1) {
                            i__2 = *m - i - 1;
                            work[j] = snrm2_(&i__2, &a[i + 1 + j * *lda], &c__1);
                            work[*n + j] = work[j];
                        } else {
                            work[j] = 0.f;
                            work[*n + j] = 0.f;
                        }
                    } else {
                        work[j] *= sqrtf(temp);
                    }
                }
            }
        }
    }
} /* sgeqpf_ */
