#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Table of constant values */
static integer c__1 = 1;
static integer c_n1 = -1;
static integer c__3 = 3;
static integer c__2 = 2;

/* Subroutine */ void zungqr_(m, n, k, a, lda, tau, work, lwork, info)
const integer *m, *n, *k;
doublecomplex *a;
const integer *lda;
const doublecomplex *tau;
doublecomplex *work;
const integer *lwork;
integer *info;
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    static integer i, j, l, nbmin, iinfo, ib, nb, ki, kk;
    static integer nx;
    static integer ldwork;
    static integer iws;

/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZUNGQR generates an M-by-N complex matrix Q with orthonormal columns, */
/*  which is defined as the first N columns of a product of K elementary  */
/*  reflectors of order M                                                 */
/*                                                                        */
/*        Q  =  H(1) H(2) . . . H(k)                                      */
/*                                                                        */
/*  as returned by ZGEQRF.                                                */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  M       (input) INTEGER                                               */
/*          The number of rows of the matrix Q. M >= 0.                   */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The number of columns of the matrix Q. M >= N >= 0.           */
/*                                                                        */
/*  K       (input) INTEGER                                               */
/*          The number of elementary reflectors whose product defines the */
/*          matrix Q. N >= K >= 0.                                        */
/*                                                                        */
/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)            */
/*          On entry, the i-th column must contain the vector which       */
/*          defines the elementary reflector H(i), for i = 1,2,...,k, as  */
/*          returned by ZGEQRF in the first k columns of its array        */
/*          argument A.                                                   */
/*          On exit, the M-by-N matrix Q.                                 */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The first dimension of the array A. LDA >= max(1,M).          */
/*                                                                        */
/*  TAU     (input) COMPLEX*16 array, dimension (K)                       */
/*          TAU(i) must contain the scalar factor of the elementary       */
/*          reflector H(i), as returned by ZGEQRF.                        */
/*                                                                        */
/*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)        */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.      */
/*                                                                        */
/*  LWORK   (input) INTEGER                                               */
/*          The dimension of the array WORK. LWORK >= max(1,N).           */
/*          For optimum performance LWORK >= N*NB, where NB is the        */
/*          optimal blocksize.                                            */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0:  successful exit                                         */
/*          < 0:  if INFO = -i, the i-th argument has an illegal value    */
/*                                                                        */
/*  ===================================================================== */

    *info = 0;
    if (*m < 0) {
        *info = -1;
    } else if (*n < 0 || *n > *m) {
        *info = -2;
    } else if (*k < 0 || *k > *n) {
        *info = -3;
    } else if (*lda < max(1,*m)) {
        *info = -5;
    } else if (*lwork < max(1,*n)) {
        *info = -8;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("ZUNGQR", &i__1);
        return;
    }

/*     Quick return if possible */

    if (*n <= 0) {
        work[0].r = 1., work[0].i = 0.;
        return;
    }

/*     Determine the block size. */

    nb = ilaenv_(&c__1, "ZUNGQR", " ", m, n, k, &c_n1);
    nbmin = 2;
    nx = 0;
    iws = *n;
    if (nb > 1 && nb < *k) {

/*        Determine when to cross over from blocked to unblocked code. */

        i__1 = ilaenv_(&c__3, "ZUNGQR", " ", m, n, k, &c_n1);
        nx = max(0,i__1);
        if (nx < *k) {

/*           Determine if workspace is large enough for blocked code. */

            ldwork = *n;
            iws = ldwork * nb;
            if (*lwork < iws) {

/*              Not enough workspace to use optimal NB:  reduce NB and */
/*              determine the minimum value of NB. */

                nb = *lwork / ldwork;
                i__1 = ilaenv_(&c__2, "ZUNGQR", " ", m, n, k, &c_n1);
                nbmin = max(2,i__1);
            }
        }
    }

    if (nb >= nbmin && nb < *k && nx < *k) {

/*        Use blocked code after the last block. */
/*        The first kk columns are handled by the block method. */

        ki = (*k - nx - 1) / nb * nb;
        kk = min(*k, ki+nb);

/*        Set A(1:kk,kk+1:n) to zero. */

        for (j = kk; j < *n; ++j) {
            for (i = 0; i < kk; ++i) {
                i__1 = i + j * *lda;
                a[i__1].r = 0., a[i__1].i = 0.;
            }
        }
    } else {
        kk = 0;
    }

/*     Use unblocked code for the last or only block. */

    if (kk < *n) {
        i__1 = *m - kk;
        i__2 = *n - kk;
        i__3 = *k - kk;
        zung2r_(&i__1, &i__2, &i__3, &a[kk+kk* *lda], lda, &tau[kk], work, &iinfo);
    }

    if (kk > 0) {

/*        Use blocked code */

        for (i = ki; nb > 0 ? i >= 0 : i <= 0; i -= nb) {
            ib = min(nb, *k - i);
            if (i+1 + ib <= *n) {

/*              Form the triangular factor of the block reflector */
/*              H = H(i) H(i+1) . . . H(i+ib-1) */

                i__1 = *m - i;
                zlarft_("Forward", "Columnwise", &i__1, &ib, &a[i+i* *lda], lda, &tau[i], work, &ldwork);

/*              Apply H to A(i:m,i+ib:n) from the left */

                i__1 = *m - i;
                i__2 = *n - i - ib;
                zlarfb_("Left", "No transpose", "Forward", "Columnwise",
                        &i__1, &i__2, &ib, &a[i+i* *lda], lda, work, &ldwork,
                        &a[i+(i+ib)* *lda], lda, &work[ib], &ldwork);
            }

/*           Apply H to rows i:m of current block */

            i__1 = *m - i;
            zung2r_(&i__1, &ib, &ib, &a[i+i* *lda], lda, &tau[i], work, &iinfo);

/*           Set rows 1:i-1 of current block to zero */

            for (j = i; j < i + ib; ++j) {
                for (l = 0; l < i; ++l) {
                    i__1 = l + j * *lda;
                    a[i__1].r = 0., a[i__1].i = 0.;
                }
            }
        }
    }

    work[0].r = (doublereal) iws, work[0].i = 0.;
} /* zungqr_ */
