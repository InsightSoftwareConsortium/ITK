#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Subroutine */ void zunghr_(n, ilo, ihi, a, lda, tau, work, lwork, info)
const integer *n;
integer *ilo, *ihi;
doublecomplex *a;
const integer *lda;
const doublecomplex *tau;
doublecomplex *work;
const integer *lwork;
integer *info;
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i, j, iinfo, nh;

/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZUNGHR generates a complex unitary matrix Q which is defined as the   */
/*  product of IHI-ILO elementary reflectors of order N, as returned by   */
/*  ZGEHRD:                                                               */
/*                                                                        */
/*  Q = H(ilo) H(ilo+1) . . . H(ihi-1).                                   */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the matrix Q. N >= 0.                            */
/*                                                                        */
/*  ILO     (input) INTEGER                                               */
/*  IHI     (input) INTEGER                                               */
/*          ILO and IHI must have the same values as in the previous call */
/*          of ZGEHRD. Q is equal to the unit matrix except in the        */
/*          submatrix Q(ilo+1:ihi,ilo+1:ihi).                             */
/*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.      */
/*                                                                        */
/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)            */
/*          On entry, the vectors which define the elementary reflectors, */
/*          as returned by ZGEHRD.                                        */
/*          On exit, the N-by-N unitary matrix Q.                         */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A. LDA >= max(1,N).        */
/*                                                                        */
/*  TAU     (input) COMPLEX*16 array, dimension (N-1)                     */
/*          TAU(i) must contain the scalar factor of the elementary       */
/*          reflector H(i), as returned by ZGEHRD.                        */
/*                                                                        */
/*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)        */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.      */
/*                                                                        */
/*  LWORK   (input) INTEGER                                               */
/*          The dimension of the array WORK. LWORK >= IHI-ILO.            */
/*          For optimum performance LWORK >= (IHI-ILO)*NB, where NB is    */
/*          the optimal blocksize.                                        */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0:  successful exit                                         */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value    */
/*                                                                        */
/*  ===================================================================== */

    *info = 0;
    if (*n < 0) {
        *info = -1;
    } else if (*ilo < 1 || *ilo > max(1,*n)) {
        *info = -2;
    } else if (*ihi < min(*ilo,*n) || *ihi > *n) {
        *info = -3;
    } else if (*lda < max(1,*n)) {
        *info = -5;
    } else /* if(complicated condition) */ {
        if (*lwork < max(1, *ihi - *ilo)) {
            *info = -8;
        }
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("ZUNGHR", &i__1);
        return;
    }

/*     Quick return if possible */

    if (*n == 0) {
        work[0].r = 1., work[0].i = 0.;
        return;
    }

/*     Shift the vectors which define the elementary reflectors one */
/*     column to the right, and set the first ilo and the last n-ihi */
/*     rows and columns to those of the unit matrix */

    for (j = *ihi - 1; j >= *ilo; --j) {
        for (i = 0; i < j; ++i) {
            i__1 = i + j * *lda;
            a[i__1].r = 0., a[i__1].i = 0.;
        }
        for (i = j + 1; i < *ihi; ++i) {
            i__1 = i + j * *lda;
            i__2 = i + (j-1) * *lda;
            a[i__1].r = a[i__2].r, a[i__1].i = a[i__2].i;
        }
        for (i = *ihi; i < *n; ++i) {
            i__1 = i + j * *lda;
            a[i__1].r = 0., a[i__1].i = 0.;
        }
    }
    for (j = 0; j < *ilo; ++j) {
        for (i = 0; i < *n; ++i) {
            i__1 = i + j * *lda;
            a[i__1].r = 0., a[i__1].i = 0.;
        }
        i__1 = j + j * *lda;
        a[i__1].r = 1., a[i__1].i = 0.;
    }
    for (j = *ihi; j < *n; ++j) {
        for (i = 0; i < *n; ++i) {
            i__1 = i + j * *lda;
            a[i__1].r = 0., a[i__1].i = 0.;
        }
        i__1 = j + j * *lda;
        a[i__1].r = 1., a[i__1].i = 0.;
    }

    nh = *ihi - *ilo;
    if (nh > 0) {

/*        Generate Q(ilo+1:ihi,ilo+1:ihi) */

        zungqr_(&nh, &nh, &nh, &a[*ilo + *ilo * *lda], lda, &tau[*ilo-1], work, lwork, &iinfo);
    }
} /* zunghr_ */
