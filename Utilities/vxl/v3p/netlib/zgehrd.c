#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Table of constant values */
static integer c__1 = 1;
static integer c_n1 = -1;
static integer c__3 = 3;
static integer c__2 = 2;
static integer c__65 = 65;
static doublecomplex c_b21 = {1.,0.};
static doublecomplex c_b24 = {-1.,0.};

/* Subroutine */ void zgehrd_(n, ilo, ihi, a, lda, tau, work, lwork, info)
const integer *n;
integer *ilo, *ihi;
doublecomplex *a;
const integer *lda;
doublecomplex *tau, *work;
integer *lwork, *info;
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;

    /* Local variables */
    static integer i;
    static doublecomplex t[4160] /* was [65][64] */;
    static integer nbmin, iinfo;
    static integer ib;
    static doublecomplex ei;
    static integer nb, nh, nx;
    static integer ldwork, iws;

/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZGEHRD reduces a complex general matrix A to upper Hessenberg form H  */
/*  by a unitary similarity transformation:  Q' * A * Q = H .             */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the matrix A.  N >= 0.                           */
/*                                                                        */
/*  ILO     (input) INTEGER                                               */
/*  IHI     (input) INTEGER                                               */
/*          It is assumed that A is already upper triangular in rows      */
/*          and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally     */
/*          set by a previous call to ZGEBAL; otherwise they should be    */
/*          set to 1 and N respectively. See Further Details.             */
/*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.      */
/*                                                                        */
/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)            */
/*          On entry, the N-by-N general matrix to be reduced.            */
/*          On exit, the upper triangle and the first subdiagonal of A    */
/*          are overwritten with the upper Hessenberg matrix H, and the   */
/*          elements below the first subdiagonal, with the array TAU,     */
/*          represent the unitary matrix Q as a product of elementary     */
/*          reflectors. See Further Details.                              */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A.  LDA >= max(1,N).       */
/*                                                                        */
/*  TAU     (output) COMPLEX*16 array, dimension (N-1)                    */
/*          The scalar factors of the elementary reflectors (see Further  */
/*          Details). Elements 1:ILO-1 and IHI:N-1 of TAU are set to      */
/*          zero.                                                         */
/*                                                                        */
/*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)        */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.      */
/*                                                                        */
/*  LWORK   (input) INTEGER                                               */
/*          The length of the array WORK.  LWORK >= max(1,N).             */
/*          For optimum performance LWORK >= N*NB, where NB is the        */
/*          optimal blocksize.                                            */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0:  successful exit                                         */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value.   */
/*                                                                        */
/*  Further Details                                                       */
/*  ===============                                                       */
/*                                                                        */
/*  The matrix Q is represented as a product of (ihi-ilo) elementary      */
/*  reflectors                                                            */
/*                                                                        */
/*     Q = H(ilo) H(ilo+1) . . . H(ihi-1).                                */
/*                                                                        */
/*  Each H(i) has the form                                                */
/*                                                                        */
/*     H(i) = I - tau * v * v'                                            */
/*                                                                        */
/*  where tau is a complex scalar, and v is a complex vector with         */
/*  v(1:i) = 0, v(i+1) = 1 and v(ihi+1:n) = 0; v(i+2:ihi) is stored on    */
/*  exit in A(i+2:ihi,i), and tau in TAU(i).                              */
/*                                                                        */
/*  The contents of A are illustrated by the following example, with      */
/*  n = 7, ilo = 2 and ihi = 6:                                           */
/*                                                                        */
/*  on entry,                        on exit,                             */
/*                                                                        */
/*  ( a   a   a   a   a   a   a )    (  a   a   h   h   h   h   a )       */
/*  (     a   a   a   a   a   a )    (      a   h   h   h   h   a )       */
/*  (     a   a   a   a   a   a )    (      h   h   h   h   h   h )       */
/*  (     a   a   a   a   a   a )    (      v2  h   h   h   h   h )       */
/*  (     a   a   a   a   a   a )    (      v2  v3  h   h   h   h )       */
/*  (     a   a   a   a   a   a )    (      v2  v3  v4  h   h   h )       */
/*  (                         a )    (                          a )       */
/*                                                                        */
/*  where a denotes an element of the original matrix A, h denotes a      */
/*  modified element of the upper Hessenberg matrix H, and vi denotes an  */
/*  element of the vector defining H(i).                                  */
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
    } else if (*lwork < max(1,*n)) {
        *info = -8;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("ZGEHRD", &i__1);
        return;
    }

/*     Set elements 1:ILO-1 and IHI:N-1 of TAU to zero */

    for (i = 0; i < *ilo-1; ++i) {
        tau[i].r = 0., tau[i].i = 0.;
    }
    for (i = max(0,*ihi-1); i < *n-1; ++i) {
        tau[i].r = 0., tau[i].i = 0.;
    }

/*     Quick return if possible */

    nh = *ihi - *ilo + 1;
    if (nh <= 1) {
        work[0].r = 1., work[0].i = 0.;
        return;
    }

/*     Determine the block size. */

    i__2 = ilaenv_(&c__1, "ZGEHRD", " ", n, ilo, ihi, &c_n1);
    nb = min(64,i__2);
    nbmin = 2;
    iws = 1;
    if (nb > 1 && nb < nh) {

/*        Determine when to cross over from blocked to unblocked code */
/*        (last block is always handled by unblocked code). */

        i__2 = ilaenv_(&c__3, "ZGEHRD", " ", n, ilo, ihi, &c_n1);
        nx = max(nb,i__2);
        if (nx < nh) {

/*           Determine if workspace is large enough for blocked code. */

            iws = *n * nb;
            if (*lwork < iws) {

/*              Not enough workspace to use optimal NB:  determine the */
/*              minimum value of NB, and reduce NB or force use of */
/*              unblocked code. */

                i__2 = ilaenv_(&c__2, "ZGEHRD", " ", n, ilo, ihi, &c_n1);
                nbmin = max(2,i__2);
                if (*lwork >= *n * nbmin) {
                    nb = *lwork / *n;
                } else {
                    nb = 1;
                }
            }
        }
    }
    ldwork = *n;

    if (nb < nbmin || nb >= nh) {

/*        Use unblocked code below */

        i = *ilo-1;

    } else {

/*        Use blocked code */

        i__1 = *ihi - 2 - nx;
        for (i = *ilo-1; nb < 0 ? i >= i__1 : i <= i__1; i += nb) {
            i__4 = *ihi-1 - i;
            ib = min(nb,i__4);

/*           Reduce columns i:i+ib-1 to Hessenberg form, returning the */
/*           matrices V and T of the block reflector H = I - V*T*V' */
/*           which performs the reduction, and also the matrix Y = A*V*T */

            i__1 = i + 1;
            zlahrd_(ihi, &i__1, &ib, &a[i * *lda], lda, &tau[i], t, &c__65, work, &ldwork);

/*           Apply the block reflector H to A(1:ihi,i+ib:ihi) from the right, */
/*           computing  A := A - Y * V'. V(i+ib,ib-1) must be set to 1. */

            i__3 = i + ib + (i + ib - 1) * *lda;
            ei.r = a[i__3].r, ei.i = a[i__3].i;
            a[i__3].r = 1., a[i__3].i = 0.;
            i__3 = *ihi - i - ib;
            zgemm_("No transpose", "Conjugate transpose", ihi, &i__3, &ib, &c_b24, work,
                   &ldwork, &a[i+ib + i * *lda], lda, &c_b21, &a[(i+ib) * *lda], lda);
            i__3 = i + ib + (i + ib - 1) * *lda;
            a[i__3].r = ei.r, a[i__3].i = ei.i;

/*           Apply the block reflector H to A(i+1:ihi,i+ib:n) from the left */

            i__3 = *ihi-1 - i;
            i__4 = *n - i - ib;
            zlarfb_("Left", "Conjugate transpose", "Forward", "Columnwise",
                    &i__3, &i__4, &ib, &a[i+1 + i * *lda], lda, t, &c__65,
                    &a[i+1 + (i + ib) * *lda], lda, work, &ldwork);
        }
    }

/*     Use unblocked code to reduce the rest of the matrix */

    i__1 = i + 1;
    zgehd2_(n, &i__1, ihi, a, lda, tau, work, &iinfo);
    work[0].r = (doublereal) iws, work[0].i = 0.;

    return;
} /* zgehrd_ */
