#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, Oct 2003: manual optimisation and clean-up */

/* Table of constant values */

static integer c__1 = 1;
static integer c_n1 = -1;
static integer c__3 = 3;
static integer c__2 = 2;

/* Subroutine */ void dgeqrf_(m, n, a, lda, tau, work, lwork, info)
integer *m, *n;
doublereal *a;
integer *lda;
doublereal *tau, *work;
integer *lwork, *info;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i, k, nbmin, iinfo;
    static integer ib, nb;
    static integer nx;
    static integer ldwork, lwkopt;
    static logical lquery;
    static integer iws, mmi;

/*  -- LAPACK routine (version 3.0) --                                    */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,        */
/*     Courant Institute, Argonne National Lab, and Rice University       */
/*     June 30, 1999                                                      */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DGEQRF computes a QR factorization of a real M-by-N matrix A:         */
/*  A = Q * R.                                                            */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  M       (input) INTEGER                                               */
/*          The number of rows of the matrix A.  M >= 0.                  */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The number of columns of the matrix A.  N >= 0.               */
/*                                                                        */
/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)      */
/*          On entry, the M-by-N matrix A.                                */
/*          On exit, the elements on and above the diagonal of the array  */
/*          contain the min(M,N)-by-N upper trapezoidal matrix R (R is    */
/*          upper triangular if m >= n); the elements below the diagonal, */
/*          with the array TAU, represent the orthogonal matrix Q as a    */
/*          product of min(m,n) elementary reflectors (see Further        */
/*          Details).                                                     */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A.  LDA >= max(1,M).       */
/*                                                                        */
/*  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))         */
/*          The scalar factors of the elementary reflectors (see Further  */
/*          Details).                                                     */
/*                                                                        */
/*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)  */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.      */
/*                                                                        */
/*  LWORK   (input) INTEGER                                               */
/*          The dimension of the array WORK.  LWORK >= max(1,N).          */
/*          For optimum performance LWORK >= N*NB, where NB is            */
/*          the optimal blocksize.                                        */
/*                                                                        */
/*          If LWORK = -1, then a workspace query is assumed; the routine */
/*          only calculates the optimal size of the WORK array, returns   */
/*          this value as the first entry of the WORK array, and no error */
/*          message related to LWORK is issued by XERBLA.                 */
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
/*     Q = H(1) H(2) . . . H(k), where k = min(m,n).                      */
/*                                                                        */
/*  Each H(i) has the form                                                */
/*                                                                        */
/*     H(i) = I - tau * v * v'                                            */
/*                                                                        */
/*  where tau is a real scalar, and v is a real vector with               */
/*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),  */
/*  and tau in TAU(i).                                                    */
/*                                                                        */
/*  ===================================================================== */

    /* Test the input arguments */

    *info = 0;
    nb = ilaenv_(&c__1, "DGEQRF", " ", m, n, &c_n1, &c_n1);
    lwkopt = *n * nb;
    *work = (doublereal) lwkopt;
    lquery = *lwork == -1;
    if      (*m < 0)           *info = 1;
    else if (*n < 0)           *info = 2;
    else if (*lda < max(1,*m)) *info = 4;
    else if (*lwork < max(1,*n) && ! lquery) *info = 7;
    if (*info != 0) {
        xerbla_("DGEQRF", info);
        *info = -(*info);
        return;
    }
    else if (lquery)
        return;

    /* Quick return if possible */

    k = min(*m,*n);
    if (k == 0) {
        *work = 1.;
        return;
    }

    nbmin = 2;
    nx = 0;
    iws = *n;
    if (nb > 1 && nb < k)
    {
        /* Determine when to cross over from blocked to unblocked code. */
        nx = ilaenv_(&c__3, "DGEQRF", " ", m, n, &c_n1, &c_n1);
        nx = max(0,nx);
        if (nx < k)
        {
            /* Determine if workspace is large enough for blocked code. */
            ldwork = *n;
            iws = ldwork * nb;
            if (*lwork < iws)
            {
                /* Not enough workspace to use optimal NB: */
                /* reduce NB and determine the minimum value of NB. */
                nb = *lwork / ldwork;
                nbmin = ilaenv_(&c__2, "DGEQRF", " ", m, n, &c_n1, &c_n1);
                nbmin = max(2,nbmin);
            }
        }
    }

    mmi = *m;
    if (nb >= nbmin && nb < k && nx < k) /* nbmin is at least 2, so is nb */
    {
        /* Use blocked code initially */
        for (i = 0; i < k-nx; i += nb, mmi -= nb) /* mmi == *m - i */
        {
            ib = min(k-i,nb);

            /* Compute the QR factorization of the current block */
            /* A(i:m,i:i+ib-1) */
            dgeqr2_(&mmi, &ib, &a[i + i * *lda], lda, &tau[i], work, &iinfo);
            if (i + ib < *n)
            {
                /* Form the triangular factor of the block reflector */
                /* H = H(i) H(i+1) . . . H(i+ib-1) */
                dlarft_("Forward", "Columnwise", &mmi, &ib, &a[i + i * *lda], lda, &tau[i], work, &ldwork);

                /* Apply H' to A(i:m,i+ib:n) from the left */
                i__1 = *n - i - ib;
                dlarfb_("Left", "Transpose", "Forward", "Columnwise", &mmi, &i__1,
                        &ib, &a[i + i * *lda], lda, work, &ldwork,
                        &a[i + (i + ib) * *lda], lda, work+ib, &ldwork);
            }
        }
    }
    else
        i = 0;

    /* Use unblocked code to factor the last or only block. */
    if (i < k) {
        i__1 = *n - i;
        dgeqr2_(&mmi, &i__1, &a[i + i * *lda], lda, &tau[i], work, &iinfo);
    }

    *work = (doublereal) iws;

} /* dgeqrf_ */
