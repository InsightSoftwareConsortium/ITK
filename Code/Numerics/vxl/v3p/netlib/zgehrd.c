/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c_n1 = -1;
static integer c__3 = 3;
static integer c__2 = 2;
static integer c__65 = 65;
static doublecomplex c_b21 = {1.,0.};
static doublecomplex c_b24 = {-1.,0.};

/* Subroutine */ int zgehrd_(n, ilo, ihi, a, lda, tau, work, lwork, info)
integer *n, *ilo, *ihi;
doublecomplex *a;
integer *lda;
doublecomplex *tau, *work;
integer *lwork, *info;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;

    /* Local variables */
    static integer i;
    static doublecomplex t[4160] /* was [65][64] */;
    static integer nbmin, iinfo;
    extern /* Subroutine */ int zgemm_(), zgehd2_();
    static integer ib;
    static doublecomplex ei;
    static integer nb, nh, nx;
    extern /* Subroutine */ int xerbla_();
    extern integer ilaenv_();
    extern /* Subroutine */ int zlarfb_(), zlahrd_();
    static integer ldwork, iws;


/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZGEHRD reduces a complex general matrix A to upper Hessenberg form H
*/
/*  by a unitary similarity transformation:  Q' * A * Q = H . */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The order of the matrix A.  N >= 0. */

/*  ILO     (input) INTEGER */
/*  IHI     (input) INTEGER */
/*          It is assumed that A is already upper triangular in rows */
/*          and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally */
/*          set by a previous call to ZGEBAL; otherwise they should be */
/*          set to 1 and N respectively. See Further Details. */
/*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N) */
/*          On entry, the N-by-N general matrix to be reduced. */
/*          On exit, the upper triangle and the first subdiagonal of A */
/*          are overwritten with the upper Hessenberg matrix H, and the */
/*          elements below the first subdiagonal, with the array TAU, */
/*          represent the unitary matrix Q as a product of elementary */
/*          reflectors. See Further Details. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,N). */

/*  TAU     (output) COMPLEX*16 array, dimension (N-1) */
/*          The scalar factors of the elementary reflectors (see Further
*/
/*          Details). Elements 1:ILO-1 and IHI:N-1 of TAU are set to */
/*          zero. */

/*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK) */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK. */

/*  LWORK   (input) INTEGER */
/*          The length of the array WORK.  LWORK >= max(1,N). */
/*          For optimum performance LWORK >= N*NB, where NB is the */
/*          optimal blocksize. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value. */

/*  Further Details */
/*  =============== */

/*  The matrix Q is represented as a product of (ihi-ilo) elementary */
/*  reflectors */

/*     Q = H(ilo) H(ilo+1) . . . H(ihi-1). */

/*  Each H(i) has the form */

/*     H(i) = I - tau * v * v' */

/*  where tau is a complex scalar, and v is a complex vector with */
/*  v(1:i) = 0, v(i+1) = 1 and v(ihi+1:n) = 0; v(i+2:ihi) is stored on */
/*  exit in A(i+2:ihi,i), and tau in TAU(i). */

/*  The contents of A are illustrated by the following example, with */
/*  n = 7, ilo = 2 and ihi = 6: */

/*  on entry,                        on exit, */

/*  ( a   a   a   a   a   a   a )    (  a   a   h   h   h   h   a ) */
/*  (     a   a   a   a   a   a )    (      a   h   h   h   h   a ) */
/*  (     a   a   a   a   a   a )    (      h   h   h   h   h   h ) */
/*  (     a   a   a   a   a   a )    (      v2  h   h   h   h   h ) */
/*  (     a   a   a   a   a   a )    (      v2  v3  h   h   h   h ) */
/*  (     a   a   a   a   a   a )    (      v2  v3  v4  h   h   h ) */
/*  (                         a )    (                          a ) */

/*  where a denotes an element of the original matrix A, h denotes a */
/*  modified element of the upper Hessenberg matrix H, and vi denotes an
*/
/*  element of the vector defining H(i). */

/*  =====================================================================
*/

/*     .. Parameters .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Local Arrays .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input parameters */

    /* Parameter adjustments */
    --work;
    --tau;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
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
        xerbla_("ZGEHRD", &i__1, 6L);
        return 0;
    }

/*     Set elements 1:ILO-1 and IHI:N-1 of TAU to zero */

    i__1 = *ilo - 1;
    for (i = 1; i <= i__1; ++i) {
        i__2 = i;
        tau[i__2].r = 0., tau[i__2].i = 0.;
/* L10: */
    }
    i__1 = *n - 1;
    for (i = max(1,*ihi); i <= i__1; ++i) {
        i__2 = i;
        tau[i__2].r = 0., tau[i__2].i = 0.;
/* L20: */
    }

/*     Quick return if possible */

    nh = *ihi - *ilo + 1;
    if (nh <= 1) {
        work[1].r = 1., work[1].i = 0.;
        return 0;
    }

/*     Determine the block size. */

/* Computing MIN */
    i__1 = 64, i__2 = ilaenv_(&c__1, "ZGEHRD", " ", n, ilo, ihi, &c_n1, 6L,
            1L);
    nb = min(i__1,i__2);
    nbmin = 2;
    iws = 1;
    if (nb > 1 && nb < nh) {

/*        Determine when to cross over from blocked to unblocked code
*/
/*        (last block is always handled by unblocked code). */

/* Computing MAX */
        i__1 = nb, i__2 = ilaenv_(&c__3, "ZGEHRD", " ", n, ilo, ihi, &c_n1,
                6L, 1L);
        nx = max(i__1,i__2);
        if (nx < nh) {

/*           Determine if workspace is large enough for blocked co
de. */

            iws = *n * nb;
            if (*lwork < iws) {

/*              Not enough workspace to use optimal NB:  deter
mine the */
/*              minimum value of NB, and reduce NB or force us
e of */
/*              unblocked code. */

/* Computing MAX */
                i__1 = 2, i__2 = ilaenv_(&c__2, "ZGEHRD", " ", n, ilo, ihi, &
                        c_n1, 6L, 1L);
                nbmin = max(i__1,i__2);
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

        i = *ilo;

    } else {

/*        Use blocked code */

        i__1 = *ihi - 1 - nx;
        i__2 = nb;
        for (i = *ilo; i__2 < 0 ? i >= i__1 : i <= i__1; i += i__2) {
/* Computing MIN */
            i__3 = nb, i__4 = *ihi - i;
            ib = min(i__3,i__4);

/*           Reduce columns i:i+ib-1 to Hessenberg form, returning
 the */
/*           matrices V and T of the block reflector H = I - V*T*V
' */
/*           which performs the reduction, and also the matrix Y =
 A*V*T */

            zlahrd_(ihi, &i, &ib, &a[i * a_dim1 + 1], lda, &tau[i], t, &c__65,
                     &work[1], &ldwork);

/*           Apply the block reflector H to A(1:ihi,i+ib:ihi) from
 the */
/*           right, computing  A := A - Y * V'. V(i+ib,ib-1) must
be set */
/*           to 1. */

            i__3 = i + ib + (i + ib - 1) * a_dim1;
            ei.r = a[i__3].r, ei.i = a[i__3].i;
            i__3 = i + ib + (i + ib - 1) * a_dim1;
            a[i__3].r = 1., a[i__3].i = 0.;
            i__3 = *ihi - i - ib + 1;
            zgemm_("No transpose", "Conjugate transpose", ihi, &i__3, &ib, &
                    c_b24, &work[1], &ldwork, &a[i + ib + i * a_dim1], lda, &
                    c_b21, &a[(i + ib) * a_dim1 + 1], lda, 12L, 19L);
            i__3 = i + ib + (i + ib - 1) * a_dim1;
            a[i__3].r = ei.r, a[i__3].i = ei.i;

/*           Apply the block reflector H to A(i+1:ihi,i+ib:n) from
 the */
/*           left */

            i__3 = *ihi - i;
            i__4 = *n - i - ib + 1;
            zlarfb_("Left", "Conjugate transpose", "Forward", "Columnwise", &
                    i__3, &i__4, &ib, &a[i + 1 + i * a_dim1], lda, t, &c__65,
                    &a[i + 1 + (i + ib) * a_dim1], lda, &work[1], &ldwork, 4L,
                     19L, 7L, 10L);
/* L30: */
        }
    }

/*     Use unblocked code to reduce the rest of the matrix */

    zgehd2_(n, &i, ihi, &a[a_offset], lda, &tau[1], &work[1], &iinfo);
    work[1].r = (doublereal) iws, work[1].i = 0.;

    return 0;

/*     End of ZGEHRD */

} /* zgehrd_ */

