/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublecomplex c_b4 = {-1.,0.};
static doublecomplex c_b5 = {1.,0.};
static integer c__1 = 1;
static doublecomplex c_b39 = {0.,0.};

/* Subroutine */ int zlahrd_(n, k, nb, a, lda, tau, t, ldt, y, ldy)
integer *n, *k, *nb;
doublecomplex *a;
integer *lda;
doublecomplex *tau, *t;
integer *ldt;
doublecomplex *y;
integer *ldy;
{
    /* System generated locals */
    integer a_dim1, a_offset, t_dim1, t_offset, y_dim1, y_offset, i__1, i__2, 
	    i__3;
    doublecomplex z__1;

    /* Local variables */
    static integer i;
    extern /* Subroutine */ int zscal_(), zgemv_(), zcopy_(), zaxpy_(), 
	    ztrmv_();
    static doublecomplex ei;
    extern /* Subroutine */ int zlarfg_(), zlacgv_();


/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLAHRD reduces the first NB columns of a complex general n-by-(n-k+1) 
*/
/*  matrix A so that elements below the k-th subdiagonal are zero. The */
/*  reduction is performed by a unitary similarity transformation */
/*  Q' * A * Q. The routine returns the matrices V and T which determine 
*/
/*  Q as a block reflector I - V*T*V', and also the matrix Y = A * V * T. 
*/

/*  This is an auxiliary routine called by ZGEHRD. */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The order of the matrix A. */

/*  K       (input) INTEGER */
/*          The offset for the reduction. Elements below the k-th */
/*          subdiagonal in the first NB columns are reduced to zero. */

/*  NB      (input) INTEGER */
/*          The number of columns to be reduced. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N-K+1) */
/*          On entry, the n-by-(n-k+1) general matrix A. */
/*          On exit, the elements on and above the k-th subdiagonal in */
/*          the first NB columns are overwritten with the corresponding */
/*          elements of the reduced matrix; the elements below the k-th */
/*          subdiagonal, with the array TAU, represent the matrix Q as a 
*/
/*          product of elementary reflectors. The other columns of A are 
*/
/*          unchanged. See Further Details. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,N). */

/*  TAU     (output) COMPLEX*16 array, dimension (NB) */
/*          The scalar factors of the elementary reflectors. See Further 
*/
/*          Details. */

/*  T       (output) COMPLEX*16 array, dimension (NB,NB) */
/*          The upper triangular matrix T. */

/*  LDT     (input) INTEGER */
/*          The leading dimension of the array T.  LDT >= NB. */

/*  Y       (output) COMPLEX*16 array, dimension (LDY,NB) */
/*          The n-by-nb matrix Y. */

/*  LDY     (input) INTEGER */
/*          The leading dimension of the array Y. LDY >= max(1,N). */

/*  Further Details */
/*  =============== */

/*  The matrix Q is represented as a product of nb elementary reflectors 
*/

/*     Q = H(1) H(2) . . . H(nb). */

/*  Each H(i) has the form */

/*     H(i) = I - tau * v * v' */

/*  where tau is a complex scalar, and v is a complex vector with */
/*  v(1:i+k-1) = 0, v(i+k) = 1; v(i+k+1:n) is stored on exit in */
/*  A(i+k+1:n,i), and tau in TAU(i). */

/*  The elements of the vectors v together form the (n-k+1)-by-nb matrix 
*/
/*  V which is needed, with T and Y, to apply the transformation to the */
/*  unreduced part of the matrix, using an update of the form: */
/*  A := (I - V*T*V') * (A - Y*V'). */

/*  The contents of A on exit are illustrated by the following example */
/*  with n = 7, k = 3 and nb = 2: */

/*     ( a   h   a   a   a ) */
/*     ( a   h   a   a   a ) */
/*     ( a   h   a   a   a ) */
/*     ( h   h   a   a   a ) */
/*     ( v1  h   a   a   a ) */
/*     ( v1  v2  a   a   a ) */
/*     ( v1  v2  a   a   a ) */

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
/*     .. External Subroutines .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */

/*     Quick return if possible */

    /* Parameter adjustments */
    y_dim1 = *ldy;
    y_offset = y_dim1 + 1;
    y -= y_offset;
    t_dim1 = *ldt;
    t_offset = t_dim1 + 1;
    t -= t_offset;
    --tau;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (*n <= 1) {
	return 0;
    }

    i__1 = *nb;
    for (i = 1; i <= i__1; ++i) {
	if (i > 1) {

/*           Update A(1:n,i) */

/*           Compute i-th column of A - Y * V' */

	    i__2 = i - 1;
	    zlacgv_(&i__2, &a[*k + i - 1 + a_dim1], lda);
	    i__2 = i - 1;
	    zgemv_("No transpose", n, &i__2, &c_b4, &y[y_offset], ldy, &a[*k 
		    + i - 1 + a_dim1], lda, &c_b5, &a[i * a_dim1 + 1], &c__1, 
		    12L);
	    i__2 = i - 1;
	    zlacgv_(&i__2, &a[*k + i - 1 + a_dim1], lda);

/*           Apply I - V * T' * V' to this column (call it b) from
 the */
/*           left, using the last column of T as workspace */

/*           Let  V = ( V1 )   and   b = ( b1 )   (first I-1 rows)
 */
/*                    ( V2 )             ( b2 ) */

/*           where V1 is unit lower triangular */

/*           w := V1' * b1 */

	    i__2 = i - 1;
	    zcopy_(&i__2, &a[*k + 1 + i * a_dim1], &c__1, &t[*nb * t_dim1 + 1]
		    , &c__1);
	    i__2 = i - 1;
	    ztrmv_("Lower", "Conjugate transpose", "Unit", &i__2, &a[*k + 1 + 
		    a_dim1], lda, &t[*nb * t_dim1 + 1], &c__1, 5L, 19L, 4L);

/*           w := w + V2'*b2 */

	    i__2 = *n - *k - i + 1;
	    i__3 = i - 1;
	    zgemv_("Conjugate transpose", &i__2, &i__3, &c_b5, &a[*k + i + 
		    a_dim1], lda, &a[*k + i + i * a_dim1], &c__1, &c_b5, &t[*
		    nb * t_dim1 + 1], &c__1, 19L);

/*           w := T'*w */

	    i__2 = i - 1;
	    ztrmv_("Upper", "Conjugate transpose", "Non-unit", &i__2, &t[
		    t_offset], ldt, &t[*nb * t_dim1 + 1], &c__1, 5L, 19L, 8L);

/*           b2 := b2 - V2*w */

	    i__2 = *n - *k - i + 1;
	    i__3 = i - 1;
	    zgemv_("No transpose", &i__2, &i__3, &c_b4, &a[*k + i + a_dim1], 
		    lda, &t[*nb * t_dim1 + 1], &c__1, &c_b5, &a[*k + i + i * 
		    a_dim1], &c__1, 12L);

/*           b1 := b1 - V1*w */

	    i__2 = i - 1;
	    ztrmv_("Lower", "No transpose", "Unit", &i__2, &a[*k + 1 + a_dim1]
		    , lda, &t[*nb * t_dim1 + 1], &c__1, 5L, 12L, 4L);
	    i__2 = i - 1;
	    zaxpy_(&i__2, &c_b4, &t[*nb * t_dim1 + 1], &c__1, &a[*k + 1 + i * 
		    a_dim1], &c__1);

	    i__2 = *k + i - 1 + (i - 1) * a_dim1;
	    a[i__2].r = ei.r, a[i__2].i = ei.i;
	}

/*        Generate the elementary reflector H(i) to annihilate */
/*        A(k+i+1:n,i) */

	i__2 = *k + i + i * a_dim1;
	ei.r = a[i__2].r, ei.i = a[i__2].i;
	i__2 = *n - *k - i + 1;
/* Computing MIN */
	i__3 = *k + i + 1;
	zlarfg_(&i__2, &ei, &a[min(i__3,*n) + i * a_dim1], &c__1, &tau[i]);
	i__2 = *k + i + i * a_dim1;
	a[i__2].r = 1., a[i__2].i = 0.;

/*        Compute  Y(1:n,i) */

	i__2 = *n - *k - i + 1;
	zgemv_("No transpose", n, &i__2, &c_b5, &a[(i + 1) * a_dim1 + 1], lda,
		 &a[*k + i + i * a_dim1], &c__1, &c_b39, &y[i * y_dim1 + 1], &
		c__1, 12L);
	i__2 = *n - *k - i + 1;
	i__3 = i - 1;
	zgemv_("Conjugate transpose", &i__2, &i__3, &c_b5, &a[*k + i + a_dim1]
		, lda, &a[*k + i + i * a_dim1], &c__1, &c_b39, &t[i * t_dim1 
		+ 1], &c__1, 19L);
	i__2 = i - 1;
	zgemv_("No transpose", n, &i__2, &c_b4, &y[y_offset], ldy, &t[i * 
		t_dim1 + 1], &c__1, &c_b5, &y[i * y_dim1 + 1], &c__1, 12L);
	zscal_(n, &tau[i], &y[i * y_dim1 + 1], &c__1);

/*        Compute T(1:i,i) */

	i__2 = i - 1;
	i__3 = i;
	z__1.r = -tau[i__3].r, z__1.i = -tau[i__3].i;
	zscal_(&i__2, &z__1, &t[i * t_dim1 + 1], &c__1);
	i__2 = i - 1;
	ztrmv_("Upper", "No transpose", "Non-unit", &i__2, &t[t_offset], ldt, 
		&t[i * t_dim1 + 1], &c__1, 5L, 12L, 8L);
	i__2 = i + i * t_dim1;
	i__3 = i;
	t[i__2].r = tau[i__3].r, t[i__2].i = tau[i__3].i;

/* L10: */
    }
    i__1 = *k + *nb + *nb * a_dim1;
    a[i__1].r = ei.r, a[i__1].i = ei.i;

    return 0;

/*     End of ZLAHRD */

} /* zlahrd_ */

