/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublecomplex c_b4 = {0.,0.};
static integer c__1 = 1;

/* Subroutine */ int zlarft_(direct, storev, n, k, v, ldv, tau, t, ldt, 
	direct_len, storev_len)
char *direct, *storev;
integer *n, *k;
doublecomplex *v;
integer *ldv;
doublecomplex *tau, *t;
integer *ldt;
ftnlen direct_len;
ftnlen storev_len;
{
    /* System generated locals */
    integer t_dim1, t_offset, v_dim1, v_offset, i__1, i__2, i__3, i__4;
    doublecomplex z__1;

    /* Local variables */
    static integer i, j;
    extern logical lsame_();
    extern /* Subroutine */ int zgemv_(), ztrmv_(), zlacgv_();
    static doublecomplex vii;


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

/*  ZLARFT forms the triangular factor T of a complex block reflector H */
/*  of order n, which is defined as a product of k elementary reflectors. 
*/

/*  If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular; 
*/

/*  If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular. 
*/

/*  If STOREV = 'C', the vector which defines the elementary reflector */
/*  H(i) is stored in the i-th column of the array V, and */

/*     H  =  I - V * T * V' */

/*  If STOREV = 'R', the vector which defines the elementary reflector */
/*  H(i) is stored in the i-th row of the array V, and */

/*     H  =  I - V' * T * V */

/*  Arguments */
/*  ========= */

/*  DIRECT  (input) CHARACTER*1 */
/*          Specifies the order in which the elementary reflectors are */
/*          multiplied to form the block reflector: */
/*          = 'F': H = H(1) H(2) . . . H(k) (Forward) */
/*          = 'B': H = H(k) . . . H(2) H(1) (Backward) */

/*  STOREV  (input) CHARACTER*1 */
/*          Specifies how the vectors which define the elementary */
/*          reflectors are stored (see also Further Details): */
/*          = 'C': columnwise */
/*          = 'R': rowwise */

/*  N       (input) INTEGER */
/*          The order of the block reflector H. N >= 0. */

/*  K       (input) INTEGER */
/*          The order of the triangular factor T (= the number of */
/*          elementary reflectors). K >= 1. */

/*  V       (input/output) COMPLEX*16 array, dimension */
/*                               (LDV,K) if STOREV = 'C' */
/*                               (LDV,N) if STOREV = 'R' */
/*          The matrix V. See further details. */

/*  LDV     (input) INTEGER */
/*          The leading dimension of the array V. */
/*          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K. 
*/

/*  TAU     (input) COMPLEX*16 array, dimension (K) */
/*          TAU(i) must contain the scalar factor of the elementary */
/*          reflector H(i). */

/*  T       (output) COMPLEX*16 array, dimension (LDT,K) */
/*          The k by k triangular factor T of the block reflector. */
/*          If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is 
*/
/*          lower triangular. The rest of the array is not used. */

/*  LDT     (input) INTEGER */
/*          The leading dimension of the array T. LDT >= K. */

/*  Further Details */
/*  =============== */

/*  The shape of the matrix V and the storage of the vectors which define 
*/
/*  the H(i) is best illustrated by the following example with n = 5 and 
*/
/*  k = 3. The elements equal to 1 are not stored; the corresponding */
/*  array elements are modified but restored on exit. The rest of the */
/*  array is not used. */

/*  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R': 
*/

/*               V = (  1       )                 V = (  1 v1 v1 v1 v1 ) 
*/
/*                   ( v1  1    )                     (     1 v2 v2 v2 ) 
*/
/*                   ( v1 v2  1 )                     (        1 v3 v3 ) 
*/
/*                   ( v1 v2 v3 ) */
/*                   ( v1 v2 v3 ) */

/*  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R': 
*/

/*               V = ( v1 v2 v3 )                 V = ( v1 v1  1       ) 
*/
/*                   ( v1 v2 v3 )                     ( v2 v2 v2  1    ) 
*/
/*                   (  1 v2 v3 )                     ( v3 v3 v3 v3  1 ) 
*/
/*                   (     1 v3 ) */
/*                   (        1 ) */

/*  ===================================================================== 
*/

/*     .. Parameters .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Executable Statements .. */

/*     Quick return if possible */

    /* Parameter adjustments */
    t_dim1 = *ldt;
    t_offset = t_dim1 + 1;
    t -= t_offset;
    --tau;
    v_dim1 = *ldv;
    v_offset = v_dim1 + 1;
    v -= v_offset;

    /* Function Body */
    if (*n == 0) {
	return 0;
    }

    if (lsame_(direct, "F", 1L, 1L)) {
	i__1 = *k;
	for (i = 1; i <= i__1; ++i) {
	    i__2 = i;
	    if (tau[i__2].r == 0. && tau[i__2].i == 0.) {

/*              H(i)  =  I */

		i__2 = i;
		for (j = 1; j <= i__2; ++j) {
		    i__3 = j + i * t_dim1;
		    t[i__3].r = 0., t[i__3].i = 0.;
/* L10: */
		}
	    } else {

/*              general case */

		i__2 = i + i * v_dim1;
		vii.r = v[i__2].r, vii.i = v[i__2].i;
		i__2 = i + i * v_dim1;
		v[i__2].r = 1., v[i__2].i = 0.;
		if (lsame_(storev, "C", 1L, 1L)) {

/*                 T(1:i-1,i) := - tau(i) * V(i:n,1:i-1)' 
* V(i:n,i) */

		    i__2 = *n - i + 1;
		    i__3 = i - 1;
		    i__4 = i;
		    z__1.r = -tau[i__4].r, z__1.i = -tau[i__4].i;
		    zgemv_("Conjugate transpose", &i__2, &i__3, &z__1, &v[i + 
			    v_dim1], ldv, &v[i + i * v_dim1], &c__1, &c_b4, &
			    t[i * t_dim1 + 1], &c__1, 19L);
		} else {

/*                 T(1:i-1,i) := - tau(i) * V(1:i-1,i:n) *
 V(i,i:n)' */

		    if (i < *n) {
			i__2 = *n - i;
			zlacgv_(&i__2, &v[i + (i + 1) * v_dim1], ldv);
		    }
		    i__2 = i - 1;
		    i__3 = *n - i + 1;
		    i__4 = i;
		    z__1.r = -tau[i__4].r, z__1.i = -tau[i__4].i;
		    zgemv_("No transpose", &i__2, &i__3, &z__1, &v[i * v_dim1 
			    + 1], ldv, &v[i + i * v_dim1], ldv, &c_b4, &t[i * 
			    t_dim1 + 1], &c__1, 12L);
		    if (i < *n) {
			i__2 = *n - i;
			zlacgv_(&i__2, &v[i + (i + 1) * v_dim1], ldv);
		    }
		}
		i__2 = i + i * v_dim1;
		v[i__2].r = vii.r, v[i__2].i = vii.i;

/*              T(1:i-1,i) := T(1:i-1,1:i-1) * T(1:i-1,i) */

		i__2 = i - 1;
		ztrmv_("Upper", "No transpose", "Non-unit", &i__2, &t[
			t_offset], ldt, &t[i * t_dim1 + 1], &c__1, 5L, 12L, 
			8L);
		i__2 = i + i * t_dim1;
		i__3 = i;
		t[i__2].r = tau[i__3].r, t[i__2].i = tau[i__3].i;
	    }
/* L20: */
	}
    } else {
	for (i = *k; i >= 1; --i) {
	    i__1 = i;
	    if (tau[i__1].r == 0. && tau[i__1].i == 0.) {

/*              H(i)  =  I */

		i__1 = *k;
		for (j = i; j <= i__1; ++j) {
		    i__2 = j + i * t_dim1;
		    t[i__2].r = 0., t[i__2].i = 0.;
/* L30: */
		}
	    } else {

/*              general case */

		if (i < *k) {
		    if (lsame_(storev, "C", 1L, 1L)) {
			i__1 = *n - *k + i + i * v_dim1;
			vii.r = v[i__1].r, vii.i = v[i__1].i;
			i__1 = *n - *k + i + i * v_dim1;
			v[i__1].r = 1., v[i__1].i = 0.;

/*                    T(i+1:k,i) := */
/*                            - tau(i) * V(1:n-k+i,i+1
:k)' * V(1:n-k+i,i) */

			i__1 = *n - *k + i;
			i__2 = *k - i;
			i__3 = i;
			z__1.r = -tau[i__3].r, z__1.i = -tau[i__3].i;
			zgemv_("Conjugate transpose", &i__1, &i__2, &z__1, &v[
				(i + 1) * v_dim1 + 1], ldv, &v[i * v_dim1 + 1]
				, &c__1, &c_b4, &t[i + 1 + i * t_dim1], &c__1,
				 19L);
			i__1 = *n - *k + i + i * v_dim1;
			v[i__1].r = vii.r, v[i__1].i = vii.i;
		    } else {
			i__1 = i + (*n - *k + i) * v_dim1;
			vii.r = v[i__1].r, vii.i = v[i__1].i;
			i__1 = i + (*n - *k + i) * v_dim1;
			v[i__1].r = 1., v[i__1].i = 0.;

/*                    T(i+1:k,i) := */
/*                            - tau(i) * V(i+1:k,1:n-k
+i) * V(i,1:n-k+i)' */

			i__1 = *n - *k + i - 1;
			zlacgv_(&i__1, &v[i + v_dim1], ldv);
			i__1 = *k - i;
			i__2 = *n - *k + i;
			i__3 = i;
			z__1.r = -tau[i__3].r, z__1.i = -tau[i__3].i;
			zgemv_("No transpose", &i__1, &i__2, &z__1, &v[i + 1 
				+ v_dim1], ldv, &v[i + v_dim1], ldv, &c_b4, &
				t[i + 1 + i * t_dim1], &c__1, 12L);
			i__1 = *n - *k + i - 1;
			zlacgv_(&i__1, &v[i + v_dim1], ldv);
			i__1 = i + (*n - *k + i) * v_dim1;
			v[i__1].r = vii.r, v[i__1].i = vii.i;
		    }

/*                 T(i+1:k,i) := T(i+1:k,i+1:k) * T(i+1:k,
i) */

		    i__1 = *k - i;
		    ztrmv_("Lower", "No transpose", "Non-unit", &i__1, &t[i + 
			    1 + (i + 1) * t_dim1], ldt, &t[i + 1 + i * t_dim1]
			    , &c__1, 5L, 12L, 8L);
		}
		i__1 = i + i * t_dim1;
		i__2 = i;
		t[i__1].r = tau[i__2].r, t[i__1].i = tau[i__2].i;
	    }
/* L40: */
	}
    }
    return 0;

/*     End of ZLARFT */

} /* zlarft_ */

