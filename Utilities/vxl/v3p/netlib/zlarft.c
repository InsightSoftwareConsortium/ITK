#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Table of constant values */
static doublecomplex c_b4 = {0.,0.};
static integer c__1 = 1;

/* Subroutine */ void zlarft_(direct, storev, n, k, v, ldv, tau, t, ldt)
const char *direct, *storev;
const integer *n, *k;
doublecomplex *v;
const integer *ldv;
const doublecomplex *tau;
doublecomplex *t;
integer *ldt;
{
    /* System generated locals */
    integer i__1, i__2;
    doublecomplex z__1;

    /* Local variables */
    static integer i, j;
    static doublecomplex vii;

/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZLARFT forms the triangular factor T of a complex block reflector H   */
/*  of order n, which is defined as a product of k elementary reflectors. */
/*                                                                        */
/*  If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;  */
/*                                                                        */
/*  If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.  */
/*                                                                        */
/*  If STOREV = 'C', the vector which defines the elementary reflector    */
/*  H(i) is stored in the i-th column of the array V, and                 */
/*                                                                        */
/*     H  =  I - V * T * V'                                               */
/*                                                                        */
/*  If STOREV = 'R', the vector which defines the elementary reflector    */
/*  H(i) is stored in the i-th row of the array V, and                    */
/*                                                                        */
/*     H  =  I - V' * T * V                                               */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  DIRECT  (input) CHARACTER*1                                           */
/*          Specifies the order in which the elementary reflectors are    */
/*          multiplied to form the block reflector:                       */
/*          = 'F': H = H(1) H(2) . . . H(k) (Forward)                     */
/*          = 'B': H = H(k) . . . H(2) H(1) (Backward)                    */
/*                                                                        */
/*  STOREV  (input) CHARACTER*1                                           */
/*          Specifies how the vectors which define the elementary         */
/*          reflectors are stored (see also Further Details):             */
/*          = 'C': columnwise                                             */
/*          = 'R': rowwise                                                */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the block reflector H. N >= 0.                   */
/*                                                                        */
/*  K       (input) INTEGER                                               */
/*          The order of the triangular factor T (= the number of         */
/*          elementary reflectors). K >= 1.                               */
/*                                                                        */
/*  V       (input/output) COMPLEX*16 array, dimension                    */
/*                               (LDV,K) if STOREV = 'C'                  */
/*                               (LDV,N) if STOREV = 'R'                  */
/*          The matrix V. See further details.                            */
/*                                                                        */
/*  LDV     (input) INTEGER                                               */
/*          The leading dimension of the array V.                         */
/*          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K.  */
/*                                                                        */
/*  TAU     (input) COMPLEX*16 array, dimension (K)                       */
/*          TAU(i) must contain the scalar factor of the elementary       */
/*          reflector H(i).                                               */
/*                                                                        */
/*  T       (output) COMPLEX*16 array, dimension (LDT,K)                  */
/*          The k by k triangular factor T of the block reflector.        */
/*          If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is */
/*          lower triangular. The rest of the array is not used.          */
/*                                                                        */
/*  LDT     (input) INTEGER                                               */
/*          The leading dimension of the array T. LDT >= K.               */
/*                                                                        */
/*  Further Details                                                       */
/*  ===============                                                       */
/*                                                                        */
/*  The shape of the matrix V and the storage of the vectors which define */
/*  the H(i) is best illustrated by the following example with n = 5 and  */
/*  k = 3. The elements equal to 1 are not stored; the corresponding      */
/*  array elements are modified but restored on exit. The rest of the     */
/*  array is not used.                                                    */
/*                                                                        */
/*  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R': */
/*                                                                        */
/*               V = (  1       )                 V = (  1 v1 v1 v1 v1 )  */
/*                   ( v1  1    )                     (     1 v2 v2 v2 )  */
/*                   ( v1 v2  1 )                     (        1 v3 v3 )  */
/*                   ( v1 v2 v3 )                                         */
/*                   ( v1 v2 v3 )                                         */
/*                                                                        */
/*  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R': */
/*                                                                        */
/*               V = ( v1 v2 v3 )                 V = ( v1 v1  1       )  */
/*                   ( v1 v2 v3 )                     ( v2 v2 v2  1    )  */
/*                   (  1 v2 v3 )                     ( v3 v3 v3 v3  1 )  */
/*                   (     1 v3 )                                         */
/*                   (        1 )                                         */
/*                                                                        */
/*  ===================================================================== */

/*     Quick return if possible */
    if (*n == 0) {
        return;
    }

    if (lsame_(direct, "F")) {
        for (i = 0; i < *k; ++i) {
            if (tau[i].r == 0. && tau[i].i == 0.) {

/*              H(i)  =  I */

                for (j = 0; j <= i; ++j) {
                    i__1 = j + i * *ldt;
                    t[i__1].r = 0., t[i__1].i = 0.;
                }
            } else {

/*              general case */

                i__2 = i + i * *ldv;
                vii.r = v[i__2].r, vii.i = v[i__2].i;
                i__2 = i + i * *ldv;
                v[i__2].r = 1., v[i__2].i = 0.;
                if (lsame_(storev, "C")) {

/*                 T(1:i-1,i) := - tau(i) * V(i:n,1:i-1)' * V(i:n,i) */

                    z__1.r = -tau[i].r, z__1.i = -tau[i].i;
                    i__1 = *n - i;
                    zgemv_("Conjugate transpose", &i__1, &i, &z__1,
                           &v[i], ldv, &v[i + i * *ldv], &c__1, &c_b4,
                           &t[i * *ldt], &c__1);
                } else {

/*                 T(1:i-1,i) := - tau(i) * V(1:i-1,i:n) * V(i,i:n)' */

                    if (i < *n-1) {
                        i__2 = *n-1 - i;
                        zlacgv_(&i__2, &v[i + (i + 1) * *ldv], ldv);
                    }
                    z__1.r = -tau[i].r, z__1.i = -tau[i].i;
                    i__2 = *n - i;
                    zgemv_("No transpose", &i, &i__2, &z__1, &v[i* *ldv], ldv,
                           &v[i+i* *ldv], ldv, &c_b4, &t[i* *ldt], &c__1);
                    if (i < *n-1) {
                        i__2 = *n-1 - i;
                        zlacgv_(&i__2, &v[i + (i + 1) * *ldv], ldv);
                    }
                }
                i__2 = i + i * *ldv;
                v[i__2].r = vii.r, v[i__2].i = vii.i;

/*              T(1:i-1,i) := T(1:i-1,1:i-1) * T(1:i-1,i) */

                ztrmv_("Upper", "No transpose", "Non-unit", &i, t, ldt, &t[i * *ldt], &c__1);
                i__2 = i + i * *ldt;
                t[i__2].r = tau[i].r, t[i__2].i = tau[i].i;
            }
        }
    } else {
        for (i = *k-1; i >= 0; --i) {
            if (tau[i].r == 0. && tau[i].i == 0.) {

/*              H(i)  =  I */

                for (j = i; j < *k; ++j) {
                    i__2 = j + i * *ldt;
                    t[i__2].r = 0., t[i__2].i = 0.;
                }
            } else {

/*              general case */

                if (i < *k-1) {
                    if (lsame_(storev, "C")) {
                        i__1 = *n - *k + i + i * *ldv;
                        vii.r = v[i__1].r, vii.i = v[i__1].i;
                        v[i__1].r = 1., v[i__1].i = 0.;

/*                    T(i+1:k,i) := */
/*                            - tau(i) * V(1:n-k+i,i+1:k)' * V(1:n-k+i,i) */

                        z__1.r = -tau[i].r, z__1.i = -tau[i].i;
                        i__1 = *n - *k + i + 1;
                        i__2 = *k - i - 1;
                        zgemv_("Conjugate transpose", &i__1, &i__2, &z__1,
                               &v[(i + 1) * *ldv], ldv, &v[i * *ldv], &c__1,
                               &c_b4, &t[i + 1 + i * *ldt], &c__1);
                        i__1 = *n - *k + i + i * *ldv;
                        v[i__1].r = vii.r, v[i__1].i = vii.i;
                    } else {
                        i__1 = i + (*n - *k + i) * *ldv;
                        vii.r = v[i__1].r, vii.i = v[i__1].i;
                        v[i__1].r = 1., v[i__1].i = 0.;

/*                    T(i+1:k,i) := */
/*                            - tau(i) * V(i+1:k,1:n-k+i) * V(i,1:n-k+i)' */

                        i__1 = *n - *k + i;
                        zlacgv_(&i__1, &v[i], ldv);
                        z__1.r = -tau[i].r, z__1.i = -tau[i].i;
                        i__1 = *k - i - 1;
                        i__2 = *n - *k + i + 1;
                        zgemv_("No transpose", &i__1, &i__2, &z__1, &v[i+1], ldv,
                               &v[i], ldv, &c_b4, &t[i+1 + i* *ldt], &c__1);
                        i__1 = *n - *k + i;
                        zlacgv_(&i__1, &v[i], ldv);
                        i__1 = i + (*n - *k + i) * *ldv;
                        v[i__1].r = vii.r, v[i__1].i = vii.i;
                    }

/*                 T(i+1:k,i) := T(i+1:k,i+1:k) * T(i+1:k,i) */

                    i__1 = *k - i - 1;
                    ztrmv_("Lower", "No transpose", "Non-unit", &i__1,
                           &t[i + 1 + (i + 1) * *ldt], ldt,
                           &t[i + 1 + i * *ldt], &c__1);
                }
                i__1 = i + i * *ldt;
                t[i__1].r = tau[i].r, t[i__1].i = tau[i].i;
            }
        }
    }
} /* zlarft_ */
