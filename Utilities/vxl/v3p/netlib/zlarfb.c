#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Table of constant values */
static integer c__1 = 1;
static doublecomplex c_b15 = {1.,0.};
static doublecomplex c_b26 = {-1.,0.};

/* Subroutine */ void zlarfb_(side, trans, direct, storev, m, n, k, v, ldv, t, ldt, c, ldc, work, ldwork)
const char *side, *trans, *direct, *storev;
const integer *m, *n, *k;
doublecomplex *v;
const integer *ldv;
doublecomplex *t;
const integer *ldt;
doublecomplex *c;
const integer *ldc;
doublecomplex *work;
const integer *ldwork;
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i, j;
    static char transt[1];

/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZLARFB applies a complex block reflector H or its transpose H' to a   */
/*  complex M-by-N matrix C, from either the left or the right.           */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  SIDE    (input) CHARACTER*1                                           */
/*          = 'L': apply H or H' from the Left                            */
/*          = 'R': apply H or H' from the Right                           */
/*                                                                        */
/*  TRANS   (input) CHARACTER*1                                           */
/*          = 'N': apply H (No transpose)                                 */
/*          = 'C': apply H' (Conjugate transpose)                         */
/*                                                                        */
/*  DIRECT  (input) CHARACTER*1                                           */
/*          Indicates how H is formed from a product of elementary        */
/*          reflectors                                                    */
/*          = 'F': H = H(1) H(2) . . . H(k) (Forward)                     */
/*          = 'B': H = H(k) . . . H(2) H(1) (Backward)                    */
/*                                                                        */
/*  STOREV  (input) CHARACTER*1                                           */
/*          Indicates how the vectors which define the elementary         */
/*          reflectors are stored:                                        */
/*          = 'C': Columnwise                                             */
/*          = 'R': Rowwise                                                */
/*                                                                        */
/*  M       (input) INTEGER                                               */
/*          The number of rows of the matrix C.                           */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The number of columns of the matrix C.                        */
/*                                                                        */
/*  K       (input) INTEGER                                               */
/*          The order of the matrix T (= the number of elementary         */
/*          reflectors whose product defines the block reflector).        */
/*                                                                        */
/*  V       (input) COMPLEX*16 array, dimension                           */
/*                                (LDV,K) if STOREV = 'C'                 */
/*                                (LDV,M) if STOREV = 'R' and SIDE = 'L'  */
/*                                (LDV,N) if STOREV = 'R' and SIDE = 'R'  */
/*          The matrix V. See further details.                            */
/*                                                                        */
/*  LDV     (input) INTEGER                                               */
/*          The leading dimension of the array V.                         */
/*          If STOREV = 'C' and SIDE = 'L', LDV >= max(1,M);              */
/*          if STOREV = 'C' and SIDE = 'R', LDV >= max(1,N);              */
/*          if STOREV = 'R', LDV >= K.                                    */
/*                                                                        */
/*  T       (input) COMPLEX*16 array, dimension (LDT,K)                   */
/*          The triangular K-by-K matrix T in the representation of the   */
/*          block reflector.                                              */
/*                                                                        */
/*  LDT     (input) INTEGER                                               */
/*          The leading dimension of the array T. LDT >= K.               */
/*                                                                        */
/*  C       (input/output) COMPLEX*16 array, dimension (LDC,N)            */
/*          On entry, the M-by-N matrix C.                                */
/*          On exit, C is overwritten by H*C or H'*C or C*H or C*H'.      */
/*                                                                        */
/*  LDC     (input) INTEGER                                               */
/*          The leading dimension of the array C. LDC >= max(1,M).        */
/*                                                                        */
/*  WORK    (workspace) COMPLEX*16 array, dimension (LDWORK,K)            */
/*                                                                        */
/*  LDWORK  (input) INTEGER                                               */
/*          The leading dimension of the array WORK.                      */
/*          If SIDE = 'L', LDWORK >= max(1,N);                            */
/*          if SIDE = 'R', LDWORK >= max(1,M).                            */
/*                                                                        */
/*  ===================================================================== */

/*     Quick return if possible */
    if (*m <= 0 || *n <= 0) {
        return;
    }

    if (lsame_(trans, "N")) {
        *transt = 'C';
    } else {
        *transt = 'N';
    }

    if (lsame_(storev, "C")) {

        if (lsame_(direct, "F")) {

/*           Let  V =  ( V1 )    (first K rows) */
/*                     ( V2 ) */
/*           where  V1  is unit lower triangular. */

            if (lsame_(side, "L")) {

/*              Form  H * C  or  H' * C  where  C = ( C1 ) */
/*                                                  ( C2 ) */

/*              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK) */

/*              W := C1' */

                for (j = 0; j < *k; ++j) {
                    zcopy_(n, &c[j], ldc, &work[j* *ldwork], &c__1);
                    zlacgv_(n, &work[j* *ldwork], &c__1);
                }

/*              W := W * V1 */

                ztrmm_("Right", "Lower", "No transpose", "Unit", n, k, &c_b15, v, ldv, work, ldwork);
                if (*m > *k) {

/*                 W := W + C2'*V2 */

                    i__1 = *m - *k;
                    zgemm_("Conjugate transpose", "No transpose", n, k, &i__1,
                           &c_b15, &c[*k], ldc, &v[*k], ldv, &c_b15, work, ldwork);
                }

/*              W := W * T'  or  W * T */

                ztrmm_("Right", "Upper", transt, "Non-unit", n, k, &c_b15, t, ldt, work, ldwork);

/*              C := C - V * W' */

                if (*m > *k) {

/*                 C2 := C2 - V2 * W' */

                    i__1 = *m - *k;
                    zgemm_("No transpose", "Conjugate transpose", &i__1, n, k,
                           &c_b26, &v[*k], ldv, work, ldwork, &c_b15, &c[*k], ldc);
                }

/*              W := W * V1' */

                ztrmm_("Right", "Lower", "Conjugate transpose", "Unit", n, k, &c_b15, v, ldv, work, ldwork);

/*              C1 := C1 - W' */

                for (j = 0; j < *k; ++j) {
                    for (i = 0; i < *n; ++i) {
                        i__1 = j + i * *ldc;
                        i__2 = i + j * *ldwork;
                        c[i__1].r -= work[i__2].r, c[i__1].i += work[i__2].i;
                    }
                }

            } else if (lsame_(side, "R")) {

/*              Form  C * H  or  C * H'  where  C = ( C1  C2 ) */

/*              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK) */

/*              W := C1 */

                for (j = 0; j < *k; ++j) {
                    zcopy_(m, &c[j* *ldc], &c__1, &work[j* *ldwork], &c__1);
                }

/*              W := W * V1 */

                ztrmm_("Right", "Lower", "No transpose", "Unit", m, k, &c_b15, v, ldv, work, ldwork);
                if (*n > *k) {

/*                 W := W + C2 * V2 */

                    i__1 = *n - *k;
                    zgemm_("No transpose", "No transpose", m, k, &i__1,
                           &c_b15, &c[*k * *ldc], ldc, &v[*k], ldv, &c_b15, work, ldwork);
                }

/*              W := W * T  or  W * T' */

                ztrmm_("Right", "Upper", trans, "Non-unit", m, k, &c_b15, t, ldt, work, ldwork);

/*              C := C - W * V' */

                if (*n > *k) {

/*                 C2 := C2 - W * V2' */

                    i__1 = *n - *k;
                    zgemm_("No transpose", "Conjugate transpose", m, &i__1, k,
                           &c_b26, work, ldwork, &v[*k], ldv, &c_b15, &c[*k * *ldc], ldc);
                }

/*              W := W * V1' */

                ztrmm_("Right", "Lower", "Conjugate transpose", "Unit", m, k, &c_b15, v, ldv, work, ldwork);

/*              C1 := C1 - W */

                for (j = 0; j < *k; ++j) {
                    for (i = 0; i < *m; ++i) {
                        i__1 = i + j * *ldc;
                        i__2 = i + j * *ldwork;
                        c[i__1].r -= work[i__2].r, c[i__1].i -= work[i__2].i;
                    }
                }
            }

        } else {

/*           Let  V =  ( V1 ) */
/*                     ( V2 )    (last K rows) */
/*           where  V2  is unit upper triangular. */

            if (lsame_(side, "L")) {

/*              Form  H * C  or  H' * C  where  C = ( C1 ) */
/*                                                  ( C2 ) */

/*              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK) */

/*              W := C2' */

                for (j = 0; j < *k; ++j) {
                    zcopy_(n, &c[*m - *k + j], ldc, &work[j* *ldwork], &c__1);
                    zlacgv_(n, &work[j* *ldwork], &c__1);
                }

/*              W := W * V2 */

                ztrmm_("Right", "Upper", "No transpose", "Unit", n, k, &c_b15, &v[*m - *k], ldv, work, ldwork);
                if (*m > *k) {

/*                 W := W + C1'*V1 */

                    i__1 = *m - *k;
                    zgemm_("Conjugate transpose", "No transpose", n, k, &i__1,
                           &c_b15, c, ldc, v, ldv, &c_b15, work, ldwork);
                }

/*              W := W * T'  or  W * T */

                ztrmm_("Right", "Lower", transt, "Non-unit", n, k, &c_b15, t, ldt, work, ldwork);

/*              C := C - V * W' */

                if (*m > *k) {

/*                 C1 := C1 - V1 * W' */

                    i__1 = *m - *k;
                    zgemm_("No transpose", "Conjugate transpose", &i__1, n, k,
                           &c_b26, v, ldv, work, ldwork, &c_b15, c, ldc);
                }

/*              W := W * V2' */

                ztrmm_("Right", "Upper", "Conjugate transpose", "Unit", n, k, &c_b15, &v[*m - *k], ldv, work, ldwork);

/*              C2 := C2 - W' */

                for (j = 0; j < *k; ++j) {
                    for (i = 0; i < *n; ++i) {
                        i__1 = *m - *k + j + i * *ldc;
                        i__2 = i + j * *ldwork;
                        c[i__1].r -= work[i__2].r, c[i__1].i += work[i__2].i;
                    }
                }

            } else if (lsame_(side, "R")) {

/*              Form  C * H  or  C * H'  where  C = ( C1  C2 ) */

/*              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK) */

/*              W := C2 */

                for (j = 0; j < *k; ++j) {
                    zcopy_(m, &c[(*n - *k + j) * *ldc], &c__1, &work[j* *ldwork], &c__1);
                }

/*              W := W * V2 */

                ztrmm_("Right", "Upper", "No transpose", "Unit", m, k, &c_b15, &v[*n - *k], ldv, work, ldwork);
                if (*n > *k) {

/*                 W := W + C1 * V1 */

                    i__1 = *n - *k;
                    zgemm_("No transpose", "No transpose", m, k, &i__1, &c_b15,
                           c, ldc, v, ldv, &c_b15, work, ldwork);
                }

/*              W := W * T  or  W * T' */

                ztrmm_("Right", "Lower", trans, "Non-unit", m, k, &c_b15, t, ldt, work, ldwork);

/*              C := C - W * V' */

                if (*n > *k) {

/*                 C1 := C1 - W * V1' */

                    i__1 = *n - *k;
                    zgemm_("No transpose", "Conjugate transpose", m, &i__1, k,
                           &c_b26, work, ldwork, v, ldv, &c_b15, c, ldc);
                }

/*              W := W * V2' */

                ztrmm_("Right", "Upper", "Conjugate transpose", "Unit", m, k, &c_b15, &v[*n - *k], ldv, work, ldwork);

/*              C2 := C2 - W */

                for (j = 0; j < *k; ++j) {
                    for (i = 0; i < *m; ++i) {
                        i__1 = i + (*n - *k + j) * *ldc;
                        i__2 = i + j * *ldwork;
                        c[i__1].r -= work[i__2].r, c[i__1].i -= work[i__2].i;
                    }
                }
            }
        }

    } else if (lsame_(storev, "R")) {

        if (lsame_(direct, "F")) {

/*           Let  V =  ( V1  V2 )    (V1: first K columns) */
/*           where  V1  is unit upper triangular. */

            if (lsame_(side, "L")) {

/*              Form  H * C  or  H' * C  where  C = ( C1 ) */
/*                                                  ( C2 ) */

/*              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK) */

/*              W := C1' */

                for (j = 0; j < *k; ++j) {
                    zcopy_(n, &c[j], ldc, &work[j* *ldwork], &c__1);
                    zlacgv_(n, &work[j* *ldwork], &c__1);
                }

/*              W := W * V1' */

                ztrmm_("Right", "Upper", "Conjugate transpose", "Unit", n, k, &c_b15, v, ldv, work, ldwork);
                if (*m > *k) {

/*                 W := W + C2'*V2' */

                    i__1 = *m - *k;
                    zgemm_("Conjugate transpose", "Conjugate transpose", n, k,
                           &i__1, &c_b15, &c[*k], ldc, &v[*k * *ldv], ldv, &c_b15, work, ldwork);
                }

/*              W := W * T'  or  W * T */

                ztrmm_("Right", "Upper", transt, "Non-unit", n, k, &c_b15, t, ldt, work, ldwork);

/*              C := C - V' * W' */

                if (*m > *k) {

/*                 C2 := C2 - V2' * W' */

                    i__1 = *m - *k;
                    zgemm_("Conjugate transpose", "Conjugate transpose", &i__1,
                           n, k, &c_b26, &v[*k * *ldv], ldv, work, ldwork, &c_b15, &c[*k], ldc);
                }

/*              W := W * V1 */

                ztrmm_("Right", "Upper", "No transpose", "Unit", n, k, &c_b15, v, ldv, work, ldwork);

/*              C1 := C1 - W' */

                for (j = 0; j < *k; ++j) {
                    for (i = 0; i < *n; ++i) {
                        i__1 = j + i * *ldc;
                        i__2 = i + j * *ldwork;
                        c[i__1].r -= work[i__2].r, c[i__1].i += work[i__2].i;
                    }
                }

            } else if (lsame_(side, "R")) {

/*              Form  C * H  or  C * H'  where  C = ( C1  C2 ) */

/*              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK) */

/*              W := C1 */

                for (j = 0; j < *k; ++j) {
                    zcopy_(m, &c[j * *ldc], &c__1, &work[j* *ldwork], &c__1);
                }

/*              W := W * V1' */

                ztrmm_("Right", "Upper", "Conjugate transpose", "Unit", m, k, &c_b15, v, ldv, work, ldwork);
                if (*n > *k) {

/*                 W := W + C2 * V2' */

                    i__1 = *n - *k;
                    zgemm_("No transpose", "Conjugate transpose", m, k, &i__1,
                           &c_b15, &c[*k * *ldc], ldc, &v[*k * *ldv], ldv, &c_b15, work, ldwork);
                }

/*              W := W * T  or  W * T' */

                ztrmm_("Right", "Upper", trans, "Non-unit", m, k, &c_b15, t, ldt, work, ldwork);

/*              C := C - W * V */

                if (*n > *k) {

/*                 C2 := C2 - W * V2 */

                    i__1 = *n - *k;
                    zgemm_("No transpose", "No transpose", m, &i__1, k, &c_b26,
                           work, ldwork, &v[*k * *ldv], ldv, &c_b15, &c[*k * *ldc], ldc);
                }

/*              W := W * V1 */

                ztrmm_("Right", "Upper", "No transpose", "Unit", m, k, &c_b15, v, ldv, work, ldwork);

/*              C1 := C1 - W */

                for (j = 0; j < *k; ++j) {
                    for (i = 0; i < *m; ++i) {
                        i__1 = i + j * *ldc;
                        i__2 = i + j * *ldwork;
                        c[i__1].r -= work[i__2].r, c[i__1].i -= work[i__2].i;
                    }
                }
            }

        } else {

/*           Let  V =  ( V1  V2 )    (V2: last K columns) */
/*           where  V2  is unit lower triangular. */

            if (lsame_(side, "L")) {

/*              Form  H * C  or  H' * C  where  C = ( C1 ) */
/*                                                  ( C2 ) */

/*              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK) */

/*              W := C2' */

                for (j = 0; j < *k; ++j) {
                    zcopy_(n, &c[*m - *k + j], ldc, &work[j* *ldwork], &c__1);
                    zlacgv_(n, &work[j* *ldwork], &c__1);
                }

/*              W := W * V2' */

                ztrmm_("Right", "Lower", "Conjugate transpose", "Unit", n, k, &c_b15, &v[(*m - *k) * *ldv], ldv, work, ldwork);
                if (*m > *k) {

/*                 W := W + C1'*V1' */

                    i__1 = *m - *k;
                    zgemm_("Conjugate transpose", "Conjugate transpose", n, k,
                           &i__1, &c_b15, c, ldc, v, ldv, &c_b15, work, ldwork);
                }

/*              W := W * T'  or  W * T */

                ztrmm_("Right", "Lower", transt, "Non-unit", n, k, &c_b15, t, ldt, work, ldwork);

/*              C := C - V' * W' */

                if (*m > *k) {

/*                 C1 := C1 - V1' * W' */

                    i__1 = *m - *k;
                    zgemm_("Conjugate transpose", "Conjugate transpose",
                           &i__1, n, k, &c_b26, v, ldv, work, ldwork, &c_b15, c, ldc);
                }

/*              W := W * V2 */

                ztrmm_("Right", "Lower", "No transpose", "Unit", n, k, &c_b15, &v[(*m - *k) * *ldv], ldv, work, ldwork);

/*              C2 := C2 - W' */

                for (j = 0; j < *k; ++j) {
                    for (i = 0; i < *n; ++i) {
                        i__1 = *m - *k + j + i * *ldc;
                        i__2 = i + j * *ldwork;
                        c[i__1].r -= work[i__2].r, c[i__1].i += work[i__2].i;
                    }
                }

            } else if (lsame_(side, "R")) {

/*              Form  C * H  or  C * H'  where  C = ( C1  C2 ) */

/*              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK) */

/*              W := C2 */

                for (j = 0; j < *k; ++j) {
                    zcopy_(m, &c[(*n - *k + j) * *ldc], &c__1, &work[j* *ldwork], &c__1);
                }

/*              W := W * V2' */

                ztrmm_("Right", "Lower", "Conjugate transpose", "Unit", m, k, &c_b15, &v[(*n - *k) * *ldv], ldv, work, ldwork);
                if (*n > *k) {

/*                 W := W + C1 * V1' */

                    i__1 = *n - *k;
                    zgemm_("No transpose", "Conjugate transpose", m, k, &i__1,
                           &c_b15, c, ldc, v, ldv, &c_b15, work, ldwork);
                }

/*              W := W * T  or  W * T' */

                ztrmm_("Right", "Lower", trans, "Non-unit", m, k, &c_b15, t, ldt, work, ldwork);

/*              C := C - W * V */

                if (*n > *k) {

/*                 C1 := C1 - W * V1 */

                    i__1 = *n - *k;
                    zgemm_("No transpose", "No transpose", m, &i__1, k,
                           &c_b26, work, ldwork, v, ldv, &c_b15, c, ldc);
                }

/*              W := W * V2 */

                ztrmm_("Right", "Lower", "No transpose", "Unit", m, k, &c_b15, &v[(*n - *k) * *ldv], ldv, work, ldwork);

/*              C1 := C1 - W */

                for (j = 0; j < *k; ++j) {
                    for (i = 0; i < *m; ++i) {
                        i__1 = i + (*n - *k + j) * *ldc;
                        i__2 = i + j * *ldwork;
                        c[i__1].r -= work[i__2].r, c[i__1].i -= work[i__2].i;
                    }
                }
            }
        }
    }
} /* zlarfb_ */
