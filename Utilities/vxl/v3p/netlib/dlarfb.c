#include "f2c.h"
#include "netlib.h"

/* Table of constant values */
static integer c__1 = 1;
static doublereal c_b14 = 1.;
static doublereal c_b25 = -1.;

/* Subroutine */ void dlarfb_(side, trans, direct, storev, m, n, k, v, ldv, t, ldt, c, ldc, work, ldwork)
const char *side, *trans, *direct, *storev;
const integer *m, *n, *k;
doublereal *v;
const integer *ldv;
doublereal *t;
const integer *ldt;
doublereal *c;
const integer *ldc;
doublereal *work;
const integer *ldwork;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i, j;
    static char transt[1];

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DLARFB applies a real block reflector H or its transpose H' to a      */
/*  real m by n matrix C, from either the left or the right.              */
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
/*          = 'T': apply H' (Transpose)                                   */
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
/*  V       (input) DOUBLE PRECISION array, dimension                     */
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
/*  T       (input) DOUBLE PRECISION array, dimension (LDT,K)             */
/*          The triangular k by k matrix T in the representation of the   */
/*          block reflector.                                              */
/*                                                                        */
/*  LDT     (input) INTEGER                                               */
/*          The leading dimension of the array T. LDT >= K.               */
/*                                                                        */
/*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)      */
/*          On entry, the m by n matrix C.                                */
/*          On exit, C is overwritten by H*C or H'*C or C*H or C*H'.      */
/*                                                                        */
/*  LDC     (input) INTEGER                                               */
/*          The leading dimension of the array C. LDA >= max(1,M).        */
/*                                                                        */
/*  WORK    (workspace) DOUBLE PRECISION array, dimension (LDWORK,K)      */
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
        *(unsigned char *)transt = 'T';
    } else {
        *(unsigned char *)transt = 'N';
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
                    dcopy_(n, &c[j], ldc, &work[j * *ldwork], &c__1);
                }

/*              W := W * V1 */

                dtrmm_("Right", "Lower", "No transpose", "Unit", n, k, &c_b14, v, ldv, work, ldwork);

                if (*m > *k) {

/*                 W := W + C2'*V2 */

                    i__1 = *m - *k;
                    dgemm_("Transpose", "No transpose", n, k, &i__1, &c_b14, &c[*k], ldc, &v[*k], ldv, &c_b14, work, ldwork);
                }

/*              W := W * T'  or  W * T */

                dtrmm_("Right", "Upper", transt, "Non-unit", n, k, &c_b14, t, ldt, work, ldwork);

/*              C := C - V * W' */

                if (*m > *k) {

/*                 C2 := C2 - V2 * W' */

                    i__1 = *m - *k;
                    dgemm_("No transpose", "Transpose", &i__1, n, k, &c_b25, &v[*k], ldv, work, ldwork, &c_b14, &c[*k], ldc);
                }

/*              W := W * V1' */

                dtrmm_("Right", "Lower", "Transpose", "Unit", n, k, &c_b14, v, ldv, work, ldwork);

/*              C1 := C1 - W' */

                for (j = 0; j < *k; ++j) {
                    for (i = 0; i < *n; ++i) {
                        c[j + i * *ldc] -= work[i + j * *ldwork];
                    }
                }

            } else if (lsame_(side, "R")) {

/*              Form  C * H  or  C * H'  where  C = ( C1  C2 ) */

/*              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK) */

/*              W := C1 */

                for (j = 0; j < *k; ++j) {
                    dcopy_(m, &c[j * *ldc], &c__1, &work[j * *ldwork], &c__1);
                }

/*              W := W * V1 */

                dtrmm_("Right", "Lower", "No transpose", "Unit", m, k, &c_b14, v, ldv, work, ldwork);
                if (*n > *k) {

/*                 W := W + C2 * V2 */

                    i__1 = *n - *k;
                    dgemm_("No transpose", "No transpose", m, k, &i__1, &c_b14,
                           &c[*k * *ldc], ldc, &v[*k], ldv, &c_b14, work, ldwork);
                }

/*              W := W * T  or  W * T' */

                dtrmm_("Right", "Upper", trans, "Non-unit", m, k, &c_b14, t, ldt, work, ldwork);

/*              C := C - W * V' */

                if (*n > *k) {

/*                 C2 := C2 - W * V2' */

                    i__1 = *n - *k;
                    dgemm_("No transpose", "Transpose", m, &i__1, k, &c_b25, work, ldwork,
                           &v[*k], ldv, &c_b14, &c[*k * *ldc], ldc);
                }

/*              W := W * V1' */

                dtrmm_("Right", "Lower", "Transpose", "Unit", m, k, &c_b14, v, ldv, work, ldwork);

/*              C1 := C1 - W */

                for (j = 0; j < *k; ++j) {
                    for (i = 0; i < *m; ++i) {
                        c[i + j * *ldc] -= work[i + j * *ldwork];
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
                    dcopy_(n, &c[*m - *k + j], ldc, &work[j * *ldwork], &c__1);
                }

/*              W := W * V2 */

                dtrmm_("Right", "Upper", "No transpose", "Unit", n, k, &c_b14, &v[*m - *k], ldv, work, ldwork);

                if (*m > *k) {

/*                 W := W + C1'*V1 */

                    i__1 = *m - *k;
                    dgemm_("Transpose", "No transpose", n, k, &i__1, &c_b14, c, ldc, v, ldv, &c_b14, work, ldwork);
                }

/*              W := W * T'  or  W * T */

                dtrmm_("Right", "Lower", transt, "Non-unit", n, k, &c_b14, t, ldt, work, ldwork);

/*              C := C - V * W' */

                if (*m > *k) {

/*                 C1 := C1 - V1 * W' */

                    i__1 = *m - *k;
                    dgemm_("No transpose", "Transpose", &i__1, n, k, &c_b25, v, ldv, work, ldwork, &c_b14, c, ldc);
                }

/*              W := W * V2' */

                dtrmm_("Right", "Upper", "Transpose", "Unit", n, k, &c_b14, &v[*m - *k], ldv, work, ldwork);

/*              C2 := C2 - W' */

                for (j = 0; j < *k; ++j) {
                    for (i = 0; i < *n; ++i) {
                        c[*m - *k + j + i * *ldc] -= work[i + j * *ldwork];
                    }
                }

            } else if (lsame_(side, "R")) {

/*              Form  C * H  or  C * H'  where  C = ( C1  C2 ) */

/*              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK) */

/*              W := C2 */

                for (j = 0; j < *k; ++j) {
                    dcopy_(m, &c[(*n - *k + j) * *ldc], &c__1, &work[j * *ldwork], &c__1);
                }

/*              W := W * V2 */

                dtrmm_("Right", "Upper", "No transpose", "Unit", m, k, &c_b14, &v[*n - *k], ldv, work, ldwork);

                if (*n > *k) {

/*                 W := W + C1 * V1 */

                    i__1 = *n - *k;
                    dgemm_("No transpose", "No transpose", m, k, &i__1, &c_b14, c, ldc, v, ldv, &c_b14, work, ldwork);
                }

/*              W := W * T  or  W * T' */

                dtrmm_("Right", "Lower", trans, "Non-unit", m, k, &c_b14, t, ldt, work, ldwork);

/*              C := C - W * V' */

                if (*n > *k) {

/*                 C1 := C1 - W * V1' */

                    i__1 = *n - *k;
                    dgemm_("No transpose", "Transpose", m, &i__1, k, &c_b25, work, ldwork, v, ldv, &c_b14, c, ldc);
                }

/*              W := W * V2' */

                dtrmm_("Right", "Upper", "Transpose", "Unit", m, k, &c_b14, &v[*n - *k], ldv, work, ldwork);

/*              C2 := C2 - W */

                for (j = 0; j < *k; ++j) {
                    for (i = 0; i < *m; ++i) {
                        c[i + (*n - *k + j) * *ldc] -= work[i + j * *ldwork];
                    }
                }
            }
        }
    } else if (lsame_(storev, "R")) {

        if (lsame_(direct, "F"))
        {
/*           Let  V =  ( V1  V2 )    (V1: first K columns) */
/*           where  V1  is unit upper triangular. */

            if (lsame_(side, "L"))
            {
/*              Form  H * C  or  H' * C  where  C = ( C1 ) */
/*                                                  ( C2 ) */

/*              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK) */

/*              W := C1' */

                for (j = 0; j < *k; ++j) {
                    dcopy_(n, &c[j], ldc, &work[j * *ldwork], &c__1);
                }

/*              W := W * V1' */

                dtrmm_("Right", "Upper", "Transpose", "Unit", n, k, &c_b14, v, ldv, work, ldwork);

                if (*m > *k)
                {
/*                 W := W + C2'*V2' */

                    i__1 = *m - *k;
                    dgemm_("Transpose", "Transpose", n, k, &i__1, &c_b14, &c[*k], ldc, &v[*k * *ldv], ldv, &c_b14, work, ldwork);
                }

/*              W := W * T'  or  W * T */

                dtrmm_("Right", "Upper", transt, "Non-unit", n, k, &c_b14, t, ldt, work, ldwork);

/*              C := C - V' * W' */

                if (*m > *k) {

/*                 C2 := C2 - V2' * W' */

                    i__1 = *m - *k;
                    dgemm_("Transpose", "Transpose", &i__1, n, k, &c_b25, &v[*k * *ldv], ldv, work, ldwork, &c_b14, &c[*k], ldc);
                }

/*              W := W * V1 */

                dtrmm_("Right", "Upper", "No transpose", "Unit", n, k, &c_b14, v, ldv, work, ldwork);

/*              C1 := C1 - W' */

                for (j = 0; j < *k; ++j) {
                    for (i = 0; i < *n; ++i) {
                        c[j + i * *ldc] -= work[i + j * *ldwork];
                    }
                }

            } else if (lsame_(side, "R")) {

/*              Form  C * H  or  C * H'  where  C = ( C1  C2 ) */

/*              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK) */

/*              W := C1 */

                for (j = 0; j < *k; ++j) {
                    dcopy_(m, &c[j * *ldc], &c__1, &work[j * *ldwork], &c__1);
                }

/*              W := W * V1' */

                dtrmm_("Right", "Upper", "Transpose", "Unit", m, k, &c_b14, v, ldv, work, ldwork);

                if (*n > *k)
                {
/*                 W := W + C2 * V2' */

                    i__1 = *n - *k;
                    dgemm_("No transpose", "Transpose", m, k, &i__1, &c_b14,
                           &c[*k * *ldc], ldc, &v[*k **ldv], ldv, &c_b14, work, ldwork);
                }

/*              W := W * T  or  W * T' */

                dtrmm_("Right", "Upper", trans, "Non-unit", m, k, &c_b14, t, ldt, work, ldwork);

/*              C := C - W * V */

                if (*n > *k)
                {
/*                 C2 := C2 - W * V2 */

                    i__1 = *n - *k;
                    dgemm_("No transpose", "No transpose", m, &i__1, k, &c_b25,
                           work, ldwork, &v[*k * *ldv], ldv, &c_b14, &c[*k * *ldc], ldc);
                }

/*              W := W * V1 */

                dtrmm_("Right", "Upper", "No transpose", "Unit", m, k, &c_b14, v, ldv, work, ldwork);

/*              C1 := C1 - W */

                for (j = 0; j < *k; ++j) {
                    for (i = 0; i < *m; ++i) {
                        c[i + j * *ldc] -= work[i + j * *ldwork];
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
                    dcopy_(n, &c[*m - *k + j], ldc, &work[j * *ldwork], &c__1);
                }

/*              W := W * V2' */

                dtrmm_("Right", "Lower", "Transpose", "Unit", n, k, &c_b14, &v[(*m - *k) * *ldv], ldv, work, ldwork);

                if (*m > *k) {

/*                 W := W + C1'*V1' */

                    i__1 = *m - *k;
                    dgemm_("Transpose", "Transpose", n, k, &i__1, &c_b14, c, ldc, v, ldv, &c_b14, work, ldwork);
                }

/*              W := W * T'  or  W * T */

                dtrmm_("Right", "Lower", transt, "Non-unit", n, k, &c_b14, t, ldt, work, ldwork);

/*              C := C - V' * W' */

                if (*m > *k) {
/*                 C1 := C1 - V1' * W' */

                    i__1 = *m - *k;
                    dgemm_("Transpose", "Transpose", &i__1, n, k, &c_b25, v, ldv, work, ldwork, &c_b14, c, ldc);
                }

/*              W := W * V2 */

                dtrmm_("Right", "Lower", "No transpose", "Unit", n, k, &c_b14, &v[(*m - *k) * *ldv], ldv, work, ldwork);

/*              C2 := C2 - W' */

                for (j = 0; j < *k; ++j) {
                    for (i = 0; i < *n; ++i) {
                        c[*m - *k + j + i * *ldc] -= work[i + j * *ldwork];
                    }
                }
            } else if (lsame_(side, "R")) {

/*              Form  C * H  or  C * H'  where  C = ( C1  C2 ) */

/*              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK) */

/*              W := C2 */

                for (j = 0; j < *k; ++j) {
                    dcopy_(m, &c[(*n - *k + j) * *ldc], &c__1, &work[j * *ldwork], &c__1);
                }

/*              W := W * V2' */

                dtrmm_("Right", "Lower", "Transpose", "Unit", m, k, &c_b14, &v[(*n - *k) * *ldv], ldv, work, ldwork);

                if (*n > *k) {

/*                 W := W + C1 * V1' */

                    i__1 = *n - *k;
                    dgemm_("No transpose", "Transpose", m, k, &i__1, &c_b14, c, ldc, v, ldv, &c_b14, work, ldwork);
                }

/*              W := W * T  or  W * T' */

                dtrmm_("Right", "Lower", trans, "Non-unit", m, k, &c_b14, t, ldt, work, ldwork);

/*              C := C - W * V */

                if (*n > *k) {

/*                 C1 := C1 - W * V1 */

                    i__1 = *n - *k;
                    dgemm_("No transpose", "No transpose", m, &i__1, k, &c_b25, work, ldwork, v, ldv, &c_b14, c, ldc);
                }

/*              W := W * V2 */

                dtrmm_("Right", "Lower", "No transpose", "Unit", m, k, &c_b14, &v[(*n - *k) * *ldv], ldv, work, ldwork);

/*              C1 := C1 - W */

                for (j = 0; j < *k; ++j) {
                    for (i = 0; i < *m; ++i) {
                        c[i + (*n - *k + j) * *ldc] -= work[i + j * *ldwork];
                    }
                }
            }
        }
    }
} /* dlarfb_ */
