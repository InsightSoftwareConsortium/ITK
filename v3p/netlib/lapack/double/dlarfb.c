/* lapack/double/dlarfb.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
        on Microsoft Windows system, link with libf2c.lib;
        on Linux or Unix systems, link with .../path/to/libf2c.a -lm
        or, if you install libf2c.a in a standard place, with -lf2c -lm
        -- in that order, at the end of the command line, as in
                cc *.o -lf2c -lm
        Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

                http://www.netlib.org/f2c/libf2c.zip
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "v3p_netlib.h"

/* Table of constant values */

static integer c__1 = 1;
static doublereal c_b14 = 1.;
static doublereal c_b25 = -1.;

/*<    >*/
/* Subroutine */ int dlarfb_(char *side, char *trans, char *direct, char *
        storev, integer *m, integer *n, integer *k, doublereal *v, integer *
        ldv, doublereal *t, integer *ldt, doublereal *c__, integer *ldc,
        doublereal *work, integer *ldwork, ftnlen side_len, ftnlen trans_len,
        ftnlen direct_len, ftnlen storev_len)
{
    /* System generated locals */
    integer c_dim1, c_offset, t_dim1, t_offset, v_dim1, v_offset, work_dim1,
            work_offset, i__1, i__2;

    /* Local variables */
    integer i__, j;
    extern /* Subroutine */ int dgemm_(char *, char *, integer *, integer *,
            integer *, doublereal *, doublereal *, integer *, doublereal *,
            integer *, doublereal *, doublereal *, integer *, ftnlen, ftnlen);
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *,
            doublereal *, integer *), dtrmm_(char *, char *, char *, char *,
            integer *, integer *, doublereal *, doublereal *, integer *,
            doublereal *, integer *, ftnlen, ftnlen, ftnlen, ftnlen);
    char transt[1];
    (void)side_len;
    (void)trans_len;
    (void)direct_len;
    (void)storev_len;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          DIRECT, SIDE, STOREV, TRANS >*/
/*<       INTEGER            K, LDC, LDT, LDV, LDWORK, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLARFB applies a real block reflector H or its transpose H' to a */
/*  real m by n matrix C, from either the left or the right. */

/*  Arguments */
/*  ========= */

/*  SIDE    (input) CHARACTER*1 */
/*          = 'L': apply H or H' from the Left */
/*          = 'R': apply H or H' from the Right */

/*  TRANS   (input) CHARACTER*1 */
/*          = 'N': apply H (No transpose) */
/*          = 'T': apply H' (Transpose) */

/*  DIRECT  (input) CHARACTER*1 */
/*          Indicates how H is formed from a product of elementary */
/*          reflectors */
/*          = 'F': H = H(1) H(2) . . . H(k) (Forward) */
/*          = 'B': H = H(k) . . . H(2) H(1) (Backward) */

/*  STOREV  (input) CHARACTER*1 */
/*          Indicates how the vectors which define the elementary */
/*          reflectors are stored: */
/*          = 'C': Columnwise */
/*          = 'R': Rowwise */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix C. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix C. */

/*  K       (input) INTEGER */
/*          The order of the matrix T (= the number of elementary */
/*          reflectors whose product defines the block reflector). */

/*  V       (input) DOUBLE PRECISION array, dimension */
/*                                (LDV,K) if STOREV = 'C' */
/*                                (LDV,M) if STOREV = 'R' and SIDE = 'L' */
/*                                (LDV,N) if STOREV = 'R' and SIDE = 'R' */
/*          The matrix V. See further details. */

/*  LDV     (input) INTEGER */
/*          The leading dimension of the array V. */
/*          If STOREV = 'C' and SIDE = 'L', LDV >= max(1,M); */
/*          if STOREV = 'C' and SIDE = 'R', LDV >= max(1,N); */
/*          if STOREV = 'R', LDV >= K. */

/*  T       (input) DOUBLE PRECISION array, dimension (LDT,K) */
/*          The triangular k by k matrix T in the representation of the */
/*          block reflector. */

/*  LDT     (input) INTEGER */
/*          The leading dimension of the array T. LDT >= K. */

/*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N) */
/*          On entry, the m by n matrix C. */
/*          On exit, C is overwritten by H*C or H'*C or C*H or C*H'. */

/*  LDC     (input) INTEGER */
/*          The leading dimension of the array C. LDA >= max(1,M). */

/*  WORK    (workspace) DOUBLE PRECISION array, dimension (LDWORK,K) */

/*  LDWORK  (input) INTEGER */
/*          The leading dimension of the array WORK. */
/*          If SIDE = 'L', LDWORK >= max(1,N); */
/*          if SIDE = 'R', LDWORK >= max(1,M). */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ONE >*/
/*<       PARAMETER          ( ONE = 1.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       CHARACTER          TRANST >*/
/*<       INTEGER            I, J >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DCOPY, DGEMM, DTRMM >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Quick return if possible */

/*<    >*/
    /* Parameter adjustments */
    v_dim1 = *ldv;
    v_offset = 1 + v_dim1;
    v -= v_offset;
    t_dim1 = *ldt;
    t_offset = 1 + t_dim1;
    t -= t_offset;
    c_dim1 = *ldc;
    c_offset = 1 + c_dim1;
    c__ -= c_offset;
    work_dim1 = *ldwork;
    work_offset = 1 + work_dim1;
    work -= work_offset;

    /* Function Body */
    if (*m <= 0 || *n <= 0) {
        return 0;
    }

/*<       IF( LSAME( TRANS, 'N' ) ) THEN >*/
    if (lsame_(trans, "N", (ftnlen)1, (ftnlen)1)) {
/*<          TRANST = 'T' >*/
        *(unsigned char *)transt = 'T';
/*<       ELSE >*/
    } else {
/*<          TRANST = 'N' >*/
        *(unsigned char *)transt = 'N';
/*<       END IF >*/
    }

/*<       IF( LSAME( STOREV, 'C' ) ) THEN >*/
    if (lsame_(storev, "C", (ftnlen)1, (ftnlen)1)) {

/*<          IF( LSAME( DIRECT, 'F' ) ) THEN >*/
        if (lsame_(direct, "F", (ftnlen)1, (ftnlen)1)) {

/*           Let  V =  ( V1 )    (first K rows) */
/*                     ( V2 ) */
/*           where  V1  is unit lower triangular. */

/*<             IF( LSAME( SIDE, 'L' ) ) THEN >*/
            if (lsame_(side, "L", (ftnlen)1, (ftnlen)1)) {

/*              Form  H * C  or  H' * C  where  C = ( C1 ) */
/*                                                  ( C2 ) */

/*              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK) */

/*              W := C1' */

/*<                DO 10 J = 1, K >*/
                i__1 = *k;
                for (j = 1; j <= i__1; ++j) {
/*<                   CALL DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 ) >*/
                    dcopy_(n, &c__[j + c_dim1], ldc, &work[j * work_dim1 + 1],
                             &c__1);
/*<    10          CONTINUE >*/
/* L10: */
                }

/*              W := W * V1 */

/*<    >*/
                dtrmm_("Right", "Lower", "No transpose", "Unit", n, k, &c_b14,
                         &v[v_offset], ldv, &work[work_offset], ldwork, (
                        ftnlen)5, (ftnlen)5, (ftnlen)12, (ftnlen)4);
/*<                IF( M.GT.K ) THEN >*/
                if (*m > *k) {

/*                 W := W + C2'*V2 */

/*<    >*/
                    i__1 = *m - *k;
                    dgemm_("Transpose", "No transpose", n, k, &i__1, &c_b14, &
                            c__[*k + 1 + c_dim1], ldc, &v[*k + 1 + v_dim1],
                            ldv, &c_b14, &work[work_offset], ldwork, (ftnlen)
                            9, (ftnlen)12);
/*<                END IF >*/
                }

/*              W := W * T'  or  W * T */

/*<    >*/
                dtrmm_("Right", "Upper", transt, "Non-unit", n, k, &c_b14, &t[
                        t_offset], ldt, &work[work_offset], ldwork, (ftnlen)5,
                         (ftnlen)5, (ftnlen)1, (ftnlen)8);

/*              C := C - V * W' */

/*<                IF( M.GT.K ) THEN >*/
                if (*m > *k) {

/*                 C2 := C2 - V2 * W' */

/*<    >*/
                    i__1 = *m - *k;
                    dgemm_("No transpose", "Transpose", &i__1, n, k, &c_b25, &
                            v[*k + 1 + v_dim1], ldv, &work[work_offset],
                            ldwork, &c_b14, &c__[*k + 1 + c_dim1], ldc, (
                            ftnlen)12, (ftnlen)9);
/*<                END IF >*/
                }

/*              W := W * V1' */

/*<    >*/
                dtrmm_("Right", "Lower", "Transpose", "Unit", n, k, &c_b14, &
                        v[v_offset], ldv, &work[work_offset], ldwork, (ftnlen)
                        5, (ftnlen)5, (ftnlen)9, (ftnlen)4);

/*              C1 := C1 - W' */

/*<                DO 30 J = 1, K >*/
                i__1 = *k;
                for (j = 1; j <= i__1; ++j) {
/*<                   DO 20 I = 1, N >*/
                    i__2 = *n;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( J, I ) = C( J, I ) - WORK( I, J ) >*/
                        c__[j + i__ * c_dim1] -= work[i__ + j * work_dim1];
/*<    20             CONTINUE >*/
/* L20: */
                    }
/*<    30          CONTINUE >*/
/* L30: */
                }

/*<             ELSE IF( LSAME( SIDE, 'R' ) ) THEN >*/
            } else if (lsame_(side, "R", (ftnlen)1, (ftnlen)1)) {

/*              Form  C * H  or  C * H'  where  C = ( C1  C2 ) */

/*              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK) */

/*              W := C1 */

/*<                DO 40 J = 1, K >*/
                i__1 = *k;
                for (j = 1; j <= i__1; ++j) {
/*<                   CALL DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 ) >*/
                    dcopy_(m, &c__[j * c_dim1 + 1], &c__1, &work[j *
                            work_dim1 + 1], &c__1);
/*<    40          CONTINUE >*/
/* L40: */
                }

/*              W := W * V1 */

/*<    >*/
                dtrmm_("Right", "Lower", "No transpose", "Unit", m, k, &c_b14,
                         &v[v_offset], ldv, &work[work_offset], ldwork, (
                        ftnlen)5, (ftnlen)5, (ftnlen)12, (ftnlen)4);
/*<                IF( N.GT.K ) THEN >*/
                if (*n > *k) {

/*                 W := W + C2 * V2 */

/*<    >*/
                    i__1 = *n - *k;
                    dgemm_("No transpose", "No transpose", m, k, &i__1, &
                            c_b14, &c__[(*k + 1) * c_dim1 + 1], ldc, &v[*k +
                            1 + v_dim1], ldv, &c_b14, &work[work_offset],
                            ldwork, (ftnlen)12, (ftnlen)12);
/*<                END IF >*/
                }

/*              W := W * T  or  W * T' */

/*<    >*/
                dtrmm_("Right", "Upper", trans, "Non-unit", m, k, &c_b14, &t[
                        t_offset], ldt, &work[work_offset], ldwork, (ftnlen)5,
                         (ftnlen)5, (ftnlen)1, (ftnlen)8);

/*              C := C - W * V' */

/*<                IF( N.GT.K ) THEN >*/
                if (*n > *k) {

/*                 C2 := C2 - W * V2' */

/*<    >*/
                    i__1 = *n - *k;
                    dgemm_("No transpose", "Transpose", m, &i__1, k, &c_b25, &
                            work[work_offset], ldwork, &v[*k + 1 + v_dim1],
                            ldv, &c_b14, &c__[(*k + 1) * c_dim1 + 1], ldc, (
                            ftnlen)12, (ftnlen)9);
/*<                END IF >*/
                }

/*              W := W * V1' */

/*<    >*/
                dtrmm_("Right", "Lower", "Transpose", "Unit", m, k, &c_b14, &
                        v[v_offset], ldv, &work[work_offset], ldwork, (ftnlen)
                        5, (ftnlen)5, (ftnlen)9, (ftnlen)4);

/*              C1 := C1 - W */

/*<                DO 60 J = 1, K >*/
                i__1 = *k;
                for (j = 1; j <= i__1; ++j) {
/*<                   DO 50 I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( I, J ) = C( I, J ) - WORK( I, J ) >*/
                        c__[i__ + j * c_dim1] -= work[i__ + j * work_dim1];
/*<    50             CONTINUE >*/
/* L50: */
                    }
/*<    60          CONTINUE >*/
/* L60: */
                }
/*<             END IF >*/
            }

/*<          ELSE >*/
        } else {

/*           Let  V =  ( V1 ) */
/*                     ( V2 )    (last K rows) */
/*           where  V2  is unit upper triangular. */

/*<             IF( LSAME( SIDE, 'L' ) ) THEN >*/
            if (lsame_(side, "L", (ftnlen)1, (ftnlen)1)) {

/*              Form  H * C  or  H' * C  where  C = ( C1 ) */
/*                                                  ( C2 ) */

/*              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK) */

/*              W := C2' */

/*<                DO 70 J = 1, K >*/
                i__1 = *k;
                for (j = 1; j <= i__1; ++j) {
/*<                   CALL DCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 ) >*/
                    dcopy_(n, &c__[*m - *k + j + c_dim1], ldc, &work[j *
                            work_dim1 + 1], &c__1);
/*<    70          CONTINUE >*/
/* L70: */
                }

/*              W := W * V2 */

/*<    >*/
                dtrmm_("Right", "Upper", "No transpose", "Unit", n, k, &c_b14,
                         &v[*m - *k + 1 + v_dim1], ldv, &work[work_offset],
                        ldwork, (ftnlen)5, (ftnlen)5, (ftnlen)12, (ftnlen)4);
/*<                IF( M.GT.K ) THEN >*/
                if (*m > *k) {

/*                 W := W + C1'*V1 */

/*<    >*/
                    i__1 = *m - *k;
                    dgemm_("Transpose", "No transpose", n, k, &i__1, &c_b14, &
                            c__[c_offset], ldc, &v[v_offset], ldv, &c_b14, &
                            work[work_offset], ldwork, (ftnlen)9, (ftnlen)12);
/*<                END IF >*/
                }

/*              W := W * T'  or  W * T */

/*<    >*/
                dtrmm_("Right", "Lower", transt, "Non-unit", n, k, &c_b14, &t[
                        t_offset], ldt, &work[work_offset], ldwork, (ftnlen)5,
                         (ftnlen)5, (ftnlen)1, (ftnlen)8);

/*              C := C - V * W' */

/*<                IF( M.GT.K ) THEN >*/
                if (*m > *k) {

/*                 C1 := C1 - V1 * W' */

/*<    >*/
                    i__1 = *m - *k;
                    dgemm_("No transpose", "Transpose", &i__1, n, k, &c_b25, &
                            v[v_offset], ldv, &work[work_offset], ldwork, &
                            c_b14, &c__[c_offset], ldc, (ftnlen)12, (ftnlen)9)
                            ;
/*<                END IF >*/
                }

/*              W := W * V2' */

/*<    >*/
                dtrmm_("Right", "Upper", "Transpose", "Unit", n, k, &c_b14, &
                        v[*m - *k + 1 + v_dim1], ldv, &work[work_offset],
                        ldwork, (ftnlen)5, (ftnlen)5, (ftnlen)9, (ftnlen)4);

/*              C2 := C2 - W' */

/*<                DO 90 J = 1, K >*/
                i__1 = *k;
                for (j = 1; j <= i__1; ++j) {
/*<                   DO 80 I = 1, N >*/
                    i__2 = *n;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J ) >*/
                        c__[*m - *k + j + i__ * c_dim1] -= work[i__ + j *
                                work_dim1];
/*<    80             CONTINUE >*/
/* L80: */
                    }
/*<    90          CONTINUE >*/
/* L90: */
                }

/*<             ELSE IF( LSAME( SIDE, 'R' ) ) THEN >*/
            } else if (lsame_(side, "R", (ftnlen)1, (ftnlen)1)) {

/*              Form  C * H  or  C * H'  where  C = ( C1  C2 ) */

/*              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK) */

/*              W := C2 */

/*<                DO 100 J = 1, K >*/
                i__1 = *k;
                for (j = 1; j <= i__1; ++j) {
/*<                   CALL DCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 ) >*/
                    dcopy_(m, &c__[(*n - *k + j) * c_dim1 + 1], &c__1, &work[
                            j * work_dim1 + 1], &c__1);
/*<   100          CONTINUE >*/
/* L100: */
                }

/*              W := W * V2 */

/*<    >*/
                dtrmm_("Right", "Upper", "No transpose", "Unit", m, k, &c_b14,
                         &v[*n - *k + 1 + v_dim1], ldv, &work[work_offset],
                        ldwork, (ftnlen)5, (ftnlen)5, (ftnlen)12, (ftnlen)4);
/*<                IF( N.GT.K ) THEN >*/
                if (*n > *k) {

/*                 W := W + C1 * V1 */

/*<    >*/
                    i__1 = *n - *k;
                    dgemm_("No transpose", "No transpose", m, k, &i__1, &
                            c_b14, &c__[c_offset], ldc, &v[v_offset], ldv, &
                            c_b14, &work[work_offset], ldwork, (ftnlen)12, (
                            ftnlen)12);
/*<                END IF >*/
                }

/*              W := W * T  or  W * T' */

/*<    >*/
                dtrmm_("Right", "Lower", trans, "Non-unit", m, k, &c_b14, &t[
                        t_offset], ldt, &work[work_offset], ldwork, (ftnlen)5,
                         (ftnlen)5, (ftnlen)1, (ftnlen)8);

/*              C := C - W * V' */

/*<                IF( N.GT.K ) THEN >*/
                if (*n > *k) {

/*                 C1 := C1 - W * V1' */

/*<    >*/
                    i__1 = *n - *k;
                    dgemm_("No transpose", "Transpose", m, &i__1, k, &c_b25, &
                            work[work_offset], ldwork, &v[v_offset], ldv, &
                            c_b14, &c__[c_offset], ldc, (ftnlen)12, (ftnlen)9)
                            ;
/*<                END IF >*/
                }

/*              W := W * V2' */

/*<    >*/
                dtrmm_("Right", "Upper", "Transpose", "Unit", m, k, &c_b14, &
                        v[*n - *k + 1 + v_dim1], ldv, &work[work_offset],
                        ldwork, (ftnlen)5, (ftnlen)5, (ftnlen)9, (ftnlen)4);

/*              C2 := C2 - W */

/*<                DO 120 J = 1, K >*/
                i__1 = *k;
                for (j = 1; j <= i__1; ++j) {
/*<                   DO 110 I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J ) >*/
                        c__[i__ + (*n - *k + j) * c_dim1] -= work[i__ + j *
                                work_dim1];
/*<   110             CONTINUE >*/
/* L110: */
                    }
/*<   120          CONTINUE >*/
/* L120: */
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }

/*<       ELSE IF( LSAME( STOREV, 'R' ) ) THEN >*/
    } else if (lsame_(storev, "R", (ftnlen)1, (ftnlen)1)) {

/*<          IF( LSAME( DIRECT, 'F' ) ) THEN >*/
        if (lsame_(direct, "F", (ftnlen)1, (ftnlen)1)) {

/*           Let  V =  ( V1  V2 )    (V1: first K columns) */
/*           where  V1  is unit upper triangular. */

/*<             IF( LSAME( SIDE, 'L' ) ) THEN >*/
            if (lsame_(side, "L", (ftnlen)1, (ftnlen)1)) {

/*              Form  H * C  or  H' * C  where  C = ( C1 ) */
/*                                                  ( C2 ) */

/*              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK) */

/*              W := C1' */

/*<                DO 130 J = 1, K >*/
                i__1 = *k;
                for (j = 1; j <= i__1; ++j) {
/*<                   CALL DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 ) >*/
                    dcopy_(n, &c__[j + c_dim1], ldc, &work[j * work_dim1 + 1],
                             &c__1);
/*<   130          CONTINUE >*/
/* L130: */
                }

/*              W := W * V1' */

/*<    >*/
                dtrmm_("Right", "Upper", "Transpose", "Unit", n, k, &c_b14, &
                        v[v_offset], ldv, &work[work_offset], ldwork, (ftnlen)
                        5, (ftnlen)5, (ftnlen)9, (ftnlen)4);
/*<                IF( M.GT.K ) THEN >*/
                if (*m > *k) {

/*                 W := W + C2'*V2' */

/*<    >*/
                    i__1 = *m - *k;
                    dgemm_("Transpose", "Transpose", n, k, &i__1, &c_b14, &
                            c__[*k + 1 + c_dim1], ldc, &v[(*k + 1) * v_dim1 +
                            1], ldv, &c_b14, &work[work_offset], ldwork, (
                            ftnlen)9, (ftnlen)9);
/*<                END IF >*/
                }

/*              W := W * T'  or  W * T */

/*<    >*/
                dtrmm_("Right", "Upper", transt, "Non-unit", n, k, &c_b14, &t[
                        t_offset], ldt, &work[work_offset], ldwork, (ftnlen)5,
                         (ftnlen)5, (ftnlen)1, (ftnlen)8);

/*              C := C - V' * W' */

/*<                IF( M.GT.K ) THEN >*/
                if (*m > *k) {

/*                 C2 := C2 - V2' * W' */

/*<    >*/
                    i__1 = *m - *k;
                    dgemm_("Transpose", "Transpose", &i__1, n, k, &c_b25, &v[(
                            *k + 1) * v_dim1 + 1], ldv, &work[work_offset],
                            ldwork, &c_b14, &c__[*k + 1 + c_dim1], ldc, (
                            ftnlen)9, (ftnlen)9);
/*<                END IF >*/
                }

/*              W := W * V1 */

/*<    >*/
                dtrmm_("Right", "Upper", "No transpose", "Unit", n, k, &c_b14,
                         &v[v_offset], ldv, &work[work_offset], ldwork, (
                        ftnlen)5, (ftnlen)5, (ftnlen)12, (ftnlen)4);

/*              C1 := C1 - W' */

/*<                DO 150 J = 1, K >*/
                i__1 = *k;
                for (j = 1; j <= i__1; ++j) {
/*<                   DO 140 I = 1, N >*/
                    i__2 = *n;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( J, I ) = C( J, I ) - WORK( I, J ) >*/
                        c__[j + i__ * c_dim1] -= work[i__ + j * work_dim1];
/*<   140             CONTINUE >*/
/* L140: */
                    }
/*<   150          CONTINUE >*/
/* L150: */
                }

/*<             ELSE IF( LSAME( SIDE, 'R' ) ) THEN >*/
            } else if (lsame_(side, "R", (ftnlen)1, (ftnlen)1)) {

/*              Form  C * H  or  C * H'  where  C = ( C1  C2 ) */

/*              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK) */

/*              W := C1 */

/*<                DO 160 J = 1, K >*/
                i__1 = *k;
                for (j = 1; j <= i__1; ++j) {
/*<                   CALL DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 ) >*/
                    dcopy_(m, &c__[j * c_dim1 + 1], &c__1, &work[j *
                            work_dim1 + 1], &c__1);
/*<   160          CONTINUE >*/
/* L160: */
                }

/*              W := W * V1' */

/*<    >*/
                dtrmm_("Right", "Upper", "Transpose", "Unit", m, k, &c_b14, &
                        v[v_offset], ldv, &work[work_offset], ldwork, (ftnlen)
                        5, (ftnlen)5, (ftnlen)9, (ftnlen)4);
/*<                IF( N.GT.K ) THEN >*/
                if (*n > *k) {

/*                 W := W + C2 * V2' */

/*<    >*/
                    i__1 = *n - *k;
                    dgemm_("No transpose", "Transpose", m, k, &i__1, &c_b14, &
                            c__[(*k + 1) * c_dim1 + 1], ldc, &v[(*k + 1) *
                            v_dim1 + 1], ldv, &c_b14, &work[work_offset],
                            ldwork, (ftnlen)12, (ftnlen)9);
/*<                END IF >*/
                }

/*              W := W * T  or  W * T' */

/*<    >*/
                dtrmm_("Right", "Upper", trans, "Non-unit", m, k, &c_b14, &t[
                        t_offset], ldt, &work[work_offset], ldwork, (ftnlen)5,
                         (ftnlen)5, (ftnlen)1, (ftnlen)8);

/*              C := C - W * V */

/*<                IF( N.GT.K ) THEN >*/
                if (*n > *k) {

/*                 C2 := C2 - W * V2 */

/*<    >*/
                    i__1 = *n - *k;
                    dgemm_("No transpose", "No transpose", m, &i__1, k, &
                            c_b25, &work[work_offset], ldwork, &v[(*k + 1) *
                            v_dim1 + 1], ldv, &c_b14, &c__[(*k + 1) * c_dim1
                            + 1], ldc, (ftnlen)12, (ftnlen)12);
/*<                END IF >*/
                }

/*              W := W * V1 */

/*<    >*/
                dtrmm_("Right", "Upper", "No transpose", "Unit", m, k, &c_b14,
                         &v[v_offset], ldv, &work[work_offset], ldwork, (
                        ftnlen)5, (ftnlen)5, (ftnlen)12, (ftnlen)4);

/*              C1 := C1 - W */

/*<                DO 180 J = 1, K >*/
                i__1 = *k;
                for (j = 1; j <= i__1; ++j) {
/*<                   DO 170 I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( I, J ) = C( I, J ) - WORK( I, J ) >*/
                        c__[i__ + j * c_dim1] -= work[i__ + j * work_dim1];
/*<   170             CONTINUE >*/
/* L170: */
                    }
/*<   180          CONTINUE >*/
/* L180: */
                }

/*<             END IF >*/
            }

/*<          ELSE >*/
        } else {

/*           Let  V =  ( V1  V2 )    (V2: last K columns) */
/*           where  V2  is unit lower triangular. */

/*<             IF( LSAME( SIDE, 'L' ) ) THEN >*/
            if (lsame_(side, "L", (ftnlen)1, (ftnlen)1)) {

/*              Form  H * C  or  H' * C  where  C = ( C1 ) */
/*                                                  ( C2 ) */

/*              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK) */

/*              W := C2' */

/*<                DO 190 J = 1, K >*/
                i__1 = *k;
                for (j = 1; j <= i__1; ++j) {
/*<                   CALL DCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 ) >*/
                    dcopy_(n, &c__[*m - *k + j + c_dim1], ldc, &work[j *
                            work_dim1 + 1], &c__1);
/*<   190          CONTINUE >*/
/* L190: */
                }

/*              W := W * V2' */

/*<    >*/
                dtrmm_("Right", "Lower", "Transpose", "Unit", n, k, &c_b14, &
                        v[(*m - *k + 1) * v_dim1 + 1], ldv, &work[work_offset]
                        , ldwork, (ftnlen)5, (ftnlen)5, (ftnlen)9, (ftnlen)4);
/*<                IF( M.GT.K ) THEN >*/
                if (*m > *k) {

/*                 W := W + C1'*V1' */

/*<    >*/
                    i__1 = *m - *k;
                    dgemm_("Transpose", "Transpose", n, k, &i__1, &c_b14, &
                            c__[c_offset], ldc, &v[v_offset], ldv, &c_b14, &
                            work[work_offset], ldwork, (ftnlen)9, (ftnlen)9);
/*<                END IF >*/
                }

/*              W := W * T'  or  W * T */

/*<    >*/
                dtrmm_("Right", "Lower", transt, "Non-unit", n, k, &c_b14, &t[
                        t_offset], ldt, &work[work_offset], ldwork, (ftnlen)5,
                         (ftnlen)5, (ftnlen)1, (ftnlen)8);

/*              C := C - V' * W' */

/*<                IF( M.GT.K ) THEN >*/
                if (*m > *k) {

/*                 C1 := C1 - V1' * W' */

/*<    >*/
                    i__1 = *m - *k;
                    dgemm_("Transpose", "Transpose", &i__1, n, k, &c_b25, &v[
                            v_offset], ldv, &work[work_offset], ldwork, &
                            c_b14, &c__[c_offset], ldc, (ftnlen)9, (ftnlen)9);
/*<                END IF >*/
                }

/*              W := W * V2 */

/*<    >*/
                dtrmm_("Right", "Lower", "No transpose", "Unit", n, k, &c_b14,
                         &v[(*m - *k + 1) * v_dim1 + 1], ldv, &work[
                        work_offset], ldwork, (ftnlen)5, (ftnlen)5, (ftnlen)
                        12, (ftnlen)4);

/*              C2 := C2 - W' */

/*<                DO 210 J = 1, K >*/
                i__1 = *k;
                for (j = 1; j <= i__1; ++j) {
/*<                   DO 200 I = 1, N >*/
                    i__2 = *n;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J ) >*/
                        c__[*m - *k + j + i__ * c_dim1] -= work[i__ + j *
                                work_dim1];
/*<   200             CONTINUE >*/
/* L200: */
                    }
/*<   210          CONTINUE >*/
/* L210: */
                }

/*<             ELSE IF( LSAME( SIDE, 'R' ) ) THEN >*/
            } else if (lsame_(side, "R", (ftnlen)1, (ftnlen)1)) {

/*              Form  C * H  or  C * H'  where  C = ( C1  C2 ) */

/*              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK) */

/*              W := C2 */

/*<                DO 220 J = 1, K >*/
                i__1 = *k;
                for (j = 1; j <= i__1; ++j) {
/*<                   CALL DCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 ) >*/
                    dcopy_(m, &c__[(*n - *k + j) * c_dim1 + 1], &c__1, &work[
                            j * work_dim1 + 1], &c__1);
/*<   220          CONTINUE >*/
/* L220: */
                }

/*              W := W * V2' */

/*<    >*/
                dtrmm_("Right", "Lower", "Transpose", "Unit", m, k, &c_b14, &
                        v[(*n - *k + 1) * v_dim1 + 1], ldv, &work[work_offset]
                        , ldwork, (ftnlen)5, (ftnlen)5, (ftnlen)9, (ftnlen)4);
/*<                IF( N.GT.K ) THEN >*/
                if (*n > *k) {

/*                 W := W + C1 * V1' */

/*<    >*/
                    i__1 = *n - *k;
                    dgemm_("No transpose", "Transpose", m, k, &i__1, &c_b14, &
                            c__[c_offset], ldc, &v[v_offset], ldv, &c_b14, &
                            work[work_offset], ldwork, (ftnlen)12, (ftnlen)9);
/*<                END IF >*/
                }

/*              W := W * T  or  W * T' */

/*<    >*/
                dtrmm_("Right", "Lower", trans, "Non-unit", m, k, &c_b14, &t[
                        t_offset], ldt, &work[work_offset], ldwork, (ftnlen)5,
                         (ftnlen)5, (ftnlen)1, (ftnlen)8);

/*              C := C - W * V */

/*<                IF( N.GT.K ) THEN >*/
                if (*n > *k) {

/*                 C1 := C1 - W * V1 */

/*<    >*/
                    i__1 = *n - *k;
                    dgemm_("No transpose", "No transpose", m, &i__1, k, &
                            c_b25, &work[work_offset], ldwork, &v[v_offset],
                            ldv, &c_b14, &c__[c_offset], ldc, (ftnlen)12, (
                            ftnlen)12);
/*<                END IF >*/
                }

/*              W := W * V2 */

/*<    >*/
                dtrmm_("Right", "Lower", "No transpose", "Unit", m, k, &c_b14,
                         &v[(*n - *k + 1) * v_dim1 + 1], ldv, &work[
                        work_offset], ldwork, (ftnlen)5, (ftnlen)5, (ftnlen)
                        12, (ftnlen)4);

/*              C1 := C1 - W */

/*<                DO 240 J = 1, K >*/
                i__1 = *k;
                for (j = 1; j <= i__1; ++j) {
/*<                   DO 230 I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J ) >*/
                        c__[i__ + (*n - *k + j) * c_dim1] -= work[i__ + j *
                                work_dim1];
/*<   230             CONTINUE >*/
/* L230: */
                    }
/*<   240          CONTINUE >*/
/* L240: */
                }

/*<             END IF >*/
            }

/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of DLARFB */

/*<       END >*/
} /* dlarfb_ */

#ifdef __cplusplus
        }
#endif
