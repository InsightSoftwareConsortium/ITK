/* lapack/double/dlarft.f -- translated by f2c (version 20050501).
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
static doublereal c_b8 = 0.;

/*<       SUBROUTINE DLARFT( DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT ) >*/
/* Subroutine */ int dlarft_(char *direct, char *storev, integer *n, integer *
        k, doublereal *v, integer *ldv, doublereal *tau, doublereal *t,
        integer *ldt, ftnlen direct_len, ftnlen storev_len)
{
    /* System generated locals */
    integer t_dim1, t_offset, v_dim1, v_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Local variables */
    integer i__, j;
    doublereal vii;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int dgemv_(char *, integer *, integer *,
            doublereal *, doublereal *, integer *, doublereal *, integer *,
            doublereal *, doublereal *, integer *, ftnlen), dtrmv_(char *,
            char *, char *, integer *, doublereal *, integer *, doublereal *,
            integer *, ftnlen, ftnlen, ftnlen);
    (void)direct_len;
    (void)storev_len;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          DIRECT, STOREV >*/
/*<       INTEGER            K, LDT, LDV, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   T( LDT, * ), TAU( * ), V( LDV, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLARFT forms the triangular factor T of a real block reflector H */
/*  of order n, which is defined as a product of k elementary reflectors. */

/*  If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular; */

/*  If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular. */

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

/*  V       (input/output) DOUBLE PRECISION array, dimension */
/*                               (LDV,K) if STOREV = 'C' */
/*                               (LDV,N) if STOREV = 'R' */
/*          The matrix V. See further details. */

/*  LDV     (input) INTEGER */
/*          The leading dimension of the array V. */
/*          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K. */

/*  TAU     (input) DOUBLE PRECISION array, dimension (K) */
/*          TAU(i) must contain the scalar factor of the elementary */
/*          reflector H(i). */

/*  T       (output) DOUBLE PRECISION array, dimension (LDT,K) */
/*          The k by k triangular factor T of the block reflector. */
/*          If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is */
/*          lower triangular. The rest of the array is not used. */

/*  LDT     (input) INTEGER */
/*          The leading dimension of the array T. LDT >= K. */

/*  Further Details */
/*  =============== */

/*  The shape of the matrix V and the storage of the vectors which define */
/*  the H(i) is best illustrated by the following example with n = 5 and */
/*  k = 3. The elements equal to 1 are not stored; the corresponding */
/*  array elements are modified but restored on exit. The rest of the */
/*  array is not used. */

/*  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R': */

/*               V = (  1       )                 V = (  1 v1 v1 v1 v1 ) */
/*                   ( v1  1    )                     (     1 v2 v2 v2 ) */
/*                   ( v1 v2  1 )                     (        1 v3 v3 ) */
/*                   ( v1 v2 v3 ) */
/*                   ( v1 v2 v3 ) */

/*  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R': */

/*               V = ( v1 v2 v3 )                 V = ( v1 v1  1       ) */
/*                   ( v1 v2 v3 )                     ( v2 v2 v2  1    ) */
/*                   (  1 v2 v3 )                     ( v3 v3 v3 v3  1 ) */
/*                   (     1 v3 ) */
/*                   (        1 ) */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ONE, ZERO >*/
/*<       PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            I, J >*/
/*<       DOUBLE PRECISION   VII >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DGEMV, DTRMV >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Quick return if possible */

/*<    >*/
    /* Parameter adjustments */
    v_dim1 = *ldv;
    v_offset = 1 + v_dim1;
    v -= v_offset;
    --tau;
    t_dim1 = *ldt;
    t_offset = 1 + t_dim1;
    t -= t_offset;

    /* Function Body */
    if (*n == 0) {
        return 0;
    }

/*<       IF( LSAME( DIRECT, 'F' ) ) THEN >*/
    if (lsame_(direct, "F", (ftnlen)1, (ftnlen)1)) {
/*<          DO 20 I = 1, K >*/
        i__1 = *k;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             IF( TAU( I ).EQ.ZERO ) THEN >*/
            if (tau[i__] == 0.) {

/*              H(i)  =  I */

/*<                DO 10 J = 1, I >*/
                i__2 = i__;
                for (j = 1; j <= i__2; ++j) {
/*<                   T( J, I ) = ZERO >*/
                    t[j + i__ * t_dim1] = 0.;
/*<    10          CONTINUE >*/
/* L10: */
                }
/*<             ELSE >*/
            } else {

/*              general case */

/*<                VII = V( I, I ) >*/
                vii = v[i__ + i__ * v_dim1];
/*<                V( I, I ) = ONE >*/
                v[i__ + i__ * v_dim1] = 1.;
/*<                IF( LSAME( STOREV, 'C' ) ) THEN >*/
                if (lsame_(storev, "C", (ftnlen)1, (ftnlen)1)) {

/*                 T(1:i-1,i) := - tau(i) * V(i:n,1:i-1)' * V(i:n,i) */

/*<    >*/
                    i__2 = *n - i__ + 1;
                    i__3 = i__ - 1;
                    d__1 = -tau[i__];
                    dgemv_("Transpose", &i__2, &i__3, &d__1, &v[i__ + v_dim1],
                             ldv, &v[i__ + i__ * v_dim1], &c__1, &c_b8, &t[
                            i__ * t_dim1 + 1], &c__1, (ftnlen)9);
/*<                ELSE >*/
                } else {

/*                 T(1:i-1,i) := - tau(i) * V(1:i-1,i:n) * V(i,i:n)' */

/*<    >*/
                    i__2 = i__ - 1;
                    i__3 = *n - i__ + 1;
                    d__1 = -tau[i__];
                    dgemv_("No transpose", &i__2, &i__3, &d__1, &v[i__ *
                            v_dim1 + 1], ldv, &v[i__ + i__ * v_dim1], ldv, &
                            c_b8, &t[i__ * t_dim1 + 1], &c__1, (ftnlen)12);
/*<                END IF >*/
                }
/*<                V( I, I ) = VII >*/
                v[i__ + i__ * v_dim1] = vii;

/*              T(1:i-1,i) := T(1:i-1,1:i-1) * T(1:i-1,i) */

/*<    >*/
                i__2 = i__ - 1;
                dtrmv_("Upper", "No transpose", "Non-unit", &i__2, &t[
                        t_offset], ldt, &t[i__ * t_dim1 + 1], &c__1, (ftnlen)
                        5, (ftnlen)12, (ftnlen)8);
/*<                T( I, I ) = TAU( I ) >*/
                t[i__ + i__ * t_dim1] = tau[i__];
/*<             END IF >*/
            }
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<       ELSE >*/
    } else {
/*<          DO 40 I = K, 1, -1 >*/
        for (i__ = *k; i__ >= 1; --i__) {
/*<             IF( TAU( I ).EQ.ZERO ) THEN >*/
            if (tau[i__] == 0.) {

/*              H(i)  =  I */

/*<                DO 30 J = I, K >*/
                i__1 = *k;
                for (j = i__; j <= i__1; ++j) {
/*<                   T( J, I ) = ZERO >*/
                    t[j + i__ * t_dim1] = 0.;
/*<    30          CONTINUE >*/
/* L30: */
                }
/*<             ELSE >*/
            } else {

/*              general case */

/*<                IF( I.LT.K ) THEN >*/
                if (i__ < *k) {
/*<                   IF( LSAME( STOREV, 'C' ) ) THEN >*/
                    if (lsame_(storev, "C", (ftnlen)1, (ftnlen)1)) {
/*<                      VII = V( N-K+I, I ) >*/
                        vii = v[*n - *k + i__ + i__ * v_dim1];
/*<                      V( N-K+I, I ) = ONE >*/
                        v[*n - *k + i__ + i__ * v_dim1] = 1.;

/*                    T(i+1:k,i) := */
/*                            - tau(i) * V(1:n-k+i,i+1:k)' * V(1:n-k+i,i) */

/*<    >*/
                        i__1 = *n - *k + i__;
                        i__2 = *k - i__;
                        d__1 = -tau[i__];
                        dgemv_("Transpose", &i__1, &i__2, &d__1, &v[(i__ + 1)
                                * v_dim1 + 1], ldv, &v[i__ * v_dim1 + 1], &
                                c__1, &c_b8, &t[i__ + 1 + i__ * t_dim1], &
                                c__1, (ftnlen)9);
/*<                      V( N-K+I, I ) = VII >*/
                        v[*n - *k + i__ + i__ * v_dim1] = vii;
/*<                   ELSE >*/
                    } else {
/*<                      VII = V( I, N-K+I ) >*/
                        vii = v[i__ + (*n - *k + i__) * v_dim1];
/*<                      V( I, N-K+I ) = ONE >*/
                        v[i__ + (*n - *k + i__) * v_dim1] = 1.;

/*                    T(i+1:k,i) := */
/*                            - tau(i) * V(i+1:k,1:n-k+i) * V(i,1:n-k+i)' */

/*<    >*/
                        i__1 = *k - i__;
                        i__2 = *n - *k + i__;
                        d__1 = -tau[i__];
                        dgemv_("No transpose", &i__1, &i__2, &d__1, &v[i__ +
                                1 + v_dim1], ldv, &v[i__ + v_dim1], ldv, &
                                c_b8, &t[i__ + 1 + i__ * t_dim1], &c__1, (
                                ftnlen)12);
/*<                      V( I, N-K+I ) = VII >*/
                        v[i__ + (*n - *k + i__) * v_dim1] = vii;
/*<                   END IF >*/
                    }

/*                 T(i+1:k,i) := T(i+1:k,i+1:k) * T(i+1:k,i) */

/*<    >*/
                    i__1 = *k - i__;
                    dtrmv_("Lower", "No transpose", "Non-unit", &i__1, &t[i__
                            + 1 + (i__ + 1) * t_dim1], ldt, &t[i__ + 1 + i__ *
                             t_dim1], &c__1, (ftnlen)5, (ftnlen)12, (ftnlen)8)
                            ;
/*<                END IF >*/
                }
/*<                T( I, I ) = TAU( I ) >*/
                t[i__ + i__ * t_dim1] = tau[i__];
/*<             END IF >*/
            }
/*<    40    CONTINUE >*/
/* L40: */
        }
/*<       END IF >*/
    }
/*<       RETURN >*/
    return 0;

/*     End of DLARFT */

/*<       END >*/
} /* dlarft_ */

#ifdef __cplusplus
        }
#endif
