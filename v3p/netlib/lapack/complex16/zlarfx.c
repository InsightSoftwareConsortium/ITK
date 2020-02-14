/* lapack/complex16/zlarfx.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
        on Microsoft Windows system, link with libf2c.lib;
        on Linux or Unix systems, link with .../path/to/libf2c.a -lm
        or, if you install libf2c.a in a standard place, with -lf2c -lm
        -- in that order, at the end of the command line, as in
                cc *.o -lf2c -lm
        Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

                http://www.netlib.org/f2c/libf2c.zip
*/

/* Disable "global optimizations" to avoid optimizer bugs in this file.
   For GCC the file should be compiled with the -fno-gcse option.  Here
   is a note from the GCC man page:

    Note: When compiling a program using computed gotos, a GCC exten-
    sion, you may get better runtime performance if you disable the
    global common subexpression elimination pass by adding -fno-gcse to
    the command line.
*/
#ifdef _MSC_VER
# pragma optimize ("g",off)
#endif

#ifdef __cplusplus
extern "C" {
#endif
#include "v3p_netlib.h"

/* Table of constant values */

static doublecomplex c_b1 = {0.,0.};
static doublecomplex c_b2 = {1.,0.};
static integer c__1 = 1;

/*<       SUBROUTINE ZLARFX( SIDE, M, N, V, TAU, C, LDC, WORK ) >*/
/* Subroutine */ int zlarfx_(char *side, integer *m, integer *n,
        doublecomplex *v, doublecomplex *tau, doublecomplex *c__, integer *
        ldc, doublecomplex *work, ftnlen side_len)
{
    /* System generated locals */
    integer c_dim1, c_offset, i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8,
            i__9, i__10, i__11;
    doublecomplex z__1, z__2, z__3, z__4, z__5, z__6, z__7, z__8, z__9, z__10,
             z__11, z__12, z__13, z__14, z__15, z__16, z__17, z__18, z__19;

    /* Builtin functions */
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    integer j;
    doublecomplex t1, t2, t3, t4, t5, t6, t7, t8, t9, v1, v2, v3, v4, v5, v6,
            v7, v8, v9, t10, v10, sum;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int zgerc_(integer *, integer *, doublecomplex *,
            doublecomplex *, integer *, doublecomplex *, integer *,
            doublecomplex *, integer *), zgemv_(char *, integer *, integer *,
            doublecomplex *, doublecomplex *, integer *, doublecomplex *,
            integer *, doublecomplex *, doublecomplex *, integer *, ftnlen);
    (void)side_len;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          SIDE >*/
/*<       INTEGER            LDC, M, N >*/
/*<       COMPLEX*16         TAU >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         C( LDC, * ), V( * ), WORK( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLARFX applies a complex elementary reflector H to a complex m by n */
/*  matrix C, from either the left or the right. H is represented in the */
/*  form */

/*        H = I - tau * v * v' */

/*  where tau is a complex scalar and v is a complex vector. */

/*  If tau = 0, then H is taken to be the unit matrix */

/*  This version uses inline code if H has order < 11. */

/*  Arguments */
/*  ========= */

/*  SIDE    (input) CHARACTER*1 */
/*          = 'L': form  H * C */
/*          = 'R': form  C * H */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix C. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix C. */

/*  V       (input) COMPLEX*16 array, dimension (M) if SIDE = 'L' */
/*                                        or (N) if SIDE = 'R' */
/*          The vector v in the representation of H. */

/*  TAU     (input) COMPLEX*16 */
/*          The value tau in the representation of H. */

/*  C       (input/output) COMPLEX*16 array, dimension (LDC,N) */
/*          On entry, the m by n matrix C. */
/*          On exit, C is overwritten by the matrix H * C if SIDE = 'L', */
/*          or C * H if SIDE = 'R'. */

/*  LDC     (input) INTEGER */
/*          The leading dimension of the array C. LDA >= max(1,M). */

/*  WORK    (workspace) COMPLEX*16 array, dimension (N) if SIDE = 'L' */
/*                                            or (M) if SIDE = 'R' */
/*          WORK is not referenced if H has order < 11. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       COMPLEX*16         ZERO, ONE >*/
/*<    >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            J >*/
/*<    >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           ZGEMV, ZGERC >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          DCONJG >*/
/*     .. */
/*     .. Executable Statements .. */

/*<    >*/
    /* Parameter adjustments */
    --v;
    c_dim1 = *ldc;
    c_offset = 1 + c_dim1;
    c__ -= c_offset;
    --work;

    /* Function Body */
    if (tau->r == 0. && tau->i == 0.) {
        return 0;
    }
/*<       IF( LSAME( SIDE, 'L' ) ) THEN >*/
    if (lsame_(side, "L", (ftnlen)1, (ftnlen)1)) {

/*        Form  H * C, where H has order m. */

/*<    >*/
        switch (*m) {
            case 1:  goto L10;
            case 2:  goto L30;
            case 3:  goto L50;
            case 4:  goto L70;
            case 5:  goto L90;
            case 6:  goto L110;
            case 7:  goto L130;
            case 8:  goto L150;
            case 9:  goto L170;
            case 10:  goto L190;
        }

/*        Code for general M */

/*        w := C'*v */

/*<    >*/
        zgemv_("Conjugate transpose", m, n, &c_b2, &c__[c_offset], ldc, &v[1],
                 &c__1, &c_b1, &work[1], &c__1, (ftnlen)19);

/*        C := C - tau * v * w' */

/*<          CALL ZGERC( M, N, -TAU, V, 1, WORK, 1, C, LDC ) >*/
        z__1.r = -tau->r, z__1.i = -tau->i;
        zgerc_(m, n, &z__1, &v[1], &c__1, &work[1], &c__1, &c__[c_offset],
                ldc);
/*<          GO TO 410 >*/
        goto L410;
/*<    10    CONTINUE >*/
L10:

/*        Special code for 1 x 1 Householder */

/*<          T1 = ONE - TAU*V( 1 )*DCONJG( V( 1 ) ) >*/
        z__3.r = tau->r * v[1].r - tau->i * v[1].i, z__3.i = tau->r * v[1].i
                + tau->i * v[1].r;
        d_cnjg(&z__4, &v[1]);
        z__2.r = z__3.r * z__4.r - z__3.i * z__4.i, z__2.i = z__3.r * z__4.i
                + z__3.i * z__4.r;
        z__1.r = 1. - z__2.r, z__1.i = 0. - z__2.i;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          DO 20 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             C( 1, J ) = T1*C( 1, J ) >*/
            i__2 = j * c_dim1 + 1;
            i__3 = j * c_dim1 + 1;
            z__1.r = t1.r * c__[i__3].r - t1.i * c__[i__3].i, z__1.i = t1.r *
                    c__[i__3].i + t1.i * c__[i__3].r;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<    30    CONTINUE >*/
L30:

/*        Special code for 2 x 2 Householder */

/*<          V1 = DCONJG( V( 1 ) ) >*/
        d_cnjg(&z__1, &v[1]);
        v1.r = z__1.r, v1.i = z__1.i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = DCONJG( V( 2 ) ) >*/
        d_cnjg(&z__1, &v[2]);
        v2.r = z__1.r, v2.i = z__1.i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          DO 40 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             SUM = V1*C( 1, J ) + V2*C( 2, J ) >*/
            i__2 = j * c_dim1 + 1;
            z__2.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__2.i = v1.r *
                    c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j * c_dim1 + 2;
            z__3.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__3.i = v2.r *
                    c__[i__3].i + v2.i * c__[i__3].r;
            z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( 1, J ) = C( 1, J ) - SUM*T1 >*/
            i__2 = j * c_dim1 + 1;
            i__3 = j * c_dim1 + 1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 2, J ) = C( 2, J ) - SUM*T2 >*/
            i__2 = j * c_dim1 + 2;
            i__3 = j * c_dim1 + 2;
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<    40    CONTINUE >*/
/* L40: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<    50    CONTINUE >*/
L50:

/*        Special code for 3 x 3 Householder */

/*<          V1 = DCONJG( V( 1 ) ) >*/
        d_cnjg(&z__1, &v[1]);
        v1.r = z__1.r, v1.i = z__1.i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = DCONJG( V( 2 ) ) >*/
        d_cnjg(&z__1, &v[2]);
        v2.r = z__1.r, v2.i = z__1.i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          V3 = DCONJG( V( 3 ) ) >*/
        d_cnjg(&z__1, &v[3]);
        v3.r = z__1.r, v3.i = z__1.i;
/*<          T3 = TAU*DCONJG( V3 ) >*/
        d_cnjg(&z__2, &v3);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t3.r = z__1.r, t3.i = z__1.i;
/*<          DO 60 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             SUM = V1*C( 1, J ) + V2*C( 2, J ) + V3*C( 3, J ) >*/
            i__2 = j * c_dim1 + 1;
            z__3.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__3.i = v1.r *
                    c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j * c_dim1 + 2;
            z__4.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__4.i = v2.r *
                    c__[i__3].i + v2.i * c__[i__3].r;
            z__2.r = z__3.r + z__4.r, z__2.i = z__3.i + z__4.i;
            i__4 = j * c_dim1 + 3;
            z__5.r = v3.r * c__[i__4].r - v3.i * c__[i__4].i, z__5.i = v3.r *
                    c__[i__4].i + v3.i * c__[i__4].r;
            z__1.r = z__2.r + z__5.r, z__1.i = z__2.i + z__5.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( 1, J ) = C( 1, J ) - SUM*T1 >*/
            i__2 = j * c_dim1 + 1;
            i__3 = j * c_dim1 + 1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 2, J ) = C( 2, J ) - SUM*T2 >*/
            i__2 = j * c_dim1 + 2;
            i__3 = j * c_dim1 + 2;
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 3, J ) = C( 3, J ) - SUM*T3 >*/
            i__2 = j * c_dim1 + 3;
            i__3 = j * c_dim1 + 3;
            z__2.r = sum.r * t3.r - sum.i * t3.i, z__2.i = sum.r * t3.i +
                    sum.i * t3.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<    60    CONTINUE >*/
/* L60: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<    70    CONTINUE >*/
L70:

/*        Special code for 4 x 4 Householder */

/*<          V1 = DCONJG( V( 1 ) ) >*/
        d_cnjg(&z__1, &v[1]);
        v1.r = z__1.r, v1.i = z__1.i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = DCONJG( V( 2 ) ) >*/
        d_cnjg(&z__1, &v[2]);
        v2.r = z__1.r, v2.i = z__1.i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          V3 = DCONJG( V( 3 ) ) >*/
        d_cnjg(&z__1, &v[3]);
        v3.r = z__1.r, v3.i = z__1.i;
/*<          T3 = TAU*DCONJG( V3 ) >*/
        d_cnjg(&z__2, &v3);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t3.r = z__1.r, t3.i = z__1.i;
/*<          V4 = DCONJG( V( 4 ) ) >*/
        d_cnjg(&z__1, &v[4]);
        v4.r = z__1.r, v4.i = z__1.i;
/*<          T4 = TAU*DCONJG( V4 ) >*/
        d_cnjg(&z__2, &v4);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t4.r = z__1.r, t4.i = z__1.i;
/*<          DO 80 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<    >*/
            i__2 = j * c_dim1 + 1;
            z__4.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__4.i = v1.r *
                    c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j * c_dim1 + 2;
            z__5.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__5.i = v2.r *
                    c__[i__3].i + v2.i * c__[i__3].r;
            z__3.r = z__4.r + z__5.r, z__3.i = z__4.i + z__5.i;
            i__4 = j * c_dim1 + 3;
            z__6.r = v3.r * c__[i__4].r - v3.i * c__[i__4].i, z__6.i = v3.r *
                    c__[i__4].i + v3.i * c__[i__4].r;
            z__2.r = z__3.r + z__6.r, z__2.i = z__3.i + z__6.i;
            i__5 = j * c_dim1 + 4;
            z__7.r = v4.r * c__[i__5].r - v4.i * c__[i__5].i, z__7.i = v4.r *
                    c__[i__5].i + v4.i * c__[i__5].r;
            z__1.r = z__2.r + z__7.r, z__1.i = z__2.i + z__7.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( 1, J ) = C( 1, J ) - SUM*T1 >*/
            i__2 = j * c_dim1 + 1;
            i__3 = j * c_dim1 + 1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 2, J ) = C( 2, J ) - SUM*T2 >*/
            i__2 = j * c_dim1 + 2;
            i__3 = j * c_dim1 + 2;
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 3, J ) = C( 3, J ) - SUM*T3 >*/
            i__2 = j * c_dim1 + 3;
            i__3 = j * c_dim1 + 3;
            z__2.r = sum.r * t3.r - sum.i * t3.i, z__2.i = sum.r * t3.i +
                    sum.i * t3.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 4, J ) = C( 4, J ) - SUM*T4 >*/
            i__2 = j * c_dim1 + 4;
            i__3 = j * c_dim1 + 4;
            z__2.r = sum.r * t4.r - sum.i * t4.i, z__2.i = sum.r * t4.i +
                    sum.i * t4.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<    80    CONTINUE >*/
/* L80: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<    90    CONTINUE >*/
L90:

/*        Special code for 5 x 5 Householder */

/*<          V1 = DCONJG( V( 1 ) ) >*/
        d_cnjg(&z__1, &v[1]);
        v1.r = z__1.r, v1.i = z__1.i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = DCONJG( V( 2 ) ) >*/
        d_cnjg(&z__1, &v[2]);
        v2.r = z__1.r, v2.i = z__1.i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          V3 = DCONJG( V( 3 ) ) >*/
        d_cnjg(&z__1, &v[3]);
        v3.r = z__1.r, v3.i = z__1.i;
/*<          T3 = TAU*DCONJG( V3 ) >*/
        d_cnjg(&z__2, &v3);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t3.r = z__1.r, t3.i = z__1.i;
/*<          V4 = DCONJG( V( 4 ) ) >*/
        d_cnjg(&z__1, &v[4]);
        v4.r = z__1.r, v4.i = z__1.i;
/*<          T4 = TAU*DCONJG( V4 ) >*/
        d_cnjg(&z__2, &v4);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t4.r = z__1.r, t4.i = z__1.i;
/*<          V5 = DCONJG( V( 5 ) ) >*/
        d_cnjg(&z__1, &v[5]);
        v5.r = z__1.r, v5.i = z__1.i;
/*<          T5 = TAU*DCONJG( V5 ) >*/
        d_cnjg(&z__2, &v5);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t5.r = z__1.r, t5.i = z__1.i;
/*<          DO 100 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<    >*/
            i__2 = j * c_dim1 + 1;
            z__5.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__5.i = v1.r *
                    c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j * c_dim1 + 2;
            z__6.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__6.i = v2.r *
                    c__[i__3].i + v2.i * c__[i__3].r;
            z__4.r = z__5.r + z__6.r, z__4.i = z__5.i + z__6.i;
            i__4 = j * c_dim1 + 3;
            z__7.r = v3.r * c__[i__4].r - v3.i * c__[i__4].i, z__7.i = v3.r *
                    c__[i__4].i + v3.i * c__[i__4].r;
            z__3.r = z__4.r + z__7.r, z__3.i = z__4.i + z__7.i;
            i__5 = j * c_dim1 + 4;
            z__8.r = v4.r * c__[i__5].r - v4.i * c__[i__5].i, z__8.i = v4.r *
                    c__[i__5].i + v4.i * c__[i__5].r;
            z__2.r = z__3.r + z__8.r, z__2.i = z__3.i + z__8.i;
            i__6 = j * c_dim1 + 5;
            z__9.r = v5.r * c__[i__6].r - v5.i * c__[i__6].i, z__9.i = v5.r *
                    c__[i__6].i + v5.i * c__[i__6].r;
            z__1.r = z__2.r + z__9.r, z__1.i = z__2.i + z__9.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( 1, J ) = C( 1, J ) - SUM*T1 >*/
            i__2 = j * c_dim1 + 1;
            i__3 = j * c_dim1 + 1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 2, J ) = C( 2, J ) - SUM*T2 >*/
            i__2 = j * c_dim1 + 2;
            i__3 = j * c_dim1 + 2;
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 3, J ) = C( 3, J ) - SUM*T3 >*/
            i__2 = j * c_dim1 + 3;
            i__3 = j * c_dim1 + 3;
            z__2.r = sum.r * t3.r - sum.i * t3.i, z__2.i = sum.r * t3.i +
                    sum.i * t3.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 4, J ) = C( 4, J ) - SUM*T4 >*/
            i__2 = j * c_dim1 + 4;
            i__3 = j * c_dim1 + 4;
            z__2.r = sum.r * t4.r - sum.i * t4.i, z__2.i = sum.r * t4.i +
                    sum.i * t4.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 5, J ) = C( 5, J ) - SUM*T5 >*/
            i__2 = j * c_dim1 + 5;
            i__3 = j * c_dim1 + 5;
            z__2.r = sum.r * t5.r - sum.i * t5.i, z__2.i = sum.r * t5.i +
                    sum.i * t5.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<   100    CONTINUE >*/
/* L100: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<   110    CONTINUE >*/
L110:

/*        Special code for 6 x 6 Householder */

/*<          V1 = DCONJG( V( 1 ) ) >*/
        d_cnjg(&z__1, &v[1]);
        v1.r = z__1.r, v1.i = z__1.i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = DCONJG( V( 2 ) ) >*/
        d_cnjg(&z__1, &v[2]);
        v2.r = z__1.r, v2.i = z__1.i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          V3 = DCONJG( V( 3 ) ) >*/
        d_cnjg(&z__1, &v[3]);
        v3.r = z__1.r, v3.i = z__1.i;
/*<          T3 = TAU*DCONJG( V3 ) >*/
        d_cnjg(&z__2, &v3);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t3.r = z__1.r, t3.i = z__1.i;
/*<          V4 = DCONJG( V( 4 ) ) >*/
        d_cnjg(&z__1, &v[4]);
        v4.r = z__1.r, v4.i = z__1.i;
/*<          T4 = TAU*DCONJG( V4 ) >*/
        d_cnjg(&z__2, &v4);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t4.r = z__1.r, t4.i = z__1.i;
/*<          V5 = DCONJG( V( 5 ) ) >*/
        d_cnjg(&z__1, &v[5]);
        v5.r = z__1.r, v5.i = z__1.i;
/*<          T5 = TAU*DCONJG( V5 ) >*/
        d_cnjg(&z__2, &v5);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t5.r = z__1.r, t5.i = z__1.i;
/*<          V6 = DCONJG( V( 6 ) ) >*/
        d_cnjg(&z__1, &v[6]);
        v6.r = z__1.r, v6.i = z__1.i;
/*<          T6 = TAU*DCONJG( V6 ) >*/
        d_cnjg(&z__2, &v6);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t6.r = z__1.r, t6.i = z__1.i;
/*<          DO 120 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<    >*/
            i__2 = j * c_dim1 + 1;
            z__6.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__6.i = v1.r *
                    c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j * c_dim1 + 2;
            z__7.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__7.i = v2.r *
                    c__[i__3].i + v2.i * c__[i__3].r;
            z__5.r = z__6.r + z__7.r, z__5.i = z__6.i + z__7.i;
            i__4 = j * c_dim1 + 3;
            z__8.r = v3.r * c__[i__4].r - v3.i * c__[i__4].i, z__8.i = v3.r *
                    c__[i__4].i + v3.i * c__[i__4].r;
            z__4.r = z__5.r + z__8.r, z__4.i = z__5.i + z__8.i;
            i__5 = j * c_dim1 + 4;
            z__9.r = v4.r * c__[i__5].r - v4.i * c__[i__5].i, z__9.i = v4.r *
                    c__[i__5].i + v4.i * c__[i__5].r;
            z__3.r = z__4.r + z__9.r, z__3.i = z__4.i + z__9.i;
            i__6 = j * c_dim1 + 5;
            z__10.r = v5.r * c__[i__6].r - v5.i * c__[i__6].i, z__10.i = v5.r
                    * c__[i__6].i + v5.i * c__[i__6].r;
            z__2.r = z__3.r + z__10.r, z__2.i = z__3.i + z__10.i;
            i__7 = j * c_dim1 + 6;
            z__11.r = v6.r * c__[i__7].r - v6.i * c__[i__7].i, z__11.i = v6.r
                    * c__[i__7].i + v6.i * c__[i__7].r;
            z__1.r = z__2.r + z__11.r, z__1.i = z__2.i + z__11.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( 1, J ) = C( 1, J ) - SUM*T1 >*/
            i__2 = j * c_dim1 + 1;
            i__3 = j * c_dim1 + 1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 2, J ) = C( 2, J ) - SUM*T2 >*/
            i__2 = j * c_dim1 + 2;
            i__3 = j * c_dim1 + 2;
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 3, J ) = C( 3, J ) - SUM*T3 >*/
            i__2 = j * c_dim1 + 3;
            i__3 = j * c_dim1 + 3;
            z__2.r = sum.r * t3.r - sum.i * t3.i, z__2.i = sum.r * t3.i +
                    sum.i * t3.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 4, J ) = C( 4, J ) - SUM*T4 >*/
            i__2 = j * c_dim1 + 4;
            i__3 = j * c_dim1 + 4;
            z__2.r = sum.r * t4.r - sum.i * t4.i, z__2.i = sum.r * t4.i +
                    sum.i * t4.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 5, J ) = C( 5, J ) - SUM*T5 >*/
            i__2 = j * c_dim1 + 5;
            i__3 = j * c_dim1 + 5;
            z__2.r = sum.r * t5.r - sum.i * t5.i, z__2.i = sum.r * t5.i +
                    sum.i * t5.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 6, J ) = C( 6, J ) - SUM*T6 >*/
            i__2 = j * c_dim1 + 6;
            i__3 = j * c_dim1 + 6;
            z__2.r = sum.r * t6.r - sum.i * t6.i, z__2.i = sum.r * t6.i +
                    sum.i * t6.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<   120    CONTINUE >*/
/* L120: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<   130    CONTINUE >*/
L130:

/*        Special code for 7 x 7 Householder */

/*<          V1 = DCONJG( V( 1 ) ) >*/
        d_cnjg(&z__1, &v[1]);
        v1.r = z__1.r, v1.i = z__1.i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = DCONJG( V( 2 ) ) >*/
        d_cnjg(&z__1, &v[2]);
        v2.r = z__1.r, v2.i = z__1.i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          V3 = DCONJG( V( 3 ) ) >*/
        d_cnjg(&z__1, &v[3]);
        v3.r = z__1.r, v3.i = z__1.i;
/*<          T3 = TAU*DCONJG( V3 ) >*/
        d_cnjg(&z__2, &v3);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t3.r = z__1.r, t3.i = z__1.i;
/*<          V4 = DCONJG( V( 4 ) ) >*/
        d_cnjg(&z__1, &v[4]);
        v4.r = z__1.r, v4.i = z__1.i;
/*<          T4 = TAU*DCONJG( V4 ) >*/
        d_cnjg(&z__2, &v4);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t4.r = z__1.r, t4.i = z__1.i;
/*<          V5 = DCONJG( V( 5 ) ) >*/
        d_cnjg(&z__1, &v[5]);
        v5.r = z__1.r, v5.i = z__1.i;
/*<          T5 = TAU*DCONJG( V5 ) >*/
        d_cnjg(&z__2, &v5);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t5.r = z__1.r, t5.i = z__1.i;
/*<          V6 = DCONJG( V( 6 ) ) >*/
        d_cnjg(&z__1, &v[6]);
        v6.r = z__1.r, v6.i = z__1.i;
/*<          T6 = TAU*DCONJG( V6 ) >*/
        d_cnjg(&z__2, &v6);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t6.r = z__1.r, t6.i = z__1.i;
/*<          V7 = DCONJG( V( 7 ) ) >*/
        d_cnjg(&z__1, &v[7]);
        v7.r = z__1.r, v7.i = z__1.i;
/*<          T7 = TAU*DCONJG( V7 ) >*/
        d_cnjg(&z__2, &v7);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t7.r = z__1.r, t7.i = z__1.i;
/*<          DO 140 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<    >*/
            i__2 = j * c_dim1 + 1;
            z__7.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__7.i = v1.r *
                    c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j * c_dim1 + 2;
            z__8.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__8.i = v2.r *
                    c__[i__3].i + v2.i * c__[i__3].r;
            z__6.r = z__7.r + z__8.r, z__6.i = z__7.i + z__8.i;
            i__4 = j * c_dim1 + 3;
            z__9.r = v3.r * c__[i__4].r - v3.i * c__[i__4].i, z__9.i = v3.r *
                    c__[i__4].i + v3.i * c__[i__4].r;
            z__5.r = z__6.r + z__9.r, z__5.i = z__6.i + z__9.i;
            i__5 = j * c_dim1 + 4;
            z__10.r = v4.r * c__[i__5].r - v4.i * c__[i__5].i, z__10.i = v4.r
                    * c__[i__5].i + v4.i * c__[i__5].r;
            z__4.r = z__5.r + z__10.r, z__4.i = z__5.i + z__10.i;
            i__6 = j * c_dim1 + 5;
            z__11.r = v5.r * c__[i__6].r - v5.i * c__[i__6].i, z__11.i = v5.r
                    * c__[i__6].i + v5.i * c__[i__6].r;
            z__3.r = z__4.r + z__11.r, z__3.i = z__4.i + z__11.i;
            i__7 = j * c_dim1 + 6;
            z__12.r = v6.r * c__[i__7].r - v6.i * c__[i__7].i, z__12.i = v6.r
                    * c__[i__7].i + v6.i * c__[i__7].r;
            z__2.r = z__3.r + z__12.r, z__2.i = z__3.i + z__12.i;
            i__8 = j * c_dim1 + 7;
            z__13.r = v7.r * c__[i__8].r - v7.i * c__[i__8].i, z__13.i = v7.r
                    * c__[i__8].i + v7.i * c__[i__8].r;
            z__1.r = z__2.r + z__13.r, z__1.i = z__2.i + z__13.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( 1, J ) = C( 1, J ) - SUM*T1 >*/
            i__2 = j * c_dim1 + 1;
            i__3 = j * c_dim1 + 1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 2, J ) = C( 2, J ) - SUM*T2 >*/
            i__2 = j * c_dim1 + 2;
            i__3 = j * c_dim1 + 2;
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 3, J ) = C( 3, J ) - SUM*T3 >*/
            i__2 = j * c_dim1 + 3;
            i__3 = j * c_dim1 + 3;
            z__2.r = sum.r * t3.r - sum.i * t3.i, z__2.i = sum.r * t3.i +
                    sum.i * t3.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 4, J ) = C( 4, J ) - SUM*T4 >*/
            i__2 = j * c_dim1 + 4;
            i__3 = j * c_dim1 + 4;
            z__2.r = sum.r * t4.r - sum.i * t4.i, z__2.i = sum.r * t4.i +
                    sum.i * t4.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 5, J ) = C( 5, J ) - SUM*T5 >*/
            i__2 = j * c_dim1 + 5;
            i__3 = j * c_dim1 + 5;
            z__2.r = sum.r * t5.r - sum.i * t5.i, z__2.i = sum.r * t5.i +
                    sum.i * t5.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 6, J ) = C( 6, J ) - SUM*T6 >*/
            i__2 = j * c_dim1 + 6;
            i__3 = j * c_dim1 + 6;
            z__2.r = sum.r * t6.r - sum.i * t6.i, z__2.i = sum.r * t6.i +
                    sum.i * t6.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 7, J ) = C( 7, J ) - SUM*T7 >*/
            i__2 = j * c_dim1 + 7;
            i__3 = j * c_dim1 + 7;
            z__2.r = sum.r * t7.r - sum.i * t7.i, z__2.i = sum.r * t7.i +
                    sum.i * t7.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<   140    CONTINUE >*/
/* L140: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<   150    CONTINUE >*/
L150:

/*        Special code for 8 x 8 Householder */

/*<          V1 = DCONJG( V( 1 ) ) >*/
        d_cnjg(&z__1, &v[1]);
        v1.r = z__1.r, v1.i = z__1.i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = DCONJG( V( 2 ) ) >*/
        d_cnjg(&z__1, &v[2]);
        v2.r = z__1.r, v2.i = z__1.i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          V3 = DCONJG( V( 3 ) ) >*/
        d_cnjg(&z__1, &v[3]);
        v3.r = z__1.r, v3.i = z__1.i;
/*<          T3 = TAU*DCONJG( V3 ) >*/
        d_cnjg(&z__2, &v3);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t3.r = z__1.r, t3.i = z__1.i;
/*<          V4 = DCONJG( V( 4 ) ) >*/
        d_cnjg(&z__1, &v[4]);
        v4.r = z__1.r, v4.i = z__1.i;
/*<          T4 = TAU*DCONJG( V4 ) >*/
        d_cnjg(&z__2, &v4);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t4.r = z__1.r, t4.i = z__1.i;
/*<          V5 = DCONJG( V( 5 ) ) >*/
        d_cnjg(&z__1, &v[5]);
        v5.r = z__1.r, v5.i = z__1.i;
/*<          T5 = TAU*DCONJG( V5 ) >*/
        d_cnjg(&z__2, &v5);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t5.r = z__1.r, t5.i = z__1.i;
/*<          V6 = DCONJG( V( 6 ) ) >*/
        d_cnjg(&z__1, &v[6]);
        v6.r = z__1.r, v6.i = z__1.i;
/*<          T6 = TAU*DCONJG( V6 ) >*/
        d_cnjg(&z__2, &v6);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t6.r = z__1.r, t6.i = z__1.i;
/*<          V7 = DCONJG( V( 7 ) ) >*/
        d_cnjg(&z__1, &v[7]);
        v7.r = z__1.r, v7.i = z__1.i;
/*<          T7 = TAU*DCONJG( V7 ) >*/
        d_cnjg(&z__2, &v7);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t7.r = z__1.r, t7.i = z__1.i;
/*<          V8 = DCONJG( V( 8 ) ) >*/
        d_cnjg(&z__1, &v[8]);
        v8.r = z__1.r, v8.i = z__1.i;
/*<          T8 = TAU*DCONJG( V8 ) >*/
        d_cnjg(&z__2, &v8);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t8.r = z__1.r, t8.i = z__1.i;
/*<          DO 160 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<    >*/
            i__2 = j * c_dim1 + 1;
            z__8.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__8.i = v1.r *
                    c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j * c_dim1 + 2;
            z__9.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__9.i = v2.r *
                    c__[i__3].i + v2.i * c__[i__3].r;
            z__7.r = z__8.r + z__9.r, z__7.i = z__8.i + z__9.i;
            i__4 = j * c_dim1 + 3;
            z__10.r = v3.r * c__[i__4].r - v3.i * c__[i__4].i, z__10.i = v3.r
                    * c__[i__4].i + v3.i * c__[i__4].r;
            z__6.r = z__7.r + z__10.r, z__6.i = z__7.i + z__10.i;
            i__5 = j * c_dim1 + 4;
            z__11.r = v4.r * c__[i__5].r - v4.i * c__[i__5].i, z__11.i = v4.r
                    * c__[i__5].i + v4.i * c__[i__5].r;
            z__5.r = z__6.r + z__11.r, z__5.i = z__6.i + z__11.i;
            i__6 = j * c_dim1 + 5;
            z__12.r = v5.r * c__[i__6].r - v5.i * c__[i__6].i, z__12.i = v5.r
                    * c__[i__6].i + v5.i * c__[i__6].r;
            z__4.r = z__5.r + z__12.r, z__4.i = z__5.i + z__12.i;
            i__7 = j * c_dim1 + 6;
            z__13.r = v6.r * c__[i__7].r - v6.i * c__[i__7].i, z__13.i = v6.r
                    * c__[i__7].i + v6.i * c__[i__7].r;
            z__3.r = z__4.r + z__13.r, z__3.i = z__4.i + z__13.i;
            i__8 = j * c_dim1 + 7;
            z__14.r = v7.r * c__[i__8].r - v7.i * c__[i__8].i, z__14.i = v7.r
                    * c__[i__8].i + v7.i * c__[i__8].r;
            z__2.r = z__3.r + z__14.r, z__2.i = z__3.i + z__14.i;
            i__9 = j * c_dim1 + 8;
            z__15.r = v8.r * c__[i__9].r - v8.i * c__[i__9].i, z__15.i = v8.r
                    * c__[i__9].i + v8.i * c__[i__9].r;
            z__1.r = z__2.r + z__15.r, z__1.i = z__2.i + z__15.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( 1, J ) = C( 1, J ) - SUM*T1 >*/
            i__2 = j * c_dim1 + 1;
            i__3 = j * c_dim1 + 1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 2, J ) = C( 2, J ) - SUM*T2 >*/
            i__2 = j * c_dim1 + 2;
            i__3 = j * c_dim1 + 2;
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 3, J ) = C( 3, J ) - SUM*T3 >*/
            i__2 = j * c_dim1 + 3;
            i__3 = j * c_dim1 + 3;
            z__2.r = sum.r * t3.r - sum.i * t3.i, z__2.i = sum.r * t3.i +
                    sum.i * t3.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 4, J ) = C( 4, J ) - SUM*T4 >*/
            i__2 = j * c_dim1 + 4;
            i__3 = j * c_dim1 + 4;
            z__2.r = sum.r * t4.r - sum.i * t4.i, z__2.i = sum.r * t4.i +
                    sum.i * t4.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 5, J ) = C( 5, J ) - SUM*T5 >*/
            i__2 = j * c_dim1 + 5;
            i__3 = j * c_dim1 + 5;
            z__2.r = sum.r * t5.r - sum.i * t5.i, z__2.i = sum.r * t5.i +
                    sum.i * t5.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 6, J ) = C( 6, J ) - SUM*T6 >*/
            i__2 = j * c_dim1 + 6;
            i__3 = j * c_dim1 + 6;
            z__2.r = sum.r * t6.r - sum.i * t6.i, z__2.i = sum.r * t6.i +
                    sum.i * t6.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 7, J ) = C( 7, J ) - SUM*T7 >*/
            i__2 = j * c_dim1 + 7;
            i__3 = j * c_dim1 + 7;
            z__2.r = sum.r * t7.r - sum.i * t7.i, z__2.i = sum.r * t7.i +
                    sum.i * t7.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 8, J ) = C( 8, J ) - SUM*T8 >*/
            i__2 = j * c_dim1 + 8;
            i__3 = j * c_dim1 + 8;
            z__2.r = sum.r * t8.r - sum.i * t8.i, z__2.i = sum.r * t8.i +
                    sum.i * t8.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<   160    CONTINUE >*/
/* L160: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<   170    CONTINUE >*/
L170:

/*        Special code for 9 x 9 Householder */

/*<          V1 = DCONJG( V( 1 ) ) >*/
        d_cnjg(&z__1, &v[1]);
        v1.r = z__1.r, v1.i = z__1.i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = DCONJG( V( 2 ) ) >*/
        d_cnjg(&z__1, &v[2]);
        v2.r = z__1.r, v2.i = z__1.i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          V3 = DCONJG( V( 3 ) ) >*/
        d_cnjg(&z__1, &v[3]);
        v3.r = z__1.r, v3.i = z__1.i;
/*<          T3 = TAU*DCONJG( V3 ) >*/
        d_cnjg(&z__2, &v3);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t3.r = z__1.r, t3.i = z__1.i;
/*<          V4 = DCONJG( V( 4 ) ) >*/
        d_cnjg(&z__1, &v[4]);
        v4.r = z__1.r, v4.i = z__1.i;
/*<          T4 = TAU*DCONJG( V4 ) >*/
        d_cnjg(&z__2, &v4);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t4.r = z__1.r, t4.i = z__1.i;
/*<          V5 = DCONJG( V( 5 ) ) >*/
        d_cnjg(&z__1, &v[5]);
        v5.r = z__1.r, v5.i = z__1.i;
/*<          T5 = TAU*DCONJG( V5 ) >*/
        d_cnjg(&z__2, &v5);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t5.r = z__1.r, t5.i = z__1.i;
/*<          V6 = DCONJG( V( 6 ) ) >*/
        d_cnjg(&z__1, &v[6]);
        v6.r = z__1.r, v6.i = z__1.i;
/*<          T6 = TAU*DCONJG( V6 ) >*/
        d_cnjg(&z__2, &v6);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t6.r = z__1.r, t6.i = z__1.i;
/*<          V7 = DCONJG( V( 7 ) ) >*/
        d_cnjg(&z__1, &v[7]);
        v7.r = z__1.r, v7.i = z__1.i;
/*<          T7 = TAU*DCONJG( V7 ) >*/
        d_cnjg(&z__2, &v7);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t7.r = z__1.r, t7.i = z__1.i;
/*<          V8 = DCONJG( V( 8 ) ) >*/
        d_cnjg(&z__1, &v[8]);
        v8.r = z__1.r, v8.i = z__1.i;
/*<          T8 = TAU*DCONJG( V8 ) >*/
        d_cnjg(&z__2, &v8);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t8.r = z__1.r, t8.i = z__1.i;
/*<          V9 = DCONJG( V( 9 ) ) >*/
        d_cnjg(&z__1, &v[9]);
        v9.r = z__1.r, v9.i = z__1.i;
/*<          T9 = TAU*DCONJG( V9 ) >*/
        d_cnjg(&z__2, &v9);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t9.r = z__1.r, t9.i = z__1.i;
/*<          DO 180 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<    >*/
            i__2 = j * c_dim1 + 1;
            z__9.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__9.i = v1.r *
                    c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j * c_dim1 + 2;
            z__10.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__10.i = v2.r
                    * c__[i__3].i + v2.i * c__[i__3].r;
            z__8.r = z__9.r + z__10.r, z__8.i = z__9.i + z__10.i;
            i__4 = j * c_dim1 + 3;
            z__11.r = v3.r * c__[i__4].r - v3.i * c__[i__4].i, z__11.i = v3.r
                    * c__[i__4].i + v3.i * c__[i__4].r;
            z__7.r = z__8.r + z__11.r, z__7.i = z__8.i + z__11.i;
            i__5 = j * c_dim1 + 4;
            z__12.r = v4.r * c__[i__5].r - v4.i * c__[i__5].i, z__12.i = v4.r
                    * c__[i__5].i + v4.i * c__[i__5].r;
            z__6.r = z__7.r + z__12.r, z__6.i = z__7.i + z__12.i;
            i__6 = j * c_dim1 + 5;
            z__13.r = v5.r * c__[i__6].r - v5.i * c__[i__6].i, z__13.i = v5.r
                    * c__[i__6].i + v5.i * c__[i__6].r;
            z__5.r = z__6.r + z__13.r, z__5.i = z__6.i + z__13.i;
            i__7 = j * c_dim1 + 6;
            z__14.r = v6.r * c__[i__7].r - v6.i * c__[i__7].i, z__14.i = v6.r
                    * c__[i__7].i + v6.i * c__[i__7].r;
            z__4.r = z__5.r + z__14.r, z__4.i = z__5.i + z__14.i;
            i__8 = j * c_dim1 + 7;
            z__15.r = v7.r * c__[i__8].r - v7.i * c__[i__8].i, z__15.i = v7.r
                    * c__[i__8].i + v7.i * c__[i__8].r;
            z__3.r = z__4.r + z__15.r, z__3.i = z__4.i + z__15.i;
            i__9 = j * c_dim1 + 8;
            z__16.r = v8.r * c__[i__9].r - v8.i * c__[i__9].i, z__16.i = v8.r
                    * c__[i__9].i + v8.i * c__[i__9].r;
            z__2.r = z__3.r + z__16.r, z__2.i = z__3.i + z__16.i;
            i__10 = j * c_dim1 + 9;
            z__17.r = v9.r * c__[i__10].r - v9.i * c__[i__10].i, z__17.i =
                    v9.r * c__[i__10].i + v9.i * c__[i__10].r;
            z__1.r = z__2.r + z__17.r, z__1.i = z__2.i + z__17.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( 1, J ) = C( 1, J ) - SUM*T1 >*/
            i__2 = j * c_dim1 + 1;
            i__3 = j * c_dim1 + 1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 2, J ) = C( 2, J ) - SUM*T2 >*/
            i__2 = j * c_dim1 + 2;
            i__3 = j * c_dim1 + 2;
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 3, J ) = C( 3, J ) - SUM*T3 >*/
            i__2 = j * c_dim1 + 3;
            i__3 = j * c_dim1 + 3;
            z__2.r = sum.r * t3.r - sum.i * t3.i, z__2.i = sum.r * t3.i +
                    sum.i * t3.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 4, J ) = C( 4, J ) - SUM*T4 >*/
            i__2 = j * c_dim1 + 4;
            i__3 = j * c_dim1 + 4;
            z__2.r = sum.r * t4.r - sum.i * t4.i, z__2.i = sum.r * t4.i +
                    sum.i * t4.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 5, J ) = C( 5, J ) - SUM*T5 >*/
            i__2 = j * c_dim1 + 5;
            i__3 = j * c_dim1 + 5;
            z__2.r = sum.r * t5.r - sum.i * t5.i, z__2.i = sum.r * t5.i +
                    sum.i * t5.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 6, J ) = C( 6, J ) - SUM*T6 >*/
            i__2 = j * c_dim1 + 6;
            i__3 = j * c_dim1 + 6;
            z__2.r = sum.r * t6.r - sum.i * t6.i, z__2.i = sum.r * t6.i +
                    sum.i * t6.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 7, J ) = C( 7, J ) - SUM*T7 >*/
            i__2 = j * c_dim1 + 7;
            i__3 = j * c_dim1 + 7;
            z__2.r = sum.r * t7.r - sum.i * t7.i, z__2.i = sum.r * t7.i +
                    sum.i * t7.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 8, J ) = C( 8, J ) - SUM*T8 >*/
            i__2 = j * c_dim1 + 8;
            i__3 = j * c_dim1 + 8;
            z__2.r = sum.r * t8.r - sum.i * t8.i, z__2.i = sum.r * t8.i +
                    sum.i * t8.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 9, J ) = C( 9, J ) - SUM*T9 >*/
            i__2 = j * c_dim1 + 9;
            i__3 = j * c_dim1 + 9;
            z__2.r = sum.r * t9.r - sum.i * t9.i, z__2.i = sum.r * t9.i +
                    sum.i * t9.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<   180    CONTINUE >*/
/* L180: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<   190    CONTINUE >*/
L190:

/*        Special code for 10 x 10 Householder */

/*<          V1 = DCONJG( V( 1 ) ) >*/
        d_cnjg(&z__1, &v[1]);
        v1.r = z__1.r, v1.i = z__1.i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = DCONJG( V( 2 ) ) >*/
        d_cnjg(&z__1, &v[2]);
        v2.r = z__1.r, v2.i = z__1.i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          V3 = DCONJG( V( 3 ) ) >*/
        d_cnjg(&z__1, &v[3]);
        v3.r = z__1.r, v3.i = z__1.i;
/*<          T3 = TAU*DCONJG( V3 ) >*/
        d_cnjg(&z__2, &v3);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t3.r = z__1.r, t3.i = z__1.i;
/*<          V4 = DCONJG( V( 4 ) ) >*/
        d_cnjg(&z__1, &v[4]);
        v4.r = z__1.r, v4.i = z__1.i;
/*<          T4 = TAU*DCONJG( V4 ) >*/
        d_cnjg(&z__2, &v4);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t4.r = z__1.r, t4.i = z__1.i;
/*<          V5 = DCONJG( V( 5 ) ) >*/
        d_cnjg(&z__1, &v[5]);
        v5.r = z__1.r, v5.i = z__1.i;
/*<          T5 = TAU*DCONJG( V5 ) >*/
        d_cnjg(&z__2, &v5);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t5.r = z__1.r, t5.i = z__1.i;
/*<          V6 = DCONJG( V( 6 ) ) >*/
        d_cnjg(&z__1, &v[6]);
        v6.r = z__1.r, v6.i = z__1.i;
/*<          T6 = TAU*DCONJG( V6 ) >*/
        d_cnjg(&z__2, &v6);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t6.r = z__1.r, t6.i = z__1.i;
/*<          V7 = DCONJG( V( 7 ) ) >*/
        d_cnjg(&z__1, &v[7]);
        v7.r = z__1.r, v7.i = z__1.i;
/*<          T7 = TAU*DCONJG( V7 ) >*/
        d_cnjg(&z__2, &v7);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t7.r = z__1.r, t7.i = z__1.i;
/*<          V8 = DCONJG( V( 8 ) ) >*/
        d_cnjg(&z__1, &v[8]);
        v8.r = z__1.r, v8.i = z__1.i;
/*<          T8 = TAU*DCONJG( V8 ) >*/
        d_cnjg(&z__2, &v8);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t8.r = z__1.r, t8.i = z__1.i;
/*<          V9 = DCONJG( V( 9 ) ) >*/
        d_cnjg(&z__1, &v[9]);
        v9.r = z__1.r, v9.i = z__1.i;
/*<          T9 = TAU*DCONJG( V9 ) >*/
        d_cnjg(&z__2, &v9);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t9.r = z__1.r, t9.i = z__1.i;
/*<          V10 = DCONJG( V( 10 ) ) >*/
        d_cnjg(&z__1, &v[10]);
        v10.r = z__1.r, v10.i = z__1.i;
/*<          T10 = TAU*DCONJG( V10 ) >*/
        d_cnjg(&z__2, &v10);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t10.r = z__1.r, t10.i = z__1.i;
/*<          DO 200 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<    >*/
            i__2 = j * c_dim1 + 1;
            z__10.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__10.i = v1.r
                    * c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j * c_dim1 + 2;
            z__11.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__11.i = v2.r
                    * c__[i__3].i + v2.i * c__[i__3].r;
            z__9.r = z__10.r + z__11.r, z__9.i = z__10.i + z__11.i;
            i__4 = j * c_dim1 + 3;
            z__12.r = v3.r * c__[i__4].r - v3.i * c__[i__4].i, z__12.i = v3.r
                    * c__[i__4].i + v3.i * c__[i__4].r;
            z__8.r = z__9.r + z__12.r, z__8.i = z__9.i + z__12.i;
            i__5 = j * c_dim1 + 4;
            z__13.r = v4.r * c__[i__5].r - v4.i * c__[i__5].i, z__13.i = v4.r
                    * c__[i__5].i + v4.i * c__[i__5].r;
            z__7.r = z__8.r + z__13.r, z__7.i = z__8.i + z__13.i;
            i__6 = j * c_dim1 + 5;
            z__14.r = v5.r * c__[i__6].r - v5.i * c__[i__6].i, z__14.i = v5.r
                    * c__[i__6].i + v5.i * c__[i__6].r;
            z__6.r = z__7.r + z__14.r, z__6.i = z__7.i + z__14.i;
            i__7 = j * c_dim1 + 6;
            z__15.r = v6.r * c__[i__7].r - v6.i * c__[i__7].i, z__15.i = v6.r
                    * c__[i__7].i + v6.i * c__[i__7].r;
            z__5.r = z__6.r + z__15.r, z__5.i = z__6.i + z__15.i;
            i__8 = j * c_dim1 + 7;
            z__16.r = v7.r * c__[i__8].r - v7.i * c__[i__8].i, z__16.i = v7.r
                    * c__[i__8].i + v7.i * c__[i__8].r;
            z__4.r = z__5.r + z__16.r, z__4.i = z__5.i + z__16.i;
            i__9 = j * c_dim1 + 8;
            z__17.r = v8.r * c__[i__9].r - v8.i * c__[i__9].i, z__17.i = v8.r
                    * c__[i__9].i + v8.i * c__[i__9].r;
            z__3.r = z__4.r + z__17.r, z__3.i = z__4.i + z__17.i;
            i__10 = j * c_dim1 + 9;
            z__18.r = v9.r * c__[i__10].r - v9.i * c__[i__10].i, z__18.i =
                    v9.r * c__[i__10].i + v9.i * c__[i__10].r;
            z__2.r = z__3.r + z__18.r, z__2.i = z__3.i + z__18.i;
            i__11 = j * c_dim1 + 10;
            z__19.r = v10.r * c__[i__11].r - v10.i * c__[i__11].i, z__19.i =
                    v10.r * c__[i__11].i + v10.i * c__[i__11].r;
            z__1.r = z__2.r + z__19.r, z__1.i = z__2.i + z__19.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( 1, J ) = C( 1, J ) - SUM*T1 >*/
            i__2 = j * c_dim1 + 1;
            i__3 = j * c_dim1 + 1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 2, J ) = C( 2, J ) - SUM*T2 >*/
            i__2 = j * c_dim1 + 2;
            i__3 = j * c_dim1 + 2;
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 3, J ) = C( 3, J ) - SUM*T3 >*/
            i__2 = j * c_dim1 + 3;
            i__3 = j * c_dim1 + 3;
            z__2.r = sum.r * t3.r - sum.i * t3.i, z__2.i = sum.r * t3.i +
                    sum.i * t3.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 4, J ) = C( 4, J ) - SUM*T4 >*/
            i__2 = j * c_dim1 + 4;
            i__3 = j * c_dim1 + 4;
            z__2.r = sum.r * t4.r - sum.i * t4.i, z__2.i = sum.r * t4.i +
                    sum.i * t4.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 5, J ) = C( 5, J ) - SUM*T5 >*/
            i__2 = j * c_dim1 + 5;
            i__3 = j * c_dim1 + 5;
            z__2.r = sum.r * t5.r - sum.i * t5.i, z__2.i = sum.r * t5.i +
                    sum.i * t5.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 6, J ) = C( 6, J ) - SUM*T6 >*/
            i__2 = j * c_dim1 + 6;
            i__3 = j * c_dim1 + 6;
            z__2.r = sum.r * t6.r - sum.i * t6.i, z__2.i = sum.r * t6.i +
                    sum.i * t6.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 7, J ) = C( 7, J ) - SUM*T7 >*/
            i__2 = j * c_dim1 + 7;
            i__3 = j * c_dim1 + 7;
            z__2.r = sum.r * t7.r - sum.i * t7.i, z__2.i = sum.r * t7.i +
                    sum.i * t7.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 8, J ) = C( 8, J ) - SUM*T8 >*/
            i__2 = j * c_dim1 + 8;
            i__3 = j * c_dim1 + 8;
            z__2.r = sum.r * t8.r - sum.i * t8.i, z__2.i = sum.r * t8.i +
                    sum.i * t8.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 9, J ) = C( 9, J ) - SUM*T9 >*/
            i__2 = j * c_dim1 + 9;
            i__3 = j * c_dim1 + 9;
            z__2.r = sum.r * t9.r - sum.i * t9.i, z__2.i = sum.r * t9.i +
                    sum.i * t9.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( 10, J ) = C( 10, J ) - SUM*T10 >*/
            i__2 = j * c_dim1 + 10;
            i__3 = j * c_dim1 + 10;
            z__2.r = sum.r * t10.r - sum.i * t10.i, z__2.i = sum.r * t10.i +
                    sum.i * t10.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<   200    CONTINUE >*/
/* L200: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<       ELSE >*/
    } else {

/*        Form  C * H, where H has order n. */

/*<    >*/
        switch (*n) {
            case 1:  goto L210;
            case 2:  goto L230;
            case 3:  goto L250;
            case 4:  goto L270;
            case 5:  goto L290;
            case 6:  goto L310;
            case 7:  goto L330;
            case 8:  goto L350;
            case 9:  goto L370;
            case 10:  goto L390;
        }

/*        Code for general N */

/*        w := C * v */

/*<    >*/
        zgemv_("No transpose", m, n, &c_b2, &c__[c_offset], ldc, &v[1], &c__1,
                 &c_b1, &work[1], &c__1, (ftnlen)12);

/*        C := C - tau * w * v' */

/*<          CALL ZGERC( M, N, -TAU, WORK, 1, V, 1, C, LDC ) >*/
        z__1.r = -tau->r, z__1.i = -tau->i;
        zgerc_(m, n, &z__1, &work[1], &c__1, &v[1], &c__1, &c__[c_offset],
                ldc);
/*<          GO TO 410 >*/
        goto L410;
/*<   210    CONTINUE >*/
L210:

/*        Special code for 1 x 1 Householder */

/*<          T1 = ONE - TAU*V( 1 )*DCONJG( V( 1 ) ) >*/
        z__3.r = tau->r * v[1].r - tau->i * v[1].i, z__3.i = tau->r * v[1].i
                + tau->i * v[1].r;
        d_cnjg(&z__4, &v[1]);
        z__2.r = z__3.r * z__4.r - z__3.i * z__4.i, z__2.i = z__3.r * z__4.i
                + z__3.i * z__4.r;
        z__1.r = 1. - z__2.r, z__1.i = 0. - z__2.i;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          DO 220 J = 1, M >*/
        i__1 = *m;
        for (j = 1; j <= i__1; ++j) {
/*<             C( J, 1 ) = T1*C( J, 1 ) >*/
            i__2 = j + c_dim1;
            i__3 = j + c_dim1;
            z__1.r = t1.r * c__[i__3].r - t1.i * c__[i__3].i, z__1.i = t1.r *
                    c__[i__3].i + t1.i * c__[i__3].r;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<   220    CONTINUE >*/
/* L220: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<   230    CONTINUE >*/
L230:

/*        Special code for 2 x 2 Householder */

/*<          V1 = V( 1 ) >*/
        v1.r = v[1].r, v1.i = v[1].i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = V( 2 ) >*/
        v2.r = v[2].r, v2.i = v[2].i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          DO 240 J = 1, M >*/
        i__1 = *m;
        for (j = 1; j <= i__1; ++j) {
/*<             SUM = V1*C( J, 1 ) + V2*C( J, 2 ) >*/
            i__2 = j + c_dim1;
            z__2.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__2.i = v1.r *
                    c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j + (c_dim1 << 1);
            z__3.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__3.i = v2.r *
                    c__[i__3].i + v2.i * c__[i__3].r;
            z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( J, 1 ) = C( J, 1 ) - SUM*T1 >*/
            i__2 = j + c_dim1;
            i__3 = j + c_dim1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 2 ) = C( J, 2 ) - SUM*T2 >*/
            i__2 = j + (c_dim1 << 1);
            i__3 = j + (c_dim1 << 1);
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<   240    CONTINUE >*/
/* L240: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<   250    CONTINUE >*/
L250:

/*        Special code for 3 x 3 Householder */

/*<          V1 = V( 1 ) >*/
        v1.r = v[1].r, v1.i = v[1].i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = V( 2 ) >*/
        v2.r = v[2].r, v2.i = v[2].i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          V3 = V( 3 ) >*/
        v3.r = v[3].r, v3.i = v[3].i;
/*<          T3 = TAU*DCONJG( V3 ) >*/
        d_cnjg(&z__2, &v3);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t3.r = z__1.r, t3.i = z__1.i;
/*<          DO 260 J = 1, M >*/
        i__1 = *m;
        for (j = 1; j <= i__1; ++j) {
/*<             SUM = V1*C( J, 1 ) + V2*C( J, 2 ) + V3*C( J, 3 ) >*/
            i__2 = j + c_dim1;
            z__3.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__3.i = v1.r *
                    c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j + (c_dim1 << 1);
            z__4.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__4.i = v2.r *
                    c__[i__3].i + v2.i * c__[i__3].r;
            z__2.r = z__3.r + z__4.r, z__2.i = z__3.i + z__4.i;
            i__4 = j + c_dim1 * 3;
            z__5.r = v3.r * c__[i__4].r - v3.i * c__[i__4].i, z__5.i = v3.r *
                    c__[i__4].i + v3.i * c__[i__4].r;
            z__1.r = z__2.r + z__5.r, z__1.i = z__2.i + z__5.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( J, 1 ) = C( J, 1 ) - SUM*T1 >*/
            i__2 = j + c_dim1;
            i__3 = j + c_dim1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 2 ) = C( J, 2 ) - SUM*T2 >*/
            i__2 = j + (c_dim1 << 1);
            i__3 = j + (c_dim1 << 1);
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 3 ) = C( J, 3 ) - SUM*T3 >*/
            i__2 = j + c_dim1 * 3;
            i__3 = j + c_dim1 * 3;
            z__2.r = sum.r * t3.r - sum.i * t3.i, z__2.i = sum.r * t3.i +
                    sum.i * t3.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<   260    CONTINUE >*/
/* L260: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<   270    CONTINUE >*/
L270:

/*        Special code for 4 x 4 Householder */

/*<          V1 = V( 1 ) >*/
        v1.r = v[1].r, v1.i = v[1].i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = V( 2 ) >*/
        v2.r = v[2].r, v2.i = v[2].i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          V3 = V( 3 ) >*/
        v3.r = v[3].r, v3.i = v[3].i;
/*<          T3 = TAU*DCONJG( V3 ) >*/
        d_cnjg(&z__2, &v3);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t3.r = z__1.r, t3.i = z__1.i;
/*<          V4 = V( 4 ) >*/
        v4.r = v[4].r, v4.i = v[4].i;
/*<          T4 = TAU*DCONJG( V4 ) >*/
        d_cnjg(&z__2, &v4);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t4.r = z__1.r, t4.i = z__1.i;
/*<          DO 280 J = 1, M >*/
        i__1 = *m;
        for (j = 1; j <= i__1; ++j) {
/*<    >*/
            i__2 = j + c_dim1;
            z__4.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__4.i = v1.r *
                    c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j + (c_dim1 << 1);
            z__5.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__5.i = v2.r *
                    c__[i__3].i + v2.i * c__[i__3].r;
            z__3.r = z__4.r + z__5.r, z__3.i = z__4.i + z__5.i;
            i__4 = j + c_dim1 * 3;
            z__6.r = v3.r * c__[i__4].r - v3.i * c__[i__4].i, z__6.i = v3.r *
                    c__[i__4].i + v3.i * c__[i__4].r;
            z__2.r = z__3.r + z__6.r, z__2.i = z__3.i + z__6.i;
            i__5 = j + (c_dim1 << 2);
            z__7.r = v4.r * c__[i__5].r - v4.i * c__[i__5].i, z__7.i = v4.r *
                    c__[i__5].i + v4.i * c__[i__5].r;
            z__1.r = z__2.r + z__7.r, z__1.i = z__2.i + z__7.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( J, 1 ) = C( J, 1 ) - SUM*T1 >*/
            i__2 = j + c_dim1;
            i__3 = j + c_dim1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 2 ) = C( J, 2 ) - SUM*T2 >*/
            i__2 = j + (c_dim1 << 1);
            i__3 = j + (c_dim1 << 1);
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 3 ) = C( J, 3 ) - SUM*T3 >*/
            i__2 = j + c_dim1 * 3;
            i__3 = j + c_dim1 * 3;
            z__2.r = sum.r * t3.r - sum.i * t3.i, z__2.i = sum.r * t3.i +
                    sum.i * t3.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 4 ) = C( J, 4 ) - SUM*T4 >*/
            i__2 = j + (c_dim1 << 2);
            i__3 = j + (c_dim1 << 2);
            z__2.r = sum.r * t4.r - sum.i * t4.i, z__2.i = sum.r * t4.i +
                    sum.i * t4.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<   280    CONTINUE >*/
/* L280: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<   290    CONTINUE >*/
L290:

/*        Special code for 5 x 5 Householder */

/*<          V1 = V( 1 ) >*/
        v1.r = v[1].r, v1.i = v[1].i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = V( 2 ) >*/
        v2.r = v[2].r, v2.i = v[2].i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          V3 = V( 3 ) >*/
        v3.r = v[3].r, v3.i = v[3].i;
/*<          T3 = TAU*DCONJG( V3 ) >*/
        d_cnjg(&z__2, &v3);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t3.r = z__1.r, t3.i = z__1.i;
/*<          V4 = V( 4 ) >*/
        v4.r = v[4].r, v4.i = v[4].i;
/*<          T4 = TAU*DCONJG( V4 ) >*/
        d_cnjg(&z__2, &v4);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t4.r = z__1.r, t4.i = z__1.i;
/*<          V5 = V( 5 ) >*/
        v5.r = v[5].r, v5.i = v[5].i;
/*<          T5 = TAU*DCONJG( V5 ) >*/
        d_cnjg(&z__2, &v5);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t5.r = z__1.r, t5.i = z__1.i;
/*<          DO 300 J = 1, M >*/
        i__1 = *m;
        for (j = 1; j <= i__1; ++j) {
/*<    >*/
            i__2 = j + c_dim1;
            z__5.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__5.i = v1.r *
                    c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j + (c_dim1 << 1);
            z__6.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__6.i = v2.r *
                    c__[i__3].i + v2.i * c__[i__3].r;
            z__4.r = z__5.r + z__6.r, z__4.i = z__5.i + z__6.i;
            i__4 = j + c_dim1 * 3;
            z__7.r = v3.r * c__[i__4].r - v3.i * c__[i__4].i, z__7.i = v3.r *
                    c__[i__4].i + v3.i * c__[i__4].r;
            z__3.r = z__4.r + z__7.r, z__3.i = z__4.i + z__7.i;
            i__5 = j + (c_dim1 << 2);
            z__8.r = v4.r * c__[i__5].r - v4.i * c__[i__5].i, z__8.i = v4.r *
                    c__[i__5].i + v4.i * c__[i__5].r;
            z__2.r = z__3.r + z__8.r, z__2.i = z__3.i + z__8.i;
            i__6 = j + c_dim1 * 5;
            z__9.r = v5.r * c__[i__6].r - v5.i * c__[i__6].i, z__9.i = v5.r *
                    c__[i__6].i + v5.i * c__[i__6].r;
            z__1.r = z__2.r + z__9.r, z__1.i = z__2.i + z__9.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( J, 1 ) = C( J, 1 ) - SUM*T1 >*/
            i__2 = j + c_dim1;
            i__3 = j + c_dim1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 2 ) = C( J, 2 ) - SUM*T2 >*/
            i__2 = j + (c_dim1 << 1);
            i__3 = j + (c_dim1 << 1);
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 3 ) = C( J, 3 ) - SUM*T3 >*/
            i__2 = j + c_dim1 * 3;
            i__3 = j + c_dim1 * 3;
            z__2.r = sum.r * t3.r - sum.i * t3.i, z__2.i = sum.r * t3.i +
                    sum.i * t3.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 4 ) = C( J, 4 ) - SUM*T4 >*/
            i__2 = j + (c_dim1 << 2);
            i__3 = j + (c_dim1 << 2);
            z__2.r = sum.r * t4.r - sum.i * t4.i, z__2.i = sum.r * t4.i +
                    sum.i * t4.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 5 ) = C( J, 5 ) - SUM*T5 >*/
            i__2 = j + c_dim1 * 5;
            i__3 = j + c_dim1 * 5;
            z__2.r = sum.r * t5.r - sum.i * t5.i, z__2.i = sum.r * t5.i +
                    sum.i * t5.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<   300    CONTINUE >*/
/* L300: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<   310    CONTINUE >*/
L310:

/*        Special code for 6 x 6 Householder */

/*<          V1 = V( 1 ) >*/
        v1.r = v[1].r, v1.i = v[1].i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = V( 2 ) >*/
        v2.r = v[2].r, v2.i = v[2].i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          V3 = V( 3 ) >*/
        v3.r = v[3].r, v3.i = v[3].i;
/*<          T3 = TAU*DCONJG( V3 ) >*/
        d_cnjg(&z__2, &v3);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t3.r = z__1.r, t3.i = z__1.i;
/*<          V4 = V( 4 ) >*/
        v4.r = v[4].r, v4.i = v[4].i;
/*<          T4 = TAU*DCONJG( V4 ) >*/
        d_cnjg(&z__2, &v4);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t4.r = z__1.r, t4.i = z__1.i;
/*<          V5 = V( 5 ) >*/
        v5.r = v[5].r, v5.i = v[5].i;
/*<          T5 = TAU*DCONJG( V5 ) >*/
        d_cnjg(&z__2, &v5);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t5.r = z__1.r, t5.i = z__1.i;
/*<          V6 = V( 6 ) >*/
        v6.r = v[6].r, v6.i = v[6].i;
/*<          T6 = TAU*DCONJG( V6 ) >*/
        d_cnjg(&z__2, &v6);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t6.r = z__1.r, t6.i = z__1.i;
/*<          DO 320 J = 1, M >*/
        i__1 = *m;
        for (j = 1; j <= i__1; ++j) {
/*<    >*/
            i__2 = j + c_dim1;
            z__6.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__6.i = v1.r *
                    c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j + (c_dim1 << 1);
            z__7.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__7.i = v2.r *
                    c__[i__3].i + v2.i * c__[i__3].r;
            z__5.r = z__6.r + z__7.r, z__5.i = z__6.i + z__7.i;
            i__4 = j + c_dim1 * 3;
            z__8.r = v3.r * c__[i__4].r - v3.i * c__[i__4].i, z__8.i = v3.r *
                    c__[i__4].i + v3.i * c__[i__4].r;
            z__4.r = z__5.r + z__8.r, z__4.i = z__5.i + z__8.i;
            i__5 = j + (c_dim1 << 2);
            z__9.r = v4.r * c__[i__5].r - v4.i * c__[i__5].i, z__9.i = v4.r *
                    c__[i__5].i + v4.i * c__[i__5].r;
            z__3.r = z__4.r + z__9.r, z__3.i = z__4.i + z__9.i;
            i__6 = j + c_dim1 * 5;
            z__10.r = v5.r * c__[i__6].r - v5.i * c__[i__6].i, z__10.i = v5.r
                    * c__[i__6].i + v5.i * c__[i__6].r;
            z__2.r = z__3.r + z__10.r, z__2.i = z__3.i + z__10.i;
            i__7 = j + c_dim1 * 6;
            z__11.r = v6.r * c__[i__7].r - v6.i * c__[i__7].i, z__11.i = v6.r
                    * c__[i__7].i + v6.i * c__[i__7].r;
            z__1.r = z__2.r + z__11.r, z__1.i = z__2.i + z__11.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( J, 1 ) = C( J, 1 ) - SUM*T1 >*/
            i__2 = j + c_dim1;
            i__3 = j + c_dim1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 2 ) = C( J, 2 ) - SUM*T2 >*/
            i__2 = j + (c_dim1 << 1);
            i__3 = j + (c_dim1 << 1);
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 3 ) = C( J, 3 ) - SUM*T3 >*/
            i__2 = j + c_dim1 * 3;
            i__3 = j + c_dim1 * 3;
            z__2.r = sum.r * t3.r - sum.i * t3.i, z__2.i = sum.r * t3.i +
                    sum.i * t3.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 4 ) = C( J, 4 ) - SUM*T4 >*/
            i__2 = j + (c_dim1 << 2);
            i__3 = j + (c_dim1 << 2);
            z__2.r = sum.r * t4.r - sum.i * t4.i, z__2.i = sum.r * t4.i +
                    sum.i * t4.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 5 ) = C( J, 5 ) - SUM*T5 >*/
            i__2 = j + c_dim1 * 5;
            i__3 = j + c_dim1 * 5;
            z__2.r = sum.r * t5.r - sum.i * t5.i, z__2.i = sum.r * t5.i +
                    sum.i * t5.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 6 ) = C( J, 6 ) - SUM*T6 >*/
            i__2 = j + c_dim1 * 6;
            i__3 = j + c_dim1 * 6;
            z__2.r = sum.r * t6.r - sum.i * t6.i, z__2.i = sum.r * t6.i +
                    sum.i * t6.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<   320    CONTINUE >*/
/* L320: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<   330    CONTINUE >*/
L330:

/*        Special code for 7 x 7 Householder */

/*<          V1 = V( 1 ) >*/
        v1.r = v[1].r, v1.i = v[1].i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = V( 2 ) >*/
        v2.r = v[2].r, v2.i = v[2].i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          V3 = V( 3 ) >*/
        v3.r = v[3].r, v3.i = v[3].i;
/*<          T3 = TAU*DCONJG( V3 ) >*/
        d_cnjg(&z__2, &v3);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t3.r = z__1.r, t3.i = z__1.i;
/*<          V4 = V( 4 ) >*/
        v4.r = v[4].r, v4.i = v[4].i;
/*<          T4 = TAU*DCONJG( V4 ) >*/
        d_cnjg(&z__2, &v4);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t4.r = z__1.r, t4.i = z__1.i;
/*<          V5 = V( 5 ) >*/
        v5.r = v[5].r, v5.i = v[5].i;
/*<          T5 = TAU*DCONJG( V5 ) >*/
        d_cnjg(&z__2, &v5);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t5.r = z__1.r, t5.i = z__1.i;
/*<          V6 = V( 6 ) >*/
        v6.r = v[6].r, v6.i = v[6].i;
/*<          T6 = TAU*DCONJG( V6 ) >*/
        d_cnjg(&z__2, &v6);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t6.r = z__1.r, t6.i = z__1.i;
/*<          V7 = V( 7 ) >*/
        v7.r = v[7].r, v7.i = v[7].i;
/*<          T7 = TAU*DCONJG( V7 ) >*/
        d_cnjg(&z__2, &v7);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t7.r = z__1.r, t7.i = z__1.i;
/*<          DO 340 J = 1, M >*/
        i__1 = *m;
        for (j = 1; j <= i__1; ++j) {
/*<    >*/
            i__2 = j + c_dim1;
            z__7.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__7.i = v1.r *
                    c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j + (c_dim1 << 1);
            z__8.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__8.i = v2.r *
                    c__[i__3].i + v2.i * c__[i__3].r;
            z__6.r = z__7.r + z__8.r, z__6.i = z__7.i + z__8.i;
            i__4 = j + c_dim1 * 3;
            z__9.r = v3.r * c__[i__4].r - v3.i * c__[i__4].i, z__9.i = v3.r *
                    c__[i__4].i + v3.i * c__[i__4].r;
            z__5.r = z__6.r + z__9.r, z__5.i = z__6.i + z__9.i;
            i__5 = j + (c_dim1 << 2);
            z__10.r = v4.r * c__[i__5].r - v4.i * c__[i__5].i, z__10.i = v4.r
                    * c__[i__5].i + v4.i * c__[i__5].r;
            z__4.r = z__5.r + z__10.r, z__4.i = z__5.i + z__10.i;
            i__6 = j + c_dim1 * 5;
            z__11.r = v5.r * c__[i__6].r - v5.i * c__[i__6].i, z__11.i = v5.r
                    * c__[i__6].i + v5.i * c__[i__6].r;
            z__3.r = z__4.r + z__11.r, z__3.i = z__4.i + z__11.i;
            i__7 = j + c_dim1 * 6;
            z__12.r = v6.r * c__[i__7].r - v6.i * c__[i__7].i, z__12.i = v6.r
                    * c__[i__7].i + v6.i * c__[i__7].r;
            z__2.r = z__3.r + z__12.r, z__2.i = z__3.i + z__12.i;
            i__8 = j + c_dim1 * 7;
            z__13.r = v7.r * c__[i__8].r - v7.i * c__[i__8].i, z__13.i = v7.r
                    * c__[i__8].i + v7.i * c__[i__8].r;
            z__1.r = z__2.r + z__13.r, z__1.i = z__2.i + z__13.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( J, 1 ) = C( J, 1 ) - SUM*T1 >*/
            i__2 = j + c_dim1;
            i__3 = j + c_dim1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 2 ) = C( J, 2 ) - SUM*T2 >*/
            i__2 = j + (c_dim1 << 1);
            i__3 = j + (c_dim1 << 1);
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 3 ) = C( J, 3 ) - SUM*T3 >*/
            i__2 = j + c_dim1 * 3;
            i__3 = j + c_dim1 * 3;
            z__2.r = sum.r * t3.r - sum.i * t3.i, z__2.i = sum.r * t3.i +
                    sum.i * t3.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 4 ) = C( J, 4 ) - SUM*T4 >*/
            i__2 = j + (c_dim1 << 2);
            i__3 = j + (c_dim1 << 2);
            z__2.r = sum.r * t4.r - sum.i * t4.i, z__2.i = sum.r * t4.i +
                    sum.i * t4.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 5 ) = C( J, 5 ) - SUM*T5 >*/
            i__2 = j + c_dim1 * 5;
            i__3 = j + c_dim1 * 5;
            z__2.r = sum.r * t5.r - sum.i * t5.i, z__2.i = sum.r * t5.i +
                    sum.i * t5.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 6 ) = C( J, 6 ) - SUM*T6 >*/
            i__2 = j + c_dim1 * 6;
            i__3 = j + c_dim1 * 6;
            z__2.r = sum.r * t6.r - sum.i * t6.i, z__2.i = sum.r * t6.i +
                    sum.i * t6.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 7 ) = C( J, 7 ) - SUM*T7 >*/
            i__2 = j + c_dim1 * 7;
            i__3 = j + c_dim1 * 7;
            z__2.r = sum.r * t7.r - sum.i * t7.i, z__2.i = sum.r * t7.i +
                    sum.i * t7.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<   340    CONTINUE >*/
/* L340: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<   350    CONTINUE >*/
L350:

/*        Special code for 8 x 8 Householder */

/*<          V1 = V( 1 ) >*/
        v1.r = v[1].r, v1.i = v[1].i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = V( 2 ) >*/
        v2.r = v[2].r, v2.i = v[2].i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          V3 = V( 3 ) >*/
        v3.r = v[3].r, v3.i = v[3].i;
/*<          T3 = TAU*DCONJG( V3 ) >*/
        d_cnjg(&z__2, &v3);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t3.r = z__1.r, t3.i = z__1.i;
/*<          V4 = V( 4 ) >*/
        v4.r = v[4].r, v4.i = v[4].i;
/*<          T4 = TAU*DCONJG( V4 ) >*/
        d_cnjg(&z__2, &v4);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t4.r = z__1.r, t4.i = z__1.i;
/*<          V5 = V( 5 ) >*/
        v5.r = v[5].r, v5.i = v[5].i;
/*<          T5 = TAU*DCONJG( V5 ) >*/
        d_cnjg(&z__2, &v5);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t5.r = z__1.r, t5.i = z__1.i;
/*<          V6 = V( 6 ) >*/
        v6.r = v[6].r, v6.i = v[6].i;
/*<          T6 = TAU*DCONJG( V6 ) >*/
        d_cnjg(&z__2, &v6);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t6.r = z__1.r, t6.i = z__1.i;
/*<          V7 = V( 7 ) >*/
        v7.r = v[7].r, v7.i = v[7].i;
/*<          T7 = TAU*DCONJG( V7 ) >*/
        d_cnjg(&z__2, &v7);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t7.r = z__1.r, t7.i = z__1.i;
/*<          V8 = V( 8 ) >*/
        v8.r = v[8].r, v8.i = v[8].i;
/*<          T8 = TAU*DCONJG( V8 ) >*/
        d_cnjg(&z__2, &v8);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t8.r = z__1.r, t8.i = z__1.i;
/*<          DO 360 J = 1, M >*/
        i__1 = *m;
        for (j = 1; j <= i__1; ++j) {
/*<    >*/
            i__2 = j + c_dim1;
            z__8.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__8.i = v1.r *
                    c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j + (c_dim1 << 1);
            z__9.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__9.i = v2.r *
                    c__[i__3].i + v2.i * c__[i__3].r;
            z__7.r = z__8.r + z__9.r, z__7.i = z__8.i + z__9.i;
            i__4 = j + c_dim1 * 3;
            z__10.r = v3.r * c__[i__4].r - v3.i * c__[i__4].i, z__10.i = v3.r
                    * c__[i__4].i + v3.i * c__[i__4].r;
            z__6.r = z__7.r + z__10.r, z__6.i = z__7.i + z__10.i;
            i__5 = j + (c_dim1 << 2);
            z__11.r = v4.r * c__[i__5].r - v4.i * c__[i__5].i, z__11.i = v4.r
                    * c__[i__5].i + v4.i * c__[i__5].r;
            z__5.r = z__6.r + z__11.r, z__5.i = z__6.i + z__11.i;
            i__6 = j + c_dim1 * 5;
            z__12.r = v5.r * c__[i__6].r - v5.i * c__[i__6].i, z__12.i = v5.r
                    * c__[i__6].i + v5.i * c__[i__6].r;
            z__4.r = z__5.r + z__12.r, z__4.i = z__5.i + z__12.i;
            i__7 = j + c_dim1 * 6;
            z__13.r = v6.r * c__[i__7].r - v6.i * c__[i__7].i, z__13.i = v6.r
                    * c__[i__7].i + v6.i * c__[i__7].r;
            z__3.r = z__4.r + z__13.r, z__3.i = z__4.i + z__13.i;
            i__8 = j + c_dim1 * 7;
            z__14.r = v7.r * c__[i__8].r - v7.i * c__[i__8].i, z__14.i = v7.r
                    * c__[i__8].i + v7.i * c__[i__8].r;
            z__2.r = z__3.r + z__14.r, z__2.i = z__3.i + z__14.i;
            i__9 = j + (c_dim1 << 3);
            z__15.r = v8.r * c__[i__9].r - v8.i * c__[i__9].i, z__15.i = v8.r
                    * c__[i__9].i + v8.i * c__[i__9].r;
            z__1.r = z__2.r + z__15.r, z__1.i = z__2.i + z__15.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( J, 1 ) = C( J, 1 ) - SUM*T1 >*/
            i__2 = j + c_dim1;
            i__3 = j + c_dim1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 2 ) = C( J, 2 ) - SUM*T2 >*/
            i__2 = j + (c_dim1 << 1);
            i__3 = j + (c_dim1 << 1);
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 3 ) = C( J, 3 ) - SUM*T3 >*/
            i__2 = j + c_dim1 * 3;
            i__3 = j + c_dim1 * 3;
            z__2.r = sum.r * t3.r - sum.i * t3.i, z__2.i = sum.r * t3.i +
                    sum.i * t3.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 4 ) = C( J, 4 ) - SUM*T4 >*/
            i__2 = j + (c_dim1 << 2);
            i__3 = j + (c_dim1 << 2);
            z__2.r = sum.r * t4.r - sum.i * t4.i, z__2.i = sum.r * t4.i +
                    sum.i * t4.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 5 ) = C( J, 5 ) - SUM*T5 >*/
            i__2 = j + c_dim1 * 5;
            i__3 = j + c_dim1 * 5;
            z__2.r = sum.r * t5.r - sum.i * t5.i, z__2.i = sum.r * t5.i +
                    sum.i * t5.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 6 ) = C( J, 6 ) - SUM*T6 >*/
            i__2 = j + c_dim1 * 6;
            i__3 = j + c_dim1 * 6;
            z__2.r = sum.r * t6.r - sum.i * t6.i, z__2.i = sum.r * t6.i +
                    sum.i * t6.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 7 ) = C( J, 7 ) - SUM*T7 >*/
            i__2 = j + c_dim1 * 7;
            i__3 = j + c_dim1 * 7;
            z__2.r = sum.r * t7.r - sum.i * t7.i, z__2.i = sum.r * t7.i +
                    sum.i * t7.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 8 ) = C( J, 8 ) - SUM*T8 >*/
            i__2 = j + (c_dim1 << 3);
            i__3 = j + (c_dim1 << 3);
            z__2.r = sum.r * t8.r - sum.i * t8.i, z__2.i = sum.r * t8.i +
                    sum.i * t8.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<   360    CONTINUE >*/
/* L360: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<   370    CONTINUE >*/
L370:

/*        Special code for 9 x 9 Householder */

/*<          V1 = V( 1 ) >*/
        v1.r = v[1].r, v1.i = v[1].i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = V( 2 ) >*/
        v2.r = v[2].r, v2.i = v[2].i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          V3 = V( 3 ) >*/
        v3.r = v[3].r, v3.i = v[3].i;
/*<          T3 = TAU*DCONJG( V3 ) >*/
        d_cnjg(&z__2, &v3);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t3.r = z__1.r, t3.i = z__1.i;
/*<          V4 = V( 4 ) >*/
        v4.r = v[4].r, v4.i = v[4].i;
/*<          T4 = TAU*DCONJG( V4 ) >*/
        d_cnjg(&z__2, &v4);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t4.r = z__1.r, t4.i = z__1.i;
/*<          V5 = V( 5 ) >*/
        v5.r = v[5].r, v5.i = v[5].i;
/*<          T5 = TAU*DCONJG( V5 ) >*/
        d_cnjg(&z__2, &v5);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t5.r = z__1.r, t5.i = z__1.i;
/*<          V6 = V( 6 ) >*/
        v6.r = v[6].r, v6.i = v[6].i;
/*<          T6 = TAU*DCONJG( V6 ) >*/
        d_cnjg(&z__2, &v6);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t6.r = z__1.r, t6.i = z__1.i;
/*<          V7 = V( 7 ) >*/
        v7.r = v[7].r, v7.i = v[7].i;
/*<          T7 = TAU*DCONJG( V7 ) >*/
        d_cnjg(&z__2, &v7);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t7.r = z__1.r, t7.i = z__1.i;
/*<          V8 = V( 8 ) >*/
        v8.r = v[8].r, v8.i = v[8].i;
/*<          T8 = TAU*DCONJG( V8 ) >*/
        d_cnjg(&z__2, &v8);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t8.r = z__1.r, t8.i = z__1.i;
/*<          V9 = V( 9 ) >*/
        v9.r = v[9].r, v9.i = v[9].i;
/*<          T9 = TAU*DCONJG( V9 ) >*/
        d_cnjg(&z__2, &v9);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t9.r = z__1.r, t9.i = z__1.i;
/*<          DO 380 J = 1, M >*/
        i__1 = *m;
        for (j = 1; j <= i__1; ++j) {
/*<    >*/
            i__2 = j + c_dim1;
            z__9.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__9.i = v1.r *
                    c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j + (c_dim1 << 1);
            z__10.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__10.i = v2.r
                    * c__[i__3].i + v2.i * c__[i__3].r;
            z__8.r = z__9.r + z__10.r, z__8.i = z__9.i + z__10.i;
            i__4 = j + c_dim1 * 3;
            z__11.r = v3.r * c__[i__4].r - v3.i * c__[i__4].i, z__11.i = v3.r
                    * c__[i__4].i + v3.i * c__[i__4].r;
            z__7.r = z__8.r + z__11.r, z__7.i = z__8.i + z__11.i;
            i__5 = j + (c_dim1 << 2);
            z__12.r = v4.r * c__[i__5].r - v4.i * c__[i__5].i, z__12.i = v4.r
                    * c__[i__5].i + v4.i * c__[i__5].r;
            z__6.r = z__7.r + z__12.r, z__6.i = z__7.i + z__12.i;
            i__6 = j + c_dim1 * 5;
            z__13.r = v5.r * c__[i__6].r - v5.i * c__[i__6].i, z__13.i = v5.r
                    * c__[i__6].i + v5.i * c__[i__6].r;
            z__5.r = z__6.r + z__13.r, z__5.i = z__6.i + z__13.i;
            i__7 = j + c_dim1 * 6;
            z__14.r = v6.r * c__[i__7].r - v6.i * c__[i__7].i, z__14.i = v6.r
                    * c__[i__7].i + v6.i * c__[i__7].r;
            z__4.r = z__5.r + z__14.r, z__4.i = z__5.i + z__14.i;
            i__8 = j + c_dim1 * 7;
            z__15.r = v7.r * c__[i__8].r - v7.i * c__[i__8].i, z__15.i = v7.r
                    * c__[i__8].i + v7.i * c__[i__8].r;
            z__3.r = z__4.r + z__15.r, z__3.i = z__4.i + z__15.i;
            i__9 = j + (c_dim1 << 3);
            z__16.r = v8.r * c__[i__9].r - v8.i * c__[i__9].i, z__16.i = v8.r
                    * c__[i__9].i + v8.i * c__[i__9].r;
            z__2.r = z__3.r + z__16.r, z__2.i = z__3.i + z__16.i;
            i__10 = j + c_dim1 * 9;
            z__17.r = v9.r * c__[i__10].r - v9.i * c__[i__10].i, z__17.i =
                    v9.r * c__[i__10].i + v9.i * c__[i__10].r;
            z__1.r = z__2.r + z__17.r, z__1.i = z__2.i + z__17.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( J, 1 ) = C( J, 1 ) - SUM*T1 >*/
            i__2 = j + c_dim1;
            i__3 = j + c_dim1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 2 ) = C( J, 2 ) - SUM*T2 >*/
            i__2 = j + (c_dim1 << 1);
            i__3 = j + (c_dim1 << 1);
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 3 ) = C( J, 3 ) - SUM*T3 >*/
            i__2 = j + c_dim1 * 3;
            i__3 = j + c_dim1 * 3;
            z__2.r = sum.r * t3.r - sum.i * t3.i, z__2.i = sum.r * t3.i +
                    sum.i * t3.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 4 ) = C( J, 4 ) - SUM*T4 >*/
            i__2 = j + (c_dim1 << 2);
            i__3 = j + (c_dim1 << 2);
            z__2.r = sum.r * t4.r - sum.i * t4.i, z__2.i = sum.r * t4.i +
                    sum.i * t4.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 5 ) = C( J, 5 ) - SUM*T5 >*/
            i__2 = j + c_dim1 * 5;
            i__3 = j + c_dim1 * 5;
            z__2.r = sum.r * t5.r - sum.i * t5.i, z__2.i = sum.r * t5.i +
                    sum.i * t5.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 6 ) = C( J, 6 ) - SUM*T6 >*/
            i__2 = j + c_dim1 * 6;
            i__3 = j + c_dim1 * 6;
            z__2.r = sum.r * t6.r - sum.i * t6.i, z__2.i = sum.r * t6.i +
                    sum.i * t6.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 7 ) = C( J, 7 ) - SUM*T7 >*/
            i__2 = j + c_dim1 * 7;
            i__3 = j + c_dim1 * 7;
            z__2.r = sum.r * t7.r - sum.i * t7.i, z__2.i = sum.r * t7.i +
                    sum.i * t7.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 8 ) = C( J, 8 ) - SUM*T8 >*/
            i__2 = j + (c_dim1 << 3);
            i__3 = j + (c_dim1 << 3);
            z__2.r = sum.r * t8.r - sum.i * t8.i, z__2.i = sum.r * t8.i +
                    sum.i * t8.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 9 ) = C( J, 9 ) - SUM*T9 >*/
            i__2 = j + c_dim1 * 9;
            i__3 = j + c_dim1 * 9;
            z__2.r = sum.r * t9.r - sum.i * t9.i, z__2.i = sum.r * t9.i +
                    sum.i * t9.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<   380    CONTINUE >*/
/* L380: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<   390    CONTINUE >*/
L390:

/*        Special code for 10 x 10 Householder */

/*<          V1 = V( 1 ) >*/
        v1.r = v[1].r, v1.i = v[1].i;
/*<          T1 = TAU*DCONJG( V1 ) >*/
        d_cnjg(&z__2, &v1);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t1.r = z__1.r, t1.i = z__1.i;
/*<          V2 = V( 2 ) >*/
        v2.r = v[2].r, v2.i = v[2].i;
/*<          T2 = TAU*DCONJG( V2 ) >*/
        d_cnjg(&z__2, &v2);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t2.r = z__1.r, t2.i = z__1.i;
/*<          V3 = V( 3 ) >*/
        v3.r = v[3].r, v3.i = v[3].i;
/*<          T3 = TAU*DCONJG( V3 ) >*/
        d_cnjg(&z__2, &v3);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t3.r = z__1.r, t3.i = z__1.i;
/*<          V4 = V( 4 ) >*/
        v4.r = v[4].r, v4.i = v[4].i;
/*<          T4 = TAU*DCONJG( V4 ) >*/
        d_cnjg(&z__2, &v4);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t4.r = z__1.r, t4.i = z__1.i;
/*<          V5 = V( 5 ) >*/
        v5.r = v[5].r, v5.i = v[5].i;
/*<          T5 = TAU*DCONJG( V5 ) >*/
        d_cnjg(&z__2, &v5);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t5.r = z__1.r, t5.i = z__1.i;
/*<          V6 = V( 6 ) >*/
        v6.r = v[6].r, v6.i = v[6].i;
/*<          T6 = TAU*DCONJG( V6 ) >*/
        d_cnjg(&z__2, &v6);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t6.r = z__1.r, t6.i = z__1.i;
/*<          V7 = V( 7 ) >*/
        v7.r = v[7].r, v7.i = v[7].i;
/*<          T7 = TAU*DCONJG( V7 ) >*/
        d_cnjg(&z__2, &v7);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t7.r = z__1.r, t7.i = z__1.i;
/*<          V8 = V( 8 ) >*/
        v8.r = v[8].r, v8.i = v[8].i;
/*<          T8 = TAU*DCONJG( V8 ) >*/
        d_cnjg(&z__2, &v8);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t8.r = z__1.r, t8.i = z__1.i;
/*<          V9 = V( 9 ) >*/
        v9.r = v[9].r, v9.i = v[9].i;
/*<          T9 = TAU*DCONJG( V9 ) >*/
        d_cnjg(&z__2, &v9);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t9.r = z__1.r, t9.i = z__1.i;
/*<          V10 = V( 10 ) >*/
        v10.r = v[10].r, v10.i = v[10].i;
/*<          T10 = TAU*DCONJG( V10 ) >*/
        d_cnjg(&z__2, &v10);
        z__1.r = tau->r * z__2.r - tau->i * z__2.i, z__1.i = tau->r * z__2.i
                + tau->i * z__2.r;
        t10.r = z__1.r, t10.i = z__1.i;
/*<          DO 400 J = 1, M >*/
        i__1 = *m;
        for (j = 1; j <= i__1; ++j) {
/*<    >*/
            i__2 = j + c_dim1;
            z__10.r = v1.r * c__[i__2].r - v1.i * c__[i__2].i, z__10.i = v1.r
                    * c__[i__2].i + v1.i * c__[i__2].r;
            i__3 = j + (c_dim1 << 1);
            z__11.r = v2.r * c__[i__3].r - v2.i * c__[i__3].i, z__11.i = v2.r
                    * c__[i__3].i + v2.i * c__[i__3].r;
            z__9.r = z__10.r + z__11.r, z__9.i = z__10.i + z__11.i;
            i__4 = j + c_dim1 * 3;
            z__12.r = v3.r * c__[i__4].r - v3.i * c__[i__4].i, z__12.i = v3.r
                    * c__[i__4].i + v3.i * c__[i__4].r;
            z__8.r = z__9.r + z__12.r, z__8.i = z__9.i + z__12.i;
            i__5 = j + (c_dim1 << 2);
            z__13.r = v4.r * c__[i__5].r - v4.i * c__[i__5].i, z__13.i = v4.r
                    * c__[i__5].i + v4.i * c__[i__5].r;
            z__7.r = z__8.r + z__13.r, z__7.i = z__8.i + z__13.i;
            i__6 = j + c_dim1 * 5;
            z__14.r = v5.r * c__[i__6].r - v5.i * c__[i__6].i, z__14.i = v5.r
                    * c__[i__6].i + v5.i * c__[i__6].r;
            z__6.r = z__7.r + z__14.r, z__6.i = z__7.i + z__14.i;
            i__7 = j + c_dim1 * 6;
            z__15.r = v6.r * c__[i__7].r - v6.i * c__[i__7].i, z__15.i = v6.r
                    * c__[i__7].i + v6.i * c__[i__7].r;
            z__5.r = z__6.r + z__15.r, z__5.i = z__6.i + z__15.i;
            i__8 = j + c_dim1 * 7;
            z__16.r = v7.r * c__[i__8].r - v7.i * c__[i__8].i, z__16.i = v7.r
                    * c__[i__8].i + v7.i * c__[i__8].r;
            z__4.r = z__5.r + z__16.r, z__4.i = z__5.i + z__16.i;
            i__9 = j + (c_dim1 << 3);
            z__17.r = v8.r * c__[i__9].r - v8.i * c__[i__9].i, z__17.i = v8.r
                    * c__[i__9].i + v8.i * c__[i__9].r;
            z__3.r = z__4.r + z__17.r, z__3.i = z__4.i + z__17.i;
            i__10 = j + c_dim1 * 9;
            z__18.r = v9.r * c__[i__10].r - v9.i * c__[i__10].i, z__18.i =
                    v9.r * c__[i__10].i + v9.i * c__[i__10].r;
            z__2.r = z__3.r + z__18.r, z__2.i = z__3.i + z__18.i;
            i__11 = j + c_dim1 * 10;
            z__19.r = v10.r * c__[i__11].r - v10.i * c__[i__11].i, z__19.i =
                    v10.r * c__[i__11].i + v10.i * c__[i__11].r;
            z__1.r = z__2.r + z__19.r, z__1.i = z__2.i + z__19.i;
            sum.r = z__1.r, sum.i = z__1.i;
/*<             C( J, 1 ) = C( J, 1 ) - SUM*T1 >*/
            i__2 = j + c_dim1;
            i__3 = j + c_dim1;
            z__2.r = sum.r * t1.r - sum.i * t1.i, z__2.i = sum.r * t1.i +
                    sum.i * t1.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 2 ) = C( J, 2 ) - SUM*T2 >*/
            i__2 = j + (c_dim1 << 1);
            i__3 = j + (c_dim1 << 1);
            z__2.r = sum.r * t2.r - sum.i * t2.i, z__2.i = sum.r * t2.i +
                    sum.i * t2.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 3 ) = C( J, 3 ) - SUM*T3 >*/
            i__2 = j + c_dim1 * 3;
            i__3 = j + c_dim1 * 3;
            z__2.r = sum.r * t3.r - sum.i * t3.i, z__2.i = sum.r * t3.i +
                    sum.i * t3.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 4 ) = C( J, 4 ) - SUM*T4 >*/
            i__2 = j + (c_dim1 << 2);
            i__3 = j + (c_dim1 << 2);
            z__2.r = sum.r * t4.r - sum.i * t4.i, z__2.i = sum.r * t4.i +
                    sum.i * t4.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 5 ) = C( J, 5 ) - SUM*T5 >*/
            i__2 = j + c_dim1 * 5;
            i__3 = j + c_dim1 * 5;
            z__2.r = sum.r * t5.r - sum.i * t5.i, z__2.i = sum.r * t5.i +
                    sum.i * t5.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 6 ) = C( J, 6 ) - SUM*T6 >*/
            i__2 = j + c_dim1 * 6;
            i__3 = j + c_dim1 * 6;
            z__2.r = sum.r * t6.r - sum.i * t6.i, z__2.i = sum.r * t6.i +
                    sum.i * t6.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 7 ) = C( J, 7 ) - SUM*T7 >*/
            i__2 = j + c_dim1 * 7;
            i__3 = j + c_dim1 * 7;
            z__2.r = sum.r * t7.r - sum.i * t7.i, z__2.i = sum.r * t7.i +
                    sum.i * t7.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 8 ) = C( J, 8 ) - SUM*T8 >*/
            i__2 = j + (c_dim1 << 3);
            i__3 = j + (c_dim1 << 3);
            z__2.r = sum.r * t8.r - sum.i * t8.i, z__2.i = sum.r * t8.i +
                    sum.i * t8.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 9 ) = C( J, 9 ) - SUM*T9 >*/
            i__2 = j + c_dim1 * 9;
            i__3 = j + c_dim1 * 9;
            z__2.r = sum.r * t9.r - sum.i * t9.i, z__2.i = sum.r * t9.i +
                    sum.i * t9.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<             C( J, 10 ) = C( J, 10 ) - SUM*T10 >*/
            i__2 = j + c_dim1 * 10;
            i__3 = j + c_dim1 * 10;
            z__2.r = sum.r * t10.r - sum.i * t10.i, z__2.i = sum.r * t10.i +
                    sum.i * t10.r;
            z__1.r = c__[i__3].r - z__2.r, z__1.i = c__[i__3].i - z__2.i;
            c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
/*<   400    CONTINUE >*/
/* L400: */
        }
/*<          GO TO 410 >*/
        goto L410;
/*<       END IF >*/
    }
/*<   410 CONTINUE >*/
L410:
/*<       RETURN >*/
    return 0;

/*     End of ZLARFX */

/*<       END >*/
} /* zlarfx_ */

#ifdef __cplusplus
        }
#endif
