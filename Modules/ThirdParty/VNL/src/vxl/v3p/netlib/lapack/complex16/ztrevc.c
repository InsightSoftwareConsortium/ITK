/* lapack/complex16/ztrevc.f -- translated by f2c (version 20050501).
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

static doublecomplex c_b2 = {1.,0.};
static integer c__1 = 1;

/*<    >*/
/* Subroutine */ int ztrevc_(char *side, char *howmny, logical *select,
        integer *n, doublecomplex *t, integer *ldt, doublecomplex *vl,
        integer *ldvl, doublecomplex *vr, integer *ldvr, integer *mm, integer
        *m, doublecomplex *work, doublereal *rwork, integer *info, ftnlen
        side_len, ftnlen howmny_len)
{
    /* System generated locals */
    integer t_dim1, t_offset, vl_dim1, vl_offset, vr_dim1, vr_offset, i__1,
            i__2, i__3, i__4, i__5;
    doublereal d__1, d__2, d__3;
    doublecomplex z__1, z__2;

    /* Builtin functions */
    double d_imag(doublecomplex *);
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    integer i__, j, k, ii, ki, is;
    doublereal ulp;
    logical allv;
    doublereal unfl, ovfl, smin;
    logical over;
    doublereal scale;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    doublereal remax;
    logical leftv, bothv;
    extern /* Subroutine */ int zgemv_(char *, integer *, integer *,
            doublecomplex *, doublecomplex *, integer *, doublecomplex *,
            integer *, doublecomplex *, doublecomplex *, integer *, ftnlen);
    logical somev;
    extern /* Subroutine */ int zcopy_(integer *, doublecomplex *, integer *,
            doublecomplex *, integer *), dlabad_(doublereal *, doublereal *);
    extern doublereal dlamch_(char *, ftnlen);
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen), zdscal_(
            integer *, doublereal *, doublecomplex *, integer *);
    extern integer izamax_(integer *, doublecomplex *, integer *);
    logical rightv;
    extern doublereal dzasum_(integer *, doublecomplex *, integer *);
    doublereal smlnum;
    extern /* Subroutine */ int zlatrs_(char *, char *, char *, char *,
            integer *, doublecomplex *, integer *, doublecomplex *,
            doublereal *, doublereal *, integer *, ftnlen, ftnlen, ftnlen,
            ftnlen);
    (void)side_len;
    (void)howmny_len;

/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          HOWMNY, SIDE >*/
/*<       INTEGER            INFO, LDT, LDVL, LDVR, M, MM, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       LOGICAL            SELECT( * ) >*/
/*<       DOUBLE PRECISION   RWORK( * ) >*/
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZTREVC computes some or all of the right and/or left eigenvectors of */
/*  a complex upper triangular matrix T. */

/*  The right eigenvector x and the left eigenvector y of T corresponding */
/*  to an eigenvalue w are defined by: */

/*               T*x = w*x,     y'*T = w*y' */

/*  where y' denotes the conjugate transpose of the vector y. */

/*  If all eigenvectors are requested, the routine may either return the */
/*  matrices X and/or Y of right or left eigenvectors of T, or the */
/*  products Q*X and/or Q*Y, where Q is an input unitary */
/*  matrix. If T was obtained from the Schur factorization of an */
/*  original matrix A = Q*T*Q', then Q*X and Q*Y are the matrices of */
/*  right or left eigenvectors of A. */

/*  Arguments */
/*  ========= */

/*  SIDE    (input) CHARACTER*1 */
/*          = 'R':  compute right eigenvectors only; */
/*          = 'L':  compute left eigenvectors only; */
/*          = 'B':  compute both right and left eigenvectors. */

/*  HOWMNY  (input) CHARACTER*1 */
/*          = 'A':  compute all right and/or left eigenvectors; */
/*          = 'B':  compute all right and/or left eigenvectors, */
/*                  and backtransform them using the input matrices */
/*                  supplied in VR and/or VL; */
/*          = 'S':  compute selected right and/or left eigenvectors, */
/*                  specified by the logical array SELECT. */

/*  SELECT  (input) LOGICAL array, dimension (N) */
/*          If HOWMNY = 'S', SELECT specifies the eigenvectors to be */
/*          computed. */
/*          If HOWMNY = 'A' or 'B', SELECT is not referenced. */
/*          To select the eigenvector corresponding to the j-th */
/*          eigenvalue, SELECT(j) must be set to .TRUE.. */

/*  N       (input) INTEGER */
/*          The order of the matrix T. N >= 0. */

/*  T       (input/output) COMPLEX*16 array, dimension (LDT,N) */
/*          The upper triangular matrix T.  T is modified, but restored */
/*          on exit. */

/*  LDT     (input) INTEGER */
/*          The leading dimension of the array T. LDT >= max(1,N). */

/*  VL      (input/output) COMPLEX*16 array, dimension (LDVL,MM) */
/*          On entry, if SIDE = 'L' or 'B' and HOWMNY = 'B', VL must */
/*          contain an N-by-N matrix Q (usually the unitary matrix Q of */
/*          Schur vectors returned by ZHSEQR). */
/*          On exit, if SIDE = 'L' or 'B', VL contains: */
/*          if HOWMNY = 'A', the matrix Y of left eigenvectors of T; */
/*                           VL is lower triangular. The i-th column */
/*                           VL(i) of VL is the eigenvector corresponding */
/*                           to T(i,i). */
/*          if HOWMNY = 'B', the matrix Q*Y; */
/*          if HOWMNY = 'S', the left eigenvectors of T specified by */
/*                           SELECT, stored consecutively in the columns */
/*                           of VL, in the same order as their */
/*                           eigenvalues. */
/*          If SIDE = 'R', VL is not referenced. */

/*  LDVL    (input) INTEGER */
/*          The leading dimension of the array VL.  LDVL >= max(1,N) if */
/*          SIDE = 'L' or 'B'; LDVL >= 1 otherwise. */

/*  VR      (input/output) COMPLEX*16 array, dimension (LDVR,MM) */
/*          On entry, if SIDE = 'R' or 'B' and HOWMNY = 'B', VR must */
/*          contain an N-by-N matrix Q (usually the unitary matrix Q of */
/*          Schur vectors returned by ZHSEQR). */
/*          On exit, if SIDE = 'R' or 'B', VR contains: */
/*          if HOWMNY = 'A', the matrix X of right eigenvectors of T; */
/*                           VR is upper triangular. The i-th column */
/*                           VR(i) of VR is the eigenvector corresponding */
/*                           to T(i,i). */
/*          if HOWMNY = 'B', the matrix Q*X; */
/*          if HOWMNY = 'S', the right eigenvectors of T specified by */
/*                           SELECT, stored consecutively in the columns */
/*                           of VR, in the same order as their */
/*                           eigenvalues. */
/*          If SIDE = 'L', VR is not referenced. */

/*  LDVR    (input) INTEGER */
/*          The leading dimension of the array VR.  LDVR >= max(1,N) if */
/*           SIDE = 'R' or 'B'; LDVR >= 1 otherwise. */

/*  MM      (input) INTEGER */
/*          The number of columns in the arrays VL and/or VR. MM >= M. */

/*  M       (output) INTEGER */
/*          The number of columns in the arrays VL and/or VR actually */
/*          used to store the eigenvectors.  If HOWMNY = 'A' or 'B', M */
/*          is set to N.  Each selected eigenvector occupies one */
/*          column. */

/*  WORK    (workspace) COMPLEX*16 array, dimension (2*N) */

/*  RWORK   (workspace) DOUBLE PRECISION array, dimension (N) */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value */

/*  Further Details */
/*  =============== */

/*  The algorithm used in this program is basically backward (forward) */
/*  substitution, with scaling to make the the code robust against */
/*  possible overflow. */

/*  Each eigenvector is normalized so that the element of largest */
/*  magnitude has magnitude 1; here the magnitude of a complex number */
/*  (x,y) is taken to be |x| + |y|. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 ) >*/
/*<       COMPLEX*16         CMZERO, CMONE >*/
/*<    >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            ALLV, BOTHV, LEFTV, OVER, RIGHTV, SOMEV >*/
/*<       INTEGER            I, II, IS, J, K, KI >*/
/*<       DOUBLE PRECISION   OVFL, REMAX, SCALE, SMIN, SMLNUM, ULP, UNFL >*/
/*<       COMPLEX*16         CDUM >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       INTEGER            IZAMAX >*/
/*<       DOUBLE PRECISION   DLAMCH, DZASUM >*/
/*<       EXTERNAL           LSAME, IZAMAX, DLAMCH, DZASUM >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA, ZCOPY, ZDSCAL, ZGEMV, ZLATRS >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, DBLE, DCMPLX, DCONJG, DIMAG, MAX >*/
/*     .. */
/*     .. Statement Functions .. */
/*<       DOUBLE PRECISION   CABS1 >*/
/*     .. */
/*     .. Statement Function definitions .. */
/*<       CABS1( CDUM ) = ABS( DBLE( CDUM ) ) + ABS( DIMAG( CDUM ) ) >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Decode and test the input parameters */

/*<       BOTHV = LSAME( SIDE, 'B' ) >*/
    /* Parameter adjustments */
    --select;
    t_dim1 = *ldt;
    t_offset = 1 + t_dim1;
    t -= t_offset;
    vl_dim1 = *ldvl;
    vl_offset = 1 + vl_dim1;
    vl -= vl_offset;
    vr_dim1 = *ldvr;
    vr_offset = 1 + vr_dim1;
    vr -= vr_offset;
    --work;
    --rwork;

    /* Function Body */
    bothv = lsame_(side, "B", (ftnlen)1, (ftnlen)1);
/*<       RIGHTV = LSAME( SIDE, 'R' ) .OR. BOTHV >*/
    rightv = lsame_(side, "R", (ftnlen)1, (ftnlen)1) || bothv;
/*<       LEFTV = LSAME( SIDE, 'L' ) .OR. BOTHV >*/
    leftv = lsame_(side, "L", (ftnlen)1, (ftnlen)1) || bothv;

/*<       ALLV = LSAME( HOWMNY, 'A' ) >*/
    allv = lsame_(howmny, "A", (ftnlen)1, (ftnlen)1);
/*<       OVER = LSAME( HOWMNY, 'B' ) >*/
    over = lsame_(howmny, "B", (ftnlen)1, (ftnlen)1);
/*<       SOMEV = LSAME( HOWMNY, 'S' ) >*/
    somev = lsame_(howmny, "S", (ftnlen)1, (ftnlen)1);

/*     Set M to the number of columns required to store the selected */
/*     eigenvectors. */

/*<       IF( SOMEV ) THEN >*/
    if (somev) {
/*<          M = 0 >*/
        *m = 0;
/*<          DO 10 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<    >*/
            if (select[j]) {
                ++(*m);
            }
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<       ELSE >*/
    } else {
/*<          M = N >*/
        *m = *n;
/*<       END IF >*/
    }

/*<       INFO = 0 >*/
    *info = 0;
/*<       IF( .NOT.RIGHTV .AND. .NOT.LEFTV ) THEN >*/
    if (! rightv && ! leftv) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( .NOT.ALLV .AND. .NOT.OVER .AND. .NOT.SOMEV ) THEN >*/
    } else if (! allv && ! over && ! somev) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( LDT.LT.MAX( 1, N ) ) THEN >*/
    } else if (*ldt < max(1,*n)) {
/*<          INFO = -6 >*/
        *info = -6;
/*<       ELSE IF( LDVL.LT.1 .OR. ( LEFTV .AND. LDVL.LT.N ) ) THEN >*/
    } else if (*ldvl < 1 || (leftv && *ldvl < *n)) {
/*<          INFO = -8 >*/
        *info = -8;
/*<       ELSE IF( LDVR.LT.1 .OR. ( RIGHTV .AND. LDVR.LT.N ) ) THEN >*/
    } else if (*ldvr < 1 || (rightv && *ldvr < *n)) {
/*<          INFO = -10 >*/
        *info = -10;
/*<       ELSE IF( MM.LT.M ) THEN >*/
    } else if (*mm < *m) {
/*<          INFO = -11 >*/
        *info = -11;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZTREVC', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZTREVC", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible. */

/*<    >*/
    if (*n == 0) {
        return 0;
    }

/*     Set the constants to control overflow. */

/*<       UNFL = DLAMCH( 'Safe minimum' ) >*/
    unfl = dlamch_("Safe minimum", (ftnlen)12);
/*<       OVFL = ONE / UNFL >*/
    ovfl = 1. / unfl;
/*<       CALL DLABAD( UNFL, OVFL ) >*/
    dlabad_(&unfl, &ovfl);
/*<       ULP = DLAMCH( 'Precision' ) >*/
    ulp = dlamch_("Precision", (ftnlen)9);
/*<       SMLNUM = UNFL*( N / ULP ) >*/
    smlnum = unfl * (*n / ulp);

/*     Store the diagonal elements of T in working array WORK. */

/*<       DO 20 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          WORK( I+N ) = T( I, I ) >*/
        i__2 = i__ + *n;
        i__3 = i__ + i__ * t_dim1;
        work[i__2].r = t[i__3].r, work[i__2].i = t[i__3].i;
/*<    20 CONTINUE >*/
/* L20: */
    }

/*     Compute 1-norm of each column of strictly upper triangular */
/*     part of T to control overflow in triangular solver. */

/*<       RWORK( 1 ) = ZERO >*/
    rwork[1] = 0.;
/*<       DO 30 J = 2, N >*/
    i__1 = *n;
    for (j = 2; j <= i__1; ++j) {
/*<          RWORK( J ) = DZASUM( J-1, T( 1, J ), 1 ) >*/
        i__2 = j - 1;
        rwork[j] = dzasum_(&i__2, &t[j * t_dim1 + 1], &c__1);
/*<    30 CONTINUE >*/
/* L30: */
    }

/*<       IF( RIGHTV ) THEN >*/
    if (rightv) {

/*        Compute right eigenvectors. */

/*<          IS = M >*/
        is = *m;
/*<          DO 80 KI = N, 1, -1 >*/
        for (ki = *n; ki >= 1; --ki) {

/*<             IF( SOMEV ) THEN >*/
            if (somev) {
/*<    >*/
                if (! select[ki]) {
                    goto L80;
                }
/*<             END IF >*/
            }
/*<             SMIN = MAX( ULP*( CABS1( T( KI, KI ) ) ), SMLNUM ) >*/
/* Computing MAX */
            i__1 = ki + ki * t_dim1;
            d__3 = ulp * ((d__1 = t[i__1].r, abs(d__1)) + (d__2 = d_imag(&t[
                    ki + ki * t_dim1]), abs(d__2)));
            smin = max(d__3,smlnum);

/*<             WORK( 1 ) = CMONE >*/
            work[1].r = 1., work[1].i = 0.;

/*           Form right-hand side. */

/*<             DO 40 K = 1, KI - 1 >*/
            i__1 = ki - 1;
            for (k = 1; k <= i__1; ++k) {
/*<                WORK( K ) = -T( K, KI ) >*/
                i__2 = k;
                i__3 = k + ki * t_dim1;
                z__1.r = -t[i__3].r, z__1.i = -t[i__3].i;
                work[i__2].r = z__1.r, work[i__2].i = z__1.i;
/*<    40       CONTINUE >*/
/* L40: */
            }

/*           Solve the triangular system: */
/*              (T(1:KI-1,1:KI-1) - T(KI,KI))*X = SCALE*WORK. */

/*<             DO 50 K = 1, KI - 1 >*/
            i__1 = ki - 1;
            for (k = 1; k <= i__1; ++k) {
/*<                T( K, K ) = T( K, K ) - T( KI, KI ) >*/
                i__2 = k + k * t_dim1;
                i__3 = k + k * t_dim1;
                i__4 = ki + ki * t_dim1;
                z__1.r = t[i__3].r - t[i__4].r, z__1.i = t[i__3].i - t[i__4]
                        .i;
                t[i__2].r = z__1.r, t[i__2].i = z__1.i;
/*<    >*/
                i__2 = k + k * t_dim1;
                if ((d__1 = t[i__2].r, abs(d__1)) + (d__2 = d_imag(&t[k + k *
                        t_dim1]), abs(d__2)) < smin) {
                    i__3 = k + k * t_dim1;
                    t[i__3].r = smin, t[i__3].i = 0.;
                }
/*<    50       CONTINUE >*/
/* L50: */
            }

/*<             IF( KI.GT.1 ) THEN >*/
            if (ki > 1) {
/*<    >*/
                i__1 = ki - 1;
                zlatrs_("Upper", "No transpose", "Non-unit", "Y", &i__1, &t[
                        t_offset], ldt, &work[1], &scale, &rwork[1], info, (
                        ftnlen)5, (ftnlen)12, (ftnlen)8, (ftnlen)1);
/*<                WORK( KI ) = SCALE >*/
                i__1 = ki;
                work[i__1].r = scale, work[i__1].i = 0.;
/*<             END IF >*/
            }

/*           Copy the vector x or Q*x to VR and normalize. */

/*<             IF( .NOT.OVER ) THEN >*/
            if (! over) {
/*<                CALL ZCOPY( KI, WORK( 1 ), 1, VR( 1, IS ), 1 ) >*/
                zcopy_(&ki, &work[1], &c__1, &vr[is * vr_dim1 + 1], &c__1);

/*<                II = IZAMAX( KI, VR( 1, IS ), 1 ) >*/
                ii = izamax_(&ki, &vr[is * vr_dim1 + 1], &c__1);
/*<                REMAX = ONE / CABS1( VR( II, IS ) ) >*/
                i__1 = ii + is * vr_dim1;
                remax = 1. / ((d__1 = vr[i__1].r, abs(d__1)) + (d__2 = d_imag(
                        &vr[ii + is * vr_dim1]), abs(d__2)));
/*<                CALL ZDSCAL( KI, REMAX, VR( 1, IS ), 1 ) >*/
                zdscal_(&ki, &remax, &vr[is * vr_dim1 + 1], &c__1);

/*<                DO 60 K = KI + 1, N >*/
                i__1 = *n;
                for (k = ki + 1; k <= i__1; ++k) {
/*<                   VR( K, IS ) = CMZERO >*/
                    i__2 = k + is * vr_dim1;
                    vr[i__2].r = 0., vr[i__2].i = 0.;
/*<    60          CONTINUE >*/
/* L60: */
                }
/*<             ELSE >*/
            } else {
/*<    >*/
                if (ki > 1) {
                    i__1 = ki - 1;
                    z__1.r = scale, z__1.i = 0.;
                    zgemv_("N", n, &i__1, &c_b2, &vr[vr_offset], ldvr, &work[
                            1], &c__1, &z__1, &vr[ki * vr_dim1 + 1], &c__1, (
                            ftnlen)1);
                }

/*<                II = IZAMAX( N, VR( 1, KI ), 1 ) >*/
                ii = izamax_(n, &vr[ki * vr_dim1 + 1], &c__1);
/*<                REMAX = ONE / CABS1( VR( II, KI ) ) >*/
                i__1 = ii + ki * vr_dim1;
                remax = 1. / ((d__1 = vr[i__1].r, abs(d__1)) + (d__2 = d_imag(
                        &vr[ii + ki * vr_dim1]), abs(d__2)));
/*<                CALL ZDSCAL( N, REMAX, VR( 1, KI ), 1 ) >*/
                zdscal_(n, &remax, &vr[ki * vr_dim1 + 1], &c__1);
/*<             END IF >*/
            }

/*           Set back the original diagonal elements of T. */

/*<             DO 70 K = 1, KI - 1 >*/
            i__1 = ki - 1;
            for (k = 1; k <= i__1; ++k) {
/*<                T( K, K ) = WORK( K+N ) >*/
                i__2 = k + k * t_dim1;
                i__3 = k + *n;
                t[i__2].r = work[i__3].r, t[i__2].i = work[i__3].i;
/*<    70       CONTINUE >*/
/* L70: */
            }

/*<             IS = IS - 1 >*/
            --is;
/*<    80    CONTINUE >*/
L80:
            ;
        }
/*<       END IF >*/
    }

/*<       IF( LEFTV ) THEN >*/
    if (leftv) {

/*        Compute left eigenvectors. */

/*<          IS = 1 >*/
        is = 1;
/*<          DO 130 KI = 1, N >*/
        i__1 = *n;
        for (ki = 1; ki <= i__1; ++ki) {

/*<             IF( SOMEV ) THEN >*/
            if (somev) {
/*<    >*/
                if (! select[ki]) {
                    goto L130;
                }
/*<             END IF >*/
            }
/*<             SMIN = MAX( ULP*( CABS1( T( KI, KI ) ) ), SMLNUM ) >*/
/* Computing MAX */
            i__2 = ki + ki * t_dim1;
            d__3 = ulp * ((d__1 = t[i__2].r, abs(d__1)) + (d__2 = d_imag(&t[
                    ki + ki * t_dim1]), abs(d__2)));
            smin = max(d__3,smlnum);

/*<             WORK( N ) = CMONE >*/
            i__2 = *n;
            work[i__2].r = 1., work[i__2].i = 0.;

/*           Form right-hand side. */

/*<             DO 90 K = KI + 1, N >*/
            i__2 = *n;
            for (k = ki + 1; k <= i__2; ++k) {
/*<                WORK( K ) = -DCONJG( T( KI, K ) ) >*/
                i__3 = k;
                d_cnjg(&z__2, &t[ki + k * t_dim1]);
                z__1.r = -z__2.r, z__1.i = -z__2.i;
                work[i__3].r = z__1.r, work[i__3].i = z__1.i;
/*<    90       CONTINUE >*/
/* L90: */
            }

/*           Solve the triangular system: */
/*              (T(KI+1:N,KI+1:N) - T(KI,KI))'*X = SCALE*WORK. */

/*<             DO 100 K = KI + 1, N >*/
            i__2 = *n;
            for (k = ki + 1; k <= i__2; ++k) {
/*<                T( K, K ) = T( K, K ) - T( KI, KI ) >*/
                i__3 = k + k * t_dim1;
                i__4 = k + k * t_dim1;
                i__5 = ki + ki * t_dim1;
                z__1.r = t[i__4].r - t[i__5].r, z__1.i = t[i__4].i - t[i__5]
                        .i;
                t[i__3].r = z__1.r, t[i__3].i = z__1.i;
/*<    >*/
                i__3 = k + k * t_dim1;
                if ((d__1 = t[i__3].r, abs(d__1)) + (d__2 = d_imag(&t[k + k *
                        t_dim1]), abs(d__2)) < smin) {
                    i__4 = k + k * t_dim1;
                    t[i__4].r = smin, t[i__4].i = 0.;
                }
/*<   100       CONTINUE >*/
/* L100: */
            }

/*<             IF( KI.LT.N ) THEN >*/
            if (ki < *n) {
/*<    >*/
                i__2 = *n - ki;
                zlatrs_("Upper", "Conjugate transpose", "Non-unit", "Y", &
                        i__2, &t[ki + 1 + (ki + 1) * t_dim1], ldt, &work[ki +
                        1], &scale, &rwork[1], info, (ftnlen)5, (ftnlen)19, (
                        ftnlen)8, (ftnlen)1);
/*<                WORK( KI ) = SCALE >*/
                i__2 = ki;
                work[i__2].r = scale, work[i__2].i = 0.;
/*<             END IF >*/
            }

/*           Copy the vector x or Q*x to VL and normalize. */

/*<             IF( .NOT.OVER ) THEN >*/
            if (! over) {
/*<                CALL ZCOPY( N-KI+1, WORK( KI ), 1, VL( KI, IS ), 1 ) >*/
                i__2 = *n - ki + 1;
                zcopy_(&i__2, &work[ki], &c__1, &vl[ki + is * vl_dim1], &c__1)
                        ;

/*<                II = IZAMAX( N-KI+1, VL( KI, IS ), 1 ) + KI - 1 >*/
                i__2 = *n - ki + 1;
                ii = izamax_(&i__2, &vl[ki + is * vl_dim1], &c__1) + ki - 1;
/*<                REMAX = ONE / CABS1( VL( II, IS ) ) >*/
                i__2 = ii + is * vl_dim1;
                remax = 1. / ((d__1 = vl[i__2].r, abs(d__1)) + (d__2 = d_imag(
                        &vl[ii + is * vl_dim1]), abs(d__2)));
/*<                CALL ZDSCAL( N-KI+1, REMAX, VL( KI, IS ), 1 ) >*/
                i__2 = *n - ki + 1;
                zdscal_(&i__2, &remax, &vl[ki + is * vl_dim1], &c__1);

/*<                DO 110 K = 1, KI - 1 >*/
                i__2 = ki - 1;
                for (k = 1; k <= i__2; ++k) {
/*<                   VL( K, IS ) = CMZERO >*/
                    i__3 = k + is * vl_dim1;
                    vl[i__3].r = 0., vl[i__3].i = 0.;
/*<   110          CONTINUE >*/
/* L110: */
                }
/*<             ELSE >*/
            } else {
/*<    >*/
                if (ki < *n) {
                    i__2 = *n - ki;
                    z__1.r = scale, z__1.i = 0.;
                    zgemv_("N", n, &i__2, &c_b2, &vl[(ki + 1) * vl_dim1 + 1],
                            ldvl, &work[ki + 1], &c__1, &z__1, &vl[ki *
                            vl_dim1 + 1], &c__1, (ftnlen)1);
                }

/*<                II = IZAMAX( N, VL( 1, KI ), 1 ) >*/
                ii = izamax_(n, &vl[ki * vl_dim1 + 1], &c__1);
/*<                REMAX = ONE / CABS1( VL( II, KI ) ) >*/
                i__2 = ii + ki * vl_dim1;
                remax = 1. / ((d__1 = vl[i__2].r, abs(d__1)) + (d__2 = d_imag(
                        &vl[ii + ki * vl_dim1]), abs(d__2)));
/*<                CALL ZDSCAL( N, REMAX, VL( 1, KI ), 1 ) >*/
                zdscal_(n, &remax, &vl[ki * vl_dim1 + 1], &c__1);
/*<             END IF >*/
            }

/*           Set back the original diagonal elements of T. */

/*<             DO 120 K = KI + 1, N >*/
            i__2 = *n;
            for (k = ki + 1; k <= i__2; ++k) {
/*<                T( K, K ) = WORK( K+N ) >*/
                i__3 = k + k * t_dim1;
                i__4 = k + *n;
                t[i__3].r = work[i__4].r, t[i__3].i = work[i__4].i;
/*<   120       CONTINUE >*/
/* L120: */
            }

/*<             IS = IS + 1 >*/
            ++is;
/*<   130    CONTINUE >*/
L130:
            ;
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of ZTREVC */

/*<       END >*/
} /* ztrevc_ */

#ifdef __cplusplus
        }
#endif
