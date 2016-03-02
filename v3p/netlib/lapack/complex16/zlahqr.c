/* lapack/complex16/zlahqr.f -- translated by f2c (version 20050501).
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

static integer c__2 = 2;
static integer c__1 = 1;

/*<    >*/
/* Subroutine */ int zlahqr_(logical *wantt, logical *wantz, integer *n,
        integer *ilo, integer *ihi, doublecomplex *h__, integer *ldh,
        doublecomplex *w, integer *iloz, integer *ihiz, doublecomplex *z__,
        integer *ldz, integer *info)
{
    /* System generated locals */
    integer h_dim1, h_offset, z_dim1, z_offset, i__1, i__2, i__3, i__4, i__5;
    doublereal d__1, d__2, d__3, d__4, d__5, d__6;
    doublecomplex z__1, z__2, z__3, z__4;

    /* Builtin functions */
    double d_imag(doublecomplex *);
    void z_sqrt(doublecomplex *, doublecomplex *), d_cnjg(doublecomplex *,
            doublecomplex *);
    double z_abs(doublecomplex *);

    /* Local variables */
    integer i__, j, k, l, m;
    doublereal s;
    doublecomplex t, u, v[2], x, y;
    integer i1=0, i2=0;
    doublecomplex t1;
    doublereal t2;
    doublecomplex v2;
    doublereal h10;
    doublecomplex h11;
    doublereal h21;
    doublecomplex h22;
    integer nh, nz;
    doublecomplex h11s;
    integer itn, its;
    doublereal ulp;
    doublecomplex sum;
    doublereal tst1;
    doublecomplex temp;
    extern /* Subroutine */ int zscal_(integer *, doublecomplex *,
            doublecomplex *, integer *);
    doublereal rtemp, rwork[1];
    extern /* Subroutine */ int zcopy_(integer *, doublecomplex *, integer *,
            doublecomplex *, integer *);
    extern doublereal dlamch_(char *, ftnlen);
    extern /* Subroutine */ int zlarfg_(integer *, doublecomplex *,
            doublecomplex *, integer *, doublecomplex *);
    extern /* Double Complex */ VOID zladiv_(doublecomplex *, doublecomplex *,
             doublecomplex *);
    extern doublereal zlanhs_(char *, integer *, doublecomplex *, integer *,
            doublereal *, ftnlen);
    doublereal smlnum;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       LOGICAL            WANTT, WANTZ >*/
/*<       INTEGER            IHI, IHIZ, ILO, ILOZ, INFO, LDH, LDZ, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         H( LDH, * ), W( * ), Z( LDZ, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLAHQR is an auxiliary routine called by ZHSEQR to update the */
/*  eigenvalues and Schur decomposition already computed by ZHSEQR, by */
/*  dealing with the Hessenberg submatrix in rows and columns ILO to IHI. */

/*  Arguments */
/*  ========= */

/*  WANTT   (input) LOGICAL */
/*          = .TRUE. : the full Schur form T is required; */
/*          = .FALSE.: only eigenvalues are required. */

/*  WANTZ   (input) LOGICAL */
/*          = .TRUE. : the matrix of Schur vectors Z is required; */
/*          = .FALSE.: Schur vectors are not required. */

/*  N       (input) INTEGER */
/*          The order of the matrix H.  N >= 0. */

/*  ILO     (input) INTEGER */
/*  IHI     (input) INTEGER */
/*          It is assumed that H is already upper triangular in rows and */
/*          columns IHI+1:N, and that H(ILO,ILO-1) = 0 (unless ILO = 1). */
/*          ZLAHQR works primarily with the Hessenberg submatrix in rows */
/*          and columns ILO to IHI, but applies transformations to all of */
/*          H if WANTT is .TRUE.. */
/*          1 <= ILO <= max(1,IHI); IHI <= N. */

/*  H       (input/output) COMPLEX*16 array, dimension (LDH,N) */
/*          On entry, the upper Hessenberg matrix H. */
/*          On exit, if WANTT is .TRUE., H is upper triangular in rows */
/*          and columns ILO:IHI, with any 2-by-2 diagonal blocks in */
/*          standard form. If WANTT is .FALSE., the contents of H are */
/*          unspecified on exit. */

/*  LDH     (input) INTEGER */
/*          The leading dimension of the array H. LDH >= max(1,N). */

/*  W       (output) COMPLEX*16 array, dimension (N) */
/*          The computed eigenvalues ILO to IHI are stored in the */
/*          corresponding elements of W. If WANTT is .TRUE., the */
/*          eigenvalues are stored in the same order as on the diagonal */
/*          of the Schur form returned in H, with W(i) = H(i,i). */

/*  ILOZ    (input) INTEGER */
/*  IHIZ    (input) INTEGER */
/*          Specify the rows of Z to which transformations must be */
/*          applied if WANTZ is .TRUE.. */
/*          1 <= ILOZ <= ILO; IHI <= IHIZ <= N. */

/*  Z       (input/output) COMPLEX*16 array, dimension (LDZ,N) */
/*          If WANTZ is .TRUE., on entry Z must contain the current */
/*          matrix Z of transformations accumulated by ZHSEQR, and on */
/*          exit Z has been updated; transformations are applied only to */
/*          the submatrix Z(ILOZ:IHIZ,ILO:IHI). */
/*          If WANTZ is .FALSE., Z is not referenced. */

/*  LDZ     (input) INTEGER */
/*          The leading dimension of the array Z. LDZ >= max(1,N). */

/*  INFO    (output) INTEGER */
/*          = 0: successful exit */
/*          > 0: if INFO = i, ZLAHQR failed to compute all the */
/*               eigenvalues ILO to IHI in a total of 30*(IHI-ILO+1) */
/*               iterations; elements i+1:ihi of W contain those */
/*               eigenvalues which have been successfully computed. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       COMPLEX*16         ZERO, ONE >*/
/*<    >*/
/*<       DOUBLE PRECISION   RZERO, HALF >*/
/*<       PARAMETER          ( RZERO = 0.0D+0, HALF = 0.5D+0 ) >*/
/*<       DOUBLE PRECISION   DAT1 >*/
/*<       PARAMETER          ( DAT1 = 0.75D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            I, I1, I2, ITN, ITS, J, K, L, M, NH, NZ >*/
/*<       DOUBLE PRECISION   H10, H21, RTEMP, S, SMLNUM, T2, TST1, ULP >*/
/*<    >*/
/*     .. */
/*     .. Local Arrays .. */
/*<       DOUBLE PRECISION   RWORK( 1 ) >*/
/*<       COMPLEX*16         V( 2 ) >*/
/*     .. */
/*     .. External Functions .. */
/*<       DOUBLE PRECISION   DLAMCH, ZLANHS >*/
/*<       COMPLEX*16         ZLADIV >*/
/*<       EXTERNAL           DLAMCH, ZLANHS, ZLADIV >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           ZCOPY, ZLARFG, ZSCAL >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, DBLE, DCONJG, DIMAG, MAX, MIN, SQRT >*/
/*     .. */
/*     .. Statement Functions .. */
/*<       DOUBLE PRECISION   CABS1 >*/
/*     .. */
/*     .. Statement Function definitions .. */
/*<       CABS1( CDUM ) = ABS( DBLE( CDUM ) ) + ABS( DIMAG( CDUM ) ) >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    h_dim1 = *ldh;
    h_offset = 1 + h_dim1;
    h__ -= h_offset;
    --w;
    z_dim1 = *ldz;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;

    /* Function Body */
    *info = 0;

/*     Quick return if possible */

/*<    >*/
    if (*n == 0) {
        return 0;
    }
/*<       IF( ILO.EQ.IHI ) THEN >*/
    if (*ilo == *ihi) {
/*<          W( ILO ) = H( ILO, ILO ) >*/
        i__1 = *ilo;
        i__2 = *ilo + *ilo * h_dim1;
        w[i__1].r = h__[i__2].r, w[i__1].i = h__[i__2].i;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       NH = IHI - ILO + 1 >*/
    nh = *ihi - *ilo + 1;
/*<       NZ = IHIZ - ILOZ + 1 >*/
    nz = *ihiz - *iloz + 1;

/*     Set machine-dependent constants for the stopping criterion. */
/*     If norm(H) <= sqrt(OVFL), overflow should not occur. */

/*<       ULP = DLAMCH( 'Precision' ) >*/
    ulp = dlamch_("Precision", (ftnlen)9);
/*<       SMLNUM = DLAMCH( 'Safe minimum' ) / ULP >*/
    smlnum = dlamch_("Safe minimum", (ftnlen)12) / ulp;

/*     I1 and I2 are the indices of the first row and last column of H */
/*     to which transformations must be applied. If eigenvalues only are */
/*     being computed, I1 and I2 are set inside the main loop. */

/*<       IF( WANTT ) THEN >*/
    if (*wantt) {
/*<          I1 = 1 >*/
        i1 = 1;
/*<          I2 = N >*/
        i2 = *n;
/*<       END IF >*/
    }

/*     ITN is the total number of QR iterations allowed. */

/*<       ITN = 30*NH >*/
    itn = nh * 30;

/*     The main loop begins here. I is the loop index and decreases from */
/*     IHI to ILO in steps of 1. Each iteration of the loop works */
/*     with the active submatrix in rows and columns L to I. */
/*     Eigenvalues I+1 to IHI have already converged. Either L = ILO, or */
/*     H(L,L-1) is negligible so that the matrix splits. */

/*<       I = IHI >*/
    i__ = *ihi;
/*<    10 CONTINUE >*/
L10:
/*<    >*/
    if (i__ < *ilo) {
        goto L130;
    }

/*     Perform QR iterations on rows and columns ILO to I until a */
/*     submatrix of order 1 splits off at the bottom because a */
/*     subdiagonal element has become negligible. */

/*<       L = ILO >*/
    l = *ilo;
/*<       DO 110 ITS = 0, ITN >*/
    i__1 = itn;
    for (its = 0; its <= i__1; ++its) {

/*        Look for a single small subdiagonal element. */

/*<          DO 20 K = I, L + 1, -1 >*/
        i__2 = l + 1;
        for (k = i__; k >= i__2; --k) {
/*<             TST1 = CABS1( H( K-1, K-1 ) ) + CABS1( H( K, K ) ) >*/
            i__3 = k - 1 + (k - 1) * h_dim1;
            i__4 = k + k * h_dim1;
            tst1 = (d__1 = h__[i__3].r, abs(d__1)) + (d__2 = d_imag(&h__[k -
                    1 + (k - 1) * h_dim1]), abs(d__2)) + ((d__3 = h__[i__4].r,
                     abs(d__3)) + (d__4 = d_imag(&h__[k + k * h_dim1]), abs(
                    d__4)));
/*<    >*/
            if (tst1 == 0.) {
                i__3 = i__ - l + 1;
                tst1 = zlanhs_("1", &i__3, &h__[l + l * h_dim1], ldh, rwork, (
                        ftnlen)1);
            }
/*<    >*/
            i__3 = k + (k - 1) * h_dim1;
/* Computing MAX */
            d__2 = ulp * tst1;
            if ((d__1 = h__[i__3].r, abs(d__1)) <= max(d__2,smlnum)) {
                goto L30;
            }
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<    30    CONTINUE >*/
L30:
/*<          L = K >*/
        l = k;
/*<          IF( L.GT.ILO ) THEN >*/
        if (l > *ilo) {

/*           H(L,L-1) is negligible */

/*<             H( L, L-1 ) = ZERO >*/
            i__2 = l + (l - 1) * h_dim1;
            h__[i__2].r = 0., h__[i__2].i = 0.;
/*<          END IF >*/
        }

/*        Exit from loop if a submatrix of order 1 has split off. */

/*<    >*/
        if (l >= i__) {
            goto L120;
        }

/*        Now the active submatrix is in rows and columns L to I. If */
/*        eigenvalues only are being computed, only the active submatrix */
/*        need be transformed. */

/*<          IF( .NOT.WANTT ) THEN >*/
        if (! (*wantt)) {
/*<             I1 = L >*/
            i1 = l;
/*<             I2 = I >*/
            i2 = i__;
/*<          END IF >*/
        }

/*<          IF( ITS.EQ.10 .OR. ITS.EQ.20 ) THEN >*/
        if (its == 10 || its == 20) {

/*           Exceptional shift. */

/*<             S = DAT1*ABS( DBLE( H( I, I-1 ) ) ) >*/
            i__2 = i__ + (i__ - 1) * h_dim1;
            s = (d__1 = h__[i__2].r, abs(d__1)) * .75;
/*<             T = S + H( I, I ) >*/
            i__2 = i__ + i__ * h_dim1;
            z__1.r = s + h__[i__2].r, z__1.i = h__[i__2].i;
            t.r = z__1.r, t.i = z__1.i;
/*<          ELSE >*/
        } else {

/*           Wilkinson's shift. */

/*<             T = H( I, I ) >*/
            i__2 = i__ + i__ * h_dim1;
            t.r = h__[i__2].r, t.i = h__[i__2].i;
/*<             U = H( I-1, I )*DBLE( H( I, I-1 ) ) >*/
            i__2 = i__ - 1 + i__ * h_dim1;
            i__3 = i__ + (i__ - 1) * h_dim1;
            d__1 = h__[i__3].r;
            z__1.r = d__1 * h__[i__2].r, z__1.i = d__1 * h__[i__2].i;
            u.r = z__1.r, u.i = z__1.i;
/*<             IF( U.NE.ZERO ) THEN >*/
            if (u.r != 0. || u.i != 0.) {
/*<                X = HALF*( H( I-1, I-1 )-T ) >*/
                i__2 = i__ - 1 + (i__ - 1) * h_dim1;
                z__2.r = h__[i__2].r - t.r, z__2.i = h__[i__2].i - t.i;
                z__1.r = z__2.r * .5, z__1.i = z__2.i * .5;
                x.r = z__1.r, x.i = z__1.i;
/*<                Y = SQRT( X*X+U ) >*/
                z__3.r = x.r * x.r - x.i * x.i, z__3.i = x.r * x.i + x.i *
                        x.r;
                z__2.r = z__3.r + u.r, z__2.i = z__3.i + u.i;
                z_sqrt(&z__1, &z__2);
                y.r = z__1.r, y.i = z__1.i;
/*<    >*/
                if (x.r * y.r + d_imag(&x) * d_imag(&y) < 0.) {
                    z__1.r = -y.r, z__1.i = -y.i;
                    y.r = z__1.r, y.i = z__1.i;
                }
/*<                T = T - ZLADIV( U, ( X+Y ) ) >*/
                z__3.r = x.r + y.r, z__3.i = x.i + y.i;
                zladiv_(&z__2, &u, &z__3);
                z__1.r = t.r - z__2.r, z__1.i = t.i - z__2.i;
                t.r = z__1.r, t.i = z__1.i;
/*<             END IF >*/
            }
/*<          END IF >*/
        }

/*        Look for two consecutive small subdiagonal elements. */

/*<          DO 40 M = I - 1, L + 1, -1 >*/
        i__2 = l + 1;
        for (m = i__ - 1; m >= i__2; --m) {

/*           Determine the effect of starting the single-shift QR */
/*           iteration at row M, and see if this would make H(M,M-1) */
/*           negligible. */

/*<             H11 = H( M, M ) >*/
            i__3 = m + m * h_dim1;
            h11.r = h__[i__3].r, h11.i = h__[i__3].i;
/*<             H22 = H( M+1, M+1 ) >*/
            i__3 = m + 1 + (m + 1) * h_dim1;
            h22.r = h__[i__3].r, h22.i = h__[i__3].i;
/*<             H11S = H11 - T >*/
            z__1.r = h11.r - t.r, z__1.i = h11.i - t.i;
            h11s.r = z__1.r, h11s.i = z__1.i;
/*<             H21 = H( M+1, M ) >*/
            i__3 = m + 1 + m * h_dim1;
            h21 = h__[i__3].r;
/*<             S = CABS1( H11S ) + ABS( H21 ) >*/
            s = (d__1 = h11s.r, abs(d__1)) + (d__2 = d_imag(&h11s), abs(d__2))
                     + abs(h21);
/*<             H11S = H11S / S >*/
            z__1.r = h11s.r / s, z__1.i = h11s.i / s;
            h11s.r = z__1.r, h11s.i = z__1.i;
/*<             H21 = H21 / S >*/
            h21 /= s;
/*<             V( 1 ) = H11S >*/
            v[0].r = h11s.r, v[0].i = h11s.i;
/*<             V( 2 ) = H21 >*/
            v[1].r = h21, v[1].i = 0.;
/*<             H10 = H( M, M-1 ) >*/
            i__3 = m + (m - 1) * h_dim1;
            h10 = h__[i__3].r;
/*<             TST1 = CABS1( H11S )*( CABS1( H11 )+CABS1( H22 ) ) >*/
            tst1 = ((d__1 = h11s.r, abs(d__1)) + (d__2 = d_imag(&h11s), abs(
                    d__2))) * ((d__3 = h11.r, abs(d__3)) + (d__4 = d_imag(&
                    h11), abs(d__4)) + ((d__5 = h22.r, abs(d__5)) + (d__6 =
                    d_imag(&h22), abs(d__6))));
/*<    >*/
            if ((d__1 = h10 * h21, abs(d__1)) <= ulp * tst1) {
                goto L50;
            }
/*<    40    CONTINUE >*/
/* L40: */
        }
/*<          H11 = H( L, L ) >*/
        i__2 = l + l * h_dim1;
        h11.r = h__[i__2].r, h11.i = h__[i__2].i;
/*<          H22 = H( L+1, L+1 ) >*/
        i__2 = l + 1 + (l + 1) * h_dim1;
        h22.r = h__[i__2].r, h22.i = h__[i__2].i;
/*<          H11S = H11 - T >*/
        z__1.r = h11.r - t.r, z__1.i = h11.i - t.i;
        h11s.r = z__1.r, h11s.i = z__1.i;
/*<          H21 = H( L+1, L ) >*/
        i__2 = l + 1 + l * h_dim1;
        h21 = h__[i__2].r;
/*<          S = CABS1( H11S ) + ABS( H21 ) >*/
        s = (d__1 = h11s.r, abs(d__1)) + (d__2 = d_imag(&h11s), abs(d__2)) +
                abs(h21);
/*<          H11S = H11S / S >*/
        z__1.r = h11s.r / s, z__1.i = h11s.i / s;
        h11s.r = z__1.r, h11s.i = z__1.i;
/*<          H21 = H21 / S >*/
        h21 /= s;
/*<          V( 1 ) = H11S >*/
        v[0].r = h11s.r, v[0].i = h11s.i;
/*<          V( 2 ) = H21 >*/
        v[1].r = h21, v[1].i = 0.;
/*<    50    CONTINUE >*/
L50:

/*        Single-shift QR step */

/*<          DO 100 K = M, I - 1 >*/
        i__2 = i__ - 1;
        for (k = m; k <= i__2; ++k) {

/*           The first iteration of this loop determines a reflection G */
/*           from the vector V and applies it from left and right to H, */
/*           thus creating a nonzero bulge below the subdiagonal. */

/*           Each subsequent iteration determines a reflection G to */
/*           restore the Hessenberg form in the (K-1)th column, and thus */
/*           chases the bulge one step toward the bottom of the active */
/*           submatrix. */

/*           V(2) is always real before the call to ZLARFG, and hence */
/*           after the call T2 ( = T1*V(2) ) is also real. */

/*<    >*/
            if (k > m) {
                zcopy_(&c__2, &h__[k + (k - 1) * h_dim1], &c__1, v, &c__1);
            }
/*<             CALL ZLARFG( 2, V( 1 ), V( 2 ), 1, T1 ) >*/
            zlarfg_(&c__2, v, &v[1], &c__1, &t1);
/*<             IF( K.GT.M ) THEN >*/
            if (k > m) {
/*<                H( K, K-1 ) = V( 1 ) >*/
                i__3 = k + (k - 1) * h_dim1;
                h__[i__3].r = v[0].r, h__[i__3].i = v[0].i;
/*<                H( K+1, K-1 ) = ZERO >*/
                i__3 = k + 1 + (k - 1) * h_dim1;
                h__[i__3].r = 0., h__[i__3].i = 0.;
/*<             END IF >*/
            }
/*<             V2 = V( 2 ) >*/
            v2.r = v[1].r, v2.i = v[1].i;
/*<             T2 = DBLE( T1*V2 ) >*/
            z__1.r = t1.r * v2.r - t1.i * v2.i, z__1.i = t1.r * v2.i + t1.i *
                    v2.r;
            t2 = z__1.r;

/*           Apply G from the left to transform the rows of the matrix */
/*           in columns K to I2. */

/*<             DO 60 J = K, I2 >*/
            i__3 = i2;
            for (j = k; j <= i__3; ++j) {
/*<                SUM = DCONJG( T1 )*H( K, J ) + T2*H( K+1, J ) >*/
                d_cnjg(&z__3, &t1);
                i__4 = k + j * h_dim1;
                z__2.r = z__3.r * h__[i__4].r - z__3.i * h__[i__4].i, z__2.i =
                         z__3.r * h__[i__4].i + z__3.i * h__[i__4].r;
                i__5 = k + 1 + j * h_dim1;
                z__4.r = t2 * h__[i__5].r, z__4.i = t2 * h__[i__5].i;
                z__1.r = z__2.r + z__4.r, z__1.i = z__2.i + z__4.i;
                sum.r = z__1.r, sum.i = z__1.i;
/*<                H( K, J ) = H( K, J ) - SUM >*/
                i__4 = k + j * h_dim1;
                i__5 = k + j * h_dim1;
                z__1.r = h__[i__5].r - sum.r, z__1.i = h__[i__5].i - sum.i;
                h__[i__4].r = z__1.r, h__[i__4].i = z__1.i;
/*<                H( K+1, J ) = H( K+1, J ) - SUM*V2 >*/
                i__4 = k + 1 + j * h_dim1;
                i__5 = k + 1 + j * h_dim1;
                z__2.r = sum.r * v2.r - sum.i * v2.i, z__2.i = sum.r * v2.i +
                        sum.i * v2.r;
                z__1.r = h__[i__5].r - z__2.r, z__1.i = h__[i__5].i - z__2.i;
                h__[i__4].r = z__1.r, h__[i__4].i = z__1.i;
/*<    60       CONTINUE >*/
/* L60: */
            }

/*           Apply G from the right to transform the columns of the */
/*           matrix in rows I1 to min(K+2,I). */

/*<             DO 70 J = I1, MIN( K+2, I ) >*/
/* Computing MIN */
            i__4 = k + 2;
            i__3 = min(i__4,i__);
            for (j = i1; j <= i__3; ++j) {
/*<                SUM = T1*H( J, K ) + T2*H( J, K+1 ) >*/
                i__4 = j + k * h_dim1;
                z__2.r = t1.r * h__[i__4].r - t1.i * h__[i__4].i, z__2.i =
                        t1.r * h__[i__4].i + t1.i * h__[i__4].r;
                i__5 = j + (k + 1) * h_dim1;
                z__3.r = t2 * h__[i__5].r, z__3.i = t2 * h__[i__5].i;
                z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
                sum.r = z__1.r, sum.i = z__1.i;
/*<                H( J, K ) = H( J, K ) - SUM >*/
                i__4 = j + k * h_dim1;
                i__5 = j + k * h_dim1;
                z__1.r = h__[i__5].r - sum.r, z__1.i = h__[i__5].i - sum.i;
                h__[i__4].r = z__1.r, h__[i__4].i = z__1.i;
/*<                H( J, K+1 ) = H( J, K+1 ) - SUM*DCONJG( V2 ) >*/
                i__4 = j + (k + 1) * h_dim1;
                i__5 = j + (k + 1) * h_dim1;
                d_cnjg(&z__3, &v2);
                z__2.r = sum.r * z__3.r - sum.i * z__3.i, z__2.i = sum.r *
                        z__3.i + sum.i * z__3.r;
                z__1.r = h__[i__5].r - z__2.r, z__1.i = h__[i__5].i - z__2.i;
                h__[i__4].r = z__1.r, h__[i__4].i = z__1.i;
/*<    70       CONTINUE >*/
/* L70: */
            }

/*<             IF( WANTZ ) THEN >*/
            if (*wantz) {

/*              Accumulate transformations in the matrix Z */

/*<                DO 80 J = ILOZ, IHIZ >*/
                i__3 = *ihiz;
                for (j = *iloz; j <= i__3; ++j) {
/*<                   SUM = T1*Z( J, K ) + T2*Z( J, K+1 ) >*/
                    i__4 = j + k * z_dim1;
                    z__2.r = t1.r * z__[i__4].r - t1.i * z__[i__4].i, z__2.i =
                             t1.r * z__[i__4].i + t1.i * z__[i__4].r;
                    i__5 = j + (k + 1) * z_dim1;
                    z__3.r = t2 * z__[i__5].r, z__3.i = t2 * z__[i__5].i;
                    z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
                    sum.r = z__1.r, sum.i = z__1.i;
/*<                   Z( J, K ) = Z( J, K ) - SUM >*/
                    i__4 = j + k * z_dim1;
                    i__5 = j + k * z_dim1;
                    z__1.r = z__[i__5].r - sum.r, z__1.i = z__[i__5].i -
                            sum.i;
                    z__[i__4].r = z__1.r, z__[i__4].i = z__1.i;
/*<                   Z( J, K+1 ) = Z( J, K+1 ) - SUM*DCONJG( V2 ) >*/
                    i__4 = j + (k + 1) * z_dim1;
                    i__5 = j + (k + 1) * z_dim1;
                    d_cnjg(&z__3, &v2);
                    z__2.r = sum.r * z__3.r - sum.i * z__3.i, z__2.i = sum.r *
                             z__3.i + sum.i * z__3.r;
                    z__1.r = z__[i__5].r - z__2.r, z__1.i = z__[i__5].i -
                            z__2.i;
                    z__[i__4].r = z__1.r, z__[i__4].i = z__1.i;
/*<    80          CONTINUE >*/
/* L80: */
                }
/*<             END IF >*/
            }

/*<             IF( K.EQ.M .AND. M.GT.L ) THEN >*/
            if (k == m && m > l) {

/*              If the QR step was started at row M > L because two */
/*              consecutive small subdiagonals were found, then extra */
/*              scaling must be performed to ensure that H(M,M-1) remains */
/*              real. */

/*<                TEMP = ONE - T1 >*/
                z__1.r = 1. - t1.r, z__1.i = 0. - t1.i;
                temp.r = z__1.r, temp.i = z__1.i;
/*<                TEMP = TEMP / ABS( TEMP ) >*/
                d__1 = z_abs(&temp);
                z__1.r = temp.r / d__1, z__1.i = temp.i / d__1;
                temp.r = z__1.r, temp.i = z__1.i;
/*<                H( M+1, M ) = H( M+1, M )*DCONJG( TEMP ) >*/
                i__3 = m + 1 + m * h_dim1;
                i__4 = m + 1 + m * h_dim1;
                d_cnjg(&z__2, &temp);
                z__1.r = h__[i__4].r * z__2.r - h__[i__4].i * z__2.i, z__1.i =
                         h__[i__4].r * z__2.i + h__[i__4].i * z__2.r;
                h__[i__3].r = z__1.r, h__[i__3].i = z__1.i;
/*<    >*/
                if (m + 2 <= i__) {
                    i__3 = m + 2 + (m + 1) * h_dim1;
                    i__4 = m + 2 + (m + 1) * h_dim1;
                    z__1.r = h__[i__4].r * temp.r - h__[i__4].i * temp.i,
                            z__1.i = h__[i__4].r * temp.i + h__[i__4].i *
                            temp.r;
                    h__[i__3].r = z__1.r, h__[i__3].i = z__1.i;
                }
/*<                DO 90 J = M, I >*/
                i__3 = i__;
                for (j = m; j <= i__3; ++j) {
/*<                   IF( J.NE.M+1 ) THEN >*/
                    if (j != m + 1) {
/*<    >*/
                        if (i2 > j) {
                            i__4 = i2 - j;
                            zscal_(&i__4, &temp, &h__[j + (j + 1) * h_dim1],
                                    ldh);
                        }
/*<                      CALL ZSCAL( J-I1, DCONJG( TEMP ), H( I1, J ), 1 ) >*/
                        i__4 = j - i1;
                        d_cnjg(&z__1, &temp);
                        zscal_(&i__4, &z__1, &h__[i1 + j * h_dim1], &c__1);
/*<                      IF( WANTZ ) THEN >*/
                        if (*wantz) {
/*<    >*/
                            d_cnjg(&z__1, &temp);
                            zscal_(&nz, &z__1, &z__[*iloz + j * z_dim1], &
                                    c__1);
/*<                      END IF >*/
                        }
/*<                   END IF >*/
                    }
/*<    90          CONTINUE >*/
/* L90: */
                }
/*<             END IF >*/
            }
/*<   100    CONTINUE >*/
/* L100: */
        }

/*        Ensure that H(I,I-1) is real. */

/*<          TEMP = H( I, I-1 ) >*/
        i__2 = i__ + (i__ - 1) * h_dim1;
        temp.r = h__[i__2].r, temp.i = h__[i__2].i;
/*<          IF( DIMAG( TEMP ).NE.RZERO ) THEN >*/
        if (d_imag(&temp) != 0.) {
/*<             RTEMP = ABS( TEMP ) >*/
            rtemp = z_abs(&temp);
/*<             H( I, I-1 ) = RTEMP >*/
            i__2 = i__ + (i__ - 1) * h_dim1;
            h__[i__2].r = rtemp, h__[i__2].i = 0.;
/*<             TEMP = TEMP / RTEMP >*/
            z__1.r = temp.r / rtemp, z__1.i = temp.i / rtemp;
            temp.r = z__1.r, temp.i = z__1.i;
/*<    >*/
            if (i2 > i__) {
                i__2 = i2 - i__;
                d_cnjg(&z__1, &temp);
                zscal_(&i__2, &z__1, &h__[i__ + (i__ + 1) * h_dim1], ldh);
            }
/*<             CALL ZSCAL( I-I1, TEMP, H( I1, I ), 1 ) >*/
            i__2 = i__ - i1;
            zscal_(&i__2, &temp, &h__[i1 + i__ * h_dim1], &c__1);
/*<             IF( WANTZ ) THEN >*/
            if (*wantz) {
/*<                CALL ZSCAL( NZ, TEMP, Z( ILOZ, I ), 1 ) >*/
                zscal_(&nz, &temp, &z__[*iloz + i__ * z_dim1], &c__1);
/*<             END IF >*/
            }
/*<          END IF >*/
        }

/*<   110 CONTINUE >*/
/* L110: */
    }

/*     Failure to converge in remaining number of iterations */

/*<       INFO = I >*/
    *info = i__;
/*<       RETURN >*/
    return 0;

/*<   120 CONTINUE >*/
L120:

/*     H(I,I-1) is negligible: one eigenvalue has converged. */

/*<       W( I ) = H( I, I ) >*/
    i__1 = i__;
    i__2 = i__ + i__ * h_dim1;
    w[i__1].r = h__[i__2].r, w[i__1].i = h__[i__2].i;

/*     Decrement number of remaining iterations, and return to start of */
/*     the main loop with new value of I. */

/*<       ITN = ITN - ITS >*/
    itn -= its;
/*<       I = L - 1 >*/
    i__ = l - 1;
/*<       GO TO 10 >*/
    goto L10;

/*<   130 CONTINUE >*/
L130:
/*<       RETURN >*/
    return 0;

/*     End of ZLAHQR */

/*<       END >*/
} /* zlahqr_ */

#ifdef __cplusplus
        }
#endif
