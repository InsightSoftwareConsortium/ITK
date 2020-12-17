/* lapack/complex16/zlartg.f -- translated by f2c (version 20090411).
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

/*<       SUBROUTINE ZLARTG( F, G, CS, SN, R ) >*/
/* Subroutine */ int zlartg_(doublecomplex *f, doublecomplex *g, doublereal *
        cs, doublecomplex *sn, doublecomplex *r__)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2, d__3, d__4, d__5, d__6, d__7, d__8, d__9, d__10;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    double log(doublereal), pow_di(doublereal *, integer *), d_imag(
            doublecomplex *), sqrt(doublereal);
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    doublereal d__;
    integer i__;
    doublereal f2, g2;
    doublecomplex ff;
    doublereal di, dr;
    doublecomplex fs, gs;
    doublereal f2s, g2s, eps, scale;
    integer count;
    doublereal safmn2;
    extern doublereal dlapy2_(doublereal *, doublereal *);
    doublereal safmx2;
    extern doublereal dlamch_(char *, ftnlen);
    doublereal safmin;


/*  -- LAPACK auxiliary routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       DOUBLE PRECISION   CS >*/
/*<       COMPLEX*16         F, G, R, SN >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLARTG generates a plane rotation so that */

/*     [  CS  SN  ]     [ F ]     [ R ] */
/*     [  __      ]  .  [   ]  =  [   ]   where CS**2 + |SN|**2 = 1. */
/*     [ -SN  CS  ]     [ G ]     [ 0 ] */

/*  This is a faster version of the BLAS1 routine ZROTG, except for */
/*  the following differences: */
/*     F and G are unchanged on return. */
/*     If G=0, then CS=1 and SN=0. */
/*     If F=0, then CS=0 and SN is chosen so that R is real. */

/*  Arguments */
/*  ========= */

/*  F       (input) COMPLEX*16 */
/*          The first component of vector to be rotated. */

/*  G       (input) COMPLEX*16 */
/*          The second component of vector to be rotated. */

/*  CS      (output) DOUBLE PRECISION */
/*          The cosine of the rotation. */

/*  SN      (output) COMPLEX*16 */
/*          The sine of the rotation. */

/*  R       (output) COMPLEX*16 */
/*          The nonzero component of the rotated vector. */

/*  Further Details */
/*  ======= ======= */

/*  3-5-96 - Modified with a new algorithm by W. Kahan and J. Demmel */

/*  This version has a few statements commented out for thread safety */
/*  (machine parameters are computed on each entry). 10 feb 03, SJH. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   TWO, ONE, ZERO >*/
/*<       PARAMETER          ( TWO = 2.0D+0, ONE = 1.0D+0, ZERO = 0.0D+0 ) >*/
/*<       COMPLEX*16         CZERO >*/
/*<       PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ) ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*     LOGICAL            FIRST */
/*<       INTEGER            COUNT, I >*/
/*<    >*/
/*<       COMPLEX*16         FF, FS, GS >*/
/*     .. */
/*     .. External Functions .. */
/*<       DOUBLE PRECISION   DLAMCH, DLAPY2 >*/
/*<       EXTERNAL           DLAMCH, DLAPY2 >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<    >*/
/*     .. */
/*     .. Statement Functions .. */
/*<       DOUBLE PRECISION   ABS1, ABSSQ >*/
/*     .. */
/*     .. Save statement .. */
/*     SAVE               FIRST, SAFMX2, SAFMIN, SAFMN2 */
/*     .. */
/*     .. Data statements .. */
/*     DATA               FIRST / .TRUE. / */
/*     .. */
/*     .. Statement Function definitions .. */
/*<       ABS1( FF ) = MAX( ABS( DBLE( FF ) ), ABS( DIMAG( FF ) ) ) >*/
/*<       ABSSQ( FF ) = DBLE( FF )**2 + DIMAG( FF )**2 >*/
/*     .. */
/*     .. Executable Statements .. */

/*     IF( FIRST ) THEN */
/*<          SAFMIN = DLAMCH( 'S' ) >*/
    safmin = dlamch_("S", (ftnlen)1);
/*<          EPS = DLAMCH( 'E' ) >*/
    eps = dlamch_("E", (ftnlen)1);
/*<    >*/
    d__1 = dlamch_("B", (ftnlen)1);
    i__1 = (integer) (log(safmin / eps) / log(dlamch_("B", (ftnlen)1)) / 2.);
    safmn2 = pow_di(&d__1, &i__1);
/*<          SAFMX2 = ONE / SAFMN2 >*/
    safmx2 = 1. / safmn2;
/*        FIRST = .FALSE. */
/*     END IF */
/*<       SCALE = MAX( ABS1( F ), ABS1( G ) ) >*/
/* Computing MAX */
/* Computing MAX */
    d__7 = (d__1 = f->r, abs(d__1)), d__8 = (d__2 = d_imag(f), abs(d__2));
/* Computing MAX */
    d__9 = (d__3 = g->r, abs(d__3)), d__10 = (d__4 = d_imag(g), abs(d__4));
    d__5 = max(d__7,d__8), d__6 = max(d__9,d__10);
    scale = max(d__5,d__6);
/*<       FS = F >*/
    fs.r = f->r, fs.i = f->i;
/*<       GS = G >*/
    gs.r = g->r, gs.i = g->i;
/*<       COUNT = 0 >*/
    count = 0;
/*<       IF( SCALE.GE.SAFMX2 ) THEN >*/
    if (scale >= safmx2) {
/*<    10    CONTINUE >*/
L10:
/*<          COUNT = COUNT + 1 >*/
        ++count;
/*<          FS = FS*SAFMN2 >*/
        z__1.r = safmn2 * fs.r, z__1.i = safmn2 * fs.i;
        fs.r = z__1.r, fs.i = z__1.i;
/*<          GS = GS*SAFMN2 >*/
        z__1.r = safmn2 * gs.r, z__1.i = safmn2 * gs.i;
        gs.r = z__1.r, gs.i = z__1.i;
/*<          SCALE = SCALE*SAFMN2 >*/
        scale *= safmn2;
/*<    >*/
        if (scale >= safmx2) {
            goto L10;
        }
/*<       ELSE IF( SCALE.LE.SAFMN2 ) THEN >*/
    } else if (scale <= safmn2) {
/*<          IF( G.EQ.CZERO ) THEN >*/
        if (g->r == 0. && g->i == 0.) {
/*<             CS = ONE >*/
            *cs = 1.;
/*<             SN = CZERO >*/
            sn->r = 0., sn->i = 0.;
/*<             R = F >*/
            r__->r = f->r, r__->i = f->i;
/*<             RETURN >*/
            return 0;
/*<          END IF >*/
        }
/*<    20    CONTINUE >*/
L20:
/*<          COUNT = COUNT - 1 >*/
        --count;
/*<          FS = FS*SAFMX2 >*/
        z__1.r = safmx2 * fs.r, z__1.i = safmx2 * fs.i;
        fs.r = z__1.r, fs.i = z__1.i;
/*<          GS = GS*SAFMX2 >*/
        z__1.r = safmx2 * gs.r, z__1.i = safmx2 * gs.i;
        gs.r = z__1.r, gs.i = z__1.i;
/*<          SCALE = SCALE*SAFMX2 >*/
        scale *= safmx2;
/*<    >*/
        if (scale <= safmn2) {
            goto L20;
        }
/*<       END IF >*/
    }
/*<       F2 = ABSSQ( FS ) >*/
/* Computing 2nd power */
    d__1 = fs.r;
/* Computing 2nd power */
    d__2 = d_imag(&fs);
    f2 = d__1 * d__1 + d__2 * d__2;
/*<       G2 = ABSSQ( GS ) >*/
/* Computing 2nd power */
    d__1 = gs.r;
/* Computing 2nd power */
    d__2 = d_imag(&gs);
    g2 = d__1 * d__1 + d__2 * d__2;
/*<       IF( F2.LE.MAX( G2, ONE )*SAFMIN ) THEN >*/
    if (f2 <= max(g2,1.) * safmin) {

/*        This is a rare case: F is very small. */

/*<          IF( F.EQ.CZERO ) THEN >*/
        if (f->r == 0. && f->i == 0.) {
/*<             CS = ZERO >*/
            *cs = 0.;
/*<             R = DLAPY2( DBLE( G ), DIMAG( G ) ) >*/
            d__2 = g->r;
            d__3 = d_imag(g);
            d__1 = dlapy2_(&d__2, &d__3);
            r__->r = d__1, r__->i = 0.;
/*           Do complex/real division explicitly with two real divisions */
/*<             D = DLAPY2( DBLE( GS ), DIMAG( GS ) ) >*/
            d__1 = gs.r;
            d__2 = d_imag(&gs);
            d__ = dlapy2_(&d__1, &d__2);
/*<             SN = DCMPLX( DBLE( GS ) / D, -DIMAG( GS ) / D ) >*/
            d__1 = gs.r / d__;
            d__2 = -d_imag(&gs) / d__;
            z__1.r = d__1, z__1.i = d__2;
            sn->r = z__1.r, sn->i = z__1.i;
/*<             RETURN >*/
            return 0;
/*<          END IF >*/
        }
/*<          F2S = DLAPY2( DBLE( FS ), DIMAG( FS ) ) >*/
        d__1 = fs.r;
        d__2 = d_imag(&fs);
        f2s = dlapy2_(&d__1, &d__2);
/*        G2 and G2S are accurate */
/*        G2 is at least SAFMIN, and G2S is at least SAFMN2 */
/*<          G2S = SQRT( G2 ) >*/
        g2s = sqrt(g2);
/*        Error in CS from underflow in F2S is at most */
/*        UNFL / SAFMN2 .lt. sqrt(UNFL*EPS) .lt. EPS */
/*        If MAX(G2,ONE)=G2, then F2 .lt. G2*SAFMIN, */
/*        and so CS .lt. sqrt(SAFMIN) */
/*        If MAX(G2,ONE)=ONE, then F2 .lt. SAFMIN */
/*        and so CS .lt. sqrt(SAFMIN)/SAFMN2 = sqrt(EPS) */
/*        Therefore, CS = F2S/G2S / sqrt( 1 + (F2S/G2S)**2 ) = F2S/G2S */
/*<          CS = F2S / G2S >*/
        *cs = f2s / g2s;
/*        Make sure abs(FF) = 1 */
/*        Do complex/real division explicitly with 2 real divisions */
/*<          IF( ABS1( F ).GT.ONE ) THEN >*/
/* Computing MAX */
        d__3 = (d__1 = f->r, abs(d__1)), d__4 = (d__2 = d_imag(f), abs(d__2));
        if (max(d__3,d__4) > 1.) {
/*<             D = DLAPY2( DBLE( F ), DIMAG( F ) ) >*/
            d__1 = f->r;
            d__2 = d_imag(f);
            d__ = dlapy2_(&d__1, &d__2);
/*<             FF = DCMPLX( DBLE( F ) / D, DIMAG( F ) / D ) >*/
            d__1 = f->r / d__;
            d__2 = d_imag(f) / d__;
            z__1.r = d__1, z__1.i = d__2;
            ff.r = z__1.r, ff.i = z__1.i;
/*<          ELSE >*/
        } else {
/*<             DR = SAFMX2*DBLE( F ) >*/
            dr = safmx2 * f->r;
/*<             DI = SAFMX2*DIMAG( F ) >*/
            di = safmx2 * d_imag(f);
/*<             D = DLAPY2( DR, DI ) >*/
            d__ = dlapy2_(&dr, &di);
/*<             FF = DCMPLX( DR / D, DI / D ) >*/
            d__1 = dr / d__;
            d__2 = di / d__;
            z__1.r = d__1, z__1.i = d__2;
            ff.r = z__1.r, ff.i = z__1.i;
/*<          END IF >*/
        }
/*<          SN = FF*DCMPLX( DBLE( GS ) / G2S, -DIMAG( GS ) / G2S ) >*/
        d__1 = gs.r / g2s;
        d__2 = -d_imag(&gs) / g2s;
        z__2.r = d__1, z__2.i = d__2;
        z__1.r = ff.r * z__2.r - ff.i * z__2.i, z__1.i = ff.r * z__2.i + ff.i
                * z__2.r;
        sn->r = z__1.r, sn->i = z__1.i;
/*<          R = CS*F + SN*G >*/
        z__2.r = *cs * f->r, z__2.i = *cs * f->i;
        z__3.r = sn->r * g->r - sn->i * g->i, z__3.i = sn->r * g->i + sn->i *
                g->r;
        z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
        r__->r = z__1.r, r__->i = z__1.i;
/*<       ELSE >*/
    } else {

/*        This is the most common case. */
/*        Neither F2 nor F2/G2 are less than SAFMIN */
/*        F2S cannot overflow, and it is accurate */

/*<          F2S = SQRT( ONE+G2 / F2 ) >*/
        f2s = sqrt(g2 / f2 + 1.);
/*        Do the F2S(real)*FS(complex) multiply with two real multiplies */
/*<          R = DCMPLX( F2S*DBLE( FS ), F2S*DIMAG( FS ) ) >*/
        d__1 = f2s * fs.r;
        d__2 = f2s * d_imag(&fs);
        z__1.r = d__1, z__1.i = d__2;
        r__->r = z__1.r, r__->i = z__1.i;
/*<          CS = ONE / F2S >*/
        *cs = 1. / f2s;
/*<          D = F2 + G2 >*/
        d__ = f2 + g2;
/*        Do complex/real division explicitly with two real divisions */
/*<          SN = DCMPLX( DBLE( R ) / D, DIMAG( R ) / D ) >*/
        d__1 = r__->r / d__;
        d__2 = d_imag(r__) / d__;
        z__1.r = d__1, z__1.i = d__2;
        sn->r = z__1.r, sn->i = z__1.i;
/*<          SN = SN*DCONJG( GS ) >*/
        d_cnjg(&z__2, &gs);
        z__1.r = sn->r * z__2.r - sn->i * z__2.i, z__1.i = sn->r * z__2.i +
                sn->i * z__2.r;
        sn->r = z__1.r, sn->i = z__1.i;
/*<          IF( COUNT.NE.0 ) THEN >*/
        if (count != 0) {
/*<             IF( COUNT.GT.0 ) THEN >*/
            if (count > 0) {
/*<                DO 30 I = 1, COUNT >*/
                i__1 = count;
                for (i__ = 1; i__ <= i__1; ++i__) {
/*<                   R = R*SAFMX2 >*/
                    z__1.r = safmx2 * r__->r, z__1.i = safmx2 * r__->i;
                    r__->r = z__1.r, r__->i = z__1.i;
/*<    30          CONTINUE >*/
/* L30: */
                }
/*<             ELSE >*/
            } else {
/*<                DO 40 I = 1, -COUNT >*/
                i__1 = -count;
                for (i__ = 1; i__ <= i__1; ++i__) {
/*<                   R = R*SAFMN2 >*/
                    z__1.r = safmn2 * r__->r, z__1.i = safmn2 * r__->i;
                    r__->r = z__1.r, r__->i = z__1.i;
/*<    40          CONTINUE >*/
/* L40: */
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }
/*<       RETURN >*/
    return 0;

/*     End of ZLARTG */

/*<       END >*/
} /* zlartg_ */

#ifdef __cplusplus
        }
#endif
