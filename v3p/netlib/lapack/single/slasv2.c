/* lapack/single/slasv2.f -- translated by f2c (version 20050501).
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

static real c_b3 = (float)2.;
static real c_b4 = (float)1.;

/*<       SUBROUTINE SLASV2( F, G, H, SSMIN, SSMAX, SNR, CSR, SNL, CSL ) >*/
/* Subroutine */ int slasv2_(real *f, real *g, real *h__, real *ssmin, real *
        ssmax, real *snr, real *csr, real *snl, real *csl)
{
    /* System generated locals */
    real r__1;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    real a, d__, l, m, r__, s, t, fa, ga, ha, ft, gt, ht, mm, tt, clt=0, crt=0,
            slt=0, srt=0;
    integer pmax;
    real temp;
    logical swap;
    real tsign;
    logical gasmal;
    extern doublereal slamch_(char *, ftnlen);


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*     .. Scalar Arguments .. */
/*<       REAL               CSL, CSR, F, G, H, SNL, SNR, SSMAX, SSMIN >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  SLASV2 computes the singular value decomposition of a 2-by-2 */
/*  triangular matrix */
/*     [  F   G  ] */
/*     [  0   H  ]. */
/*  On return, abs(SSMAX) is the larger singular value, abs(SSMIN) is the */
/*  smaller singular value, and (CSL,SNL) and (CSR,SNR) are the left and */
/*  right singular vectors for abs(SSMAX), giving the decomposition */

/*     [ CSL  SNL ] [  F   G  ] [ CSR -SNR ]  =  [ SSMAX   0   ] */
/*     [-SNL  CSL ] [  0   H  ] [ SNR  CSR ]     [  0    SSMIN ]. */

/*  Arguments */
/*  ========= */

/*  F       (input) REAL */
/*          The (1,1) element of the 2-by-2 matrix. */

/*  G       (input) REAL */
/*          The (1,2) element of the 2-by-2 matrix. */

/*  H       (input) REAL */
/*          The (2,2) element of the 2-by-2 matrix. */

/*  SSMIN   (output) REAL */
/*          abs(SSMIN) is the smaller singular value. */

/*  SSMAX   (output) REAL */
/*          abs(SSMAX) is the larger singular value. */

/*  SNL     (output) REAL */
/*  CSL     (output) REAL */
/*          The vector (CSL, SNL) is a unit left singular vector for the */
/*          singular value abs(SSMAX). */

/*  SNR     (output) REAL */
/*  CSR     (output) REAL */
/*          The vector (CSR, SNR) is a unit right singular vector for the */
/*          singular value abs(SSMAX). */

/*  Further Details */
/*  =============== */

/*  Any input parameter may be aliased with any output parameter. */

/*  Barring over/underflow and assuming a guard digit in subtraction, all */
/*  output quantities are correct to within a few units in the last */
/*  place (ulps). */

/*  In IEEE arithmetic, the code works correctly if one matrix element is */
/*  infinite. */

/*  Overflow will not occur unless the largest singular value itself */
/*  overflows or is within a few ulps of overflow. (On machines with */
/*  partial overflow, like the Cray, overflow may occur if the largest */
/*  singular value is within a factor of 2 of overflow.) */

/*  Underflow is harmless if underflow is gradual. Otherwise, results */
/*  may correspond to a matrix modified by perturbations of size near */
/*  the underflow threshold. */

/* ===================================================================== */

/*     .. Parameters .. */
/*<       REAL               ZERO >*/
/*<       PARAMETER          ( ZERO = 0.0E0 ) >*/
/*<       REAL               HALF >*/
/*<       PARAMETER          ( HALF = 0.5E0 ) >*/
/*<       REAL               ONE >*/
/*<       PARAMETER          ( ONE = 1.0E0 ) >*/
/*<       REAL               TWO >*/
/*<       PARAMETER          ( TWO = 2.0E0 ) >*/
/*<       REAL               FOUR >*/
/*<       PARAMETER          ( FOUR = 4.0E0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            GASMAL, SWAP >*/
/*<       INTEGER            PMAX >*/
/*<    >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, SIGN, SQRT >*/
/*     .. */
/*     .. External Functions .. */
/*<       REAL               SLAMCH >*/
/*<       EXTERNAL           SLAMCH >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       FT = F >*/
    ft = *f;
/*<       FA = ABS( FT ) >*/
    fa = dabs(ft);
/*<       HT = H >*/
    ht = *h__;
/*<       HA = ABS( H ) >*/
    ha = dabs(*h__);

/*     PMAX points to the maximum absolute element of matrix */
/*       PMAX = 1 if F largest in absolute values */
/*       PMAX = 2 if G largest in absolute values */
/*       PMAX = 3 if H largest in absolute values */

/*<       PMAX = 1 >*/
    pmax = 1;
/*<       SWAP = ( HA.GT.FA ) >*/
    swap = ha > fa;
/*<       IF( SWAP ) THEN >*/
    if (swap) {
/*<          PMAX = 3 >*/
        pmax = 3;
/*<          TEMP = FT >*/
        temp = ft;
/*<          FT = HT >*/
        ft = ht;
/*<          HT = TEMP >*/
        ht = temp;
/*<          TEMP = FA >*/
        temp = fa;
/*<          FA = HA >*/
        fa = ha;
/*<          HA = TEMP >*/
        ha = temp;

/*        Now FA .ge. HA */

/*<       END IF >*/
    }
/*<       GT = G >*/
    gt = *g;
/*<       GA = ABS( GT ) >*/
    ga = dabs(gt);
/*<       IF( GA.EQ.ZERO ) THEN >*/
    if (ga == (float)0.) {

/*        Diagonal matrix */

/*<          SSMIN = HA >*/
        *ssmin = ha;
/*<          SSMAX = FA >*/
        *ssmax = fa;
/*<          CLT = ONE >*/
        clt = (float)1.;
/*<          CRT = ONE >*/
        crt = (float)1.;
/*<          SLT = ZERO >*/
        slt = (float)0.;
/*<          SRT = ZERO >*/
        srt = (float)0.;
/*<       ELSE >*/
    } else {
/*<          GASMAL = .TRUE. >*/
        gasmal = TRUE_;
/*<          IF( GA.GT.FA ) THEN >*/
        if (ga > fa) {
/*<             PMAX = 2 >*/
            pmax = 2;
/*<             IF( ( FA / GA ).LT.SLAMCH( 'EPS' ) ) THEN >*/
            if (fa / ga < slamch_("EPS", (ftnlen)3)) {

/*              Case of very large GA */

/*<                GASMAL = .FALSE. >*/
                gasmal = FALSE_;
/*<                SSMAX = GA >*/
                *ssmax = ga;
/*<                IF( HA.GT.ONE ) THEN >*/
                if (ha > (float)1.) {
/*<                   SSMIN = FA / ( GA / HA ) >*/
                    *ssmin = fa / (ga / ha);
/*<                ELSE >*/
                } else {
/*<                   SSMIN = ( FA / GA )*HA >*/
                    *ssmin = fa / ga * ha;
/*<                END IF >*/
                }
/*<                CLT = ONE >*/
                clt = (float)1.;
/*<                SLT = HT / GT >*/
                slt = ht / gt;
/*<                SRT = ONE >*/
                srt = (float)1.;
/*<                CRT = FT / GT >*/
                crt = ft / gt;
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<          IF( GASMAL ) THEN >*/
        if (gasmal) {

/*           Normal case */

/*<             D = FA - HA >*/
            d__ = fa - ha;
/*<             IF( D.EQ.FA ) THEN >*/
            if (d__ == fa) {

/*              Copes with infinite F or H */

/*<                L = ONE >*/
                l = (float)1.;
/*<             ELSE >*/
            } else {
/*<                L = D / FA >*/
                l = d__ / fa;
/*<             END IF >*/
            }

/*           Note that 0 .le. L .le. 1 */

/*<             M = GT / FT >*/
            m = gt / ft;

/*           Note that abs(M) .le. 1/macheps */

/*<             T = TWO - L >*/
            t = (float)2. - l;

/*           Note that T .ge. 1 */

/*<             MM = M*M >*/
            mm = m * m;
/*<             TT = T*T >*/
            tt = t * t;
/*<             S = SQRT( TT+MM ) >*/
            s = sqrt(tt + mm);

/*           Note that 1 .le. S .le. 1 + 1/macheps */

/*<             IF( L.EQ.ZERO ) THEN >*/
            if (l == (float)0.) {
/*<                R = ABS( M ) >*/
                r__ = dabs(m);
/*<             ELSE >*/
            } else {
/*<                R = SQRT( L*L+MM ) >*/
                r__ = sqrt(l * l + mm);
/*<             END IF >*/
            }

/*           Note that 0 .le. R .le. 1 + 1/macheps */

/*<             A = HALF*( S+R ) >*/
            a = (s + r__) * (float).5;

/*           Note that 1 .le. A .le. 1 + abs(M) */

/*<             SSMIN = HA / A >*/
            *ssmin = ha / a;
/*<             SSMAX = FA*A >*/
            *ssmax = fa * a;
/*<             IF( MM.EQ.ZERO ) THEN >*/
            if (mm == (float)0.) {

/*              Note that M is very tiny */

/*<                IF( L.EQ.ZERO ) THEN >*/
                if (l == (float)0.) {
/*<                   T = SIGN( TWO, FT )*SIGN( ONE, GT ) >*/
                    t = r_sign(&c_b3, &ft) * r_sign(&c_b4, &gt);
/*<                ELSE >*/
                } else {
/*<                   T = GT / SIGN( D, FT ) + M / T >*/
                    t = gt / r_sign(&d__, &ft) + m / t;
/*<                END IF >*/
                }
/*<             ELSE >*/
            } else {
/*<                T = ( M / ( S+T )+M / ( R+L ) )*( ONE+A ) >*/
                t = (m / (s + t) + m / (r__ + l)) * (a + (float)1.);
/*<             END IF >*/
            }
/*<             L = SQRT( T*T+FOUR ) >*/
            l = sqrt(t * t + (float)4.);
/*<             CRT = TWO / L >*/
            crt = (float)2. / l;
/*<             SRT = T / L >*/
            srt = t / l;
/*<             CLT = ( CRT+SRT*M ) / A >*/
            clt = (crt + srt * m) / a;
/*<             SLT = ( HT / FT )*SRT / A >*/
            slt = ht / ft * srt / a;
/*<          END IF >*/
        }
/*<       END IF >*/
    }
/*<       IF( SWAP ) THEN >*/
    if (swap) {
/*<          CSL = SRT >*/
        *csl = srt;
/*<          SNL = CRT >*/
        *snl = crt;
/*<          CSR = SLT >*/
        *csr = slt;
/*<          SNR = CLT >*/
        *snr = clt;
/*<       ELSE >*/
    } else {
/*<          CSL = CLT >*/
        *csl = clt;
/*<          SNL = SLT >*/
        *snl = slt;
/*<          CSR = CRT >*/
        *csr = crt;
/*<          SNR = SRT >*/
        *snr = srt;
/*<       END IF >*/
    }

/*     Correct signs of SSMAX and SSMIN */

/*<    >*/
    if (pmax == 1) {
        tsign = r_sign(&c_b4, csr) * r_sign(&c_b4, csl) * r_sign(&c_b4, f);
    }
/*<    >*/
    if (pmax == 2) {
        tsign = r_sign(&c_b4, snr) * r_sign(&c_b4, csl) * r_sign(&c_b4, g);
    }
/*<    >*/
    if (pmax == 3) {
        tsign = r_sign(&c_b4, snr) * r_sign(&c_b4, snl) * r_sign(&c_b4, h__);
    }
/*<       SSMAX = SIGN( SSMAX, TSIGN ) >*/
    *ssmax = r_sign(ssmax, &tsign);
/*<       SSMIN = SIGN( SSMIN, TSIGN*SIGN( ONE, F )*SIGN( ONE, H ) ) >*/
    r__1 = tsign * r_sign(&c_b4, f) * r_sign(&c_b4, h__);
    *ssmin = r_sign(ssmin, &r__1);
/*<       RETURN >*/
    return 0;

/*     End of SLASV2 */

/*<       END >*/
} /* slasv2_ */

#ifdef __cplusplus
        }
#endif
