/* lapack/double/dlasv2.f -- translated by f2c (version 20050501).
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

static doublereal c_b3 = 2.;
static doublereal c_b4 = 1.;

/*<       SUBROUTINE DLASV2( F, G, H, SSMIN, SSMAX, SNR, CSR, SNL, CSL ) >*/
/* Subroutine */ int dlasv2_(doublereal *f, doublereal *g, doublereal *h__,
        doublereal *ssmin, doublereal *ssmax, doublereal *snr, doublereal *
        csr, doublereal *snl, doublereal *csl)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal a, d__, l, m, r__, s, t, fa, ga, ha, ft, gt, ht, mm, tt, clt=0,
            crt=0, slt=0, srt=0;
    integer pmax;
    doublereal temp;
    logical swap;
    doublereal tsign;
    extern doublereal dlamch_(char *, ftnlen);
    logical gasmal;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*     .. Scalar Arguments .. */
/*<       DOUBLE PRECISION   CSL, CSR, F, G, H, SNL, SNR, SSMAX, SSMIN >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLASV2 computes the singular value decomposition of a 2-by-2 */
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

/*  F       (input) DOUBLE PRECISION */
/*          The (1,1) element of the 2-by-2 matrix. */

/*  G       (input) DOUBLE PRECISION */
/*          The (1,2) element of the 2-by-2 matrix. */

/*  H       (input) DOUBLE PRECISION */
/*          The (2,2) element of the 2-by-2 matrix. */

/*  SSMIN   (output) DOUBLE PRECISION */
/*          abs(SSMIN) is the smaller singular value. */

/*  SSMAX   (output) DOUBLE PRECISION */
/*          abs(SSMAX) is the larger singular value. */

/*  SNL     (output) DOUBLE PRECISION */
/*  CSL     (output) DOUBLE PRECISION */
/*          The vector (CSL, SNL) is a unit left singular vector for the */
/*          singular value abs(SSMAX). */

/*  SNR     (output) DOUBLE PRECISION */
/*  CSR     (output) DOUBLE PRECISION */
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
/*<       DOUBLE PRECISION   ZERO >*/
/*<       PARAMETER          ( ZERO = 0.0D0 ) >*/
/*<       DOUBLE PRECISION   HALF >*/
/*<       PARAMETER          ( HALF = 0.5D0 ) >*/
/*<       DOUBLE PRECISION   ONE >*/
/*<       PARAMETER          ( ONE = 1.0D0 ) >*/
/*<       DOUBLE PRECISION   TWO >*/
/*<       PARAMETER          ( TWO = 2.0D0 ) >*/
/*<       DOUBLE PRECISION   FOUR >*/
/*<       PARAMETER          ( FOUR = 4.0D0 ) >*/
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
/*<       DOUBLE PRECISION   DLAMCH >*/
/*<       EXTERNAL           DLAMCH >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       FT = F >*/
    ft = *f;
/*<       FA = ABS( FT ) >*/
    fa = abs(ft);
/*<       HT = H >*/
    ht = *h__;
/*<       HA = ABS( H ) >*/
    ha = abs(*h__);

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
    ga = abs(gt);
/*<       IF( GA.EQ.ZERO ) THEN >*/
    if (ga == 0.) {

/*        Diagonal matrix */

/*<          SSMIN = HA >*/
        *ssmin = ha;
/*<          SSMAX = FA >*/
        *ssmax = fa;
/*<          CLT = ONE >*/
        clt = 1.;
/*<          CRT = ONE >*/
        crt = 1.;
/*<          SLT = ZERO >*/
        slt = 0.;
/*<          SRT = ZERO >*/
        srt = 0.;
/*<       ELSE >*/
    } else {
/*<          GASMAL = .TRUE. >*/
        gasmal = TRUE_;
/*<          IF( GA.GT.FA ) THEN >*/
        if (ga > fa) {
/*<             PMAX = 2 >*/
            pmax = 2;
/*<             IF( ( FA / GA ).LT.DLAMCH( 'EPS' ) ) THEN >*/
            if (fa / ga < dlamch_("EPS", (ftnlen)3)) {

/*              Case of very large GA */

/*<                GASMAL = .FALSE. >*/
                gasmal = FALSE_;
/*<                SSMAX = GA >*/
                *ssmax = ga;
/*<                IF( HA.GT.ONE ) THEN >*/
                if (ha > 1.) {
/*<                   SSMIN = FA / ( GA / HA ) >*/
                    *ssmin = fa / (ga / ha);
/*<                ELSE >*/
                } else {
/*<                   SSMIN = ( FA / GA )*HA >*/
                    *ssmin = fa / ga * ha;
/*<                END IF >*/
                }
/*<                CLT = ONE >*/
                clt = 1.;
/*<                SLT = HT / GT >*/
                slt = ht / gt;
/*<                SRT = ONE >*/
                srt = 1.;
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
                l = 1.;
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
            t = 2. - l;

/*           Note that T .ge. 1 */

/*<             MM = M*M >*/
            mm = m * m;
/*<             TT = T*T >*/
            tt = t * t;
/*<             S = SQRT( TT+MM ) >*/
            s = sqrt(tt + mm);

/*           Note that 1 .le. S .le. 1 + 1/macheps */

/*<             IF( L.EQ.ZERO ) THEN >*/
            if (l == 0.) {
/*<                R = ABS( M ) >*/
                r__ = abs(m);
/*<             ELSE >*/
            } else {
/*<                R = SQRT( L*L+MM ) >*/
                r__ = sqrt(l * l + mm);
/*<             END IF >*/
            }

/*           Note that 0 .le. R .le. 1 + 1/macheps */

/*<             A = HALF*( S+R ) >*/
            a = (s + r__) * .5;

/*           Note that 1 .le. A .le. 1 + abs(M) */

/*<             SSMIN = HA / A >*/
            *ssmin = ha / a;
/*<             SSMAX = FA*A >*/
            *ssmax = fa * a;
/*<             IF( MM.EQ.ZERO ) THEN >*/
            if (mm == 0.) {

/*              Note that M is very tiny */

/*<                IF( L.EQ.ZERO ) THEN >*/
                if (l == 0.) {
/*<                   T = SIGN( TWO, FT )*SIGN( ONE, GT ) >*/
                    t = d_sign(&c_b3, &ft) * d_sign(&c_b4, &gt);
/*<                ELSE >*/
                } else {
/*<                   T = GT / SIGN( D, FT ) + M / T >*/
                    t = gt / d_sign(&d__, &ft) + m / t;
/*<                END IF >*/
                }
/*<             ELSE >*/
            } else {
/*<                T = ( M / ( S+T )+M / ( R+L ) )*( ONE+A ) >*/
                t = (m / (s + t) + m / (r__ + l)) * (a + 1.);
/*<             END IF >*/
            }
/*<             L = SQRT( T*T+FOUR ) >*/
            l = sqrt(t * t + 4.);
/*<             CRT = TWO / L >*/
            crt = 2. / l;
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
        tsign = d_sign(&c_b4, csr) * d_sign(&c_b4, csl) * d_sign(&c_b4, f);
    }
/*<    >*/
    if (pmax == 2) {
        tsign = d_sign(&c_b4, snr) * d_sign(&c_b4, csl) * d_sign(&c_b4, g);
    }
/*<    >*/
    if (pmax == 3) {
        tsign = d_sign(&c_b4, snr) * d_sign(&c_b4, snl) * d_sign(&c_b4, h__);
    }
/*<       SSMAX = SIGN( SSMAX, TSIGN ) >*/
    *ssmax = d_sign(ssmax, &tsign);
/*<       SSMIN = SIGN( SSMIN, TSIGN*SIGN( ONE, F )*SIGN( ONE, H ) ) >*/
    d__1 = tsign * d_sign(&c_b4, f) * d_sign(&c_b4, h__);
    *ssmin = d_sign(ssmin, &d__1);
/*<       RETURN >*/
    return 0;

/*     End of DLASV2 */

/*<       END >*/
} /* dlasv2_ */

#ifdef __cplusplus
        }
#endif
