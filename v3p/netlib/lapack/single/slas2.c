/* lapack/single/slas2.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE SLAS2( F, G, H, SSMIN, SSMAX ) >*/
/* Subroutine */ int slas2_(real *f, real *g, real *h__, real *ssmin, real *
        ssmax)
{
    /* System generated locals */
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    real c__, fa, ga, ha, as, at, au, fhmn, fhmx;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*<       REAL               F, G, H, SSMAX, SSMIN >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  SLAS2  computes the singular values of the 2-by-2 matrix */
/*     [  F   G  ] */
/*     [  0   H  ]. */
/*  On return, SSMIN is the smaller singular value and SSMAX is the */
/*  larger singular value. */

/*  Arguments */
/*  ========= */

/*  F       (input) REAL */
/*          The (1,1) element of the 2-by-2 matrix. */

/*  G       (input) REAL */
/*          The (1,2) element of the 2-by-2 matrix. */

/*  H       (input) REAL */
/*          The (2,2) element of the 2-by-2 matrix. */

/*  SSMIN   (output) REAL */
/*          The smaller singular value. */

/*  SSMAX   (output) REAL */
/*          The larger singular value. */

/*  Further Details */
/*  =============== */

/*  Barring over/underflow, all output quantities are correct to within */
/*  a few units in the last place (ulps), even in the absence of a guard */
/*  digit in addition/subtraction. */

/*  In IEEE arithmetic, the code works correctly if one matrix element is */
/*  infinite. */

/*  Overflow will not occur unless the largest singular value itself */
/*  overflows, or is within a few ulps of overflow. (On machines with */
/*  partial overflow, like the Cray, overflow may occur if the largest */
/*  singular value is within a factor of 2 of overflow.) */

/*  Underflow is harmless if underflow is gradual. Otherwise, results */
/*  may correspond to a matrix modified by perturbations of size near */
/*  the underflow threshold. */

/*  ==================================================================== */

/*     .. Parameters .. */
/*<       REAL               ZERO >*/
/*<       PARAMETER          ( ZERO = 0.0E0 ) >*/
/*<       REAL               ONE >*/
/*<       PARAMETER          ( ONE = 1.0E0 ) >*/
/*<       REAL               TWO >*/
/*<       PARAMETER          ( TWO = 2.0E0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       REAL               AS, AT, AU, C, FA, FHMN, FHMX, GA, HA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, MIN, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       FA = ABS( F ) >*/
    fa = dabs(*f);
/*<       GA = ABS( G ) >*/
    ga = dabs(*g);
/*<       HA = ABS( H ) >*/
    ha = dabs(*h__);
/*<       FHMN = MIN( FA, HA ) >*/
    fhmn = dmin(fa,ha);
/*<       FHMX = MAX( FA, HA ) >*/
    fhmx = dmax(fa,ha);
/*<       IF( FHMN.EQ.ZERO ) THEN >*/
    if (fhmn == (float)0.) {
/*<          SSMIN = ZERO >*/
        *ssmin = (float)0.;
/*<          IF( FHMX.EQ.ZERO ) THEN >*/
        if (fhmx == (float)0.) {
/*<             SSMAX = GA >*/
            *ssmax = ga;
/*<          ELSE >*/
        } else {
/*<    >*/
/* Computing 2nd power */
            r__1 = dmin(fhmx,ga) / dmax(fhmx,ga);
            *ssmax = dmax(fhmx,ga) * sqrt(r__1 * r__1 + (float)1.);
/*<          END IF >*/
        }
/*<       ELSE >*/
    } else {
/*<          IF( GA.LT.FHMX ) THEN >*/
        if (ga < fhmx) {
/*<             AS = ONE + FHMN / FHMX >*/
            as = fhmn / fhmx + (float)1.;
/*<             AT = ( FHMX-FHMN ) / FHMX >*/
            at = (fhmx - fhmn) / fhmx;
/*<             AU = ( GA / FHMX )**2 >*/
/* Computing 2nd power */
            r__1 = ga / fhmx;
            au = r__1 * r__1;
/*<             C = TWO / ( SQRT( AS*AS+AU )+SQRT( AT*AT+AU ) ) >*/
            c__ = (float)2. / (sqrt(as * as + au) + sqrt(at * at + au));
/*<             SSMIN = FHMN*C >*/
            *ssmin = fhmn * c__;
/*<             SSMAX = FHMX / C >*/
            *ssmax = fhmx / c__;
/*<          ELSE >*/
        } else {
/*<             AU = FHMX / GA >*/
            au = fhmx / ga;
/*<             IF( AU.EQ.ZERO ) THEN >*/
            if (au == (float)0.) {

/*              Avoid possible harmful underflow if exponent range */
/*              asymmetric (true SSMIN may not underflow even if */
/*              AU underflows) */

/*<                SSMIN = ( FHMN*FHMX ) / GA >*/
                *ssmin = fhmn * fhmx / ga;
/*<                SSMAX = GA >*/
                *ssmax = ga;
/*<             ELSE >*/
            } else {
/*<                AS = ONE + FHMN / FHMX >*/
                as = fhmn / fhmx + (float)1.;
/*<                AT = ( FHMX-FHMN ) / FHMX >*/
                at = (fhmx - fhmn) / fhmx;
/*<    >*/
/* Computing 2nd power */
                r__1 = as * au;
/* Computing 2nd power */
                r__2 = at * au;
                c__ = (float)1. / (sqrt(r__1 * r__1 + (float)1.) + sqrt(r__2 *
                         r__2 + (float)1.));
/*<                SSMIN = ( FHMN*C )*AU >*/
                *ssmin = fhmn * c__ * au;
/*<                SSMIN = SSMIN + SSMIN >*/
                *ssmin += *ssmin;
/*<                SSMAX = GA / ( C+C ) >*/
                *ssmax = ga / (c__ + c__);
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }
/*<       RETURN >*/
    return 0;

/*     End of SLAS2 */

/*<       END >*/
} /* slas2_ */

#ifdef __cplusplus
        }
#endif
