/* dlas2.f -- translated by f2c (version of 4 June 1993  1:43:59).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/*<       SUBROUTINE DLAS2( F, G, H, SSMIN, SSMAX ) >*/
/* Subroutine */ int dlas2_(doublereal *f, doublereal *g, doublereal *h,
        doublereal *ssmin, doublereal *ssmax)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal fhmn, fhmx, c, fa, ga, ha, as, at, au;


/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*<       DOUBLE PRECISION   F, G, H, SSMAX, SSMIN >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLAS2  computes the singular values of the 2-by-2 matrix */
/*     [  F   G  ] */
/*     [  0   H  ]. */
/*  On return, SSMIN is the smaller singular value and SSMAX is the */
/*  larger singular value. */

/*  Arguments */
/*  ========= */

/*  F       (input) DOUBLE PRECISION */
/*          The (1,1) element of the 2-by-2 matrix. */

/*  G       (input) DOUBLE PRECISION */
/*          The (1,2) element of the 2-by-2 matrix. */

/*  H       (input) DOUBLE PRECISION */
/*          The (2,2) element of the 2-by-2 matrix. */

/*  SSMIN   (output) DOUBLE PRECISION */
/*          The smaller singular value. */

/*  SSMAX   (output) DOUBLE PRECISION */
/*          The larger singular value. */

/*  Further Details */
/*  =============== */

/*  Barring over/underflow, all output quantities are correct to within */
/*  a few units in the last place (ulps), even in the absence of a guard
*/
/*  digit in addition/subtraction. */

/*  In IEEE arithmetic, the code works correctly if one matrix element is
*/
/*  infinite. */

/*  Overflow will not occur unless the largest singular value itself */
/*  overflows, or is within a few ulps of overflow. (On machines with */
/*  partial overflow, like the Cray, overflow may occur if the largest */
/*  singular value is within a factor of 2 of overflow.) */

/*  Underflow is harmless if underflow is gradual. Otherwise, results */
/*  may correspond to a matrix modified by perturbations of size near */
/*  the underflow threshold. */

/*  ====================================================================
*/

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO >*/
/*<       PARAMETER          ( ZERO = 0.0D0 ) >*/
/*<       DOUBLE PRECISION   ONE >*/
/*<       PARAMETER          ( ONE = 1.0D0 ) >*/
/*<       DOUBLE PRECISION   TWO >*/
/*<       PARAMETER          ( TWO = 2.0D0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       DOUBLE PRECISION   AS, AT, AU, C, FA, FHMN, FHMX, GA, HA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, MIN, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       FA = ABS( F ) >*/
    fa = abs(*f);
/*<       GA = ABS( G ) >*/
    ga = abs(*g);
/*<       HA = ABS( H ) >*/
    ha = abs(*h);
/*<       FHMN = MIN( FA, HA ) >*/
    fhmn = min(fa,ha);
/*<       FHMX = MAX( FA, HA ) >*/
    fhmx = max(fa,ha);
/*<       IF( FHMN.EQ.ZERO ) THEN >*/
    if (fhmn == 0.) {
/*<          SSMIN = ZERO >*/
        *ssmin = 0.;
/*<          IF( FHMX.EQ.ZERO ) THEN >*/
        if (fhmx == 0.) {
/*<             SSMAX = GA >*/
            *ssmax = ga;
/*<          ELSE >*/
        } else {
/*<    >*/
/* Computing 2nd power */
            d__1 = min(fhmx,ga) / max(fhmx,ga);
            *ssmax = max(fhmx,ga) * sqrt(d__1 * d__1 + 1.);
/*<          END IF >*/
        }
/*<       ELSE >*/
    } else {
/*<          IF( GA.LT.FHMX ) THEN >*/
        if (ga < fhmx) {
/*<             AS = ONE + FHMN / FHMX >*/
            as = fhmn / fhmx + 1.;
/*<             AT = ( FHMX-FHMN ) / FHMX >*/
            at = (fhmx - fhmn) / fhmx;
/*<             AU = ( GA / FHMX )**2 >*/
/* Computing 2nd power */
            d__1 = ga / fhmx;
            au = d__1 * d__1;
/*<             C = TWO / ( SQRT( AS*AS+AU )+SQRT( AT*AT+AU ) ) >*/
            c = 2. / (sqrt(as * as + au) + sqrt(at * at + au));
/*<             SSMIN = FHMN*C >*/
            *ssmin = fhmn * c;
/*<             SSMAX = FHMX / C >*/
            *ssmax = fhmx / c;
/*<          ELSE >*/
        } else {
/*<             AU = FHMX / GA >*/
            au = fhmx / ga;
/*<             IF( AU.EQ.ZERO ) THEN >*/
            if (au == 0.) {

/*              Avoid possible harmful underflow if exponent r
ange */
/*              asymmetric (true SSMIN may not underflow even
if */
/*              AU underflows) */

/*<                SSMIN = ( FHMN*FHMX ) / GA >*/
                *ssmin = fhmn * fhmx / ga;
/*<                SSMAX = GA >*/
                *ssmax = ga;
/*<             ELSE >*/
            } else {
/*<                AS = ONE + FHMN / FHMX >*/
                as = fhmn / fhmx + 1.;
/*<                AT = ( FHMX-FHMN ) / FHMX >*/
                at = (fhmx - fhmn) / fhmx;
/*<    >*/
/* Computing 2nd power */
                d__1 = as * au;
/* Computing 2nd power */
                d__2 = at * au;
                c = 1. / (sqrt(d__1 * d__1 + 1.) + sqrt(d__2 * d__2 + 1.));
/*<                SSMIN = ( FHMN*C )*AU >*/
                *ssmin = fhmn * c * au;
/*<                SSMIN = SSMIN + SSMIN >*/
                *ssmin += *ssmin;
/*<                SSMAX = GA / ( C+C ) >*/
                *ssmax = ga / (c + c);
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }
/*<       RETURN >*/
    return 0;

/*     End of DLAS2 */

/*<       END >*/
} /* dlas2_ */

