#include "f2c.h"
#include "netlib.h"
extern double log(double), sqrt(double); /* #include <math.h> */

/* Subroutine */ void dlartg_(doublereal *f, doublereal *g, doublereal *cs, doublereal *sn, doublereal *r)
{
    /* Initialized data */
    static logical first = TRUE_;

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    static integer i;
    static doublereal scale;
    static integer count;
    static doublereal f1, g1, safmn2, safmx2;
    static doublereal safmin, eps;

/*  -- LAPACK auxiliary routine (version 2.0) --                          */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,        */
/*     Courant Institute, Argonne National Lab, and Rice University       */
/*     September 30, 1994                                                 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DLARTG generate a plane rotation so that                              */
/*                                                                        */
/*     [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.        */
/*     [ -SN  CS  ]     [ G ]     [ 0 ]                                   */
/*                                                                        */
/*  This is a slower, more accurate version of the BLAS1 routine DROTG,   */
/*  with the following other differences:                                 */
/*     F and G are unchanged on return.                                   */
/*     If G=0, then CS=1 and SN=0.                                        */
/*     If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any        */
/*        floating point operations (saves work in DBDSQR when            */
/*        there are zeros on the diagonal).                               */
/*                                                                        */
/*  If F exceeds G in magnitude, CS will be positive.                     */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  F       (input) DOUBLE PRECISION                                      */
/*          The first component of vector to be rotated.                  */
/*                                                                        */
/*  G       (input) DOUBLE PRECISION                                      */
/*          The second component of vector to be rotated.                 */
/*                                                                        */
/*  CS      (output) DOUBLE PRECISION                                     */
/*          The cosine of the rotation.                                   */
/*                                                                        */
/*  SN      (output) DOUBLE PRECISION                                     */
/*          The sine of the rotation.                                     */
/*                                                                        */
/*  R       (output) DOUBLE PRECISION                                     */
/*          The nonzero component of the rotated vector.                  */
/*                                                                        */
/*  ===================================================================== */

    if (first) {
        first = FALSE_;
        safmin = dlamch_("S");
        eps = dlamch_("E");
        d__1 = dlamch_("B");
        i__1 = (integer) (log(safmin / eps) / log(dlamch_("B")) / 2.);
        safmn2 = pow_di(&d__1, &i__1);
        safmx2 = 1. / safmn2;
    }
    if (*g == 0.) {
        *cs = 1.; *sn = 0.;
        *r = *f;
    } else if (*f == 0.) {
        *cs = 0.; *sn = 1.;
        *r = *g;
    } else {
        f1 = *f; g1 = *g;
        scale = max(abs(f1),abs(g1));
        count = 0;
        if (scale >= safmx2) {
            while (scale >= safmx2) {
                ++count;
                f1 *= safmn2;
                g1 *= safmn2;
                scale = max(abs(f1),abs(g1));
            }
            *r = sqrt(f1 * f1 + g1 * g1);
            *cs = f1 / *r;
            *sn = g1 / *r;
            for (i = 1; i <= count; ++i) {
                *r *= safmx2;
            }
        } else if (scale <= safmn2) {
            while (scale <= safmn2) {
                ++count;
                f1 *= safmx2;
                g1 *= safmx2;
                scale = max(abs(f1),abs(g1));
            }
            *r = sqrt(f1 * f1 + g1 * g1);
            *cs = f1 / *r;
            *sn = g1 / *r;
            for (i = 1; i <= count; ++i) {
                *r *= safmn2;
            }
        } else {
            *r = sqrt(f1 * f1 + g1 * g1);
            *cs = f1 / *r;
            *sn = g1 / *r;
        }
        if (abs(*f) > abs(*g) && *cs < 0.) {
            *cs = -(*cs);
            *sn = -(*sn);
            *r = -(*r);
        }
    }
} /* dlartg_ */
