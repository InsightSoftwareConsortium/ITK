#include "f2c.h"
#include "netlib.h"
extern double log(double), sqrt(double); /* #include <math.h> */

/* Subroutine */ void slartg_(real *f, real *g, real *cs, real *sn, real *r)
{
    /* Initialized data */
    static logical first = TRUE_;

    /* System generated locals */
    integer i__1;
    real r__1;

    /* Local variables */
    static integer i;
    static real scale;
    static integer count;
    static real f1, g1, safmn2, safmx2;
    static real safmin, eps;

/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  SLARTG generate a plane rotation so that                              */
/*                                                                        */
/*     [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.        */
/*     [ -SN  CS  ]     [ G ]     [ 0 ]                                   */
/*                                                                        */
/*  This is a slower, more accurate version of the BLAS1 routine SROTG,   */
/*  with the following other differences:                                 */
/*     F and G are unchanged on return.                                   */
/*     If G=0, then CS=1 and SN=0.                                        */
/*     If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any        */
/*        floating point operations (saves work in SBDSQR when            */
/*        there are zeros on the diagonal).                               */
/*                                                                        */
/*  If F exceeds G in magnitude, CS will be positive.                     */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  F       (input) REAL                                                  */
/*          The first component of vector to be rotated.                  */
/*                                                                        */
/*  G       (input) REAL                                                  */
/*          The second component of vector to be rotated.                 */
/*                                                                        */
/*  CS      (output) REAL                                                 */
/*          The cosine of the rotation.                                   */
/*                                                                        */
/*  SN      (output) REAL                                                 */
/*          The sine of the rotation.                                     */
/*                                                                        */
/*  R       (output) REAL                                                 */
/*          The nonzero component of the rotated vector.                  */
/*                                                                        */
/*  ===================================================================== */

    if (first) {
        first = FALSE_;
        safmin = slamch_("S");
        eps = slamch_("E");
        r__1 = slamch_("B");
        i__1 = (integer) (log(safmin / eps) / log((double)slamch_("B")) / 2.f);
        safmn2 = pow_ri(&r__1, &i__1);
        safmx2 = 1.f / safmn2;
    }
    if (*g == 0.f) {
        *cs = 1.f; *sn = 0.f;
        *r = *f;
    } else if (*f == 0.f) {
        *cs = 0.f; *sn = 1.f;
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
            *r = sqrtf(f1 * f1 + g1 * g1);
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
            *r = sqrtf(f1 * f1 + g1 * g1);
            *cs = f1 / *r;
            *sn = g1 / *r;
            for (i = 1; i <= count; ++i) {
                *r *= safmn2;
            }
        } else {
            *r = sqrtf(f1 * f1 + g1 * g1);
            *cs = f1 / *r;
            *sn = g1 / *r;
        }
        if (abs(*f) > abs(*g) && *cs < 0.f) {
            *cs = -(*cs);
            *sn = -(*sn);
            *r = -(*r);
        }
    }
} /* slartg_ */
