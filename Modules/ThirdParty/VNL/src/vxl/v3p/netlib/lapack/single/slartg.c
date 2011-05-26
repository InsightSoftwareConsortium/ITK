/* lapack/single/slartg.f -- translated by f2c (version 20050501).
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

/* Initialization function just calls the function once so that its
   runtime-initialized constants are initialized.  After the first
   call it is safe to call the function from multiple threads at
   once.  */
void v3p_netlib_slartg_init()
{
  real f=0, g=0, cs=0, sn=0, r=0;
  slartg_(&f, &g, &cs, &sn, &r);
}

/*<       SUBROUTINE SLARTG( F, G, CS, SN, R ) >*/
/* Subroutine */ int slartg_(real *f, real *g, real *cs, real *sn, real *r__)
{
    /* Initialized data */

    static logical first = TRUE_; /* runtime-initialized constant */

    /* System generated locals */
    integer i__1;
    real r__1, r__2;

    /* Builtin functions */
    double log(doublereal), pow_ri(real *, integer *), sqrt(doublereal);

    /* Local variables */
    integer i__;
    real f1, g1, eps, scale;
    integer count;
    static real safmn2, safmx2; /* runtime-initialized constant */
    extern doublereal slamch_(char *, ftnlen);
    static real safmin; /* runtime-initialized constant */


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*<       REAL               CS, F, G, R, SN >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  SLARTG generate a plane rotation so that */

/*     [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1. */
/*     [ -SN  CS  ]     [ G ]     [ 0 ] */

/*  This is a slower, more accurate version of the BLAS1 routine SROTG, */
/*  with the following other differences: */
/*     F and G are unchanged on return. */
/*     If G=0, then CS=1 and SN=0. */
/*     If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any */
/*        floating point operations (saves work in SBDSQR when */
/*        there are zeros on the diagonal). */

/*  If F exceeds G in magnitude, CS will be positive. */

/*  Arguments */
/*  ========= */

/*  F       (input) REAL */
/*          The first component of vector to be rotated. */

/*  G       (input) REAL */
/*          The second component of vector to be rotated. */

/*  CS      (output) REAL */
/*          The cosine of the rotation. */

/*  SN      (output) REAL */
/*          The sine of the rotation. */

/*  R       (output) REAL */
/*          The nonzero component of the rotated vector. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       REAL               ZERO >*/
/*<       PARAMETER          ( ZERO = 0.0E0 ) >*/
/*<       REAL               ONE >*/
/*<       PARAMETER          ( ONE = 1.0E0 ) >*/
/*<       REAL               TWO >*/
/*<       PARAMETER          ( TWO = 2.0E0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            FIRST >*/
/*<       INTEGER            COUNT, I >*/
/*<       REAL               EPS, F1, G1, SAFMIN, SAFMN2, SAFMX2, SCALE >*/
/*     .. */
/*     .. External Functions .. */
/*<       REAL               SLAMCH >*/
/*<       EXTERNAL           SLAMCH >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, INT, LOG, MAX, SQRT >*/
/*     .. */
/*     .. Save statement .. */
/*<       SAVE               FIRST, SAFMX2, SAFMIN, SAFMN2 >*/
/*     .. */
/*     .. Data statements .. */
/*<       DATA               FIRST / .TRUE. / >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       IF( FIRST ) THEN >*/
    if (first) {
/*<          FIRST = .FALSE. >*/
        first = FALSE_;
/*<          SAFMIN = SLAMCH( 'S' ) >*/
        safmin = slamch_("S", (ftnlen)1);
/*<          EPS = SLAMCH( 'E' ) >*/
        eps = slamch_("E", (ftnlen)1);
/*<    >*/
        r__1 = slamch_("B", (ftnlen)1);
        i__1 = (integer) (log(safmin / eps) / log(slamch_("B", (ftnlen)1)) / (
                float)2.);
        safmn2 = pow_ri(&r__1, &i__1);
/*<          SAFMX2 = ONE / SAFMN2 >*/
        safmx2 = (float)1. / safmn2;
/*<       END IF >*/
    }
/*<       IF( G.EQ.ZERO ) THEN >*/
    if (*g == (float)0.) {
/*<          CS = ONE >*/
        *cs = (float)1.;
/*<          SN = ZERO >*/
        *sn = (float)0.;
/*<          R = F >*/
        *r__ = *f;
/*<       ELSE IF( F.EQ.ZERO ) THEN >*/
    } else if (*f == (float)0.) {
/*<          CS = ZERO >*/
        *cs = (float)0.;
/*<          SN = ONE >*/
        *sn = (float)1.;
/*<          R = G >*/
        *r__ = *g;
/*<       ELSE >*/
    } else {
/*<          F1 = F >*/
        f1 = *f;
/*<          G1 = G >*/
        g1 = *g;
/*<          SCALE = MAX( ABS( F1 ), ABS( G1 ) ) >*/
/* Computing MAX */
        r__1 = dabs(f1), r__2 = dabs(g1);
        scale = dmax(r__1,r__2);
/*<          IF( SCALE.GE.SAFMX2 ) THEN >*/
        if (scale >= safmx2) {
/*<             COUNT = 0 >*/
            count = 0;
/*<    10       CONTINUE >*/
L10:
/*<             COUNT = COUNT + 1 >*/
            ++count;
/*<             F1 = F1*SAFMN2 >*/
            f1 *= safmn2;
/*<             G1 = G1*SAFMN2 >*/
            g1 *= safmn2;
/*<             SCALE = MAX( ABS( F1 ), ABS( G1 ) ) >*/
/* Computing MAX */
            r__1 = dabs(f1), r__2 = dabs(g1);
            scale = dmax(r__1,r__2);
/*<    >*/
            if (scale >= safmx2) {
                goto L10;
            }
/*<             R = SQRT( F1**2+G1**2 ) >*/
/* Computing 2nd power */
            r__1 = f1;
/* Computing 2nd power */
            r__2 = g1;
            *r__ = sqrt(r__1 * r__1 + r__2 * r__2);
/*<             CS = F1 / R >*/
            *cs = f1 / *r__;
/*<             SN = G1 / R >*/
            *sn = g1 / *r__;
/*<             DO 20 I = 1, COUNT >*/
            i__1 = count;
            for (i__ = 1; i__ <= i__1; ++i__) {
/*<                R = R*SAFMX2 >*/
                *r__ *= safmx2;
/*<    20       CONTINUE >*/
/* L20: */
            }
/*<          ELSE IF( SCALE.LE.SAFMN2 ) THEN >*/
        } else if (scale <= safmn2) {
/*<             COUNT = 0 >*/
            count = 0;
/*<    30       CONTINUE >*/
L30:
/*<             COUNT = COUNT + 1 >*/
            ++count;
/*<             F1 = F1*SAFMX2 >*/
            f1 *= safmx2;
/*<             G1 = G1*SAFMX2 >*/
            g1 *= safmx2;
/*<             SCALE = MAX( ABS( F1 ), ABS( G1 ) ) >*/
/* Computing MAX */
            r__1 = dabs(f1), r__2 = dabs(g1);
            scale = dmax(r__1,r__2);
/*<    >*/
            if (scale <= safmn2) {
                goto L30;
            }
/*<             R = SQRT( F1**2+G1**2 ) >*/
/* Computing 2nd power */
            r__1 = f1;
/* Computing 2nd power */
            r__2 = g1;
            *r__ = sqrt(r__1 * r__1 + r__2 * r__2);
/*<             CS = F1 / R >*/
            *cs = f1 / *r__;
/*<             SN = G1 / R >*/
            *sn = g1 / *r__;
/*<             DO 40 I = 1, COUNT >*/
            i__1 = count;
            for (i__ = 1; i__ <= i__1; ++i__) {
/*<                R = R*SAFMN2 >*/
                *r__ *= safmn2;
/*<    40       CONTINUE >*/
/* L40: */
            }
/*<          ELSE >*/
        } else {
/*<             R = SQRT( F1**2+G1**2 ) >*/
/* Computing 2nd power */
            r__1 = f1;
/* Computing 2nd power */
            r__2 = g1;
            *r__ = sqrt(r__1 * r__1 + r__2 * r__2);
/*<             CS = F1 / R >*/
            *cs = f1 / *r__;
/*<             SN = G1 / R >*/
            *sn = g1 / *r__;
/*<          END IF >*/
        }
/*<          IF( ABS( F ).GT.ABS( G ) .AND. CS.LT.ZERO ) THEN >*/
        if (dabs(*f) > dabs(*g) && *cs < (float)0.) {
/*<             CS = -CS >*/
            *cs = -(*cs);
/*<             SN = -SN >*/
            *sn = -(*sn);
/*<             R = -R >*/
            *r__ = -(*r__);
/*<          END IF >*/
        }
/*<       END IF >*/
    }
/*<       RETURN >*/
    return 0;

/*     End of SLARTG */

/*<       END >*/
} /* slartg_ */

#ifdef __cplusplus
        }
#endif
