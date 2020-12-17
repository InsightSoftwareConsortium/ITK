/* lapack/double/dlartg.f -- translated by f2c (version 20050501).
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
void v3p_netlib_dlartg_init()
{
  doublereal f=0, g=0, cs=0, sn=0, r=0;
  dlartg_(&f, &g, &cs, &sn, &r);
}

/*<       SUBROUTINE DLARTG( F, G, CS, SN, R ) >*/
/* Subroutine */ int dlartg_(doublereal *f, doublereal *g, doublereal *cs,
        doublereal *sn, doublereal *r__)
{
    /* Initialized data */

    static logical first = TRUE_; /* runtime-initialized constant */

    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double log(doublereal), pow_di(doublereal *, integer *), sqrt(doublereal);

    /* Local variables */
    integer i__;
    doublereal f1, g1, eps, scale;
    integer count;
    static doublereal safmn2, safmx2; /* runtime-initialized constant */
    extern doublereal dlamch_(char *, ftnlen);
    static doublereal safmin; /* runtime-initialized constant */


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*<       DOUBLE PRECISION   CS, F, G, R, SN >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLARTG generate a plane rotation so that */

/*     [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1. */
/*     [ -SN  CS  ]     [ G ]     [ 0 ] */

/*  This is a slower, more accurate version of the BLAS1 routine DROTG, */
/*  with the following other differences: */
/*     F and G are unchanged on return. */
/*     If G=0, then CS=1 and SN=0. */
/*     If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any */
/*        floating point operations (saves work in DBDSQR when */
/*        there are zeros on the diagonal). */

/*  If F exceeds G in magnitude, CS will be positive. */

/*  Arguments */
/*  ========= */

/*  F       (input) DOUBLE PRECISION */
/*          The first component of vector to be rotated. */

/*  G       (input) DOUBLE PRECISION */
/*          The second component of vector to be rotated. */

/*  CS      (output) DOUBLE PRECISION */
/*          The cosine of the rotation. */

/*  SN      (output) DOUBLE PRECISION */
/*          The sine of the rotation. */

/*  R       (output) DOUBLE PRECISION */
/*          The nonzero component of the rotated vector. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO >*/
/*<       PARAMETER          ( ZERO = 0.0D0 ) >*/
/*<       DOUBLE PRECISION   ONE >*/
/*<       PARAMETER          ( ONE = 1.0D0 ) >*/
/*<       DOUBLE PRECISION   TWO >*/
/*<       PARAMETER          ( TWO = 2.0D0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            FIRST >*/
/*<       INTEGER            COUNT, I >*/
/*<       DOUBLE PRECISION   EPS, F1, G1, SAFMIN, SAFMN2, SAFMX2, SCALE >*/
/*     .. */
/*     .. External Functions .. */
/*<       DOUBLE PRECISION   DLAMCH >*/
/*<       EXTERNAL           DLAMCH >*/
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
/*<          SAFMIN = DLAMCH( 'S' ) >*/
        safmin = dlamch_("S", (ftnlen)1);
/*<          EPS = DLAMCH( 'E' ) >*/
        eps = dlamch_("E", (ftnlen)1);
/*<    >*/
        d__1 = dlamch_("B", (ftnlen)1);
        i__1 = (integer) (log(safmin / eps) / log(dlamch_("B", (ftnlen)1)) /
                2.);
        safmn2 = pow_di(&d__1, &i__1);
/*<          SAFMX2 = ONE / SAFMN2 >*/
        safmx2 = 1. / safmn2;
/*<       END IF >*/
    }
/*<       IF( G.EQ.ZERO ) THEN >*/
    if (*g == 0.) {
/*<          CS = ONE >*/
        *cs = 1.;
/*<          SN = ZERO >*/
        *sn = 0.;
/*<          R = F >*/
        *r__ = *f;
/*<       ELSE IF( F.EQ.ZERO ) THEN >*/
    } else if (*f == 0.) {
/*<          CS = ZERO >*/
        *cs = 0.;
/*<          SN = ONE >*/
        *sn = 1.;
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
        d__1 = abs(f1), d__2 = abs(g1);
        scale = max(d__1,d__2);
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
            d__1 = abs(f1), d__2 = abs(g1);
            scale = max(d__1,d__2);
/*<    >*/
            if (scale >= safmx2) {
                goto L10;
            }
/*<             R = SQRT( F1**2+G1**2 ) >*/
/* Computing 2nd power */
            d__1 = f1;
/* Computing 2nd power */
            d__2 = g1;
            *r__ = sqrt(d__1 * d__1 + d__2 * d__2);
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
            d__1 = abs(f1), d__2 = abs(g1);
            scale = max(d__1,d__2);
/*<    >*/
            if (scale <= safmn2) {
                goto L30;
            }
/*<             R = SQRT( F1**2+G1**2 ) >*/
/* Computing 2nd power */
            d__1 = f1;
/* Computing 2nd power */
            d__2 = g1;
            *r__ = sqrt(d__1 * d__1 + d__2 * d__2);
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
            d__1 = f1;
/* Computing 2nd power */
            d__2 = g1;
            *r__ = sqrt(d__1 * d__1 + d__2 * d__2);
/*<             CS = F1 / R >*/
            *cs = f1 / *r__;
/*<             SN = G1 / R >*/
            *sn = g1 / *r__;
/*<          END IF >*/
        }
/*<          IF( ABS( F ).GT.ABS( G ) .AND. CS.LT.ZERO ) THEN >*/
        if (abs(*f) > abs(*g) && *cs < 0.) {
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

/*     End of DLARTG */

/*<       END >*/
} /* dlartg_ */

#ifdef __cplusplus
        }
#endif
