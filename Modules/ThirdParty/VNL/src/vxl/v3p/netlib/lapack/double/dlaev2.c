/* dlaev2.f -- translated by f2c (version 20060506).
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

/*<       SUBROUTINE DLAEV2( A, B, C, RT1, RT2, CS1, SN1 ) >*/
/* Subroutine */ int dlaev2_(doublereal *a, doublereal *b, doublereal *c__,
        doublereal *rt1, doublereal *rt2, doublereal *cs1, doublereal *sn1)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal ab, df, cs, ct, tb, sm, tn, rt, adf, acs;
    integer sgn1, sgn2;
    doublereal acmn, acmx;


/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*     .. Scalar Arguments .. */
/*<       DOUBLE PRECISION   A, B, C, CS1, RT1, RT2, SN1 >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLAEV2 computes the eigendecomposition of a 2-by-2 symmetric matrix */
/*     [  A   B  ] */
/*     [  B   C  ]. */
/*  On return, RT1 is the eigenvalue of larger absolute value, RT2 is the */
/*  eigenvalue of smaller absolute value, and (CS1,SN1) is the unit right */
/*  eigenvector for RT1, giving the decomposition */

/*     [ CS1  SN1 ] [  A   B  ] [ CS1 -SN1 ]  =  [ RT1  0  ] */
/*     [-SN1  CS1 ] [  B   C  ] [ SN1  CS1 ]     [  0  RT2 ]. */

/*  Arguments */
/*  ========= */

/*  A       (input) DOUBLE PRECISION */
/*          The (1,1) element of the 2-by-2 matrix. */

/*  B       (input) DOUBLE PRECISION */
/*          The (1,2) element and the conjugate of the (2,1) element of */
/*          the 2-by-2 matrix. */

/*  C       (input) DOUBLE PRECISION */
/*          The (2,2) element of the 2-by-2 matrix. */

/*  RT1     (output) DOUBLE PRECISION */
/*          The eigenvalue of larger absolute value. */

/*  RT2     (output) DOUBLE PRECISION */
/*          The eigenvalue of smaller absolute value. */

/*  CS1     (output) DOUBLE PRECISION */
/*  SN1     (output) DOUBLE PRECISION */
/*          The vector (CS1, SN1) is a unit right eigenvector for RT1. */

/*  Further Details */
/*  =============== */

/*  RT1 is accurate to a few ulps barring over/underflow. */

/*  RT2 may be inaccurate if there is massive cancellation in the */
/*  determinant A*C-B*B; higher precision or correctly rounded or */
/*  correctly truncated arithmetic would be needed to compute RT2 */
/*  accurately in all cases. */

/*  CS1 and SN1 are accurate to a few ulps barring over/underflow. */

/*  Overflow is possible only if RT1 is within a factor of 5 of overflow. */
/*  Underflow is harmless if the input data is 0 or exceeds */
/*     underflow_threshold / macheps. */

/* ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ONE >*/
/*<       PARAMETER          ( ONE = 1.0D0 ) >*/
/*<       DOUBLE PRECISION   TWO >*/
/*<       PARAMETER          ( TWO = 2.0D0 ) >*/
/*<       DOUBLE PRECISION   ZERO >*/
/*<       PARAMETER          ( ZERO = 0.0D0 ) >*/
/*<       DOUBLE PRECISION   HALF >*/
/*<       PARAMETER          ( HALF = 0.5D0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            SGN1, SGN2 >*/
/*<    >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Compute the eigenvalues */

/*<       SM = A + C >*/
    sm = *a + *c__;
/*<       DF = A - C >*/
    df = *a - *c__;
/*<       ADF = ABS( DF ) >*/
    adf = abs(df);
/*<       TB = B + B >*/
    tb = *b + *b;
/*<       AB = ABS( TB ) >*/
    ab = abs(tb);
/*<       IF( ABS( A ).GT.ABS( C ) ) THEN >*/
    if (abs(*a) > abs(*c__)) {
/*<          ACMX = A >*/
        acmx = *a;
/*<          ACMN = C >*/
        acmn = *c__;
/*<       ELSE >*/
    } else {
/*<          ACMX = C >*/
        acmx = *c__;
/*<          ACMN = A >*/
        acmn = *a;
/*<       END IF >*/
    }
/*<       IF( ADF.GT.AB ) THEN >*/
    if (adf > ab) {
/*<          RT = ADF*SQRT( ONE+( AB / ADF )**2 ) >*/
/* Computing 2nd power */
        d__1 = ab / adf;
        rt = adf * sqrt(d__1 * d__1 + 1.);
/*<       ELSE IF( ADF.LT.AB ) THEN >*/
    } else if (adf < ab) {
/*<          RT = AB*SQRT( ONE+( ADF / AB )**2 ) >*/
/* Computing 2nd power */
        d__1 = adf / ab;
        rt = ab * sqrt(d__1 * d__1 + 1.);
/*<       ELSE >*/
    } else {

/*        Includes case AB=ADF=0 */

/*<          RT = AB*SQRT( TWO ) >*/
        rt = ab * sqrt(2.);
/*<       END IF >*/
    }
/*<       IF( SM.LT.ZERO ) THEN >*/
    if (sm < 0.) {
/*<          RT1 = HALF*( SM-RT ) >*/
        *rt1 = (sm - rt) * .5;
/*<          SGN1 = -1 >*/
        sgn1 = -1;

/*        Order of execution important. */
/*        To get fully accurate smaller eigenvalue, */
/*        next line needs to be executed in higher precision. */

/*<          RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B >*/
        *rt2 = acmx / *rt1 * acmn - *b / *rt1 * *b;
/*<       ELSE IF( SM.GT.ZERO ) THEN >*/
    } else if (sm > 0.) {
/*<          RT1 = HALF*( SM+RT ) >*/
        *rt1 = (sm + rt) * .5;
/*<          SGN1 = 1 >*/
        sgn1 = 1;

/*        Order of execution important. */
/*        To get fully accurate smaller eigenvalue, */
/*        next line needs to be executed in higher precision. */

/*<          RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B >*/
        *rt2 = acmx / *rt1 * acmn - *b / *rt1 * *b;
/*<       ELSE >*/
    } else {

/*        Includes case RT1 = RT2 = 0 */

/*<          RT1 = HALF*RT >*/
        *rt1 = rt * .5;
/*<          RT2 = -HALF*RT >*/
        *rt2 = rt * -.5;
/*<          SGN1 = 1 >*/
        sgn1 = 1;
/*<       END IF >*/
    }

/*     Compute the eigenvector */

/*<       IF( DF.GE.ZERO ) THEN >*/
    if (df >= 0.) {
/*<          CS = DF + RT >*/
        cs = df + rt;
/*<          SGN2 = 1 >*/
        sgn2 = 1;
/*<       ELSE >*/
    } else {
/*<          CS = DF - RT >*/
        cs = df - rt;
/*<          SGN2 = -1 >*/
        sgn2 = -1;
/*<       END IF >*/
    }
/*<       ACS = ABS( CS ) >*/
    acs = abs(cs);
/*<       IF( ACS.GT.AB ) THEN >*/
    if (acs > ab) {
/*<          CT = -TB / CS >*/
        ct = -tb / cs;
/*<          SN1 = ONE / SQRT( ONE+CT*CT ) >*/
        *sn1 = 1. / sqrt(ct * ct + 1.);
/*<          CS1 = CT*SN1 >*/
        *cs1 = ct * *sn1;
/*<       ELSE >*/
    } else {
/*<          IF( AB.EQ.ZERO ) THEN >*/
        if (ab == 0.) {
/*<             CS1 = ONE >*/
            *cs1 = 1.;
/*<             SN1 = ZERO >*/
            *sn1 = 0.;
/*<          ELSE >*/
        } else {
/*<             TN = -CS / TB >*/
            tn = -cs / tb;
/*<             CS1 = ONE / SQRT( ONE+TN*TN ) >*/
            *cs1 = 1. / sqrt(tn * tn + 1.);
/*<             SN1 = TN*CS1 >*/
            *sn1 = tn * *cs1;
/*<          END IF >*/
        }
/*<       END IF >*/
    }
/*<       IF( SGN1.EQ.SGN2 ) THEN >*/
    if (sgn1 == sgn2) {
/*<          TN = CS1 >*/
        tn = *cs1;
/*<          CS1 = -SN1 >*/
        *cs1 = -(*sn1);
/*<          SN1 = TN >*/
        *sn1 = tn;
/*<       END IF >*/
    }
/*<       RETURN >*/
    return 0;

/*     End of DLAEV2 */

/*<       END >*/
} /* dlaev2_ */

#ifdef __cplusplus
        }
#endif
