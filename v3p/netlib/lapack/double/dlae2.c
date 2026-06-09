/* lapack/double/dlae2.f -- translated by f2c (version 20090411).
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

/*<       SUBROUTINE DLAE2( A, B, C, RT1, RT2 ) >*/
/* Subroutine */ int dlae2_(doublereal *a, doublereal *b, doublereal *c__,
        doublereal *rt1, doublereal *rt2)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal ab, df, tb, sm, rt, adf, acmn, acmx;


/*  -- LAPACK auxiliary routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       DOUBLE PRECISION   A, B, C, RT1, RT2 >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLAE2  computes the eigenvalues of a 2-by-2 symmetric matrix */
/*     [  A   B  ] */
/*     [  B   C  ]. */
/*  On return, RT1 is the eigenvalue of larger absolute value, and RT2 */
/*  is the eigenvalue of smaller absolute value. */

/*  Arguments */
/*  ========= */

/*  A       (input) DOUBLE PRECISION */
/*          The (1,1) element of the 2-by-2 matrix. */

/*  B       (input) DOUBLE PRECISION */
/*          The (1,2) and (2,1) elements of the 2-by-2 matrix. */

/*  C       (input) DOUBLE PRECISION */
/*          The (2,2) element of the 2-by-2 matrix. */

/*  RT1     (output) DOUBLE PRECISION */
/*          The eigenvalue of larger absolute value. */

/*  RT2     (output) DOUBLE PRECISION */
/*          The eigenvalue of smaller absolute value. */

/*  Further Details */
/*  =============== */

/*  RT1 is accurate to a few ulps barring over/underflow. */

/*  RT2 may be inaccurate if there is massive cancellation in the */
/*  determinant A*C-B*B; higher precision or correctly rounded or */
/*  correctly truncated arithmetic would be needed to compute RT2 */
/*  accurately in all cases. */

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
/*<       DOUBLE PRECISION   AB, ACMN, ACMX, ADF, DF, RT, SM, TB >*/
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

/*        Order of execution important. */
/*        To get fully accurate smaller eigenvalue, */
/*        next line needs to be executed in higher precision. */

/*<          RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B >*/
        *rt2 = acmx / *rt1 * acmn - *b / *rt1 * *b;
/*<       ELSE IF( SM.GT.ZERO ) THEN >*/
    } else if (sm > 0.) {
/*<          RT1 = HALF*( SM+RT ) >*/
        *rt1 = (sm + rt) * .5;

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
/*<       END IF >*/
    }
/*<       RETURN >*/
    return 0;

/*     End of DLAE2 */

/*<       END >*/
} /* dlae2_ */

#ifdef __cplusplus
        }
#endif
