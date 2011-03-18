/* mathews/simpson.f -- translated by f2c (version 20050501).
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

/*     NUMERICAL METHODS: FORTRAN Programs, (c) John H. Mathews 1994 */
/*     To accompany the text: */
/*     NUMERICAL METHODS for Mathematics, Science and Engineering, 2nd Ed, 1992 */
/*     Prentice Hall, Englewood Cliffs, New Jersey, 07632, U.S.A. */
/*     This free software is complements of the author. */

/*     Algorithm 7.2 (Composite Simpson Rule). */
/*     Section 7.2, Composite Trapezoidal and Simpson's Rule, Page 365 */

/*     comment added by Kongbin Kang */
/*     F: integrand function */
/*     A: lower integration limit */
/*     B: higher integration limit */
/*     M: number of intervals. Notice, the subintervals used is 2M */
/*     Srule: output parameter to store simpson rule result */
/*<       SUBROUTINE SIMPRU(F,A,B,M,Srule) >*/
/* Subroutine */ int simpru_(doublereal (*f)(doublereal*),
                             doublereal *a, doublereal *b, integer *m,
                             doublereal *srule)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    doublereal h__;
    integer k;
    doublereal x, sum, sumodd, sumeven;

/*<       INTEGER K,M >*/
/*<       DOUBLE PRECISION A,B,H,Sum,SumEven,SumOdd,Srule,X >*/
/*<       EXTERNAL F >*/
/*<       H=(B-A)/(2*M) >*/
    h__ = (*b - *a) / (*m << 1);
/*<       SumEven=0 >*/
    sumeven = 0.;
/*<       DO K=1,(M-1) >*/
    i__1 = *m - 1;
    for (k = 1; k <= i__1; ++k) {
/*<         X=A+H*2*K >*/
        x = *a + h__ * 2 * k;
/*<         SumEven=SumEven+F(X) >*/
        sumeven += (*f)(&x);
/*<       ENDDO >*/
    }
/*<       SumOdd=0 >*/
    sumodd = 0.;
/*<       DO K=1,M >*/
    i__1 = *m;
    for (k = 1; k <= i__1; ++k) {
/*<         X=A+H*(2*K-1) >*/
        x = *a + h__ * ((k << 1) - 1);
/*<         SumOdd=SumOdd+F(X) >*/
        sumodd += (*f)(&x);
/*<       ENDDO >*/
    }
/*<       Sum=H*(F(A)+F(B)+2*SumEven+4*SumOdd)/3 >*/
    sum = h__ * ((*f)(a) + (*f)(b) + sumeven * 2 + sumodd * 4) / 3;
/*<       Srule=Sum >*/
    *srule = sum;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* simpru_ */

/*<       SUBROUTINE XSIMPRU(F,A,B,M,Srule) >*/
/* Subroutine */ int xsimpru_(doublereal (*f)(doublereal*),
                              doublereal *a, doublereal *b, integer *
                              m, doublereal *srule)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    doublereal h__;
    integer k;
    doublereal x, sum, sumodd, sumeven;

/*     This subroutine uses labeled DO loop(s). */
/*<       INTEGER K,M >*/
/*<       DOUBLE PRECISION A,B,H,Sum,SumEven,SumOdd,Srule,X >*/
/*<       EXTERNAL F >*/
/*<       H=(B-A)/(2*M) >*/
    h__ = (*b - *a) / (*m << 1);
/*<       SumEven=0 >*/
    sumeven = 0.;
/*<       DO 10 K=1,(M-1) >*/
    i__1 = *m - 1;
    for (k = 1; k <= i__1; ++k) {
/*<         X=A+H*2*K >*/
        x = *a + h__ * 2 * k;
/*<         SumEven=SumEven+F(X) >*/
        sumeven += (*f)(&x);
/*< 10    CONTINUE >*/
/* L10: */
    }
/*<       SumOdd=0 >*/
    sumodd = 0.;
/*<       DO 20 K=1,M >*/
    i__1 = *m;
    for (k = 1; k <= i__1; ++k) {
/*<         X=A+H*(2*K-1) >*/
        x = *a + h__ * ((k << 1) - 1);
/*<         SumOdd=SumOdd+F(X) >*/
        sumodd += (*f)(&x);
/*< 20    CONTINUE >*/
/* L20: */
    }
/*<       Sum=H*(F(A)+F(B)+2*SumEven+4*SumOdd)/3 >*/
    sum = h__ * ((*f)(a) + (*f)(b) + sumeven * 2 + sumodd * 4) / 3;
/*<       Srule=Sum >*/
    *srule = sum;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* xsimpru_ */

#ifdef __cplusplus
        }
#endif
