/* mathews/trapezod.f -- translated by f2c (version 20050501).
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

/*     Algorithm 7.1 (Composite Trapezoidal Rule). */
/*     Section 7.2, Composite Trapezoidal and Simpson's Rule, Page 365 */

/*<       SUBROUTINE TRAPRU(F,A,B,M,Trule) >*/
/* Subroutine */ int trapru_(
  v3p_netlib_doublereal (*f)(v3p_netlib_doublereal*),
  doublereal *a, doublereal *b, integer *m, doublereal *trule
  )
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    doublereal h__;
    integer k;
    doublereal x, sum;

/*<       INTEGER K,M >*/
/*<       DOUBLE PRECISION A,B,H,Sum,Trule,X >*/
/*<       EXTERNAL F >*/
/*<       H=(B-A)/M >*/
    h__ = (*b - *a) / *m;
/*<       Sum=0 >*/
    sum = 0.;
/*<       DO K=1,M-1 >*/
    i__1 = *m - 1;
    for (k = 1; k <= i__1; ++k) {
/*<         X=A+H*K >*/
        x = *a + h__ * k;
/*<         Sum=Sum+F(X) >*/
        sum += (*f)(&x);
/*<       ENDDO >*/
    }
/*<       Sum=H*(F(A)+F(B)+2*Sum)/2 >*/
    sum = h__ * ((*f)(a) + (*f)(b) + sum * 2) / 2;
/*<       Trule=Sum >*/
    *trule = sum;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* trapru_ */

/*<       SUBROUTINE XTRAPRU(F,A,B,M,Trule) >*/
/* Subroutine */ int xtrapru_(
  v3p_netlib_doublereal (*f)(v3p_netlib_doublereal*),
  doublereal *a, doublereal *b, integer * m, doublereal *trule
  )
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    doublereal h__;
    integer k;
    doublereal x, sum;

/*     This subroutine uses labeled DO loop(s). */
/*<       INTEGER K,M >*/
/*<       DOUBLE PRECISION A,B,H,Sum,Trule,X >*/
/*<       EXTERNAL F >*/
/*<       H=(B-A)/M >*/
    h__ = (*b - *a) / *m;
/*<       Sum=0 >*/
    sum = 0.;
/*<       DO 10 K=1,M-1 >*/
    i__1 = *m - 1;
    for (k = 1; k <= i__1; ++k) {
/*<         X=A+H*K >*/
        x = *a + h__ * k;
/*<         Sum=Sum+F(X) >*/
        sum += (*f)(&x);
/*< 10    CONTINUE >*/
/* L10: */
    }
/*<       Sum=H*(F(A)+F(B)+2*Sum)/2 >*/
    sum = h__ * ((*f)(a) + (*f)(b) + sum * 2) / 2;
/*<       Trule=Sum >*/
    *trule = sum;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* xtrapru_ */

#ifdef __cplusplus
        }
#endif
