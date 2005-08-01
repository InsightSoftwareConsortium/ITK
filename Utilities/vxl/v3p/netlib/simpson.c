/* simpson.f -- translated by f2c (version 20020621). */
#include "f2c.h"

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
/* Subroutine */
int simpru_(E_fp f, doublereal *a, doublereal *b, integer *m, doublereal *srule)
{
    /* Local variables */
    static doublereal h__;
    static integer k;
    static doublereal x, sum, sumodd, sumeven;

    h__ = (*b - *a) / (*m << 1);
    sumeven = 0.f;
    for (k = 1; k < *m; ++k) {
      x = *a + h__ * (k << 1);
      sumeven += (*f)(&x);
    }
    sumodd = 0.f;
    for (k = 0; k < *m; ++k) {
      x = *a + h__ * ((k << 1) + 1);
      sumodd += (*f)(&x);
    }
    sum = h__ * ((*f)(a) + (*f)(b) + sumeven * 2 + sumodd * 4) / 3;
    *srule = sum;
    return 0;
} /* simpru_ */

/* Subroutine */
int xsimpru_(E_fp f, doublereal *a, doublereal *b, integer *m, doublereal *srule)
{
    /* Local variables */
    static doublereal h__;
    static integer k;
    static doublereal x, sum, sumodd, sumeven;

/*     This subroutine uses labeled DO loop(s). */
    h__ = (*b - *a) / (*m << 1);
    sumeven = 0.f;
    for (k = 1; k < *m; ++k) {
      x = *a + h__ * (k << 1);
      sumeven += (*f)(&x);
    }
    sumodd = 0.f;
    for (k = 0; k < *m; ++k) {
      x = *a + h__ * ((k << 1) + 1);
      sumodd += (*f)(&x);
    }
    sum = h__ * ((*f)(a) + (*f)(b) + sumeven * 2 + sumodd * 4) / 3;
    *srule = sum;
    return 0;
} /* xsimpru_ */
