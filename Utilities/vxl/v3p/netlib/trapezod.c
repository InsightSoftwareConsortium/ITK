/* trapezod.f -- translated by f2c (version 20020621). */
#include "f2c.h"

/*     NUMERICAL METHODS: FORTRAN Programs, (c) John H. Mathews 1994 */
/*     To accompany the text: */
/*     NUMERICAL METHODS for Mathematics, Science and Engineering, 2nd Ed, 1992 */
/*     Prentice Hall, Englewood Cliffs, New Jersey, 07632, U.S.A. */
/*     This free software is complements of the author. */

/*     Algorithm 7.1 (Composite Trapezoidal Rule). */
/*     Section 7.2, Composite Trapezoidal and Simpson's Rule, Page 365 */

/* Subroutine */
int trapru_(E_fp f, doublereal *a, doublereal *b, integer *m, doublereal *trule)
{
    /* Local variables */
    static doublereal h__;
    static integer k;
    static doublereal x;
    static doublereal sum;

    h__ = (*b - *a) / *m;
    sum = 0.f;
    for (k = 1; k < *m; ++k) {
      x = *a + h__ * k;
      sum += (*f)(&x);
    }
    sum = h__ * ((*f)(a) + (*f)(b) + sum * 2) / 2;
    *trule = sum;
    return 0;
} /* trapru_ */

/* Subroutine */
int xtrapru_(E_fp f, doublereal *a, doublereal *b, integer *m, doublereal *trule)
{
    /* Local variables */
    static doublereal h__;
    static integer k;
    static doublereal x, sum;

/*     This subroutine uses labeled DO loop(s). */
    h__ = (*b - *a) / *m;
    sum = 0.f;
    for (k = 1; k < *m; ++k) {
      x = *a + h__ * k;
      sum += (*f)(&x);
    }
    sum = h__ * ((*f)(a) + (*f)(b) + sum * 2) / 2;
    *trule = sum;
    return 0;
} /* xtrapru_ */
