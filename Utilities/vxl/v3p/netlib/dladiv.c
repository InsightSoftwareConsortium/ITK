#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001, to allow p/q being equal to a,b,c or d */

/* Subroutine */ void dladiv_(a, b, c, d, p, q)
const doublereal *a, *b, *c, *d;
doublereal *p, *q;
{
    static doublereal e, f, t;

/*  -- LAPACK auxiliary routine (version 2.0) --                          */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,        */
/*     Courant Institute, Argonne National Lab, and Rice University       */
/*     October 31, 1992                                                   */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DLADIV performs complex division in  real arithmetic                  */
/*                                                                        */
/*                        a + i*b                                         */
/*             p + i*q = ---------                                        */
/*                        c + i*d                                         */
/*                                                                        */
/*  The algorithm is due to Robert L. Smith and can be found              */
/*  in D. Knuth, The art of Computer Programming, Vol.2, p.195            */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  A       (input) DOUBLE PRECISION                                      */
/*  B       (input) DOUBLE PRECISION                                      */
/*  C       (input) DOUBLE PRECISION                                      */
/*  D       (input) DOUBLE PRECISION                                      */
/*          The scalars a, b, c, and d in the above expression.           */
/*                                                                        */
/*  P       (output) DOUBLE PRECISION                                     */
/*  Q       (output) DOUBLE PRECISION                                     */
/*          The scalars p and q in the above expression.                  */
/*                                                                        */
/*  ===================================================================== */

    if (abs(*d) < abs(*c)) {
        e = *d / *c;
        f = *c + *d * e;
        t  = (*a + *b * e) / f;
        *q = (*b - *a * e) / f;
        *p = t;
    } else {
        e = *c / *d;
        f = *d + *c * e;
        t  = (*a * e + *b) / f;
        *q = (*b * e - *a) / f;
        *p = t;
    }
} /* dladiv_ */

