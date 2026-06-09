/* lapack/double/dladiv.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE DLADIV( A, B, C, D, P, Q ) >*/
/* Subroutine */ int dladiv_(doublereal *a, doublereal *b, doublereal *c__,
        doublereal *d__, doublereal *p, doublereal *q)
{
    doublereal e, f;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*     .. Scalar Arguments .. */
/*<       DOUBLE PRECISION   A, B, C, D, P, Q >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLADIV performs complex division in  real arithmetic */

/*                        a + i*b */
/*             p + i*q = --------- */
/*                        c + i*d */

/*  The algorithm is due to Robert L. Smith and can be found */
/*  in D. Knuth, The art of Computer Programming, Vol.2, p.195 */

/*  Arguments */
/*  ========= */

/*  A       (input) DOUBLE PRECISION */
/*  B       (input) DOUBLE PRECISION */
/*  C       (input) DOUBLE PRECISION */
/*  D       (input) DOUBLE PRECISION */
/*          The scalars a, b, c, and d in the above expression. */

/*  P       (output) DOUBLE PRECISION */
/*  Q       (output) DOUBLE PRECISION */
/*          The scalars p and q in the above expression. */

/*  ===================================================================== */

/*     .. Local Scalars .. */
/*<       DOUBLE PRECISION   E, F >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       IF( ABS( D ).LT.ABS( C ) ) THEN >*/
    if (abs(*d__) < abs(*c__)) {
/*<          E = D / C >*/
        e = *d__ / *c__;
/*<          F = C + D*E >*/
        f = *c__ + *d__ * e;
/*<          P = ( A+B*E ) / F >*/
        *p = (*a + *b * e) / f;
/*<          Q = ( B-A*E ) / F >*/
        *q = (*b - *a * e) / f;
/*<       ELSE >*/
    } else {
/*<          E = C / D >*/
        e = *c__ / *d__;
/*<          F = D + C*E >*/
        f = *d__ + *c__ * e;
/*<          P = ( B+A*E ) / F >*/
        *p = (*b + *a * e) / f;
/*<          Q = ( -A+B*E ) / F >*/
        *q = (-(*a) + *b * e) / f;
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of DLADIV */

/*<       END >*/
} /* dladiv_ */

#ifdef __cplusplus
        }
#endif
