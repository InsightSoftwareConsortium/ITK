/* lapack/single/slapll.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE SLAPLL( N, X, INCX, Y, INCY, SSMIN ) >*/
/* Subroutine */ int slapll_(integer *n, real *x, integer *incx, real *y,
        integer *incy, real *ssmin)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    real c__, a11, a12, a22, tau;
    extern doublereal sdot_(integer *, real *, integer *, real *, integer *);
    extern /* Subroutine */ int slas2_(real *, real *, real *, real *, real *)
            ;
    real ssmax;
    extern /* Subroutine */ int saxpy_(integer *, real *, real *, integer *,
            real *, integer *), slarfg_(integer *, real *, real *, integer *,
            real *);


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     March 31, 1993 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INCX, INCY, N >*/
/*<       REAL               SSMIN >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       REAL               X( * ), Y( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  Given two column vectors X and Y, let */

/*                       A = ( X Y ). */

/*  The subroutine first computes the QR factorization of A = Q*R, */
/*  and then computes the SVD of the 2-by-2 upper triangular matrix R. */
/*  The smaller singular value of R is returned in SSMIN, which is used */
/*  as the measurement of the linear dependency of the vectors X and Y. */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The length of the vectors X and Y. */

/*  X       (input/output) REAL array, */
/*                         dimension (1+(N-1)*INCX) */
/*          On entry, X contains the N-vector X. */
/*          On exit, X is overwritten. */

/*  INCX    (input) INTEGER */
/*          The increment between successive elements of X. INCX > 0. */

/*  Y       (input/output) REAL array, */
/*                         dimension (1+(N-1)*INCY) */
/*          On entry, Y contains the N-vector Y. */
/*          On exit, Y is overwritten. */

/*  INCY    (input) INTEGER */
/*          The increment between successive elements of Y. INCY > 0. */

/*  SSMIN   (output) REAL */
/*          The smallest singular value of the N-by-2 matrix A = ( X Y ). */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       REAL               ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       REAL               A11, A12, A22, C, SSMAX, TAU >*/
/*     .. */
/*     .. External Functions .. */
/*<       REAL               SDOT >*/
/*<       EXTERNAL           SDOT >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           SAXPY, SLARFG, SLAS2 >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Quick return if possible */

/*<       IF( N.LE.1 ) THEN >*/
    /* Parameter adjustments */
    --y;
    --x;

    /* Function Body */
    if (*n <= 1) {
/*<          SSMIN = ZERO >*/
        *ssmin = (float)0.;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Compute the QR factorization of the N-by-2 matrix ( X Y ) */

/*<       CALL SLARFG( N, X( 1 ), X( 1+INCX ), INCX, TAU ) >*/
    slarfg_(n, &x[1], &x[*incx + 1], incx, &tau);
/*<       A11 = X( 1 ) >*/
    a11 = x[1];
/*<       X( 1 ) = ONE >*/
    x[1] = (float)1.;

/*<       C = -TAU*SDOT( N, X, INCX, Y, INCY ) >*/
    c__ = -tau * sdot_(n, &x[1], incx, &y[1], incy);
/*<       CALL SAXPY( N, C, X, INCX, Y, INCY ) >*/
    saxpy_(n, &c__, &x[1], incx, &y[1], incy);

/*<       CALL SLARFG( N-1, Y( 1+INCY ), Y( 1+2*INCY ), INCY, TAU ) >*/
    i__1 = *n - 1;
    slarfg_(&i__1, &y[*incy + 1], &y[(*incy << 1) + 1], incy, &tau);

/*<       A12 = Y( 1 ) >*/
    a12 = y[1];
/*<       A22 = Y( 1+INCY ) >*/
    a22 = y[*incy + 1];

/*     Compute the SVD of 2-by-2 Upper triangular matrix. */

/*<       CALL SLAS2( A11, A12, A22, SSMIN, SSMAX ) >*/
    slas2_(&a11, &a12, &a22, ssmin, &ssmax);

/*<       RETURN >*/
    return 0;

/*     End of SLAPLL */

/*<       END >*/
} /* slapll_ */

#ifdef __cplusplus
        }
#endif
