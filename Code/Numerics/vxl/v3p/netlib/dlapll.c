/* dlapll.f -- translated by f2c (version of 4 June 1993  1:43:59).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/*<       SUBROUTINE DLAPLL( N, X, INCX, Y, INCY, SSMIN ) >*/
/* Subroutine */ int dlapll_(integer *n, doublereal *x, integer *incx,
        doublereal *y, integer *incy, doublereal *ssmin)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *,
            integer *);
    extern /* Subroutine */ int dlas2_(doublereal *, doublereal *, doublereal
            *, doublereal *, doublereal *);
    static doublereal c;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *,
            integer *, doublereal *, integer *);
    static doublereal ssmax, a11, a12, a22;
    extern /* Subroutine */ int dlarfg_(integer *, doublereal *, doublereal *,
             integer *, doublereal *);
    static doublereal tau;


/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     March 31, 1993 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INCX, INCY, N >*/
/*<       DOUBLE PRECISION   SSMIN >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   X( * ), Y( * ) >*/
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

/*  X       (input/output) DOUBLE PRECISION array, */
/*                         dimension (1+(N-1)*INCX) */
/*          On entry, X contains the N-vector X. */
/*          On exit, X is overwritten. */

/*  INCX    (input) INTEGER */
/*          The increment between successive elements of X. INCX > 0. */

/*  Y       (input/output) DOUBLE PRECISION array, */
/*                         dimension (1+(N-1)*INCY) */
/*          On entry, Y contains the N-vector Y. */
/*          On exit, Y is overwritten. */

/*  INCY    (input) INTEGER */
/*          The increment between successive elements of Y. INCY > 0. */

/*  SSMIN   (output) DOUBLE PRECISION */
/*          The smallest singular value of the N-by-2 matrix A = ( X Y ).
*/

/*  =====================================================================
*/

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       DOUBLE PRECISION   A11, A12, A22, C, SSMAX, TAU >*/
/*     .. */
/*     .. External Functions .. */
/*<       DOUBLE PRECISION   DDOT >*/
/*<       EXTERNAL           DDOT >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DAXPY, DLARFG, DLAS2 >*/
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
        *ssmin = 0.;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Compute the QR factorization of the N-by-2 matrix ( X Y ) */

/*<       CALL DLARFG( N, X( 1 ), X( 1+INCX ), INCX, TAU ) >*/
    dlarfg_(n, &x[1], &x[*incx + 1], incx, &tau);
/*<       A11 = X( 1 ) >*/
    a11 = x[1];
/*<       X( 1 ) = ONE >*/
    x[1] = 1.;

/*<       C = -TAU*DDOT( N, X, INCX, Y, INCY ) >*/
    c = -tau * ddot_(n, &x[1], incx, &y[1], incy);
/*<       CALL DAXPY( N, C, X, INCX, Y, INCY ) >*/
    daxpy_(n, &c, &x[1], incx, &y[1], incy);

/*<       CALL DLARFG( N-1, Y( 1+INCY ), Y( 1+2*INCY ), INCY, TAU ) >*/
    i__1 = *n - 1;
    dlarfg_(&i__1, &y[*incy + 1], &y[(*incy << 1) + 1], incy, &tau);

/*<       A12 = Y( 1 ) >*/
    a12 = y[1];
/*<       A22 = Y( 1+INCY ) >*/
    a22 = y[*incy + 1];

/*     Compute the SVD of 2-by-2 Upper triangular matrix. */

/*<       CALL DLAS2( A11, A12, A22, SSMIN, SSMAX ) >*/
    dlas2_(&a11, &a12, &a22, ssmin, &ssmax);

/*<       RETURN >*/
    return 0;

/*     End of DLAPLL */

/*<       END >*/
} /* dlapll_ */

