/* dlapmt.f -- translated by f2c (version of 4 June 1993  1:43:59).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/*<       SUBROUTINE DLAPMT( FORWRD, M, N, X, LDX, K ) >*/
/* Subroutine */ int dlapmt_(logical *forwrd, integer *m, integer *n, 
	doublereal *x, integer *ldx, integer *k)
{
    /* System generated locals */
    integer x_dim1, x_offset, i__1, i__2;

    /* Local variables */
    static doublereal temp;
    static integer i, j, ii, in;


/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     March 31, 1993 */

/*     .. Scalar Arguments .. */
/*<       LOGICAL            FORWRD >*/
/*<       INTEGER            LDX, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       INTEGER            K( * ) >*/
/*<       DOUBLE PRECISION   X( LDX, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLAPMT rearranges the columns of the M by N matrix X as specified */
/*  by the permutation K(1),K(2),...,K(N) of the integers 1,...,N. */
/*  If FORWRD = .TRUE.,  forward permutation: */

/*       X(*,K(J)) is moved X(*,J) for J = 1,2,...,N. */

/*  If FORWRD = .FALSE., backward permutation: */

/*       X(*,J) is moved to X(*,K(J)) for J = 1,2,...,N. */

/*  Arguments */
/*  ========= */

/*  FORWRD  (input) LOGICAL */
/*          = .TRUE., forward permutation */
/*          = .FALSE., backward permutation */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix X. M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix X. N >= 0. */

/*  X       (input/output) DOUBLE PRECISION array, dimension (LDX,N) */
/*          On entry, the M by N matrix X. */
/*          On exit, X contains the permuted matrix X. */

/*  LDX     (input) INTEGER */
/*          The leading dimension of the array X, LDX >= MAX(1,M). */

/*  K       (input) INTEGER array, dimension (N) */
/*          On entry, K contains the permutation vector. */

/*  ===================================================================== 
*/

/*     .. Local Scalars .. */
/*<       INTEGER            I, II, IN, J >*/
/*<       DOUBLE PRECISION   TEMP >*/
/*     .. */
/*     .. Executable Statements .. */

/*<    >*/
    /* Parameter adjustments */
    --k;
    x_dim1 = *ldx;
    x_offset = x_dim1 + 1;
    x -= x_offset;

    /* Function Body */
    if (*n <= 1) {
	return 0;
    }

/*<       DO 10 I = 1, N >*/
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/*<          K( I ) = -K( I ) >*/
	k[i] = -k[i];
/*<    10 CONTINUE >*/
/* L10: */
    }

/*<       IF( FORWRD ) THEN >*/
    if (*forwrd) {

/*        Forward permutation */

/*<          DO 50 I = 1, N >*/
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {

/*<    >*/
	    if (k[i] > 0) {
		goto L40;
	    }

/*<             J = I >*/
	    j = i;
/*<             K( J ) = -K( J ) >*/
	    k[j] = -k[j];
/*<             IN = K( J ) >*/
	    in = k[j];

/*<    20       CONTINUE >*/
L20:
/*<    >*/
	    if (k[in] > 0) {
		goto L40;
	    }

/*<             DO 30 II = 1, M >*/
	    i__2 = *m;
	    for (ii = 1; ii <= i__2; ++ii) {
/*<                TEMP = X( II, J ) >*/
		temp = x[ii + j * x_dim1];
/*<                X( II, J ) = X( II, IN ) >*/
		x[ii + j * x_dim1] = x[ii + in * x_dim1];
/*<                X( II, IN ) = TEMP >*/
		x[ii + in * x_dim1] = temp;
/*<    30       CONTINUE >*/
/* L30: */
	    }

/*<             K( IN ) = -K( IN ) >*/
	    k[in] = -k[in];
/*<             J = IN >*/
	    j = in;
/*<             IN = K( IN ) >*/
	    in = k[in];
/*<             GO TO 20 >*/
	    goto L20;

/*<    40       CONTINUE >*/
L40:

/*<    50    CONTINUE >*/
/* L50: */
	    ;
	}

/*<       ELSE >*/
    } else {

/*        Backward permutation */

/*<          DO 90 I = 1, N >*/
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {

/*<    >*/
	    if (k[i] > 0) {
		goto L80;
	    }

/*<             K( I ) = -K( I ) >*/
	    k[i] = -k[i];
/*<             J = K( I ) >*/
	    j = k[i];
/*<    60       CONTINUE >*/
L60:
/*<    >*/
	    if (j == i) {
		goto L80;
	    }

/*<             DO 70 II = 1, M >*/
	    i__2 = *m;
	    for (ii = 1; ii <= i__2; ++ii) {
/*<                TEMP = X( II, I ) >*/
		temp = x[ii + i * x_dim1];
/*<                X( II, I ) = X( II, J ) >*/
		x[ii + i * x_dim1] = x[ii + j * x_dim1];
/*<                X( II, J ) = TEMP >*/
		x[ii + j * x_dim1] = temp;
/*<    70       CONTINUE >*/
/* L70: */
	    }

/*<             K( J ) = -K( J ) >*/
	    k[j] = -k[j];
/*<             J = K( J ) >*/
	    j = k[j];
/*<             GO TO 60 >*/
	    goto L60;

/*<    80       CONTINUE >*/
L80:

/*<    90    CONTINUE >*/
/* L90: */
	    ;
	}

/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of DLAPMT */

/*<       END >*/
} /* dlapmt_ */

