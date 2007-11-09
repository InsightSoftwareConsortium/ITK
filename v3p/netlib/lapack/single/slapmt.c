/* lapack/single/slapmt.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE SLAPMT( FORWRD, M, N, X, LDX, K ) >*/
/* Subroutine */ int slapmt_(logical *forwrd, integer *m, integer *n, real *x,
         integer *ldx, integer *k)
{
    /* System generated locals */
    integer x_dim1, x_offset, i__1, i__2;

    /* Local variables */
    integer i__, j, ii, in;
    real temp;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     March 31, 1993 */

/*     .. Scalar Arguments .. */
/*<       LOGICAL            FORWRD >*/
/*<       INTEGER            LDX, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       INTEGER            K( * ) >*/
/*<       REAL               X( LDX, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  SLAPMT rearranges the columns of the M by N matrix X as specified */
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

/*  X       (input/output) REAL array, dimension (LDX,N) */
/*          On entry, the M by N matrix X. */
/*          On exit, X contains the permuted matrix X. */

/*  LDX     (input) INTEGER */
/*          The leading dimension of the array X, LDX >= MAX(1,M). */

/*  K       (input) INTEGER array, dimension (N) */
/*          On entry, K contains the permutation vector. */

/*  ===================================================================== */

/*     .. Local Scalars .. */
/*<       INTEGER            I, II, J, IN >*/
/*<       REAL               TEMP >*/
/*     .. */
/*     .. Executable Statements .. */

/*<    >*/
    /* Parameter adjustments */
    x_dim1 = *ldx;
    x_offset = 1 + x_dim1;
    x -= x_offset;
    --k;

    /* Function Body */
    if (*n <= 1) {
        return 0;
    }

/*<       DO 10 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          K( I ) = -K( I ) >*/
        k[i__] = -k[i__];
/*<    10 CONTINUE >*/
/* L10: */
    }

/*<       IF( FORWRD ) THEN >*/
    if (*forwrd) {

/*        Forward permutation */

/*<          DO 60 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {

/*<    >*/
            if (k[i__] > 0) {
                goto L40;
            }

/*<             J = I >*/
            j = i__;
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

/*<    60    CONTINUE >*/
/* L60: */
            ;
        }

/*<       ELSE >*/
    } else {

/*        Backward permutation */

/*<          DO 110 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {

/*<    >*/
            if (k[i__] > 0) {
                goto L100;
            }

/*<             K( I ) = -K( I ) >*/
            k[i__] = -k[i__];
/*<             J = K( I ) >*/
            j = k[i__];
/*<    80       CONTINUE >*/
L80:
/*<    >*/
            if (j == i__) {
                goto L100;
            }

/*<             DO 90 II = 1, M >*/
            i__2 = *m;
            for (ii = 1; ii <= i__2; ++ii) {
/*<                TEMP = X( II, I ) >*/
                temp = x[ii + i__ * x_dim1];
/*<                X( II, I ) = X( II, J ) >*/
                x[ii + i__ * x_dim1] = x[ii + j * x_dim1];
/*<                X( II, J ) = TEMP >*/
                x[ii + j * x_dim1] = temp;
/*<    90       CONTINUE >*/
/* L90: */
            }

/*<             K( J ) = -K( J ) >*/
            k[j] = -k[j];
/*<             J = K( J ) >*/
            j = k[j];
/*<             GO TO 80 >*/
            goto L80;

/*<   100       CONTINUE >*/
L100:
/*<   110    CONTINUE >*/
/* L110: */
            ;
        }

/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of SLAPMT */

/*<       END >*/
} /* slapmt_ */

#ifdef __cplusplus
        }
#endif
