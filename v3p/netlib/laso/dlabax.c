/* laso/dlabax.f -- translated by f2c (version 20050501).
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

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;


/* *********************************************************************** */

/*<       SUBROUTINE DLABAX(N, NBAND, A, X, Y) >*/
/* Subroutine */ int dlabax_(integer *n, integer *nband, doublereal *a,
        doublereal *x, doublereal *y)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    integer i__, k, l, m;
    doublereal zero[1];
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *,
            doublereal *, integer *);


/*  THIS SUBROUTINE SETS Y = A*X */
/*  WHERE X AND Y ARE VECTORS OF LENGTH N */
/*  AND A IS AN  N X NBAND  SYMMETRIC BAND MATRIX */

/*  FORMAL PARAMETERS */

/*<       INTEGER N, NBAND >*/
/*<       DOUBLE PRECISION A(NBAND,1), X(1), Y(1) >*/

/*  LOCAL VARIABLES */

/*<       INTEGER I, K, L, M >*/
/*<       DOUBLE PRECISION ZERO(1) >*/

/*  FUNCTIONS CALLED */

/*<       INTEGER MIN0 >*/

/*  SUBROUTINES CALLED */

/*     DCOPY */

/*<       ZERO(1) = 0.0D0 >*/
    /* Parameter adjustments */
    a_dim1 = *nband;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --x;
    --y;

    /* Function Body */
    zero[0] = 0.;
/*<       CALL DCOPY(N, ZERO, 0, Y, 1) >*/
    dcopy_(n, zero, &c__0, &y[1], &c__1);
/*<       DO 20 K = 1, N >*/
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
/*<          Y(K) = Y(K) + A(1,K)*X(K) >*/
        y[k] += a[k * a_dim1 + 1] * x[k];
/*<          M = MIN0(N-K+1, NBAND) >*/
/* Computing MIN */
        i__2 = *n - k + 1;
        m = min(i__2,*nband);
/*<          IF(M .LT. 2) GO TO 20 >*/
        if (m < 2) {
            goto L20;
        }
/*<          DO 10 I = 2, M >*/
        i__2 = m;
        for (i__ = 2; i__ <= i__2; ++i__) {
/*<             L = K + I - 1 >*/
            l = k + i__ - 1;
/*<             Y(L) = Y(L) + A(I,K)*X(K) >*/
            y[l] += a[i__ + k * a_dim1] * x[k];
/*<             Y(K) = Y(K) + A(I,K)*X(L) >*/
            y[k] += a[i__ + k * a_dim1] * x[l];
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<    20 CONTINUE >*/
L20:
        ;
    }
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* dlabax_ */

#ifdef __cplusplus
        }
#endif
