/* laso/dvsort.f -- translated by f2c (version 20050501).
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

static integer c__1 = 1;


/* ------------------------------------------------------------------- */

/*<       SUBROUTINE DVSORT(NUM, VAL, RES, IFLAG, V, NMVEC, N, VEC) >*/
/* Subroutine */ int dvsort_(integer *num, doublereal *val, doublereal *res,
        integer *iflag, doublereal *v, integer *nmvec, integer *n, doublereal
        *vec)
{
    /* System generated locals */
    integer vec_dim1, vec_offset, i__1, i__2;

    /* Local variables */
    integer i__, k, m;
    doublereal temp;
    extern /* Subroutine */ int dswap_(integer *, doublereal *, integer *,
            doublereal *, integer *);

/*<       INTEGER NUM, IFLAG, NMVEC, N  >*/
/*<       DOUBLE PRECISION VAL(1), RES(1), V(1), VEC(NMVEC,1) >*/

/*  THIS SUBROUTINE SORTS THE EIGENVALUES (VAL) IN ASCENDING ORDER */
/*  WHILE CONCURRENTLY SWAPPING THE RESIDUALS AND VECTORS. */
/*<       INTEGER I, K, M >*/
/*<       DOUBLE PRECISION TEMP >*/
/*<       IF(NUM .LE. 1) RETURN >*/
    /* Parameter adjustments */
    --val;
    --res;
    --v;
    vec_dim1 = *nmvec;
    vec_offset = 1 + vec_dim1;
    vec -= vec_offset;

    /* Function Body */
    if (*num <= 1) {
        return 0;
    }
/*<       DO 20 I = 2, NUM >*/
    i__1 = *num;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*<          M = NUM - I + 1 >*/
        m = *num - i__ + 1;
/*<          DO 10 K = 1, M >*/
        i__2 = m;
        for (k = 1; k <= i__2; ++k) {
/*<             IF(VAL(K) .LE. VAL(K+1)) GO TO 10 >*/
            if (val[k] <= val[k + 1]) {
                goto L10;
            }
/*<             TEMP = VAL(K) >*/
            temp = val[k];
/*<             VAL(K) = VAL(K+1) >*/
            val[k] = val[k + 1];
/*<             VAL(K+1) = TEMP >*/
            val[k + 1] = temp;
/*<             TEMP = RES(K) >*/
            temp = res[k];
/*<             RES(K) = RES(K+1) >*/
            res[k] = res[k + 1];
/*<             RES(K+1) = TEMP >*/
            res[k + 1] = temp;
/*<             CALL DSWAP(N, VEC(1,K), 1, VEC(1,K+1), 1) >*/
            dswap_(n, &vec[k * vec_dim1 + 1], &c__1, &vec[(k + 1) * vec_dim1
                    + 1], &c__1);
/*<             IF(IFLAG .EQ. 0) GO TO 10 >*/
            if (*iflag == 0) {
                goto L10;
            }
/*<             TEMP = V(K) >*/
            temp = v[k];
/*<             V(K) = V(K+1) >*/
            v[k] = v[k + 1];
/*<             V(K+1) = TEMP >*/
            v[k + 1] = temp;
/*<    10    CONTINUE >*/
L10:
            ;
        }
/*<    20 CONTINUE >*/
/* L20: */
    }
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* dvsort_ */

#ifdef __cplusplus
        }
#endif
