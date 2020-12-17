/* laso/dlager.f -- translated by f2c (version 20050501).
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


/* *********************************************************************** */

/*<       SUBROUTINE DLAGER(N, NBAND, NSTART, A, TMIN, TMAX) >*/
/* Subroutine */ int dlager_(integer *n, integer *nband, integer *nstart,
        doublereal *a, doublereal *tmin, doublereal *tmax)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    doublereal d__1, d__2;

    /* Local variables */
    integer i__, k, l, m;
    doublereal temp;


/*  THIS SUBROUTINE COMPUTES BOUNDS ON THE SPECTRUM OF A BY */
/*  EXAMINING THE GERSCHGORIN CIRCLES. ONLY THE NEWLY CREATED */
/*  CIRCLES ARE EXAMINED */

/*  FORMAL PARAMETERS */

/*<       INTEGER N, NBAND, NSTART >*/
/*<       DOUBLE PRECISION A(NBAND,1), TMIN, TMAX >*/

/*  LOCAL VARIABLES */

/*<       INTEGER I, K, L, M >*/
/*<       DOUBLE PRECISION TEMP >*/

/*  FUNCTIONS CALLED */

/*<       INTEGER MIN0 >*/
/*<       DOUBLE PRECISION DMIN1, DMAX1 >*/

/*<       DO 50 K = NSTART, N >*/
    /* Parameter adjustments */
    a_dim1 = *nband;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (k = *nstart; k <= i__1; ++k) {
/*<          TEMP = 0.0D0 >*/
        temp = 0.;
/*<          DO 10 I = 2, NBAND >*/
        i__2 = *nband;
        for (i__ = 2; i__ <= i__2; ++i__) {
/*<             TEMP = TEMP + DABS(A(I,K)) >*/
            temp += (d__1 = a[i__ + k * a_dim1], abs(d__1));
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<    20    L = MIN0(K,NBAND) >*/
/* L20: */
        l = min(k,*nband);
/*<          IF(L .EQ. 1) GO TO 40 >*/
        if (l == 1) {
            goto L40;
        }
/*<          DO 30 I = 2, L >*/
        i__2 = l;
        for (i__ = 2; i__ <= i__2; ++i__) {
/*<             M = K - I + 1 >*/
            m = k - i__ + 1;
/*<             TEMP = TEMP + DABS(A(I,M)) >*/
            temp += (d__1 = a[i__ + m * a_dim1], abs(d__1));
/*<    30    CONTINUE >*/
/* L30: */
        }
/*<    40    TMIN = DMIN1(TMIN, A(1,K)-TEMP) >*/
L40:
/* Computing MIN */
        d__1 = *tmin, d__2 = a[k * a_dim1 + 1] - temp;
        *tmin = min(d__1,d__2);
/*<          TMAX = DMAX1(TMAX, A(1,K)+TEMP) >*/
/* Computing MAX */
        d__1 = *tmax, d__2 = a[k * a_dim1 + 1] + temp;
        *tmax = max(d__1,d__2);
/*<    50 CONTINUE >*/
/* L50: */
    }
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* dlager_ */

#ifdef __cplusplus
        }
#endif
