/* laso/dmvpc.f -- translated by f2c (version 20050501).
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


/* ------------------------------------------------------------------ */

/*<    >*/
/* Subroutine */ int dmvpc_(integer *nblock, doublereal *bet, integer *maxj,
        integer *j, doublereal *s, integer *number, doublereal *resnrm,
        doublereal *orthcf, doublereal *rv)
{
    /* System generated locals */
    integer bet_dim1, bet_offset, s_dim1, s_offset, i__1, i__2;
    doublereal d__1, d__2, d__3;

    /* Local variables */
    integer i__, k, m;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *,
            integer *), dnrm2_(integer *, doublereal *, integer *);


/*<       INTEGER NBLOCK, MAXJ, J, NUMBER >*/
/*<    >*/

/* THIS SUBROUTINE COMPUTES THE NORM AND THE SMALLEST ELEMENT */
/* (IN ABSOLUTE VALUE) OF THE VECTOR BET*SJI, WHERE SJI */
/* IS AN NBLOCK VECTOR OF THE LAST NBLOCK ELEMENTS OF THE ITH */
/* EIGENVECTOR OF T.  THESE QUANTITIES ARE THE RESIDUAL NORM */
/* AND THE ORTHOGONALITY COEFFICIENT RESPECTIVELY FOR THE */
/* CORRESPONDING RITZ PAIR.  THE ORTHOGONALITY COEFFICIENT IS */
/* NORMALIZED TO ACCOUNT FOR THE LOCAL REORTHOGONALIZATION. */

/*<       INTEGER I, K, M >*/
/*<       DOUBLE PRECISION DDOT, DNRM2, DABS, DMIN1 >*/

/*<       M = J - NBLOCK + 1 >*/
    /* Parameter adjustments */
    bet_dim1 = *nblock;
    bet_offset = 1 + bet_dim1;
    bet -= bet_offset;
    s_dim1 = *maxj;
    s_offset = 1 + s_dim1;
    s -= s_offset;
    --resnrm;
    --orthcf;
    --rv;

    /* Function Body */
    m = *j - *nblock + 1;
/*<       DO 20 I=1,NUMBER >*/
    i__1 = *number;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          DO 10 K=1,NBLOCK >*/
        i__2 = *nblock;
        for (k = 1; k <= i__2; ++k) {
/*<             RV(K) = DDOT(NBLOCK,S(M,I),1,BET(K,1),NBLOCK) >*/
            rv[k] = ddot_(nblock, &s[m + i__ * s_dim1], &c__1, &bet[k +
                    bet_dim1], nblock);
/*<             IF (K.EQ.1) ORTHCF(I) = DABS(RV(K)) >*/
            if (k == 1) {
                orthcf[i__] = (d__1 = rv[k], abs(d__1));
            }
/*<             ORTHCF(I) = DMIN1(ORTHCF(I),DABS(RV(K))) >*/
/* Computing MIN */
            d__2 = orthcf[i__], d__3 = (d__1 = rv[k], abs(d__1));
            orthcf[i__] = min(d__2,d__3);
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<          RESNRM(I) = DNRM2(NBLOCK,RV,1) >*/
        resnrm[i__] = dnrm2_(nblock, &rv[1], &c__1);
/*<    20 CONTINUE >*/
/* L20: */
    }
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* dmvpc_ */

#ifdef __cplusplus
        }
#endif
