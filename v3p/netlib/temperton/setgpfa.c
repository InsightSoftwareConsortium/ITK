/* temperton/setgpfa.f -- translated by f2c (version 20050501).
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

static integer c__2 = 2;
static integer c__3 = 3;
static integer c__5 = 5;

/*        SUBROUTINE 'SETGPFA' */
/*        SETUP ROUTINE FOR SELF-SORTING IN-PLACE */
/*            GENERALIZED PRIME FACTOR (COMPLEX) FFT [GPFA] */

/*        CALL SETGPFA(TRIGS,N,NPQR,INFO) */

/*        INPUT : */
/*        ----- */
/*        N IS THE LENGTH OF THE TRANSFORMS. N MUST BE OF THE FORM: */
/*          ----------------------------------- */
/*            N = (2**IP) * (3**IQ) * (5**IR) */
/*          ----------------------------------- */

/*        OUTPUT: */
/*        ------ */
/*        TRIGS IS A TABLE OF TWIDDLE FACTORS, */
/*          OF LENGTH 2*IPQR (REAL) WORDS, WHERE: */
/*          -------------------------------------- */
/*            IPQR = (2**IP) + (3**IQ) + (5**IR) */
/*          -------------------------------------- */
/*        NPQR = THREE INTEGERS HOLDING IP, IQ, IR */
/*        INFO = SET TO 0 ON SUCCESS AND -1 ON FAILURE */

/*        WRITTEN BY CLIVE TEMPERTON 1990 */

/* ---------------------------------------------------------------------- */

/*<       SUBROUTINE SETGPFA(TRIGS,N,NPQR,INFO) >*/
/* Subroutine */ int setgpfa_(real *trigs, integer *n, integer *npqr, integer
        *info)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer pow_ii(integer *, integer *);
    double asin(doublereal), cos(doublereal), sin(doublereal);

    /* Local variables */
    integer i__, k, kk, ni, nj[3], ll, ip, iq, nn, ir;
    real del;
    integer ifac, kink, irot;
    real angle, twopi;


/*<       REAL TRIGS(*) >*/
/*<       INTEGER N, NPQR(3), INFO >*/
/*<       DIMENSION NJ(3) >*/
/*<       REAL DEL >*/
/*<       REAL ANGLE, TWOPI >*/
/*<       INFO = 0 >*/
    /* Parameter adjustments */
    --npqr;
    --trigs;

    /* Function Body */
    *info = 0;

/*     DECOMPOSE N INTO FACTORS 2,3,5 */
/*     ------------------------------ */
/*<       NN = N >*/
    nn = *n;
/*<       IFAC = 2 >*/
    ifac = 2;

/*<       DO 30 LL = 1 , 3 >*/
    for (ll = 1; ll <= 3; ++ll) {
/*<       KK = 0 >*/
        kk = 0;
/*<    10 CONTINUE >*/
L10:
/*<       IF (MOD(NN,IFAC).NE.0) GO TO 20 >*/
        if (nn % ifac != 0) {
            goto L20;
        }
/*<       KK = KK + 1 >*/
        ++kk;
/*<       NN = NN / IFAC >*/
        nn /= ifac;
/*<       GO TO 10 >*/
        goto L10;
/*<    20 CONTINUE >*/
L20:
/*<       NPQR(LL) = KK >*/
        npqr[ll] = kk;
/*<       IFAC = IFAC + LL >*/
        ifac += ll;
/*<    30 CONTINUE >*/
/* L30: */
    }

/*<       IF (NN.NE.1) THEN >*/
    if (nn != 1) {
/*        WRITE(6,40) N */
/*  40    FORMAT(' *** WARNING!!!',I10,' IS NOT A LEGAL VALUE OF N ***') */
/*<          INFO = -1 >*/
        *info = -1;
/*<          RETURN >*/
        return 0;
/*<       ENDIF >*/
    }

/*<       IP = NPQR(1) >*/
    ip = npqr[1];
/*<       IQ = NPQR(2) >*/
    iq = npqr[2];
/*<       IR = NPQR(3) >*/
    ir = npqr[3];

/*     COMPUTE LIST OF ROTATED TWIDDLE FACTORS */
/*     --------------------------------------- */
/*<       NJ(1) = 2**IP >*/
    nj[0] = pow_ii(&c__2, &ip);
/*<       NJ(2) = 3**IQ >*/
    nj[1] = pow_ii(&c__3, &iq);
/*<       NJ(3) = 5**IR >*/
    nj[2] = pow_ii(&c__5, &ir);

/*<       TWOPI = 4.0 * ASIN(1.0) >*/
    twopi = asin(1.f) * 4.f;
/*<       I = 1 >*/
    i__ = 1;

/*<       DO 60 LL = 1 , 3 >*/
    for (ll = 1; ll <= 3; ++ll) {
/*<       NI = NJ(LL) >*/
        ni = nj[ll - 1];
/*<       IF (NI.EQ.1) GO TO 60 >*/
        if (ni == 1) {
            goto L60;
        }

/*<       DEL = TWOPI / FLOAT(NI) >*/
        del = twopi / (real) ni;
/*<       IROT = N / NI >*/
        irot = *n / ni;
/*<       KINK = MOD(IROT,NI) >*/
        kink = irot % ni;
/*<       KK = 0 >*/
        kk = 0;

/*<       DO 50 K = 1 , NI >*/
        i__1 = ni;
        for (k = 1; k <= i__1; ++k) {
/*<       ANGLE = FLOAT(KK) * DEL >*/
            angle = (real) kk * del;
/*<       TRIGS(I) = COS(ANGLE) >*/
            trigs[i__] = cos(angle);
/*<       TRIGS(I+1) = SIN(ANGLE) >*/
            trigs[i__ + 1] = sin(angle);
/*<       I = I + 2 >*/
            i__ += 2;
/*<       KK = KK + KINK >*/
            kk += kink;
/*<       IF (KK.GT.NI) KK = KK - NI >*/
            if (kk > ni) {
                kk -= ni;
            }
/*<    50 CONTINUE >*/
/* L50: */
        }
/*<    60 CONTINUE >*/
L60:
        ;
    }

/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* setgpfa_ */

#ifdef __cplusplus
        }
#endif
