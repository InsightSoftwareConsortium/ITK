/* setgpfa.f -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__3 = 3;
static integer c__5 = 5;

/*        SUBROUTINE 'SETGPFA' */
/*        SETUP ROUTINE FOR SELF-SORTING IN-PLACE */
/*            GENERALIZED PRIME FACTOR (COMPLEX) FFT [GPFA] */

/*        CALL SETGPFA(TRIGS,N) */

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

/*        WRITTEN BY CLIVE TEMPERTON 1990 */

/* ---------------------------------------------------------------------- */

/*<       SUBROUTINE SETGPFA(TRIGS,N,IRES,INFO) >*/
/* Subroutine */ int dsetgpfa_(doublereal *trigs, integer *n, integer *ires, 
	integer *info)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer pow_ii(integer *, integer *);
    double asin(doublereal), cos(doublereal), sin(doublereal);

    /* Local variables */
    static integer ifac, kink, irot, i__, k;
    static doublereal angle, twopi;
    static integer kk, ni, nj[3], ll, ip, iq, nn, ir;
    static doublereal del;


/*<       DIMENSION TRIGS(*) >*/
/*<       DIMENSION NJ(3) >*/
/*<       DIMENSION IRES(3) >*/

/*     DECOMPOSE N INTO FACTORS 2,3,5 */
/*     ------------------------------ */
/*<       NN = N >*/
    /* Parameter adjustments */
    --ires;
    --trigs;

    /* Function Body */
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
/*<       NJ(LL) = KK >*/
	nj[ll - 1] = kk;
/*<       IFAC = IFAC + LL >*/
	ifac += ll;
/*<    30 CONTINUE >*/
/* L30: */
    }

/*<       IF (NN.NE.1) THEN >*/
    if (nn != 1) {
/*<          INFO = -1 >*/
	*info = -1;
/*<          RETURN >*/
	return 0;
/*<       ENDIF >*/
    }

/*<       IP = NJ(1) >*/
    ip = nj[0];
/*<       IQ = NJ(2) >*/
    iq = nj[1];
/*<       IR = NJ(3) >*/
    ir = nj[2];
/*<       IRES(1) = IP >*/
    ires[1] = ip;
/*<       IRES(2) = IQ >*/
    ires[2] = iq;
/*<       IRES (3) = IR >*/
    ires[3] = ir;

/*     COMPUTE LIST OF ROTATED TWIDDLE FACTORS */
/*     --------------------------------------- */
/*<       NJ(1) = 2**IP >*/
    nj[0] = pow_ii(&c__2, &ip);
/*<       NJ(2) = 3**IQ >*/
    nj[1] = pow_ii(&c__3, &iq);
/*<       NJ(3) = 5**IR >*/
    nj[2] = pow_ii(&c__5, &ir);

/*<       TWOPI = 4.0 * ASIN(1.0) >*/
    twopi = asin(1.) * 4.;
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
	del = twopi / (doublereal) ni;
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
	    angle = (doublereal) kk * del;
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

/*<       INFO = 0 >*/
    *info = 0;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* setgpfa_ */

#ifdef __cplusplus
	}
#endif
