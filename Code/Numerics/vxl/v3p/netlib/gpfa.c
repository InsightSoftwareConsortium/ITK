/* gpfa.f -- translated by f2c (version 19951025).
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

/*        SUBROUTINE 'GPFA' */
/*        SELF-SORTING IN-PLACE GENERALIZED PRIME FACTOR (COMPLEX) FFT */

/*        *** THIS IS THE ALL-FORTRAN VERSION *** */
/*            ------------------------------- */

/*        CALL GPFA(A,B,TRIGS,INC,JUMP,N,LOT,ISIGN) */

/*        A IS FIRST REAL INPUT/OUTPUT VECTOR */
/*        B IS FIRST IMAGINARY INPUT/OUTPUT VECTOR */
/*        TRIGS IS A TABLE OF TWIDDLE FACTORS, PRECALCULATED */
/*              BY CALLING SUBROUTINE 'SETGPFA' */
/*        INC IS THE INCREMENT WITHIN EACH DATA VECTOR */
/*        JUMP IS THE INCREMENT BETWEEN DATA VECTORS */
/*        N IS THE LENGTH OF THE TRANSFORMS: */
/*          ----------------------------------- */
/*            N = (2**IP) * (3**IQ) * (5**IR) */
/*          ----------------------------------- */
/*        LOT IS THE NUMBER OF TRANSFORMS */
/*        ISIGN = +1 FOR FORWARD TRANSFORM */
/*              = -1 FOR INVERSE TRANSFORM */

/*        WRITTEN BY CLIVE TEMPERTON */
/*        RECHERCHE EN PREVISION NUMERIQUE */
/*        ATMOSPHERIC ENVIRONMENT SERVICE, CANADA */

/* ---------------------------------------------------------------------- */

/*        DEFINITION OF TRANSFORM */
/*        ----------------------- */

/*        X(J) = SUM(K=0,...,N-1)(C(K)*EXP(ISIGN*2*I*J*K*PI/N)) */

/* --------------------------------------------------------------------- */

/*        FOR A MATHEMATICAL DEVELOPMENT OF THE ALGORITHM USED, */
/*        SEE: */

/*        C TEMPERTON : "A GENERALIZED PRIME FACTOR FFT ALGORITHM */
/*          FOR ANY N = (2**P)(3**Q)(5**R)", */
/*          SIAM J. SCI. STAT. COMP., MAY 1992. */

/* ---------------------------------------------------------------------- */

/*<       SUBROUTINE GPFA(A,B,TRIGS,INC,JUMP,N,LOT,ISIGN,NJ,INFO) >*/
/* Subroutine */ int gpfa_(real *a, real *b, real *trigs, integer *inc, 
	integer *jump, integer *n, integer *lot, integer *isign, integer *nj, 
	integer *info)
{
    /* Builtin functions */
    integer pow_ii(integer *, integer *);

    /* Local variables */
    static integer i__;
    extern /* Subroutine */ int gpfa2f_(real *, real *, real *, integer *, 
	    integer *, integer *, integer *, integer *, integer *), gpfa3f_(
	    real *, real *, real *, integer *, integer *, integer *, integer *
	    , integer *, integer *), gpfa5f_(real *, real *, real *, integer *
	    , integer *, integer *, integer *, integer *, integer *);
    static integer ip, iq, ir;


/*<       DIMENSION A(*), B(*), TRIGS(*) >*/
/*<       DIMENSION NJ(3) >*/
/*<       IP = NJ(1) >*/
    /* Parameter adjustments */
    --nj;
    --trigs;
    --b;
    --a;

    /* Function Body */
    ip = nj[1];
/*<       IQ = NJ(2) >*/
    iq = nj[2];
/*<       IR = NJ(3) >*/
    ir = nj[3];

/*     COMPUTE THE TRANSFORM */
/*     --------------------- */
/*<       I = 1 >*/
    i__ = 1;
/*<       IF (IP.GT.0) THEN >*/
    if (ip > 0) {
/*<          CALL GPFA2F(A,B,TRIGS,INC,JUMP,N,IP,LOT,ISIGN) >*/
	gpfa2f_(&a[1], &b[1], &trigs[1], inc, jump, n, &ip, lot, isign);
/*<          I = I + 2 * ( 2**IP) >*/
	i__ += pow_ii(&c__2, &ip) << 1;
/*<       ENDIF >*/
    }
/*<       IF (IQ.GT.0) THEN >*/
    if (iq > 0) {
/*<          CALL GPFA3F(A,B,TRIGS(I),INC,JUMP,N,IQ,LOT,ISIGN) >*/
	gpfa3f_(&a[1], &b[1], &trigs[i__], inc, jump, n, &iq, lot, isign);
/*<          I = I + 2 * (3**IQ) >*/
	i__ += pow_ii(&c__3, &iq) << 1;
/*<       ENDIF >*/
    }
/*<       IF (IR.GT.0) THEN >*/
    if (ir > 0) {
/*<          CALL GPFA5F(A,B,TRIGS(I),INC,JUMP,N,IR,LOT,ISIGN) >*/
	gpfa5f_(&a[1], &b[1], &trigs[i__], inc, jump, n, &ir, lot, isign);
/*<       ENDIF >*/
    }

/*<       INFO = 0 >*/
    *info = 0;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* gpfa_ */

#ifdef __cplusplus
	}
#endif
