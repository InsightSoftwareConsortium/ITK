/* dgpfa.f -- translated by f2c (version 19951025).
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

/*     This is the version to be translated into C++ */
/*     using f2c with the following parameters: */
/*     f2c -c -C++ -P -r8 */
/*     where -c == include fortran code as comments */
/*           -C++ == gen. C++ code */
/*           -P == gen prototypes */
/*           -r8 == gen double (instead of float) */

/*        CALL GPFA(A,B,TRIGS,INC,JUMP,N,LOT,ISIGN,NIPQ,INFO) */

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
/*     NIPQ is an array containing the number of factors (for */
/*     power of 2,3 and 5 */
/*     INFO is set to -1 if there is a problem, 0 otherwise */

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

/*<       SUBROUTINE DGPFA(A,B,TRIGS,INC,JUMP,N,LOT,ISIGN,NPQR,INFO) >*/
/* Subroutine */ int dgpfa_(doublereal *a, doublereal *b, doublereal *trigs, 
	integer *inc, integer *jump, integer *n, integer *lot, integer *isign,
	 integer *npqr, integer *info)
{
    /* Builtin functions */
    integer pow_ii(integer *, integer *);

    /* Local variables */
    static integer i__, ip, iq, ir;
    extern /* Subroutine */ int dgpfa2f_(doublereal *, doublereal *, 
	    doublereal *, integer *, integer *, integer *, integer *, integer 
	    *, integer *), dgpfa3f_(doublereal *, doublereal *, doublereal *, 
	    integer *, integer *, integer *, integer *, integer *, integer *),
	     dgpfa5f_(doublereal *, doublereal *, doublereal *, integer *, 
	    integer *, integer *, integer *, integer *, integer *);


/*<       DIMENSION A(*), B(*), TRIGS(*) >*/
/*<       DIMENSION NPQR(3) >*/
/*<       IP = NPQR(1) >*/
    /* Parameter adjustments */
    --npqr;
    --trigs;
    --b;
    --a;

    /* Function Body */
    ip = npqr[1];
/*<       IQ = NPQR(2) >*/
    iq = npqr[2];
/*<       IR = NPQR(3) >*/
    ir = npqr[3];

/*     COMPUTE THE TRANSFORM */
/*     --------------------- */
/*     IMPORTANT: call the *double* versions (and not the floats) */
/*     i.e. calls to *D*gpfaxf instead of gpfaxf */
/*<       I = 1 >*/
    i__ = 1;
/*<       IF (IP.GT.0) THEN >*/
    if (ip > 0) {
/*<          CALL DGPFA2F(A,B,TRIGS,INC,JUMP,N,IP,LOT,ISIGN) >*/
	dgpfa2f_(&a[1], &b[1], &trigs[1], inc, jump, n, &ip, lot, isign);
/*<          I = I + 2 * ( 2**IP) >*/
	i__ += pow_ii(&c__2, &ip) << 1;
/*<       ENDIF >*/
    }
/*<       IF (IQ.GT.0) THEN >*/
    if (iq > 0) {
/*<          CALL DGPFA3F(A,B,TRIGS(I),INC,JUMP,N,IQ,LOT,ISIGN) >*/
	dgpfa3f_(&a[1], &b[1], &trigs[i__], inc, jump, n, &iq, lot, isign);
/*<          I = I + 2 * (3**IQ) >*/
	i__ += pow_ii(&c__3, &iq) << 1;
/*<       ENDIF >*/
    }
/*<       IF (IR.GT.0) THEN >*/
    if (ir > 0) {
/*<          CALL DGPFA5F(A,B,TRIGS(I),INC,JUMP,N,IR,LOT,ISIGN) >*/
	dgpfa5f_(&a[1], &b[1], &trigs[i__], inc, jump, n, &ir, lot, isign);
/*<       ENDIF >*/
    }

/*<       INFO = 0 >*/
    *info = 0;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* dgpfa_ */

#ifdef __cplusplus
	}
#endif
