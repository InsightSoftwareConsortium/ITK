/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;
static doublereal c_b15 = 10.;
static real c_b88 = (float)0.;

/*   VERSION 2    DOES NOT USE EISPACK */

/* ------------------------------------------------------------------ */

/* Subroutine */ int snlaso_(op, iovect, n, nval, nfig, nperm, nmval, val, 
	nmvec, vec, nblock, maxop, maxj, work, ind, ierr)
/* Subroutine */ int (*op) (), (*iovect) ();
integer *n, *nval, *nfig, *nperm, *nmval;
real *val;
integer *nmvec;
real *vec;
integer *nblock, *maxop, *maxj;
real *work;
integer *ind, *ierr;
{
    /* System generated locals */
    integer vec_dim1, vec_offset, val_dim1, val_offset, i__1, i__2;
    real r__1;

    /* Local variables */
    static real temp, tarr[1];
    extern doublereal snrm2_();
    static integer i, m, nband;
    static real delta;
    static logical small;
    extern /* Subroutine */ int snwla_();
    static integer i1, i2, i3, i4, i5, i6, i7, i8, i9;
    extern /* Subroutine */ int scopy_();
    static integer i10, i11, i12, i13, nv;
    extern /* Subroutine */ int snppla_();
    static logical raritz;
    extern /* Subroutine */ int sortqr_(), svsort_();
    static real eps;
    static integer nop;



/* AUTHOR/IMPLEMENTER D.S.SCOTT-B.N.PARLETT/D.S.SCOTT */

/* COMPUTER SCIENCES DEPARTMENT */
/* UNIVERSITY OF TEXAS AT AUSTIN */
/* AUSTIN, TX 78712 */

/* VERSION 2 ORIGINATED APRIL 1982 */

/* CURRENT VERSION  JUNE 1983 */

/* SNLASO FINDS A FEW EIGENVALUES AND EIGENVECTORS AT EITHER END OF */
/* THE SPECTRUM OF A LARGE SPARSE SYMMETRIC MATRIX.  THE SUBROUTINE */
/* SNLASO IS PRIMARILY A DRIVER FOR SUBROUTINE SNWLA WHICH IMPLEMENTS */
/* THE LANCZOS ALGORITHM WITH SELECTIVE ORTHOGONALIZATION AND */
/* SUBROUTINE SNPPLA WHICH POST PROCESSES THE OUTPUT OF SNWLA. */
/* HOWEVER SNLASO DOES CHECK FOR INCONSISTENCIES IN THE CALLING */
/* PARAMETERS AND DOES PREPROCESS ANY USER SUPPLIED EIGENPAIRS. */
/* SNLASO ALWAYS LOOKS FOR THE SMALLEST (LEFTMOST) EIGENVALUES.  IF */
/* THE LARGEST EIGENVALUES ARE DESIRED SNLASO IMPLICITLY USES THE */
/* NEGATIVE OF THE MATRIX. */


/* ON INPUT */


/*   OP   A USER SUPPLIED SUBROUTINE WITH CALLING SEQUENCE */
/*     OP(N,M,P,Q).  P AND Q ARE N X M MATRICES AND Q IS */
/*     RETURNED AS THE MATRIX TIMES P. */

/*   IOVECT   A USER SUPPLIED SUBROUTINE WITH CALLING SEQUENCE */
/*     IOVECT(N,M,Q,J,K).  Q IS AN N X M MATRIX.  IF K = 0 */
/*     THE COLUMNS OF Q ARE STORED AS THE (J-M+1)TH THROUGH */
/*     THE JTH LANCZOS VECTORS.  IF K = 1 THEN Q IS RETURNED */
/*     AS THE (J-M+1)TH THROUGH THE JTH LANCZOS VECTORS.  SEE */
/*     DOCUMENTATION FOR FURTHER DETAILS AND EXAMPLES. */

/*   N   THE ORDER OF THE MATRIX. */

/*   NVAL   NVAL SPECIFIES THE EIGENVALUES TO BE FOUND. */
/*     ABS(NVAL)  IS THE NUMBER OF EIGENVALUES DESIRED. */
/*     IF NVAL < 0 THE ALGEBRAICALLY SMALLEST (LEFTMOST) */
/*     EIGENVALUES ARE FOUND.  IF NVAL > 0 THE ALGEBRAICALLY */
/*     LARGEST (RIGHTMOST) EIGENVALUES ARE FOUND.  NVAL MUST NOT */
/*     BE ZERO.  ABS(NVAL) MUST BE LESS THAN  MAXJ/2. */

/*   NFIG   THE NUMBER OF DECIMAL DIGITS OF ACCURACY DESIRED IN THE */
/*     EIGENVALUES.  NFIG MUST BE GREATER THAN OR EQUAL TO 1. */

/*   NPERM   AN INTEGER VARIABLE WHICH SPECIFIES THE NUMBER OF USER */
/*     SUPPLIED EIGENPAIRS.  IN MOST CASES NPERM WILL BE ZERO.  SEE */
/*     DOCUMENTAION FOR FURTHER DETAILS OF USING NPERM GREATER */
/*     THAN ZERO.  NPERM MUST NOT BE LESS THAN ZERO. */

/*   NMVAL   THE ROW DIMENSION OF THE ARRAY VAL.  NMVAL MUST BE GREATER */
/*     THAN OR EQUAL TO ABS(NVAL). */

/*   VAL   A TWO DIMENSIONAL REAL ARRAY OF ROW */
/*     DIMENSION NMVAL AND COLUMN DIMENSION AT LEAST 4.  IF NPERM */
/*     IS GREATER THAN ZERO THEN CERTAIN INFORMATION MUST BE STORED */
/*     IN VAL.  SEE DOCUMENTATION FOR DETAILS. */

/*   NMVEC   THE ROW DIMENSION OF THE ARRAY VEC.  NMVEC MUST BE GREATER */
/*     THAN OR EQUAL TO N. */

/*   VEC   A TWO DIMENSIONAL REAL ARRAY OF ROW */
/*     DIMENSION NMVEC AND COLUMN DIMENSION AT LEAST ABS(NVAL).  IF */
/*     NPERM > 0 THEN THE FIRST NPERM COLUMNS OF VEC MUST */
/*     CONTAIN THE USER SUPPLIED EIGENVECTORS. */

/*   NBLOCK   THE BLOCK SIZE.  SEE DOCUMENTATION FOR CHOOSING */
/*     AN APPROPRIATE VALUE FOR NBLOCK.  NBLOCK MUST BE GREATER */
/*     THAN ZERO AND LESS THAN  MAXJ/6. */

/*   MAXOP   AN UPPER BOUND ON THE NUMBER OF CALLS TO THE SUBROUTINE */
/*     OP.  SNLASO TERMINATES WHEN MAXOP IS EXCEEDED.  SEE */
/*     DOCUMENTATION FOR GUIDELINES IN CHOOSING A VALUE FOR MAXOP. */

/*   MAXJ   AN INDICATION OF THE AVAILABLE STORAGE (SEE WORK AND */
/*     DOCUMENTATION ON IOVECT).  FOR THE FASTEST CONVERGENCE MAXJ */
/*     SHOULD BE AS LARGE AS POSSIBLE, ALTHOUGH IT IS USELESS TO HAVE */
/*     MAXJ LARGER THAN MAXOP*NBLOCK. */

/*   WORK   A REAL ARRAY OF DIMENSION AT LEAST AS */
/*     LARGE AS */

/*         2*N*NBLOCK + MAXJ*(NBLOCK+NV+2) + 2*NBLOCK*NBLOCK + 3*NV */

/*            + THE MAXIMUM OF */
/*                 N*NBLOCK */
/*                   AND */
/*         MAXJ*(2*NBLOCK+3) + 2*NV + 6 + (2*NBLOCK+2)*(NBLOCK+1) */

/*     WHERE NV = ABS(NVAL) */

/*     THE FIRST N*NBLOCK ELEMENTS OF WORK MUST CONTAIN THE DESIRED */
/*     STARTING VECTORS.  SEE DOCUMENTATION FOR GUIDELINES IN */
/*     CHOOSING STARTING VECTORS. */

/*   IND   AN INTEGER ARRAY OF DIMENSION AT LEAST ABS(NVAL). */

/*   IERR   AN INTEGER VARIABLE. */


/* ON OUTPUT */


/*   NPERM   THE NUMBER OF EIGENPAIRS NOW KNOWN. */

/*   VEC   THE FIRST NPERM COLUMNS OF VEC CONTAIN THE EIGENVECTORS. */

/*   VAL   THE FIRST COLUMN OF VAL CONTAINS THE CORRESPONDING */
/*     EIGENVALUES.  THE SECOND COLUMN CONTAINS THE RESIDUAL NORMS OF */
/*     THE EIGENPAIRS WHICH ARE BOUNDS ON THE ACCURACY OF THE EIGEN- */
/*     VALUES.  THE THIRD COLUMN CONTAINS MORE REALISTIC ESTIMATES */
/*     OF THE ACCURACY OF THE EIGENVALUES.  THE FOURTH COLUMN CONTAINS */
/*     ESTIMATES OF THE ACCURACY OF THE EIGENVECTORS.  SEE */
/*     DOCUMENTATION FOR FURTHER INFORMATION ON THESE QUANTITIES. */

/*   WORK   IF WORK IS TERMINATED BEFORE COMPLETION (IERR = -2) */
/*     THE FIRST N*NBLOCK ELEMENTS OF WORK CONTAIN THE BEST VECTORS */
/*     FOR RESTARTING THE ALGORITHM AND SNLASO CAN BE IMMEDIATELY */
/*     RECALLED TO CONTINUE WORKING ON THE PROBLEM. */

/*   IND   IND(1)  CONTAINS THE ACTUAL NUMBER OF CALLS TO OP.  ON SOME */
/*     OCCASIONS THE NUMBER OF CALLS TO OP MAY BE SLIGHTLY LARGER */
/*     THAN MAXOP. */

/*   IERR   AN ERROR COMPLETION CODE.  THE NORMAL COMPLETION CODE IS */
/*     ZERO.  SEE THE DOCUMENTATION FOR INTERPRETATIONS OF NON-ZERO */
/*     COMPLETION CODES. */


/* INTERNAL VARIABLES. */



/* NOP   RETURNED FROM SNWLA AS THE NUMBER OF CALLS TO THE */
/*   SUBROUTINE OP. */

/* NV   SET EQUAL TO ABS(NVAL), THE NUMBER OF EIGENVALUES DESIRED, */
/*   AND PASSED TO SNWLA. */

/* SMALL   SET TO .TRUE. IF THE SMALLEST EIGENVALUES ARE DESIRED. */

/* RARITZ   RETURNED FROM SNWLA AND PASSED TO SNPPLA.  RARITZ IS .TRUE. */
/*   IF A FINAL RAYLEIGH-RITZ PROCEDURE IS NEEDED. */

/* DELTA   RETURNED FROM SNWLA AS THE EIGENVALUE OF THE MATRIX */
/*   WHICH IS CLOSEST TO THE DESIRED EIGENVALUES. */

/* SNPPLA   A SUBROUTINE FOR POST-PROCESSING THE EIGENVECTORS COMPUTED */
/*   BY SNWLA. */

/* SNWLA   A SUBROUTINE FOR IMPLEMENTING THE LANCZOS ALGORITHM */
/*   WITH SELECTIVE ORTHOGONALIZATION. */

/* SMVPC   A SUBROUTINE FOR COMPUTING THE RESIDUAL NORM AND */
/*   ORTHOGONALITY COEFFICIENT OF GIVEN RITZ VECTORS. */

/* SORTQR   A SUBROUTINE FOR ORTHONORMALIZING A BLOCK OF VECTORS */
/*   USING HOUSEHOLDER REFLECTIONS. */

/* SAXPY,SCOPY,SDOT,SNRM2,SSCAL,SSWAP   A SUBSET OF THE BASIC LINEAR */
/*   ALGEBRA SUBPROGRAMS USED FOR VECTOR MANIPULATION. */

/* SLARAN   A SUBROUTINE TO GENERATE RANDOM VECTORS */

/* SLAEIG, SLAGER, SLABCM, SLABFC   SUBROUTINES FOR BAND EIGENVALUE */
/*   CALCULATIONS. */

/* ------------------------------------------------------------------ */

/* THIS SECTION CHECKS FOR INCONSISTENCY IN THE INPUT PARAMETERS. */

    /* Parameter adjustments */
    --ind;
    --work;
    vec_dim1 = *nmvec;
    vec_offset = vec_dim1 + 1;
    vec -= vec_offset;
    val_dim1 = *nmval;
    val_offset = val_dim1 + 1;
    val -= val_offset;

    /* Function Body */
    nv = abs(*nval);
    ind[1] = 0;
    *ierr = 0;
    if (*n < *nblock * 6) {
	*ierr = 1;
    }
    if (*nfig <= 0) {
	*ierr += 2;
    }
    if (*nmvec < *n) {
	*ierr += 4;
    }
    if (*nperm < 0) {
	*ierr += 8;
    }
    if (*maxj < *nblock * 6) {
	*ierr += 16;
    }
    if (nv < max(1,*nperm)) {
	*ierr += 32;
    }
    if (nv > *nmval) {
	*ierr += 64;
    }
    if (nv > *maxop) {
	*ierr += 128;
    }
    if (nv >= *maxj / 2) {
	*ierr += 256;
    }
    if (*nblock < 1) {
	*ierr += 512;
    }
    if (*ierr != 0) {
	return 0;
    }

    small = *nval < 0;

/* ------------------------------------------------------------------ */

/* THIS SECTION SORTS AND ORTHONORMALIZES THE USER SUPPLIED VECTORS. */
/* IF A USER SUPPLIED VECTOR IS ZERO OR IF SIGNIFICANT CANCELLATION */
/* OCCURS IN THE ORTHOGONALIZATION PROCESS THEN IERR IS SET TO  -1 */
/* AND SNLASO TERMINATES. */

    if (*nperm == 0) {
	goto L110;
    }

/* THIS NEGATES THE USER SUPPLIED EIGENVALUES WHEN THE LARGEST */
/* EIGENVALUES ARE DESIRED, SINCE SNWLA WILL IMPLICITLY USE THE */
/* NEGATIVE OF THE MATRIX. */

    if (small) {
	goto L20;
    }
    i__1 = *nperm;
    for (i = 1; i <= i__1; ++i) {
	val[i + val_dim1] = -(doublereal)val[i + val_dim1];
/* L10: */
    }

/* THIS SORTS THE USER SUPPLIED VALUES AND VECTORS. */

L20:
    svsort_(nperm, &val[val_offset], &val[(val_dim1 << 1) + 1], &c__0, tarr, 
	    nmvec, n, &vec[vec_offset]);

/* THIS STORES THE NORMS OF THE VECTORS FOR LATER COMPARISON. */
/* IT ALSO INSURES THAT THE RESIDUAL NORMS ARE POSITIVE. */

    i__1 = *nperm;
    for (i = 1; i <= i__1; ++i) {
	val[i + (val_dim1 << 1)] = (r__1 = val[i + (val_dim1 << 1)], dabs(
		r__1));
	val[i + val_dim1 * 3] = snrm2_(n, &vec[i * vec_dim1 + 1], &c__1);
/* L60: */
    }

/* THIS PERFORMS THE ORTHONORMALIZATION. */

    m = *n * *nblock + 1;
    sortqr_(nmvec, n, nperm, &vec[vec_offset], &work[m]);
    m = *n * *nblock - *nperm;
    i__1 = *nperm;
    for (i = 1; i <= i__1; ++i) {
	m = m + *nperm + 1;
	if ((r__1 = work[m], dabs(r__1)) > val[i + val_dim1 * 3] * (float).9) 
		{
	    goto L70;
	}
	*ierr = -1;
	return 0;

L70:
	;
    }

/* THIS COPIES THE RESIDUAL NORMS INTO THE CORRECT LOCATIONS IN */
/* THE ARRAY WORK FOR LATER REFERENCE IN SNWLA. */

    m = (*n << 1) * *nblock + 1;
    scopy_(nperm, &val[(val_dim1 << 1) + 1], &c__1, &work[m], &c__1);

/* THIS SETS EPS TO AN APPROXIMATION OF THE RELATIVE MACHINE */
/* PRECISION */

/* ***THIS SHOULD BE REPLACED BY AN ASSIGNMENT STATEMENT */
/* ***IN A PRODUCTION CODE */

L110:
    eps = (float)1.;
    for (i = 1; i <= 1000; ++i) {
	eps *= (float).5;
	temp = eps + (float)1.;
	if (temp == (float)1.) {
	    goto L130;
	}
/* L120: */
    }

/* ------------------------------------------------------------------ */

/* THIS SECTION CALLS SNWLA WHICH IMPLEMENTS THE LANCZOS ALGORITHM */
/* WITH SELECTIVE ORTHOGONALIZATION. */

L130:
    nband = *nblock + 1;
    i1 = *n * *nblock + 1;
    i2 = i1 + *n * *nblock;
    i3 = i2 + nv;
    i4 = i3 + nv;
    i5 = i4 + nv;
    i6 = i5 + *maxj * nband;
    i7 = i6 + *nblock * *nblock;
    i8 = i7 + *nblock * *nblock;
    i9 = i8 + *maxj * (nv + 1);
    i10 = i9 + *nblock;
    i11 = i10 + (nv << 1) + 6;
    i12 = i11 + *maxj * ((*nblock << 1) + 1);
    i13 = i12 + *maxj;
    snwla_(op, iovect, n, &nband, &nv, nfig, nperm, &val[val_offset], nmvec, &
	    vec[vec_offset], nblock, maxop, maxj, &nop, &work[1], &work[i1], &
	    work[i2], &work[i3], &work[i4], &work[i5], &work[i6], &work[i7], &
	    work[i8], &work[i9], &work[i10], &work[i11], &work[i12], &work[
	    i13], &ind[1], &small, &raritz, &delta, &eps, ierr);

/* ------------------------------------------------------------------ */

/* THIS SECTION CALLS SNPPLA (THE POST PROCESSOR). */

    if (*nperm == 0) {
	goto L140;
    }
    i1 = *n * *nblock + 1;
    i2 = i1 + *nperm * *nperm;
    i3 = i2 + *nperm * *nperm;
/* Computing MAX */
    i__1 = *n * *nblock, i__2 = (*nperm << 1) * *nperm;
    i4 = i3 + max(i__1,i__2);
    i5 = i4 + *n * *nblock;
    i6 = i5 + (*nperm << 1) + 4;
    snppla_(op, iovect, n, nperm, &nop, nmval, &val[val_offset], nmvec, &vec[
	    vec_offset], nblock, &work[i1], &work[i2], &work[i3], &work[i4], &
	    work[i5], &work[i6], &delta, &small, &raritz, &eps);

L140:
    ind[1] = nop;
    return 0;
} /* snlaso_ */


/* ------------------------------------------------------------------ */

#include <stdio.h>
#define fsm_debug /*printf("%d: n=%d\n", __LINE__, *n)*/
/* Subroutine */ int snwla_(op, iovect, n, nband, nval, nfig, nperm, val, 
	nmvec, vec, nblock, maxop, maxj, nop, p1, p0, res, tau, otau, t, alp, 
	bet, s, p2, bound, atemp, vtemp, d, ind, small, raritz, delta, eps, 
	ierr)
/* Subroutine */ int (*op) (), (*iovect) ();
integer *n, *nband, *nval, *nfig, *nperm;
real *val;
integer *nmvec;
real *vec;
integer *nblock, *maxop, *maxj, *nop;
real *p1, *p0, *res, *tau, *otau, *t, *alp, *bet, *s, *p2, *bound, *atemp, *
	vtemp, *d;
integer *ind;
logical *small, *raritz;
real *delta, *eps;
integer *ierr;
{
    /* System generated locals */
    integer vec_dim1, vec_offset, p0_dim1, p0_offset, p1_dim1, p1_offset, 
	    p2_dim1, p2_offset, t_dim1, t_offset, alp_dim1, alp_offset, 
	    bet_dim1, bet_offset, s_dim1, s_offset, i__1, i__2, i__3, i__4;
    real r__1, r__2, r__3;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(), pow_dd();

    /* Local variables */
    static real tola, temp, tolg, tmin, tmax;
    extern doublereal sdot_();
    static real tarr[1];
    static logical test;
    static real zero[1], utol;
    extern doublereal snrm2_();
    static integer i, j, k, l, m;
    extern /* Subroutine */ int sscal_();
    static integer ngood, nleft;
    static real anorm;
    static integer mtemp;
    extern /* Subroutine */ int smvpc_();
    static integer i1;
    static real pnorm, epsrt, rnorm;
    extern /* Subroutine */ int scopy_(), saxpy_();
    static integer ii, ng;
    extern /* Subroutine */ int slaeig_(), slager_();
    static real betmin, alpmin, betmax, alpmax;
    static integer ntheta;
    extern /* Subroutine */ int slaran_();
    static logical enough;
    static integer number, nstart;
    extern /* Subroutine */ int sortqr_(), svsort_();



/* SNWLA IMPLEMENTS THE LANCZOS ALGORITHM WITH SELECTIVE */
/* ORTHOGONALIZATION. */

/*   NBAND  NBLOCK + 1  THE BAND WIDTH OF T. */

/*   NVAL   THE NUMBER OF DESIRED EIGENVALUES. */

/*   NPERM   THE NUMBER OF PERMANENT VECTORS (THOSE EIGENVECTORS */
/*     INPUT BY THE USER OR THOSE EIGENVECTORS SAVED WHEN THE */
/*     ALGORITHM IS ITERATED).  PERMANENT VECTORS ARE ORTHOGONAL */
/*     TO THE CURRENT KRYLOV SUBSPACE. */

/*   NOP   THE NUMBER OF CALLS TO OP. */

/*   P0, P1, AND P2   THE CURRENT BLOCKS OF LANCZOS VECTORS. */

/*   RES   THE (APPROXIMATE) RESIDUAL NORMS OF THE PERMANENT VECTORS. */

/*   TAU AND OTAU   USED TO MONITOR THE NEED FOR ORTHOGONALIZATION. */

/*   T   THE BAND MATRIX. */

/*   ALP   THE CURRENT DIAGONAL BLOCK. */

/*   BET   THE CURRENT OFF DIAGONAL BLOCK. */

/*   BOUND, ATEMP, VTEMP, D  TEMPORARY STORAGE USED BY THE BAND */
/*      EIGENVALUE SOLVER SLAEIG. */

/*   S   EIGENVECTORS OF T. */

/*   SMALL   .TRUE.  IF THE SMALL EIGENVALUES ARE DESIRED. */

/*   RARITZ  RETURNED AS  .TRUE.  IF A FINAL RAYLEIGH-RITZ PROCEDURE */
/*     IS TO BE DONE. */

/*   DELTA   RETURNED AS THE VALUE OF THE (NVAL+1)TH EIGENVALUE */
/*     OF THE MATRIX.  USED IN ESTIMATING THE ACCURACY OF THE */
/*     COMPUTED EIGENVALUES. */


/* INTERNAL VARIABLES USED */


/* J   THE CURRENT DIMENSION OF T.  (THE DIMENSION OF THE CURRENT */
/*    KRYLOV SUBSPACE. */

/* NGOOD   THE NUMBER OF GOOD RITZ VECTORS (GOOD VECTORS */
/*    LIE IN THE CURRENT KRYLOV SUBSPACE). */

/* NLEFT   THE NUMBER OF VALUES WHICH REMAIN TO BE DETERMINED, */
/*    I.E.  NLEFT = NVAL - NPERM. */

/* NUMBER = NPERM + NGOOD. */

/* ANORM   AN ESTIMATE OF THE NORM OF THE MATRIX. */

/* EPS   THE RELATIVE MACHINE PRECISION. */

/* UTOL   THE USER TOLERANCE. */

/*TARR  AN ARRAY OF LENGTH ONE USED TO INSURE TYPE CONSISTENCY IN CALLS TO
*/
/*       SLAEIG */

/*ZERO  AN ARRAY OF LENGTH ONE CONTAINING ZERO, USED TO INSURE TYPE CONSIS
TENCY*/
/*       IN CALLS TO SCOPY */

fsm_debug;
    /* Parameter adjustments */
    --ind;
    --d;
    --vtemp;
    --atemp;
    --bound;
    p2_dim1 = *n;
    p2_offset = p2_dim1 + 1;
    p2 -= p2_offset;
    s_dim1 = *maxj;
    s_offset = s_dim1 + 1;
    s -= s_offset;
    bet_dim1 = *nblock;
    bet_offset = bet_dim1 + 1;
    bet -= bet_offset;
    alp_dim1 = *nblock;
    alp_offset = alp_dim1 + 1;
    alp -= alp_offset;
    t_dim1 = *nband;
    t_offset = t_dim1 + 1;
    t -= t_offset;
    --otau;
    --tau;
    --res;
    p0_dim1 = *n;
    p0_offset = p0_dim1 + 1;
    p0 -= p0_offset;
    p1_dim1 = *n;
    p1_offset = p1_dim1 + 1;
    p1 -= p1_offset;
    vec_dim1 = *nmvec;
    vec_offset = vec_dim1 + 1;
    vec -= vec_offset;
    --val;
fsm_debug;

    /* Function Body */
    zero[0] = (float)0.;
    rnorm = (float)0.;
    if (*nperm != 0) {
/* Computing MAX */
	r__1 = -(doublereal)val[1], r__2 = val[*nperm];
	rnorm = dmax(r__1,r__2);
    }
    pnorm = rnorm;
    *delta = (float)1e31;
    epsrt = sqrt(*eps);
    nleft = *nval - *nperm;
    *nop = 0;
    number = *nperm;
    *raritz = FALSE_;
/* Computing MAX */
    d__1 = (doublereal) (-(doublereal)((real) (*nfig)));
    r__1 = (real) (*n) * *eps, r__2 = pow_dd(&c_b15, &d__1);
    utol = dmax(r__1,r__2);
    j = *maxj;

/* ------------------------------------------------------------------ */

/* ANY ITERATION OF THE ALGORITHM BEGINS HERE. */

L30:
fsm_debug;
    i__1 = *nblock;
    for (i = 1; i <= i__1; ++i) {
	temp = snrm2_(n, &p1[i * p1_dim1 + 1], &c__1);
fsm_debug;
	if (temp == (float)0.) {
	    slaran_(n, &p1[i * p1_dim1 + 1]);
fsm_debug;
	}
/* L50: */
    }
fsm_debug;
    if (*nperm == 0) {
	goto L70;
    }
    i__1 = *nperm;
    for (i = 1; i <= i__1; ++i) {
	tau[i] = (float)1.;
	otau[i] = (float)0.;
/* L60: */
    }
L70:
fsm_debug;
    i__1 = *n * *nblock;
    scopy_(&i__1, zero, &c__0, &p0[p0_offset], &c__1);
fsm_debug;
    i__1 = *nblock * *nblock;
    scopy_(&i__1, zero, &c__0, &bet[bet_offset], &c__1);
fsm_debug;
    i__1 = j * *nband;
    scopy_(&i__1, zero, &c__0, &t[t_offset], &c__1);
fsm_debug;
    mtemp = *nval + 1;
    i__1 = mtemp;
fsm_debug;
    for (i = 1; i <= i__1; ++i) {
	scopy_(&j, zero, &c__0, &s[i * s_dim1 + 1], &c__1);
/* L75: */
    }
    ngood = 0;
    tmin = (float)1e30;
    tmax = (float)-1e30;
    test = TRUE_;
    enough = FALSE_;
    betmax = (float)0.;
    j = 0;

/* ------------------------------------------------------------------ */

/* THIS SECTION TAKES A SINGLE BLOCK LANCZOS STEP. */

L80:
    j += *nblock;

/* THIS IS THE SELECTIVE ORTHOGONALIZATION. */
fsm_debug;

    if (number == 0) {
	goto L110;
    }
    i__1 = number;
    for (i = 1; i <= i__1; ++i) {
	if (tau[i] < epsrt) {
	    goto L100;
	}
	test = TRUE_;
	tau[i] = (float)0.;
	if (otau[i] != (float)0.) {
	    otau[i] = (float)1.;
	}
	i__2 = *nblock;
	for (k = 1; k <= i__2; ++k) {
	    temp = -(doublereal)sdot_(n, &vec[i * vec_dim1 + 1], &c__1, &p1[k 
		    * p1_dim1 + 1], &c__1);
	    saxpy_(n, &temp, &vec[i * vec_dim1 + 1], &c__1, &p1[k * p1_dim1 + 
		    1], &c__1);

/* THIS CHECKS FOR TOO GREAT A LOSS OF ORTHOGONALITY BETWEEN A */
/* NEW LANCZOS VECTOR AND A GOOD RITZ VECTOR.  THE ALGORITHM IS */
/* TERMINATED IF TOO MUCH ORTHOGONALITY IS LOST. */

	    if ((r__1 = temp * bet[k + k * bet_dim1], dabs(r__1)) > (real) (*
		    n) * epsrt * anorm && i > *nperm) {
		goto L380;
	    }
/* L90: */
	}
L100:
	;
    }

/* IF NECESSARY, THIS REORTHONORMALIZES P1 AND UPDATES BET. */

L110:
    if (! test) {
	goto L160;
    }
fsm_debug;
    sortqr_(n, n, nblock, &p1[p1_offset], &alp[alp_offset]);
    test = FALSE_;
    if (j == *nblock) {
	goto L160;
    }
    i__1 = *nblock;
    for (i = 1; i <= i__1; ++i) {
	if (alp[i + i * alp_dim1] > (float)0.) {
	    goto L130;
	}
	m = j - (*nblock << 1) + i;
	l = *nblock + 1;
	i__2 = *nblock;
	for (k = i; k <= i__2; ++k) {
	    bet[i + k * bet_dim1] = -(doublereal)bet[i + k * bet_dim1];
	    t[l + m * t_dim1] = -(doublereal)t[l + m * t_dim1];
	    --l;
	    ++m;
/* L120: */
	}
L130:
	;
    }

/* THIS IS THE LANCZOS STEP. */

L160:
fsm_debug;
    (*op)(n, nblock, &p1[p1_offset], &p2[p2_offset]);
    ++(*nop);
    (*iovect)(n, nblock, &p1[p1_offset], &j, &c__0);

/* THIS COMPUTES P2=P2-P0*BET(TRANSPOSE) */

    i__1 = *nblock;
    for (i = 1; i <= i__1; ++i) {
	i__2 = *nblock;
	for (k = i; k <= i__2; ++k) {
	    r__1 = -(doublereal)bet[i + k * bet_dim1];
	    saxpy_(n, &r__1, &p0[k * p0_dim1 + 1], &c__1, &p2[i * p2_dim1 + 1]
		    , &c__1);
/* L170: */
	}
/* L180: */
    }

/* THIS COMPUTES ALP AND P2=P2-P1*ALP. */

    i__1 = *nblock;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	for (k = 1; k <= i__2; ++k) {
	    ii = i - k + 1;
	    alp[ii + k * alp_dim1] = sdot_(n, &p1[i * p1_dim1 + 1], &c__1, &
		    p2[k * p2_dim1 + 1], &c__1);
	    r__1 = -(doublereal)alp[ii + k * alp_dim1];
	    saxpy_(n, &r__1, &p1[i * p1_dim1 + 1], &c__1, &p2[k * p2_dim1 + 1]
		    , &c__1);
	    if (k != i) {
		r__1 = -(doublereal)alp[ii + k * alp_dim1];
		saxpy_(n, &r__1, &p1[k * p1_dim1 + 1], &c__1, &p2[i * p2_dim1 
			+ 1], &c__1);
	    }
/* L190: */
	}
/* L200: */
    }

/*  REORTHOGONALIZATION OF THE SECOND BLOCK */

    if (j != *nblock) {
	goto L220;
    }
    i__1 = *nblock;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	for (k = 1; k <= i__2; ++k) {
	    temp = sdot_(n, &p1[i * p1_dim1 + 1], &c__1, &p2[k * p2_dim1 + 1],
		     &c__1);
	    r__1 = -(doublereal)temp;
	    saxpy_(n, &r__1, &p1[i * p1_dim1 + 1], &c__1, &p2[k * p2_dim1 + 1]
		    , &c__1);
	    if (k != i) {
		r__1 = -(doublereal)temp;
		saxpy_(n, &r__1, &p1[k * p1_dim1 + 1], &c__1, &p2[i * p2_dim1 
			+ 1], &c__1);
	    }
	    ii = i - k + 1;
	    alp[ii + k * alp_dim1] += temp;
/* L210: */
	}
/* L215: */
    }

/* THIS ORTHONORMALIZES THE NEXT BLOCK */

L220:
    sortqr_(n, n, nblock, &p2[p2_offset], &bet[bet_offset]);

/* THIS STORES ALP AND BET IN T. */

    i__1 = *nblock;
    for (i = 1; i <= i__1; ++i) {
	m = j - *nblock + i;
	i__2 = *nblock;
	for (k = i; k <= i__2; ++k) {
	    l = k - i + 1;
	    t[l + m * t_dim1] = alp[l + i * alp_dim1];
/* L230: */
	}
	i__2 = i;
	for (k = 1; k <= i__2; ++k) {
	    l = *nblock - i + k + 1;
	    t[l + m * t_dim1] = bet[k + i * bet_dim1];
/* L240: */
	}
/* L250: */
    }

/* THIS NEGATES T IF SMALL IS FALSE. */

    if (*small) {
	goto L280;
    }
    m = j - *nblock + 1;
    i__1 = j;
    for (i = m; i <= i__1; ++i) {
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    t[k + i * t_dim1] = -(doublereal)t[k + i * t_dim1];
/* L260: */
	}
/* L270: */
    }

/* THIS SHIFTS THE LANCZOS VECTORS */

L280:
    i__1 = *nblock * *n;
    scopy_(&i__1, &p1[p1_offset], &c__1, &p0[p0_offset], &c__1);
    i__1 = *nblock * *n;
    scopy_(&i__1, &p2[p2_offset], &c__1, &p1[p1_offset], &c__1);
    i__1 = j - *nblock + 1;
    slager_(&j, nband, &i__1, &t[t_offset], &tmin, &tmax);
/* Computing MAX */
    r__1 = max(rnorm,tmax), r__2 = -(doublereal)tmin;
    anorm = dmax(r__1,r__2);
    if (number == 0) {
	goto L305;
    }

/* THIS COMPUTES THE EXTREME EIGENVALUES OF ALP. */

    scopy_(nblock, zero, &c__0, &p2[p2_offset], &c__1);
    slaeig_(nblock, nblock, &c__1, &c__1, &alp[alp_offset], tarr, nblock, &p2[
	    p2_offset], &bound[1], &atemp[1], &d[1], &vtemp[1], eps, &tmin, &
	    tmax);
    alpmin = tarr[0];
    scopy_(nblock, zero, &c__0, &p2[p2_offset], &c__1);
    slaeig_(nblock, nblock, nblock, nblock, &alp[alp_offset], tarr, nblock, &
	    p2[p2_offset], &bound[1], &atemp[1], &d[1], &vtemp[1], eps, &tmin,
	     &tmax);
    alpmax = tarr[0];

/* THIS COMPUTES ALP = BET(TRANSPOSE)*BET. */

L305:
    i__1 = *nblock;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	for (k = 1; k <= i__2; ++k) {
	    l = i - k + 1;
	    i__3 = *nblock - i + 1;
	    alp[l + k * alp_dim1] = sdot_(&i__3, &bet[i + i * bet_dim1], 
		    nblock, &bet[k + i * bet_dim1], nblock);
/* L300: */
	}
/* L310: */
    }
    if (number == 0) {
	goto L330;
    }

/* THIS COMPUTES THE SMALLEST SINGULAR VALUE OF BET. */

    scopy_(nblock, zero, &c__0, &p2[p2_offset], &c__1);
    r__1 = anorm * anorm;
    slaeig_(nblock, nblock, &c__1, &c__1, &alp[alp_offset], tarr, nblock, &p2[
	    p2_offset], &bound[1], &atemp[1], &d[1], &vtemp[1], eps, &c_b88, &
	    r__1);
    betmin = sqrt(tarr[0]);

/* THIS UPDATES TAU AND OTAU. */

    i__1 = number;
    for (i = 1; i <= i__1; ++i) {
/* Computing MAX */
	r__1 = alpmax - val[i], r__2 = val[i] - alpmin;
	temp = (tau[i] * dmax(r__1,r__2) + otau[i] * betmax + *eps * anorm) / 
		betmin;
	if (i <= *nperm) {
	    temp += res[i] / betmin;
	}
	otau[i] = tau[i];
	tau[i] = temp;
/* L320: */
    }

/* THIS COMPUTES THE LARGEST SINGULAR VALUE OF BET. */

L330:
    scopy_(nblock, zero, &c__0, &p2[p2_offset], &c__1);
    r__1 = anorm * anorm;
    slaeig_(nblock, nblock, nblock, nblock, &alp[alp_offset], tarr, nblock, &
	    p2[p2_offset], &bound[1], &atemp[1], &d[1], &vtemp[1], eps, &
	    c_b88, &r__1);
    betmax = sqrt(tarr[0]);
    if (j <= *nblock << 1) {
	goto L80;
    }

/* ------------------------------------------------------------------ */

/* THIS SECTION COMPUTES AND EXAMINES THE SMALLEST NONGOOD AND */
/* LARGEST DESIRED EIGENVALUES OF T TO SEE IF A CLOSER LOOK */
/* IS JUSTIFIED. */

    tolg = epsrt * anorm;
    tola = utol * rnorm;
    if (*maxj - j < *nblock || *nop >= *maxop && nleft != 0) {
	goto L390;
    }
    goto L400;

/* ------------------------------------------------------------------ */

/* THIS SECTION COMPUTES SOME EIGENVALUES AND EIGENVECTORS OF T TO */
/* SEE IF FURTHER ACTION IS INDICATED, ENTRY IS AT 380 OR 390 IF AN */
/* ITERATION (OR TERMINATION) IS KNOWN TO BE NEEDED, OTHERWISE ENTRY */
/* IS AT 400. */

L380:
    j -= *nblock;
    *ierr = -8;
L390:
    if (nleft == 0) {
	return 0;
    }
    test = TRUE_;
L400:
/* Computing MIN */
    i__1 = j / 2, i__2 = nleft + 1;
    ntheta = min(i__1,i__2);
printf("%d: calling slaeig_()\n", __LINE__);
    slaeig_(&j, nband, &c__1, &ntheta, &t[t_offset], &val[number + 1], maxj, &
	    s[s_offset], &bound[1], &atemp[1], &d[1], &vtemp[1], eps, &tmin, &
	    tmax);
printf("%d: done slaeig_()\n", __LINE__);
    smvpc_(nblock, &bet[bet_offset], maxj, &j, &s[s_offset], &ntheta, &atemp[
	    1], &vtemp[1], &d[1]);

/* THIS CHECKS FOR TERMINATION OF A CHECK RUN */

    if (nleft != 0 || j < *nblock * 6) {
	goto L410;
    }
    if (val[number + 1] - atemp[1] > val[*nperm] - tola) {
	goto L790;
    }

/* THIS UPDATES NLEFT BY EXAMINING THE COMPUTED EIGENVALUES OF T */
/* TO DETERMINE IF SOME PERMANENT VALUES ARE NO LONGER DESIRED. */

L410:
    if (ntheta <= nleft) {
	goto L470;
    }
    if (*nperm == 0) {
	goto L430;
    }
    m = number + nleft + 1;
    if (val[m] >= val[*nperm]) {
	goto L430;
    }
    --(*nperm);
    ngood = 0;
    number = *nperm;
    ++nleft;
    goto L400;

/* THIS UPDATES DELTA. */

L430:
    m = number + nleft + 1;
/* Computing MIN */
    r__1 = *delta, r__2 = val[m];
    *delta = dmin(r__1,r__2);
    enough = TRUE_;
    if (nleft == 0) {
	goto L80;
    }
    ntheta = nleft;
    vtemp[ntheta + 1] = (float)1.;

/* ------------------------------------------------------------------ */

/* THIS SECTION EXAMINES THE COMPUTED EIGENPAIRS IN DETAIL. */

/* THIS CHECKS FOR ENOUGH ACCEPTABLE VALUES. */

    if (! (test || enough)) {
	goto L470;
    }
    *delta = dmin(*delta,anorm);
/* Computing MAX */
/* Computing MAX */
    r__3 = -(doublereal)val[number + 1];
    r__1 = rnorm, r__2 = dmax(r__3,*delta);
    pnorm = dmax(r__1,r__2);
    tola = utol * pnorm;
    nstart = 0;
    i__1 = ntheta;
    for (i = 1; i <= i__1; ++i) {
	m = number + i;
/* Computing MIN */
	r__1 = atemp[i] * atemp[i] / (*delta - val[m]), r__2 = atemp[i];
	if (dmin(r__1,r__2) > tola) {
	    goto L450;
	}
	ind[i] = -1;
	goto L460;

L450:
	enough = FALSE_;
	if (! test) {
	    goto L470;
	}
	ind[i] = 1;
	++nstart;
L460:
	;
    }

/*  COPY VALUES OF IND INTO VTEMP */

    i__1 = ntheta;
    for (i = 1; i <= i__1; ++i) {
	vtemp[i] = (real) ind[i];
/* L465: */
    }
    goto L500;

/* THIS CHECKS FOR NEW GOOD VECTORS. */

L470:
    ng = 0;
    i__1 = ntheta;
    for (i = 1; i <= i__1; ++i) {
	if (vtemp[i] > tolg) {
	    goto L480;
	}
	++ng;
	vtemp[i] = (float)-1.;
	goto L490;

L480:
	vtemp[i] = (float)1.;
L490:
	;
    }

    if (ng <= ngood) {
	goto L80;
    }
    nstart = ntheta - ng;

/* ------------------------------------------------------------------ */

/* THIS SECTION COMPUTES AND NORMALIZES THE INDICATED RITZ VECTORS. */
/* IF NEEDED (TEST = .TRUE.), NEW STARTING VECTORS ARE COMPUTED. */

L500:
    test = test && ! enough;
    ngood = ntheta - nstart;
    ++nstart;
    ++ntheta;

/* THIS ALIGNS THE DESIRED (ACCEPTABLE OR GOOD) EIGENVALUES AND */
/* EIGENVECTORS OF T.  THE OTHER EIGENVECTORS ARE SAVED FOR */
/* FORMING STARTING VECTORS, IF NECESSARY.  IT ALSO SHIFTS THE */
/* EIGENVALUES TO OVERWRITE THE GOOD VALUES FROM THE PREVIOUS */
/* PAUSE. */

    scopy_(&ntheta, &val[number + 1], &c__1, &val[*nperm + 1], &c__1);
    if (nstart == 0) {
	goto L580;
    }
    if (nstart == ntheta) {
	goto L530;
    }
    svsort_(&ntheta, &vtemp[1], &atemp[1], &c__1, &val[*nperm + 1], maxj, &j, 
	    &s[s_offset]);

/* THES ACCUMULATES THE J-VECTORS USED TO FORM THE STARTING */
/* VECTORS. */

L530:
    if (! test) {
	nstart = 0;
    }
    if (! test) {
	goto L580;
    }

/*  FIND MINIMUM ATEMP VALUE TO AVOID POSSIBLE OVERFLOW */

    temp = atemp[1];
    i__1 = nstart;
    for (i = 1; i <= i__1; ++i) {
/* Computing MIN */
	r__1 = temp, r__2 = atemp[i];
	temp = dmin(r__1,r__2);
/* L535: */
    }
    m = ngood + 1;
    l = ngood + min(nstart,*nblock);
    i__1 = l;
    for (i = m; i <= i__1; ++i) {
	r__1 = temp / atemp[i];
	sscal_(&j, &r__1, &s[i * s_dim1 + 1], &c__1);
/* L540: */
    }
    m = (nstart - 1) / *nblock;
    if (m == 0) {
	goto L570;
    }
    l = ngood + *nblock;
    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
	i__2 = *nblock;
	for (k = 1; k <= i__2; ++k) {
	    ++l;
	    if (l > ntheta) {
		goto L570;
	    }
	    i1 = ngood + k;
	    r__1 = temp / atemp[l];
	    saxpy_(&j, &r__1, &s[l * s_dim1 + 1], &c__1, &s[i1 * s_dim1 + 1], 
		    &c__1);
/* L550: */
	}
/* L560: */
    }
L570:
    nstart = min(nstart,*nblock);

/* THIS STORES THE RESIDUAL NORMS OF THE NEW PERMANENT VECTORS. */

L580:
    if (ngood == 0 || ! (test || enough)) {
	goto L600;
    }
    i__1 = ngood;
    for (i = 1; i <= i__1; ++i) {
	m = *nperm + i;
	res[m] = atemp[i];
/* L590: */
    }

/* THIS COMPUTES THE RITZ VECTORS BY SEQUENTIALLY RECALLING THE */
/* LANCZOS VECTORS. */

L600:
    number = *nperm + ngood;
    if (test || enough) {
	i__1 = *n * *nblock;
	scopy_(&i__1, zero, &c__0, &p1[p1_offset], &c__1);
    }
    if (ngood == 0) {
	goto L620;
    }
    m = *nperm + 1;
    i__1 = number;
    for (i = m; i <= i__1; ++i) {
	scopy_(n, zero, &c__0, &vec[i * vec_dim1 + 1], &c__1);
/* L610: */
    }
L620:
    i__1 = j;
    i__2 = *nblock;
    for (i = *nblock; i__2 < 0 ? i >= i__1 : i <= i__1; i += i__2) {
	(*iovect)(n, nblock, &p2[p2_offset], &i, &c__1);
	i__3 = *nblock;
	for (k = 1; k <= i__3; ++k) {
	    m = i - *nblock + k;
	    if (nstart == 0) {
		goto L640;
	    }
	    i__4 = nstart;
	    for (l = 1; l <= i__4; ++l) {
		i1 = ngood + l;
		saxpy_(n, &s[m + i1 * s_dim1], &p2[k * p2_dim1 + 1], &c__1, &
			p1[l * p1_dim1 + 1], &c__1);
/* L630: */
	    }
L640:
	    if (ngood == 0) {
		goto L660;
	    }
	    i__4 = ngood;
	    for (l = 1; l <= i__4; ++l) {
		i1 = l + *nperm;
		saxpy_(n, &s[m + l * s_dim1], &p2[k * p2_dim1 + 1], &c__1, &
			vec[i1 * vec_dim1 + 1], &c__1);
/* L650: */
	    }
L660:
	    ;
	}
/* L670: */
    }
    if (test || enough) {
	goto L690;
    }

/* THIS NORMALIZES THE RITZ VECTORS AND INITIALIZES THE */
/* TAU RECURRENCE. */

    m = *nperm + 1;
    i__2 = number;
    for (i = m; i <= i__2; ++i) {
	temp = (float)1. / snrm2_(n, &vec[i * vec_dim1 + 1], &c__1);
	sscal_(n, &temp, &vec[i * vec_dim1 + 1], &c__1);
	tau[i] = (float)1.;
	otau[i] = (float)1.;
/* L680: */
    }

/*  SHIFT S VECTORS TO ALIGN FOR LATER CALL TO SLAEIG */

    scopy_(&ntheta, &val[*nperm + 1], &c__1, &vtemp[1], &c__1);
    svsort_(&ntheta, &vtemp[1], &atemp[1], &c__0, tarr, maxj, &j, &s[s_offset]
	    );
    goto L80;

/* ------------------------------------------------------------------ */

/* THIS SECTION PREPARES TO ITERATE THE ALGORITHM BY SORTING THE */
/* PERMANENT VALUES, RESETTING SOME PARAMETERS, AND ORTHONORMALIZING */
/* THE PERMANENT VECTORS. */

L690:
    if (ngood == 0 && *nop >= *maxop) {
	goto L810;
    }
    if (ngood == 0) {
	goto L30;
    }

/* THIS ORTHONORMALIZES THE VECTORS */

    i__2 = *nperm + ngood;
    sortqr_(nmvec, n, &i__2, &vec[vec_offset], &s[s_offset]);

/* THIS SORTS THE VALUES AND VECTORS. */

    if (*nperm != 0) {
	i__2 = *nperm + ngood;
	svsort_(&i__2, &val[1], &res[1], &c__0, &temp, nmvec, n, &vec[
		vec_offset]);
    }
    *nperm += ngood;
    nleft -= ngood;
/* Computing MAX */
    r__1 = -(doublereal)val[1], r__2 = val[*nperm];
    rnorm = dmax(r__1,r__2);

/* THIS DECIDES WHERE TO GO NEXT. */

    if (*nop >= *maxop && nleft != 0) {
	goto L810;
    }
    if (nleft != 0) {
	goto L30;
    }
    if (val[*nval] - val[1] < tola) {
	goto L790;
    }

/* THIS DOES A CLUSTER TEST TO SEE IF A CHECK RUN IS NEEDED */
/* TO LOOK FOR UNDISCLOSED MULTIPLICITIES. */

    m = *nperm - *nblock + 1;
    if (m <= 0) {
	return 0;
    }
    i__2 = m;
    for (i = 1; i <= i__2; ++i) {
	l = i + *nblock - 1;
	if (val[l] - val[i] < tola) {
	    goto L30;
	}
/* L780: */
    }

/* THIS DOES A CLUSTER TEST TO SEE IF A FINAL RAYLEIGH-RITZ */
/* PROCEDURE IS NEEDED. */

L790:
    m = *nperm - *nblock;
    if (m <= 0) {
	return 0;
    }
    i__2 = m;
    for (i = 1; i <= i__2; ++i) {
	l = i + *nblock;
	if (val[l] - val[i] >= tola) {
	    goto L800;
	}
	*raritz = TRUE_;
	return 0;
L800:
	;
    }

    return 0;

/* ------------------------------------------------------------------ */

/* THIS REPORTS THAT MAXOP WAS EXCEEDED. */

L810:
    *ierr = -2;
    goto L790;

} /* snwla_ */

