#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

static void slabax_(const integer *n, const integer *nband, real *a, real *x, real *y);
static void slabcm_(const integer *n, const integer *nband, const integer *nl, const integer *nr,
                    real *a, real *eigval, const integer *lde, real *eigvec,
                    real *atol, real *artol, real *bound, real *atemp, real *d, real *vtemp);
static void slabfc_(const integer *n, const integer *nband, real *a, real *sigma, const integer *number,
                    const integer *lde, real *eigvec, integer *numl, integer *ldad,
                    real *atemp, real *d, real *atol);
static void slaeig_(const integer *n, const integer *nband, const integer *nl, const integer *nr,
                    real *a, real *eigval, const integer *lde,
                    real *eigvec, real *bound, real *atemp, real *d,
                    real *vtemp, real *eps, real *tmin, real *tmax);
static void slager_(const integer *n, const integer *nband, const integer *nstart,
                    real *a, real *tmin, real *tmax);
static void slaran_(const integer *n, real *x);
static void smvpc_(const integer *nblock, const real *bet, const integer *maxj, const integer *j,
                   const real *s, const integer *number, real *resnrm, real *orthcf, real *rv);
static void snppla_(void (*op)(const integer*,const integer*,const real*,real*),
                    void (*iovect)(const integer*,const integer*,real*,const integer*,const integer*),
                    const integer *n, const integer *nperm, integer *nop, const integer *nmval,
                    real *val, const integer *nmvec, real *vec, const integer *nblock,
                    real *h, real *hv, real *p, real *q, real *bound,
                    real *d, real *delta, logical *small, logical *raritz, real *eps);
static void snwla_(void (*op)(const integer*,const integer*,const real*,real*),
                   void (*iovect)(const integer*,const integer*,real*,const integer*,const integer*),
                   const integer *n, const integer *nband, const integer *nval,
                   const integer *nfig, integer *nperm, real *val, const integer *nmvec, real *vec,
                   const integer *nblock, const integer *maxop, const integer *maxj, integer *nop,
                   real *p1, real *p0, real *res, real *tau, real *otau,
                   real *t, real *alp, real *bet, real *s, real *p2,
                   real *bound, real *atemp, real *vtemp,
                   real *d, integer *ind, logical *small, logical *raritz,
                   real *delta, real *eps, integer *ierr);
static void sortqr_(const integer *nz, const integer *n, const integer *nblock, real *z, real *b);
static void svsort_(const integer *num, real *val, real *res, const integer *iflag,
                    real *v, const integer *nmvec, const integer *n, real *vec);

/* Table of constant values */
static integer c__0 = 0;
static integer c__1 = 1;
static real c__10 = 0.1f;
static real c__00 = 0.0f;

/*   VERSION 2    DOES NOT USE EISPACK */

/* ------------------------------------------------------------------ */

/* Subroutine */ void snlaso_(op, iovect, n, nval, nfig, nperm, nmval, val, nmvec, vec, nblock, maxop, maxj, work, ind, ierr)
void (*op) (const integer* n,const integer* m,const real* p,real* q);
void (*iovect) (const integer* n,const integer* m,real* q,const integer* j,const integer* k);
const integer *n, *nval, *nfig, *nmval;
integer *nperm;
real *val;
const integer *nmvec;
real *vec;
const integer *nblock, *maxop, *maxj;
real *work;
integer *ind, *ierr;
{
    /* Local variables */
    static real temp, tarr;
    static integer i, m, nband;
    static real delta;
    static logical small;
    static integer i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, nv;
    static logical raritz;
    static real eps;
    static integer nop;

/* AUTHOR/IMPLEMENTER D.S.SCOTT-B.N.PARLETT/D.S.SCOTT */
/*                                                    */
/* COMPUTER SCIENCES DEPARTMENT                       */
/* UNIVERSITY OF TEXAS AT AUSTIN                      */
/* AUSTIN, TX 78712                                   */
/*                                                    */
/* VERSION 2 ORIGINATED APRIL 1982                    */
/*                                                    */
/* CURRENT VERSION  JUNE 1983                         */

/* SNLASO FINDS A FEW EIGENVALUES AND EIGENVECTORS AT EITHER END OF     */
/* THE SPECTRUM OF A LARGE SPARSE SYMMETRIC MATRIX.  THE SUBROUTINE     */
/* SNLASO IS PRIMARILY A DRIVER FOR SUBROUTINE SNWLA WHICH IMPLEMENTS   */
/* THE LANCZOS ALGORITHM WITH SELECTIVE ORTHOGONALIZATION AND           */
/* SUBROUTINE SNPPLA WHICH POST PROCESSES THE OUTPUT OF SNWLA.          */
/* HOWEVER SNLASO DOES CHECK FOR INCONSISTENCIES IN THE CALLING         */
/* PARAMETERS AND DOES PREPROCESS ANY USER SUPPLIED EIGENPAIRS.         */
/* SNLASO ALWAYS LOOKS FOR THE SMALLEST (LEFTMOST) EIGENVALUES.  IF     */
/* THE LARGEST EIGENVALUES ARE DESIRED SNLASO IMPLICITLY USES THE       */
/* NEGATIVE OF THE MATRIX.                                              */
/*                                                                      */
/* ON INPUT                                                             */
/*                                                                      */
/*   OP   A USER SUPPLIED SUBROUTINE WITH CALLING SEQUENCE              */
/*     OP(N,M,P,Q).  P AND Q ARE N X M MATRICES AND Q IS                */
/*     RETURNED AS THE MATRIX TIMES P.                                  */
/*                                                                      */
/*   IOVECT   A USER SUPPLIED SUBROUTINE WITH CALLING SEQUENCE          */
/*     IOVECT(N,M,Q,J,K).  Q IS AN N X M MATRIX.  IF K = 0              */
/*     THE COLUMNS OF Q ARE STORED AS THE (J-M+1)TH THROUGH             */
/*     THE JTH LANCZOS VECTORS.  IF K = 1 THEN Q IS RETURNED            */
/*     AS THE (J-M+1)TH THROUGH THE JTH LANCZOS VECTORS.  SEE           */
/*     DOCUMENTATION FOR FURTHER DETAILS AND EXAMPLES.                  */
/*                                                                      */
/*   N   THE ORDER OF THE MATRIX.                                       */
/*                                                                      */
/*   NVAL   NVAL SPECIFIES THE EIGENVALUES TO BE FOUND.                 */
/*     ABS(NVAL)  IS THE NUMBER OF EIGENVALUES DESIRED.                 */
/*     IF NVAL < 0 THE ALGEBRAICALLY SMALLEST (LEFTMOST)                */
/*     EIGENVALUES ARE FOUND.  IF NVAL > 0 THE ALGEBRAICALLY            */
/*     LARGEST (RIGHTMOST) EIGENVALUES ARE FOUND.  NVAL MUST NOT        */
/*     BE ZERO.  ABS(NVAL) MUST BE LESS THAN  MAXJ/2.                   */
/*                                                                      */
/*   NFIG   THE NUMBER OF DECIMAL DIGITS OF ACCURACY DESIRED IN THE     */
/*     EIGENVALUES.  NFIG MUST BE GREATER THAN OR EQUAL TO 1.           */
/*                                                                      */
/*   NPERM   AN INTEGER VARIABLE WHICH SPECIFIES THE NUMBER OF USER     */
/*     SUPPLIED EIGENPAIRS.  IN MOST CASES NPERM WILL BE ZERO.  SEE     */
/*     DOCUMENTAION FOR FURTHER DETAILS OF USING NPERM GREATER          */
/*     THAN ZERO.  NPERM MUST NOT BE LESS THAN ZERO.                    */
/*                                                                      */
/*   NMVAL   THE ROW DIMENSION OF THE ARRAY VAL.  NMVAL MUST BE GREATER */
/*     THAN OR EQUAL TO ABS(NVAL).                                      */
/*                                                                      */
/*   VAL   A TWO DIMENSIONAL REAL ARRAY OF ROW                          */
/*     DIMENSION NMVAL AND COLUMN DIMENSION AT LEAST 4.  IF NPERM       */
/*     IS GREATER THAN ZERO THEN CERTAIN INFORMATION MUST BE STORED     */
/*     IN VAL.  SEE DOCUMENTATION FOR DETAILS.                          */
/*                                                                      */
/*   NMVEC   THE ROW DIMENSION OF THE ARRAY VEC.  NMVEC MUST BE GREATER */
/*     THAN OR EQUAL TO N.                                              */
/*                                                                      */
/*   VEC   A TWO DIMENSIONAL REAL ARRAY OF ROW                          */
/*     DIMENSION NMVEC AND COLUMN DIMENSION AT LEAST ABS(NVAL).  IF     */
/*     NPERM > 0 THEN THE FIRST NPERM COLUMNS OF VEC MUST               */
/*     CONTAIN THE USER SUPPLIED EIGENVECTORS.                          */
/*                                                                      */
/*   NBLOCK   THE BLOCK SIZE.  SEE DOCUMENTATION FOR CHOOSING           */
/*     AN APPROPRIATE VALUE FOR NBLOCK.  NBLOCK MUST BE GREATER         */
/*     THAN ZERO AND LESS THAN  MAXJ/6.                                 */
/*                                                                      */
/*   MAXOP   AN UPPER BOUND ON THE NUMBER OF CALLS TO THE SUBROUTINE    */
/*     OP.  SNLASO TERMINATES WHEN MAXOP IS EXCEEDED.  SEE              */
/*     DOCUMENTATION FOR GUIDELINES IN CHOOSING A VALUE FOR MAXOP.      */
/*                                                                      */
/*   MAXJ   AN INDICATION OF THE AVAILABLE STORAGE (SEE WORK AND        */
/*     DOCUMENTATION ON IOVECT).  FOR THE FASTEST CONVERGENCE MAXJ      */
/*     SHOULD BE AS LARGE AS POSSIBLE, ALTHOUGH IT IS USELESS TO HAVE   */
/*     MAXJ LARGER THAN MAXOP*NBLOCK.                                   */
/*                                                                      */
/*   WORK   A REAL ARRAY OF DIMENSION AT LEAST AS                       */
/*     LARGE AS                                                         */
/*                                                                      */
/*         2*N*NBLOCK + MAXJ*(NBLOCK+NV+2) + 2*NBLOCK*NBLOCK + 3*NV     */
/*                                                                      */
/*            + THE MAXIMUM OF                                          */
/*                 N*NBLOCK                                             */
/*                   AND                                                */
/*         MAXJ*(2*NBLOCK+3) + 2*NV + 6 + (2*NBLOCK+2)*(NBLOCK+1)       */
/*                                                                      */
/*     WHERE NV = ABS(NVAL)                                             */
/*                                                                      */
/*     THE FIRST N*NBLOCK ELEMENTS OF WORK MUST CONTAIN THE DESIRED     */
/*     STARTING VECTORS.  SEE DOCUMENTATION FOR GUIDELINES IN           */
/*     CHOOSING STARTING VECTORS.                                       */
/*                                                                      */
/*   IND   AN INTEGER ARRAY OF DIMENSION AT LEAST ABS(NVAL).            */
/*                                                                      */
/*   IERR   AN INTEGER VARIABLE.                                        */
/*                                                                      */
/* ON OUTPUT                                                            */
/*                                                                      */
/*   NPERM   THE NUMBER OF EIGENPAIRS NOW KNOWN.                        */
/*                                                                      */
/*   VEC   THE FIRST NPERM COLUMNS OF VEC CONTAIN THE EIGENVECTORS.     */
/*                                                                      */
/*   VAL   THE FIRST COLUMN OF VAL CONTAINS THE CORRESPONDING           */
/*     EIGENVALUES.  THE SECOND COLUMN CONTAINS THE RESIDUAL NORMS OF   */
/*     THE EIGENPAIRS WHICH ARE BOUNDS ON THE ACCURACY OF THE EIGEN-    */
/*     VALUES.  THE THIRD COLUMN CONTAINS MORE REALISTIC ESTIMATES      */
/*     OF THE ACCURACY OF THE EIGENVALUES.  THE FOURTH COLUMN CONTAINS  */
/*     ESTIMATES OF THE ACCURACY OF THE EIGENVECTORS.  SEE              */
/*     DOCUMENTATION FOR FURTHER INFORMATION ON THESE QUANTITIES.       */
/*                                                                      */
/*   WORK   IF WORK IS TERMINATED BEFORE COMPLETION (IERR = -2)         */
/*     THE FIRST N*NBLOCK ELEMENTS OF WORK CONTAIN THE BEST VECTORS     */
/*     FOR RESTARTING THE ALGORITHM AND SNLASO CAN BE IMMEDIATELY       */
/*     RECALLED TO CONTINUE WORKING ON THE PROBLEM.                     */
/*                                                                      */
/*   IND   IND(1)  CONTAINS THE ACTUAL NUMBER OF CALLS TO OP.  ON SOME  */
/*     OCCASIONS THE NUMBER OF CALLS TO OP MAY BE SLIGHTLY LARGER       */
/*     THAN MAXOP.                                                      */
/*                                                                      */
/*   IERR   AN ERROR COMPLETION CODE.  THE NORMAL COMPLETION CODE IS    */
/*     ZERO.  SEE THE DOCUMENTATION FOR INTERPRETATIONS OF NON-ZERO     */
/*     COMPLETION CODES.                                                */
/*                                                                      */
/* INTERNAL VARIABLES.                                                  */
/*                                                                      */
/* NOP   RETURNED FROM SNWLA AS THE NUMBER OF CALLS TO THE              */
/*   SUBROUTINE OP.                                                     */
/*                                                                      */
/* NV   SET EQUAL TO ABS(NVAL), THE NUMBER OF EIGENVALUES DESIRED,      */
/*   AND PASSED TO SNWLA.                                               */
/*                                                                      */
/* SMALL   SET TO .TRUE. IF THE SMALLEST EIGENVALUES ARE DESIRED.       */
/*                                                                      */
/* RARITZ   RETURNED FROM SNWLA AND PASSED TO SNPPLA.  RARITZ IS .TRUE. */
/*   IF A FINAL RAYLEIGH-RITZ PROCEDURE IS NEEDED.                      */
/*                                                                      */
/* DELTA   RETURNED FROM SNWLA AS THE EIGENVALUE OF THE MATRIX          */
/*   WHICH IS CLOSEST TO THE DESIRED EIGENVALUES.                       */
/*                                                                      */
/* SNPPLA   A SUBROUTINE FOR POST-PROCESSING THE EIGENVECTORS COMPUTED  */
/*   BY SNWLA.                                                          */
/*                                                                      */
/* SNWLA   A SUBROUTINE FOR IMPLEMENTING THE LANCZOS ALGORITHM          */
/*   WITH SELECTIVE ORTHOGONALIZATION.                                  */
/*                                                                      */
/* SMVPC   A SUBROUTINE FOR COMPUTING THE RESIDUAL NORM AND             */
/*   ORTHOGONALITY COEFFICIENT OF GIVEN RITZ VECTORS.                   */
/*                                                                      */
/* SORTQR   A SUBROUTINE FOR ORTHONORMALIZING A BLOCK OF VECTORS        */
/*   USING HOUSEHOLDER REFLECTIONS.                                     */
/*                                                                      */
/* SAXPY,SCOPY,SDOT,SNRM2,SSCAL,SSWAP   A SUBSET OF THE BASIC LINEAR    */
/*   ALGEBRA SUBPROGRAMS USED FOR VECTOR MANIPULATION.                  */
/*                                                                      */
/* SLARAN   A SUBROUTINE TO GENERATE RANDOM VECTORS                     */
/*                                                                      */
/* SLAEIG, SLAGER, SLABCM, SLABFC   SUBROUTINES FOR BAND EIGENVALUE     */
/*   CALCULATIONS.                                                      */
/*                                                                      */
/* ------------------------------------------------------------------   */

/* THIS SECTION CHECKS FOR INCONSISTENCY IN THE INPUT PARAMETERS. */

    nv = abs(*nval);
    ind[0] = 0;
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
        return;
    }

    small = *nval < 0;

/* ------------------------------------------------------------------ */

/* THIS SECTION SORTS AND ORTHONORMALIZES THE USER SUPPLIED VECTORS. */
/* IF A USER SUPPLIED VECTOR IS ZERO OR IF SIGNIFICANT CANCELLATION  */
/* OCCURS IN THE ORTHOGONALIZATION PROCESS THEN IERR IS SET TO  -1   */
/* AND SNLASO TERMINATES. */

    if (*nperm == 0) {
        goto L110;
    }

/* THIS NEGATES THE USER SUPPLIED EIGENVALUES WHEN THE LARGEST */
/* EIGENVALUES ARE DESIRED, SINCE SNWLA WILL IMPLICITLY USE THE */
/* NEGATIVE OF THE MATRIX. */

    if (!small)
    for (i = 0; i < *nperm; ++i) {
        val[i] = -val[i];
    }

/* THIS SORTS THE USER SUPPLIED VALUES AND VECTORS. */

    svsort_(nperm, val, &val[*nmval], &c__0, &tarr, nmvec, n, vec);

/* THIS STORES THE NORMS OF THE VECTORS FOR LATER COMPARISON. */
/* IT ALSO INSURES THAT THE RESIDUAL NORMS ARE POSITIVE. */

    for (i = 0; i < *nperm; ++i) {
        val[i + *nmval] = abs(val[i + *nmval]);
        val[i + *nmval * 2] = snrm2_(n, &vec[i * *nmvec], &c__1);
    }

/* THIS PERFORMS THE ORTHONORMALIZATION. */

    m = *n * *nblock;
    sortqr_(nmvec, n, nperm, vec, &work[m]);
    for (i = 0; i < *nperm; ++i, m += *nperm + 1) {
        if (abs(work[m]) <= val[i + *nmval * 2] * .9f) {
            *ierr = -1;
            return;
        }
    }

/* THIS COPIES THE RESIDUAL NORMS INTO THE CORRECT LOCATIONS IN */
/* THE ARRAY WORK FOR LATER REFERENCE IN SNWLA. */

    m = (*n << 1) * *nblock;
    scopy_(nperm, &val[*nmval], &c__1, &work[m], &c__1);

/* THIS SETS EPS TO AN APPROXIMATION OF THE RELATIVE MACHINE */
/* PRECISION */

/* ***THIS SHOULD BE REPLACED BY AN ASSIGNMENT STATEMENT */
/* ***IN A PRODUCTION CODE */

L110:
    eps = 1.f;
    for (i = 0; i < 1000; ++i) {
        eps *= .5f;
        temp = eps + 1.f;
        if (temp == 1.f) {
            break;
        }
    }

/* ------------------------------------------------------------------ */

/* THIS SECTION CALLS SNWLA WHICH IMPLEMENTS THE LANCZOS ALGORITHM */
/* WITH SELECTIVE ORTHOGONALIZATION. */

    nband = *nblock + 1;
    i1 = *n * *nblock;
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
    snwla_(op, iovect, n, &nband, &nv, nfig, nperm, val, nmvec, vec, nblock,
           maxop, maxj, &nop, work, &work[i1], &work[i2], &work[i3], &work[i4],
           &work[i5], &work[i6], &work[i7], &work[i8], &work[i9], &work[i10],
           &work[i11], &work[i12], &work[i13], ind, &small, &raritz, &delta, &eps, ierr);

/* ------------------------------------------------------------------ */

/* THIS SECTION CALLS SNPPLA (THE POST PROCESSOR). */

    if (*nperm == 0) {
        ind[0] = nop;
        return;
    }
    i1 = *n * *nblock;
    i2 = i1 + *nperm * *nperm;
    i3 = i2 + *nperm * *nperm;
    i4 = i3 + max(*n * *nblock, 2 * *nperm * *nperm);
    i5 = i4 + *n * *nblock;
    i6 = i5 + (*nperm << 1) + 4;
    snppla_(op, iovect, n, nperm, &nop, nmval, val, nmvec, vec, nblock,
            &work[i1], &work[i2], &work[i3], &work[i4], &work[i5], &work[i6],
            &delta, &small, &raritz, &eps);

    ind[0] = nop;
    return;
} /* snlaso_ */


/* ------------------------------------------------------------------ */

/* Subroutine */
static void snwla_(op, iovect, n, nband, nval, nfig, nperm, val,
                   nmvec, vec, nblock, maxop, maxj, nop, p1, p0, res, tau, otau, t, alp,
                   bet, s, p2, bound, atemp, vtemp, d, ind, small, raritz, delta, eps, ierr)
/* Subroutine */
void (*op) (const integer*,const integer*,const real*,real*);
/* Subroutine */
void (*iovect) (const integer*,const integer*,real*,const integer*,const integer*);
const integer *n, *nband, *nval, *nfig;
integer *nperm;
real *val;
const integer *nmvec;
real *vec;
const integer *nblock, *maxop, *maxj;
integer *nop;
real *p1, *p0, *res, *tau, *otau, *t, *alp, *bet, *s, *p2, *bound, *atemp, *vtemp, *d;
integer *ind;
logical *small, *raritz;
real *delta, *eps;
integer *ierr;
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Local variables */
    static real tola, temp, tolg, tmin, tmax, tarr;
    static logical test;
    static real zero=0.f, utol;
    static integer i, j, k, l, m;
    static integer ngood, nleft;
    static real anorm;
    static integer mtemp;
    static integer i1;
    static real pnorm, epsrt, rnorm;
    static integer ng;
    static real betmin, alpmin, betmax, alpmax;
    static integer ntheta;
    static logical enough;
    static integer number, nstart;

/* SNWLA IMPLEMENTS THE LANCZOS ALGORITHM WITH SELECTIVE              */
/* ORTHOGONALIZATION.                                                 */
/*                                                                    */
/* NBAND  NBLOCK + 1  THE BAND WIDTH OF T.                            */
/*                                                                    */
/* NVAL   THE NUMBER OF DESIRED EIGENVALUES.                          */
/*                                                                    */
/* NPERM   THE NUMBER OF PERMANENT VECTORS (THOSE EIGENVECTORS        */
/*         INPUT BY THE USER OR THOSE EIGENVECTORS SAVED WHEN THE     */
/*         ALGORITHM IS ITERATED).  PERMANENT VECTORS ARE ORTHOGONAL  */
/*         TO THE CURRENT KRYLOV SUBSPACE.                            */
/*                                                                    */
/* NOP   THE NUMBER OF CALLS TO OP.                                   */
/*                                                                    */
/* P0, P1, AND P2   THE CURRENT BLOCKS OF LANCZOS VECTORS.            */
/*                                                                    */
/* RES   THE (APPROXIMATE) RESIDUAL NORMS OF THE PERMANENT VECTORS.   */
/*                                                                    */
/* TAU AND OTAU   USED TO MONITOR THE NEED FOR ORTHOGONALIZATION.     */
/*                                                                    */
/* T     THE BAND MATRIX.                                             */
/*                                                                    */
/* ALP   THE CURRENT DIAGONAL BLOCK.                                  */
/*                                                                    */
/* BET   THE CURRENT OFF DIAGONAL BLOCK.                              */
/*                                                                    */
/* BOUND, ATEMP, VTEMP, D                                             */
/*       TEMPORARY STORAGE USED BY THE BAND EIGENVALUE SOLVER SLAEIG. */
/*                                                                    */
/* S     EIGENVECTORS OF T.                                           */
/*                                                                    */
/* SMALL   .TRUE.  IF THE SMALL EIGENVALUES ARE DESIRED.              */
/*                                                                    */
/* RARITZ  RETURNED AS  .TRUE.  IF A FINAL RAYLEIGH-RITZ PROCEDURE    */
/*         IS TO BE DONE.                                             */
/*                                                                    */
/* DELTA   RETURNED AS THE VALUE OF THE (NVAL+1)TH EIGENVALUE         */
/*         OF THE MATRIX.  USED IN ESTIMATING THE ACCURACY OF THE     */
/*         COMPUTED EIGENVALUES.                                      */
/*                                                                    */
/* INTERNAL VARIABLES USED                                            */
/*                                                                    */
/* J      THE CURRENT DIMENSION OF T.  (THE DIMENSION OF THE CURRENT  */
/*        KRYLOV SUBSPACE.                                            */
/*                                                                    */
/* NGOOD  THE NUMBER OF GOOD RITZ VECTORS (GOOD VECTORS               */
/*        LIE IN THE CURRENT KRYLOV SUBSPACE).                        */
/*                                                                    */
/* NLEFT  THE NUMBER OF VALUES WHICH REMAIN TO BE DETERMINED,         */
/*        I.E.  NLEFT = NVAL - NPERM.                                 */
/*                                                                    */
/* NUMBER = NPERM + NGOOD.                                            */
/*                                                                    */
/* ANORM  AN ESTIMATE OF THE NORM OF THE MATRIX.                      */
/*                                                                    */
/* EPS    THE RELATIVE MACHINE PRECISION.                             */
/*                                                                    */
/* UTOL   THE USER TOLERANCE.                                         */
/*                                                                    */
/* TARR   AN ARRAY OF LENGTH ONE USED TO INSURE TYPE CONSISTENCY IN   */
/*        CALLS TO SLAEIG                                             */
/*                                                                    */
/* ZERO   AN ARRAY OF LENGTH ONE CONTAINING ZERO, USED TO INSURE TYPE */
/*        CONSISTENCY IN CALLS TO SCOPY                               */

    rnorm = 0.f;
    if (*nperm != 0) {
        rnorm = max(-val[0],val[*nperm-1]);
    }
    pnorm = rnorm;
    *delta = 1e31f;
    epsrt = sqrtf(*eps);
    nleft = *nval - *nperm;
    *nop = 0;
    number = *nperm;
    *raritz = FALSE_;
    utol = max((*n) * *eps, pow_ri(&c__10, nfig));
    j = *maxj;

/* ------------------------------------------------------------------ */

/* ANY ITERATION OF THE ALGORITHM BEGINS HERE. */

L30:
    for (i = 0; i < *nblock; ++i) {
        temp = snrm2_(n, &p1[i * *n], &c__1);
        if (temp == 0.f) {
            slaran_(n, &p1[i * *n]);
        }
    }
    for (i = 0; i < *nperm; ++i) {
        tau[i] = 1.f;
        otau[i] = 0.f;
    }
    i__1 = *n * *nblock;
    scopy_(&i__1, &zero, &c__0, p0, &c__1);
    i__1 = *nblock * *nblock;
    scopy_(&i__1, &zero, &c__0, bet, &c__1);
    i__1 = j * *nband;
    scopy_(&i__1, &zero, &c__0, t, &c__1);
    mtemp = *nval + 1;
    for (i = 0; i < mtemp; ++i) {
        scopy_(&j, &zero, &c__0, &s[i * *maxj], &c__1);
    }
    ngood = 0;
    tmin = 1e30f;
    tmax = -1e30f;
    test = TRUE_;
    enough = FALSE_;
    betmax = 0.f;
    j = 0;

/* ------------------------------------------------------------------ */

/* THIS SECTION TAKES A SINGLE BLOCK LANCZOS STEP. */

L80:
    j += *nblock;

/* THIS IS THE SELECTIVE ORTHOGONALIZATION. */

    for (i = 0; i < number; ++i) {
        if (tau[i] < epsrt) {
            continue;
        }
        test = TRUE_;
        tau[i] = 0.f;
        if (otau[i] != 0.f) {
            otau[i] = 1.f;
        }
        for (k = 0; k < *nblock; ++k) {
            temp = -sdot_(n, &vec[i * *nmvec], &c__1, &p1[k * *n], &c__1);
            saxpy_(n, &temp, &vec[i * *nmvec], &c__1, &p1[k * *n], &c__1);

/* THIS CHECKS FOR TOO GREAT A LOSS OF ORTHOGONALITY BETWEEN A */
/* NEW LANCZOS VECTOR AND A GOOD RITZ VECTOR.  THE ALGORITHM IS */
/* TERMINATED IF TOO MUCH ORTHOGONALITY IS LOST. */

            if (abs(temp * bet[k + k * *nblock]) > (*n) * epsrt * anorm && i >= *nperm) {
                goto L380;
            }
        }
    }

/* IF NECESSARY, THIS REORTHONORMALIZES P1 AND UPDATES BET. */

    if (test)
        sortqr_(n, n, nblock, p1, alp);
    if (test && j != *nblock)
    for (i = 0; i < *nblock; ++i) {
        if (alp[i + i * *nblock] > 0.f) {
            continue;
        }
        m = j - (*nblock << 1) + i;
        l = *nblock;
        for (k = i; k < *nblock; ++k, --l, ++m) {
            bet[i + k * *nblock] = -bet[i + k * *nblock];
            t[l + m * *nband] = -t[l + m * *nband];
        }
    }
    test = FALSE_;

/* THIS IS THE LANCZOS STEP. */

    (*op)(n, nblock, p1, p2);
    ++(*nop);
    (*iovect)(n, nblock, p1, &j, &c__0);

/* THIS COMPUTES P2=P2-P0*BET(TRANSPOSE) */

    for (i = 0; i < *nblock; ++i) {
        for (k = i; k < *nblock; ++k) {
            r__1 = -bet[i + k * *nblock];
            saxpy_(n, &r__1, &p0[k * *n], &c__1, &p2[i * *n], &c__1);
        }
    }

/* THIS COMPUTES ALP AND P2=P2-P1*ALP. */

    for (i = 0; i < *nblock; ++i) {
        for (k = 0; k <= i; ++k) {
            i1 = i - k;
            alp[i1 + k * *nblock] = sdot_(n, &p1[i * *n], &c__1, &p2[k * *n], &c__1);
            r__1 = -alp[i1 + k * *nblock];
            saxpy_(n, &r__1, &p1[i * *n], &c__1, &p2[k * *n], &c__1);
            if (k != i) {
                r__1 = -alp[i1 + k * *nblock];
                saxpy_(n, &r__1, &p1[k * *n], &c__1, &p2[i * *n], &c__1);
            }
        }
    }

/*  REORTHOGONALIZATION OF THE SECOND BLOCK */

    if (j == *nblock)
    for (i = 0; i < *nblock; ++i) {
        for (k = 0; k <= i; ++k) {
            temp = -sdot_(n, &p1[i * *n], &c__1, &p2[k * *n], &c__1);
            saxpy_(n, &temp, &p1[i * *n], &c__1, &p2[k * *n], &c__1);
            if (k != i) {
                saxpy_(n, &temp, &p1[k * *n], &c__1, &p2[i * *n], &c__1);
            }
            i1 = i - k;
            alp[i1 + k * *nblock] += temp;
        }
    }

/* THIS ORTHONORMALIZES THE NEXT BLOCK */

    sortqr_(n, n, nblock, p2, bet);

/* THIS STORES ALP AND BET IN T. */

    for (i = 0; i < *nblock; ++i) {
        m = j - *nblock + i;
        for (k = i; k < *nblock; ++k) {
            l = k - i;
            t[l + m * *nband] = alp[l + i * *nblock];
        }
        for (k = 0; k <= i; ++k) {
            l = *nblock - i + k;
            t[l + m * *nband] = bet[k + i * *nblock];
        }
    }

/* THIS NEGATES T IF SMALL IS FALSE. */

    if (! *small)
    for (i = j - *nblock; i < j; ++i) {
        for (k = 0; k <= l; ++k) { /* FIXME *** This must be an error! (already in the fortran code) -- l is undefined *** */
            t[k + i * *nband] = -t[k + i * *nband];
        }
    }

/* THIS SHIFTS THE LANCZOS VECTORS */

    i__1 = *nblock * *n;
    scopy_(&i__1, p1, &c__1, p0, &c__1);
    scopy_(&i__1, p2, &c__1, p1, &c__1);
    i__1 = j - *nblock + 1;
    slager_(&j, nband, &i__1, t, &tmin, &tmax);
    anorm = max(max(rnorm,tmax),-tmin);

/* THIS COMPUTES THE EXTREME EIGENVALUES OF ALP. */

    if (number != 0) {
        scopy_(nblock, &zero, &c__0, p2, &c__1);
        slaeig_(nblock, nblock, &c__1, &c__1, alp, &tarr, nblock, p2, bound, atemp, d, vtemp, eps, &tmin, &tmax);
        alpmin = tarr;
        scopy_(nblock, &zero, &c__0, p2, &c__1);
        slaeig_(nblock, nblock, nblock, nblock, alp, &tarr, nblock, p2, bound, atemp, d, vtemp, eps, &tmin, &tmax);
        alpmax = tarr;
    }

/* THIS COMPUTES ALP = BET(TRANSPOSE)*BET. */

    for (i = 0; i < *nblock; ++i) {
        for (k = 0; k <= i; ++k) {
            l = i - k;
            i__1 = *nblock - i;
            alp[l + k * *nblock] = sdot_(&i__1, &bet[i + i * *nblock], nblock, &bet[k + i * *nblock], nblock);
        }
    }
    if (number == 0) {
        goto L330;
    }

/* THIS COMPUTES THE SMALLEST SINGULAR VALUE OF BET. */

    scopy_(nblock, &zero, &c__0, p2, &c__1);
    r__1 = anorm * anorm;
    slaeig_(nblock, nblock, &c__1, &c__1, alp, &tarr, nblock, p2, bound, atemp, d, vtemp, eps, &c__00, &r__1);
    betmin = sqrtf(tarr);

/* THIS UPDATES TAU AND OTAU. */

    for (i = 0; i < number; ++i) {
        temp = (tau[i] * max(alpmax-val[i],val[i]-alpmin) + otau[i] * betmax + *eps * anorm) / betmin;
        if (i < *nperm) {
            temp += res[i] / betmin;
        }
        otau[i] = tau[i];
        tau[i] = temp;
    }

/* THIS COMPUTES THE LARGEST SINGULAR VALUE OF BET. */

L330:
    scopy_(nblock, &zero, &c__0, p2, &c__1);
    r__1 = anorm * anorm;
    slaeig_(nblock, nblock, nblock, nblock, alp, &tarr, nblock, p2, bound, atemp, d, vtemp, eps, &c__00, &r__1);
    betmax = sqrtf(tarr);
    if (j <= *nblock << 1) {
        goto L80;
    }

/* ------------------------------------------------------------------ */

/* THIS SECTION COMPUTES AND EXAMINES THE SMALLEST NONGOOD AND */
/* LARGEST DESIRED EIGENVALUES OF T TO SEE IF A CLOSER LOOK */
/* IS JUSTIFIED. */

    tolg = epsrt * anorm;
    tola = utol * rnorm;
    if (*maxj - j < *nblock || ( *nop >= *maxop && nleft != 0 ) ) {
        goto L390;
    }
    else
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
        return;
    }
    test = TRUE_;
L400:
    ntheta = min(j/2, nleft+1);
    slaeig_(&j, nband, &c__1, &ntheta, t, &val[number], maxj, s, bound, atemp, d, vtemp, eps, &tmin, &tmax);
    smvpc_(nblock, bet, maxj, &j, s, &ntheta, atemp, vtemp, d);

/* THIS CHECKS FOR TERMINATION OF A CHECK RUN */

    if (nleft == 0 && j >= *nblock * 6) {
        if (val[number] - atemp[0] > val[*nperm-1] - tola) {
            goto L790;
        }
    }

/* THIS UPDATES NLEFT BY EXAMINING THE COMPUTED EIGENVALUES OF T */
/* TO DETERMINE IF SOME PERMANENT VALUES ARE NO LONGER DESIRED. */

    if (ntheta <= nleft) {
        goto L470;
    }
    if (*nperm != 0 && val[number+nleft] < val[*nperm-1]) {
        --(*nperm);
        ngood = 0;
        number = *nperm;
        ++nleft;
        goto L400;
    }

/* THIS UPDATES DELTA. */

    *delta = min(*delta,val[number+nleft]);
    enough = TRUE_;
    if (nleft == 0) {
        goto L80;
    }
    ntheta = nleft;
    vtemp[ntheta] = 1.f;

/* ------------------------------------------------------------------ */

/* THIS SECTION EXAMINES THE COMPUTED EIGENPAIRS IN DETAIL. */

/* THIS CHECKS FOR ENOUGH ACCEPTABLE VALUES. */

    if (! (test || enough)) {
        goto L470;
    }
    *delta = min(*delta,anorm);
    pnorm = max(rnorm,max(-val[number],*delta));
    tola = utol * pnorm;
    nstart = 0;
    for (i = 0; i < ntheta; ++i) {
        if (min(atemp[i]*atemp[i]/(*delta-val[number+i]), atemp[i]) <= tola) {
            ind[i] = -1;
            continue;
        }
        enough = FALSE_;
        if (! test) {
            goto L470;
        }
        ind[i] = 1;
        ++nstart;
    }

/*  COPY VALUES OF IND INTO VTEMP */

    for (i = 0; i < ntheta; ++i) {
        vtemp[i] = (real) ind[i];
    }
    goto L500;

/* THIS CHECKS FOR NEW GOOD VECTORS. */

L470:
    ng = 0;
    for (i = 0; i < ntheta; ++i) {
        if (vtemp[i] > tolg) {
            vtemp[i] = 1.f;
        }
        else {
            ++ng;
            vtemp[i] = -1.f;
        }
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

    scopy_(&ntheta, &val[number], &c__1, &val[*nperm], &c__1);
    if (nstart == 0) {
        goto L580;
    }
    if (nstart != ntheta) {
        svsort_(&ntheta, vtemp, atemp, &c__1, &val[*nperm], maxj, &j, s);
    }

/* THES ACCUMULATES THE J-VECTORS USED TO FORM THE STARTING */
/* VECTORS. */

    if (! test) {
        nstart = 0;
    }
    if (! test) {
        goto L580;
    }

/*  FIND MINIMUM ATEMP VALUE TO AVOID POSSIBLE OVERFLOW */

    temp = atemp[0];
    for (i = 0; i < nstart; ++i) {
        temp = min(temp,atemp[i]);
    }
    l = ngood + min(nstart,*nblock);
    for (i = ngood; i < l; ++i) {
        r__1 = temp / atemp[i];
        sscal_(&j, &r__1, &s[i * *maxj], &c__1);
    }
    m = (nstart - 1) / *nblock;
    l = ngood + *nblock;
    for (i = 0; i < m; ++i) {
        for (k = 0; k < *nblock; ++k, ++l) {
            if (l >= ntheta) {
                goto L570;
            }
            i1 = ngood + k;
            r__1 = temp / atemp[l];
            saxpy_(&j, &r__1, &s[l * *maxj], &c__1, &s[i1 * *maxj], &c__1);
        }
    }
L570:
    nstart = min(nstart,*nblock);

/* THIS STORES THE RESIDUAL NORMS OF THE NEW PERMANENT VECTORS. */

L580:
    if (test || enough)
    for (i = 0; i < ngood; ++i) {
        res[*nperm+i] = atemp[i];
    }

/* THIS COMPUTES THE RITZ VECTORS BY SEQUENTIALLY RECALLING THE */
/* LANCZOS VECTORS. */

    number = *nperm + ngood;
    if (test || enough) {
        i__1 = *n * *nblock;
        scopy_(&i__1, &zero, &c__0, p1, &c__1);
    }
    if (ngood != 0)
    for (i = *nperm; i < number; ++i) {
        scopy_(n, &zero, &c__0, &vec[i * *nmvec], &c__1);
    }
    for (i = *nblock; *nblock < 0 ? i >= j : i <= j; i += *nblock) {
        (*iovect)(n, nblock, p2, &i, &c__1);
        for (k = 0; k < *nblock; ++k) {
            m = i - *nblock + k;
            for (l = 0; l < nstart; ++l) {
                i1 = ngood + l;
                saxpy_(n, &s[m + i1 * *maxj], &p2[k * *n], &c__1, &p1[l * *n], &c__1);
            }
            for (l = 0; l < ngood; ++l) {
                i1 = l + *nperm;
                saxpy_(n, &s[m + l * *maxj], &p2[k * *n], &c__1, &vec[i1 * *nmvec], &c__1);
            }
        }
    }
    if (test || enough) {
        goto L690;
    }

/* THIS NORMALIZES THE RITZ VECTORS AND INITIALIZES THE */
/* TAU RECURRENCE. */

    for (i = *nperm; i < number; ++i) {
        temp = 1.f / snrm2_(n, &vec[i * *nmvec], &c__1);
        sscal_(n, &temp, &vec[i * *nmvec], &c__1);
        tau[i] = 1.f;
        otau[i] = 1.f;
    }

/*  SHIFT S VECTORS TO ALIGN FOR LATER CALL TO SLAEIG */

    scopy_(&ntheta, &val[*nperm], &c__1, vtemp, &c__1);
    svsort_(&ntheta, vtemp, atemp, &c__0, &tarr, maxj, &j, s);
    goto L80;

/* ------------------------------------------------------------------ */

/* THIS SECTION PREPARES TO ITERATE THE ALGORITHM BY SORTING THE */
/* PERMANENT VALUES, RESETTING SOME PARAMETERS, AND ORTHONORMALIZING */
/* THE PERMANENT VECTORS. */

L690:
    if (ngood == 0 && *nop >= *maxop) {
        *ierr = -2; /* THIS REPORTS THAT MAXOP WAS EXCEEDED. */
        goto L790;
    }
    if (ngood == 0) {
        goto L30;
    }

/* THIS ORTHONORMALIZES THE VECTORS */

    i__1 = *nperm + ngood;
    sortqr_(nmvec, n, &i__1, vec, s);

/* THIS SORTS THE VALUES AND VECTORS. */

    if (*nperm != 0) {
        i__1 = *nperm + ngood;
        svsort_(&i__1, val, res, &c__0, &temp, nmvec, n, vec);
    }
    *nperm += ngood;
    nleft -= ngood;
    rnorm = max(-val[0],val[*nperm-1]);

/* THIS DECIDES WHERE TO GO NEXT. */

    if (*nop >= *maxop && nleft != 0) {
        *ierr = -2; /* THIS REPORTS THAT MAXOP WAS EXCEEDED. */
        goto L790;
    }
    if (nleft != 0) {
        goto L30;
    }
    if (val[*nval-1] - val[0] < tola) {
        goto L790;
    }

/* THIS DOES A CLUSTER TEST TO SEE IF A CHECK RUN IS NEEDED */
/* TO LOOK FOR UNDISCLOSED MULTIPLICITIES. */

    m = *nperm - *nblock;
    for (i = 0; i <= m; ++i) {
        if (val[i + *nblock - 1] - val[i] < tola) {
            goto L30;
        }
    }

/* THIS DOES A CLUSTER TEST TO SEE IF A FINAL RAYLEIGH-RITZ */
/* PROCEDURE IS NEEDED. */

L790:
    m = *nperm - *nblock;
    for (i = 0; i < m; ++i) {
        if (val[i + *nblock] - val[i] < tola) {
            *raritz = TRUE_;
            break;
        }
    }
} /* snwla_ */


/* *********************************************************************** */

/* Subroutine */
static void slabax_(n, nband, a, x, y)
const integer *n, *nband;
real *a, *x, *y;
{
    /* Local variables */
    static real zero = 0.f;
    static integer i, k, m;

/*  THIS SUBROUTINE SETS Y = A*X */
/*  WHERE X AND Y ARE VECTORS OF LENGTH N */
/*  AND A IS AN  N X NBAND  SYMMETRIC BAND MATRIX */

    scopy_(n, &zero, &c__0, y, &c__1);
    for (k = 0; k < *n; ++k) {
        y[k] += a[k * *nband] * x[k];
        m = min(*n-k,*nband);
        for (i = 1; i < m; ++i) {
            y[k+i] += a[i + k * *nband] * x[k];
            y[k]   += a[i + k * *nband] * x[k+i];
        }
    }
} /* slabax_ */


/* *********************************************************************** */

/* Subroutine */
static void slabcm_(n, nband, nl, nr, a, eigval, lde, eigvec, atol, artol, bound, atemp, d, vtemp)
const integer *n, *nband, *nl, *nr;
real *a, *eigval;
const integer *lde;
real *eigvec, *atol, *artol, *bound, *atemp, *d, *vtemp;
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Local variables */
    static logical flag_;
    static real errb;
    static integer nval, numl;
    static integer i, j;
    static real sigma, resid;
    static real vnorm;
    static real rq;
    static integer numvec;
    static real gap;

/*  THIS SUBROUTINE ORGANIZES THE CALCULATION OF THE EIGENVALUES */
/*  FOR THE BNDEIG PACKAGE.  EIGENVALUES ARE COMPUTED BY */
/*  A MODIFIED RAYLEIGH QUOTIENT ITERATION.  THE EIGENVALUE COUNT */
/*  OBTAINED BY EACH FACTORIZATION IS USED TO OCCASIONALLY OVERRIDE */
/*  THE COMPUTED RAYLEIGH QUOTIENT WITH A DIFFERENT SHIFT TO */
/*  INSURE CONVERGENCE TO THE DESIRED EIGENVALUES. */

/*  REPLACE ZERO VECTORS BY RANDOM */

    nval = *nr - *nl + 1;
    flag_ = FALSE_;
    for (i = 0; i < nval; ++i) {
        if (sdot_(n, &eigvec[i * *lde], &c__1, &eigvec[i * *lde], &c__1) == 0.f) {
            slaran_(n, &eigvec[i * *lde]);
        }
    }

/*  LOOP OVER EIGENVALUES */

    sigma = bound[(nval << 1) + 1];
    for (j = 0; j < nval; ++j) {
        numl = j+1;

/*  PREPARE TO COMPUTE FIRST RAYLEIGH QUOTIENT */

L10:
        slabax_(n, nband, a, &eigvec[j * *lde], vtemp);
        vnorm = snrm2_(n, vtemp, &c__1);
        if (vnorm != 0.f) {
            r__1 = 1.f / vnorm;
            sscal_(n, &r__1, vtemp, &c__1);
            sscal_(n, &r__1, &eigvec[j * *lde], &c__1);
            r__1 = -sigma;
            saxpy_(n, &r__1, &eigvec[j * *lde], &c__1, vtemp, &c__1);
        }

/*  LOOP OVER SHIFTS */

/*  COMPUTE RAYLEIGH QUOTIENT, RESIDUAL NORM, AND CURRENT TOLERANCE */

L20:
        vnorm = snrm2_(n, &eigvec[j * *lde], &c__1);
        if (vnorm == 0.f) {
            slaran_(n, &eigvec[j * *lde]);
            goto L10;
        }

        rq = sigma + sdot_(n, &eigvec[j * *lde], &c__1, vtemp, &c__1) / vnorm / vnorm;
        r__1 = sigma - rq;
        saxpy_(n, &r__1, &eigvec[j * *lde], &c__1, vtemp, &c__1);
        resid = max(*atol,snrm2_(n, vtemp, &c__1) / vnorm);
        r__1 = 1.f / vnorm;
        sscal_(n, &r__1, &eigvec[j * *lde], &c__1);

/*  ACCEPT EIGENVALUE IF THE INTERVAL IS SMALL ENOUGH */

        if (bound[(j << 1) + 3] - bound[(j << 1) + 2] < *atol * 3.f) {
            goto L300;
        }

/*  COMPUTE MINIMAL ERROR BOUND */

        errb = resid;
        gap = min(bound[(j << 1) + 4] - rq,rq - bound[(j << 1) + 1]);
        if (gap > resid) {
            errb = max(*atol,resid * resid / gap);
        }

/*  TENTATIVE NEW SHIFT */

        sigma = (bound[(j << 1) + 2] + bound[(j << 1) + 3]) * .5f;

/*  CHECK FOR TERMINALTION */

        if (resid > *atol * 2.f) {
            goto L40;
        }
        if (rq - errb > bound[(j << 1) + 1] && rq + errb < bound[(j << 1) + 4]) {
            goto L310;
        }

/*  RQ IS TO THE LEFT OF THE INTERVAL */

L40:
        if (rq >= bound[(j << 1) + 2]) {
            goto L50;
        }
        if (rq - errb > bound[(j << 1) + 1]) {
            goto L100;
        }
        if (rq + errb < bound[(j << 1) + 2]) {
            slaran_(n, &eigvec[j * *lde]);
        }
        goto L200;

/*  RQ IS TO THE RIGHT OF THE INTERVAL */

L50:
        if (rq <= bound[(j << 1) + 3]) {
            goto L100;
        }
        if (rq + errb < bound[(j << 1) + 4]) {
            goto L100;
        }

/*  SAVE THE REJECTED VECTOR IF INDICATED */

        if (rq - errb <= bound[(j << 1) + 3]) {
            goto L200;
        }
        for (i = j; i < nval; ++i) {
            if (bound[(i << 1) + 3] > rq) {
                scopy_(n, &eigvec[j * *lde], &c__1, &eigvec[i * *lde], &c__1);
                break;
            }
        }
        slaran_(n, &eigvec[j * *lde]);
        goto L200;

/*  PERTURB RQ TOWARD THE MIDDLE */

L100:
        if (sigma < rq-errb) {
            sigma = rq-errb;
        }
        if (sigma > rq+errb) {
            sigma = rq+errb;
        }

/*  FACTOR AND SOLVE */

L200:
        for (i = j; i < nval; ++i) {
            if (sigma < bound[(i << 1) + 2]) {
                break;
            }
        }
        numvec = i - j;
        numvec = min(numvec,*nband+2);
        if (resid < *artol) {
            numvec = min(1,numvec);
        }
        scopy_(n, &eigvec[j * *lde], &c__1, vtemp, &c__1);
        i__1 = (*nband << 1) - 1;
        slabfc_(n, nband, a, &sigma, &numvec, lde, &eigvec[j * *lde], &numl, &i__1, atemp, d, atol);

/*  PARTIALLY SCALE EXTRA VECTORS TO PREVENT UNDERFLOW OR OVERFLOW */

        for (i = j+1; i < numvec+j; ++i) {
            r__1 = 1.f / vnorm;
            sscal_(n, &r__1, &eigvec[i * *lde], &c__1);
        }

/*  UPDATE INTERVALS */

        numl -= *nl - 1;
        if (numl >= 0) {
            bound[1] = min(bound[1],sigma);
        }
        for (i = j; i < nval; ++i) {
            if (sigma < bound[(i << 1) + 2]) {
                goto L20;
            }
            if (numl <= i)
                bound[(i << 1) + 2] = sigma;
            else
                bound[(i << 1) + 3] = sigma;
        }
        if (numl < nval + 1) {
            if (sigma > bound[(nval << 1) + 2])
                bound[(nval << 1) + 2] = sigma;
        }
        goto L20;

/*  ACCEPT AN EIGENPAIR */

L300:
        slaran_(n, &eigvec[j * *lde]);
        flag_ = TRUE_;
        goto L310;

L305:
        flag_ = FALSE_;
        rq = (bound[(j << 1) + 2] + bound[(j << 1) + 3]) * .5f;
        i__1 = (*nband << 1) - 1;
        slabfc_(n, nband, a, &rq, &numvec, lde, &eigvec[j * *lde], &numl, &i__1, atemp, d, atol);
        vnorm = snrm2_(n, &eigvec[j * *lde], &c__1);
        if (vnorm != 0.f) {
            r__1 = 1.f / vnorm;
            sscal_(n, &r__1, &eigvec[j * *lde], &c__1);
        }

/*  ORTHOGONALIZE THE NEW EIGENVECTOR AGAINST THE OLD ONES */

L310:
        eigval[j] = rq;
        for (i = 0; i < j; ++i) {
            r__1 = -sdot_(n, &eigvec[i * *lde], &c__1, &eigvec[j * *lde], &c__1);
            saxpy_(n, &r__1, &eigvec[i * *lde], &c__1, &eigvec[j * *lde], &c__1);
        }
        vnorm = snrm2_(n, &eigvec[j * *lde], &c__1);
        if (vnorm == 0.f) {
            goto L305;
        }
        r__1 = 1.f / vnorm;
        sscal_(n, &r__1, &eigvec[j * *lde], &c__1);

/*   ORTHOGONALIZE LATER VECTORS AGAINST THE CONVERGED ONE */

        if (flag_) {
            goto L305;
        }
        for (i = j+1; i < nval; ++i) {
            r__1 = -sdot_(n, &eigvec[j * *lde], &c__1, &eigvec[i * *lde], &c__1);
            saxpy_(n, &r__1, &eigvec[j * *lde], &c__1, &eigvec[i * *lde], &c__1);
        }
    }
} /* slabcm_ */


/* *********************************************************************** */

/* Subroutine */
static void slabfc_(n, nband, a, sigma, number, lde, eigvec, numl, ldad, atemp, d, atol)
const integer *n, *nband;
real *a, *sigma;
const integer *number, *lde;
real *eigvec;
integer *numl, *ldad;
real *atemp, *d, *atol;
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Local variables */
    static real zero=0.f;
    static integer i, j, k, l, m;
    static integer la, ld, nb1, lpm;

/*  THIS SUBROUTINE FACTORS (A-SIGMA*I) WHERE A IS A GIVEN BAND */
/*  MATRIX AND SIGMA IS AN INPUT PARAMETER.  IT ALSO SOLVES ZERO */
/*  OR MORE SYSTEMS OF LINEAR EQUATIONS.  IT RETURNS THE NUMBER */
/*  OF EIGENVALUES OF A LESS THAN SIGMA BY COUNTING THE STURM */
/*  SEQUENCE DURING THE FACTORIZATION.  TO OBTAIN THE STURM */
/*  SEQUENCE COUNT WHILE ALLOWING NON-SYMMETRIC PIVOTING FOR */
/*  STABILITY, THE CODE USES A GUPTA'S MULTIPLE PIVOTING */
/*  ALGORITHM. */

/*  INITIALIZE */

    nb1 = *nband - 1;
    *numl = 0;
    i__1 = *ldad * *nband;
    scopy_(&i__1, &zero, &c__0, d, &c__1);

/*   LOOP OVER COLUMNS OF A */

    for (k = 0; k < *n; ++k) {

/*   ADD A COLUMN OF A TO D */

        d[nb1 + nb1 * *ldad] = a[k * *nband] - *sigma;
        m = min(k,nb1);
        for (i = 0; i < m; ++i) {
            la = k - i - 1;
            ld = nb1 - i - 1;
            d[ld + nb1 * *ldad] = a[i + 1 + la * *nband];
        }

        m = min(*n-k-1,nb1);
        for (i = 0; i < m; ++i) {
            ld = *nband + i;
            d[ld + nb1 * *ldad] = a[i + 1 + k * *nband];
        }

/*   TERMINATE */

        lpm = 1;
        for (i = 0; i < nb1; ++i) {
            l = k - nb1 + i;
            if (d[i + nb1 * *ldad] == 0.f) {
                continue;
            }
            if (abs(d[i + i * *ldad]) >= abs(d[i + nb1 * *ldad])) {
                goto L50;
            }
            if ( (d[i + nb1 * *ldad] < 0.f && d[i + i * *ldad] < 0.f ) ||
                 (d[i + nb1 * *ldad] > 0.f && d[i + i * *ldad] >= 0.f) ) {
                lpm = -lpm;
            }
            i__1 = *ldad - i;
            sswap_(&i__1, &d[i + i * *ldad], &c__1, &d[i + nb1 * *ldad], &c__1);
            sswap_(number, &eigvec[l], lde, &eigvec[k], lde);
L50:
            i__1 = *ldad - i - 1;
            r__1 = -d[i + nb1 * *ldad] / d[i + i * *ldad];
            saxpy_(&i__1, &r__1, &d[i + 1 + i * *ldad], &c__1, &d[i + 1 + nb1 * *ldad], &c__1);
            r__1 = -d[i + nb1 * *ldad] / d[i + i * *ldad];
            saxpy_(number, &r__1, &eigvec[l], lde, &eigvec[k], lde);
        }

/*  UPDATE STURM SEQUENCE COUNT */

        if (d[nb1 + nb1 * *ldad] < 0.f) {
            lpm = -lpm;
        }
        if (lpm < 0) {
            ++(*numl);
        }
        if (k == *n-1) {
            goto L110;
        }

/*   COPY FIRST COLUMN OF D INTO ATEMP */
        if (k >= nb1) {
            l = k - nb1;
            scopy_(ldad, d, &c__1, &atemp[l * *ldad], &c__1);
        }

/*   SHIFT THE COLUMNS OF D OVER AND UP */

        for (i = 0; i < nb1; ++i) {
            i__1 = *ldad - i - 1;
            scopy_(&i__1, &d[i + 1 + (i + 1) * *ldad], &c__1, &d[i + i * *ldad], &c__1);
            d[*ldad - 1 + i * *ldad] = 0.f;
        }
    }

/*  TRANSFER D TO ATEMP */

L110:
    for (i = 0; i < *nband; ++i) {
        i__1 = *nband - i;
        l = *n - i__1;
        scopy_(&i__1, &d[i + i * *ldad], &c__1, &atemp[l * *ldad], &c__1);
    }

/*   BACK SUBSTITUTION */

    if (*number == 0) {
        return;
    }
    for (k = *n-1; k >= 0; --k) {
        if (abs(atemp[k * *ldad]) <= *atol) {
            atemp[k * *ldad] = r_sign(atol, &atemp[k * *ldad]);
        }

        for (i = 0; i < *number; ++i) {
            eigvec[k + i * *lde] /= atemp[k * *ldad];
            m = min(*ldad-1,k);
            for (j = 0; j < m; ++j) {
                l = k - j - 1;
                eigvec[l + i * *lde] -= atemp[j + 1 + l * *ldad] * eigvec[k + i * *lde];
            }
        }
    }
} /* slabfc_ */


/* Subroutine */
static void slaeig_(n, nband, nl, nr, a, eigval, lde, eigvec, bound, atemp, d, vtemp, eps, tmin, tmax)
const integer *n, *nband, *nl, *nr;
real *a, *eigval;
const integer *lde;
real *eigvec, *bound, *atemp, *d, *vtemp, *eps, *tmin, *tmax;
{
    /* Local variables */
    static real atol;
    static integer nval, i;
    static real artol;

/*  THIS IS A SPECIALIZED VERSION OF THE SUBROUTINE BNDEIG TAILORED */
/*  SPECIFICALLY FOR USE BY THE LASO PACKAGE. */

/*  SET PARAMETERS */

    atol = *n * *eps * max(*tmax,-(*tmin));
    artol = atol / sqrtf(*eps);
    nval = *nr - *nl + 1;

/*   CHECK FOR SPECIAL CASE OF N = 1 */

    if (*n == 1) {
        eigval[0] = a[0];
        eigvec[0] = 1.f;
        return;
    }

/*   SET UP INITIAL EIGENVALUE BOUNDS */

    for (i = 1; i <= nval; ++i) {
        bound[(i << 1)] = *tmin;
        bound[(i << 1) + 1] = *tmax;
    }
    bound[1] = *tmax;
    bound[(nval << 1) + 2] = *tmin;
    if (*nl == 1) {
        bound[1] = *tmin;
    }
    if (*nr == *n) {
        bound[(nval << 1) + 2] = *tmax;
    }

    slabcm_(n, nband, nl, nr, a, eigval, lde, eigvec, &atol, &artol, bound, atemp, d, vtemp);
} /* slaeig_ */


/* *********************************************************************** */

/* Subroutine */
static void slager_(n, nband, nstart, a, tmin, tmax)
const integer *n, *nband, *nstart;
real *a, *tmin, *tmax;
{
    /* Local variables */
    static real temp;
    static integer i, k, l;

/*  THIS SUBROUTINE COMPUTES BOUNDS ON THE SPECTRUM OF A BY */
/*  EXAMINING THE GERSCHGORIN CIRCLES. ONLY THE NEWLY CREATED */
/*  CIRCLES ARE EXAMINED */

    for (k = *nstart - 1; k < *n; ++k) {
        temp = 0.f;
        for (i = 1; i < *nband; ++i) {
            temp += abs(a[i + k * *nband]);
        }
        l = min(k,*nband-1);
        for (i = 1; i <= l; ++i) {
            temp += abs(a[i + (k-i) * *nband]);
        }
        *tmin = min(*tmin,a[k * *nband] - temp);
        *tmax = max(*tmax,a[k * *nband] + temp);
    }
} /* slager_ */


/* *********************************************************************** */

/* Subroutine */
static void slaran_(n, x)
const integer *n;
real *x;
{
    /* Initialized data */
    static integer iurand = 0;

    /* Local variables */
    static integer i;

/*  THIS SUBROUTINE SETS THE VECTOR X TO RANDOM NUMBERS */

/*  INITIALIZE SEED */

    for (i = 0; i < *n; ++i) {
        x[i] = (real)urand_(&iurand) - .5f;
    }
} /* slaran_ */


/* ------------------------------------------------------------------ */

/* Subroutine */
static void smvpc_(nblock, bet, maxj, j, s, number, resnrm, orthcf, rv)
const integer *nblock;
const real *bet;
const integer *maxj, *j;
const real *s;
const integer *number;
real *resnrm, *orthcf, *rv;
{
    /* Local variables */
    static integer i, k, m;

/* THIS SUBROUTINE COMPUTES THE NORM AND THE SMALLEST ELEMENT */
/* (IN ABSOLUTE VALUE) OF THE VECTOR BET*SJI, WHERE SJI */
/* IS AN NBLOCK VECTOR OF THE LAST NBLOCK ELEMENTS OF THE ITH */
/* EIGENVECTOR OF T.  THESE QUANTITIES ARE THE RESIDUAL NORM */
/* AND THE ORTHOGONALITY COEFFICIENT RESPECTIVELY FOR THE */
/* CORRESPONDING RITZ PAIR.  THE ORTHOGONALITY COEFFICIENT IS */
/* NORMALIZED TO ACCOUNT FOR THE LOCAL REORTHOGONALIZATION. */

    m = *j - *nblock;
    for (i = 0; i < *number; ++i) {
        rv[0] = sdot_(nblock, &s[m + i * *maxj], &c__1, &bet[0], nblock);
        orthcf[i] = abs(rv[0]);
        for (k = 1; k < *nblock; ++k) {
            rv[k] = sdot_(nblock, &s[m + i * *maxj], &c__1, &bet[k], nblock);
            orthcf[i] = min(orthcf[i], abs(rv[k]));
        }
        resnrm[i] = snrm2_(nblock, rv, &c__1);
    }
} /* smvpc_ */


/* ------------------------------------------------------------------ */

/* Subroutine */
static void snppla_(op, iovect, n, nperm, nop, nmval, val, nmvec,
                    vec, nblock, h, hv, p, q, bound, d, delta, small, raritz, eps)
/* Subroutine */
void (*op) (const integer*,const integer*,const real*,real*);
/* Subroutine */
void (*iovect) (const integer*,const integer*,real*,const integer*,const integer*);
const integer *n, *nperm, *nmval;
integer *nop;
real *val;
const integer *nmvec;
real *vec;
const integer *nblock;
real *h, *hv, *p, *q, *bound, *d, *delta;
logical *small, *raritz;
real *eps;
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Local variables */
    static real hmin, hmax, temp;
    static real zero=0.f;
    static integer i, j, k, l, m;
    static integer jj, kk;

/* THIS SUBROUTINE POST PROCESSES THE EIGENVECTORS.  BLOCK MATRIX */
/* VECTOR PRODUCTS ARE USED TO MINIMIZED THE NUMBER OF CALLS TO OP. */

/* IF RARITZ IS .TRUE.  A FINAL RAYLEIGH-RITZ PROCEDURE IS APPLIED */
/* TO THE EIGENVECTORS. */

    if (! (*raritz)) {
        goto L190;
    }

/* ------------------------------------------------------------------ */

/* THIS CONSTRUCTS H=Q*AQ, WHERE THE COLUMNS OF Q ARE THE */
/* APPROXIMATE EIGENVECTORS.  TEMP = -1 IS USED WHEN SMALL IS */
/* FALSE TO AVOID HAVING TO RESORT THE EIGENVALUES AND EIGENVECTORS */
/* COMPUTED BY SLAEIG. */

    i__1 = *nperm * *nperm;
    scopy_(&i__1, &zero, &c__0, h, &c__1);
    temp = -1.f;
    if (*small) {
        temp = 1.f;
    }
    m = *nperm % *nblock;
    if (m == 0) {
        goto L40;
    }
    for (i = 0; i < m; ++i) {
        scopy_(n, &vec[i * *nmvec], &c__1, &p[i * *n], &c__1);
    }
    (*iovect)(n, &m, p, &m, &c__0);
    (*op)(n, &m, p, q);
    ++(*nop);
    for (i = 0; i < m; ++i) {
        for (j = i; j < *nperm; ++j) {
            jj = j - i;
            h[jj + i * *nperm] = temp * sdot_(n, &vec[j * *nmvec], &c__1, &q[i * *n], &c__1);
        }
    }
    if (*nperm < *nblock) {
        goto L90;
    }
L40:
    m += *nblock;
    for (i = m; *nblock < 0 ? i >= *nperm : i <= *nperm; i += *nblock) {
        for (j = 0; j < *nblock; ++j) {
            l = i - *nblock + j;
            scopy_(n, &vec[l * *nmvec], &c__1, &p[j * *n], &c__1);
        }
        (*iovect)(n, nblock, p, &i, &c__0);
        (*op)(n, nblock, p, q);
        ++(*nop);
        for (j = 0; j < *nblock; ++j) {
            l = i - *nblock + j;
            for (k = l; k < *nperm; ++k) {
                kk = k - l;
                h[kk + l * *nperm] = temp * sdot_(n, &vec[k * *nmvec], &c__1, &q[j * *n], &c__1);
            }
        }
    }

/* THIS COMPUTES THE SPECTRAL DECOMPOSITION OF H. */

L90:
    hmin = h[0];
    hmax = h[0];
    slager_(nperm, nperm, &c__1, h, &hmin, &hmax);
    slaeig_(nperm, nperm, &c__1, nperm, h, val, nperm, hv, bound, p, d, q, eps, &hmin, &hmax);

/* THIS COMPUTES THE RITZ VECTORS--THE COLUMNS OF */
/* Y = QS WHERE S IS THE MATRIX OF EIGENVECTORS OF H. */

    for (i = 0; i < *nperm; ++i) {
        scopy_(n, &zero, &c__0, &vec[i * *nmvec], &c__1);
    }
    m = *nperm % *nblock;
    if (m == 0) {
        goto L150;
    }
    (*iovect)(n, &m, p, &m, &c__1);
    for (i = 0; i < m; ++i) {
        for (j = 0; j < *nperm; ++j) {
            saxpy_(n, &hv[i + j * *nperm], &p[i * *n], &c__1, &vec[j * *nmvec], &c__1);
        }
    }
    if (*nperm < *nblock) {
        goto L190;
    }
L150:
    m += *nblock;
    for (i = m; *nblock < 0 ? i >= *nperm : i <= *nperm; i += *nblock) {
        (*iovect)(n, nblock, p, &i, &c__1);
        for (j = 0; j < *nblock; ++j) {
            l = i - *nblock + j;
            for (k = 0; k < *nperm; ++k) {
                saxpy_(n, &hv[l + k * *nperm], &p[j * *n], &c__1, &vec[k * *nmvec], &c__1);
            }
        }
    }

/* ------------------------------------------------------------------ */

/* THIS SECTION COMPUTES THE RAYLEIGH QUOTIENTS (IN VAL(*,1)) */
/* AND RESIDUAL NORMS (IN VAL(*,2)) OF THE EIGENVECTORS. */

L190:
    if (! (*small)) {
        *delta = -(*delta);
    }
    m = *nperm % *nblock;
    if (m == 0) {
        goto L220;
    }
    for (i = 0; i < m; ++i) {
        scopy_(n, &vec[i * *nmvec], &c__1, &p[i * *n], &c__1);
    }
    (*op)(n, &m, p, q);
    ++(*nop);
    for (i = 0; i < m; ++i) {
        val[i] = sdot_(n, &p[i * *n], &c__1, &q[i * *n], &c__1);
        r__1 = -val[i];
        saxpy_(n, &r__1, &p[i * *n], &c__1, &q[i * *n], &c__1);
        val[i + *nmval] = snrm2_(n, &q[i * *n], &c__1);
    }
    if (*nperm < *nblock) {
        goto L260;
    }
L220:
    ++m;
    for (i = m; *nblock < 0 ? i >= *nperm : i <= *nperm; i += *nblock) {
        for (j = 0; j < *nblock; ++j) {
            l = i - 1 + j;
            scopy_(n, &vec[l * *nmvec], &c__1, &p[j * *n], &c__1);
        }
        (*op)(n, nblock, p, q);
        ++(*nop);
        for (j = 0; j < *nblock; ++j) {
            l = i - 1 + j;
            val[l] = sdot_(n, &p[j * *n], &c__1, &q[j * *n], &c__1);
            r__1 = -val[l];
            saxpy_(n, &r__1, &p[j * *n], &c__1, &q[j * *n], &c__1);
            val[l + *nmval] = snrm2_(n, &q[j * *n], &c__1);
        }
    }

/* THIS COMPUTES THE ACCURACY ESTIMATES.  FOR CONSISTENCY WITH SILASO */

L260:
    for (i = 0; i < *nperm; ++i) {
        temp = *delta - val[i];
        if (! (*small)) {
            temp = -temp;
        }
        val[i + *nmval * 3] = 0.f;
        if (temp > 0.f) {
            val[i + *nmval * 3] = val[i + *nmval] / temp;
        }
        val[i + *nmval * 2] = val[i + *nmval * 3] * val[i + *nmval];
    }

} /* snppla_ */

/* ------------------------------------------------------------------ */

/* Subroutine */
static void sortqr_(nz, n, nblock, z, b)
const integer *nz, *n, *nblock;
real *z, *b;
{
    /* System generated locals */
    real r__1;

    /* Local variables */
    static real temp;
    static integer i, k;
    static real sigma;
    static integer length;
    static real tau;

/* THIS SUBROUTINE COMPUTES THE QR FACTORIZATION OF THE N X NBLOCK */
/* MATRIX Z.  Q IS FORMED IN PLACE AND RETURNED IN Z.  R IS */
/* RETURNED IN B. */

/* THIS SECTION REDUCES Z TO TRIANGULAR FORM. */

    for (i = 0; i < *nblock; ++i) {

/* THIS FORMS THE ITH REFLECTION. */

        length = *n - i;
        r__1 = snrm2_(&length, &z[i + i * *nz], &c__1);
        sigma = r_sign(&r__1, &z[i + i * *nz]);
        b[i + i * *nblock] = -sigma;
        z[i + i * *nz] += sigma;
        tau = sigma * z[i + i * *nz];

/* THIS APPLIES THE ROTATION TO THE REST OF THE COLUMNS. */

        for (k = i+1; k < *nblock; ++k) {
            if (tau != 0.f) {
                temp = -sdot_(&length, &z[i + i * *nz], &c__1, &z[i + k * *nz], &c__1) / tau;
                saxpy_(&length, &temp, &z[i + i * *nz], &c__1, &z[i + k * *nz], &c__1);
            }
            b[i + k * *nblock] = z[i + k * *nz];
            z[i + k * *nz] = 0.f;
        }
    }

/* THIS ACCUMULATES THE REFLECTIONS IN REVERSE ORDER. */

    for (i = *nblock-1; i >= 0; --i) {

/* THIS RECREATES THE ITH = NBLOCK-M+1)TH REFLECTION. */

        sigma = -b[i + i * *nblock];
        tau = z[i + i * *nz] * sigma;
        if (tau == 0.f) {
            goto L60;
        }
        length = *n - i;

/* THIS APPLIES IT TO THE LATER COLUMNS. */

        for (k = i+1; k < *nblock; ++k) {
            temp = -sdot_(&length, &z[i + i * *nz], &c__1, &z[i + k * *nz], &c__1) / tau;
            saxpy_(&length, &temp, &z[i + i * *nz], &c__1, &z[i + k * *nz], &c__1);
        }
        r__1 = -1.f / sigma;
        sscal_(&length, &r__1, &z[i + i * *nz], &c__1);
L60:
        z[i + i * *nz] += 1.f;
    }
} /* sortqr_ */


/* ------------------------------------------------------------------- */

/* Subroutine */
static void svsort_(num, val, res, iflag, v, nmvec, n, vec)
const integer *num;
real *val, *res;
const integer *iflag;
real *v;
const integer *nmvec, *n;
real *vec;
{
    /* Local variables */
    static real temp;
    static integer kk, k, m;

/*  THIS SUBROUTINE SORTS THE EIGENVALUES (VAL) IN ASCENDING ORDER */
/*  WHILE CONCURRENTLY SWAPPING THE RESIDUALS AND VECTORS. */

    for (m = *num - 1; m > 0; --m) {
        for (k = 0; k < m; ++k) {
            kk = k+1;
            if (val[k] <= val[kk])
                continue;
            temp = val[k]; val[k] = val[kk]; val[kk] = temp;
            temp = res[k]; res[k] = res[kk]; res[kk] = temp;
            sswap_(n, &vec[k * *nmvec], &c__1, &vec[kk * *nmvec], &c__1);
            if (*iflag != 0) {
                temp = v[k]; v[k] = v[kk]; v[kk] = temp;
            }
        }
    }
} /* svsort_ */
