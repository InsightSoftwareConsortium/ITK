/* laso/dnlaso.f -- translated by f2c (version 20050501).
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
static doublereal c_b15 = 10.;
static doublereal c_b88 = 0.;

/*   VERSION 2    DOES NOT USE EISPACK */

/* ------------------------------------------------------------------ */

/*<    >*/
/* Subroutine */ int dnlaso_(
        void (*op)(integer*,integer*,doublereal*,doublereal*),
        void (*iovect)(integer*,integer*,doublereal*,integer*,integer*),
        integer *n, integer *nval,
        integer *nfig, integer *nperm, integer *nmval, doublereal *val,
        integer *nmvec, doublereal *vec, integer *nblock, integer *maxop,
        integer *maxj, doublereal *work, integer *ind, integer *ierr)
{
    /* System generated locals */
    integer vec_dim1, vec_offset, val_dim1, val_offset, i__1, i__2;
    doublereal d__1;

    /* Local variables */
    integer i__, m, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13,
            nv;
    doublereal eps;
    integer nop;
    doublereal temp, tarr[1];
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    integer nband;
    doublereal delta;
    extern /* Subroutine */ int dnwla_(
            void (*op)(integer*,integer*,doublereal*,doublereal*),
            void (*iovect)(integer*,integer*,doublereal*,integer*,integer*),
            integer *, integer *,
            integer *, integer *, integer *, doublereal *, integer *,
            doublereal *, integer *, integer *, integer *, integer *,
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *, doublereal *, integer *, logical *, logical *,
            doublereal *, doublereal *, integer *);
    logical small;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *,
            doublereal *, integer *),
            dnppla_(void (*op)(integer*,integer*,doublereal*,doublereal*),
                    void (*iovect)(integer*,integer*,doublereal*,integer*,integer*),
                    integer *, integer *
                    , integer *, integer *, doublereal *, integer *, doublereal *,
                    integer *, doublereal *, doublereal *, doublereal *, doublereal *,
                    doublereal *, doublereal *, doublereal *, logical *, logical *,
                    doublereal *);
    logical raritz;
    extern /* Subroutine */ int dortqr_(integer *, integer *, integer *,
            doublereal *, doublereal *), dvsort_(integer *, doublereal *,
            doublereal *, integer *, doublereal *, integer *, integer *,
            doublereal *);


/*<    >*/
/*<       DOUBLE PRECISION VEC(NMVEC,1), VAL(NMVAL,4), WORK(1) >*/
/*<       EXTERNAL OP, IOVECT >*/

/* AUTHOR/IMPLEMENTER D.S.SCOTT-B.N.PARLETT/D.S.SCOTT */

/* COMPUTER SCIENCES DEPARTMENT */
/* UNIVERSITY OF TEXAS AT AUSTIN */
/* AUSTIN, TX 78712 */

/* VERSION 2 ORIGINATED APRIL 1982 */

/* CURRENT VERSION  JUNE 1983 */

/* DNLASO FINDS A FEW EIGENVALUES AND EIGENVECTORS AT EITHER END OF */
/* THE SPECTRUM OF A LARGE SPARSE SYMMETRIC MATRIX.  THE SUBROUTINE */
/* DNLASO IS PRIMARILY A DRIVER FOR SUBROUTINE DNWLA WHICH IMPLEMENTS */
/* THE LANCZOS ALGORITHM WITH SELECTIVE ORTHOGONALIZATION AND */
/* SUBROUTINE DNPPLA WHICH POST PROCESSES THE OUTPUT OF DNWLA. */
/* HOWEVER DNLASO DOES CHECK FOR INCONSISTENCIES IN THE CALLING */
/* PARAMETERS AND DOES PREPROCESS ANY USER SUPPLIED EIGENPAIRS. */
/* DNLASO ALWAYS LOOKS FOR THE SMALLEST (LEFTMOST) EIGENVALUES.  IF */
/* THE LARGEST EIGENVALUES ARE DESIRED DNLASO IMPLICITLY USES THE */
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
/*     DABS(NVAL)  IS THE NUMBER OF EIGENVALUES DESIRED. */
/*     IF NVAL < 0 THE ALGEBRAICALLY SMALLEST (LEFTMOST) */
/*     EIGENVALUES ARE FOUND.  IF NVAL > 0 THE ALGEBRAICALLY */
/*     LARGEST (RIGHTMOST) EIGENVALUES ARE FOUND.  NVAL MUST NOT */
/*     BE ZERO.  DABS(NVAL) MUST BE LESS THAN  MAXJ/2. */

/*   NFIG   THE NUMBER OF DECIMAL DIGITS OF ACCURACY DESIRED IN THE */
/*     EIGENVALUES.  NFIG MUST BE GREATER THAN OR EQUAL TO 1. */

/*   NPERM   AN INTEGER VARIABLE WHICH SPECIFIES THE NUMBER OF USER */
/*     SUPPLIED EIGENPAIRS.  IN MOST CASES NPERM WILL BE ZERO.  SEE */
/*     DOCUMENTAION FOR FURTHER DETAILS OF USING NPERM GREATER */
/*     THAN ZERO.  NPERM MUST NOT BE LESS THAN ZERO. */

/*   NMVAL   THE ROW DIMENSION OF THE ARRAY VAL.  NMVAL MUST BE GREATER */
/*     THAN OR EQUAL TO DABS(NVAL). */

/*   VAL   A TWO DIMENSIONAL DOUBLE PRECISION ARRAY OF ROW */
/*     DIMENSION NMVAL AND COLUMN DIMENSION AT LEAST 4.  IF NPERM */
/*     IS GREATER THAN ZERO THEN CERTAIN INFORMATION MUST BE STORED */
/*     IN VAL.  SEE DOCUMENTATION FOR DETAILS. */

/*   NMVEC   THE ROW DIMENSION OF THE ARRAY VEC.  NMVEC MUST BE GREATER */
/*     THAN OR EQUAL TO N. */

/*   VEC   A TWO DIMENSIONAL DOUBLE PRECISION ARRAY OF ROW */
/*     DIMENSION NMVEC AND COLUMN DIMENSION AT LEAST DABS(NVAL).  IF */
/*     NPERM > 0 THEN THE FIRST NPERM COLUMNS OF VEC MUST */
/*     CONTAIN THE USER SUPPLIED EIGENVECTORS. */

/*   NBLOCK   THE BLOCK SIZE.  SEE DOCUMENTATION FOR CHOOSING */
/*     AN APPROPRIATE VALUE FOR NBLOCK.  NBLOCK MUST BE GREATER */
/*     THAN ZERO AND LESS THAN  MAXJ/6. */

/*   MAXOP   AN UPPER BOUND ON THE NUMBER OF CALLS TO THE SUBROUTINE */
/*     OP.  DNLASO TERMINATES WHEN MAXOP IS EXCEEDED.  SEE */
/*     DOCUMENTATION FOR GUIDELINES IN CHOOSING A VALUE FOR MAXOP. */

/*   MAXJ   AN INDICATION OF THE AVAILABLE STORAGE (SEE WORK AND */
/*     DOCUMENTATION ON IOVECT).  FOR THE FASTEST CONVERGENCE MAXJ */
/*     SHOULD BE AS LARGE AS POSSIBLE, ALTHOUGH IT IS USELESS TO HAVE */
/*     MAXJ LARGER THAN MAXOP*NBLOCK. */

/*   WORK   A DOUBLE PRECISION ARRAY OF DIMENSION AT LEAST AS */
/*     LARGE AS */

/*         2*N*NBLOCK + MAXJ*(NBLOCK+NV+2) + 2*NBLOCK*NBLOCK + 3*NV */

/*            + THE MAXIMUM OF */
/*                 N*NBLOCK */
/*                   AND */
/*         MAXJ*(2*NBLOCK+3) + 2*NV + 6 + (2*NBLOCK+2)*(NBLOCK+1) */

/*     WHERE NV = DABS(NVAL) */

/*     THE FIRST N*NBLOCK ELEMENTS OF WORK MUST CONTAIN THE DESIRED */
/*     STARTING VECTORS.  SEE DOCUMENTATION FOR GUIDELINES IN */
/*     CHOOSING STARTING VECTORS. */

/*   IND   AN INTEGER ARRAY OF DIMENSION AT LEAST DABS(NVAL). */

/*   IERR   AN INTEGER VARIABLE. */


/* ON OUTPUT */


/*   NPERM   THE NUMBER OF EIGENPAIRS NOW KNOWN. */

/*   VEC   THE FIRST NPERM COLUMNS OF VEC CONTAIN THE EIGENVECTORS. */

/*   VAL   THE FIRST COLUMN OF VAL CONTAINS THE CORRESPONDING */
/*     EIGENVALUES.  THE SECOND COLUMN CONTAINS THE RESIDUAL NORMS OF */
/*     THE EIGENPAIRS WHICH ARE BOUNDS ON THE ACCURACY OF THE EIGEN- */
/*     VALUES.  THE THIRD COLUMN CONTAINS MORE DOUBLE PRECISIONISTIC ESTIMATES */
/*     OF THE ACCURACY OF THE EIGENVALUES.  THE FOURTH COLUMN CONTAINS */
/*     ESTIMATES OF THE ACCURACY OF THE EIGENVECTORS.  SEE */
/*     DOCUMENTATION FOR FURTHER INFORMATION ON THESE QUANTITIES. */

/*   WORK   IF WORK IS TERMINATED BEFORE COMPLETION (IERR = -2) */
/*     THE FIRST N*NBLOCK ELEMENTS OF WORK CONTAIN THE BEST VECTORS */
/*     FOR RESTARTING THE ALGORITHM AND DNLASO CAN BE IMMEDIATELY */
/*     RECALLED TO CONTINUE WORKING ON THE PROBLEM. */

/*   IND   IND(1)  CONTAINS THE ACTUAL NUMBER OF CALLS TO OP.  ON SOME */
/*     OCCASIONS THE NUMBER OF CALLS TO OP MAY BE SLIGHTLY LARGER */
/*     THAN MAXOP. */

/*   IERR   AN ERROR COMPLETION CODE.  THE NORMAL COMPLETION CODE IS */
/*     ZERO.  SEE THE DOCUMENTATION FOR INTERPRETATIONS OF NON-ZERO */
/*     COMPLETION CODES. */


/* INTERNAL VARIABLES. */


/*<    >*/
/*<       LOGICAL RARITZ, SMALL >*/
/*<       DOUBLE PRECISION DELTA, EPS, TEMP, DNRM2, DABS, TARR(1) >*/
/*<       EXTERNAL DNPPLA, DNWLA, DORTQR, DCOPY, DNRM2, DVSORT >*/

/* NOP   RETURNED FROM DNWLA AS THE NUMBER OF CALLS TO THE */
/*   SUBROUTINE OP. */

/* NV   SET EQUAL TO DABS(NVAL), THE NUMBER OF EIGENVALUES DESIRED, */
/*   AND PASSED TO DNWLA. */

/* SMALL   SET TO .TRUE. IF THE SMALLEST EIGENVALUES ARE DESIRED. */

/* RARITZ   RETURNED FROM DNWLA AND PASSED TO DNPPLA.  RARITZ IS .TRUE. */
/*   IF A FINAL RAYLEIGH-RITZ PROCEDURE IS NEEDED. */

/* DELTA   RETURNED FROM DNWLA AS THE EIGENVALUE OF THE MATRIX */
/*   WHICH IS CLOSEST TO THE DESIRED EIGENVALUES. */

/* DNPPLA   A SUBROUTINE FOR POST-PROCESSING THE EIGENVECTORS COMPUTED */
/*   BY DNWLA. */

/* DNWLA   A SUBROUTINE FOR IMPLEMENTING THE LANCZOS ALGORITHM */
/*   WITH SELECTIVE ORTHOGONALIZATION. */

/* DMVPC   A SUBROUTINE FOR COMPUTING THE RESIDUAL NORM AND */
/*   ORTHOGONALITY COEFFICIENT OF GIVEN RITZ VECTORS. */

/* DORTQR   A SUBROUTINE FOR ORTHONORMALIZING A BLOCK OF VECTORS */
/*   USING HOUSEHOLDER REFLECTIONS. */

/* DAXPY,DCOPY,DDOT,DNRM2,DSCAL,DSWAP   A SUBSET OF THE BASIC LINEAR */
/*   ALGEBRA SUBPROGRAMS USED FOR VECTOR MANIPULATION. */

/* DLARAN   A SUBROUTINE TO GENERATE RANDOM VECTORS */

/* DLAEIG, DLAGER, DLABCM, DLABFC   SUBROUTINES FOR BAND EIGENVALUE */
/*   CALCULATIONS. */

/* ------------------------------------------------------------------ */

/* THIS SECTION CHECKS FOR INCONSISTENCY IN THE INPUT PARAMETERS. */

/*<       NV = IABS(NVAL) >*/
    /* Parameter adjustments */
    val_dim1 = *nmval;
    val_offset = 1 + val_dim1;
    val -= val_offset;
    vec_dim1 = *nmvec;
    vec_offset = 1 + vec_dim1;
    vec -= vec_offset;
    --work;
    --ind;

    /* Function Body */
    nv = abs(*nval);
/*<       IND(1) = 0 >*/
    ind[1] = 0;
/*<       IERR = 0 >*/
    *ierr = 0;
/*<       IF (N.LT.6*NBLOCK) IERR = 1 >*/
    if (*n < *nblock * 6) {
        *ierr = 1;
    }
/*<       IF (NFIG.LE.0) IERR = IERR + 2 >*/
    if (*nfig <= 0) {
        *ierr += 2;
    }
/*<       IF (NMVEC.LT.N) IERR = IERR + 4 >*/
    if (*nmvec < *n) {
        *ierr += 4;
    }
/*<       IF (NPERM.LT.0) IERR = IERR + 8 >*/
    if (*nperm < 0) {
        *ierr += 8;
    }
/*<       IF (MAXJ.LT.6*NBLOCK) IERR = IERR + 16 >*/
    if (*maxj < *nblock * 6) {
        *ierr += 16;
    }
/*<       IF (NV.LT.MAX0(1,NPERM)) IERR = IERR + 32 >*/
    if (nv < max(1,*nperm)) {
        *ierr += 32;
    }
/*<       IF (NV.GT.NMVAL) IERR = IERR + 64 >*/
    if (nv > *nmval) {
        *ierr += 64;
    }
/*<       IF (NV.GT.MAXOP) IERR = IERR + 128 >*/
    if (nv > *maxop) {
        *ierr += 128;
    }
/*<       IF (NV.GE.MAXJ/2) IERR = IERR + 256 >*/
    if (nv >= *maxj / 2) {
        *ierr += 256;
    }
/*<       IF (NBLOCK.LT.1) IERR = IERR + 512 >*/
    if (*nblock < 1) {
        *ierr += 512;
    }
/*<       IF (IERR.NE.0) RETURN >*/
    if (*ierr != 0) {
        return 0;
    }

/*<       SMALL = NVAL.LT.0 >*/
    small = *nval < 0;

/* ------------------------------------------------------------------ */

/* THIS SECTION SORTS AND ORTHONORMALIZES THE USER SUPPLIED VECTORS. */
/* IF A USER SUPPLIED VECTOR IS ZERO OR IF DSIGNIFICANT CANCELLATION */
/* OCCURS IN THE ORTHOGONALIZATION PROCESS THEN IERR IS SET TO  -1 */
/* AND DNLASO TERMINATES. */

/*<       IF (NPERM.EQ.0) GO TO 110 >*/
    if (*nperm == 0) {
        goto L110;
    }

/* THIS NEGATES THE USER SUPPLIED EIGENVALUES WHEN THE LARGEST */
/* EIGENVALUES ARE DESIRED, SINCE DNWLA WILL IMPLICITLY USE THE */
/* NEGATIVE OF THE MATRIX. */

/*<       IF (SMALL) GO TO 20 >*/
    if (small) {
        goto L20;
    }
/*<       DO 10 I=1,NPERM >*/
    i__1 = *nperm;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          VAL(I,1) = -VAL(I,1) >*/
        val[i__ + val_dim1] = -val[i__ + val_dim1];
/*<    10 CONTINUE >*/
/* L10: */
    }

/* THIS SORTS THE USER SUPPLIED VALUES AND VECTORS. */

/*<    20 CALL DVSORT(NPERM, VAL, VAL(1,2), 0, TARR, NMVEC, N, VEC) >*/
L20:
    dvsort_(nperm, &val[val_offset], &val[(val_dim1 << 1) + 1], &c__0, tarr,
            nmvec, n, &vec[vec_offset]);

/* THIS STORES THE NORMS OF THE VECTORS FOR LATER COMPARISON. */
/* IT ALSO INSURES THAT THE RESIDUAL NORMS ARE POSITIVE. */

/*<       DO 60 I=1,NPERM >*/
    i__1 = *nperm;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          VAL(I,2) = DABS(VAL(I,2)) >*/
        val[i__ + (val_dim1 << 1)] = (d__1 = val[i__ + (val_dim1 << 1)], abs(
                d__1));
/*<          VAL(I,3) = DNRM2(N,VEC(1,I),1) >*/
        val[i__ + val_dim1 * 3] = dnrm2_(n, &vec[i__ * vec_dim1 + 1], &c__1);
/*<    60 CONTINUE >*/
/* L60: */
    }

/* THIS PERFORMS THE ORTHONORMALIZATION. */

/*<       M = N*NBLOCK + 1 >*/
    m = *n * *nblock + 1;
/*<       CALL DORTQR(NMVEC, N, NPERM, VEC, WORK(M)) >*/
    dortqr_(nmvec, n, nperm, &vec[vec_offset], &work[m]);
/*<       M = N*NBLOCK - NPERM >*/
    m = *n * *nblock - *nperm;
/*<       DO 70 I = 1, NPERM >*/
    i__1 = *nperm;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          M = M + NPERM + 1 >*/
        m = m + *nperm + 1;
/*<          IF(DABS(WORK(M)) .GT. 0.9*VAL(I,3)) GO TO 70 >*/
        if ((d__1 = work[m], abs(d__1)) > val[i__ + val_dim1 * 3] * (float).9)
                 {
            goto L70;
        }
/*<          IERR = -1 >*/
        *ierr = -1;
/*<          RETURN >*/
        return 0;

/*<    70 CONTINUE >*/
L70:
        ;
    }

/* THIS COPIES THE RESIDUAL NORMS INTO THE CORRECT LOCATIONS IN */
/* THE ARRAY WORK FOR LATER REFERENCE IN DNWLA. */

/*<       M = 2*N*NBLOCK + 1 >*/
    m = (*n << 1) * *nblock + 1;
/*<       CALL DCOPY(NPERM, VAL(1,2), 1, WORK(M), 1) >*/
    dcopy_(nperm, &val[(val_dim1 << 1) + 1], &c__1, &work[m], &c__1);

/* THIS SETS EPS TO AN APPROXIMATION OF THE RELATIVE MACHINE */
/* PRECISION */

/* ***THIS SHOULD BE REPLACED BY AN ASDSIGNMENT STATEMENT */
/* ***IN A PRODUCTION CODE */

/*<   110 EPS = 1.0D0 >*/
L110:
    eps = 1.;
/*<       DO 120 I = 1,1000 >*/
    for (i__ = 1; i__ <= 1000; ++i__) {
/*<          EPS = 0.5D0*EPS >*/
        eps *= .5;
/*<          TEMP = 1.0D0 + EPS >*/
        temp = eps + 1.;
/*<          IF(TEMP.EQ.1.0D0) GO TO 130 >*/
        if (temp == 1.) {
            goto L130;
        }
/*<   120 CONTINUE >*/
/* L120: */
    }

/* ------------------------------------------------------------------ */

/* THIS SECTION CALLS DNWLA WHICH IMPLEMENTS THE LANCZOS ALGORITHM */
/* WITH SELECTIVE ORTHOGONALIZATION. */

/*<   130 NBAND = NBLOCK + 1 >*/
L130:
    nband = *nblock + 1;
/*<       I1 = 1 + N*NBLOCK >*/
    i1 = *n * *nblock + 1;
/*<       I2 = I1 + N*NBLOCK >*/
    i2 = i1 + *n * *nblock;
/*<       I3 = I2 + NV >*/
    i3 = i2 + nv;
/*<       I4 = I3 + NV >*/
    i4 = i3 + nv;
/*<       I5 = I4 + NV >*/
    i5 = i4 + nv;
/*<       I6 = I5 + MAXJ*NBAND >*/
    i6 = i5 + *maxj * nband;
/*<       I7 = I6 + NBLOCK*NBLOCK >*/
    i7 = i6 + *nblock * *nblock;
/*<       I8 = I7 + NBLOCK*NBLOCK >*/
    i8 = i7 + *nblock * *nblock;
/*<       I9 = I8 + MAXJ*(NV+1) >*/
    i9 = i8 + *maxj * (nv + 1);
/*<       I10 = I9 + NBLOCK >*/
    i10 = i9 + *nblock;
/*<       I11 = I10 + 2*NV + 6 >*/
    i11 = i10 + (nv << 1) + 6;
/*<       I12 = I11 + MAXJ*(2*NBLOCK+1) >*/
    i12 = i11 + *maxj * ((*nblock << 1) + 1);
/*<       I13 = I12 + MAXJ >*/
    i13 = i12 + *maxj;
/*<    >*/
    dnwla_(op, iovect, n, &nband, &nv, nfig, nperm, &val[
            val_offset], nmvec, &vec[vec_offset], nblock, maxop, maxj, &nop, &
            work[1], &work[i1], &work[i2], &work[i3], &work[i4], &work[i5], &
            work[i6], &work[i7], &work[i8], &work[i9], &work[i10], &work[i11],
             &work[i12], &work[i13], &ind[1], &small, &raritz, &delta, &eps,
            ierr);

/* ------------------------------------------------------------------ */

/* THIS SECTION CALLS DNPPLA (THE POST PROCESSOR). */

/*<       IF (NPERM.EQ.0) GO TO 140 >*/
    if (*nperm == 0) {
        goto L140;
    }
/*<       I1 = N*NBLOCK + 1 >*/
    i1 = *n * *nblock + 1;
/*<       I2 = I1 + NPERM*NPERM >*/
    i2 = i1 + *nperm * *nperm;
/*<       I3 = I2 + NPERM*NPERM >*/
    i3 = i2 + *nperm * *nperm;
/*<       I4 = I3 + MAX0(N*NBLOCK,2*NPERM*NPERM) >*/
/* Computing MAX */
    i__1 = *n * *nblock, i__2 = (*nperm << 1) * *nperm;
    i4 = i3 + max(i__1,i__2);
/*<       I5 = I4 + N*NBLOCK >*/
    i5 = i4 + *n * *nblock;
/*<       I6 = I5 + 2*NPERM + 4 >*/
    i6 = i5 + (*nperm << 1) + 4;
/*<    >*/
    dnppla_(op, iovect, n, nperm, &nop, nmval, &val[val_offset],
            nmvec, &vec[vec_offset], nblock, &work[i1], &work[i2], &work[i3],
            &work[i4], &work[i5], &work[i6], &delta, &small, &raritz, &eps);

/*<   140 IND(1) = NOP >*/
L140:
    ind[1] = nop;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* dnlaso_ */


/* ------------------------------------------------------------------ */

/*<    >*/
/* Subroutine */ int dnwla_(
        void (*op)(integer*,integer*,doublereal*,doublereal*),
        void (*iovect)(integer*,integer*,doublereal*,integer*,integer*),
        integer *n, integer *nband,
        integer *nval, integer *nfig, integer *nperm, doublereal *val,
        integer *nmvec, doublereal *vec, integer *nblock, integer *maxop,
        integer *maxj, integer *nop, doublereal *p1, doublereal *p0,
        doublereal *res, doublereal *tau, doublereal *otau, doublereal *t,
        doublereal *alp, doublereal *bet, doublereal *s, doublereal *p2,
        doublereal *bound, doublereal *atemp, doublereal *vtemp, doublereal *
        d__, integer *ind, logical *small, logical *raritz, doublereal *delta,
         doublereal *eps, integer *ierr)
{
    /* System generated locals */
    integer vec_dim1, vec_offset, p0_dim1, p0_offset, p1_dim1, p1_offset,
            p2_dim1, p2_offset, t_dim1, t_offset, alp_dim1, alp_offset,
            bet_dim1, bet_offset, s_dim1, s_offset, i__1, i__2, i__3, i__4;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(doublereal), pow_dd(doublereal *, doublereal *);

    /* Local variables */
    integer i__, j, k, l=0, m, i1, ii, ng;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *,
            integer *);
    doublereal tola=0, temp, tolg=0, tmin, tmax, tarr[1];
    logical test;
    doublereal utol;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *);
    integer ngood, nleft;
    doublereal anorm=0;
    extern /* Subroutine */ int dmvpc_(integer *, doublereal *, integer *,
            integer *, doublereal *, integer *, doublereal *, doublereal *,
            doublereal *), dcopy_(integer *, doublereal *, integer *,
            doublereal *, integer *);
    integer mtemp;
    doublereal dzero[1];
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *,
            integer *, doublereal *, integer *);
    doublereal pnorm, epsrt, rnorm;
    extern /* Subroutine */ int dlaeig_(integer *, integer *, integer *,
            integer *, doublereal *, doublereal *, integer *, doublereal *,
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *, doublereal *, doublereal *), dlager_(integer *,
            integer *, integer *, doublereal *, doublereal *, doublereal *),
            dlaran_(integer *, doublereal *);
    doublereal betmin, alpmin=0, betmax, alpmax=0;
    integer ntheta;
    logical enough;
    integer number, nstart;
    extern /* Subroutine */ int dortqr_(integer *, integer *, integer *,
            doublereal *, doublereal *), dvsort_(integer *, doublereal *,
            doublereal *, integer *, doublereal *, integer *, integer *,
            doublereal *);


/*<    >*/
/*<       LOGICAL RARITZ, SMALL >*/
/*<    >*/
/*<       EXTERNAL OP, IOVECT >*/

/* DNWLA IMPLEMENTS THE LANCZOS ALGORITHM WITH SELECTIVE */
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
/*      EIGENVALUE SOLVER DLAEIG. */

/*   S   EIGENVECTORS OF T. */

/*   SMALL   .TRUE.  IF THE SMALL EIGENVALUES ARE DESIRED. */

/*   RARITZ  RETURNED AS  .TRUE.  IF A FINAL RAYLEIGH-RITZ PROCEDURE */
/*     IS TO BE DONE. */

/*   DELTA   RETURNED AS THE VALUE OF THE (NVAL+1)TH EIGENVALUE */
/*     OF THE MATRIX.  USED IN ESTIMATING THE ACCURACY OF THE */
/*     COMPUTED EIGENVALUES. */


/* INTERNAL VARIABLES USED */

/*<    >*/
/*<       LOGICAL ENOUGH, TEST >*/
/*<    >*/
/*<    >*/

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

/* TARR  AN ARRAY OF LENGTH ONE USED TO INSURE TYPE CONSISTENCY IN CALLS TO */
/*       DLAEIG */

/* DZERO AN ARRAY OF LENGTH ONE CONTAINING DZERO, USED TO INSURE TYPE */
/*       CONSISTENCY IN CALLS TO DCOPY */

/*<       DZERO(1) = 0.0D0 >*/
    /* Parameter adjustments */
    p2_dim1 = *n;
    p2_offset = 1 + p2_dim1;
    p2 -= p2_offset;
    p0_dim1 = *n;
    p0_offset = 1 + p0_dim1;
    p0 -= p0_offset;
    p1_dim1 = *n;
    p1_offset = 1 + p1_dim1;
    p1 -= p1_offset;
    t_dim1 = *nband;
    t_offset = 1 + t_dim1;
    t -= t_offset;
    --val;
    vec_dim1 = *nmvec;
    vec_offset = 1 + vec_dim1;
    vec -= vec_offset;
    bet_dim1 = *nblock;
    bet_offset = 1 + bet_dim1;
    bet -= bet_offset;
    alp_dim1 = *nblock;
    alp_offset = 1 + alp_dim1;
    alp -= alp_offset;
    s_dim1 = *maxj;
    s_offset = 1 + s_dim1;
    s -= s_offset;
    --res;
    --tau;
    --otau;
    --bound;
    --atemp;
    --vtemp;
    --d__;
    --ind;

    /* Function Body */
    dzero[0] = 0.;
/*<       RNORM = 0.0D0 >*/
    rnorm = 0.;
/*<       IF (NPERM.NE.0) RNORM = DMAX1(-VAL(1),VAL(NPERM)) >*/
    if (*nperm != 0) {
/* Computing MAX */
        d__1 = -val[1], d__2 = val[*nperm];
        rnorm = max(d__1,d__2);
    }
/*<       PNORM = RNORM >*/
//    pnorm = rnorm;
/*<       DELTA = 10.D30 >*/
    *delta = 1e31;
/*<       EPSRT = DSQRT(EPS) >*/
    epsrt = sqrt(*eps);
/*<       NLEFT = NVAL - NPERM >*/
    nleft = *nval - *nperm;
/*<       NOP = 0 >*/
    *nop = 0;
/*<       NUMBER = NPERM >*/
    number = *nperm;
/*<       RARITZ = .FALSE. >*/
    *raritz = FALSE_;
/*<       UTOL = DMAX1(DBLE(FLOAT(N))*EPS,10.0D0**DBLE((-FLOAT(NFIG)))) >*/
/* Computing MAX */
    d__3 = (doublereal) (-((real) (*nfig)));
    d__1 = (doublereal) ((real) (*n)) * *eps, d__2 = pow_dd(&c_b15, &d__3);
    utol = max(d__1,d__2);
/*<       J = MAXJ >*/
    j = *maxj;

/* ------------------------------------------------------------------ */

/* ANY ITERATION OF THE ALGORITHM BEGINS HERE. */

/*<    30 DO 50 I=1,NBLOCK >*/
L30:
    i__1 = *nblock;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          TEMP = DNRM2(N,P1(1,I),1) >*/
        temp = dnrm2_(n, &p1[i__ * p1_dim1 + 1], &c__1);
/*<          IF (TEMP.EQ.0D0) CALL DLARAN(N, P1(1,I)) >*/
        if (temp == 0.) {
            dlaran_(n, &p1[i__ * p1_dim1 + 1]);
        }
/*<    50 CONTINUE >*/
/* L50: */
    }
/*<       IF (NPERM.EQ.0) GO TO 70 >*/
    if (*nperm == 0) {
        goto L70;
    }
/*<       DO 60 I=1,NPERM >*/
    i__1 = *nperm;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          TAU(I) = 1.0D0 >*/
        tau[i__] = 1.;
/*<          OTAU(I) = 0.0D0 >*/
        otau[i__] = 0.;
/*<    60 CONTINUE >*/
/* L60: */
    }
/*<    70 CALL DCOPY(N*NBLOCK, DZERO, 0, P0, 1) >*/
L70:
    i__1 = *n * *nblock;
    dcopy_(&i__1, dzero, &c__0, &p0[p0_offset], &c__1);
/*<       CALL DCOPY(NBLOCK*NBLOCK, DZERO, 0, BET, 1) >*/
    i__1 = *nblock * *nblock;
    dcopy_(&i__1, dzero, &c__0, &bet[bet_offset], &c__1);
/*<       CALL DCOPY(J*NBAND, DZERO, 0, T, 1) >*/
    i__1 = j * *nband;
    dcopy_(&i__1, dzero, &c__0, &t[t_offset], &c__1);
/*<       MTEMP = NVAL + 1 >*/
    mtemp = *nval + 1;
/*<       DO 75 I = 1, MTEMP >*/
    i__1 = mtemp;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          CALL DCOPY(J, DZERO, 0, S(1,I), 1) >*/
        dcopy_(&j, dzero, &c__0, &s[i__ * s_dim1 + 1], &c__1);
/*<    75 CONTINUE >*/
/* L75: */
    }
/*<       NGOOD = 0 >*/
    ngood = 0;
/*<       TMIN = 1.0D30 >*/
    tmin = 1e30;
/*<       TMAX = -1.0D30 >*/
    tmax = -1e30;
/*<       TEST = .TRUE. >*/
    test = TRUE_;
/*<       ENOUGH = .FALSE. >*/
    enough = FALSE_;
/*<       BETMAX = 0.0D0 >*/
    betmax = 0.;
/*<       J = 0 >*/
    j = 0;

/* ------------------------------------------------------------------ */

/* THIS SECTION TAKES A SINGLE BLOCK LANCZOS STEP. */

/*<    80 J = J + NBLOCK >*/
L80:
    j += *nblock;

/* THIS IS THE SELECTIVE ORTHOGONALIZATION. */

/*<       IF (NUMBER.EQ.0) GO TO 110 >*/
    if (number == 0) {
        goto L110;
    }
/*<       DO 100 I=1,NUMBER >*/
    i__1 = number;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          IF (TAU(I).LT.EPSRT) GO TO 100 >*/
        if (tau[i__] < epsrt) {
            goto L100;
        }
/*<          TEST = .TRUE. >*/
        test = TRUE_;
/*<          TAU(I) = 0.0D0 >*/
        tau[i__] = 0.;
/*<          IF (OTAU(I).NE.0.0D0) OTAU(I) = 1.0D0 >*/
        if (otau[i__] != 0.) {
            otau[i__] = 1.;
        }
/*<          DO 90 K=1,NBLOCK >*/
        i__2 = *nblock;
        for (k = 1; k <= i__2; ++k) {
/*<             TEMP = -DDOT(N,VEC(1,I),1,P1(1,K),1) >*/
            temp = -ddot_(n, &vec[i__ * vec_dim1 + 1], &c__1, &p1[k * p1_dim1
                    + 1], &c__1);
/*<             CALL DAXPY(N, TEMP, VEC(1,I), 1, P1(1,K), 1) >*/
            daxpy_(n, &temp, &vec[i__ * vec_dim1 + 1], &c__1, &p1[k * p1_dim1
                    + 1], &c__1);

/* THIS CHECKS FOR TOO GREAT A LOSS OF ORTHOGONALITY BETWEEN A */
/* NEW LANCZOS VECTOR AND A GOOD RITZ VECTOR.  THE ALGORITHM IS */
/* TERMINATED IF TOO MUCH ORTHOGONALITY IS LOST. */

/*<    >*/
            if ((d__1 = temp * bet[k + k * bet_dim1], abs(d__1)) > (
                    doublereal) ((real) (*n)) * epsrt * anorm && i__ > *nperm)
                     {
                goto L380;
            }
/*<    90    CONTINUE >*/
/* L90: */
        }
/*<   100 CONTINUE >*/
L100:
        ;
    }

/* IF NECESSARY, THIS REORTHONORMALIZES P1 AND UPDATES BET. */

/*<   110 IF(.NOT. TEST) GO TO 160 >*/
L110:
    if (! test) {
        goto L160;
    }
/*<       CALL DORTQR(N, N, NBLOCK, P1, ALP) >*/
    dortqr_(n, n, nblock, &p1[p1_offset], &alp[alp_offset]);
/*<       TEST = .FALSE. >*/
    test = FALSE_;
/*<       IF(J .EQ. NBLOCK) GO TO 160 >*/
    if (j == *nblock) {
        goto L160;
    }
/*<       DO 130 I = 1,NBLOCK >*/
    i__1 = *nblock;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          IF(ALP(I,I) .GT. 0.0D0) GO TO 130 >*/
        if (alp[i__ + i__ * alp_dim1] > 0.) {
            goto L130;
        }
/*<          M = J - 2*NBLOCK + I >*/
        m = j - (*nblock << 1) + i__;
/*<          L = NBLOCK + 1 >*/
        l = *nblock + 1;
/*<          DO 120 K = I,NBLOCK >*/
        i__2 = *nblock;
        for (k = i__; k <= i__2; ++k) {
/*<             BET(I,K) = -BET(I,K) >*/
            bet[i__ + k * bet_dim1] = -bet[i__ + k * bet_dim1];
/*<             T(L,M) = -T(L,M) >*/
            t[l + m * t_dim1] = -t[l + m * t_dim1];
/*<             L = L - 1 >*/
            --l;
/*<             M = M + 1 >*/
            ++m;
/*<   120    CONTINUE >*/
/* L120: */
        }
/*<   130 CONTINUE >*/
L130:
        ;
    }

/* THIS IS THE LANCZOS STEP. */

/*<   160 CALL OP(N, NBLOCK, P1, P2) >*/
L160:
    (*op)(n, nblock, &p1[p1_offset], &p2[p2_offset]);
/*<       NOP = NOP + 1 >*/
    ++(*nop);
/*<       CALL IOVECT(N, NBLOCK, P1, J, 0) >*/
    (*iovect)(n, nblock, &p1[p1_offset], &j, &c__0);

/* THIS COMPUTES P2=P2-P0*BET(TRANSPOSE) */

/*<       DO 180 I=1,NBLOCK >*/
    i__1 = *nblock;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          DO 170 K=I,NBLOCK >*/
        i__2 = *nblock;
        for (k = i__; k <= i__2; ++k) {
/*<             CALL DAXPY(N, -BET(I,K), P0(1,K), 1, P2(1,I), 1) >*/
            d__1 = -bet[i__ + k * bet_dim1];
            daxpy_(n, &d__1, &p0[k * p0_dim1 + 1], &c__1, &p2[i__ * p2_dim1 +
                    1], &c__1);
/*<   170    CONTINUE >*/
/* L170: */
        }
/*<   180 CONTINUE >*/
/* L180: */
    }

/* THIS COMPUTES ALP AND P2=P2-P1*ALP. */

/*<       DO 200 I=1,NBLOCK >*/
    i__1 = *nblock;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          DO 190 K=1,I >*/
        i__2 = i__;
        for (k = 1; k <= i__2; ++k) {
/*<             II = I - K + 1 >*/
            ii = i__ - k + 1;
/*<             ALP(II,K) = DDOT(N,P1(1,I),1,P2(1,K),1) >*/
            alp[ii + k * alp_dim1] = ddot_(n, &p1[i__ * p1_dim1 + 1], &c__1, &
                    p2[k * p2_dim1 + 1], &c__1);
/*<             CALL DAXPY(N, -ALP(II,K), P1(1,I), 1, P2(1,K), 1) >*/
            d__1 = -alp[ii + k * alp_dim1];
            daxpy_(n, &d__1, &p1[i__ * p1_dim1 + 1], &c__1, &p2[k * p2_dim1 +
                    1], &c__1);
/*<    >*/
            if (k != i__) {
                d__1 = -alp[ii + k * alp_dim1];
                daxpy_(n, &d__1, &p1[k * p1_dim1 + 1], &c__1, &p2[i__ *
                        p2_dim1 + 1], &c__1);
            }
/*<   190   CONTINUE >*/
/* L190: */
        }
/*<   200 CONTINUE >*/
/* L200: */
    }

/*  REORTHOGONALIZATION OF THE SECOND BLOCK */

/*<       IF(J .NE. NBLOCK) GO TO 220 >*/
    if (j != *nblock) {
        goto L220;
    }
/*<       DO 215 I=1,NBLOCK >*/
    i__1 = *nblock;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          DO 210 K=1,I >*/
        i__2 = i__;
        for (k = 1; k <= i__2; ++k) {
/*<             TEMP = DDOT(N,P1(1,I),1,P2(1,K),1) >*/
            temp = ddot_(n, &p1[i__ * p1_dim1 + 1], &c__1, &p2[k * p2_dim1 +
                    1], &c__1);
/*<             CALL DAXPY(N, -TEMP, P1(1,I), 1, P2(1,K), 1) >*/
            d__1 = -temp;
            daxpy_(n, &d__1, &p1[i__ * p1_dim1 + 1], &c__1, &p2[k * p2_dim1 +
                    1], &c__1);
/*<    >*/
            if (k != i__) {
                d__1 = -temp;
                daxpy_(n, &d__1, &p1[k * p1_dim1 + 1], &c__1, &p2[i__ *
                        p2_dim1 + 1], &c__1);
            }
/*<             II = I - K + 1 >*/
            ii = i__ - k + 1;
/*<             ALP(II,K) = ALP(II,K) + TEMP             >*/
            alp[ii + k * alp_dim1] += temp;
/*<   210   CONTINUE >*/
/* L210: */
        }
/*<   215 CONTINUE >*/
/* L215: */
    }

/* THIS ORTHONORMALIZES THE NEXT BLOCK */

/*<   220 CALL DORTQR(N, N, NBLOCK, P2, BET) >*/
L220:
    dortqr_(n, n, nblock, &p2[p2_offset], &bet[bet_offset]);

/* THIS STORES ALP AND BET IN T. */

/*<       DO 250 I=1,NBLOCK >*/
    i__1 = *nblock;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          M = J - NBLOCK + I >*/
        m = j - *nblock + i__;
/*<          DO 230 K=I,NBLOCK >*/
        i__2 = *nblock;
        for (k = i__; k <= i__2; ++k) {
/*<             L = K - I + 1 >*/
            l = k - i__ + 1;
/*<             T(L,M) = ALP(L,I) >*/
            t[l + m * t_dim1] = alp[l + i__ * alp_dim1];
/*<   230    CONTINUE >*/
/* L230: */
        }
/*<          DO 240 K=1,I >*/
        i__2 = i__;
        for (k = 1; k <= i__2; ++k) {
/*<             L = NBLOCK - I + K + 1 >*/
            l = *nblock - i__ + k + 1;
/*<             T(L,M) = BET(K,I) >*/
            t[l + m * t_dim1] = bet[k + i__ * bet_dim1];
/*<   240    CONTINUE >*/
/* L240: */
        }
/*<   250 CONTINUE >*/
/* L250: */
    }

/* THIS NEGATES T IF SMALL IS FALSE. */

/*<       IF (SMALL) GO TO 280 >*/
    if (*small) {
        goto L280;
    }
/*<       M = J - NBLOCK + 1 >*/
    m = j - *nblock + 1;
/*<       DO 270 I=M,J >*/
    i__1 = j;
    for (i__ = m; i__ <= i__1; ++i__) {
/*<          DO 260 K=1,L >*/
        i__2 = l;
        for (k = 1; k <= i__2; ++k) {
/*<             T(K,I) = -T(K,I) >*/
            t[k + i__ * t_dim1] = -t[k + i__ * t_dim1];
/*<   260    CONTINUE >*/
/* L260: */
        }
/*<   270 CONTINUE >*/
/* L270: */
    }

/* THIS SHIFTS THE LANCZOS VECTORS */

/*<   280 CALL DCOPY(NBLOCK*N, P1, 1, P0, 1) >*/
L280:
    i__1 = *nblock * *n;
    dcopy_(&i__1, &p1[p1_offset], &c__1, &p0[p0_offset], &c__1);
/*<       CALL DCOPY(NBLOCK*N, P2, 1, P1, 1) >*/
    i__1 = *nblock * *n;
    dcopy_(&i__1, &p2[p2_offset], &c__1, &p1[p1_offset], &c__1);
/*<       CALL DLAGER(J, NBAND, J-NBLOCK+1, T, TMIN, TMAX) >*/
    i__1 = j - *nblock + 1;
    dlager_(&j, nband, &i__1, &t[t_offset], &tmin, &tmax);
/*<       ANORM = DMAX1(RNORM, TMAX, -TMIN) >*/
/* Computing MAX */
    d__1 = max(rnorm,tmax), d__2 = -tmin;
    anorm = max(d__1,d__2);
/*<       IF (NUMBER.EQ.0) GO TO 305 >*/
    if (number == 0) {
        goto L305;
    }

/* THIS COMPUTES THE EXTREME EIGENVALUES OF ALP. */

/*<       CALL DCOPY(NBLOCK, DZERO, 0, P2, 1) >*/
    dcopy_(nblock, dzero, &c__0, &p2[p2_offset], &c__1);
/*<    >*/
    dlaeig_(nblock, nblock, &c__1, &c__1, &alp[alp_offset], tarr, nblock, &p2[
            p2_offset], &bound[1], &atemp[1], &d__[1], &vtemp[1], eps, &tmin,
            &tmax);
/*<       ALPMIN = TARR(1) >*/
    alpmin = tarr[0];
/*<       CALL DCOPY(NBLOCK, DZERO, 0, P2, 1) >*/
    dcopy_(nblock, dzero, &c__0, &p2[p2_offset], &c__1);
/*<    >*/
    dlaeig_(nblock, nblock, nblock, nblock, &alp[alp_offset], tarr, nblock, &
            p2[p2_offset], &bound[1], &atemp[1], &d__[1], &vtemp[1], eps, &
            tmin, &tmax);
/*<       ALPMAX = TARR(1) >*/
    alpmax = tarr[0];

/* THIS COMPUTES ALP = BET(TRANSPOSE)*BET. */

/*<   305 DO 310 I = 1, NBLOCK >*/
L305:
    i__1 = *nblock;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          DO 300 K = 1, I >*/
        i__2 = i__;
        for (k = 1; k <= i__2; ++k) {
/*<             L = I - K + 1 >*/
            l = i__ - k + 1;
/*<    >*/
            i__3 = *nblock - i__ + 1;
            alp[l + k * alp_dim1] = ddot_(&i__3, &bet[i__ + i__ * bet_dim1],
                    nblock, &bet[k + i__ * bet_dim1], nblock);
/*<   300    CONTINUE >*/
/* L300: */
        }
/*<   310 CONTINUE >*/
/* L310: */
    }
/*<       IF(NUMBER .EQ. 0) GO TO 330 >*/
    if (number == 0) {
        goto L330;
    }

/* THIS COMPUTES THE SMALLEST SINGULAR VALUE OF BET. */

/*<       CALL DCOPY(NBLOCK, DZERO, 0, P2, 1) >*/
    dcopy_(nblock, dzero, &c__0, &p2[p2_offset], &c__1);
/*<    >*/
    d__1 = anorm * anorm;
    dlaeig_(nblock, nblock, &c__1, &c__1, &alp[alp_offset], tarr, nblock, &p2[
            p2_offset], &bound[1], &atemp[1], &d__[1], &vtemp[1], eps, &c_b88,
             &d__1);
/*<       BETMIN = DSQRT(TARR(1)) >*/
    betmin = sqrt(tarr[0]);

/* THIS UPDATES TAU AND OTAU. */

/*<       DO 320 I=1,NUMBER >*/
    i__1 = number;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<    >*/
/* Computing MAX */
        d__1 = alpmax - val[i__], d__2 = val[i__] - alpmin;
        temp = (tau[i__] * max(d__1,d__2) + otau[i__] * betmax + *eps * anorm)
                 / betmin;
/*<          IF (I.LE.NPERM) TEMP = TEMP + RES(I)/BETMIN >*/
        if (i__ <= *nperm) {
            temp += res[i__] / betmin;
        }
/*<          OTAU(I) = TAU(I) >*/
        otau[i__] = tau[i__];
/*<          TAU(I) = TEMP >*/
        tau[i__] = temp;
/*<   320 CONTINUE >*/
/* L320: */
    }

/* THIS COMPUTES THE LARGEST SINGULAR VALUE OF BET. */

/*<   330 CALL DCOPY(NBLOCK, DZERO, 0, P2, 1) >*/
L330:
    dcopy_(nblock, dzero, &c__0, &p2[p2_offset], &c__1);
/*<    >*/
    d__1 = anorm * anorm;
    dlaeig_(nblock, nblock, nblock, nblock, &alp[alp_offset], tarr, nblock, &
            p2[p2_offset], &bound[1], &atemp[1], &d__[1], &vtemp[1], eps, &
            c_b88, &d__1);
/*<       BETMAX = DSQRT(TARR(1)) >*/
    betmax = sqrt(tarr[0]);
/*<       IF (J.LE.2*NBLOCK) GO TO 80 >*/
    if (j <= *nblock << 1) {
        goto L80;
    }

/* ------------------------------------------------------------------ */

/* THIS SECTION COMPUTES AND EXAMINES THE SMALLEST NONGOOD AND */
/* LARGEST DESIRED EIGENVALUES OF T TO SEE IF A CLOSER LOOK */
/* IS JUSTIFIED. */

/*<       TOLG = EPSRT*ANORM >*/
    tolg = epsrt * anorm;
/*<       TOLA = UTOL*RNORM >*/
    tola = utol * rnorm;
/*<    >*/
    if (*maxj - j < *nblock || (*nop >= *maxop && nleft != 0)) {
        goto L390;
    }
/*<       GO TO 400 >*/
    goto L400;

/* ------------------------------------------------------------------ */

/* THIS SECTION COMPUTES SOME EIGENVALUES AND EIGENVECTORS OF T TO */
/* SEE IF FURTHER ACTION IS INDICATED, ENTRY IS AT 380 OR 390 IF AN */
/* ITERATION (OR TERMINATION) IS KNOWN TO BE NEEDED, OTHERWISE ENTRY */
/* IS AT 400. */

/*<   380 J = J - NBLOCK >*/
L380:
    j -= *nblock;
/*<       IERR = -8 >*/
    *ierr = -8;
/*<   390 IF (NLEFT.EQ.0) RETURN >*/
L390:
    if (nleft == 0) {
        return 0;
    }
/*<       TEST = .TRUE. >*/
    test = TRUE_;
/*<   400 NTHETA = MIN0(J/2,NLEFT+1) >*/
L400:
/* Computing MIN */
    i__1 = j / 2, i__2 = nleft + 1;
    ntheta = min(i__1,i__2);
/*<    >*/
    dlaeig_(&j, nband, &c__1, &ntheta, &t[t_offset], &val[number + 1], maxj, &
            s[s_offset], &bound[1], &atemp[1], &d__[1], &vtemp[1], eps, &tmin,
             &tmax);
/*<       CALL DMVPC(NBLOCK, BET, MAXJ, J, S, NTHETA, ATEMP, VTEMP, D) >*/
    dmvpc_(nblock, &bet[bet_offset], maxj, &j, &s[s_offset], &ntheta, &atemp[
            1], &vtemp[1], &d__[1]);

/* THIS CHECKS FOR TERMINATION OF A CHECK RUN */

/*<       IF(NLEFT .NE. 0 .OR. J .LT. 6*NBLOCK) GO TO 410 >*/
    if (nleft != 0 || j < *nblock * 6) {
        goto L410;
    }
/*<       IF(VAL(NUMBER+1)-ATEMP(1) .GT. VAL(NPERM) - TOLA) GO TO 790  >*/
    if (val[number + 1] - atemp[1] > val[*nperm] - tola) {
        goto L790;
    }

/* THIS UPDATES NLEFT BY EXAMINING THE COMPUTED EIGENVALUES OF T */
/* TO DETERMINE IF SOME PERMANENT VALUES ARE NO LONGER DESIRED. */

/*<  410  IF (NTHETA.LE.NLEFT) GO TO 470 >*/
L410:
    if (ntheta <= nleft) {
        goto L470;
    }
/*<       IF (NPERM.EQ.0) GO TO 430 >*/
    if (*nperm == 0) {
        goto L430;
    }
/*<       M = NUMBER + NLEFT + 1 >*/
    m = number + nleft + 1;
/*<       IF (VAL(M).GE.VAL(NPERM)) GO TO 430 >*/
    if (val[m] >= val[*nperm]) {
        goto L430;
    }
/*<       NPERM = NPERM - 1 >*/
    --(*nperm);
/*<       NGOOD = 0 >*/
    ngood = 0;
/*<       NUMBER = NPERM >*/
    number = *nperm;
/*<       NLEFT = NLEFT + 1 >*/
    ++nleft;
/*<       GO TO 400 >*/
    goto L400;

/* THIS UPDATES DELTA. */

/*<   430 M = NUMBER + NLEFT + 1 >*/
L430:
    m = number + nleft + 1;
/*<       DELTA = DMIN1(DELTA,VAL(M)) >*/
/* Computing MIN */
    d__1 = *delta, d__2 = val[m];
    *delta = min(d__1,d__2);
/*<       ENOUGH = .TRUE. >*/
    enough = TRUE_;
/*<       IF(NLEFT .EQ. 0) GO TO 80 >*/
    if (nleft == 0) {
        goto L80;
    }
/*<       NTHETA = NLEFT >*/
    ntheta = nleft;
/*<       VTEMP(NTHETA+1) = 1 >*/
    vtemp[ntheta + 1] = 1.;

/* ------------------------------------------------------------------ */

/* THIS SECTION EXAMINES THE COMPUTED EIGENPAIRS IN DETAIL. */

/* THIS CHECKS FOR ENOUGH ACCEPTABLE VALUES. */

/*<       IF (.NOT.(TEST .OR. ENOUGH)) GO TO 470 >*/
    if (! (test || enough)) {
        goto L470;
    }
/*<       DELTA = DMIN1(DELTA,ANORM) >*/
    *delta = min(*delta,anorm);
/*<       PNORM = DMAX1(RNORM,DMAX1(-VAL(NUMBER+1),DELTA)) >*/
/* Computing MAX */
/* Computing MAX */
    d__3 = -val[number + 1];
    d__1 = rnorm, d__2 = max(d__3,*delta);
    pnorm = max(d__1,d__2);
/*<       TOLA = UTOL*PNORM >*/
    tola = utol * pnorm;
/*<       NSTART = 0 >*/
    nstart = 0;
/*<       DO 460 I=1,NTHETA >*/
    i__1 = ntheta;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          M = NUMBER + I >*/
        m = number + i__;
/*<    >*/
/* Computing MIN */
        d__1 = atemp[i__] * atemp[i__] / (*delta - val[m]), d__2 = atemp[i__];
        if (min(d__1,d__2) > tola) {
            goto L450;
        }
/*<          IND(I) = -1 >*/
        ind[i__] = -1;
/*<          GO TO 460 >*/
        goto L460;

/*<   450    ENOUGH = .FALSE. >*/
L450:
        enough = FALSE_;
/*<          IF (.NOT.TEST) GO TO 470 >*/
        if (! test) {
            goto L470;
        }
/*<          IND(I) = 1 >*/
        ind[i__] = 1;
/*<          NSTART = NSTART + 1 >*/
        ++nstart;
/*<   460 CONTINUE >*/
L460:
        ;
    }

/*  COPY VALUES OF IND INTO VTEMP */

/*<       DO 465 I = 1,NTHETA >*/
    i__1 = ntheta;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          VTEMP(I) = DBLE(FLOAT(IND(I))) >*/
        vtemp[i__] = (doublereal) ((real) ind[i__]);
/*<   465 CONTINUE >*/
/* L465: */
    }
/*<       GO TO 500 >*/
    goto L500;

/* THIS CHECKS FOR NEW GOOD VECTORS. */

/*<   470 NG = 0 >*/
L470:
    ng = 0;
/*<       DO 490 I=1,NTHETA >*/
    i__1 = ntheta;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          IF (VTEMP(I).GT.TOLG) GO TO 480 >*/
        if (vtemp[i__] > tolg) {
            goto L480;
        }
/*<          NG = NG + 1 >*/
        ++ng;
/*<          VTEMP(I) = -1 >*/
        vtemp[i__] = -1.;
/*<          GO TO 490 >*/
        goto L490;

/*<   480    VTEMP(I) = 1 >*/
L480:
        vtemp[i__] = 1.;
/*<   490 CONTINUE >*/
L490:
        ;
    }

/*<       IF (NG.LE.NGOOD) GO TO 80 >*/
    if (ng <= ngood) {
        goto L80;
    }
/*<       NSTART = NTHETA - NG >*/
    nstart = ntheta - ng;

/* ------------------------------------------------------------------ */

/* THIS SECTION COMPUTES AND NORMALIZES THE INDICATED RITZ VECTORS. */
/* IF NEEDED (TEST = .TRUE.), NEW STARTING VECTORS ARE COMPUTED. */

/*<   500 TEST = TEST .AND. .NOT.ENOUGH >*/
L500:
    test = test && ! enough;
/*<       NGOOD = NTHETA - NSTART >*/
    ngood = ntheta - nstart;
/*<       NSTART = NSTART + 1 >*/
    ++nstart;
/*<       NTHETA = NTHETA + 1 >*/
    ++ntheta;

/* THIS ALIGNS THE DESIRED (ACCEPTABLE OR GOOD) EIGENVALUES AND */
/* EIGENVECTORS OF T.  THE OTHER EIGENVECTORS ARE SAVED FOR */
/* FORMING STARTING VECTORS, IF NECESSARY.  IT ALSO SHIFTS THE */
/* EIGENVALUES TO OVERWRITE THE GOOD VALUES FROM THE PREVIOUS */
/* PAUSE. */

/*<       CALL DCOPY(NTHETA, VAL(NUMBER+1), 1, VAL(NPERM+1), 1) >*/
    dcopy_(&ntheta, &val[number + 1], &c__1, &val[*nperm + 1], &c__1);
/*<       IF (NSTART.EQ.0) GO TO 580 >*/
    if (nstart == 0) {
        goto L580;
    }
/*<       IF (NSTART.EQ.NTHETA) GO TO 530 >*/
    if (nstart == ntheta) {
        goto L530;
    }
/*<    >*/
    dvsort_(&ntheta, &vtemp[1], &atemp[1], &c__1, &val[*nperm + 1], maxj, &j,
            &s[s_offset]);

/* THES ACCUMULATES THE J-VECTORS USED TO FORM THE STARTING */
/* VECTORS. */

/*<   530 IF (.NOT.TEST) NSTART = 0 >*/
L530:
    if (! test) {
        nstart = 0;
    }
/*<       IF (.NOT.TEST) GO TO 580 >*/
    if (! test) {
        goto L580;
    }

/*  FIND MINIMUM ATEMP VALUE TO AVOID POSSIBLE OVERFLOW */

/*<       TEMP = ATEMP(1) >*/
    temp = atemp[1];
/*<       DO 535 I = 1, NSTART >*/
    i__1 = nstart;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          TEMP = DMIN1(TEMP, ATEMP(I)) >*/
/* Computing MIN */
        d__1 = temp, d__2 = atemp[i__];
        temp = min(d__1,d__2);
/*<   535 CONTINUE >*/
/* L535: */
    }
/*<       M = NGOOD + 1 >*/
    m = ngood + 1;
/*<       L = NGOOD + MIN0(NSTART,NBLOCK) >*/
    l = ngood + min(nstart,*nblock);
/*<       DO 540 I=M,L >*/
    i__1 = l;
    for (i__ = m; i__ <= i__1; ++i__) {
/*<          CALL DSCAL(J, TEMP/ATEMP(I), S(1,I), 1) >*/
        d__1 = temp / atemp[i__];
        dscal_(&j, &d__1, &s[i__ * s_dim1 + 1], &c__1);
/*<   540 CONTINUE >*/
/* L540: */
    }
/*<       M = (NSTART-1)/NBLOCK >*/
    m = (nstart - 1) / *nblock;
/*<       IF (M.EQ.0) GO TO 570 >*/
    if (m == 0) {
        goto L570;
    }
/*<       L = NGOOD + NBLOCK >*/
    l = ngood + *nblock;
/*<       DO 560 I=1,M >*/
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          DO 550 K=1,NBLOCK >*/
        i__2 = *nblock;
        for (k = 1; k <= i__2; ++k) {
/*<             L = L + 1 >*/
            ++l;
/*<             IF (L.GT.NTHETA) GO TO 570 >*/
            if (l > ntheta) {
                goto L570;
            }
/*<             I1 = NGOOD + K >*/
            i1 = ngood + k;
/*<             CALL DAXPY(J, TEMP/ATEMP(L), S(1,L), 1, S(1,I1), 1) >*/
            d__1 = temp / atemp[l];
            daxpy_(&j, &d__1, &s[l * s_dim1 + 1], &c__1, &s[i1 * s_dim1 + 1],
                    &c__1);
/*<   550    CONTINUE >*/
/* L550: */
        }
/*<   560 CONTINUE >*/
/* L560: */
    }
/*<   570 NSTART = MIN0(NSTART,NBLOCK) >*/
L570:
    nstart = min(nstart,*nblock);

/* THIS STORES THE RESIDUAL NORMS OF THE NEW PERMANENT VECTORS. */

/*<   580 IF (NGOOD.EQ.0 .OR. .NOT.(TEST .OR. ENOUGH)) GO TO 600 >*/
L580:
    if (ngood == 0 || ! (test || enough)) {
        goto L600;
    }
/*<       DO 590 I=1,NGOOD >*/
    i__1 = ngood;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          M = NPERM + I >*/
        m = *nperm + i__;
/*<          RES(M) = ATEMP(I) >*/
        res[m] = atemp[i__];
/*<   590 CONTINUE >*/
/* L590: */
    }

/* THIS COMPUTES THE RITZ VECTORS BY SEQUENTIALLY RECALLING THE */
/* LANCZOS VECTORS. */

/*<   600 NUMBER = NPERM + NGOOD >*/
L600:
    number = *nperm + ngood;
/*<       IF (TEST .OR. ENOUGH) CALL DCOPY(N*NBLOCK, DZERO, 0, P1, 1) >*/
    if (test || enough) {
        i__1 = *n * *nblock;
        dcopy_(&i__1, dzero, &c__0, &p1[p1_offset], &c__1);
    }
/*<       IF (NGOOD.EQ.0) GO TO 620 >*/
    if (ngood == 0) {
        goto L620;
    }
/*<       M = NPERM + 1 >*/
    m = *nperm + 1;
/*<       DO 610 I=M,NUMBER >*/
    i__1 = number;
    for (i__ = m; i__ <= i__1; ++i__) {
/*<          CALL DCOPY(N, DZERO, 0, VEC(1,I), 1) >*/
        dcopy_(n, dzero, &c__0, &vec[i__ * vec_dim1 + 1], &c__1);
/*<   610 CONTINUE >*/
/* L610: */
    }
/*<   620 DO 670 I=NBLOCK,J,NBLOCK >*/
L620:
    i__1 = j;
    i__2 = *nblock;
    for (i__ = *nblock; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<          CALL IOVECT(N, NBLOCK, P2, I, 1) >*/
        (*iovect)(n, nblock, &p2[p2_offset], &i__, &c__1);
/*<          DO 660 K=1,NBLOCK >*/
        i__3 = *nblock;
        for (k = 1; k <= i__3; ++k) {
/*<             M = I - NBLOCK + K >*/
            m = i__ - *nblock + k;
/*<             IF (NSTART.EQ.0) GO TO 640 >*/
            if (nstart == 0) {
                goto L640;
            }
/*<             DO 630 L=1,NSTART >*/
            i__4 = nstart;
            for (l = 1; l <= i__4; ++l) {
/*<                I1 = NGOOD + L >*/
                i1 = ngood + l;
/*<                CALL DAXPY(N, S(M,I1), P2(1,K), 1, P1(1,L), 1) >*/
                daxpy_(n, &s[m + i1 * s_dim1], &p2[k * p2_dim1 + 1], &c__1, &
                        p1[l * p1_dim1 + 1], &c__1);
/*<   630       CONTINUE >*/
/* L630: */
            }
/*<   640       IF (NGOOD.EQ.0) GO TO 660 >*/
L640:
            if (ngood == 0) {
                goto L660;
            }
/*<             DO 650 L=1,NGOOD >*/
            i__4 = ngood;
            for (l = 1; l <= i__4; ++l) {
/*<                I1 = L + NPERM >*/
                i1 = l + *nperm;
/*<                CALL DAXPY(N, S(M,L), P2(1,K), 1, VEC(1,I1), 1) >*/
                daxpy_(n, &s[m + l * s_dim1], &p2[k * p2_dim1 + 1], &c__1, &
                        vec[i1 * vec_dim1 + 1], &c__1);
/*<   650       CONTINUE >*/
/* L650: */
            }
/*<   660    CONTINUE >*/
L660:
            ;
        }
/*<   670 CONTINUE >*/
/* L670: */
    }
/*<       IF (TEST .OR. ENOUGH) GO TO 690 >*/
    if (test || enough) {
        goto L690;
    }

/* THIS NORMALIZES THE RITZ VECTORS AND INITIALIZES THE */
/* TAU RECURRENCE. */

/*<       M = NPERM + 1 >*/
    m = *nperm + 1;
/*<       DO 680 I=M,NUMBER >*/
    i__2 = number;
    for (i__ = m; i__ <= i__2; ++i__) {
/*<          TEMP = 1.0D0/DNRM2(N,VEC(1,I),1) >*/
        temp = 1. / dnrm2_(n, &vec[i__ * vec_dim1 + 1], &c__1);
/*<          CALL DSCAL(N, TEMP, VEC(1,I), 1) >*/
        dscal_(n, &temp, &vec[i__ * vec_dim1 + 1], &c__1);
/*<          TAU(I) = 1.0D0 >*/
        tau[i__] = 1.;
/*<          OTAU(I) = 1.0D0 >*/
        otau[i__] = 1.;
/*<   680 CONTINUE >*/
/* L680: */
    }

/*  SHIFT S VECTORS TO ALIGN FOR LATER CALL TO DLAEIG */

/*<       CALL DCOPY(NTHETA, VAL(NPERM+1), 1, VTEMP, 1) >*/
    dcopy_(&ntheta, &val[*nperm + 1], &c__1, &vtemp[1], &c__1);
/*<       CALL DVSORT(NTHETA, VTEMP, ATEMP, 0, TARR, MAXJ, J, S) >*/
    dvsort_(&ntheta, &vtemp[1], &atemp[1], &c__0, tarr, maxj, &j, &s[s_offset]
            );
/*<       GO TO 80 >*/
    goto L80;

/* ------------------------------------------------------------------ */

/* THIS SECTION PREPARES TO ITERATE THE ALGORITHM BY SORTING THE */
/* PERMANENT VALUES, RESETTING SOME PARAMETERS, AND ORTHONORMALIZING */
/* THE PERMANENT VECTORS. */

/*<   690 IF (NGOOD.EQ.0 .AND. NOP.GE.MAXOP) GO TO 810 >*/
L690:
    if (ngood == 0 && *nop >= *maxop) {
        goto L810;
    }
/*<       IF (NGOOD.EQ.0) GO TO 30 >*/
    if (ngood == 0) {
        goto L30;
    }

/* THIS ORTHONORMALIZES THE VECTORS */

/*<       CALL DORTQR(NMVEC, N, NPERM+NGOOD, VEC, S) >*/
    i__2 = *nperm + ngood;
    dortqr_(nmvec, n, &i__2, &vec[vec_offset], &s[s_offset]);

/* THIS SORTS THE VALUES AND VECTORS. */

/*<    >*/
    if (*nperm != 0) {
        i__2 = *nperm + ngood;
        dvsort_(&i__2, &val[1], &res[1], &c__0, &temp, nmvec, n, &vec[
                vec_offset]);
    }
/*<       NPERM = NPERM + NGOOD >*/
    *nperm += ngood;
/*<       NLEFT = NLEFT - NGOOD >*/
    nleft -= ngood;
/*<       RNORM = DMAX1(-VAL(1),VAL(NPERM)) >*/
/* Computing MAX */
    d__1 = -val[1], d__2 = val[*nperm];
    rnorm = max(d__1,d__2);

/* THIS DECIDES WHERE TO GO NEXT. */

/*<       IF (NOP.GE.MAXOP .AND. NLEFT.NE.0) GO TO 810 >*/
    if (*nop >= *maxop && nleft != 0) {
        goto L810;
    }
/*<       IF (NLEFT.NE.0) GO TO 30 >*/
    if (nleft != 0) {
        goto L30;
    }
/*<       IF (VAL(NVAL)-VAL(1).LT.TOLA) GO TO 790 >*/
    if (val[*nval] - val[1] < tola) {
        goto L790;
    }

/* THIS DOES A CLUSTER TEST TO SEE IF A CHECK RUN IS NEEDED */
/* TO LOOK FOR UNDISCLOSED MULTIPLICITIES. */

/*<       M = NPERM - NBLOCK + 1 >*/
    m = *nperm - *nblock + 1;
/*<       IF (M.LE.0) RETURN >*/
    if (m <= 0) {
        return 0;
    }
/*<       DO 780 I=1,M >*/
    i__2 = m;
    for (i__ = 1; i__ <= i__2; ++i__) {
/*<          L = I + NBLOCK - 1 >*/
        l = i__ + *nblock - 1;
/*<          IF (VAL(L)-VAL(I).LT.TOLA) GO TO 30 >*/
        if (val[l] - val[i__] < tola) {
            goto L30;
        }
/*<   780 CONTINUE >*/
/* L780: */
    }

/* THIS DOES A CLUSTER TEST TO SEE IF A FINAL RAYLEIGH-RITZ */
/* PROCEDURE IS NEEDED. */

/*<   790 M = NPERM - NBLOCK >*/
L790:
    m = *nperm - *nblock;
/*<       IF (M.LE.0) RETURN >*/
    if (m <= 0) {
        return 0;
    }
/*<       DO 800 I=1,M >*/
    i__2 = m;
    for (i__ = 1; i__ <= i__2; ++i__) {
/*<          L = I + NBLOCK >*/
        l = i__ + *nblock;
/*<          IF (VAL(L)-VAL(I).GE.TOLA) GO TO 800 >*/
        if (val[l] - val[i__] >= tola) {
            goto L800;
        }
/*<          RARITZ = .TRUE. >*/
        *raritz = TRUE_;
/*<          RETURN >*/
        return 0;
/*<   800 CONTINUE >*/
L800:
        ;
    }

/*<       RETURN >*/
    return 0;

/* ------------------------------------------------------------------ */

/* THIS REPORTS THAT MAXOP WAS EXCEEDED. */

/*<   810 IERR = -2 >*/
L810:
    *ierr = -2;
/*<       GO TO 790 >*/
    goto L790;

/*<       END >*/
} /* dnwla_ */

#ifdef __cplusplus
        }
#endif
