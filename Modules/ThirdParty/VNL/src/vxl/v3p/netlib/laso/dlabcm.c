/* laso/dlabcm.f -- translated by f2c (version 20050501).
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


/* *********************************************************************** */

/*<    >*/
/* Subroutine */ int dlabcm_(integer *n, integer *nband, integer *nl, integer
        *nr, doublereal *a, doublereal *eigval, integer *lde, doublereal *
        eigvec, doublereal *atol, doublereal *artol, doublereal *bound,
        doublereal *atemp, doublereal *d__, doublereal *vtemp)
{
    /* System generated locals */
    integer a_dim1, a_offset, eigvec_dim1, eigvec_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Local variables */
    integer i__, j, l, m;
    doublereal rq, gap;
    logical flag__;
    doublereal errb;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *,
            integer *);
    integer nval, numl;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *);
    doublereal sigma, resid;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *,
            doublereal *, integer *), daxpy_(integer *, doublereal *,
            doublereal *, integer *, doublereal *, integer *);
    doublereal vnorm;
    extern /* Subroutine */ int dlabfc_(integer *, integer *, doublereal *,
            doublereal *, integer *, integer *, doublereal *, integer *,
            integer *, doublereal *, doublereal *, doublereal *), dlabax_(
            integer *, integer *, doublereal *, doublereal *, doublereal *),
            dlaran_(integer *, doublereal *);
    integer numvec;


/*  THIS SUBROUTINE ORGANIZES THE CALCULATION OF THE EIGENVALUES */
/*  FOR THE BNDEIG PACKAGE.  EIGENVALUES ARE COMPUTED BY */
/*  A MODIFIED RAYLEIGH QUOTIENT ITERATION.  THE EIGENVALUE COUNT */
/*  OBTAINED BY EACH FACTORIZATION IS USED TO OCCASIONALLY OVERRIDE */
/*  THE COMPUTED RAYLEIGH QUOTIENT WITH A DIFFERENT SHIFT TO */
/*  INSURE CONVERGENCE TO THE DESIRED EIGENVALUES. */

/*  FORMAL PARAMETERS. */

/*<       INTEGER N, NBAND, NL, NR, LDE >*/
/*<    >*/


/*  LOCAL VARIABLES */

/*<       LOGICAL FLAG >*/
/*<       INTEGER I, J, L, M, NUML, NUMVEC, NVAL >*/
/*<       DOUBLE PRECISION ERRB, GAP, RESID, RQ, SIGMA, VNORM >*/


/*  FUNCTIONS CALLED */

/*<       INTEGER MIN0 >*/
/*<       DOUBLE PRECISION DMAX1, DMIN1, DDOT, DNRM2 >*/

/*  SUBROUTINES CALLED */

/*     DLABAX, DLABFC, DLARAN, DAXPY, DCOPY, DSCAL */

/*  REPLACE ZERO VECTORS BY RANDOM */

/*<       NVAL = NR - NL + 1 >*/
    /* Parameter adjustments */
    a_dim1 = *nband;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --eigval;
    eigvec_dim1 = *lde;
    eigvec_offset = 1 + eigvec_dim1;
    eigvec -= eigvec_offset;
    bound -= 3;
    --atemp;
    --d__;
    --vtemp;

    /* Function Body */
    nval = *nr - *nl + 1;
/*<       FLAG = .FALSE. >*/
    flag__ = FALSE_;
/*<       DO 5 I = 1, NVAL >*/
    i__1 = nval;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<    >*/
        if (ddot_(n, &eigvec[i__ * eigvec_dim1 + 1], &c__1, &eigvec[i__ *
                eigvec_dim1 + 1], &c__1) == 0.) {
            dlaran_(n, &eigvec[i__ * eigvec_dim1 + 1]);
        }
/*<     5 CONTINUE >*/
/* L5: */
    }

/*  LOOP OVER EIGENVALUES */

/*<       SIGMA = BOUND(2,NVAL+1) >*/
    sigma = bound[((nval + 1) << 1) + 2];
/*<       DO 400 J = 1, NVAL >*/
    i__1 = nval;
    for (j = 1; j <= i__1; ++j) {
/*<          NUML = J >*/
        numl = j;

/*  PREPARE TO COMPUTE FIRST RAYLEIGH QUOTIENT */

/*<    10    CALL DLABAX(N, NBAND, A, EIGVEC(1,J), VTEMP) >*/
L10:
        dlabax_(n, nband, &a[a_offset], &eigvec[j * eigvec_dim1 + 1], &vtemp[
                1]);
/*<          VNORM = DNRM2(N, VTEMP, 1) >*/
        vnorm = dnrm2_(n, &vtemp[1], &c__1);
/*<          IF(VNORM .EQ. 0.0D0) GO TO 20 >*/
        if (vnorm == 0.) {
            goto L20;
        }
/*<          CALL DSCAL(N, 1.0D0/VNORM, VTEMP, 1) >*/
        d__1 = 1. / vnorm;
        dscal_(n, &d__1, &vtemp[1], &c__1);
/*<          CALL DSCAL(N, 1.0D0/VNORM, EIGVEC(1,J), 1) >*/
        d__1 = 1. / vnorm;
        dscal_(n, &d__1, &eigvec[j * eigvec_dim1 + 1], &c__1);
/*<          CALL DAXPY(N, -SIGMA, EIGVEC(1,J), 1, VTEMP, 1) >*/
        d__1 = -sigma;
        daxpy_(n, &d__1, &eigvec[j * eigvec_dim1 + 1], &c__1, &vtemp[1], &
                c__1);

/*  LOOP OVER SHIFTS */

/*  COMPUTE RAYLEIGH QUOTIENT, RESIDUAL NORM, AND CURRENT TOLERANCE */

/*<    20       VNORM = DNRM2(N, EIGVEC(1,J), 1) >*/
L20:
        vnorm = dnrm2_(n, &eigvec[j * eigvec_dim1 + 1], &c__1);
/*<             IF(VNORM .NE. 0.0D0) GO TO 30 >*/
        if (vnorm != 0.) {
            goto L30;
        }
/*<             CALL DLARAN(N, EIGVEC(1,J)) >*/
        dlaran_(n, &eigvec[j * eigvec_dim1 + 1]);
/*<             GO TO 10 >*/
        goto L10;

/*<    >*/
L30:
        rq = sigma + ddot_(n, &eigvec[j * eigvec_dim1 + 1], &c__1, &vtemp[1],
                &c__1) / vnorm / vnorm;
/*<             CALL DAXPY(N, SIGMA-RQ, EIGVEC(1,J), 1, VTEMP, 1) >*/
        d__1 = sigma - rq;
        daxpy_(n, &d__1, &eigvec[j * eigvec_dim1 + 1], &c__1, &vtemp[1], &
                c__1);
/*<             RESID = DMAX1(ATOL, DNRM2(N, VTEMP, 1)/VNORM) >*/
/* Computing MAX */
        d__1 = *atol, d__2 = dnrm2_(n, &vtemp[1], &c__1) / vnorm;
        resid = max(d__1,d__2);
/*<             CALL DSCAL(N, 1.0/VNORM, EIGVEC(1,J), 1) >*/
        d__1 = (float)1. / vnorm;
        dscal_(n, &d__1, &eigvec[j * eigvec_dim1 + 1], &c__1);

/*  ACCEPT EIGENVALUE IF THE INTERVAL IS SMALL ENOUGH */

/*<             IF(BOUND(2,J+1) - BOUND(1,J+1) .LT. 3.0D0*ATOL) GO TO 300 >*/
        if (bound[((j + 1) << 1) + 2] - bound[((j + 1) << 1) + 1] < *atol * 3.) {
            goto L300;
        }

/*  COMPUTE MINIMAL ERROR BOUND */

/*<             ERRB = RESID >*/
        errb = resid;
/*<             GAP = DMIN1(BOUND(1,J+2) - RQ, RQ - BOUND(2,J)) >*/
/* Computing MIN */
        d__1 = bound[((j + 2) << 1) + 1] - rq, d__2 = rq - bound[(j << 1) + 2];
        gap = min(d__1,d__2);
/*<             IF(GAP .GT. RESID) ERRB = DMAX1(ATOL, RESID*RESID/GAP) >*/
        if (gap > resid) {
/* Computing MAX */
            d__1 = *atol, d__2 = resid * resid / gap;
            errb = max(d__1,d__2);
        }

/*  TENTATIVE NEW SHIFT */

/*<             SIGMA = 0.5D0*(BOUND(1,J+1) + BOUND(2,J+1)) >*/
        sigma = (bound[((j + 1) << 1) + 1] + bound[((j + 1) << 1) + 2]) * .5;

/*  CHECK FOR TERMINALTION */

/*<             IF(RESID .GT. 2.0D0*ATOL) GO TO 40 >*/
        if (resid > *atol * 2.) {
            goto L40;
        }
/*<    >*/
        if (rq - errb > bound[(j << 1) + 2] && rq + errb < bound[((j + 2) << 1)
                + 1]) {
            goto L310;
        }

/*  RQ IS TO THE LEFT OF THE INTERVAL */

/*<    40       IF(RQ .GE. BOUND(1,J+1)) GO TO 50 >*/
L40:
        if (rq >= bound[((j + 1) << 1) + 1]) {
            goto L50;
        }
/*<             IF(RQ - ERRB .GT. BOUND(2,J)) GO TO 100 >*/
        if (rq - errb > bound[(j << 1) + 2]) {
            goto L100;
        }
/*<             IF(RQ + ERRB .LT. BOUND(1,J+1)) CALL DLARAN(N,EIGVEC(1,J)) >*/
        if (rq + errb < bound[((j + 1) << 1) + 1]) {
            dlaran_(n, &eigvec[j * eigvec_dim1 + 1]);
        }
/*<             GO TO 200 >*/
        goto L200;

/*  RQ IS TO THE RIGHT OF THE INTERVAL */

/*<    50       IF(RQ .LE. BOUND(2,J+1)) GO TO 100 >*/
L50:
        if (rq <= bound[((j + 1) << 1) + 2]) {
            goto L100;
        }
/*<             IF(RQ + ERRB .LT. BOUND(1,J+2)) GO TO 100 >*/
        if (rq + errb < bound[((j + 2) << 1) + 1]) {
            goto L100;
        }

/*  SAVE THE REJECTED VECTOR IF INDICATED */

/*<             IF(RQ - ERRB .LE. BOUND(2,J+1)) GO TO 200 >*/
        if (rq - errb <= bound[((j + 1) << 1) + 2]) {
            goto L200;
        }
/*<             DO 60 I = J, NVAL >*/
        i__2 = nval;
        for (i__ = j; i__ <= i__2; ++i__) {
/*<                IF(BOUND(2,I+1) .GT. RQ) GO TO 70 >*/
            if (bound[((i__ + 1) << 1) + 2] > rq) {
                goto L70;
            }
/*<    60       CONTINUE >*/
/* L60: */
        }
/*<             GO TO 80 >*/
        goto L80;

/*<    70       CALL DCOPY(N, EIGVEC(1,J), 1, EIGVEC(1,I), 1) >*/
L70:
        dcopy_(n, &eigvec[j * eigvec_dim1 + 1], &c__1, &eigvec[i__ *
                eigvec_dim1 + 1], &c__1);

/*<    80       CALL DLARAN(N, EIGVEC(1,J)) >*/
L80:
        dlaran_(n, &eigvec[j * eigvec_dim1 + 1]);
/*<             GO TO 200 >*/
        goto L200;

/*  PERTURB RQ TOWARD THE MIDDLE */

/*<   100       IF(SIGMA .LT. RQ) SIGMA = DMAX1(SIGMA, RQ-ERRB) >*/
L100:
        if (sigma < rq) {
/* Computing MAX */
            d__1 = sigma, d__2 = rq - errb;
            sigma = max(d__1,d__2);
        }
/*<             IF(SIGMA .GT. RQ) SIGMA = DMIN1(SIGMA, RQ+ERRB) >*/
        if (sigma > rq) {
/* Computing MIN */
            d__1 = sigma, d__2 = rq + errb;
            sigma = min(d__1,d__2);
        }

/*  FACTOR AND SOLVE */

/*<   200       DO 210 I = J, NVAL >*/
L200:
        i__2 = nval;
        for (i__ = j; i__ <= i__2; ++i__) {
/*<                IF(SIGMA .LT. BOUND(1,I+1)) GO TO 220 >*/
            if (sigma < bound[((i__ + 1) << 1) + 1]) {
                goto L220;
            }
/*<   210       CONTINUE >*/
/* L210: */
        }
/*<             I = NVAL + 1 >*/
        i__ = nval + 1;
/*<   220       NUMVEC = I - J >*/
L220:
        numvec = i__ - j;
/*<             NUMVEC = MIN0(NUMVEC, NBAND + 2) >*/
/* Computing MIN */
        i__2 = numvec, i__3 = *nband + 2;
        numvec = min(i__2,i__3);
/*<             IF(RESID .LT. ARTOL) NUMVEC = MIN0(1,NUMVEC) >*/
        if (resid < *artol) {
            numvec = min(1,numvec);
        }
/*<             CALL DCOPY(N, EIGVEC(1,J), 1, VTEMP, 1) >*/
        dcopy_(n, &eigvec[j * eigvec_dim1 + 1], &c__1, &vtemp[1], &c__1);
/*<    >*/
        i__2 = (*nband << 1) - 1;
        dlabfc_(n, nband, &a[a_offset], &sigma, &numvec, lde, &eigvec[j *
                eigvec_dim1 + 1], &numl, &i__2, &atemp[1], &d__[1], atol);

/*  PARTIALLY SCALE EXTRA VECTORS TO PREVENT UNDERFLOW OR OVERFLOW */

/*<             IF(NUMVEC .EQ. 1) GO TO 227 >*/
        if (numvec == 1) {
            goto L227;
        }
/*<             L = NUMVEC - 1  >*/
        l = numvec - 1;
/*<             DO 225 I = 1,L >*/
        i__2 = l;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                M = J + I >*/
            m = j + i__;
/*<                CALL DSCAL(N, 1.0D0/VNORM, EIGVEC(1,M), 1) >*/
            d__1 = 1. / vnorm;
            dscal_(n, &d__1, &eigvec[m * eigvec_dim1 + 1], &c__1);
/*<   225       CONTINUE >*/
/* L225: */
        }

/*  UPDATE INTERVALS */

/*<   227       NUML = NUML - NL + 1 >*/
L227:
        numl = numl - *nl + 1;
/*<             IF(NUML .GE. 0) BOUND(2,1) = DMIN1(BOUND(2,1), SIGMA) >*/
        if (numl >= 0) {
            bound[4] = min(bound[4],sigma);
        }
/*<             DO 230 I = J, NVAL >*/
        i__2 = nval;
        for (i__ = j; i__ <= i__2; ++i__) {
/*<                IF(SIGMA .LT. BOUND(1,I+1)) GO TO 20 >*/
            if (sigma < bound[((i__ + 1) << 1) + 1]) {
                goto L20;
            }
/*<                IF(NUML .LT. I) BOUND(1,I+1) = SIGMA >*/
            if (numl < i__) {
                bound[((i__ + 1) << 1) + 1] = sigma;
            }
/*<                IF(NUML .GE. I) BOUND(2,I+1) = SIGMA >*/
            if (numl >= i__) {
                bound[((i__ + 1) << 1) + 2] = sigma;
            }
/*<   230       CONTINUE >*/
/* L230: */
        }
/*<    >*/
        if (numl < nval + 1) {
/* Computing MAX */
            d__1 = sigma, d__2 = bound[((nval + 2) << 1) + 1];
            bound[((nval + 2) << 1) + 1] = max(d__1,d__2);
        }
/*<             GO TO 20 >*/
        goto L20;

/*  ACCEPT AN EIGENPAIR */

/*<   300    CALL DLARAN(N, EIGVEC(1,J)) >*/
L300:
        dlaran_(n, &eigvec[j * eigvec_dim1 + 1]);
/*<          FLAG = .TRUE. >*/
        flag__ = TRUE_;
/*<          GO TO 310 >*/
        goto L310;

/*<   305    FLAG = .FALSE. >*/
L305:
        flag__ = FALSE_;
/*<          RQ = 0.5D0*(BOUND(1,J+1) + BOUND(2,J+1)) >*/
        rq = (bound[((j + 1) << 1) + 1] + bound[((j + 1) << 1) + 2]) * .5;
/*<    >*/
        i__2 = (*nband << 1) - 1;
        dlabfc_(n, nband, &a[a_offset], &rq, &numvec, lde, &eigvec[j *
                eigvec_dim1 + 1], &numl, &i__2, &atemp[1], &d__[1], atol);
/*<          VNORM = DNRM2(N, EIGVEC(1,J), 1) >*/
        vnorm = dnrm2_(n, &eigvec[j * eigvec_dim1 + 1], &c__1);
/*<          IF(VNORM .NE. 0.0) CALL DSCAL(N, 1.0D0/VNORM, EIGVEC(1,J), 1) >*/
        if (vnorm != (float)0.) {
            d__1 = 1. / vnorm;
            dscal_(n, &d__1, &eigvec[j * eigvec_dim1 + 1], &c__1);
        }

/*  ORTHOGONALIZE THE NEW EIGENVECTOR AGAINST THE OLD ONES */

/*<   310    EIGVAL(J) = RQ >*/
L310:
        eigval[j] = rq;
/*<          IF(J .EQ. 1) GO TO 330 >*/
        if (j == 1) {
            goto L330;
        }
/*<          M = J - 1 >*/
        m = j - 1;
/*<          DO 320 I = 1, M >*/
        i__2 = m;
        for (i__ = 1; i__ <= i__2; ++i__) {
/*<    >*/
            d__1 = -ddot_(n, &eigvec[i__ * eigvec_dim1 + 1], &c__1, &eigvec[j
                    * eigvec_dim1 + 1], &c__1);
            daxpy_(n, &d__1, &eigvec[i__ * eigvec_dim1 + 1], &c__1, &eigvec[j
                    * eigvec_dim1 + 1], &c__1);
/*<   320    CONTINUE >*/
/* L320: */
        }
/*<   330    VNORM = DNRM2(N, EIGVEC(1,J), 1) >*/
L330:
        vnorm = dnrm2_(n, &eigvec[j * eigvec_dim1 + 1], &c__1);
/*<          IF(VNORM .EQ. 0.0D0) GO TO 305 >*/
        if (vnorm == 0.) {
            goto L305;
        }
/*<          CALL DSCAL(N, 1.0D0/VNORM, EIGVEC(1,J), 1) >*/
        d__1 = 1. / vnorm;
        dscal_(n, &d__1, &eigvec[j * eigvec_dim1 + 1], &c__1);

/*   ORTHOGONALIZE LATER VECTORS AGAINST THE CONVERGED ONE */

/*<          IF(FLAG) GO TO 305 >*/
        if (flag__) {
            goto L305;
        }
/*<          IF(J .EQ. NVAL) RETURN >*/
        if (j == nval) {
            return 0;
        }
/*<          M = J + 1 >*/
        m = j + 1;
/*<          DO 340 I = M, NVAL >*/
        i__2 = nval;
        for (i__ = m; i__ <= i__2; ++i__) {
/*<    >*/
            d__1 = -ddot_(n, &eigvec[j * eigvec_dim1 + 1], &c__1, &eigvec[i__
                    * eigvec_dim1 + 1], &c__1);
            daxpy_(n, &d__1, &eigvec[j * eigvec_dim1 + 1], &c__1, &eigvec[i__
                    * eigvec_dim1 + 1], &c__1);
/*<   340    CONTINUE >*/
/* L340: */
        }
/*<   400 CONTINUE >*/
/* L400: */
    }
/*<       RETURN >*/
    return 0;

/*<   500 CONTINUE >*/
/* L500: */
/*<       END >*/
    return 0;
} /* dlabcm_ */

#ifdef __cplusplus
        }
#endif
