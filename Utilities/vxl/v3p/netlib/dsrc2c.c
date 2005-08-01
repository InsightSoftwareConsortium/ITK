#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, Oct 2003: manual optimisation and clean-up */

extern double log(double), sqrt(double); /* #include <math.h> */
extern long time(long *timer);           /* #include <time.h> */

extern doublereal cheby_(doublereal *, doublereal *, doublereal *, integer *, doublereal *, doublereal *);
extern doublereal determ_(integer *, doublereal *, doublereal *);
extern doublereal eigvns_(integer *, doublereal *, doublereal *, doublereal *, integer *);
extern doublereal eigvss_(integer *, doublereal *, doublereal *, doublereal *, integer *, integer *);
extern doublereal itpackddot_(integer *, doublereal *, integer *, doublereal *, integer *);
extern doublereal pbeta_(integer *, integer *, integer *, doublereal *, doublereal *, doublereal *, doublereal *);
extern doublereal pvtbv_(integer *, integer *, integer *, doublereal *, doublereal *);
extern doublereal tau_(integer *);
extern doublereal timer_(real *);
extern integer bisrch_(integer *, integer *, integer *);
extern integer ipstr_(doublereal *);
extern logical chgsme_(doublereal *, integer *);
extern logical omgchg_(integer *);
extern logical omgstr_(integer *);
extern logical tstchg_(integer *);

/***** BEGIN VXL ADDITIONS ****/

/* Turn off warnings in f2c generated code */
#if defined(_MSC_VER)
# if defined(__ICL)
#  pragma warning(disable: 239 264 1011 )
# else
#  pragma warning(disable: 4101 4244 4554 4756 4723)
# endif
#endif

/***** END VXL ADDITIONS ****/

/* Common Block Declarations */

static struct {
    integer in;
    integer is;
    integer isym;
    integer itmax;
    integer level;
    integer nout;
} itcom1_;

#define itcom1_1 itcom1_

static struct {
    logical adapt;
    logical betadt;
    logical caseii;
    logical halt;
    logical partad;
} itcom2_;

#define itcom2_1 itcom2_

static struct {
    doublereal bdelnm;
    doublereal betab;
    doublereal cme;
    doublereal delnnm;
    doublereal delsnm;
    doublereal ff;
    doublereal gamma;
    doublereal omega;
    doublereal qa;
    doublereal qt;
    doublereal rho;
    doublereal rrr;
    doublereal sige;
    doublereal sme;
    doublereal specr;
    doublereal spr;
    doublereal drelpr;
    doublereal stptst;
    doublereal udnm;
    doublereal zeta;
} itcom3_;

#define itcom3_1 itcom3_

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;
static doublereal c_b21 = 0.;
static integer c__2 = 2;
static integer c__3 = 3;
static integer c__4 = 4;
static doublereal c_b286 = 1.;
static integer c__5 = 5;
static integer c__6 = 6;
static integer c__7 = 7;

/* Subroutine */
int jcg_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u,
         integer *iwksp, integer *nw, doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr)
{
    /* Local variables */
    static integer n3, nb, ib1, ib2, ib3, ib4, ib5, ier;
    static doublereal tol;
    static doublereal temp;
    static integer loop;
    static doublereal time1, time2;
    static real timi1, timj1, timi2, timj2;
    static integer idgts;
    static doublereal digit1, digit2;
    static integer itmax1;
    static integer ierper;

/*     ITPACK 2C MAIN SUBROUTINE  JCG  (JACOBI CONJUGATE GRADIENT) */
/*     EACH OF THE MAIN SUBROUTINES:                               */
/*           JCG, JSI, SOR, SSORCG, SSORSI, RSCG, RSSI             */
/*     CAN BE USED INDEPENDENTLY OF THE OTHERS                     */

/*          THIS SUBROUTINE, JCG, DRIVES THE JACOBI CONJUGATE            */
/*          GRADIENT ALGORITHM.                                          */
/*                                                                       */
/* ... PARAMETER LIST:                                                   */
/*                                                                       */
/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX.              */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF     */
/*                 THE SPARSE MATRIX REPRESENTATION.                     */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE      */
/*                 MATRIX REPRESENTATION.                                */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE      */
/*                 OF THE MATRIX PROBLEM.                                */
/*          U      INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U CONTAINS THE   */
/*                 INITIAL GUESS TO THE SOLUTION. ON OUTPUT, IT CONTAINS */
/*                 THE LATEST ESTIMATE TO THE SOLUTION.                  */
/*          IWKSP  INTEGER VECTOR WORKSPACE OF LENGTH 3*N                */
/*          NW     INPUT INTEGER.  LENGTH OF AVAILABLE WKSP.  ON OUTPUT, */
/*                 IPARM(8) IS AMOUNT USED.                              */
/*          WKSP   D.P. VECTOR USED FOR WORKING SPACE.  JACOBI CONJUGATE */
/*                 GRADIENT NEEDS THIS TO BE IN LENGTH AT LEAST          */
/*                 4*N + 2*ITMAX,  IF ISYM = 0  (SYMMETRIC STORAGE)      */
/*                 4*N + 4*ITMAX,  IF ISYM = 1  (NONSYMMETRIC STORAGE)   */
/*                 HERE ITMAX = IPARM(1) AND ISYM = IPARM(5)             */
/*                 (ITMAX IS THE MAXIMUM ALLOWABLE NUMBER OF ITERATIONS) */
/*          IPARM  INTEGER VECTOR OF LENGTH 12.  ALLOWS USER TO SPECIFY  */
/*                 SOME INTEGER PARAMETERS WHICH AFFECT THE METHOD.      */
/*          RPARM  D.P. VECTOR OF LENGTH 12. ALLOWS USER TO SPECIFY SOME */
/*                 D.P. PARAMETERS WHICH AFFECT THE METHOD.              */
/*          IER    OUTPUT INTEGER.  ERROR FLAG. (= IERR)                 */
/*                                                                       */
/* ... JCG SUBPROGRAM REFERENCES:                                        */
/*                                                                       */
/*          FROM ITPACK    BISRCH, CHGCON, DETERM, DFAULT, ECHALL,       */
/*                         ECHOUT, EIGVNS, EIGVSS, EQRT1S, ITERM, TIMER, */
/*                         ITJCG, IVFILL, PARCON, PERMAT,                */
/*                         PERROR, PERVEC, PJAC, PMULT, PRBNDX,          */
/*                         PSTOP, QSORT, DAXPY, SBELM, SCAL, DCOPY,      */
/*                         DDOT, SUM3, UNSCAL, VEVMW, VFILL, VOUT,       */
/*                         WEVMW, ZBRENT                                 */
/*          SYSTEM         DABS, DLOG10, DBLE(AMAX0), DMAX1, MOD, DSQRT  */
/*                                                                       */
/*     VERSION:  ITPACK 2C (MARCH 1982)                                  */
/*                                                                       */
/*     CODE WRITTEN BY:  DAVID KINCAID, ROGER GRIMES, JOHN RESPESS       */
/*                       CENTER FOR NUMERICAL ANALYSIS                   */
/*                       UNIVERSITY OF TEXAS                             */
/*                       AUSTIN, TX  78712                               */
/*                       (512) 471-1242                                  */
/*                                                                       */
/*     FOR ADDITIONAL DETAILS ON THE                                     */
/*          (A) SUBROUTINE SEE TOMS ARTICLE 1982                         */
/*          (B) ALGORITHM  SEE CNA REPORT 150                            */
/*                                                                       */
/*     BASED ON THEORY BY:  DAVID YOUNG, DAVID KINCAID, LOU HAGEMAN      */
/*                                                                       */
/*     REFERENCE THE BOOK:  APPLIED ITERATIVE METHODS                    */
/*                          L. HAGEMAN, D. YOUNG                         */
/*                          ACADEMIC PRESS, 1981                         */
/*                                                                       */
/*     **************************************************                */
/*     *               IMPORTANT NOTE                   *                */
/*     *                                                *                */
/*     *      WHEN INSTALLING ITPACK ROUTINES ON A      *                */
/*     *  DIFFERENT COMPUTER, RESET SOME OF THE VALUES  *                */
/*     *  IN  SUBROUTNE DFAULT.   MOST IMPORTANT ARE    *                */
/*     *                                                *                */
/*     *   DRELPR      MACHINE RELATIVE PRECISION       *                */
/*     *   RPARM(1)    STOPPING CRITERION               *                */
/*     *                                                *                */
/*     *   ALSO CHANGE SYSTEM-DEPENDENT ROUTINE         *                */
/*     *   SECOND USED IN TIMER                         *                */
/*     *                                                *                */
/*     **************************************************                */
/*                                                                       */
/* ... VARIABLES IN COMMON BLOCK - ITCOM1                                */
/*                                                                       */
/*     IN     - ITERATION NUMBER                                         */
/*     IS     - ITERATION NUMBER WHEN PARAMETERS LAST CHANGED            */
/*     ISYM   - SYMMETRIC/NONSYMMETRIC STORAGE FORMAT SWITCH             */
/*     ITMAX  - MAXIMUM NUMBER OF ITERATIONS ALLOWED                     */
/*     LEVEL  - LEVEL OF OUTPUT CONTROL SWITCH                           */
/*     NOUT   - OUTPUT UNIT NUMBER                                       */
/*                                                                       */
/* ... VARIABLES IN COMMON BLOCK - ITCOM2                                */
/*                                                                       */
/*     ADAPT  - FULLY ADAPTIVE PROCEDURE SWITCH                          */
/*     BETADT - SWITCH FOR ADAPTIVE DETERMINATION OF BETA                */
/*     CASEII - ADAPTIVE PROCEDURE CASE SWITCH                           */
/*     HALT   - STOPPING TEST SWITCH                                     */
/*     PARTAD - PARTIALLY ADAPTIVE PROCEDURE SWITCH                      */
/*                                                                       */
/* ... VARIABLES IN COMMON BLOCK - ITCOM3                                */
/*                                                                       */
/*     BDELNM - TWO NORM OF B TIMES DELTA-SUPER-N                        */
/*     BETAB  - ESTIMATE FOR THE SPECTRAL RADIUS OF LU MATRIX            */
/*     CME    - ESTIMATE OF LARGEST EIGENVALUE                           */
/*     DELNNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION N          */
/*     DELSNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION S          */
/*     FF     - ADAPTIVE PROCEDURE DAMPING FACTOR                        */
/*     GAMMA  - ACCELERATION PARAMETER                                   */
/*     OMEGA  - OVERRELAXATION PARAMETER FOR SOR AND SSOR                */
/*     QA     - PSEUDO-RESIDUAL RATIO                                    */
/*     QT     - VIRTUAL SPECTRAL RADIUS                                  */
/*     RHO    - ACCELERATION PARAMETER                                   */
/*     RRR    - ADAPTIVE PARAMETER                                       */
/*     SIGE   - PARAMETER SIGMA-SUB-E                                    */
/*     SME    - ESTIMATE OF SMALLEST EIGENVALUE                          */
/*     SPECR  - SPECTRAL RADIUS ESTIMATE FOR SSOR                        */
/*     DRELPR - MACHINE RELATIVE PRECISION                               */
/*     STPTST - STOPPING PARAMETER                                       */
/*     UDNM   - TWO NORM OF U                                            */
/*     ZETA   - STOPPING CRITERION                                       */

    itcom1_1.level = iparm[1];
    itcom1_1.nout = iparm[3];
    ier = 0;
    if (iparm[0] <= 0)
        return 0;

    if (iparm[10] == 0)
        timj1 = timer_((real*)0);

    if (itcom1_1.level < 3)
        echout_(iparm, rparm, &c__1);
    else
        echall_(n, ia, ja, a, rhs, iparm, rparm, &c__1);
    temp = itcom3_1.drelpr * 500.;
    if (itcom3_1.zeta < temp)
        itcom3_1.zeta = temp;

    time1 = rparm[8];
    time2 = rparm[9];
    digit1 = rparm[10];
    digit2 = rparm[11];

    /* ... VERIFY N */

    if (*n <= 0) {
        ier = 11;
        goto L370;
    }

    /* ... REMOVE ROWS AND COLUMNS IF REQUESTED */

    if (iparm[9] != 0) {
        tol = rparm[7];
        ivfill_(n, iwksp, &c__0);
        vfill_(n, wksp, &c_b21);
        sbelm_(n, ia, ja, a, rhs, iwksp, wksp, &tol, &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
        if (ier != 0)
            goto L370;
    }

    /* ... INITIALIZE WKSP BASE ADDRESSES. */

    ib1 = 0;
    ib2 = ib1 + *n;
    ib3 = ib2 + *n;
    ib4 = ib3 + *n;
    ib5 = ib4 + *n;
    iparm[7] = (*n << 2) + (itcom1_1.itmax << 1);
    if (itcom1_1.isym != 0)
        iparm[7] += itcom1_1.itmax << 1;

    if (*nw < iparm[7]) {
        ier = 12;
        goto L370;
    }

    /* ... PERMUTE TO  RED-BLACK SYSTEM IF REQUESTED */

    nb = iparm[8];
    if (nb < 0)
        goto L170;

    n3 = *n * 3;
    ivfill_(&n3, iwksp, &c__0);
    prbndx_(n, &nb, ia, ja, iwksp, &iwksp[ib2], &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L370;

    /* ... PERMUTE MATRIX AND RHS */

    permat_(n, ia, ja, a, iwksp, &iwksp[ib3], &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L370;

    pervec_(n, rhs, iwksp);
    pervec_(n, u, iwksp);

    /* ... SCALE LINEAR SYSTEM, U, AND RHS BY THE SQUARE ROOT OF THE DIAGONAL ELEMENTS. */

L170:
    vfill_(&iparm[7], wksp, &c_b21);
    scal_(n, ia, ja, a, rhs, u, wksp, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L370;

    if (iparm[10] == 0)
        timi1 = timer_((real*)0);

    /* ... COMPUTE INITIAL PSEUDO-RESIDUAL */

    itpackdcopy_(n, rhs, &c__1, &wksp[ib2], &c__1);
    pjac_(n, ia, ja, a, u, &wksp[ib2]);
    vevmw_(n, &wksp[ib2], u);

    /* ... ITERATION SEQUENCE */

    itmax1 = itcom1_1.itmax + 1;
    for (loop = 1; loop <= itmax1; ++loop) {
        itcom1_1.in = loop - 1;
        if (itcom1_1.in % 2 == 1)
            goto L240;

        /* ... CODE FOR THE EVEN ITERATIONS. */

        /*     U           = U(IN)             WKSP(IB2) = DEL(IN) */
        /*     WKSP(IB1)   = U(IN-1)           WKSP(IB3) = DEL(IN-1) */

        itjcg_(n, ia, ja, a, u, &wksp[ib1], &wksp[ib2], &wksp[ib3], &wksp[ib4], &wksp[ib5]);

        if (itcom2_1.halt)
            goto L280;

        continue;

        /* ... CODE FOR THE ODD ITERATIONS. */

        /*     U           = U(IN-1)           WKSP(IB2) = DEL(IN-1) */
        /*     WKSP(IB1)   = U(IN)             WKSP(IB3) = DEL(IN) */

L240:
        itjcg_(n, ia, ja, a, &wksp[ib1], u, &wksp[ib3], &wksp[ib2], &wksp[ib4], &wksp[ib5]);

        if (itcom2_1.halt)
            goto L280;
    }

    /* ... ITMAX HAS BEEN REACHED */

    if (iparm[10] == 0) {
        timi2 = timer_((real*)0);
        time1 = (doublereal) (timi2 - timi1);
    }
    ier = 13;
    if (iparm[2] == 0)
        rparm[0] = itcom3_1.stptst;

    goto L310;

    /* ... METHOD HAS CONVERGED */

L280:
    if (iparm[10] == 0) {
        timi2 = timer_((real*)0);
        time1 = (doublereal) (timi2 - timi1);
    }

    /* ... PUT SOLUTION INTO U IF NOT ALREADY THERE. */

L310:
    if (itcom1_1.in % 2 == 1)
        itpackdcopy_(n, &wksp[ib1], &c__1, u, &c__1);

    /* ... UNSCALE THE MATRIX, SOLUTION, AND RHS VECTORS. */

    unscal_(n, ia, ja, a, rhs, u, wksp);

    /* ... UN-PERMUTE MATRIX,RHS, AND SOLUTION */

    if (iparm[8] < 0)
        goto L340;

    permat_(n, ia, ja, a, &iwksp[ib2], &iwksp[ib3], &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ierper);
    if (ierper != 0) {
        if (ier == 0)
            ier = ierper;

        goto L370;
    }

    pervec_(n, rhs, &iwksp[ib2]);
    pervec_(n, u, &iwksp[ib2]);

    /* ... OPTIONAL ERROR ANALYSIS */

L340:
    idgts = iparm[11];
    if (idgts >= 0) {
        if (iparm[1] <= 0)
            idgts = 0;

        perror_(n, ia, ja, a, rhs, u, wksp, &digit1, &digit2, &idgts);
    }

    /* ... SET RETURN PARAMETERS IN IPARM AND RPARM */

    iparm[7] -= (itcom1_1.itmax - itcom1_1.in) << 1;
    if (iparm[10] == 0) {
        timj2 = timer_((real*)0);
        time2 = (doublereal) (timj2 - timj1);
    }
    if (itcom1_1.isym != 0)
        iparm[7] -= (itcom1_1.itmax - itcom1_1.in) << 1;

    if (iparm[2] == 0) {
        iparm[0] = itcom1_1.in;
        iparm[8] = nb;
        rparm[1] = itcom3_1.cme;
        rparm[2] = itcom3_1.sme;
        rparm[8] = time1;
        rparm[9] = time2;
        rparm[10] = digit1;
        rparm[11] = digit2;
    }

L370:
    *ierr = ier;
    if (itcom1_1.level >= 3)
        echall_(n, ia, ja, a, rhs, iparm, rparm, &c__2);

    return 0;
} /* jcg_ */

/* Subroutine */
int jsi_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u,
         integer *iwksp, integer *nw, doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr)
{
    /* Local variables */
    static integer n3, nb, ib1, ib2, ib3, ier;
    static doublereal tol;
    static integer icnt;
    static doublereal temp;
    static integer loop;
    static doublereal time1, time2;
    static real timi1, timj1, timi2, timj2;
    static integer idgts;
    static doublereal digit1, digit2;
    static integer itmax1;
    static integer ierper;

/*     ITPACK 2C MAIN SUBROUTINE  JSI  (JACOBI SEMI-ITERATIVE)    */
/*     EACH OF THE MAIN SUBROUTINES:                              */
/*           JCG, JSI, SOR, SSORCG, SSORSI, RSCG, RSSI            */
/*     CAN BE USED INDEPENDENTLY OF THE OTHERS                    */

/*          THIS SUBROUTINE, JSI, DRIVES THE JACOBI SEMI-                */
/*          ITERATION ALGORITHM.                                         */
/*                                                                       */
/* ... PARAMETER LIST:                                                   */
/*                                                                       */
/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX.              */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF     */
/*                 THE SPARSE MATRIX REPRESENTATION.                     */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE      */
/*                 MATRIX REPRESENTATION.                                */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE      */
/*                 OF THE MATRIX PROBLEM.                                */
/*          U      INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U CONTAINS THE   */
/*                 INITIAL GUESS TO THE SOLUTION. ON OUTPUT, IT CONTAINS */
/*                 THE LATEST ESTIMATE TO THE SOLUTION.                  */
/*          IWKSP  INTEGER VECTOR WORKSPACE OF LENGTH 3*N                */
/*          NW     INPUT INTEGER.  LENGTH OF AVAILABLE WKSP.  ON OUTPUT, */
/*                 IPARM(8) IS AMOUNT USED.                              */
/*          WKSP   D.P. VECTOR USED FOR WORKING SPACE.  JACOBI SI        */
/*                 NEEDS THIS TO BE IN LENGTH AT LEAST                   */
/*                 2*N                                                   */
/*          IPARM  INTEGER VECTOR OF LENGTH 12.  ALLOWS USER TO SPECIFY  */
/*                 SOME INTEGER PARAMETERS WHICH AFFECT THE METHOD.      */
/*          RPARM  D.P. VECTOR OF LENGTH 12. ALLOWS USER TO SPECIFY SOME */
/*                 D.P. PARAMETERS WHICH AFFECT THE METHOD.              */
/*          IER    OUTPUT INTEGER.  ERROR FLAG. (= IERR)                 */
/*                                                                       */
/* ... JSI SUBPROGRAM REFERENCES:                                        */
/*                                                                       */
/*          FROM ITPACK   BISRCH, CHEBY, CHGSI, CHGSME, DFAULT, ECHALL,  */
/*                        ECHOUT, ITERM, TIMER, ITJSI, IVFILL, PAR       */
/*                        PERMAT, PERROR, PERVEC, PJAC, PMULT, PRBNDX,   */
/*                        PSTOP, PVTBV, QSORT, DAXPY, SBELM, SCAL,       */
/*                        DCOPY, DDOT, SUM3, TSTCHG, UNSCAL, VEVMW,      */
/*                        VFILL, VOUT, WEVMW                             */
/*          SYSTEM        DABS, DLOG10, DBLE(AMAX0), DMAX1, DBLE(FLOAT), */
/*                        MOD,DSQRT                                      */
/*                                                                       */
/*     VERSION:  ITPACK 2C (MARCH 1982)                                  */
/*                                                                       */
/*     CODE WRITTEN BY:  DAVID KINCAID, ROGER GRIMES, JOHN RESPESS       */
/*                       CENTER FOR NUMERICAL ANALYSIS                   */
/*                       UNIVERSITY OF TEXAS                             */
/*                       AUSTIN, TX  78712                               */
/*                       (512) 471-1242                                  */
/*                                                                       */
/*     FOR ADDITIONAL DETAILS ON THE                                     */
/*          (A) SUBROUTINE SEE TOMS ARTICLE 1982                         */
/*          (B) ALGORITHM  SEE CNA REPORT 150                            */
/*                                                                       */
/*     BASED ON THEORY BY:  DAVID YOUNG, DAVID KINCAID, LOU HAGEMAN      */
/*                                                                       */
/*     REFERENCE THE BOOK:  APPLIED ITERATIVE METHODS                    */
/*                          L. HAGEMAN, D. YOUNG                         */
/*                          ACADEMIC PRESS, 1981                         */
/*                                                                       */
/*     **************************************************                */
/*     *               IMPORTANT NOTE                   *                */
/*     *                                                *                */
/*     *      WHEN INSTALLING ITPACK ROUTINES ON A      *                */
/*     *  DIFFERENT COMPUTER, RESET SOME OF THE VALUES  *                */
/*     *  IN  SUBROUTNE DFAULT.   MOST IMPORTANT ARE    *                */
/*     *                                                *                */
/*     *   DRELPR      MACHINE RELATIVE PRECISION       *                */
/*     *   RPARM(1)    STOPPING CRITERION               *                */
/*     *                                                *                */
/*     *   ALSO CHANGE SYSTEM-DEPENDENT ROUTINE         *                */
/*     *   SECOND USED IN TIMER                         *                */
/*     *                                                *                */
/*     **************************************************                */
/*                                                                       */
/*     SPECIFICATIONS FOR ARGUMENTS                                      */
/*                                                                       */
/*     SPECIFICATIONS FOR LOCAL VARIABLES                                */
/*                                                                       */
/* ... VARIABLES IN COMMON BLOCK - ITCOM1                                */
/*                                                                       */
/*     IN     - ITERATION NUMBER                                         */
/*     IS     - ITERATION NUMBER WHEN PARAMETERS LAST CHANGED            */
/*     ISYM   - SYMMETRIC/NONSYMMETRIC STORAGE FORMAT SWITCH             */
/*     ITMAX  - MAXIMUM NUMBER OF ITERATIONS ALLOWED                     */
/*     LEVEL  - LEVEL OF OUTPUT CONTROL SWITCH                           */
/*     NOUT   - OUTPUT UNIT NUMBER                                       */
/*                                                                       */
/* ... VARIABLES IN COMMON BLOCK - ITCOM2                                */
/*                                                                       */
/*     ADAPT  - FULLY ADAPTIVE PROCEDURE SWITCH                          */
/*     BETADT - SWITCH FOR ADAPTIVE DETERMINATION OF BETA                */
/*     CASEII - ADAPTIVE PROCEDURE CASE SWITCH                           */
/*     HALT   - STOPPING TEST SWITCH                                     */
/*     PARTAD - PARTIALLY ADAPTIVE PROCEDURE SWITCH                      */
/*                                                                       */
/* ... VARIABLES IN COMMON BLOCK - ITCOM3                                */
/*                                                                       */
/*     BDELNM - TWO NORM OF B TIMES DELTA-SUPER-N                        */
/*     BETAB  - ESTIMATE FOR THE SPECTRAL RADIUS OF LU MATRIX            */
/*     CME    - ESTIMATE OF LARGEST EIGENVALUE                           */
/*     DELNNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION N          */
/*     DELSNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION S          */
/*     FF     - ADAPTIVE PROCEDURE DAMPING FACTOR                        */
/*     GAMMA  - ACCELERATION PARAMETER                                   */
/*     OMEGA  - OVERRELAXATION PARAMETER FOR SOR AND SSOR                */
/*     QA     - PSEUDO-RESIDUAL RATIO                                    */
/*     QT     - VIRTUAL SPECTRAL RADIUS                                  */
/*     RHO    - ACCELERATION PARAMETER                                   */
/*     RRR    - ADAPTIVE PARAMETER                                       */
/*     SIGE   - PARAMETER SIGMA-SUB-E                                    */
/*     SME    - ESTIMATE OF SMALLEST EIGENVALUE                          */
/*     SPECR  - SPECTRAL RADIUS ESTIMATE FOR SSOR                        */
/*     DRELPR - MACHINE RELATIVE PRECISION                               */
/*     STPTST - STOPPING PARAMETER                                       */
/*     UDNM   - TWO NORM OF U                                            */
/*     ZETA   - STOPPING CRITERION                                       */

    itcom1_1.level = iparm[1];
    itcom1_1.nout = iparm[3];
    ier = 0;
    if (iparm[0] <= 0)
        return 0;

    if (iparm[10] == 0)
        timj1 = timer_((real*)0);

    if (itcom1_1.level < 3)
        echout_(iparm, rparm, &c__2);
    else
        echall_(n, ia, ja, a, rhs, iparm, rparm, &c__1);
    temp = itcom3_1.drelpr * 500.;
    if (itcom3_1.zeta < temp)
        itcom3_1.zeta = temp;

    time1 = rparm[8];
    time2 = rparm[9];
    digit1 = rparm[10];
    digit2 = rparm[11];

    /* ... VERIFY N */

    if (*n <= 0) {
        ier = 21;
        goto L360;
    }

    /* ... REMOVE ROWS AND COLUMNS IF REQUESTED */

    if (iparm[9] != 0) {
        tol = rparm[7];
        ivfill_(n, iwksp, &c__0);
        vfill_(n, wksp, &c_b21);
        sbelm_(n, ia, ja, a, rhs, iwksp, wksp, &tol, &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
        if (ier != 0)
            goto L360;
    }

    /* ... INITIALIZE WKSP BASE ADDRESSES. */

    ib1 = 0;
    ib2 = ib1 + *n;
    ib3 = ib2 + *n;
    iparm[7] = *n << 1;
    if (*nw < iparm[7]) {
        ier = 22;
        goto L360;
    }

    /* ... PERMUTE TO  RED-BLACK SYSTEM IF REQUESTED */

    nb = iparm[8];
    if (nb < 0)
        goto L170;

    n3 = *n * 3;
    ivfill_(&n3, iwksp, &c__0);
    prbndx_(n, &nb, ia, ja, iwksp, &iwksp[ib2], &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L360;

    /* ... PERMUTE MATRIX AND RHS */

    permat_(n, ia, ja, a, iwksp, &iwksp[ib3], &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L360;

    pervec_(n, rhs, iwksp);
    pervec_(n, u, iwksp);

    /* ... SCALE LINEAR SYSTEM, U, AND RHS BY THE SQUARE ROOT OF THE DIAGONAL ELEMENTS. */

L170:
    vfill_(&iparm[7], wksp, &c_b21);
    scal_(n, ia, ja, a, rhs, u, wksp, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L360;

    if (iparm[10] == 0)
        timi1 = timer_((real*)0);

    /* ... ITERATION SEQUENCE */

    itmax1 = itcom1_1.itmax + 1;
    for (loop = 1; loop <= itmax1; ++loop) {
        itcom1_1.in = loop - 1;
        if (itcom1_1.in % 2 == 1)
            goto L230;

    /* ... CODE FOR THE EVEN ITERATIONS. */

    /*     U           = U(IN) */
    /*     WKSP(IB1)   = U(IN-1) */

        itjsi_(n, ia, ja, a, rhs, u, &wksp[ib1], &wksp[ib2], &icnt);

        if (itcom2_1.halt)
            goto L270;

        continue;

    /* ... CODE FOR THE ODD ITERATIONS. */

    /*     U           = U(IN-1) */
    /*     WKSP(IB1)   = U(IN) */

L230:
        itjsi_(n, ia, ja, a, rhs, &wksp[ib1], u, &wksp[ib2], &icnt);

        if (itcom2_1.halt)
            goto L270;
    }

    /* ... ITMAX HAS BEEN REACHED */

    if (iparm[10] == 0) {
        timi2 = timer_((real*)0);
        time1 = (doublereal) (timi2 - timi1);
    }
    ier = 23;
    if (iparm[2] == 0)
        rparm[0] = itcom3_1.stptst;

    goto L300;

    /* ... METHOD HAS CONVERGED */

L270:
    if (iparm[10] == 0) {
        timi2 = timer_((real*)0);
        time1 = (doublereal) (timi2 - timi1);
    }

    /* ... PUT SOLUTION INTO U IF NOT ALREADY THERE. */

L300:
    if (itcom1_1.in % 2 == 1)
        itpackdcopy_(n, &wksp[ib1], &c__1, u, &c__1);

    /* ... UNSCALE THE MATRIX, SOLUTION, AND RHS VECTORS. */

    unscal_(n, ia, ja, a, rhs, u, wksp);

    /* ... UN-PERMUTE MATRIX,RHS, AND SOLUTION */

    if (iparm[8] < 0)
        goto L330;

    permat_(n, ia, ja, a, &iwksp[ib2], &iwksp[ib3], &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ierper);
    if (ierper != 0) {
        if (ier == 0)
            ier = ierper;

        goto L360;
    }

    pervec_(n, rhs, &iwksp[ib2]);
    pervec_(n, u, &iwksp[ib2]);

    /* ... OPTIONAL ERROR ANALYSIS */

L330:
    idgts = iparm[11];
    if (idgts >= 0) {
        if (iparm[1] <= 0)
            idgts = 0;

        perror_(n, ia, ja, a, rhs, u, wksp, &digit1, &digit2, &idgts);
    }

    /* ... SET RETURN PARAMETERS IN IPARM AND RPARM */

    if (iparm[10] == 0) {
        timj2 = timer_((real*)0);
        time2 = (doublereal) (timj2 - timj1);
    }
    if (iparm[2] == 0) {
        iparm[0] = itcom1_1.in;
        iparm[8] = nb;
        rparm[1] = itcom3_1.cme;
        rparm[2] = itcom3_1.sme;
        rparm[8] = time1;
        rparm[9] = time2;
        rparm[10] = digit1;
        rparm[11] = digit2;
    }

L360:
    *ierr = ier;
    if (itcom1_1.level >= 3)
        echall_(n, ia, ja, a, rhs, iparm, rparm, &c__2);

    return 0;
} /* jsi_ */

/* Subroutine */
int sor_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u,
         integer *iwksp, integer *nw, doublereal *wksp, integer *iparm, doublereal *rparm, integer* ierr)
{
    /* Local variables */
    static integer n3, nb, ib1, ib2, ib3, ier;
    static doublereal tol;
    static doublereal temp;
    static integer loop;
    static doublereal time1, time2;
    static real timi1, timj1, timi2, timj2;
    static integer idgts;
    static doublereal digit1, digit2;
    static integer itmax1;
    static integer ierper;

/*     ITPACK 2C MAIN SUBROUTINE  SOR  (SUCCESSIVE OVERRELATION) */
/*     EACH OF THE MAIN SUBROUTINES:                             */
/*           JCG, JSI, SOR, SSORCG, SSORSI, RSCG, RSSI           */
/*     CAN BE USED INDEPENDENTLY OF THE OTHERS                   */

/*          THIS SUBROUTINE, SOR, DRIVES THE  SUCCESSIVE                 */
/*          OVERRELAXATION ALGORITHM.                                    */
/*                                                                       */
/* ... PARAMETER LIST:                                                   */
/*                                                                       */
/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX.              */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF     */
/*                 THE SPARSE MATRIX REPRESENTATION.                     */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE      */
/*                 MATRIX REPRESENTATION                                 */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE      */
/*                 OF THE MATRIX PROBLEM.                                */
/*          U      INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U CONTAINS THE   */
/*                 INITIAL GUESS TO THE SOLUTION. ON OUTPUT, IT CONTAINS */
/*                 THE LATEST ESTIMATE TO THE SOLUTION.                  */
/*          IWKSP  INTEGER VECTOR WORKSPACE OF LENGTH 3*N                */
/*          NW     INPUT INTEGER.  LENGTH OF AVAILABLE WKSP.  ON OUTPUT, */
/*                 IPARM(8) IS AMOUNT USED.                              */
/*          WKSP   D.P. VECTOR USED FOR WORKING SPACE.  SOR NEEDS THIS   */
/*                 TO BE IN LENGTH AT LEAST  N                           */
/*          IPARM  INTEGER VECTOR OF LENGTH 12.  ALLOWS USER TO SPECIFY  */
/*                 SOME INTEGER PARAMETERS WHICH AFFECT THE METHOD.      */
/*          RPARM  D.P. VECTOR OF LENGTH 12. ALLOWS USER TO SPECIFY SOME */
/*                 D.P. PARAMETERS WHICH AFFECT THE METHOD.              */
/*          IER    OUTPUT INTEGER.  ERROR FLAG. (= IERR)                 */
/*                                                                       */
/* ... SOR SUBPROGRAM REFERENCES:                                        */
/*                                                                       */
/*          FROM ITPACK   BISRCH, DFAULT, ECHALL, ECHOUT, IPSTR, ITERM,  */
/*                        TIMER, ITSOR, IVFILL, PERMAT, PERROR,          */
/*                        PERVEC, PFSOR1, PMULT, PRBNDX, PSTOP, QSORT,   */
/*                        SBELM, SCAL, DCOPY, DDOT, TAU, UNSCAL, VFILL,  */
/*                        VOUT, WEVMW                                    */
/*          SYSTEM        DABS, DLOG10, DBLE(AMAX0), DMAX1, DBLE(FLOAT), */
/*                        DSQRT                                          */
/*                                                                       */
/*     VERSION:  ITPACK 2C (MARCH 1982)                                  */
/*                                                                       */
/*     CODE WRITTEN BY:  DAVID KINCAID, ROGER GRIMES, JOHN RESPESS       */
/*                       CENTER FOR NUMERICAL ANALYSIS                   */
/*                       UNIVERSITY OF TEXAS                             */
/*                       AUSTIN, TX  78712                               */
/*                       (512) 471-1242                                  */
/*                                                                       */
/*     FOR ADDITIONAL DETAILS ON THE                                     */
/*          (A) SUBROUTINE SEE TOMS ARTICLE 1982                         */
/*          (B) ALGORITHM  SEE CNA REPORT 150                            */
/*                                                                       */
/*     BASED ON THEORY BY:  DAVID YOUNG, DAVID KINCAID, LOU HAGEMAN      */
/*                                                                       */
/*     REFERENCE THE BOOK:  APPLIED ITERATIVE METHODS                    */
/*                          L. HAGEMAN, D. YOUNG                         */
/*                          ACADEMIC PRESS, 1981                         */
/*                                                                       */
/*     **************************************************                */
/*     *               IMPORTANT NOTE                   *                */
/*     *                                                *                */
/*     *      WHEN INSTALLING ITPACK ROUTINES ON A      *                */
/*     *  DIFFERENT COMPUTER, RESET SOME OF THE VALUES  *                */
/*     *  IN  SUBROUTNE DFAULT.   MOST IMPORTANT ARE    *                */
/*     *                                                *                */
/*     *   DRELPR      MACHINE RELATIVE PRECISION       *                */
/*     *   RPARM(1)    STOPPING CRITERION               *                */
/*     *                                                *                */
/*     *   ALSO CHANGE SYSTEM-DEPENDENT ROUTINE         *                */
/*     *   SECOND USED IN TIMER                         *                */
/*     *                                                *                */
/*     **************************************************                */
/*                                                                       */
/*     SPECIFICATIONS FOR ARGUMENTS                                      */
/*                                                                       */
/*     SPECIFICATIONS FOR LOCAL VARIABLES                                */
/*                                                                       */
/* ... VARIABLES IN COMMON BLOCK - ITCOM1                                */
/*                                                                       */
/*     IN     - ITERATION NUMBER                                         */
/*     IS     - ITERATION NUMBER WHEN PARAMETERS LAST CHANGED            */
/*     ISYM   - SYMMETRIC/NONSYMMETRIC STORAGE FORMAT SWITCH             */
/*     ITMAX  - MAXIMUM NUMBER OF ITERATIONS ALLOWED                     */
/*     LEVEL  - LEVEL OF OUTPUT CONTROL SWITCH                           */
/*     NOUT   - OUTPUT UNIT NUMBER                                       */
/*                                                                       */
/* ... VARIABLES IN COMMON BLOCK - ITCOM2                                */
/*                                                                       */
/*     ADAPT  - FULLY ADAPTIVE PROCEDURE SWITCH                          */
/*     BETADT - SWITCH FOR ADAPTIVE DETERMINATION OF BETA                */
/*     CASEII - ADAPTIVE PROCEDURE CASE SWITCH                           */
/*     HALT   - STOPPING TEST SWITCH                                     */
/*     PARTAD - PARTIALLY ADAPTIVE PROCEDURE SWITCH                      */
/*                                                                       */
/* ... VARIABLES IN COMMON BLOCK - ITCOM3                                */
/*                                                                       */
/*     BDELNM - TWO NORM OF B TIMES DELTA-SUPER-N                        */
/*     BETAB  - ESTIMATE FOR THE SPECTRAL RADIUS OF LU MATRIX            */
/*     CME    - ESTIMATE OF LARGEST EIGENVALUE                           */
/*     DELNNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION N          */
/*     DELSNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION S          */
/*     FF     - ADAPTIVE PROCEDURE DAMPING FACTOR                        */
/*     GAMMA  - ACCELERATION PARAMETER                                   */
/*     OMEGA  - OVERRELAXATION PARAMETER FOR SOR AND SSOR                */
/*     QA     - PSEUDO-RESIDUAL RATIO                                    */
/*     QT     - VIRTUAL SPECTRAL RADIUS                                  */
/*     RHO    - ACCELERATION PARAMETER                                   */
/*     RRR    - ADAPTIVE PARAMETER                                       */
/*     SIGE   - PARAMETER SIGMA-SUB-E                                    */
/*     SME    - ESTIMATE OF SMALLEST EIGENVALUE                          */
/*     SPECR  - SPECTRAL RADIUS ESTIMATE FOR SSOR                        */
/*     DRELPR - MACHINE RELATIVE PRECISION                               */
/*     STPTST - STOPPING PARAMETER                                       */
/*     UDNM   - TWO NORM OF U                                            */
/*     ZETA   - STOPPING CRITERION                                       */

    itcom1_1.level = iparm[1];
    itcom1_1.nout = iparm[3];
    ier = 0;
    if (iparm[0] <= 0)
        return 0;

    if (iparm[10] == 0)
        timj1 = timer_((real*)0);

    if (itcom1_1.level < 3)
        echout_(iparm, rparm, &c__3);
    else
        echall_(n, ia, ja, a, rhs, iparm, rparm, &c__1);
    temp = itcom3_1.drelpr * 500.;
    if (itcom3_1.zeta < temp)
        itcom3_1.zeta = temp;

    time1 = rparm[8];
    time2 = rparm[9];
    digit1 = rparm[10];
    digit2 = rparm[11];

    /* ... VERIFY N */

    if (*n <= 0) {
        ier = 31;
        goto L360;
    }

    /* ... REMOVE ROWS AND COLUMNS IF REQUESTED */

    if (iparm[9] != 0) {
        tol = rparm[7];
        ivfill_(n, iwksp, &c__0);
        vfill_(n, wksp, &c_b21);
        sbelm_(n, ia, ja, a, rhs, iwksp, wksp, &tol, &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
        if (ier != 0)
            goto L360;
    }

    /* ... INITIALIZE WKSP BASE ADDRESSES. */

    ib1 = 0;
    ib2 = ib1 + *n;
    ib3 = ib2 + *n;
    iparm[7] = *n;
    if (*nw < iparm[7]) {
        ier = 32;
        goto L360;
    }

    /* ... PERMUTE TO  RED-BLACK SYSTEM IF REQUESTED */

    nb = iparm[8];
    if (nb < 0)
        goto L170;

    n3 = *n * 3;
    ivfill_(&n3, iwksp, &c__0);
    prbndx_(n, &nb, ia, ja, iwksp, &iwksp[ib2], &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L360;

    /* ... PERMUTE MATRIX AND RHS */

    permat_(n, ia, ja, a, iwksp, &iwksp[ib3], &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L360;

    pervec_(n, rhs, iwksp);
    pervec_(n, u, iwksp);

    /* ... SCALE LINEAR SYSTEM, U, AND RHS BY THE SQUARE ROOT OF THE */
    /* ... DIAGONAL ELEMENTS. */

L170:
    vfill_(&iparm[7], wksp, &c_b21);
    scal_(n, ia, ja, a, rhs, u, wksp, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L360;

    if (iparm[10] == 0)
        timi1 = timer_((real*)0);

    /* ... ITERATION SEQUENCE */

    itmax1 = itcom1_1.itmax + 1;
    for (loop = 1; loop <= itmax1; ++loop) {
        itcom1_1.in = loop - 1;

    /* ... CODE FOR ONE ITERATION. */

    /*     U           = U(IN) */

        itsor_(n, ia, ja, a, rhs, u, &wksp[ib1]);

        if (itcom2_1.halt)
            goto L270;
    }

    /* ... ITMAX HAS BEEN REACHED */

    if (iparm[10] == 0) {
        timi2 = timer_((real*)0);
        time1 = (doublereal) (timi2 - timi1);
    }
    ier = 33;
    if (iparm[2] == 0)
        rparm[0] = itcom3_1.stptst;

    goto L300;

    /* ... METHOD HAS CONVERGED */

L270:
    if (iparm[10] == 0) {
        timi2 = timer_((real*)0);
        time1 = (doublereal) (timi2 - timi1);
    }

    /* ... UNSCALE THE MATRIX, SOLUTION, AND RHS VECTORS. */

L300:
    unscal_(n, ia, ja, a, rhs, u, wksp);

    /* ... UN-PERMUTE MATRIX,RHS, AND SOLUTION */

    if (iparm[8] < 0)
        goto L330;

    permat_(n, ia, ja, a, &iwksp[ib2], &iwksp[ib3], &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ierper);
    if (ierper != 0) {
        if (ier == 0)
            ier = ierper;

        goto L360;
    }

    pervec_(n, rhs, &iwksp[ib2]);
    pervec_(n, u, &iwksp[ib2]);

    /* ... OPTIONAL ERROR ANALYSIS */

L330:
    idgts = iparm[11];
    if (idgts >= 0) {
        if (iparm[1] <= 0)
            idgts = 0;

        perror_(n, ia, ja, a, rhs, u, wksp, &digit1, &digit2, &idgts);
    }

    /* ... SET RETURN PARAMETERS IN IPARM AND RPARM */

    if (iparm[10] == 0) {
        timj2 = timer_((real*)0);
        time2 = (doublereal) (timj2 - timj1);
    }
    if (iparm[2] == 0) {
        iparm[0] = itcom1_1.in;
        iparm[8] = nb;
        rparm[1] = itcom3_1.cme;
        rparm[2] = itcom3_1.sme;
        rparm[4] = itcom3_1.omega;
        rparm[8] = time1;
        rparm[9] = time2;
        rparm[10] = digit1;
        rparm[11] = digit2;
    }

L360:
    *ierr = ier;
    if (itcom1_1.level >= 3)
        echall_(n, ia, ja, a, rhs, iparm, rparm, &c__2);

    return 0;
} /* sor_ */

/* Subroutine */
int ssorcg_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u,
            integer *iwksp, integer *nw, doublereal *wksp, integer *iparm, doublereal *rparm, integer* ierr)
{
    /* Local variables */
    static integer n3, nb, ib1, ib2, ib3, ib4, ib5, ib6, ib7, ier;
    static doublereal tol;
    static doublereal temp;
    static integer loop;
    static doublereal time1, time2;
    static real timi1, timj1, timi2, timj2;
    static integer idgts;
    static doublereal digit1, digit2;
    static integer itmax1;
    static doublereal betnew;
    static integer ierper;

/*     ITPACK 2C MAIN SUBROUTINE  SSORCG  (SYMMETRIC SUCCESSIVE OVER-    */
/*                                        RELAXATION CONJUGATE GRADIENT) */
/*     EACH OF THE MAIN SUBROUTINES:                                     */
/*           JCG, JSI, SOR, SSORCG, SSORSI, RSCG, RSSI                   */
/*     CAN BE USED INDEPENDENTLY OF THE OTHERS                           */

/*          THIS SUBROUTINE, SSORCG, DRIVES THE  SYMMETRIC SOR-CG         */
/*          ALGORITHM.                                                    */
/*                                                                        */
/* ... PARAMETER LIST:                                                    */
/*                                                                        */
/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX.               */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF      */
/*                 THE SPARSE MATRIX REPRESENTATION.                      */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE       */
/*                 MATRIX REPRESENTATION.                                 */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE       */
/*                 OF THE MATRIX PROBLEM.                                 */
/*          U      INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U CONTAINS THE    */
/*                 INITIAL GUESS TO THE SOLUTION. ON OUTPUT, IT CONTAINS  */
/*                 THE LATEST ESTIMATE TO THE SOLUTION.                   */
/*          IWKSP  INTEGER VECTOR WORKSPACE OF LENGTH 3*N                 */
/*          NW     INPUT INTEGER.  LENGTH OF AVAILABLE WKSP.  ON OUTPUT,  */
/*                 IPARM(8) IS AMOUNT USED.                               */
/*          WKSP   D.P. VECTOR USED FOR WORKING SPACE.  SSOR-CG           */
/*                 NEEDS TO BE IN LENGTH AT LEAST                         */
/*                 6*N + 2*ITMAX,  IF IPARM(5)=0  (SYMMETRIC STORAGE)     */
/*                 6*N + 4*ITMAX,  IF IPARM(5)=1  (NONSYMMETRIC STORAGE)  */
/*          IPARM  INTEGER VECTOR OF LENGTH 12.  ALLOWS USER TO SPECIFY   */
/*                 SOME INTEGER PARAMETERS WHICH AFFECT THE METHOD.  IF   */
/*          RPARM  D.P. VECTOR OF LENGTH 12. ALLOWS USER TO SPECIFY SOME  */
/*                 D.P. PARAMETERS WHICH AFFECT THE METHOD.               */
/*          IER    OUTPUT INTEGER.  ERROR FLAG. (= IERR)                  */
/*                                                                        */
/* ... SSORCG SUBPROGRAM REFERENCES:                                      */
/*                                                                        */
/*          FROM ITPACK    BISRCH, CHGCON, DETERM, DFAULT, ECHALL,        */
/*                         ECHOUT, EIGVNS, EIGVSS, EQRT1S, ITERM, TIMER,  */
/*                         ITSRCG, IVFILL, OMEG, OMGCHG, OMGSTR,          */
/*                         PARCON, PBETA, PBSOR, PERMAT, PERROR,          */
/*                         PERVEC, PFSOR, PJAC, PMULT, PRBNDX, PSTOP, PVT */
/*                         QSORT, SBELM, SCAL, DCOPY, DDOT, SUM3,         */
/*                         UNSCAL, VEVMW, VEVPW, VFILL, VOUT, WEVMW,      */
/*                         ZBRENT                                         */
/*          SYSTEM         DABS, DLOG, DLOG10, DBLE(AMAX0), DMAX1, AMIN1, */
/*                         MOD, DSQRT                                     */
/*                                                                        */
/*     VERSION:  ITPACK 2C (MARCH 1982)                                   */
/*                                                                        */
/*     CODE WRITTEN BY:  DAVID KINCAID, ROGER GRIMES, JOHN RESPESS        */
/*                       CENTER FOR NUMERICAL ANALYSIS                    */
/*                       UNIVERSITY OF TEXAS                              */
/*                       AUSTIN, TX  78712                                */
/*                       (512) 471-1242                                   */
/*                                                                        */
/*     FOR ADDITIONAL DETAILS ON THE                                      */
/*          (A) SUBROUTINE SEE TOMS ARTICLE 1982                          */
/*          (B) ALGORITHM  SEE CNA REPORT 150                             */
/*                                                                        */
/*     BASED ON THEORY BY:  DAVID YOUNG, DAVID KINCAID, LOU HAGEMAN       */
/*                                                                        */
/*     REFERENCE THE BOOK:  APPLIED ITERATIVE METHODS                     */
/*                          L. HAGEMAN, D. YOUNG                          */
/*                          ACADEMIC PRESS, 1981                          */
/*                                                                        */
/*     **************************************************                 */
/*     *               IMPORTANT NOTE                   *                 */
/*     *                                                *                 */
/*     *      WHEN INSTALLING ITPACK ROUTINES ON A      *                 */
/*     *  DIFFERENT COMPUTER, RESET SOME OF THE VALUES  *                 */
/*     *  IN  SUBROUTNE DFAULT.   MOST IMPORTANT ARE    *                 */
/*     *                                                *                 */
/*     *   DRELPR      MACHINE RELATIVE PRECISION       *                 */
/*     *   RPARM(1)    STOPPING CRITERION               *                 */
/*     *                                                *                 */
/*     *   ALSO CHANGE SYSTEM-DEPENDENT ROUTINE         *                 */
/*     *   SECOND USED IN TIMER                         *                 */
/*     *                                                *                 */
/*     **************************************************                 */
/*                                                                        */
/*     SPECIFICATIONS FOR ARGUMENTS                                       */
/*                                                                        */
/*     SPECIFICATIONS FOR LOCAL VARIABLES                                 */
/*                                                                        */
/* ... VARIABLES IN COMMON BLOCK - ITCOM1                                 */
/*                                                                        */
/*     IN     - ITERATION NUMBER                                          */
/*     IS     - ITERATION NUMBER WHEN PARAMETERS LAST CHANGED             */
/*     ISYM   - SYMMETRIC/NONSYMMETRIC STORAGE FORMAT SWITCH              */
/*     ITMAX  - MAXIMUM NUMBER OF ITERATIONS ALLOWED                      */
/*     LEVEL  - LEVEL OF OUTPUT CONTROL SWITCH                            */
/*     NOUT   - OUTPUT UNIT NUMBER                                        */
/*                                                                        */
/* ... VARIABLES IN COMMON BLOCK - ITCOM2                                 */
/*                                                                        */
/*     ADAPT  - FULLY ADAPTIVE PROCEDURE SWITCH                           */
/*     BETADT - SWITCH FOR ADAPTIVE DETERMINATION OF BETA                 */
/*     CASEII - ADAPTIVE PROCEDURE CASE SWITCH                            */
/*     HALT   - STOPPING TEST SWITCH                                      */
/*     PARTAD - PARTIALLY ADAPTIVE PROCEDURE SWITCH                       */
/*                                                                        */
/* ... VARIABLES IN COMMON BLOCK - ITCOM3                                 */
/*                                                                        */
/*     BDELNM - TWO NORM OF B TIMES DELTA-SUPER-N                         */
/*     BETAB  - ESTIMATE FOR THE SPECTRAL RADIUS OF LU MATRIX             */
/*     CME    - ESTIMATE OF LARGEST EIGENVALUE                            */
/*     DELNNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION N           */
/*     DELSNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION S           */
/*     FF     - ADAPTIVE PROCEDURE DAMPING FACTOR                         */
/*     GAMMA  - ACCELERATION PARAMETER                                    */
/*     OMEGA  - OVERRELAXATION PARAMETER FOR SOR AND SSOR                 */
/*     QA     - PSEUDO-RESIDUAL RATIO                                     */
/*     QT     - VIRTUAL SPECTRAL RADIUS                                   */
/*     RHO    - ACCELERATION PARAMETER                                    */
/*     RRR    - ADAPTIVE PARAMETER                                        */
/*     SIGE   - PARAMETER SIGMA-SUB-E                                     */
/*     SME    - ESTIMATE OF SMALLEST EIGENVALUE                           */
/*     SPECR  - SPECTRAL RADIUS ESTIMATE FOR SSOR                         */
/*     DRELPR - MACHINE RELATIVE PRECISION                                */
/*     STPTST - STOPPING PARAMETER                                        */
/*     UDNM   - TWO NORM OF U                                             */
/*     ZETA   - STOPPING CRITERION                                        */

    itcom1_1.level = iparm[1];
    itcom1_1.nout = iparm[3];
    if (iparm[8] >= 0)
        iparm[5] = 2;

    ier = 0;
    if (iparm[0] <= 0)
        return 0;

    if (iparm[10] == 0)
        timj1 = timer_((real*)0);

    if (itcom1_1.level < 3)
        echout_(iparm, rparm, &c__4);
    else
        echall_(n, ia, ja, a, rhs, iparm, rparm, &c__1);
    temp = itcom3_1.drelpr * 500.;
    if (itcom3_1.zeta < temp)
        itcom3_1.zeta = temp;

    time1 = rparm[8];
    time2 = rparm[9];
    digit1 = rparm[10];
    digit2 = rparm[11];

    /* ... VERIFY N */

    if (*n <= 0) {
        ier = 41;
        goto L390;
    }

    /* ... REMOVE ROWS AND COLUMNS IF REQUESTED */

    if (iparm[9] != 0) {
        tol = rparm[7];
        ivfill_(n, iwksp, &c__0);
        vfill_(n, wksp, &c_b21);
        sbelm_(n, ia, ja, a, rhs, iwksp, wksp, &tol, &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
        if (ier != 0)
            goto L390;
    }

    /* ... INITIALIZE WKSP BASE ADDRESSES. */

    ib1 = 0;
    ib2 = ib1 + *n;
    ib3 = ib2 + *n;
    ib4 = ib3 + *n;
    ib5 = ib4 + *n;
    ib6 = ib5 + *n;
    ib7 = ib6 + *n;
    iparm[7] = *n * 6 + (itcom1_1.itmax << 1);
    if (itcom1_1.isym != 0)
        iparm[7] += itcom1_1.itmax << 1;

    if (*nw < iparm[7]) {
        ier = 42;
        goto L390;
    }

    /* ... PERMUTE TO  RED-BLACK SYSTEM IF REQUESTED */

    nb = iparm[8];
    if (nb < 0)
        goto L170;

    n3 = *n * 3;
    ivfill_(&n3, iwksp, &c__0);
    prbndx_(n, &nb, ia, ja, iwksp, &iwksp[ib2], &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L390;

    /* ... PERMUTE MATRIX AND RHS */

    permat_(n, ia, ja, a, iwksp, &iwksp[ib3], &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L390;

    pervec_(n, rhs, iwksp);
    pervec_(n, u, iwksp);

    /* ... SCALE LINEAR SYSTEM, U, AND RHS BY THE SQUARE ROOT OF THE */
    /* ... DIAGONAL ELEMENTS. */

L170:
    vfill_(&iparm[7], wksp, &c_b21);
    scal_(n, ia, ja, a, rhs, u, wksp, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L390;

    if (iparm[10] == 0)
        timi1 = timer_((real*)0);

    /* ... SPECIAL PROCEDURE FOR FULLY ADAPTIVE CASE. */

    if (! itcom2_1.adapt)
        goto L250;

    if (itcom2_1.betadt) {
        vfill_(n, &wksp[ib1], &c_b286);
        betnew = pbeta_(n, ia, ja, a, &wksp[ib1], &wksp[ib2], &wksp[ib3]) / (doublereal)(*n);
        itcom3_1.betab = max(max(itcom3_1.betab,.25),betnew);
    }

    omeg_(&c_b21, &c__1);
    itcom1_1.is = 0;

    /* ... INITIALIZE FORWARD PSEUDO-RESIDUAL */

L250:
    itpackdcopy_(n, rhs, &c__1, &wksp[ib1], &c__1);
    itpackdcopy_(n, u, &c__1, &wksp[ib2], &c__1);
    pfsor_(n, ia, ja, a, &wksp[ib2], &wksp[ib1]);
    vevmw_(n, &wksp[ib2], u);

    /* ... ITERATION SEQUENCE */

    itmax1 = itcom1_1.itmax + 1;
    for (loop = 1; loop <= itmax1; ++loop) {
        itcom1_1.in = loop - 1;
        if (itcom1_1.in % 2 == 1)
            goto L260;

        /* ... CODE FOR THE EVEN ITERATIONS. */

        /*     U           = U(IN)       WKSP(IB2) = C(IN) */
        /*     WKSP(IB1)   = U(IN-1)     WKSP(IB3) = C(IN-1) */

        itsrcg_(n, ia, ja, a, rhs, u, &wksp[ib1], &wksp[ib2], &wksp[ib3], &wksp[ib4], &wksp[ib5], &wksp[ib6], &wksp[ib7]);

        if (itcom2_1.halt)
            goto L300;

        continue;

        /* ... CODE FOR THE ODD ITERATIONS. */

        /*     U           = U(IN-1)     WKSP(IB2) = C(IN-1) */
        /*     WKSP(IB1)   = U(IN)       WKSP(IB3) =C(IN) */

L260:
        itsrcg_(n, ia, ja, a, rhs, &wksp[ib1], u, &wksp[ib3], &wksp[ib2], &wksp[ib4], &wksp[ib5], &wksp[ib6], &wksp[ib7]);

        if (itcom2_1.halt)
            goto L300;
    }

    /* ... ITMAX HAS BEEN REACHED */

    if (iparm[10] == 0) {
        timi2 = timer_((real*)0);
        time1 = (doublereal) (timi2 - timi1);
    }
    ier = 43;
    if (iparm[2] == 0)
        rparm[0] = itcom3_1.stptst;

    goto L330;

    /* ... METHOD HAS CONVERGED */

L300:
    if (iparm[10] == 0) {
        timi2 = timer_((real*)0);
        time1 = (doublereal) (timi2 - timi1);
    }

    /* ... PUT SOLUTION INTO U IF NOT ALREADY THERE. */

L330:
    if (itcom1_1.in % 2 == 1)
        itpackdcopy_(n, &wksp[ib1], &c__1, u, &c__1);

    /* ... UNSCALE THE MATRIX, SOLUTION, AND RHS VECTORS. */

    unscal_(n, ia, ja, a, rhs, u, wksp);

    /* ... UN-PERMUTE MATRIX,RHS, AND SOLUTION */

    if (iparm[8] < 0)
        goto L360;

    permat_(n, ia, ja, a, &iwksp[ib2], &iwksp[ib3], &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ierper);
    if (ierper != 0) {
        if (ier == 0)
            ier = ierper;

        goto L390;
    }

    pervec_(n, rhs, &iwksp[ib2]);
    pervec_(n, u, &iwksp[ib2]);

    /* ... OPTIONAL ERROR ANALYSIS */

L360:
    idgts = iparm[11];
    if (idgts >= 0) {
        if (iparm[1] <= 0)
            idgts = 0;

        perror_(n, ia, ja, a, rhs, u, wksp, &digit1, &digit2, &idgts);
    }

    /* ... SET RETURN PARAMETERS IN IPARM AND RPARM */

    if (iparm[10] == 0) {
        timj2 = timer_((real*)0);
        time2 = (doublereal) (timj2 - timj1);
    }
    iparm[7] -= (itcom1_1.itmax - itcom1_1.in) << 1;
    if (itcom1_1.isym != 0)
        iparm[7] -= (itcom1_1.itmax - itcom1_1.in) << 1;

    if (iparm[2] == 0) {
        iparm[0] = itcom1_1.in;
        iparm[8] = nb;
        rparm[1] = itcom3_1.cme;
        rparm[2] = itcom3_1.sme;
        rparm[4] = itcom3_1.omega;
        rparm[5] = itcom3_1.specr;
        rparm[6] = itcom3_1.betab;
        rparm[8] = time1;
        rparm[9] = time2;
        rparm[10] = digit1;
        rparm[11] = digit2;
    }

L390:
    *ierr = ier;
    if (itcom1_1.level >= 3)
        echall_(n, ia, ja, a, rhs, iparm, rparm, &c__2);

    return 0;
} /* ssorcg_ */

/* Subroutine */
int ssorsi_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u,
            integer *iwksp, integer *nw, doublereal *wksp, integer *iparm, doublereal *rparm, integer* ierr)
{
    /* Local variables */
    static integer n3, nb, ib1, ib2, ib3, ib4, ib5, ier;
    static doublereal tol;
    static doublereal temp;
    static integer loop;
    static doublereal time1, time2;
    static real timi1, timj1, timi2, timj2;
    static integer idgts;
    static doublereal digit1, digit2;
    static integer itmax1;
    static doublereal betnew;
    static integer ierper;

/*     ITPACK 2C MAIN SUBROUTINE  SSORSI  (SYMMETRIC SUCCESSIVE RELAX- */
/*                                         ATION SEMI-ITERATION)       */
/*     EACH OF THE MAIN SUBROUTINES:                                   */
/*           JCG, JSI, SOR, SSORCG, SSORSI, RSCG, RSSI                 */
/*     CAN BE USED INDEPENDENTLY OF THE OTHERS                         */

/*          THIS SUBROUTINE, SSORSI, DRIVES THE  SYMMETRIC SOR-SI         */
/*          ALGORITHM.                                                    */
/*                                                                        */
/* ... PARAMETER LIST:                                                    */
/*                                                                        */
/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX.               */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF      */
/*                 THE SPARSE MATRIX REPRESENTATION.                      */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE       */
/*                 MATRIX REPRESENTATION.                                 */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE       */
/*                 OF THE MATRIX PROBLEM.                                 */
/*          U      INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U CONTAINS THE    */
/*                 INITIAL GUESS TO THE SOLUTION. ON OUTPUT, IT CONTAINS  */
/*                 THE LATEST ESTIMATE TO THE SOLUTION.                   */
/*          IWKSP  INTEGER VECTOR WORKSPACE OF LENGTH 3*N                 */
/*          NW     INPUT INTEGER.  LENGTH OF AVAILABLE WKSP.  ON OUTPUT,  */
/*                 IPARM(8) IS AMOUNT USED.                               */
/*          WKSP   D.P. VECTOR USED FOR WORKING SPACE.  SSORSI            */
/*                 NEEDS THIS TO BE IN LENGTH AT LEAST  5*N               */
/*          IPARM  INTEGER VECTOR OF LENGTH 12.  ALLOWS USER TO SPECIFY   */
/*                 SOME INTEGER PARAMETERS WHICH AFFECT THE METHOD.  IF   */
/*          RPARM  D.P. VECTOR OF LENGTH 12. ALLOWS USER TO SPECIFY SOME  */
/*                 D.P. PARAMETERS WHICH AFFECT THE METHOD.               */
/*          IER    OUTPUT INTEGER.  ERROR FLAG. (= IERR)                  */
/*                                                                        */
/* ... SSORSI SUBPROGRAM REFERENCES:                                      */
/*                                                                        */
/*          FROM ITPACK    BISRCH, CHEBY, CHGSI, DFAULT, ECHALL, ECHOUT,  */
/*                         ITERM, TIMER, ITSRSI, IVFILL, OMEG,            */
/*                         OMGSTR, PARSI, PBETA, PERMAT, PERROR,          */
/*                         PERVEC, PFSOR, PMULT, PRBNDX, PSSOR1,          */
/*                         PSTOP, PVTBV, QSORT, SBELM, SCAL, DCOPY,       */
/*                         DDOT, SUM3, TSTCHG, UNSCAL, VEVPW, VFILL,      */
/*                         VOUT, WEVMW                                    */
/*          SYSTEM         DABS, DLOG, DLOG10, DBLE(AMAX0), DMAX1,        */
/*                         DBLE(FMOD), DSQRT                              */
/*                                                                        */
/*     VERSION:  ITPACK 2C (MARCH 1982)                                   */
/*                                                                        */
/*     CODE WRITTEN BY:  DAVID KINCAID, ROGER GRIMES, JOHN RESPESS        */
/*                       CENTER FOR NUMERICAL ANALYSIS                    */
/*                       UNIVERSITY OF TEXAS                              */
/*                       AUSTIN, TX  78712                                */
/*                       (512) 471-1242                                   */
/*                                                                        */
/*     FOR ADDITIONAL DETAILS ON THE                                      */
/*          (A) SUBROUTINE SEE TOMS ARTICLE 1982                          */
/*          (B) ALGORITHM  SEE CNA REPORT 150                             */
/*                                                                        */
/*     BASED ON THEORY BY:  DAVID YOUNG, DAVID KINCAID, LOU HAGEMAN       */
/*                                                                        */
/*     REFERENCE THE BOOK:  APPLIED ITERATIVE METHODS                     */
/*                          L. HAGEMAN, D. YOUNG                          */
/*                          ACADEMIC PRESS, 1981                          */
/*                                                                        */
/*     **************************************************                 */
/*     *               IMPORTANT NOTE                   *                 */
/*     *                                                *                 */
/*     *      WHEN INSTALLING ITPACK ROUTINES ON A      *                 */
/*     *  DIFFERENT COMPUTER, RESET SOME OF THE VALUES  *                 */
/*     *  IN  SUBROUTNE DFAULT.   MOST IMPORTANT ARE    *                 */
/*     *                                                *                 */
/*     *   DRELPR      MACHINE RELATIVE PRECISION       *                 */
/*     *   RPARM(1)    STOPPING CRITERION               *                 */
/*     *                                                *                 */
/*     *   ALSO CHANGE SYSTEM-DEPENDENT ROUTINE         *                 */
/*     *   SECOND USED IN TIMER                         *                 */
/*     *                                                *                 */
/*     **************************************************                 */
/*                                                                        */
/*     SPECIFICATIONS FOR ARGUMENTS                                       */
/*                                                                        */
/*     SPECIFICATIONS FOR LOCAL VARIABLES                                 */
/*                                                                        */
/* ... VARIABLES IN COMMON BLOCK - ITCOM1                                 */
/*                                                                        */
/*     IN     - ITERATION NUMBER                                          */
/*     ISYM   - SYMMETRIC/NONSYMMETRIC STORAGE FORMAT SWITCH              */
/*     IS     - ITERATION NUMBER WHEN PARAMETERS LAST CHANGED             */
/*     ITMAX  - MAXIMUM NUMBER OF ITERATIONS ALLOWED                      */
/*     LEVEL  - LEVEL OF OUTPUT CONTROL SWITCH                            */
/*     NOUT   - OUTPUT UNIT NUMBER                                        */
/*                                                                        */
/* ... VARIABLES IN COMMON BLOCK - ITCOM2                                 */
/*                                                                        */
/*     ADAPT  - FULLY ADAPTIVE PROCEDURE SWITCH                           */
/*     BETADT - SWITCH FOR ADAPTIVE DETERMINATION OF BETA                 */
/*     CASEII - ADAPTIVE PROCEDURE CASE SWITCH                            */
/*     HALT   - STOPPING TEST SWITCH                                      */
/*     PARTAD - PARTIALLY ADAPTIVE PROCEDURE SWITCH                       */
/*                                                                        */
/* ... VARIABLES IN COMMON BLOCK - ITCOM3                                 */
/*                                                                        */
/*     BDELNM - TWO NORM OF B TIMES DELTA-SUPER-N                         */
/*     BETAB  - ESTIMATE FOR THE SPECTRAL RADIUS OF LU MATRIX             */
/*     CME    - ESTIMATE OF LARGEST EIGENVALUE                            */
/*     DELNNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION N           */
/*     DELSNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION S           */
/*     FF     - ADAPTIVE PROCEDURE DAMPING FACTOR                         */
/*     GAMMA  - ACCELERATION PARAMETER                                    */
/*     OMEGA  - OVERRELAXATION PARAMETER FOR SOR AND SSOR                 */
/*     QA     - PSEUDO-RESIDUAL RATIO                                     */
/*     QT     - VIRTUAL SPECTRAL RADIUS                                   */
/*     RHO    - ACCELERATION PARAMETER                                    */
/*     RRR    - ADAPTIVE PARAMETER                                        */
/*     SIGE   - PARAMETER SIGMA-SUB-E                                     */
/*     SME    - ESTIMATE OF SMALLEST EIGENVALUE                           */
/*     SPECR  - SPECTRAL RADIUS ESTIMATE FOR SSOR                         */
/*     DRELPR - MACHINE RELATIVE PRECISION                                */
/*     STPTST - STOPPING PARAMETER                                        */
/*     UDNM   - TWO NORM OF U                                             */
/*     ZETA   - STOPPING CRITERION                                        */

    itcom1_1.level = iparm[1];
    itcom1_1.nout = iparm[3];
    if (iparm[8] >= 0)
        iparm[5] = 2;

    ier = 0;
    if (iparm[0] <= 0)
        return 0;

    if (iparm[10] == 0)
        timj1 = timer_((real*)0);

    if (itcom1_1.level < 3)
        echout_(iparm, rparm, &c__5);
    else
        echall_(n, ia, ja, a, rhs, iparm, rparm, &c__1);
    temp = itcom3_1.drelpr * 500.;
    if (itcom3_1.zeta < temp)
        itcom3_1.zeta = temp;

    time1 = rparm[8];
    time2 = rparm[9];
    digit1 = rparm[10];
    digit2 = rparm[11];

    /* ... VERIFY N */

    if (*n <= 0) {
        ier = 51;
        goto L380;
    }

    /* ... REMOVE ROWS AND COLUMNS IF REQUESTED */

    if (iparm[9] != 0) {
        tol = rparm[7];
        ivfill_(n, iwksp, &c__0);
        vfill_(n, wksp, &c_b21);
        sbelm_(n, ia, ja, a, rhs, iwksp, wksp, &tol, &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
        if (ier != 0)
            goto L380;
    }

    /* ... INITIALIZE WKSP BASE ADDRESSES. */

    ib1 = 0;
    ib2 = ib1 + *n;
    ib3 = ib2 + *n;
    ib4 = ib3 + *n;
    ib5 = ib4 + *n;
    iparm[7] = *n * 5;
    if (*nw < iparm[7])
        ier = 52;

    /* ... PERMUTE TO  RED-BLACK SYSTEM IF REQUESTED */

    nb = iparm[8];
    if (nb < 0)
        goto L170;

    n3 = *n * 3;
    ivfill_(&n3, iwksp, &c__0);
    prbndx_(n, &nb, ia, ja, iwksp, &iwksp[ib2], &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L380;

    /* ... PERMUTE MATRIX AND RHS */

    permat_(n, ia, ja, a, iwksp, &iwksp[ib3], &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L380;

    pervec_(n, rhs, iwksp);
    pervec_(n, u, iwksp);

    /* ... SCALE LINEAR SYSTEM, U, AND RHS BY THE SQUARE ROOT OF THE */
    /* ... DIAGONAL ELEMENTS. */

L170:
    vfill_(&iparm[7], wksp, &c_b21);
    scal_(n, ia, ja, a, rhs, u, wksp, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L380;

    if (iparm[10] == 0)
        timi1 = timer_((real*)0);

    /* ... SPECIAL PROCEDURE FOR FULLY ADAPTIVE CASE. */

    if (! itcom2_1.adapt)
        goto L240;

    if (itcom2_1.betadt) {
        vfill_(n, &wksp[ib1], &c_b286);
        betnew = pbeta_(n, ia, ja, a, &wksp[ib1], &wksp[ib2], &wksp[ib3]) / (doublereal)(*n);
        itcom3_1.betab = max(max(itcom3_1.betab,.25),betnew);
    }

    omeg_(&c_b21, &c__1);
    itcom1_1.is = 0;

    /* ... ITERATION SEQUENCE */

L240:
    itmax1 = itcom1_1.itmax + 1;
    for (loop = 1; loop <= itmax1; ++loop) {
        itcom1_1.in = loop - 1;
        if (itcom1_1.in % 2 == 1)
            goto L250;

        /* ... CODE FOR THE EVEN ITERATIONS. */

        /*     U           = U(IN) */
        /*     WKSP(IB1)   = U(IN-1) */

        itsrsi_(n, ia, ja, a, rhs, u, &wksp[ib1], &wksp[ib2], &wksp[ib3], &wksp[ib4], &wksp[ib5]);

        if (itcom2_1.halt)
            goto L290;

        continue;

        /* ... CODE FOR THE ODD ITERATIONS. */

        /*     U           = U(IN-1) */
        /*     WKSP(IB1)   = U(IN) */

L250:
        itsrsi_(n, ia, ja, a, rhs, &wksp[ib1], u, &wksp[ib2], &wksp[ib3], &wksp[ib4], &wksp[ib5]);

        if (itcom2_1.halt)
            goto L290;
    }

    /* ... ITMAX HAS BEEN REACHED */

    if (iparm[10] == 0) {
        timi2 = timer_((real*)0);
        time1 = (doublereal) (timi2 - timi1);
    }
    ier = 53;
    if (iparm[2] == 0)
        rparm[0] = itcom3_1.stptst;

    goto L320;

    /* ... METHOD HAS CONVERGED */

L290:
    if (iparm[10] == 0) {
        timi2 = timer_((real*)0);
        time1 = (doublereal) (timi2 - timi1);
    }

    /* ... PUT SOLUTION INTO U IF NOT ALREADY THERE. */

L320:
    if (itcom1_1.in % 2 == 1)
        itpackdcopy_(n, &wksp[ib1], &c__1, u, &c__1);

    /* ... UNSCALE THE MATRIX, SOLUTION, AND RHS VECTORS. */

    unscal_(n, ia, ja, a, rhs, u, wksp);

    /* ... UN-PERMUTE MATRIX,RHS, AND SOLUTION */

    if (iparm[8] < 0)
        goto L350;

    permat_(n, ia, ja, a, &iwksp[ib2], &iwksp[ib3], &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ierper);
    if (ierper != 0) {
        if (ier == 0)
            ier = ierper;

        goto L380;
    }

    pervec_(n, rhs, &iwksp[ib2]);
    pervec_(n, u, &iwksp[ib2]);

    /* ... OPTIONAL ERROR ANALYSIS */

L350:
    idgts = iparm[11];
    if (idgts >= 0) {
        if (iparm[1] <= 0)
            idgts = 0;

        perror_(n, ia, ja, a, rhs, u, wksp, &digit1, &digit2, &idgts);
    }

    /* ... SET RETURN PARAMETERS IN IPARM AND RPARM */

    if (iparm[10] == 0) {
        timj2 = timer_((real*)0);
        time2 = (doublereal) (timj2 - timj1);
    }
    if (iparm[2] == 0) {
        iparm[0] = itcom1_1.in;
        iparm[8] = nb;
        rparm[1] = itcom3_1.cme;
        rparm[2] = itcom3_1.sme;
        rparm[4] = itcom3_1.omega;
        rparm[5] = itcom3_1.specr;
        rparm[6] = itcom3_1.betab;
        rparm[8] = time1;
        rparm[9] = time2;
        rparm[10] = digit1;
        rparm[11] = digit2;
    }

L380:
    *ierr = ier;
    if (itcom1_1.level >= 3)
        echall_(n, ia, ja, a, rhs, iparm, rparm, &c__2);

    return 0;
} /* ssorsi_ */

/* Subroutine */
int rscg_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u,
          integer *iwksp, integer *nw, doublereal *wksp, integer *iparm, doublereal *rparm, integer* ierr)
{
    /* Local variables */
    static integer n3, nb, nr, ib1, ib2, ib3, ib4, ib5, jb3, ier;
    static doublereal tol;
    static doublereal temp;
    static integer loop;
    static doublereal time1, time2;
    static real timi1, timj1, timi2, timj2;
    static integer idgts;
    static doublereal digit1, digit2;
    static integer itmax1;
    static integer ierper;

/*     ITPACK 2C MAIN SUBROUTINE  RSCG  (REDUCED SYSTEM CONJUGATE */
/*                                       GRADIENT)                */
/*     EACH OF THE MAIN SUBROUTINES:                              */
/*           JCG, JSI, SOR, SSORCG, SSORSI, RSCG, RSSI            */
/*     CAN BE USED INDEPENDENTLY OF THE OTHERS                    */

/*          THIS SUBROUTINE, RSCG, DRIVES THE  REDUCED SYSTEM CG          */
/*          ALGORITHM.                                                    */
/*                                                                        */
/* ... PARAMETER LIST:                                                    */
/*                                                                        */
/*          N     INPUT INTEGER.  DIMENSION OF THE MATRIX.                */
/*                 IN THE RED-BLACK MATRIX.                               */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF      */
/*                 THE SPARSE MATRIX REPRESENTATION.                      */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE       */
/*                 MATRIX REPRESENTATION.                                 */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE       */
/*                 OF THE MATRIX PROBLEM.                                 */
/*          U      INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U CONTAINS THE    */
/*                 INITIAL GUESS TO THE SOLUTION. ON OUTPUT, IT CONTAINS  */
/*                 THE LATEST ESTIMATE TO THE SOLUTION.                   */
/*          IWKSP  INTEGER VECTOR WORKSPACE OF LENGTH 3*N                 */
/*          NW     INPUT INTEGER.  LENGTH OF AVAILABLE WKSP.  ON OUTPUT,  */
/*                 IPARM(8) IS AMOUNT USED.                               */
/*          WKSP   D.P. VECTOR USED FOR WORKING SPACE.  RSCG NEEDS        */
/*                 THIS TO BE IN LENGTH AT LEAST                          */
/*                 N+3*NB+2*ITMAX, IF IPARM(5)=0  (SYMMETRIC STORAGE)     */
/*                 N+3*NB+4*ITMAX, IF IPARM(5)=1  (NONSYMMETRIC STORAGE)  */
/*                 HERE NB IS THE ORDER OF THE BLACK SUBSYSTEM            */
/*          IPARM  INTEGER VECTOR OF LENGTH 12.  ALLOWS USER TO SPECIFY   */
/*                 SOME INTEGER PARAMETERS WHICH AFFECT THE METHOD.  IF   */
/*          RPARM  D.P. VECTOR OF LENGTH 12. ALLOWS USER TO SPECIFY SOME  */
/*                 D.P. PARAMETERS WHICH AFFECT THE METHOD.               */
/*          IER    OUTPUT INTEGER. ERROR FLAG. (= IERR)                   */
/*                                                                        */
/* ... RSCG SUBPROGRAM REFERENCES:                                        */
/*                                                                        */
/*          FROM ITPACK    BISRCH, CHGCON, DETERM, DFAULT, ECHALL,        */
/*                         ECHOUT, EIGVNS, EIGVSS, EQRT1S, ITERM, TIMER   */
/*                         ITRSCG, IVFILL, PARCON, PERMAT,                */
/*                         PERROR, PERVEC, PMULT, PRBNDX, PRSBLK,         */
/*                         PRSRED, PSTOP, QSORT, SBELM, SCAL, DCOPY,      */
/*                         DDOT, SUM3, UNSCAL, VFILL, VOUT, WEVMW,        */
/*                         ZBRENT                                         */
/*          SYSTEM         DABS, DLOG10, DBLE(AMAX0), DMAX1, MOD, DSQRT   */
/*                                                                        */
/*     VERSION:  ITPACK 2C (MARCH 1982)                                   */
/*                                                                        */
/*     CODE WRITTEN BY:  DAVID KINCAID, ROGER GRIMES, JOHN RESPESS        */
/*                       CENTER FOR NUMERICAL ANALYSIS                    */
/*                       UNIVERSITY OF TEXAS                              */
/*                       AUSTIN, TX  78712                                */
/*                       (512) 471-1242                                   */
/*                                                                        */
/*     FOR ADDITIONAL DETAILS ON THE                                      */
/*          (A) SUBROUTINE SEE TOMS ARTICLE 1982                          */
/*          (B) ALGORITHM  SEE CNA REPORT 150                             */
/*                                                                        */
/*     BASED ON THEORY BY:  DAVID YOUNG, DAVID KINCAID, LOU HAGEMAN       */
/*                                                                        */
/*     REFERENCE THE BOOK:  APPLIED ITERATIVE METHODS                     */
/*                          L. HAGEMAN, D. YOUNG                          */
/*                          ACADEMIC PRESS, 1981                          */
/*                                                                        */
/*     **************************************************                 */
/*     *               IMPORTANT NOTE                   *                 */
/*     *                                                *                 */
/*     *      WHEN INSTALLING ITPACK ROUTINES ON A      *                 */
/*     *  DIFFERENT COMPUTER, RESET SOME OF THE VALUES  *                 */
/*     *  IN  SUBROUTNE DFAULT.   MOST IMPORTANT ARE    *                 */
/*     *                                                *                 */
/*     *   DRELPR      MACHINE RELATIVE PRECISION       *                 */
/*     *   RPARM(1)    STOPPING CRITERION               *                 */
/*     *                                                *                 */
/*     *   ALSO CHANGE SYSTEM-DEPENDENT ROUTINE         *                 */
/*     *   SECOND USED IN TIMER                         *                 */
/*     *                                                *                 */
/*     **************************************************                 */
/*                                                                        */
/*     SPECIFICATIONS FOR ARGUMENTS                                       */
/*                                                                        */
/*     SPECIFICATIONS FOR LOCAL VARIABLES                                 */
/*                                                                        */
/* ... VARIABLES IN COMMON BLOCK - ITCOM1                                 */
/*                                                                        */
/*     IN     - ITERATION NUMBER                                          */
/*     IS     - ITERATION NUMBER WHEN PARAMETERS LAST CHANGED             */
/*     ISYM   - SYMMETRIC/NONSYMMETRIC STORAGE FORMAT SWITCH              */
/*     ITMAX  - MAXIMUM NUMBER OF ITERATIONS ALLOWED                      */
/*     LEVEL  - LEVEL OF OUTPUT CONTROL SWITCH                            */
/*     NOUT   - OUTPUT UNIT NUMBER                                        */
/*                                                                        */
/* ... VARIABLES IN COMMON BLOCK - ITCOM2                                 */
/*                                                                        */
/*     ADAPT  - FULLY ADAPTIVE PROCEDURE SWITCH                           */
/*     BETADT - SWITCH FOR ADAPTIVE DETERMINATION OF BETA                 */
/*     CASEII - ADAPTIVE PROCEDURE CASE SWITCH                            */
/*     HALT   - STOPPING TEST SWITCH                                      */
/*     PARTAD - PARTIALLY ADAPTIVE PROCEDURE SWITCH                       */
/*                                                                        */
/* ... VARIABLES IN COMMON BLOCK - ITCOM3                                 */
/*                                                                        */
/*     BDELNM - TWO NORM OF B TIMES DELTA-SUPER-N                         */
/*     BETAB  - ESTIMATE FOR THE SPECTRAL RADIUS OF LU MATRIX             */
/*     CME    - ESTIMATE OF LARGEST EIGENVALUE                            */
/*     DELNNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION N           */
/*     DELSNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION S           */
/*     FF     - ADAPTIVE PROCEDURE DAMPING FACTOR                         */
/*     GAMMA  - ACCELERATION PARAMETER                                    */
/*     OMEGA  - OVERRELAXATION PARAMETER FOR SOR AND SSOR                 */
/*     QA     - PSEUDO-RESIDUAL RATIO                                     */
/*     QT     - VIRTUAL SPECTRAL RADIUS                                   */
/*     RHO    - ACCELERATION PARAMETER                                    */
/*     RRR    - ADAPTIVE PARAMETER                                        */
/*     SIGE   - PARAMETER SIGMA-SUB-E                                     */
/*     SME    - ESTIMATE OF SMALLEST EIGENVALUE                           */
/*     SPECR  - SPECTRAL RADIUS ESTIMATE FOR SSOR                         */
/*     DRELPR - MACHINE RELATIVE PRECISION                                */
/*     STPTST - STOPPING PARAMETER                                        */
/*     UDNM   - TWO NORM OF U                                             */
/*     ZETA   - STOPPING CRITERION                                        */

    itcom1_1.level = iparm[1];
    itcom1_1.nout = iparm[3];
    ier = 0;
    if (iparm[0] <= 0)
        return 0;

    if (iparm[10] == 0)
        timj1 = timer_((real*)0);

    if (itcom1_1.level < 3)
        echout_(iparm, rparm, &c__6);
    else
        echall_(n, ia, ja, a, rhs, iparm, rparm, &c__1);
    temp = itcom3_1.drelpr * 500.;
    if (itcom3_1.zeta < temp)
        itcom3_1.zeta = temp;

    time1 = rparm[8];
    time2 = rparm[9];
    digit1 = rparm[10];
    digit2 = rparm[11];

    /* ... VERIFY N */

    if (*n <= 0) {
        ier = 61;
        goto L430;
    }

    /* ... REMOVE ROWS AND COLUMNS IF REQUESTED */

    if (iparm[9] != 0) {
        tol = rparm[7];
        ivfill_(n, iwksp, &c__0);
        vfill_(n, wksp, &c_b21);
        sbelm_(n, ia, ja, a, rhs, iwksp, wksp, &tol, &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
        if (ier != 0)
            goto L430;
    }

    /* ... INITIALIZE WKSP BASE ADDRESSES. */

    ib1 = 0;
    ib2 = ib1 + *n;
    jb3 = ib2 + *n;

    /* ... PERMUTE TO  RED-BLACK SYSTEM IF POSSIBLE */

    nb = iparm[8];
    if (nb < 0) {
        n3 = *n * 3;
        ivfill_(&n3, iwksp, &c__0);
        prbndx_(n, &nb, ia, ja, iwksp, &iwksp[ib2], &itcom1_1.level, &itcom1_1.nout, &ier);
        if (ier != 0)
            goto L430;
    }

    if (nb < 0 || nb > *n) {
        ier = 64;
        goto L430;
    }
    if (nb == 0 || nb == *n)
        nb = *n / 2;

    /* ... PERMUTE MATRIX AND RHS */

    if (iparm[8] < 0) {
        permat_(n, ia, ja, a, iwksp, &iwksp[jb3], &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
        if (ier != 0)
            goto L430;

        pervec_(n, rhs, iwksp);
        pervec_(n, u, iwksp);
    }

    /* ... FINISH WKSP BASE ADDRESSES */

    ib3 = ib2 + nb;
    ib4 = ib3 + nb;
    ib5 = ib4 + nb;
    nr = *n - nb;
    iparm[7] = *n + nb * 3 + (itcom1_1.itmax << 1);
    if (itcom1_1.isym != 0)
        iparm[7] += itcom1_1.itmax << 1;

    if (*nw < iparm[7]) {
        ier = 62;
        goto L430;
    }

    /* ... SCALE LINEAR SYSTEM, U, AND RHS BY THE SQUARE ROOT OF THE */
    /* ... DIAGONAL ELEMENTS. */

    vfill_(&iparm[7], wksp, &c_b21);
    scal_(n, ia, ja, a, rhs, u, wksp, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L430;

    if (iparm[10] == 0)
        timi1 = timer_((real*)0);

    /* ... INITIALIZE FORWARD PSEUDO-RESIDUAL */

    if (*n <= 1) {
        u[0] = rhs[0];
        goto L330;
    }
    itpackdcopy_(&nr, rhs, &c__1, &wksp[ib1], &c__1);
    prsred_(&nb, &nr, ia, ja, a, &u[nr], &wksp[ib1]);
    itpackdcopy_(&nb, &rhs[nr], &c__1, &wksp[ib2], &c__1);
    prsblk_(&nb, &nr, ia, ja, a, &wksp[ib1], &wksp[ib2]);
    vevmw_(&nb, &wksp[ib2], &u[nr]);

    /* ... ITERATION SEQUENCE */

    itmax1 = itcom1_1.itmax + 1;
    for (loop = 1; loop <= itmax1; ++loop) {
        itcom1_1.in = loop - 1;
        if (itcom1_1.in % 2 == 1)
            goto L290;

        /* ... CODE FOR THE EVEN ITERATIONS. */

        /*     U           = U(IN)       WKSP(IB2) = D(IN) */
        /*     WKSP(IB1)   = U(IN-1)     WKSP(IB3) = D(IN-1) */

        itrscg_(n, &nb, ia, ja, a, u, &wksp[ib1], &wksp[ib2], &wksp[ib3], &wksp[ib4], &wksp[ib5]);

        if (itcom2_1.halt)
            goto L330;

        continue;

        /* ... CODE FOR THE ODD ITERATIONS. */

        /*     U           = U(IN-1)     WKSP(IB2) = D(IN-1) */
        /*     WKSP(IB1)   = U(IN)       WKSP(IB3) = D(IN) */

L290:
        itrscg_(n, &nb, ia, ja, a, &wksp[ib1], u, &wksp[ib3], &wksp[ib2], &wksp[ib4], &wksp[ib5]);

        if (itcom2_1.halt)
            goto L330;
    }

    /* ... ITMAX HAS BEEN REACHED */

    if (iparm[10] == 0) {
        timi2 = timer_((real*)0);
        time1 = (doublereal) (timi2 - timi1);
    }
    ier = 63;
    if (iparm[2] == 0)
        rparm[0] = itcom3_1.stptst;

    goto L360;

    /* ... METHOD HAS CONVERGED */

L330:
    if (iparm[10] == 0) {
        timi2 = timer_((real*)0);
        time1 = (doublereal) (timi2 - timi1);
    }

    /* ... PUT SOLUTION INTO U IF NOT ALREADY THERE. */

L360:
    if (*n != 1) {
        if (itcom1_1.in % 2 == 1)
            itpackdcopy_(n, &wksp[ib1], &c__1, u, &c__1);

        itpackdcopy_(&nr, rhs, &c__1, u, &c__1);
        prsred_(&nb, &nr, ia, ja, a, &u[nr], u);
    }

    /* ... UNSCALE THE MATRIX, SOLUTION, AND RHS VECTORS. */

    unscal_(n, ia, ja, a, rhs, u, wksp);

    /* ... UN-PERMUTE MATRIX,RHS, AND SOLUTION */

    if (iparm[8] >= 0)
        goto L400;

    permat_(n, ia, ja, a, &iwksp[ib2], &iwksp[jb3], &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ierper);
    if (ierper != 0) {
        if (ier == 0)
            ier = ierper;

        goto L430;
    }

    pervec_(n, rhs, &iwksp[ib2]);
    pervec_(n, u, &iwksp[ib2]);

    /* ... OPTIONAL ERROR ANALYSIS */

L400:
    idgts = iparm[11];
    if (idgts >= 0) {
        if (iparm[1] <= 0)
            idgts = 0;

        perror_(n, ia, ja, a, rhs, u, wksp, &digit1, &digit2, &idgts);
    }

        /* ... SET RETURN PARAMETERS IN IPARM AND RPARM */

    if (iparm[10] == 0) {
        timj2 = timer_((real*)0);
        time2 = (doublereal) (timj2 - timj1);
    }
    iparm[7] -= (itcom1_1.itmax - itcom1_1.in) << 1;
    if (itcom1_1.isym != 0)
        iparm[7] -= (itcom1_1.itmax - itcom1_1.in) << 1;

    if (iparm[2] == 0) {
        iparm[0] = itcom1_1.in;
        iparm[8] = nb;
        rparm[1] = itcom3_1.cme;
        rparm[2] = itcom3_1.sme;
        rparm[8] = time1;
        rparm[9] = time2;
        rparm[10] = digit1;
        rparm[11] = digit2;
    }

L430:
    *ierr = ier;
    if (itcom1_1.level >= 3)
        echall_(n, ia, ja, a, rhs, iparm, rparm, &c__2);

    return 0;
} /* rscg_ */

/* Subroutine */
int rssi_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u,
          integer *iwksp, integer *nw, doublereal *wksp, integer *iparm, doublereal *rparm, integer* ierr)
{
    /* Local variables */
    static integer n3, nb, nr, ib1, ib2, jb3, ier;
    static doublereal tol;
    static doublereal temp;
    static integer loop;
    static doublereal time1, time2;
    static real timi1, timj1, timi2, timj2;
    static integer idgts;
    static doublereal digit1, digit2;
    static integer itmax1;
    static integer ierper;

/*     ITPACK 2C MAIN SUBROUTINE  RSSI  (REDUCED SYSTEM SEMI-ITERATIVE) */
/*     EACH OF THE MAIN SUBROUTINES:                                    */
/*           JCG, JSI, SOR, SSORCG, SSORSI, RSCG, RSSI                  */
/*     CAN BE USED INDEPENDENTLY OF THE OTHERS                          */

/*          THIS SUBROUTINE, RSSI, DRIVES THE  REDUCED SYSTEM SI          */
/*          ALGORITHM.                                                    */
/*                                                                        */
/* ... PARAMETER LIST:                                                    */
/*                                                                        */
/*          N     INPUT INTEGER.  DIMENSION OF THE MATRIX.                */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF      */
/*                 THE SPARSE MATRIX REPRESENTATION.                      */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE       */
/*                 MATRIX REPRESENTATION.                                 */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE       */
/*                 OF THE MATRIX PROBLEM.                                 */
/*          U      INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U CONTAINS THE    */
/*                 INITIAL GUESS TO THE SOLUTION. ON OUTPUT, IT CONTAINS  */
/*                 THE LATEST ESTIMATE TO THE SOLUTION.                   */
/*          IWKSP  INTEGER VECTOR WORKSPACE OF LENGTH 3*N                 */
/*          NW     INPUT INTEGER.  LENGTH OF AVAILABLE WKSP.  ON OUTPUT,  */
/*                 IPARM(8) IS AMOUNT USED.                               */
/*          WKSP   D.P. VECTOR USED FOR WORKING SPACE.  RSSI              */
/*                 NEEDS THIS TO BE IN LENGTH AT LEAST  N + NB            */
/*                 HERE NB IS THE ORDER OF THE BLACK SUBSYSTEM            */
/*          IPARM  INTEGER VECTOR OF LENGTH 12.  ALLOWS USER TO SPECIFY   */
/*                 SOME INTEGER PARAMETERS WHICH AFFECT THE METHOD.  IF   */
/*          RPARM  D.P. VECTOR OF LENGTH 12. ALLOWS USER TO SPECIFY SOME  */
/*                 D.P. PARAMETERS WHICH AFFECT THE METHOD.               */
/*          IER     OUTPUT INTEGER.  ERROR FLAG. (= IERR)                 */
/*                                                                        */
/* ... RSSI SUBPROGRAM REFERENCES:                                        */
/*                                                                        */
/*          FROM ITPACK    BISRCH, CHEBY, CHGSI, DFAULT, ECHALL,          */
/*                         ECHOUT, ITERM, TIMER, ITRSSI, IVFILL,          */
/*                         PARSI, PERMAT, PERROR, PERVEC, PMULT,          */
/*                         PRBNDX, PRSBLK, PRSRED, PSTOP, QSORT,          */
/*                         DAXPY, SBELM, SCAL, DCOPY, DDOT, SUM3,         */
/*                         TSTCHG, UNSCAL, VEVMW, VFILL, VOUT,            */
/*                         WEVMW                                          */
/*          SYSTEM         DABS, DLOG10, DBLE(AMAX0), DMAX1, DBLE(FLOAT), */
/*                         DSQRT                                          */
/*                                                                        */
/*     VERSION:  ITPACK 2C (MARCH 1982)                                   */
/*                                                                        */
/*     CODE WRITTEN BY:  DAVID KINCAID, ROGER GRIMES, JOHN RESPESS        */
/*                       CENTER FOR NUMERICAL ANALYSIS                    */
/*                       UNIVERSITY OF TEXAS                              */
/*                       AUSTIN, TX  78712                                */
/*                       (512) 471-1242                                   */
/*                                                                        */
/*     FOR ADDITIONAL DETAILS ON THE                                      */
/*          (A) SUBROUTINE SEE TOMS ARTICLE 1982                          */
/*          (B) ALGORITHM  SEE CNA REPORT 150                             */
/*                                                                        */
/*     BASED ON THEORY BY:  DAVID YOUNG, DAVID KINCAID, LOU HAGEMAN       */
/*                                                                        */
/*     REFERENCE THE BOOK:  APPLIED ITERATIVE METHODS                     */
/*                          L. HAGEMAN, D. YOUNG                          */
/*                          ACADEMIC PRESS, 1981                          */
/*                                                                        */
/*     **************************************************                 */
/*     *               IMPORTANT NOTE                   *                 */
/*     *                                                *                 */
/*     *      WHEN INSTALLING ITPACK ROUTINES ON A      *                 */
/*     *  DIFFERENT COMPUTER, RESET SOME OF THE VALUES  *                 */
/*     *  IN  SUBROUTNE DFAULT.   MOST IMPORTANT ARE    *                 */
/*     *                                                *                 */
/*     *   DRELPR      MACHINE RELATIVE PRECISION       *                 */
/*     *   RPARM(1)    STOPPING CRITERION               *                 */
/*     *                                                *                 */
/*     *   ALSO CHANGE SYSTEM-DEPENDENT ROUTINE         *                 */
/*     *   SECOND USED IN TIMER                         *                 */
/*     *                                                *                 */
/*     **************************************************                 */
/*                                                                        */
/*     SPECIFICATIONS FOR ARGUMENTS                                       */
/*                                                                        */
/*     SPECIFICATIONS FOR LOCAL VARIABLES                                 */
/*                                                                        */
/* ... VARIABLES IN COMMON BLOCK - ITCOM1                                 */
/*                                                                        */
/*     IN     - ITERATION NUMBER                                          */
/*     IS     - ITERATION NUMBER WHEN PARAMETERS LAST CHANGED             */
/*     ISYM   - SYMMETRIC/NONSYMMETRIC STORAGE FORMAT SWITCH              */
/*     ITMAX  - MAXIMUM NUMBER OF ITERATIONS ALLOWED                      */
/*     LEVEL  - LEVEL OF OUTPUT CONTROL SWITCH                            */
/*     NOUT   - OUTPUT UNIT NUMBER                                        */
/*                                                                        */
/* ... VARIABLES IN COMMON BLOCK - ITCOM2                                 */
/*                                                                        */
/*     ADAPT  - FULLY ADAPTIVE PROCEDURE SWITCH                           */
/*     BETADT - SWITCH FOR ADAPTIVE DETERMINATION OF BETA                 */
/*     CASEII - ADAPTIVE PROCEDURE CASE SWITCH                            */
/*     HALT   - STOPPING TEST SWITCH                                      */
/*     PARTAD - PARTIALLY ADAPTIVE PROCEDURE SWITCH                       */
/*                                                                        */
/* ... VARIABLES IN COMMON BLOCK - ITCOM3                                 */
/*                                                                        */
/*     BDELNM - TWO NORM OF B TIMES DELTA-SUPER-N                         */
/*     BETAB  - ESTIMATE FOR THE SPECTRAL RADIUS OF LU MATRIX             */
/*     CME    - ESTIMATE OF LARGEST EIGENVALUE                            */
/*     DELNNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION N           */
/*     DELSNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION S           */
/*     FF     - ADAPTIVE PROCEDURE DAMPING FACTOR                         */
/*     GAMMA  - ACCELERATION PARAMETER                                    */
/*     OMEGA  - OVERRELAXATION PARAMETER FOR SOR AND SSOR                 */
/*     QA     - PSEUDO-RESIDUAL RATIO                                     */
/*     QT     - VIRTUAL SPECTRAL RADIUS                                   */
/*     RHO    - ACCELERATION PARAMETER                                    */
/*     RRR    - ADAPTIVE PARAMETER                                        */
/*     SIGE   - PARAMETER SIGMA-SUB-E                                     */
/*     SME    - ESTIMATE OF SMALLEST EIGENVALUE                           */
/*     SPECR  - SPECTRAL RADIUS ESTIMATE FOR SSOR                         */
/*     DRELPR - MACHINE RELATIVE PRECISION                                */
/*     STPTST - STOPPING PARAMETER                                        */
/*     UDNM   - TWO NORM OF U                                             */
/*     ZETA   - STOPPING CRITERION                                        */

    itcom1_1.level = iparm[1];
    itcom1_1.nout = iparm[3];
    ier = 0;
    if (iparm[0] <= 0)
        return 0;

    if (iparm[10] == 0)
        timj1 = timer_((real*)0);

    if (itcom1_1.level < 3)
        echout_(iparm, rparm, &c__7);
    else
        echall_(n, ia, ja, a, rhs, iparm, rparm, &c__1);
    temp = itcom3_1.drelpr * 500.;
    if (itcom3_1.zeta < temp)
        itcom3_1.zeta = temp;

    time1 = rparm[8];
    time2 = rparm[9];
    digit1 = rparm[10];
    digit2 = rparm[11];

    /* ... VERIFY N */

    if (*n <= 0) {
        ier = 71;
        goto L420;
    }

    /* ... REMOVE ROWS AND COLUMNS IF REQUESTED */

    if (iparm[9] != 0) {
        tol = rparm[7];
        ivfill_(n, iwksp, &c__0);
        vfill_(n, wksp, &c_b21);
        sbelm_(n, ia, ja, a, rhs, iwksp, wksp, &tol, &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
    }

    /* ... INITIALIZE WKSP BASE ADDRESSES. */

    ib1 = 0;
    ib2 = ib1 + *n;
    jb3 = ib2 + *n;

    /* ... PERMUTE TO  RED-BLACK SYSTEM IF POSSIBLE */

    nb = iparm[8];
    if (nb < 0) {
        n3 = *n * 3;
        ivfill_(&n3, iwksp, &c__0);
        prbndx_(n, &nb, ia, ja, iwksp, &iwksp[ib2], &itcom1_1.level, &itcom1_1.nout, &ier);
        if (ier != 0)
            goto L420;
    }

    if (nb < 0 || nb > *n) {
        ier = 74;
        goto L420;
    }
    if (nb == 0 || nb == *n)
        nb = *n / 2;

    /* ... PERMUTE MATRIX AND RHS */

    if (iparm[8] < 0) {
        permat_(n, ia, ja, a, iwksp, &iwksp[jb3], &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
        if (ier != 0)
            goto L420;

        pervec_(n, rhs, iwksp);
        pervec_(n, u, iwksp);
    }

    /* ... INITIALIZE WKSP BASE ADDRESSES */

    nr = *n - nb;

    iparm[7] = *n + nb;
    if (*nw < iparm[7]) {
        ier = 72;
        goto L420;
    }

    /* ... SCALE LINEAR SYSTEM, U, AND RHS BY THE SQUARE ROOT OF THE */
    /* ... DIAGONAL ELEMENTS. */

    vfill_(&iparm[7], wksp, &c_b21);
    scal_(n, ia, ja, a, rhs, u, wksp, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier != 0)
        goto L420;

    if (iparm[10] == 0)
        timi1 = timer_((real*)0);

    /* ... ITERATION SEQUENCE */

    if (*n <= 1) {
        u[0] = rhs[0];
        goto L320;
    }
    itmax1 = itcom1_1.itmax + 1;
    for (loop = 1; loop <= itmax1; ++loop) {
        itcom1_1.in = loop - 1;
        if (itcom1_1.in % 2 == 1)
            goto L280;

        /* ... CODE FOR THE EVEN ITERATIONS. */

        /*     U           = U(IN) */
        /*     WKSP(IB1)   = U(IN-1) */

        itrssi_(n, &nb, ia, ja, a, rhs, u, &wksp[ib1], &wksp[ib2]);

        if (itcom2_1.halt)
            goto L320;

        continue;

        /* ... CODE FOR THE ODD ITERATIONS. */

        /*     U           = U(IN-1) */
        /*     WKSP(IB1)   = U(IN) */

L280:
        itrssi_(n, &nb, ia, ja, a, rhs, &wksp[ib1], u, &wksp[ib2]);

        if (itcom2_1.halt)
            goto L320;
    }

    /* ... ITMAX HAS BEEN REACHED */

    if (iparm[10] == 0) {
        timi2 = timer_((real*)0);
        time1 = (doublereal) (timi2 - timi1);
    }
    ier = 73;
    if (iparm[2] == 0)
        rparm[0] = itcom3_1.stptst;

    goto L350;

    /* ... METHOD HAS CONVERGED */

L320:
    if (iparm[10] == 0) {
        timi2 = timer_((real*)0);
        time1 = (doublereal) (timi2 - timi1);
    }

    /* ... PUT SOLUTION INTO U IF NOT ALREADY THERE. */

L350:
    if (*n != 1) {
        if (itcom1_1.in % 2 == 1)
            itpackdcopy_(n, &wksp[ib1], &c__1, u, &c__1);

        itpackdcopy_(&nr, rhs, &c__1, u, &c__1);
        prsred_(&nb, &nr, ia, ja, a, &u[nr], u);
    }

    /* ... UNSCALE THE MATRIX, SOLUTION, AND RHS VECTORS. */

    unscal_(n, ia, ja, a, rhs, u, wksp);

    /* ... UN-PERMUTE MATRIX,RHS, AND SOLUTION */

    if (iparm[8] >= 0)
        goto L390;

    permat_(n, ia, ja, a, &iwksp[ib2], &iwksp[jb3], &itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ierper);
    if (ierper != 0) {
        if (ier == 0)
            ier = ierper;

        goto L420;
    }

    pervec_(n, rhs, &iwksp[ib2]);
    pervec_(n, u, &iwksp[ib2]);

    /* ... OPTIONAL ERROR ANALYSIS */

L390:
    idgts = iparm[11];
    if (idgts >= 0) {
        if (iparm[1] <= 0)
            idgts = 0;

        perror_(n, ia, ja, a, rhs, u, wksp, &digit1, &digit2, &idgts);
    }

    /* ... SET RETURN PARAMETERS IN IPARM AND RPARM */

    if (iparm[10] == 0) {
        timj2 = timer_((real*)0);
        time2 = (doublereal) (timj2 - timj1);
    }
    if (iparm[2] == 0) {
        iparm[0] = itcom1_1.in;
        iparm[8] = nb;
        rparm[1] = itcom3_1.cme;
        rparm[2] = itcom3_1.sme;
        rparm[8] = time1;
        rparm[9] = time2;
        rparm[10] = digit1;
        rparm[11] = digit2;
    }

L420:
    *ierr = ier;
    if (itcom1_1.level >= 3)
        echall_(n, ia, ja, a, rhs, iparm, rparm, &c__2);

    return 0;
} /* rssi_ */

/* Subroutine */
int itjcg_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *u, doublereal *u1,
           doublereal *d, doublereal *d1, doublereal *dtwd, doublereal *tri)
{
    static doublereal c1, c2, c3, c4;
    static logical q1;
    static doublereal con;
    static doublereal dnrm;
    static doublereal dtnrm;
    static doublereal gamold;
    static doublereal rhoold;

/*          THIS SUBROUTINE, ITJCG, PERFORMS ONE ITERATION OF THE         */
/*          JACOBI CONJUGATE GRADIENT ALGORITHM.  IT IS CALLED BY JCG.    */
/*                                                                        */
/* ... PARAMETER LIST:                                                    */
/*                                                                        */
/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX.               */
/*          IA,JA  INPUT INTEGER VECTORS.  CONTAINS INFORMATION DEFINING  */
/*                 THE SPARSE MATRIX REPRESENTATION.                      */
/*          A      INPUT D.P. VECTOR. CONTAINS THE NONZERO VALUES OF THE  */
/*                 LINEAR SYSTEM.                                         */
/*          U      INPUT D.P. VECTOR.  CONTAINS THE VALUE OF THE          */
/*                 SOLUTION VECTOR AT THE END OF IN ITERATIONS.           */
/*          U1     INPUT/OUTPUT D.P. VECTOR.  ON INPUT, IT CONTAINS       */
/*                 THE VALUE OF THE SOLUTION AT THE END OF THE IN-1       */
/*                 ITERATION.  ON OUTPUT, IT WILL CONTAIN THE NEWEST      */
/*                 ESTIMATE FOR THE SOLUTION VECTOR.                      */
/*          D      INPUT D.P. VECTOR.  CONTAINS THE PSEUDO-RESIDUAL       */
/*                 VECTOR AFTER IN ITERATIONS.                            */
/*          D1     INPUT/OUTPUT D.P. VECTOR.  ON INPUT, D1 CONTAINS       */
/*                 THE PSEUDO-RESIDUAL VECTOR AFTER IN-1 ITERATIONS.  ON  */
/*                 OUTPUT, IT WILL CONTAIN THE NEWEST PSEUDO-RESIDUAL     */
/*                 VECTOR.                                                */
/*          DTWD   D.P. ARRAY.  USED IN THE COMPUTATIONS OF THE           */
/*                 ACCELERATION PARAMETER GAMMA AND THE NEW PSEUDO-       */
/*                 RESIDUAL.                                              */
/*          TRI    D.P. ARRAY.  STORES THE TRIDIAGONAL MATRIX ASSOCIATED  */
/*                 WITH THE EIGENVALUES OF THE CONJUGATE GRADIENT         */
/*                 POLYNOMIAL.                                            */

    /* ... COMPUTE NEW ESTIMATE FOR CME IF ADAPT = .TRUE. */

    if (itcom2_1.adapt)
        chgcon_(tri, &gamold, &rhoold, &c__1);

    /* ... TEST FOR STOPPING */

    itcom3_1.delnnm = itpackddot_(n, d, &c__1, d, &c__1);
    dnrm = itcom3_1.delnnm;
    con = itcom3_1.cme;
    pstop_(n, u, &dnrm, &con, &c__1, &q1);
    if (itcom2_1.halt)
        goto L30;

    /* ... COMPUTE RHO AND GAMMA - ACCELERATION PARAMETERS */

    vfill_(n, dtwd, &c_b21);
    pjac_(n, ia, ja, a, d, dtwd);
    dtnrm = itpackddot_(n, d, &c__1, dtwd, &c__1);
    if (itcom1_1.isym != 0)
        rhoold = itpackddot_(n, dtwd, &c__1, d1, &c__1);

    parcon_(&dtnrm, &c1, &c2, &c3, &c4, &gamold, &rhoold, &c__1);

    /* ... COMPUTE U(IN+1) AND D(IN+1) */

    sum3_(n, &c1, d, &c2, u, &c3, u1);
    sum3_(n, &c1, dtwd, &c4, d, &c3, d1);

    /* ... OUTPUT INTERMEDIATE INFORMATION */

L30:
    iterm_(n, a, u, dtwd, &c__1);

    return 0;
} /* itjcg_ */

/* Subroutine */
int itjsi_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *rhs,
           doublereal *u, doublereal *u1, doublereal *d, integer *icnt)
{
    static doublereal c1, c2, c3;
    static logical q1;
    static doublereal con;
    static doublereal dnrm;
    static doublereal dtnrm;
    static doublereal oldnrm;

/*          THIS SUBROUTINE, ITJSI, PERFORMS ONE ITERATION OF THE         */
/*          JACOBI SEMI-ITERATIVE ALGORITHM.  IT IS CALLED BY JSI.        */
/*                                                                        */
/* ... PARAMETER LIST:                                                    */
/*                                                                        */
/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX.               */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF      */
/*                 THE SPARSE MATRIX REPRESENTATION.                      */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE       */
/*                 MATRIX REPRESENTATION.                                 */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE       */
/*                 OF THE MATRIX PROBLEM.                                 */
/*          U      INPUT D.P. VECTOR.  CONTAINS THE ESTIMATE FOR THE      */
/*                 SOLUTION VECTOR AFTER IN ITERATIONS.                   */
/*          U1     INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U1 CONTAINS THE   */
/*                 SOLUTION VECTOR AFTER IN-1 ITERATIONS.  ON OUTPUT,     */
/*                 IT WILL CONTAIN THE NEWEST ESTIMATE FOR THE SOLUTION   */
/*                 VECTOR.                                                */
/*          D      D.P. ARRAY.  D IS USED FOR THE COMPUTATION OF THE      */
/*                 PSEUDO-RESIDUAL ARRAY FOR THE CURRENT ITERATION.       */
/*          ICNT   NUMBER OF ITERATIONS SINCE LAST CHANGE OF SME          */

    if (itcom1_1.in == 0)
        *icnt = 0;

    /* ... COMPUTE PSEUDO-RESIDUALS */

    itpackdcopy_(n, rhs, &c__1, d, &c__1);
    pjac_(n, ia, ja, a, u, d);
    vevmw_(n, d, u);

    /* ... STOPPING AND ADAPTIVE CHANGE TESTS */

    oldnrm = itcom3_1.delnnm;
    itcom3_1.delnnm = itpackddot_(n, d, &c__1, d, &c__1);
    dnrm = itcom3_1.delnnm;
    con = itcom3_1.cme;
    pstop_(n, u, &dnrm, &con, &c__1, &q1);
    if (itcom2_1.halt)
        goto L40;

    if (! itcom2_1.adapt)
        goto L30;

    if (! tstchg_(&c__1))
        goto L10;

    /* ... CHANGE ITERATIVE PARAMETERS (CME) */

    dtnrm = pvtbv_(n, ia, ja, a, d);
    chgsi_(&dtnrm, &c__1);
    if (! itcom2_1.adapt)
        goto L30;

    goto L20;

    /* ... TEST IF SME NEEDS TO BE CHANGED AND CHANGE IF NECESSARY. */

L10:
    if (itcom2_1.caseii)
        goto L30;

    if (! chgsme_(&oldnrm, icnt))
        goto L30;

    *icnt = 0;

    /* ... COMPUTE U(IN+1) AFTER CHANGE OF PARAMETERS */

L20:
    itpackdcopy_(n, u, &c__1, u1, &c__1);
    itpackdaxpy_(n, &itcom3_1.gamma, d, &c__1, u1, &c__1);
    goto L40;

    /* ... COMPUTE U(IN+1) WITHOUT CHANGE OF PARAMETERS */

L30:
    parsi_(&c1, &c2, &c3, &c__1);
    sum3_(n, &c1, d, &c2, u, &c3, u1);

    /* ... OUTPUT INTERMEDIATE INFORMATION */

L40:
    iterm_(n, a, u, d, &c__2);

    return 0;
} /* itjsi_ */

/* Subroutine */
int itsor_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, doublereal *wk)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal h;
    static logical q1;
    static integer ip;
    static integer iss;
    static doublereal dnrm;
    static integer iphat;
    static doublereal spcrm1;
    static logical change;
    static doublereal omegap;
    static integer ipstar;

/*          THIS SUBROUTINE, ITSOR, PERFORMS ONE ITERATION OF THE         */
/*          SUCCESSIVE OVERRELAXATION ALGORITHM.  IT IS CALLED BY SOR.    */
/*                                                                        */
/* ... PARAMETER LIST:                                                    */
/*                                                                        */
/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX.               */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF      */
/*                 THE SPARSE MATRIX REPRESENTATION.                      */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE       */
/*                 MATRIX REPRESENTATION.                                 */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE       */
/*                 OF THE MATRIX PROBLEM.                                 */
/*          U      INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U CONTAINS THE    */
/*                 SOLUTION VECTOR AFTER IN ITERATIONS.  ON OUTPUT,       */
/*                 IT WILL CONTAIN THE NEWEST ESTIMATE FOR THE SOLUTION   */
/*                 VECTOR.                                                */
/*          WK     D.P. ARRAY.  WORK VECTOR OF LENGTH N.                  */

    /* ... SET INITIAL PARAMETERS NOT ALREADY SET */

    if (itcom1_1.in != 0)
        goto L20;

    pstop_(n, u, &c_b21, &c_b21, &c__0, &q1);
    if (! itcom2_1.adapt) {
        change = FALSE_;
        ip = 0;
        iphat = 2;
        iss = 0;
        goto L30;
    }

    change = TRUE_;
    ip = 0;
    omegap = itcom3_1.omega;
    itcom3_1.omega = 1.;
    iss = 0;
    iphat = 2;
    ipstar = 4;
    if (omegap <= 1.)
        change = FALSE_;

    /* ... RESET OMEGA, IPHAT, AND IPSTAR (CIRCLE A IN FLOWCHART) */

L20:
    if (change) {
        change = FALSE_;
        ++itcom1_1.is;
        ip = 0;
        iss = 0;
        itcom3_1.omega = min(omegap,tau_(&itcom1_1.is));
        iphat = max(3, (integer)((itcom3_1.omega-1.)/(2.-itcom3_1.omega)));
        ipstar = ipstr_(&itcom3_1.omega);
    }

    /* ... COMPUTE U (IN + 1) AND NORM OF DEL(S,P) - CIRCLE B IN FLOW CHART */

L30:
    itcom3_1.delsnm = itcom3_1.delnnm;
    spcrm1 = itcom3_1.specr;
    itpackdcopy_(n, rhs, &c__1, wk, &c__1);
    pfsor1_(n, ia, ja, a, u, wk);
    if (itcom3_1.delnnm == 0.)
        goto L40;

    if (itcom1_1.in != 0)
        itcom3_1.specr = itcom3_1.delnnm / itcom3_1.delsnm;

    if (ip < iphat)
        goto L70;

    /* ... STOPPING TEST, SET H */

    if (itcom3_1.specr >= 1.)
        goto L70;

    if (itcom3_1.specr > itcom3_1.omega - 1.) {
        h = itcom3_1.specr;
        goto L50;
    }
L40:
    ++iss;
    h = itcom3_1.omega - 1.;

    /* ... PERFORM STOPPING TEST. */

L50:
    dnrm = itcom3_1.delnnm * itcom3_1.delnnm;
    pstop_(n, u, &dnrm, &h, &c__1, &q1);
    if (itcom2_1.halt)
        goto L70;

    /* ... METHOD HAS NOT CONVERGED YET, TEST FOR CHANGING OMEGA */

    if (! itcom2_1.adapt)
        goto L70;

    if (ip < ipstar)
        goto L70;

    if (itcom3_1.omega <= 1.) {
        itcom3_1.cme = sqrt((abs(itcom3_1.specr)));
        omegap = 2. / (sqrt(abs(1. - itcom3_1.specr)) + 1.);
        change = TRUE_;
        goto L70;
    }

    if (iss != 0)
        goto L70;

    d__1 = itcom3_1.omega - 1.;
    if (itcom3_1.specr <= pow_dd(&d__1, &itcom3_1.ff))
        goto L70;

    /* ... CHANGE PARAMETERS */

    if (itcom3_1.specr + 5e-5 > spcrm1) {
        itcom3_1.cme = (itcom3_1.specr + itcom3_1.omega - 1.) /
                       (sqrt((abs(itcom3_1.specr))) * itcom3_1.omega);
        omegap = 2. / (sqrt(abs(1. - itcom3_1.cme * itcom3_1.cme)) + 1.);
        change = TRUE_;
    }

    /* ... OUTPUT INTERMEDIATE INFORMATION */

L70:
    iterm_(n, a, u, wk, &c__3);
    ++ip;

    return 0;
} /* itsor_ */

/* Subroutine */
int itsrcg_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *rhs,
            doublereal *u, doublereal *u1, doublereal *c, doublereal *c1,
            doublereal *d, doublereal *dl, doublereal *wk, doublereal *tri)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static logical q1;
    static doublereal t1, t2, t3, t4, con;
    static doublereal dnrm;
    static doublereal gamold;
    static doublereal betnew, rhoold;

/*          THIS SUBROUTINE, ITSRCG, PERFORMS ONE ITERATION OF THE        */
/*          SYMMETRIC SOR CONJUGATE GRADIENT ALGORITHM.  IT IS CALLED BY  */
/*          SSORCG.                                                       */
/*                                                                        */
/* ... PARAMETER LIST:                                                    */
/*                                                                        */
/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX.               */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF      */
/*                 THE SPARSE MATRIX REPRESENTATION.                      */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE       */
/*                 MATRIX REPRESENTATION.                                 */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE       */
/*                 OF THE MATRIX PROBLEM.                                 */
/*          U      INPUT D.P. VECTOR.  CONTAINS THE ESTIMATE OF THE       */
/*                 SOLUTION VECTOR AFTER IN ITERATIONS.                   */
/*          U1     INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U1 CONTAINS THE   */
/*                 THE ESTIMATE FOR THE SOLUTION AFTER IN-1 ITERATIONS.   */
/*                 ON OUTPUT, U1 CONTAINS THE UPDATED ESTIMATE.           */
/*          C      INPUT D.P. VECTOR.  CONTAINS THE FORWARD RESIDUAL      */
/*                 AFTER IN ITERATIONS.                                   */
/*          C1     INPUT/OUTPUT D.P. VECTOR.  ON INPUT, C1 CONTAINS       */
/*                 THE FORWARD RESIDUAL AFTER IN-1 ITERATIONS.  ON        */
/*                 OUTPUT, C1 CONTAINS THE UPDATED FORWARD RESIDUAL.      */
/*          D      D.P. VECTOR.  IS USED TO COMPUTE THE BACKWARD PSEUDO-  */
/*                 RESIDUAL VECTOR FOR THE CURRENT ITERATION.             */
/*          DL     D.P. VECTOR.  IS USED IN THE COMPUTATIONS OF THE       */
/*                 ACCELERATION PARAMETERS.                               */
/*          WK     D.P. VECTOR.  WORKING SPACE OF LENGTH N.               */
/*          TRI    D.P. VECTOR. STORES THE TRIDIAGONAL MATRIX ASSOCIATED  */
/*                 WITH THE CONJUGATE GRADIENT ACCELERATION.              */

    /* ... CALCULATE S-PRIME FOR ADAPTIVE PROCEDURE. */

    if (itcom2_1.adapt || itcom2_1.partad)
        chgcon_(tri, &gamold, &rhoold, &c__3);

    /* ... COMPUTE BACKWARD RESIDUAL */

    itpackdcopy_(n, rhs, &c__1, wk, &c__1);
    itpackdcopy_(n, c, &c__1, d, &c__1);
    vevpw_(n, d, u);
    pbsor_(n, ia, ja, a, d, wk);
    vevmw_(n, d, u);

    /* ... COMPUTE ACCELERATION PARAMETERS AND THEN U(IN+1) (IN U1) */

    itpackdcopy_(n, d, &c__1, dl, &c__1);
    vfill_(n, wk, &c_b21);
    pfsor_(n, ia, ja, a, dl, wk);
    wevmw_(n, d, dl);
    itcom3_1.delnnm = itpackddot_(n, c, &c__1, c, &c__1);
    if (itcom3_1.delnnm != 0.) {
        dnrm = itpackddot_(n, c, &c__1, dl, &c__1);
        if (dnrm != 0.) {
            if (itcom1_1.isym != 0)
                rhoold = itpackddot_(n, c, &c__1, c1, &c__1) - itpackddot_(n, dl, &c__1, c1, &c__1);

            parcon_(&dnrm, &t1, &t2, &t3, &t4, &gamold, &rhoold, &c__3);
            sum3_(n, &t1, d, &t2, u, &t3, u1);
        }
    }

    /* ... TEST FOR STOPPING */

    itcom3_1.bdelnm = itpackddot_(n, d, &c__1, d, &c__1);
    dnrm = itcom3_1.bdelnm;
    con = itcom3_1.specr;
    pstop_(n, u, &dnrm, &con, &c__1, &q1);
    if (itcom2_1.halt)
        goto L100;

    /* ... IF NON- OR PARTIALLY-ADAPTIVE, COMPUTE C(IN+1) AND EXIT. */

    if (! itcom2_1.adapt) {
        d__1 = -t1;
        sum3_(n, &d__1, dl, &t2, c, &t3, c1);
        goto L100;
    }

    /* ... FULLY ADAPTIVE PROCEDURE */

    if (omgstr_(&c__1))
        goto L90;

    /* ... PARAMETERS HAVE BEEN UNCHANGED.  COMPUTE C(IN+1) AND EXIT. */

    if (! omgchg_(&c__1)) {
        d__1 = -t1;
        sum3_(n, &d__1, dl, &t2, c, &t3, c1);
        goto L100;
    }

    /* ... IT HAS BEEN DECIDED TO CHANGE PARAMETERS */
    /*        (1) COMPUTE NEW BETAB IF BETADT = .TRUE. */

    if (itcom2_1.betadt) {
        betnew = pbeta_(n, ia, ja, a, d, wk, c1) / itcom3_1.bdelnm;
        itcom3_1.betab = max(max(itcom3_1.betab,.25),betnew);
    }

    /* ...    (2) COMPUTE NEW CME, OMEGA, AND SPECR */

    if (! itcom2_1.caseii) {
        dnrm = pvtbv_(n, ia, ja, a, d);
        goto L80;
    }
    vfill_(n, wk, &c_b21);
    pjac_(n, ia, ja, a, d, wk);
    dnrm = itpackddot_(n, wk, &c__1, wk, &c__1);
L80:
    omeg_(&dnrm, &c__3);

    /* ...    (3) COMPUTE NEW FORWARD RESIDUAL SINCE OMEGA HAS BEEN CHANGED. */

L90:
    itpackdcopy_(n, rhs, &c__1, wk, &c__1);
    itpackdcopy_(n, u1, &c__1, c1, &c__1);
    pfsor_(n, ia, ja, a, c1, wk);
    vevmw_(n, c1, u1);

    /* ... OUTPUT INTERMEDIATE RESULTS. */

L100:
    iterm_(n, a, u, wk, &c__4);

    return 0;
} /* itsrcg_ */

/* Subroutine */
int itsrsi_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u,
            doublereal *u1, doublereal *c, doublereal *d, doublereal *ctwd, doublereal *wk)
{
    /* Local variables */
    static doublereal c1, c2, c3;
    static logical q1;
    static doublereal con;
    static doublereal dnrm;
    static doublereal betnew;

/*          THIS SUBROUTINE, ITSRSI, PERFORMS ONE ITERATION OF THE        */
/*          SYMMETRIC SOR SEMI-ITERATION ALGORITHM.  IT IS CALLED BY      */
/*          SSORSI.                                                       */
/*                                                                        */
/* ... PARAMETER LIST:                                                    */
/*                                                                        */
/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX.               */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF      */
/*                 THE SPARSE MATRIX REPRESENTATION.                      */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE       */
/*                 MATRIX REPRESENTATION.                                 */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE       */
/*                 OF THE MATRIX PROBLEM.                                 */
/*          U      INPUT D.P. VECTOR.  CONTAINS THE ESTIMATE OF THE       */
/*                 SOLUTION VECTOR AFTER IN ITERATIONS.                   */
/*          U1     INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U1 CONTAINS THE   */
/*                 THE ESTIMATE FOR THE SOLUTION AFTER IN-1 ITERATIONS.   */
/*                 ON OUTPUT, U1 CONTAINS THE UPDATED ESTIMATE.           */
/*          C      D.P. VECTOR.  IS USED TO COMPUTE THE FORWARD PSEUDO-   */
/*                 RESIDUAL VECTOR FOR THE CURRENT ITERATION.             */
/*          D      D.P. VECTOR.  IS USED TO COMPUTE THE BACKWARD PSEUDO-  */
/*                 RESIDUAL VECTOR FOR THE CURRENT ITERATION.             */
/*          CTWD   D.P. VECTOR.  IS USED IN THE COMPUTATIONS OF THE       */
/*                 ACCELERATION PARAMETERS.                               */
/*          WK     D.P. VECTOR.  WORKING SPACE OF LENGTH N.               */

    /* ... COMPUTE PSEUDO-RESIDUALS (FORWARD AND BACKWARD) */

    itpackdcopy_(n, rhs, &c__1, wk, &c__1);
    itpackdcopy_(n, u, &c__1, ctwd, &c__1);
    pssor1_(n, ia, ja, a, ctwd, wk, c, d);

    /* ... COMPUTE U(IN+1) -- CONTAINED IN THE VECTOR U1. */

    parsi_(&c1, &c2, &c3, &c__3);
    sum3_(n, &c1, d, &c2, u, &c3, u1);

    /* ... TEST FOR STOPPING */

    itcom3_1.bdelnm = itpackddot_(n, d, &c__1, d, &c__1);
    dnrm = itcom3_1.bdelnm;
    con = itcom3_1.specr;
    pstop_(n, u, &dnrm, &con, &c__1, &q1);
    if (itcom2_1.halt || ! (itcom2_1.adapt || itcom2_1.partad))
        goto L40;

    /* ... ADAPTIVE PROCEDURE */

    if (omgstr_(&c__1))
        goto L40;

    itcom3_1.delnnm = itpackddot_(n, c, &c__1, c, &c__1);
    if (itcom1_1.in == itcom1_1.is)
        itcom3_1.delsnm = itcom3_1.delnnm;

    if (itcom1_1.in == 0 || ! tstchg_(&c__1))
        goto L40;

    /* ... IT HAS BEEN DECIDED TO CHANGE PARAMETERS. */
    /* ...    (1) COMPUTE CTWD */

    itpackdcopy_(n, d, &c__1, ctwd, &c__1);
    vfill_(n, wk, &c_b21);
    pfsor_(n, ia, ja, a, ctwd, wk);
    vevpw_(n, ctwd, c);
    vevmw_(n, ctwd, d);

    /* ...    (2) COMPUTE NEW SPECTRAL RADIUS FOR CURRENT OMEGA. */

    dnrm = itpackddot_(n, c, &c__1, ctwd, &c__1);
    chgsi_(&dnrm, &c__3);
    if (! itcom2_1.adapt)
        goto L40;

    /* ...    (3) COMPUTE NEW BETAB IF BETADT = .TRUE. */

    if (itcom2_1.betadt) {
        betnew = pbeta_(n, ia, ja, a, d, wk, ctwd) / itcom3_1.bdelnm;
        itcom3_1.betab = max(max(itcom3_1.betab,.25),betnew);
    }

    /* ...    (4) COMPUTE NEW CME, OMEGA, AND SPECR. */

    if (! itcom2_1.caseii) {
        dnrm = pvtbv_(n, ia, ja, a, d);
        goto L30;
    }
    vfill_(n, wk, &c_b21);
    pjac_(n, ia, ja, a, d, wk);
    dnrm = itpackddot_(n, wk, &c__1, wk, &c__1);
L30:
    omeg_(&dnrm, &c__3);

    /* ... OUTPUT INTERMEDIATE INFORMATION */

L40:
    iterm_(n, a, u, wk, &c__5);

    return 0;
} /* itsrsi_ */

/* Subroutine */
int itrscg_(integer *n, integer *nb, integer *ia, integer *ja, doublereal *a, doublereal *ub,
            doublereal *ub1, doublereal *db, doublereal *db1, doublereal *wb, doublereal *tri)
{
    static doublereal c1, c2, c3, c4;
    static logical q1;
    static integer nr;
    static doublereal con;
    static doublereal dnrm;
    static doublereal gamold;
    static doublereal rhoold;

/*          THIS SUBROUTINE, ITRSCG, PERFORMS ONE ITERATION OF THE        */
/*          REDUCED SYSTEM CONJUGATE GRADIENT ALGORITHM.  IT IS           */
/*          CALLED BY RSCG.                                               */
/*                                                                        */
/* ... PARAMETER LIST:                                                    */
/*                                                                        */
/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX.               */
/*          NB     INPUT INTEGER.  CONTAINS THE NUMBER OF BLACK POINTS    */
/*                 IN THE RED-BLACK MATRIX.                               */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF      */
/*                 THE SPARSE MATRIX REPRESENTATION.                      */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE       */
/*                 MATRIX REPRESENTATION.                                 */
/*          UB     INPUT D.P. VECTOR.  CONTAINS THE ESTIMATE FOR THE      */
/*                 SOLUTION ON THE BLACK POINTS AFTER IN ITERATIONS.      */
/*          UB1    INPUT/OUTPUT D.P. VECTOR.  ON INPUT, UB1 CONTAINS THE  */
/*                 SOLUTION VECTOR AFTER IN-1 ITERATIONS.  ON OUTPUT,     */
/*                 IT WILL CONTAIN THE NEWEST ESTIMATE FOR THE SOLUTION   */
/*                 VECTOR.  THIS IS ONLY FOR THE BLACK POINTS.            */
/*          DB     INPUT D.P. ARRAY.  DB CONTAINS THE VALUE OF THE        */
/*                 CURRENT PSEUDO-RESIDUAL ON THE BLACK POINTS.           */
/*          DB1    INPUT/OUTPUT D.P. ARRAY.  DB1 CONTAINS THE PSEUDO-     */
/*                 RESIDUAL ON THE BLACK POINTS FOR THE IN-1 ITERATION    */
/*                 ON INPUT.  ON OUTPUT, IT IS FOR THE IN+1 ITERATION.    */
/*          WB     D.P. ARRAY.  WB IS USED FOR COMPUTATIONS INVOLVING     */
/*                 BLACK VECTORS.                                         */
/*          TRI    D.P. ARRAY.  STORES THE TRIDIAGONAL MATRIX ASSOCIATED  */
/*                 WITH CONJUGATE GRADIENT ACCELERATION.                  */

    /* ... COMPUTE NEW ESTIMATE FOR CME IF ADAPT = .TRUE. */

    nr = *n - *nb;
    if (itcom2_1.adapt)
        chgcon_(tri, &gamold, &rhoold, &c__2);

    /* ... TEST FOR STOPPING */

    itcom3_1.delnnm = itpackddot_(nb, db, &c__1, db, &c__1);
    dnrm = itcom3_1.delnnm;
    con = itcom3_1.cme;
    pstop_(nb, &ub[nr], &dnrm, &con, &c__2, &q1);
    if (itcom2_1.halt)
        goto L30;

    /* ... COMPUTE ACCELERATION PARAMETERS */

    vfill_(&nr, ub1, &c_b21);
    prsred_(nb, &nr, ia, ja, a, db, ub1);
    vfill_(nb, wb, &c_b21);
    prsblk_(nb, &nr, ia, ja, a, ub1, wb);
    dnrm = itpackddot_(nb, db, &c__1, wb, &c__1);
    if (itcom1_1.isym != 0)
        rhoold = itpackddot_(nb, wb, &c__1, db1, &c__1);

    parcon_(&dnrm, &c1, &c2, &c3, &c4, &gamold, &rhoold, &c__2);

    /* ... COMPUTE UB(IN+1) AND DB(IN+1) */

    sum3_(nb, &c1, db, &c2, &ub[nr], &c3, &ub1[nr]);
    sum3_(nb, &c1, wb, &c4, db, &c3, db1);

    /* ... OUTPUT INTERMEDIATE INFORMATION */

L30:
    iterm_(nb, &a[nr], &ub[nr], wb, &c__6);

    return 0;
} /* itrscg_ */

/* Subroutine */
int itrssi_(integer *n, integer *nb, integer *ia, integer *ja, doublereal *a,
            doublereal *rhs, doublereal *ub, doublereal *ub1, doublereal *db)
{
    static doublereal c1, c2, c3;
    static logical q1;
    static integer nr;
    static doublereal dnrm;
    static doublereal cnst;

/*          THIS SUBROUTINE, ITRSSI, PERFORMS ONE ITERATION OF THE        */
/*          REDUCED SYSTEM SEMI-ITERATION ALGORITHM.  IT IS               */
/*          CALLED BY RSSI.                                               */
/*                                                                        */
/* ... PARAMETER LIST:                                                    */
/*                                                                        */
/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX.               */
/*          NB     INPUT INTEGER.  CONTAINS THE NUMBER OF BLACK POINTS    */
/*                 IN THE RED-BLACK MATRIX.                               */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF      */
/*                 THE SPARSE MATRIX REPRESENTATION.                      */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE       */
/*                 MATRIX REPRESENTATION.                                 */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE       */
/*                 OF THE MATRIX PROBLEM.                                 */
/*          UB     INPUT D.P. VECTOR.  CONTAINS THE ESTIMATE FOR THE      */
/*                 SOLUTION ON THE BLACK POINTS AFTER IN ITERATIONS.      */
/*          UB1    INPUT/OUTPUT D.P. VECTOR.  ON INPUT, UB1 CONTAINS THE  */
/*                 SOLUTION VECTOR AFTER IN-1 ITERATIONS.  ON OUTPUT,     */
/*                 IT WILL CONTAIN THE NEWEST ESTIMATE FOR THE SOLUTION   */
/*                 VECTOR.  THIS IS ONLY FOR THE BLACK POINTS.            */
/*          DB     INPUT D.P. ARRAY.  DB CONTAINS THE VALUE OF THE        */
/*                 CURRENT PSEUDO-RESIDUAL ON THE BLACK POINTS.           */

    /* ... COMPUTE UR(IN) INTO UB */

    nr = *n - *nb;
    itpackdcopy_(&nr, rhs, &c__1, ub, &c__1);
    prsred_(nb, &nr, ia, ja, a, &ub[nr], ub);

    /* ... COMPUTE PSEUDO-RESIDUAL, DB(IN) */

    itpackdcopy_(nb, &rhs[nr], &c__1, db, &c__1);
    prsblk_(nb, &nr, ia, ja, a, ub, db);
    vevmw_(nb, db, &ub[nr]);

    /* ... TEST FOR STOPPING */

    itcom3_1.delnnm = itpackddot_(nb, db, &c__1, db, &c__1);
    dnrm = itcom3_1.delnnm;
    cnst = itcom3_1.cme;
    pstop_(nb, &ub[nr], &dnrm, &cnst, &c__2, &q1);
    if (itcom2_1.halt)
        goto L20;

    if (! itcom2_1.adapt)
        goto L10;

    /* ... TEST TO CHANGE PARAMETERS */

    if (! tstchg_(&c__2))
        goto L10;

    /* ... CHANGE PARAMETERS */

    vfill_(&nr, ub1, &c_b21);
    prsred_(nb, &nr, ia, ja, a, db, ub1);
    dnrm = itpackddot_(&nr, ub1, &c__1, ub1, &c__1);
    chgsi_(&dnrm, &c__2);
    if (itcom2_1.adapt) { /* ... COMPUTE UB(N+1) AFTER CHANGING PARAMETERS */
        itpackdcopy_(nb, &ub[nr], &c__1, &ub1[nr], &c__1);
        itpackdaxpy_(nb, &itcom3_1.gamma, db, &c__1, &ub1[nr], &c__1);
        goto L20;
    }
    /* ... COMPUTE UB(N+1) WITHOUT CHANGE OF PARAMETERS */
L10:
    parsi_(&c1, &c2, &c3, &c__2);
    sum3_(nb, &c1, db, &c2, &ub[nr], &c3, &ub1[nr]);

    /* ... OUTPUT INTERMEDIATE INFORMATION */

L20:
    iterm_(nb, &a[nr], &ub[nr], db, &c__7);

    return 0;
} /* itrssi_ */

integer bisrch_(integer *n, integer *k, integer *l)
{
    /* Local variables */
    static integer jmid, jleft, jright;

/* ... BISRCH IS AN INTEGER FUNCTION WHICH USES A BISECTION SEARCH */
/*     TO FIND THE ENTRY J IN THE ARRAY K SUCH THAT THE VALUE L IS */
/*     GREATER THAN OR EQUAL TO K(J) AND STRICTLY LESS THAN K(J+1). */

/* ... PARAMETER LIST: */

/*          N      INTEGER LENGTH OF VECTOR K */
/*          K      INTEGER VECTOR */
/*          L      INTEGER CONSTANT SUCH THAT  K(J) .GE. L .LT. K(J+1) */
/*                 WITH J RETURNED AS VALUE OF INTEGER FUNCTION BISRCH */

    if (*n == 2)
        return 1;

    jleft = 1;
    jright = *n;
    jmid = (*n + 1) / 2;

L10:
    if (*l >= k[jmid-1]) /* ...... L .GE. K(LEFT)  AND  L .LT. K(JMID) */
        jleft = jmid;
    else                 /* ...... L .GE. K(JMID)  AND  L .LT. K(JRIGHT) */
        jright = jmid;

    /* ...... TEST FOR CONVERGENCE */

    if (jright - jleft == 1) /* ...... BISECTION SEARCH FINISHED */
        return jleft;

    jmid = jleft + (jright - jleft + 1) / 2;
    goto L10;
} /* bisrch_ */

doublereal cheby_(doublereal *qa, doublereal *qt, doublereal *rrr, integer *
                  ip, doublereal *cme, doublereal *sme)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal x, y, z;

/*     COMPUTES THE SOLUTION TO THE CHEBYSHEV EQUATION */

/* ... PARAMETER LIST: */

/*          QA     RATIO OF PSEUDO-RESIDUALS */
/*          QT     VIRTUAL SPECTRAL RADIUS */
/*          RRR    ADAPTIVE PARAMETER */
/*          IP     NUMBER OF ITERATIONS SINCE LAST CHANGE OF */
/*                     PARAMETERS */
/*          CME,   ESTIMATES FOR THE LARGEST AND SMALLEST EIGEN- */
/*          SME      VALUES OF THE ITERATION MATRIX */

    z = (*qa + sqrt(abs(*qa * *qa - *qt * *qt))) * .5 * (pow_di(rrr, ip) + 1.);
    d__1 = 1. / (doublereal) ((real) (*ip));
    x = pow_dd(&z, &d__1);
    y = (x + *rrr / x) / (*rrr + 1.);

    return (*cme + *sme + y * (2. - *cme - *sme)) * .5;
} /* cheby_ */

/* Subroutine */
int chgcon_(doublereal *tri, doublereal *gamold, doublereal *rhoold, integer *ibmth)
{
    /* Local variables */
    static integer ip, ib3;
    static doublereal end;
    static integer ier;
    static doublereal cmold, start;

/*     COMPUTES THE NEW ESTIMATE FOR THE LARGEST EIGENVALUE FOR */
/*     CONJUGATE GRADIENT ACCELERATION. */

/* ... PARAMETER LIST: */

/*          TRI    TRIDIAGONAL MATRIX ASSOCIATED WITH THE EIGENVALUES */
/*                    OF THE CONJUGATE GRADIENT POLYNOMIAL */
/*          GAMOLD */
/*            AND */
/*          RHOOLD PREVIOUS VALUES OF ACCELERATION PARAMETERS */
/*          IBMTH  INDICATOR OF BASIC METHOD BEING ACCELERATED BY CG */
/*                      IBMTH = 1,  JACOBI */
/*                            = 2,  REDUCED SYSTEM */
/*                            = 3,  SSOR */

    switch (*ibmth) {
        case 1:  goto L10;
        case 2:  goto L20;
        case 3:  goto L30;
    }

    /* ... JACOBI CONJUGATE GRADIENT */

L10:
    start = itcom3_1.cme;
    ip = itcom1_1.in;
    goto L40;

    /* ... REDUCED SYSTEM CG */

L20:
    start = itcom3_1.cme * itcom3_1.cme;
    ip = itcom1_1.in;
    goto L40;

    /* ... SSOR CG */

L30:
    if (itcom2_1.adapt)
        start = itcom3_1.spr;

    if (! itcom2_1.adapt)
        start = itcom3_1.specr;

    ip = itcom1_1.in - itcom1_1.is;

    /* ... DEFINE THE MATRIX */

L40:
    if (ip >= 2)
        goto L60;

    if (ip != 1) { /* ... IP = 0 */
        end = 0.;
        cmold = 0.;
    }
    else {         /* ... IP = 1 */
        end = 1. - 1. / itcom3_1.gamma;
        tri[0] = end;
        tri[1] = 0.;
    }
    goto L110;

    /* ... IP > 1 */

L60:
    if (ip > 2 && abs(start - cmold) <= itcom3_1.zeta * start)
        goto L120;
    cmold = start;

    /* ... COMPUTE THE LARGEST EIGENVALUE */

    tri[(ip << 1) - 2] = 1. - 1. / itcom3_1.gamma;
    tri[(ip << 1) - 1] = (itcom3_1.rho - 1.) / (itcom3_1.rho * *rhoold * itcom3_1.gamma * *gamold);
    if (itcom1_1.isym == 0)
        end = eigvss_(&ip, tri, &start, &itcom3_1.zeta, &itcom1_1.itmax, &ier);

    else {
        ib3 = ip + ip / 2 + 1;
        end = eigvns_(&ip, tri, &tri[ip << 1], &tri[ib3 << 1], &ier);
    }

    if (ier != 0)
        goto L130;

    /* ... SET SPECTRAL RADIUS FOR THE VARIOUS METHODS */

L110:
    if (*ibmth == 1)
        itcom3_1.cme = end;

    if (*ibmth == 2)
        itcom3_1.cme = sqrt((abs(end)));

    if (*ibmth == 3 && itcom2_1.adapt)
        itcom3_1.spr = end;

    if (*ibmth == 3 && ! itcom2_1.adapt)
        itcom3_1.specr = end;

    return 0;

    /* ... RELATIVE CHANGE IN CME IS LESS THAN ZETA.  THEREFORE STOP */
    /*     CHANGING. */

L120:
    itcom2_1.adapt = FALSE_;
    itcom2_1.partad = FALSE_;
    return 0;

    /* ... ESTIMATE FOR CME > 1.D0.  THEREFORE NEED TO STOP ADAPTIVE */
    /*     PROCEDURE AND KEEP OLD VALUE OF CME. */

L130:
    itcom2_1.adapt = FALSE_;
    itcom2_1.partad = FALSE_;

    return 0;
} /* chgcon_ */

/* Subroutine */
int chgsi_(doublereal *dtnrm, integer *ibmth)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static doublereal zm1, zm2;
    static doublereal cmold;

/* ... COMPUTES NEW CHEBYSHEV ACCELERATION PARAMETERS ADAPTIVELY. */

/* ... PARAMETER LIST: */

/*          DTNRM  NUMERATOR OF RAYLEIGH QUOTIENT */
/*          IBMTH  INDICATOR OF BASIC METHOD BEING ACCELERATED BY SI */
/*                      IBMTH = 1,   JACOBI */
/*                            = 2,   REDUCED SYSTEM */
/*                            = 3,   SYMMETRIC SOR */

    switch (*ibmth) {
        case 1:  goto L10;
        case 2:  goto L30;
        case 3:  goto L50;
    }

    /*     --------------------- */
    /* ... JACOBI SEMI-ITERATIVE */
    /*     --------------------- */

    /* ... CHEBYSHEV EQUATION */

L10:
    if (itcom1_1.in == 0)
        zm1 = itcom3_1.cme;

    if (itcom1_1.in != 0) {
        i__1 = itcom1_1.in - itcom1_1.is;
        zm1 = cheby_(&itcom3_1.qa, &itcom3_1.qt, &itcom3_1.rrr, &i__1, &itcom3_1.cme, &itcom3_1.sme);
    }

    /* ... RAYLEIGH QUOTIENT */

    zm2 = *dtnrm / itcom3_1.delnnm;

    /* ... COMPUTATION OF ITERATIVE PARAMETERS */

    cmold = itcom3_1.cme;
    itcom3_1.cme = max(max(zm1,zm2),cmold);
    if (itcom3_1.cme >= 1.)
        goto L20;

    if (itcom2_1.caseii)
        itcom3_1.sme = -itcom3_1.cme;

    itcom3_1.sige = (itcom3_1.cme - itcom3_1.sme) / (2. - itcom3_1.cme - itcom3_1.sme);
    itcom3_1.gamma = 2. / (2. - itcom3_1.cme - itcom3_1.sme);
    itcom3_1.rrr = (1. - sqrt(abs(1. - itcom3_1.sige * itcom3_1.sige))) /
                   (sqrt(abs(1. - itcom3_1.sige * itcom3_1.sige)) + 1.);
    itcom1_1.is = itcom1_1.in;
    itcom3_1.delsnm = itcom3_1.delnnm;
    itcom3_1.rho = 1.;
    return 0;

    /* ... ADAPTIVE PROCEDURE FAILED FOR JACOBI SI */

L20:
    itcom3_1.cme = cmold;
    itcom2_1.adapt = FALSE_;
    return 0;

    /*     ----------------------------- */
    /* ... REDUCED SYSTEM SEMI-ITERATIVE */
    /*     ----------------------------- */

    /* ... CHEBYSHEV EQUATION */

L30:
    if (itcom1_1.in == 0)
        zm1 = itcom3_1.cme;

    if (itcom1_1.in != 0) {
        i__1 = (itcom1_1.in - itcom1_1.is) << 1;
        zm1 = cheby_(&itcom3_1.qa, &itcom3_1.qt, &itcom3_1.rrr, &i__1, &c_b21, &c_b21);
    }

    /* ... RAYLEIGH QUOTIENT */

    zm2 = sqrt(abs(*dtnrm / itcom3_1.delnnm));

    /* ... COMPUTATION OF NEW ITERATIVE PARAMETERS */

    cmold = itcom3_1.cme;
    itcom3_1.cme = max(max(zm1,zm2),cmold);
    if (itcom3_1.cme >= 1.) {
        /* ... ADAPTIVE PROCEDURE FAILED FOR REDUCED SYSTEM SI */
        itcom3_1.cme = cmold;
        itcom2_1.adapt = FALSE_;
        return 0;
    }
    itcom3_1.sige = itcom3_1.cme * itcom3_1.cme / (2. - itcom3_1.cme * itcom3_1.cme);
    itcom3_1.gamma = 2. / (2. - itcom3_1.cme * itcom3_1.cme);
    itcom3_1.rrr = (1. - sqrt(abs(1. - itcom3_1.cme * itcom3_1.cme))) /
                   (sqrt(abs(1. - itcom3_1.cme * itcom3_1.cme)) + 1.);
    itcom1_1.is = itcom1_1.in;
    itcom3_1.delsnm = itcom3_1.delnnm;
    itcom3_1.rho = 1.;
    return 0;

    /*     ----------------------------- */
    /* ... SYMMETRIC SOR SEMI-ITERATIVE */
    /*     ---------------------------- */

L50:
    if (itcom3_1.specr == 0.)
        itcom3_1.specr = .171572875;

    if (itcom1_1.in != 0) {
        i__1 = itcom1_1.in - itcom1_1.is;
        zm1 = cheby_(&itcom3_1.qa, &itcom3_1.qt, &itcom3_1.rrr, &i__1, &itcom3_1.specr, &c_b21);
    }
    else {
        zm1 = itcom3_1.specr;
        itcom3_1.spr = itcom3_1.specr;
    }

    /* ... RAYLEIGH QUOTIENT */

    zm2 = *dtnrm / itcom3_1.delnnm;

    /* ... COMPUTATION OF NEW ESTIMATE FOR SPECTRAL RADIUS */

    /* ... PARTIALLY ADAPTIVE SSOR SI */

    if (! itcom2_1.adapt) {
        itcom3_1.specr = max(max(zm1,zm2),itcom3_1.specr);
        itcom1_1.is = itcom1_1.in + 1;
        itcom3_1.delsnm = itcom3_1.delnnm;
        return 0;
    }

    /* ... FULLY ADAPTIVE SSOR SI */

    itcom3_1.spr = max(max(zm1,zm2),itcom3_1.spr);
    return 0;
} /* chgsi_ */

logical chgsme_(doublereal *oldnrm, integer *icnt)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Local variables */
    static doublereal q, z;
    static integer ip;
    static doublereal rn, wp, sm1, sm2;

/* ... THIS FUNCTION TESTS FOR JACOBI SI WHETHER SME SHOULD BE CHANGED */
/* ... WHEN CASEII = .FALSE..  IF THE TEST IS POSITIVE THE NEW VALUE */
/* ... OF SME IS COMPUTED. */

/* ... PARAMETER LIST: */

/*          OLDNRM SQUARE OF THE NORM OF THE PSEUDO-RESIDUAL */
/*                    AT THE LAST ITERATION */
/*          ICNT   NUMBER OF ITERATIONS SINCE LAST CHANGE OF */
/*                    PARAMETERS */

    rn = sqrt(itcom3_1.delnnm / *oldnrm);
    if (! (itcom3_1.qa > 1. && rn > 1.))
        return FALSE_;

    if (itcom1_1.in <= itcom1_1.is + 2)
        return FALSE_;

    ++(*icnt);
    if (*icnt < 3)
        return FALSE_;

    /* ... CHANGE SME IN J-SI ADAPTIVE PROCEDURE */

    sm1 = 0.;
    sm2 = 0.;
    if (itcom3_1.sme >= itcom3_1.cme)
        goto L10;

    /* ... COMPUTE SM1 */

    ip = itcom1_1.in - itcom1_1.is;
    q = itcom3_1.qa * (pow_di(&itcom3_1.rrr, &ip) + 1.) / (sqrt(pow_di(&itcom3_1.rrr, &ip)) * 2.);
    d__1 = q + sqrt(q * q - 1.);
    d__2 = 1. / (doublereal) ((real) ip);
    z = pow_dd(&d__1, &d__2);
    wp = (z * z + 1.) / (z * 2.);
    sm1 = (itcom3_1.cme + itcom3_1.sme - wp * (itcom3_1.cme - itcom3_1.sme)) * .5;

    /* ... COMPUTE SM2 */

    i__1 = ip - 1;
    q = rn * (pow_di(&itcom3_1.rrr, &ip) + 1.) / ((pow_di(&itcom3_1.rrr, &i__1) + 1.) * sqrt(itcom3_1.rrr));
    wp = (q * q + 1.) / (q * 2.);
    sm2 = (itcom3_1.cme + itcom3_1.sme - wp * (itcom3_1.cme - itcom3_1.sme)) *
       .5;

L10:
    itcom3_1.sme = min(min(min(sm1 * 1.25,sm2 * 1.25),itcom3_1.sme),-1.);
    itcom3_1.sige = (itcom3_1.cme - itcom3_1.sme) / (2. - itcom3_1.cme - itcom3_1.sme);
    itcom3_1.gamma = 2. / (2. - itcom3_1.cme - itcom3_1.sme);
    itcom3_1.rrr = (1. - sqrt(1. - itcom3_1.sige * itcom3_1.sige)) /
                   (sqrt(1. - itcom3_1.sige * itcom3_1.sige) + 1.);
    itcom1_1.is = itcom1_1.in;
    itcom3_1.delsnm = itcom3_1.delnnm;
    itcom3_1.rho = 1.;

    return TRUE_;
} /* chgsme_ */

/* Subroutine */
int itpackdaxpy_(integer *n, doublereal *da, doublereal *dx, integer *incx, doublereal *dy, integer *incy)
{
    /* Local variables */
    static integer i, m, ix, iy, ns;

    /*     OVERWRITE DOUBLE PRECISION DY WITH DOUBLE PRECISION DA*DX + DY. */

    if (*n <= 0 || *da == 0.)
        return 0;

    if (*incx == *incy) {
        if (*incx < 1)
            goto L10;
        else if (*incx == 1)
            goto L30;
        else
            goto L70;
    }
L10:

    /*        CODE FOR NONEQUAL OR NONPOSITIVE INCREMENTS. */

    ix = 0;
    iy = 0;
    if (*incx < 0)
        ix = (-(*n) + 1) * *incx;

    if (*incy < 0)
        iy = (-(*n) + 1) * *incy;

    for (i = 0; i < *n; ++i) {
        dy[iy] += *da * dx[ix];
        ix += *incx;
        iy += *incy;
    }
    return 0;

    /*        CODE FOR BOTH INCREMENTS EQUAL TO 1 */

    /*        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 4. */

L30:
    m = *n - (*n / 4 << 2);
    for (i = 0; i < m; ++i)
        dy[i] += *da * dx[i];

    for (i = m; i < *n; i += 4) {
        dy[i] += *da * dx[i];
        dy[i + 1] += *da * dx[i + 1];
        dy[i + 2] += *da * dx[i + 2];
        dy[i + 3] += *da * dx[i + 3];
    }
    return 0;

    /*        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS. */

L70:
    ns = *n * *incx;
    for (i = 0; *incx < 0 ? i >= ns-1 : i < ns; i += *incx)
        dy[i] = *da * dx[i] + dy[i];

    return 0;
} /* itpackdaxpy_ */

/* Subroutine */
int itpackdcopy_(integer *n, doublereal *dx, integer *incx, doublereal *dy, integer *incy)
{
    /* Local variables */
    static integer i, m, ix, iy, ns;

    /*     COPY DOUBLE PRECISION DX TO DOUBLE PRECISION DY. */

    if (*n <= 0)
        return 0;

    if (*incx == *incy) {
        if (*incx < 1)
            goto L10;
        else if (*incx == 1)
            goto L30;
        else
            goto L70;
    }
L10:

    /*        CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS. */

    ix = 0;
    iy = 0;
    if (*incx < 0)
        ix = (-(*n) + 1) * *incx;

    if (*incy < 0)
        iy = (-(*n) + 1) * *incy;

    for (i = 0; i < *n; ++i) {
        dy[iy] = dx[ix];
        ix += *incx;
        iy += *incy;
    }
    return 0;

    /*        CODE FOR BOTH INCREMENTS EQUAL TO 1 */

    /*        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 7. */

L30:
    m = *n - *n / 7 * 7;
    for (i = 0; i < m; ++i)
        dy[i] = dx[i];

    for (i = m; i < *n; i += 7) {
        dy[i] = dx[i];
        dy[i + 1] = dx[i + 1];
        dy[i + 2] = dx[i + 2];
        dy[i + 3] = dx[i + 3];
        dy[i + 4] = dx[i + 4];
        dy[i + 5] = dx[i + 5];
        dy[i + 6] = dx[i + 6];
    }
    return 0;

    /*        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS. */

L70:
    ns = *n * *incx;
    for (i = 0; *incx < 0 ? i >= ns-1 : i < ns; i += *incx)
        dy[i] = dx[i];

    return 0;
} /* itpackdcopy_ */

doublereal itpackddot_(integer *n, doublereal *dx, integer *incx, doublereal *dy, integer *incy)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static integer i, m, ix, iy, ns;

    /*     RETURNS THE DOT PRODUCT OF DOUBLE PRECISION DX AND DY. */

    ret_val = 0.;
    if (*n <= 0)
        return 0.;

    if (*incx == *incy) {
        if (*incx < 1)
            goto L10;
        else if (*incx == 1)
            goto L30;
        else
            goto L70;
    }
L10:

    /*         CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS. */

    ix = 0;
    iy = 0;
    if (*incx < 0)
        ix = (-(*n) + 1) * *incx;

    if (*incy < 0)
        iy = (-(*n) + 1) * *incy;

    for (i = 0; i < *n; ++i) {
        ret_val += dx[ix] * dy[iy];
        ix += *incx;
        iy += *incy;
    }
    return ret_val;

    /*        CODE FOR BOTH INCREMENTS EQUAL TO 1. */

    /*        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5. */

L30:
    m = *n - *n / 5 * 5;
    for (i = 0; i < m; ++i)
        ret_val += dx[i] * dy[i];

    for (i = m; i < *n; i += 5)
        ret_val += dx[i]*dy[i] + dx[i+1]*dy[i+1] + dx[i+2]*dy[i+2] + dx[i+3]*dy[i+3] + dx[i+4]*dy[i+4];
    return ret_val;

    /*         CODE FOR POSITIVE EQUAL INCREMENTS .NE.1. */

L70:
    ns = *n * *incx;
    for (i = 0; *incx < 0 ? i >= ns-1 : i < ns; i += *incx)
        ret_val += dx[i] * dy[i];

    return ret_val;
} /* itpackddot_ */

doublereal determ_(integer *n, doublereal *tri, doublereal *xlmda)
{
    /* Local variables */
    static integer l;
    static doublereal d1, d2, d3;
    static integer icnt;

/*     THIS SUBROUTINE COMPUTES THE DETERMINANT OF A SYMMETRIC */
/*     TRIDIAGONAL MATRIX GIVEN BY TRI. DET(TRI - XLMDA*I) = 0 */

/* ... PARAMETER LIST */

/*          N      ORDER OF TRIDIAGONAL SYSTEM */
/*          TRI    SYMMETRIC TRIDIAGONAL MATRIX OF ORDER N */
/*          XLMDA  ARGUMENT FOR CHARACTERISTIC EQUATION */

    d2 = tri[(*n << 1) - 2] - *xlmda;
    d1 = d2 * (tri[(*n << 1) - 4] - *xlmda) - tri[(*n << 1) - 1];
    if (*n == 2)
        return d1;

    for (icnt = 2; icnt < *n; ++icnt) {
        l = *n - icnt + 1;
        d3 = d2;
        d2 = d1;
        d1 = (tri[((l - 1) << 1) - 2] - *xlmda) * d2 - d3 * tri[(l << 1) - 1];
    }

    return d1;
} /* determ_ */

/* Subroutine */
int dfault_(integer *iparm, doublereal *rparm)
{
/* ... THIS SUBROUTINE SETS THE DEFAULT VALUES OF IPARM AND RPARM. */

/* ... PARAMETER LIST: */

/*          IPARM */
/*           AND */
/*          RPARM  ARRAYS SPECIFYING OPTIONS AND TOLERANCES */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN MAIN SUBROUTINE */

/*     DRELPR  - COMPUTER PRECISION (APPROX.) */
/*     IF INSTALLER OF PACKAGE DOES NOT KNOW DRELPR VALUE, */
/*     AN APPROXIMATE VALUE CAN BE DETERMINED FROM A SIMPLE */
/*     FORTRAN PROGRAM SUCH AS */

/*     DOUBLE PRECISION DRELPR, TEMP */
/*     DRELPR = 1.0D0 */
/*   2 DRELPR = 0.5D0*DRELPR */
/*     TEMP = DRELPR + 1.0D0 */
/*     IF(TEMP .GT. 1.0D0)  GO TO 2 */
/*     WRITE(6,3) DRELPR */
/*   3 FORMAT(5X,D15.8) */
/*     STOP */
/*     END */

/*     SOME VALUES ARE: */

/*     DRELPR = 1.26D-29  FOR CDC CYBER 170/750  (APPROX.) 2**-96 */
/*            = 2.22D-16  FOR DEC 10             (APPROX.) 2**-52 */
/*            = 7.11D-15  FOR VAX 11/780         (APPROX.) 2**-47 */
/*            = 1.14D-13  FOR IBM 370/158        (APPROX.) 2**-43 */

/*             *** SHOULD BE CHANGED FOR OTHER MACHINES *** */

/*     TO FACILITATE CONVERGENCE, RPARM(1) SHOULD BE SET TO */
/*          500.*DRELPR OR LARGER */

    itcom3_1.drelpr = 7.11e-15;

    iparm[0] = 100;
    iparm[1] = 0;
    iparm[2] = 0;
    iparm[3] = 6;
    iparm[4] = 0;
    iparm[5] = 1;
    iparm[6] = 1;
    iparm[7] = 0;
    iparm[8] = -1;
    iparm[9] = 0;
    iparm[10] = 0;
    iparm[11] = 0;

    rparm[0] = 5e-6;
    rparm[1] = 0.;
    rparm[2] = 0.;
    rparm[3] = .75;
    rparm[4] = 1.;
    rparm[5] = 0.;
    rparm[6] = .25;
    rparm[7] = itcom3_1.drelpr * 100.;
    rparm[8] = 0.;
    rparm[9] = 0.;
    rparm[10] = 0.;
    rparm[11] = 0.;

    return 0;
} /* dfault_ */

/* Subroutine */
int echall_(integer *nn, integer *ia, integer *ja, doublereal *a, doublereal *rhs,
            integer *iparm, doublereal *rparm, integer *icall)
{
  (void)nn; (void)ia; (void)ja; (void)a; (void)rhs;
/* ... THIS ROUTINE INITIALIZES THE ITPACK COMMON BLOCKS FROM THE */
/* ... INFORMATION CONTAINED IN IPARM AND RPARM. ECHALL ALSO PRINTS THE */
/* ... VALUES OF ALL THE PARAMETERS IN IPARM AND RPARM. */

/* ... PARAMETER LIST: */

/*          IPARM */
/*           AND */
/*          RPARM  ARRAYS OF PARAMETERS SPECIFYING OPTIONS AND */
/*                    TOLERANCES */
/*          ICALL  INDICATOR OF WHICH PARAMETERS ARE BEING PRINTED */
/*                    ICALL = 1,  INITIAL PARAMETERS */
/*                    ICALL = 2,  FINAL PARAMETERS */

    if (*icall != 1)
        return 0;

    /* ... INITIALIZE ITPACK COMMON */

    itcom3_1.zeta = rparm[0];
    itcom3_1.cme = rparm[1];
    itcom3_1.sme = rparm[2];
    itcom3_1.ff = rparm[3];
    itcom3_1.omega = rparm[4];
    itcom3_1.specr = rparm[5];
    itcom3_1.betab = rparm[6];
    itcom1_1.itmax = iparm[0];
    itcom1_1.level = iparm[1];
    itcom1_1.isym = iparm[4];

    itcom2_1.adapt = FALSE_;
    itcom2_1.partad = FALSE_;
    itcom2_1.betadt = FALSE_;
    if (iparm[5] == 1 || iparm[5] == 3)
        itcom2_1.adapt = TRUE_;

    if (iparm[5] == 1)
        itcom2_1.betadt = TRUE_;

    if (iparm[5] == 2)
        itcom2_1.partad = TRUE_;

    itcom2_1.caseii = FALSE_;
    if (iparm[6] == 2)
        itcom2_1.caseii = TRUE_;

    if (itcom2_1.caseii)
        itcom3_1.sme = -itcom3_1.cme;

    if (! itcom2_1.caseii && itcom3_1.sme == 0.)
        itcom3_1.sme = -1.;

    itcom3_1.spr = itcom3_1.sme;

    /* ... SET REST OF COMMON VARIABLES TO ZERO */

    itcom1_1.in = 0;
    itcom1_1.is = 0;
    itcom2_1.halt = FALSE_;
    itcom3_1.bdelnm = 0.;
    itcom3_1.delnnm = 0.;
    itcom3_1.delsnm = 0.;
    itcom3_1.gamma = 0.;
    itcom3_1.qa = 0.;
    itcom3_1.qt = 0.;
    itcom3_1.rho = 0.;
    itcom3_1.rrr = 0.;
    itcom3_1.sige = 0.;
    itcom3_1.stptst = 0.;
    itcom3_1.udnm = 0.;

    if (itcom1_1.level <= 4)
        return 0;

    /*     THIS SECTION OF ECHALL CAUSES PRINTING OF THE LINEAR SYSTEM AND */
    /*     THE ITERATIVE PARAMETERS */

    return 0;
} /* echall_ */

/* Subroutine */
int echout_(integer *iparm, doublereal *rparm, integer *imthd)
{
/*     THIS ROUTINE INITIALIZES THE ITPACK COMMON BLOCKS FROM THE */
/*     INFORMATION CONTAINED IN IPARM AND RPARM. */

/* ... PARAMETER LIST: */

/*          IPARM */
/*           AND */
/*          RPARM  ARRAYS OF PARAMETERS SPECIFYING OPTIONS AND */
/*                    TOLERANCES */
/*          IMTHD  INDICATOR OF METHOD */
/*                    IMTHD = 1,  JCG */
/*                    IMTHD = 2,  JSI */
/*                    IMTHD = 3,  SOR */
/*                    IMTHD = 4,  SSORCG */
/*                    IMTHD = 5,  SSORSI */
/*                    IMTHD = 6,  RSCG */
/*                    IMTHD = 7,  RSSI */

    itcom3_1.zeta = rparm[0];
    itcom3_1.cme = rparm[1];
    itcom3_1.sme = rparm[2];
    itcom3_1.ff = rparm[3];
    itcom3_1.omega = rparm[4];
    itcom3_1.specr = rparm[5];
    itcom3_1.betab = rparm[6];
    itcom1_1.itmax = iparm[0];
    itcom1_1.level = iparm[1];
    itcom1_1.isym = iparm[4];

    itcom2_1.adapt = FALSE_;
    itcom2_1.partad = FALSE_;
    itcom2_1.betadt = FALSE_;
    if (iparm[5] == 1 || iparm[5] == 3)
        itcom2_1.adapt = TRUE_;

    if (iparm[5] == 1)
        itcom2_1.betadt = TRUE_;

    if (iparm[5] == 2)
        itcom2_1.partad = TRUE_;

    itcom2_1.caseii = FALSE_;
    if (iparm[6] == 2)
        itcom2_1.caseii = TRUE_;

    if (itcom2_1.caseii)
        itcom3_1.sme = -itcom3_1.cme;

    if (! itcom2_1.caseii && itcom3_1.sme == 0.)
        itcom3_1.sme = -1.;

    itcom3_1.spr = itcom3_1.sme;

    /* ... SET REST OF COMMON VARIABLES TO ZERO */

    itcom1_1.in = 0;
    itcom1_1.is = 0;
    itcom2_1.halt = FALSE_;
    itcom3_1.bdelnm = 0.;
    itcom3_1.delnnm = 0.;
    itcom3_1.delsnm = 0.;
    itcom3_1.gamma = 0.;
    itcom3_1.qa = 0.;
    itcom3_1.qt = 0.;
    itcom3_1.rho = 0.;
    itcom3_1.rrr = 0.;
    itcom3_1.sige = 0.;
    itcom3_1.stptst = 0.;
    itcom3_1.udnm = 0.;
    if (itcom1_1.level <= 2)
        return 0;

    /* ... THIS SECTION OF ECHOUT ECHOES THE INPUT VALUES FOR THE INITIAL */
    /*     ITERATIVE PARAMETERS */

    switch (*imthd) {
        case 1:  goto L80;
        case 2:  goto L20;
        case 3:  goto L100;
        case 4:  goto L60;
        case 5:  goto L40;
        case 6:  goto L80;
        case 7:  goto L20;
    }

    /* ... JSI, RSSI */

L20:
    return 0;

    /* ... SSORSI */

L40:
    return 0;

    /* ... SSORCG */

L60:
    return 0;

    /* ... JCG, RSCG */

L80:
    if (itcom2_1.adapt)
        return 0;

L100:
    return 0;
} /* echout_ */

doublereal eigvns_(integer *n, doublereal *tri, doublereal *d, doublereal *e2, integer *ier)
{
    /* Local variables */
    static integer i;

/*     COMPUTES THE LARGEST EIGENVALUE OF A SYMMETRIC TRIDIAGONAL MATRIX */
/*     FOR CONJUGATE GRADIENT ACCELERATION. */

/* ... PARAMETER LIST: */

/*          N      ORDER OF TRIDIAGONAL SYSTEM */
/*          TRI    SYMMETRIC TRIDIAGONAL MATRIX OF ORDER N */
/*          D      ARRAY FOR EQRT1S(NEGATIVE DIAGONAL ELEMENTS) */
/*          E2     ARRAY FOR EQRT1S (SUPER DIAGONAL ELEMENTS) */
/*          IER    ERROR FLAG: ON RETURN, IER=0 INDICATES THAT */
/*                    THE LARGEST EIGENVALUE OF TRI WAS FOUND. */

    d[0] = -tri[0];
    for (i = 1; i < *n; ++i) {
        d[i] = -tri[i << 1];
        e2[i] = abs(tri[(i << 1) + 1]);
    }

    eqrt1s_(d, e2, n, &c__1, &c__0, ier);
    return -d[0];
} /* eigvns_ */

doublereal eigvss_(integer *n, doublereal *tri, doublereal *start, doublereal *zeta, integer *itmax, integer *ier)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal a, b, eps;
    static integer nsig, itmp, maxfn;

/*     COMPUTES THE LARGEST EIGENVALUE OF A SYMMETRIC TRIDIAGONAL MATRIX */
/*     FOR CONJUGATE GRADIENT ACCELERATION. */
/*     MODIFIED IMSL ROUTINE ZBRENT USED. */

/* ... PARAMETER LIST: */

/*          N      ORDER OF TRIDIAGONAL SYSTEM */
/*          TRI    SYMMETRIC TRIDIAGONAL MATRIX OF ORDER N */
/*          START  INITIAL LOWER BOUND OF INTERVAL CONTAINING ROOT */
/*          ZETA   STOPPING CRITERIA */
/*          IER    ERROR FLAG: ON RETURN, IER=0 INDICATES THAT */
/*                    THE LARGEST EIGENVALUE OF TRI WAS FOUND. */

    d__1 = abs(*zeta);
    itmp = (integer) ((real) (-d_lg10(&d__1)));
    nsig = max(itmp,4);
    maxfn = max(*itmax,50);

    /*     EPS = DMIN1(ZETA,0.5D-4) */

    eps = 0.;
    a = *start;
    b = 1.;
    zbrent_(n, tri, &eps, &nsig, &a, &b, &maxfn, ier);
    return b;
} /* eigvss_ */

/* Subroutine */
int eqrt1s_(doublereal *d, doublereal *e2, integer *n, integer *m, integer *isw, integer *ierr)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal f;
    static integer i, j, k;
    static doublereal p, q, r, s;
    static integer ii, jj;
    static doublereal ep, qp;
    static integer ier;
    static doublereal err, tot, dlam, delta;

/*   MODIFIED IMSL ROUTINE NAME   - EQRT1S */

/* ----------------------------------------------------------------------- */

/*   COMPUTER            - CDC/SINGLE */

/*   LATEST REVISION     - JUNE 1, 1980 */

/*   PURPOSE             - SMALLEST OR LARGEST M EIGENVALUES OF A */
/*                           SYMMETRIC TRIDIAGONAL MATRIX */

/*   USAGE               - CALL EQRT1S (D,E2,N,M,ISW,IER) */

/*   ARGUMENTS    D      - INPUT VECTOR OF LENGTH N CONTAINING */
/*                           THE DIAGONAL ELEMENTS OF THE MATRIX.  THE */
/*                           COMPUTED EIGENVALUES REPLACE THE FIRST M */
/*                           COMPONENTS OF THE VECTOR D IN NON- */
/*                           DECREASING SEQUENCE, WHILE THE REMAINING */
/*                           COMPONENTS ARE LOST. */
/*                E2     - INPUT VECTOR OF LENGTH N CONTAINING */
/*                           THE SQUARES OF THE OFF-DIAGONAL ELEMENTS */
/*                           OF THE MATRIX.  INPUT E2 IS DESTROYED. */
/*                N      - INPUT SCALAR CONTAINING THE ORDER OF THE */
/*                           MATRIX. */
/*                M      - INPUT SCALAR CONTAINING THE NUMBER OF */
/*                           SMALLEST EIGENVALUES DESIRED (M IS */
/*                           LESS THAN OR EQUAL TO N). */
/*                ISW    - INPUT SCALAR MEANING AS FOLLOWS - */
/*                           ISW=1 MEANS THAT THE MATRIX IS KNOWN TO BE */
/*                             POSITIVE DEFINITE. */
/*                           ISW=0 MEANS THAT THE MATRIX IS NOT KNOWN */
/*                             TO BE POSITIVE DEFINITE. */
/*                IER    - ERROR PARAMETER. (OUTPUT) (= IERR) */
/*                           WARNING ERROR */
/*                             IER = 601 INDICATES THAT SUCCESSIVE */
/*                               ITERATES TO THE K-TH EIGENVALUE WERE NOT */
/*                               MONOTONE INCREASING. THE VALUE K IS */
/*                               STORED IN E2(1). */
/*                           TERMINAL ERROR */
/*                             IER = 602 INDICATES THAT ISW=1 BUT MATRIX */
/*                               IS NOT POSITIVE DEFINITE */

/*   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32 */
/*                       - SINGLE/H36,H48,H60 */

/*   NOTATION            - INFORMATION ON SPECIAL NOTATION AND */
/*                           CONVENTIONS IS AVAILABLE IN THE MANUAL */
/*                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP */

/*   REMARKS      AS WRITTEN, THE ROUTINE COMPUTES THE M SMALLEST */
/*                EIGENVALUES. TO COMPUTE THE M LARGEST EIGENVALUES, */
/*                REVERSE THE SIGN OF EACH ELEMENT OF D BEFORE AND */
/*                AFTER CALLING THE ROUTINE. IN THIS CASE, ISW MUST */
/*                EQUAL ZERO. */

/*   COPYRIGHT           - 1980 BY IMSL, INC. ALL RIGHTS RESERVED. */

/*   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN */
/*                           APPLIED TO THIS CODE. NO OTHER WARRANTY, */
/*                           EXPRESSED OR IMPLIED, IS APPLICABLE. */

/* ----------------------------------------------------------------------- */

/*                                  SPECIFICATIONS FOR LOCAL VARIABLES */

/*                                  DRELPR = MACHINE PRECISION */

    ier = 0;
    dlam = 0.;
    err = 0.;
    s = 0.;

    /*                                  LOOK FOR SMALL SUB-DIAGONAL ENTRIES */
    /*                                  DEFINE INITIAL SHIFT FROM LOWER */
    /*                                  GERSCHGORIN BOUND. */

    tot = d[0];
    q = 0.;
    j = 0;
    for (i = 0; i < *n; ++i) {
        p = q;
        if (i == 0 || p <= itcom3_1.drelpr * (abs(d[i]) + abs(d[i-1])))
            e2[i] = 0.;

    /*                                  COUNT IF E2(I) HAS UNDERFLOWED */

        if (e2[i] == 0.)
            ++j;

        q = 0.;
        if (i != *n-1)
            q = sqrt(abs(e2[i + 1]));

        tot = min(d[i] - p - q,tot);
    }
    if (*isw != 1 || tot > 0.) {
        for (i = 0; i < *n; ++i)
            d[i] -= tot;
    }
    else
        tot = 0.;

    for (k = 0; k < *m; ++k)
    {
        /* NEXT QR TRANSFORMATION */

L70:
        tot += s;
        delta = d[*n] - s;
        i = *n - 1;
        f = abs(itcom3_1.drelpr * tot);
        if (dlam < f)
            dlam = f;

        if (delta > dlam)
            goto L90;

        if (delta >= -dlam)
            goto L170;

        ier = 602;
        goto L210;

        /* REPLACE SMALL SUB-DIAGONAL SQUARES */
        /* BY ZERO TO REDUCE THE INCIDENCE OF */
        /* UNDERFLOWS */

L90:
        if (k != *n-1) {
            for (j = k+1; j < *n; ++j) {
                d__1 = itcom3_1.drelpr * (d[j] + d[j - 1]);
                if (e2[j] <= d__1 * d__1)
                    e2[j] = 0.;
            }
        }
        f = e2[*n-1] / delta;
        qp = delta + f;
        p = 1.;
        for (ii = 0; ii < *n-k-1; ++ii) {
            i = *n - ii - 2;
            q = d[i] - s - f;
            r = q / qp;
            p = p * r + 1.;
            ep = f * r;
            d[i + 1] = qp + ep;
            delta = q - ep;
            if (delta <= dlam) {
                if (delta >= -dlam)
                    goto L170;

                ier = 602;
                goto L210;
            }

            f = e2[i] / q;
            qp = delta + f;
            e2[i + 1] = qp * ep;
        }

        d[k] = qp;
        s = qp / p;
        if (tot + s > tot)
            goto L70;

        ier = 601;
        ++k;
        e2[0] = (doublereal) k;
        --k;

        /* SET ERROR -- IRREGULAR END */
        /* DEFLATE MINIMUM DIAGONAL ELEMENT */

        s = 0.;
        delta = qp;
        for (j = k; j < *n; ++j) {
            if (d[j] <= delta) {
                i = j;
                delta = d[j];
            }
        }

        /* CONVERGENCE */

L170:
        if (i < *n-1)
            e2[i + 1] = e2[i] * f / qp;

        for (jj = 0; jj < i-k; ++jj) {
            j = i - jj - 1;
            d[j + 1] = d[j] - s;
            e2[j + 1] = e2[j];
        }

        d[k] = tot;
        err += abs(delta);
        e2[k] = err;
    }
L210:
    *ierr = ier;
    return 0;
} /* eqrt1s_ */

integer ipstr_(doublereal *omega)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer ip;
    static doublereal wm1;

/*     FINDS THE SMALLEST INTEGER, IPSTR, GREATER THAN 5 SUCH THAT */
/*          IPSTR * (OMEGA-1)**(IPSTR-1) .LE. 0.50. IPSTR WILL BE SET */
/*          IN LOOP. */

/* ... PARAMETER LIST: */

/*          OMEGA  RELAXATION FACTOR FOR SOR METHOD */

    wm1 = *omega - 1.;

    for (ip = 6; ip <= 940; ++ip) {
        i__1 = ip - 1;
        if ((doublereal) ((real) ip) * pow_di(&wm1, &i__1) <= .5)
            return ip;
    }
    return 940;
} /* ipstr_ */

/* Subroutine */
int iterm_(integer *nn, doublereal *a, doublereal *u, doublereal *wk, integer *imthdd)
{
    /* Local variables */
    static integer i;

    (void)imthdd;
/*     THIS ROUTINE PRODUCES THE ITERATION SUMMARY LINE AT THE END */
/*     OF EACH ITERATION. IF LEVEL = 5, THE LATEST APPROXIMATION */
/*     TO THE SOLUTION WILL BE PRINTED. */

/* ... PARAMETER LIST: */

/*          NN     ORDER OF SYSTEM OR, FOR REDUCED SYSTEM */
/*                 ROUTINES, ORDER OF BLACK SUBSYSTEM */
/*          A      ITERATION MATRIX */
/*          U      SOLUTION ESTIMATE */
/*          WK     WORK ARRAY OF LENGTH NN */
/*          IMTHD  INDICATOR OF METHOD (=IMTHDD) */
/*                    IMTHD = 1,  JCG */
/*                    IMTHD = 2,  JSI */
/*                    IMTHD = 3,  SOR */
/*                    IMTHD = 4,  SSORCG */
/*                    IMTHD = 5,  SSORSI */
/*                    IMTHD = 6,  RSCG */
/*                    IMTHD = 7,  RSSI */

    /* ... PRINT VARIOUS PARAMETERS AFTER EACH ITERATION */

    if (itcom1_1.level < 2)
        return 0;

    /* ... PRINT HEADER FOR JCG AND RSCG */

    /* ... PRINT SUMMARY LINE */

    /* ... PRINT HEADER FOR SSOR-SI */

    /* ... PRINT SUMMARY LINE */

    /* ... PRINT HEADER FOR J-SI AND RS-SI */

    /* ... PRINT SUMMARY LINE */

    /* ... PRINT VARIOUS PARAMETERS AFTER EACH ITERATION FOR SOR. */

    /* ... PRINT HEADER FOR SOR */

    /* ... PRINT SUMMARY LINE FOR SOR */

    /* ... PRINT VARIOUS PARAMETERS AFTER EACH ITERATION FOR SSOR-CG. */

    /* ... PRINT HEADER FOR SSOR-CG */

    /* ... PRINT SUMMARY LINE FOR SSOR-CG */

    if (itcom1_1.level < 4)
        return 0;

    for (i = 0; i < *nn; ++i)
        wk[i] = u[i] / a[i];

    return 0;
} /* iterm_ */

/* Subroutine */
int ivfill_(integer *n, integer *iv, integer *ival)
{
    /* Local variables */
    static integer i, m;

/*     FILLS AN INTEGER VECTOR, IV, WITH AN INTEGER VALUE, IVAL. */

/* ... PARAMETER LIST: */

/*          N      INTEGER LENGTH OF VECTOR IV */
/*          IV     INTEGER VECTOR */
/*          IVAL   INTEGER CONSTANT THAT FILLS FIRST N LOCATIONS OF IV */

    if (*n <= 0)
        return 0;

    /*     CLEAN UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 10 */

    m = *n % 10;
    for (i = 0; i < m; ++i)
        iv[i] = *ival;

    for (i = m; i < *n; i += 10) {
        iv[i] = *ival;
        iv[i + 1] = *ival;
        iv[i + 2] = *ival;
        iv[i + 3] = *ival;
        iv[i + 4] = *ival;
        iv[i + 5] = *ival;
        iv[i + 6] = *ival;
        iv[i + 7] = *ival;
        iv[i + 8] = *ival;
        iv[i + 9] = *ival;
    }

    return 0;
} /* ivfill_ */

/* Subroutine */
int omeg_(doublereal *dnrm, integer *iflag)
{
    /* Local variables */
    static doublereal zm1, zm2, temp;

/*     COMPUTES NEW VALUES FOR  CME, OMEGA, AND SPECR FOR */
/*     FULLY ADAPTIVE SSOR METHODS. */

/* ... PARAMETER LIST: */

/*          DNRM   NUMERATOR OF RAYLEIGH QUOTIENT */
/*          IFLAG  INDICATOR OF APPROPRIATE ENTRY POINT */

    zm1 = 0.;
    zm2 = 0.;
    if (*iflag == 1)
        goto L10;

    /* ... IFLAG .NE. 1, COMPUTE NEW ESTIMATE FOR CME */

    zm1 = ((1.-itcom3_1.spr) * (itcom3_1.betab * (itcom3_1.omega * itcom3_1.omega) + 1.) - itcom3_1.omega * (2.-itcom3_1.omega)) /
          (itcom3_1.omega * (itcom3_1.omega - 1. - itcom3_1.spr));

    if (! itcom2_1.caseii)
        zm2 = *dnrm / itcom3_1.bdelnm;

    if (itcom2_1.caseii)
        zm2 = sqrt(abs(*dnrm / itcom3_1.bdelnm));

    itcom3_1.cme = max(max(itcom3_1.cme,zm1),zm2);

    /* ... IFLAG = 1, OR CONTINUATION OF IFLAG .NE. 1 */

    /*        COMPUTE NEW VALUES OF OMEGA AND SPECR BASED ON CME AND BETAB */

L10:
    itcom1_1.is = itcom1_1.in + 1;
    itcom3_1.delsnm = itcom3_1.delnnm;
    if (itcom3_1.cme >= itcom3_1.betab * 4.)
        goto L30;

    /* ... CME .LT. 4.D0*BETAB */

    temp = sqrt(abs(1. - itcom3_1.cme * 2. + itcom3_1.betab * 4.));
    itcom3_1.omega = max(2. / (temp + 1.),1.);
    temp = (1. - itcom3_1.cme) / temp;
    itcom3_1.specr = (1. - temp) / (temp + 1.);
    if (abs(itcom3_1.omega - 1.) < itcom3_1.drelpr)
        itcom3_1.specr = 0.;

    return 0;

    /* ... CME .GE. 4.D0*BETAB */

    /* ... OMEGA-STAR WILL BE CHOSEN */

L30:
    itcom3_1.cme = sqrt((abs(itcom3_1.betab))) * 2.;
    itcom3_1.omega = 2. / (sqrt(abs(1. - itcom3_1.betab * 4.)) + 1.);
    itcom3_1.specr = itcom3_1.omega - 1.;
    itcom2_1.adapt = FALSE_;
    itcom2_1.partad = FALSE_;

    return 0;
} /* omeg_ */

logical omgchg_(integer *ndummy)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal del1, del2;
    (void)ndummy;

/* ... THIS FUNCTION TESTS TO SEE WHETHER OMEGA SHOULD BE CHANGED */
/* ... FOR SSOR CG METHOD. */

/* ... PARAMETER LIST: */

/*          NDUMMY ARBITRARY INTEGER PARAMETER */

    /* ... STATEMENT FUNCTION PHI(X) */

    if (itcom1_1.in - itcom1_1.is < 3)
        return FALSE_;

    if (itcom3_1.specr == 0.)
        goto L10;

    if (itcom3_1.specr >= itcom3_1.spr)
        return FALSE_;

    d__1 = 1. - itcom3_1.specr / itcom3_1.spr;
    del1 = -log(abs((1. - sqrt(abs(1.-itcom3_1.specr))) /
                    (1. + sqrt(abs(1.-itcom3_1.specr))) /
                    ((1. - sqrt(abs(d__1))) /
                     (1. + sqrt(abs(d__1))))));
    del2 = -log(abs((1. - sqrt(abs(1. - itcom3_1.spr))) / (1. + sqrt(abs(1. - itcom3_1.spr)))));
    if (del1 / del2 >= itcom3_1.ff)
        return FALSE_;

L10:
    return TRUE_;
} /* omgchg_ */

logical omgstr_(integer *ndummy)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal temp, temp1, omstar;

    (void)ndummy;

/*     TESTS FOR FULLY ADAPTIVE SSOR METHODS WHETHER OMEGA-STAR */
/*     SHOULD BE USED FOR OMEGA AND THE ADAPTIVE PROCESS TURNED */
/*     OFF. */

/* ... PARAMETER LIST: */

/*          NDUMMY ARBITRARY INTEGER PARAMETER */

    /* ... STATEMENT FUNCTION PHI(X) */

    if (itcom3_1.betab >= .25 || ! itcom2_1.adapt)
        return FALSE_;

    omstar = 2. / (sqrt(abs(1. - itcom3_1.betab * 4.)) + 1.);

    /* ... TEST TO CHOSE OMEGA-STAR */

    if (omstar > 1. && itcom3_1.specr > 0.) {
        temp = log(abs((1. - sqrt(abs(2. - omstar))) / (1. + sqrt(abs(2. - omstar)))));
        temp1 = log(abs((1. - sqrt(abs(1. - itcom3_1.specr))) / (1. + sqrt(abs(1. - itcom3_1.specr)))));
        if (temp / temp1 < itcom3_1.ff)
            return FALSE_;
    }

    /* ... OMEGA-STAR WAS CHOSEN */

    itcom3_1.omega = omstar;
    itcom3_1.specr = itcom3_1.omega - 1.;
    itcom2_1.adapt = FALSE_;
    itcom2_1.partad = FALSE_;
    itcom3_1.cme = sqrt((abs(itcom3_1.betab))) * 2.;
    d__1 = (1. - sqrt(abs(itcom3_1.specr))) / (1. + sqrt(abs(itcom3_1.specr)));
    itcom3_1.rrr = d__1 * d__1;
    itcom3_1.gamma = 2. / (2. - itcom3_1.specr);
    itcom3_1.sige = itcom3_1.specr / (2. - itcom3_1.specr);
    itcom3_1.rho = 1.;
    itcom1_1.is = itcom1_1.in + 1;
    itcom3_1.delsnm = itcom3_1.delnnm;

    return TRUE_;
} /* omgstr_ */

/* Subroutine */
int parcon_(doublereal *dtnrm, doublereal *c1, doublereal *c2, doublereal *c3, doublereal *c4,
            doublereal *gamold, doublereal * rhotmp, integer *ibmth)
{
    static integer ip;
    static doublereal rhoold;

/*     COMPUTES ACCELERATION PARAMETERS FOR CONJUGATE GRADIENT */
/*     ACCELERATED METHODS. */

/* ... PARAMETER LIST: */

/*          DTNRM  INNER PRODUCT OF RESIDUALS */
/*          C1     OUTPUT: RHO*GAMMA */
/*          C2     OUTPUT: RHO */
/*          C3     OUTPUT: 1-RHO */
/*          C4     OUTPUT: RHO*(1-GAMMA) */
/*          GAMOLD OUTPUT: VALUE OF GAMMA AT PRECEDING ITERATION */
/*          RHOTMP LAST ESTIMATE FOR VALUE OF RHO */
/*          IBMTH  INDICATOR OF BASIC METHOD BEING ACCELERATED BY CG */
/*                      IBMTH = 1,   JACOBI */
/*                            = 2,   REDUCED SYSTEM */
/*                            = 3,   SSOR */

    ip = itcom1_1.in - itcom1_1.is;

    /* ... SET RHOOLD AND GAMOLD */

    rhoold = itcom3_1.rho;
    *gamold = itcom3_1.gamma;

    /* ... COMPUTE GAMMA (IN+1) */

    /* ... FOR JACOBI OR REDUCED SYSTEM CG */

    if (*ibmth <= 2)
        itcom3_1.gamma = 1. / (1. - *dtnrm / itcom3_1.delnnm);

    /* ... FOR SSOR CG */

    if (*ibmth == 3)
        itcom3_1.gamma = itcom3_1.delnnm / *dtnrm;

    /* ... COMPUTE RHO (IN+1) */

    itcom3_1.rho = 1.;
    if (ip != 0) {
        if (itcom1_1.isym != 0)
            itcom3_1.rho = 1. / (1. - itcom3_1.gamma * *rhotmp / itcom3_1.delsnm);
        else
            itcom3_1.rho = 1. / (1. - itcom3_1.gamma * itcom3_1.delnnm / (*gamold * itcom3_1.delsnm * rhoold));
    }

    /* ... COMPUTE CONSTANTS C1, C2, C3, AND C4 */

    itcom3_1.delsnm = itcom3_1.delnnm;
    *rhotmp = rhoold;
    *c1 = itcom3_1.rho * itcom3_1.gamma;
    *c2 = itcom3_1.rho;
    *c3 = 1. - itcom3_1.rho;
    *c4 = itcom3_1.rho * (1. - itcom3_1.gamma);

    return 0;
} /* parcon_ */

/* Subroutine */
int parsi_(doublereal *c1, doublereal *c2, doublereal *c3, integer *ibmth)
{
    /* Local variables */
    static integer ip;

/*     COMPUTES ACCELERATION PARAMETERS FOR SEMI-ITERATIVE */
/*     ACCELERATED METHODS. */

/* ... PARAMETER LIST: */

/*          C1,C2 */
/*           AND */
/*           C3    OUTPUT ACCELERATION PARAMETERS */
/*          IBMTH  INDICATOR OF BASIC METHOD BEING ACCELERATED BY SI */
/*                      IBMTH = 1, JACOBI */
/*                            = 2, REDUCED SYSTEM */
/*                            = 3, SSOR */

    ip = itcom1_1.in - itcom1_1.is;
    if (ip == 0)
        goto L30;

    if (ip != 1) {
        itcom3_1.rho = 1. / (1. - itcom3_1.sige * itcom3_1.sige * itcom3_1.rho * .25);
        goto L20;
    }
    itcom3_1.rho = 1. / (1. - itcom3_1.sige * itcom3_1.sige * .5);

L20:
    *c1 = itcom3_1.rho * itcom3_1.gamma;
    *c2 = itcom3_1.rho;
    *c3 = 1. - itcom3_1.rho;

    return 0;

    /* ... NONADAPTIVE INITIALIZATION FOR SEMI-ITERATIVE METHODS */

L30:
    switch (*ibmth) {
        case 1:  goto L40;
        case 2:  goto L50;
        case 3:  goto L60;
    }

    /* ... JSI */

L40:
    if (itcom2_1.caseii)
        itcom3_1.sme = -itcom3_1.cme;

    itcom3_1.gamma = 2. / (2. - itcom3_1.cme - itcom3_1.sme);
    itcom3_1.sige = (itcom3_1.cme - itcom3_1.sme) / (2. - itcom3_1.cme - itcom3_1.sme);
    goto L70;

    /* ... REDUCED SYSTEM SI */

L50:
    itcom3_1.gamma = 2. / (2. - itcom3_1.cme * itcom3_1.cme);
    itcom3_1.sige = itcom3_1.cme * itcom3_1.cme / (2. - itcom3_1.cme * itcom3_1.cme);
    itcom3_1.rrr = (1. - sqrt(abs(1. - itcom3_1.cme * itcom3_1.cme))) / (sqrt(abs(1. - itcom3_1.cme * itcom3_1.cme)) + 1.);
    goto L70;

    /* ... SSORSI */

L60:
    itcom3_1.gamma = 2. / (2. - itcom3_1.specr);
    itcom3_1.sige = itcom3_1.specr / (2. - itcom3_1.specr);
    itcom3_1.rrr = (1. - sqrt(abs(1. - itcom3_1.sige * itcom3_1.sige))) / (sqrt(abs(1. - itcom3_1.sige * itcom3_1.sige)) + 1.);

L70:
    itcom3_1.rho = 1.;
    *c1 = itcom3_1.gamma;
    *c2 = 1.;
    *c3 = 0.;

    return 0;
} /* parsi_ */

doublereal pbeta_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *v, doublereal *w1, doublereal *w2)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static integer i, k, ii, jj, jai;
    static doublereal sum;
    static integer jajj, ibgn, iend, itmp;
    static doublereal temp1, temp2;

/*     ... COMPUTES THE NUMERATOR FOR THE COMPUTATION OF BETAB IN */
/*     ...  SSOR METHODS. */

/* ... PARAMETER LIST: */

/*          N      DIMENSION OF MATRIX */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          W1,W2  WORKSPACE VECTORS OF LENGTH N */

    ret_val = 0.;
    if (itcom1_1.isym == 0)
        goto L110;

    /*     ************** NON - SYMMETRIC SECTION ******************** */

    for (i = 0; i < *n; ++i)
        w1[i] = v[i];

    temp1 = 0.;
    temp2 = 0.;
    itmp = 2;
    ibgn = ia[0] - 1;
    iend = ia[itmp-1] - 1;
    for (i = ibgn; i < iend; ++i) {
        jai = ja[i] - 1;
        temp1 -= a[i] * w1[jai];
    }
    w1[0] = temp1;
    w2[0] = 0.;
    for (k = 1; k < *n-1; ++k) {
        temp1 = 0.;
        temp2 = 0.;
        ibgn = ia[k] - 1;
        iend = ia[k + 1] - 1;
        for (i = ibgn; i < iend; ++i) {
            jai = ja[i] - 1;
            if (jai > k)
                temp1 -= a[i] * w1[jai];
            else
                temp2 -= a[i] * w1[jai];
        }
        w1[k] = temp1;
        w2[k] = temp2;
    }
    temp2 = 0.;
    ibgn = ia[*n-1] - 1;
    iend = ia[*n] - 1;
    for (i = ibgn; i < iend; ++i) {
        jai = ja[i] - 1;
        temp2 -= a[i] * w1[jai];
    }
    w2[*n-1] = temp2;
    for (i = 0; i < *n; ++i)
        ret_val += v[i] * w2[i];

    return ret_val;

    /*     **************** SYMMETRIC SECTION ************************* */

L110:
    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        if (ibgn >= iend)
            continue;
        sum = 0.;
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            sum += a[jj] * v[jajj];
        }
        ret_val += sum * sum;
    }
    return ret_val;
} /* pbeta_ */

/* Subroutine */
int pbsor_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *u, doublereal *rhs)
{
    /* Local variables */
    static integer i, ii, jj;
    static doublereal ui, sum, omm1;
    static integer jajj, ibgn, iend;

/*     ... THIS SUBROUTINE COMPUTES A BACKWARD SOR SWEEP. */

/* ... PARAMETER LIST: */

/*          N      ORDER OF SYSTEM */
/*          OMEGA  RELAXATION FACTOR */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          U      LATEST ESTIMATE OF SOLUTION */
/*          RHS    RIGHT HAND SIDE OF MATRIX PROBLEM */

    omm1 = itcom3_1.omega - 1.;
    if (itcom1_1.isym == 0)
        goto L40;

    /*     *************** NON - SYMMETRIC SECTION ********************** */

    for (i = 0; i < *n; ++i) {
        ii = *n - i - 1;
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        sum = rhs[ii];
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            sum -= a[jj] * u[jajj];
        }
        u[ii] = itcom3_1.omega * sum - omm1 * u[ii];
    }
    return 0;

    /*     ***************** SYMMETRIC SECTION ************************** */

L40:
    for (ii = 0; ii < *n; ++ii) {
        ui = u[ii];
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            rhs[jajj] -= a[jj] * ui;
        }
    }

    for (i = 0; i < *n; ++i) {
        ii = *n - i - 1;
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        sum = rhs[ii];
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            sum -= a[jj] * u[jajj];
        }
        u[ii] = itcom3_1.omega * sum - omm1 * u[ii];
    }
    return 0;
} /* pbsor_ */

/* Subroutine */
int qsort_(integer *n, integer *key, doublereal *data, integer *error)
{
    /* Initialized data */

    static integer tiny = 9;
    static integer stklen = 30;

    /* Local variables */
    static doublereal d;
    static integer i, j, k, v, jm1, ip1, top;
    static logical done;
    static integer left, llen, rlen, lfrh2, stack[30], right;

/*     ================================================================== */

/*     Q U I C K S O R T */

/*         IN THE STYLE OF THE CACM PAPER BY BOB SEDGEWICK, OCTOBER 1978 */

/*     INPUT: */
/*         N    -- NUMBER OF ELEMENTS TO BE SORTED */
/*         KEY  -- AN ARRAY OF LENGTH  N  CONTAINING THE VALUES */
/*                 WHICH ARE TO BE SORTED */
/*         DATA -- A SECOND ARRAY OF LENGTH  N  CONTAINING DATA */
/*                 ASSOCIATED WITH THE INDIVIDUAL KEYS. */

/*     OUTPUT: */
/*         KEY  -- WILL BE ARRANGED SO THAT VALUES ARE IN INCREASING */
/*                 ORDER */
/*         DATA -- REARRANGED TO CORRESPOND TO REARRANGED KEYS */
/*         ERROR -- WILL BE ZERO UNLESS YOUR INPUT FILE WAS OF TRULY */
/*                  ENORMOUS LENGTH, IN WHICH CASE IT WILL BE EQUAL TO 1. */

/*     ================================================================== */

/*     ... PROGRAM IS A DIRECT TRANSLATION INTO FORTRAN OF SEDGEWICK^S */
/*         PROGRAM 2, WHICH IS NON-RECURSIVE, IGNORES FILES OF LENGTH */
/*         LESS THAN 'TINY' DURING PARTITIONING, AND USES MEDIAN OF THREE */
/*         PARTITIONING. */

    if (*n == 1)
        return 0;

    if (*n <= 0)
        goto L240;

    *error = 0;
    top = 1;
    left = 0;
    right = *n - 1;
    done = *n <= tiny;

    if (done)
        goto L150;

    ivfill_(&stklen, stack, &c__0);

    /*     =========================================================== */
    /*     QUICKSORT -- PARTITION THE FILE UNTIL NO SUBFILE REMAINS OF */
    /*     LENGTH GREATER THAN 'TINY' */
    /*     =========================================================== */

    /*     ... WHILE NOT DONE DO ... */

L10:
    if (done)
        goto L150;

    /*         ... FIND MEDIAN OF LEFT, RIGHT AND MIDDLE ELEMENTS OF CURRENT */
    /*             SUBFILE, WHICH IS  KEY(LEFT), ..., KEY(RIGHT) */

    lfrh2 = (left + right) / 2;
    k = key[lfrh2];
    d = data[lfrh2];
    key[lfrh2] = key[left];
    data[lfrh2] = data[left];
    key[left] = k;
    data[left] = d;

    if (key[left + 1] > key[right]) {
        k = key[left + 1];
        d = data[left + 1];
        key[left + 1] = key[right];
        data[left + 1] = data[right];
        key[right] = k;
        data[right] = d;
    }

    if (key[left] > key[right]) {
        k = key[left];
        d = data[left];
        key[left] = key[right];
        data[left] = data[right];
        key[right] = k;
        data[right] = d;
    }

    if (key[left + 1] > key[left]) {
        k = key[left + 1];
        d = data[left + 1];
        key[left + 1] = key[left];
        data[left + 1] = data[left];
        key[left] = k;
        data[left] = d;
    }

    v = key[left];

    /* ... V IS NOW THE MEDIAN VALUE OF THE THREE KEYS.  NOW MOVE */
    /*     FROM THE LEFT AND RIGHT ENDS SIMULTANEOUSLY, EXCHANGING */
    /*     KEYS AND DATA UNTIL ALL KEYS LESS THAN  V  ARE PACKED TO */
    /*     THE LEFT, ALL KEYS LARGER THAN  V  ARE PACKED TO THE */
    /*     RIGHT. */

    i = left + 1;
    j = right;

    /*  LOOP */
    /*      REPEAT I = I+1 UNTIL KEY(I) >= V; */
    /*      REPEAT J = J-1 UNTIL KEY(J) <= V; */
    /*  EXIT IF J < I; */
    /*      << EXCHANGE KEYS I AND J >> */
    /*  END */

L50:
    while (key[++i] < v) ;

    while (key[--j] > v) ;

    if (j >= i) {
        k = key[i];
        d = data[i];
        key[i] = key[j];
        data[i] = data[j];
        key[j] = k;
        data[j] = d;
        goto L50;
    }

    k = key[left];
    d = data[left];
    key[left] = key[j];
    data[left] = data[j];
    key[j] = k;
    data[j] = d;

    /* ... WE HAVE NOW PARTITIONED THE FILE INTO TWO SUBFILES, */
    /*     ONE IS (LEFT ... J-1)  AND THE OTHER IS (I...RIGHT). */
    /*     PROCESS THE SMALLER NEXT.  STACK THE LARGER ONE. */

    llen = j - left;
    rlen = right - i + 1;
    if (max(llen,rlen) > tiny)
        goto L100;

    /*             ... BOTH SUBFILES ARE TINY, SO UNSTACK NEXT LARGER FILE */

    if (top != 1) {
        top += -2;
        left = stack[top - 1] - 1;
        right = stack[top] - 1;
    }
    else
        done = TRUE_;

    goto L10;

    /*             ... ELSE ONE OR BOTH SUBFILES ARE LARGE */

L100:
    if (min(llen,rlen) > tiny)
        goto L120;

    /*             ... ONE SUBFILE IS SMALL, ONE LARGE.  IGNORE THE SMALL ONE */

    if (llen <= rlen)
        left = i;
    else
        right = j - 1;

    goto L10;

    /*         ... ELSE BOTH ARE LARGER THAN TINY.  ONE MUST BE STACKED. */

L120:
    if (top >= stklen)
        goto L240;

    if (llen <= rlen) {
        stack[top - 1] = i + 1;
        stack[top] = right + 1;
        right = j - 1;
    }
    else {
        stack[top - 1] = left + 1;
        stack[top] = j;
        left = i;
    }

    top += 2;

    goto L10;

    /*     ------------------------------------------------------------ */
    /*     INSERTION SORT THE ENTIRE FILE, WHICH CONSISTS OF A LIST */
    /*     OF 'TINY' SUBFILES, LOCALLY OUT OF ORDER, GLOBALLY IN ORDER. */
    /*     ------------------------------------------------------------ */

    /*     ... FIRST, FIND LARGEST ELEMENT IN 'KEY' */

L150:
    i = *n - 2;
    left = max(0, *n-tiny) - 1;
    j = *n - 1;
    k = key[j];

L160:
    if (i <= left)
        goto L180;

    if (key[i] > k) {
        k = key[i];
        j = i;
    }

    --i;
    goto L160;

L180:
    if (j != *n - 1) {
        /* ... LARGEST ELEMENT WILL BE IN  KEY(N) */
        key[j] = key[*n-1];
        key[*n-1] = k;
        d = data[*n-1];
        data[*n-1] = data[j];
        data[j] = d;
    }

    /* ... INSERTION SORT ... FOR I := N-1 STEP -1 TO 1 DO ... */

    i = *n - 2;
    ip1 = *n - 1;

L200:
    if (key[i] <= key[ip1])
        goto L220;

    /*             ... OUT OF ORDER ... MOVE UP TO CORRECT PLACE */

    k = key[i];
    d = data[i];
    j = ip1;
    jm1 = i;

    /*             ... REPEAT ... UNTIL 'CORRECT PLACE FOR K FOUND' */

L210:
    key[jm1] = key[j];
    data[jm1] = data[j];
    jm1 = j;
    ++j;
    if (key[j] < k)
        goto L210;

    key[jm1] = k;
    data[jm1] = d;

L220:
    ip1 = i;
    --i;
    if (i >= 0)
        goto L200;

L230:
    return 0;

L240:
    *error = 1;
    goto L230;
} /* qsort_ */

/* Subroutine */
int permat_(integer *n, integer *ia, integer *ja, doublereal *a, integer *p,
            integer *newia, integer *isym, integer * level, integer *nout, integer* ierr)
{
    /* Local variables */
    static integer i, j, k, ip, jp, jaj, ier, ipp, ibgn, iend;
    static doublereal save;
    static integer nels;
    static doublereal temp;
    static integer next;

    (void)level; (void)nout;
/* ********************************************************************* */

/* ... SUBROUTINE PERMAT TAKES THE SPARSE MATRIX REPRESENTATION */
/*     OF THE MATRIX STORED IN THE ARRAYS IA, JA, AND A AND */
/*     PERMUTES BOTH ROWS AND COLUMNS OVERWRITING THE PREVIOUS */
/*     STRUCTURE. */

/* ... PARAMETER LIST: */

/*         N      ORDER OF SYSTEM */
/*         IA,JA  INTEGER ARRAYS OF THE SPARSE MATRIX REPRESENTATION */
/*         A      D.P. ARRAY OF THE SPARSE MATRIX REPRESENTATION */
/*         P      PERMUTATION VECTOR */
/*         NEWIA  INTEGER WORK VECTOR OF LENGTH N */
/*         ISYM   SYMMETRIC/NONSYMMETRIC STORAGE SWITCH */
/*         LEVEL  SWITCH CONTROLLING LEVEL OF OUTPUT */
/*         NOUT OUTPUT UNIT NUMBER */
/*         IER    OUTPUT ERROR FLAG (= IERR) */

/*                   IER =   0  NORMAL RETURN */
/*                   IER = 301  NO ENTRY IN ITH ROW OF ORIGINAL */
/*                              MATRIX. IF LEVEL IS GREATER THAN */
/*                              0, I WILL BE PRINTED */
/*                   IER = 302  THERE IS NO ENTRY IN THE ITH ROW */
/*                              OF THE PERMUTED MATRIX */
/*                   IER = 303  ERROR RETURN FROM QSORT IN */
/*                              SORTING THE ITH ROW OF THE */
/*                              PERMUTED MATRIX */
/* ... IT IS ASSUMED THAT THE I-TH ENTRY OF THE PERMUTATION VECTOR */
/*     P INDICATES THE ROW THE I-TH ROW GETS MAPPED INTO.  (I.E. */
/*     IF ( P(I) = J ) ROW I GETS MAPPED INTO ROW J.) */

/* ... THE ARRAY NEWIA IS AN INTEGER WORK VECTOR OF LENGTH N WHICH */
/*     KEEPS TRACK OF WHERE THE ROWS BEGIN IN THE PERMUTED STRUCTURE. */

/* ... PERMAT IS CAPABLE OF PERMUTING BOTH THE SYMMETRIC AND NON- */
/*     SYMMETRIC FORM OF IA, JA, AND A.  IF ( ISYM .EQ. 0 ) SYMMETRIC */
/*     FORM IS ASSUMED. */

/* ... TWO EXTERNAL MODULES ARE USED BY PERMAT.  THE FIRST IS INTEGER */
/*     FUNCTION BISRCH WHICH USES A BISECTION SEARCH ( ORDER LOG-BASE-2 */
/*     OF N+1 ) THROUGH THE ARRAY IA TO FIND THE ROW INDEX OF AN ARBI- */
/*     TRARY ENTRY EXTRACTED FROM THE ARRAY JA. THE SECOND IS SUBROUTINE */
/*     QSORT WHICH PERFORMS A QUICK SORT TO PLACE THE ENTRIES IN */
/*     THE PERMUTED ROWS IN COLUMN ORDER. */

/* ********************************************************************* */

    /* ... PREPROCESSING PHASE */

    /* ...... DETERMINE THE NUMBER OF NONZEROES IN THE ROWS OF THE PERMUTED */
    /*        MATRIX AND STORE THAT IN NEWIA.  THEN SWEEP THRU NEWIA TO MAKE */
    /*        NEWIA(I) POINT TO THE BEGINNING OF EACH ROW IN THE PERMUTED */
    /*        DATA STRUCTURE.  ALSO NEGATE ALL THE ENTRIES IN JA TO INDICATE */
    /*        THAT THOSE ENTRIES HAVE NOT BEEN MOVED YET. */

    ier = 0;
    nels = ia[*n] - 1;
    for (i = 0; i < *n; ++i)
        newia[i] = 0;

    for (i = 0; i < *n; ++i) {
        ip = p[i] - 1;
        ibgn = ia[i] - 1;
        iend = ia[i + 1] - 1;
        if (ibgn >= iend)
            goto L90;

        for (j = ibgn; j < iend; ++j) {
            ipp = ip;
            jaj = ja[j];
            jp = p[jaj-1] - 1;
            if (*isym == 0 && ip > jp)
                ipp = jp;

            ++newia[ipp];
            ja[j] = -jaj;
        }
    }
    ibgn = 0;
    for (i = 0; i < *n; ++i) {
        k = ibgn + newia[i];
        newia[i] = ibgn+1;
        ibgn = k;
    }

    /* ...... PREPROCESSING NOW FINISHED. */

    /* ...... NOW PERMUTE JA AND A.  THIS PERMUTATION WILL PERFORM THE */
    /*        FOLLOWING STEPS */

    /*           1.  FIND THE FIRST ENTRY IN JA NOT PERMUTED WHICH IS */
    /*               INDICATED BY AN NEGATIVE VALUE IN JA */
    /*           2.  COMPUTE WHICH ROW THE CURRENT ENTRY IS IN.  THIS */
    /*               IS COMPUTED BY A BISECTION SEARCH THRU THE ARRAY */
    /*               IA. */
    /*           3.  USING THE PERMUTATION ARRAY P AND THE ARRAY NEWIA */
    /*               COMPUTE WHERE THE CURRENT ENTRY IS TO BE PLACED. */
    /*           4.  THEN PICK UP THE ENTRY WHERE THE CURRENT ENTRY WILL */
    /*               GO.  PUT THE CURRENT ENTRY IN PLACE.  THEN MAKE THE */
    /*               DISPLACED ENTRY THE CURRENT ENTRY AND LOOP TO STEP 2. */
    /*           5.  THIS PROCESS WILL END WHEN THE NEXT ENTRY HAS ALREADY */
    /*               BEEN MOVED.  THEN LOOP TO STEP 1. */

    for (j = 0; j < nels; ++j) {
        if (ja[j] > 0)
            continue;
        jaj = -ja[j];
        save = a[j];
        next = j + 1;
        ja[j] = jaj;
L50:
        jp = p[jaj-1] - 1;
        k = *n + 1;
        i = bisrch_(&k, ia, &next) - 1;
        ip = p[i] - 1;
        ipp = ip;
        if (*isym == 0 && ip > jp) {
            ipp = jp;
            jp = ip;
        }
        next = newia[ipp] - 1;

        temp = save; save = a[next]; a[next] = temp;

        jaj = -ja[next];
        ja[next] = jp + 1;
        ++newia[ipp];
        if (jaj > 0) {
            ++next;
            goto L50;
        }
    }

    /* ...... THE MATRIX IS NOW PERMUTED BUT THE ROWS MAY NOT BE IN */
    /*        ORDER.  THE REMAINDER OF THIS SUBROUTINE PERFORMS */
    /*        A QUICK SORT ON EACH ROW TO SORT THE ENTRIES IN */
    /*        COLUMN ORDER.  THE IA ARRAY IS ALSO CORRECTED FROM */
    /*        INFORMATION STORED IN THE NEWIA ARRAY.  NEWIA(I) NOW */
    /*        POINTS TO THE FIRST ENTRY OF ROW I+1. */

    ia[0] = 1;
    for (i = 0; i < *n; ++i) {
        ia[i + 1] = newia[i];
        k = ia[i + 1] - ia[i];
        if (k == 1)
            continue;
        if (k < 1)
            goto L110;

        ibgn = ia[i] - 1;
        qsort_(&k, &ja[ibgn], &a[ibgn], &ier);
        if (ier != 0)
            goto L130;
    }

    /* ...... END OF MATRIX PERMUTATION */

    goto L150;

    /* ... ERROR TRAPS */

    /* ...... NO ENTRY IN ROW I IN THE ORIGINAL SYSTEM */

L90:
    ier = 301;
    goto L150;

    /* ...... NO ENTRY IN ROW I IN THE PERMUTED SYSTEM */

L110:
    ier = 302;
    goto L150;

    /* ...... ERROR RETURN FROM SUBROUTINE QSORT */

L130:
    ier = 303;

L150:
    *ierr = ier;
    return 0;
} /* permat_ */

/* Subroutine */
int perror_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *rhs,
            doublereal *u, doublereal *w, doublereal *digtt1, doublereal *digtt2, integer *idgtts)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal bnrm, temp, rnrm;
    static integer idgts;
    static doublereal digit1, digit2;

/*     PERROR COMPUTES THE RESIDUAL, R = RHS - A*U.  THE USER */
/*     ALSO HAS THE OPTION OF PRINTING THE RESIDUAL AND/OR THE */
/*     UNKNOWN VECTOR DEPENDING ON IDGTS. */

/* ... PARAMETER LIST: */

/*          N      DIMENSION OF MATRIX */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          RHS    RIGHT HAND SIDE OF MATRIX PROBLEM */
/*          U      LATEST ESTIMATE OF SOLUTION */
/*          W      WORKSPACE VECTOR */
/*          DIGIT1 OUTPUT: MEASURE OF ACCURACY OF STOPPING TEST (= DIGTT1 */
/*          DIGIT2 OUTPUT: MEASURE OF ACCURACY OF SOLUTION (= DIGTT2) */
/*          IDGTS   PARAMETER CONTROLING LEVEL OF OUTPUT (= IDGTTS) */
/*                    IF IDGTS < 1 OR IDGTS > 4, THEN NO OUTPUT. */
/*                            = 1, THEN NUMBER OF DIGITS IS PRINTED, PRO- */
/*                                 VIDED LEVEL .GE. 1 */
/*                            = 2, THEN SOLUTION VECTOR IS PRINTED, PRO- */
/*                                 VIDED LEVEL .GE. 1 */
/*                            = 3, THEN RESIDUAL VECTOR IS PRINTED, PRO- */
/*                                 VIDED LEVEL .GE. 1 */
/*                            = 4, THEN BOTH VECTORS ARE PRINTED, PRO- */
/*                                 VIDED LEVEL .GE. 1 */

    idgts = *idgtts;
    digit1 = 0.;
    digit2 = 0.;
    if (*n <= 0)
        goto L40;

    d__1 = abs(itcom3_1.drelpr);
    digit1 = -d_lg10(&d__1);
    if (itcom3_1.stptst > 0.) {
        d__1 = abs(itcom3_1.stptst);
        digit1 = -d_lg10(&d__1);
    }
    bnrm = itpackddot_(n, rhs, &c__1, rhs, &c__1);
    if (bnrm == 0.)
        goto L10;

    pmult_(n, ia, ja, a, u, w);
    wevmw_(n, rhs, w);
    rnrm = itpackddot_(n, w, &c__1, w, &c__1);
    temp = rnrm / bnrm;
    if (temp != 0.) {
        d__1 = abs(temp);
        digit2 = -d_lg10(&d__1) / 2.;
        goto L20;
    }
L10:
    d__1 = abs(itcom3_1.drelpr);
    digit2 = -d_lg10(&d__1);

L20:
    if (itcom1_1.level > 0) {
        if (idgts == 2 || idgts == 4)
            vout_(n, u, &c__2, &itcom1_1.nout);
        if (idgts == 3 || idgts == 4)
            vout_(n, w, &c__1, &itcom1_1.nout);
    }

L40:
    *digtt1 = digit1;
    *digtt2 = digit2;
    return 0;
} /* perror_ */

/* Subroutine */
int pervec_(integer *n, doublereal *v, integer *p)
{
    /* Local variables */
    static integer ii, now;
    static doublereal save, temp;
    static integer next;

/*     THIS SUBROUTINE PERMUTES A D.P. VECTOR AS DICTATED BY THE */
/*     PERMUTATION VECTOR, P.  IF P(I) = J, THEN V(J) GETS V(I). */

/* ... PARAMETER LIST: */

/*          V      D.P. VECTOR OF LENGTH N */
/*          P     INTEGER PERMUTATION VECTOR */

    if (*n <= 0)
        return 0;

    for (ii = 0; ii < *n; ++ii) {
        if (p[ii] < 0)
            continue;

        next = p[ii];
        save = v[ii];
        while (p[next-1] >= 0) {
            temp = save;
            save = v[next-1];
            v[next-1] = temp;

            now = next;
            next = p[now-1];
            p[now-1] = -next;
        }
    }

    for (ii = 0; ii < *n; ++ii)
        p[ii] = -p[ii];

    return 0;
} /* pervec_ */

/* Subroutine */
int pfsor_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *u, doublereal *rhs)
{
    /* Local variables */
    static integer ii, jj;
    static doublereal ui, sum, omm1;
    static integer jajj, ibgn, iend;

/*         THIS SUBROUTINE COMPUTES A FORWARD SOR SWEEP. */

/* ... PARAMETER LIST: */

/*         N       ORDER OF SYSTEM */
/*          OMEGA  RELAXATION FACTOR */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          U      LATEST ESTIMATE OF SOLUTION */
/*          RHS    RIGHT HAND SIDE OF MATRIX PROBLEM */

    omm1 = itcom3_1.omega - 1.;
    if (itcom1_1.isym == 0)
        goto L40;

    /*     *********** NON - SYMMETRIC SECTION ********************* */

    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        sum = rhs[ii];
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            sum -= a[jj] * u[jajj];
        }
        ui = itcom3_1.omega * sum - omm1 * u[ii];
        u[ii] = ui;
    }
    return 0;

    /*     ************* SYMMETRIC SECTION ************************* */

L40:
    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        sum = rhs[ii];
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            sum -= a[jj] * u[jajj];
        }
        ui = itcom3_1.omega * sum - omm1 * u[ii];
        u[ii] = ui;
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            rhs[jajj] -= a[jj] * ui;
        }
    }
    return 0;
} /* pfsor_ */

/* Subroutine */
int pfsor1_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *u, doublereal *rhs)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static integer ii, jj;
    static doublereal ui, sum, omm1;
    static integer jajj, ibgn, iend;
    static doublereal sumd;

/*         THIS SUBROUTINE COMPUTES A FORWARD SOR SWEEP ON U AND */
/*         COMPUTES THE NORM OF THE PSEUDO-RESIDUAL VECTOR. */

/* ... PARAMETER LIST: */

/*          N      ORDER OF SYSTEM */
/*          OMEGA  RELAXATION FACTOR */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          U      LATEST ESTIMATE OF SOLUTION */
/*          RHS    RIGHT HAND SIDE OF MATRIX PROBLEM */

    omm1 = itcom3_1.omega - 1.;
    sumd = 0.;
    if (itcom1_1.isym == 0)
        goto L40;

    /*     **************** NON - SYMMETRIC SECTION ****************** */

    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        sum = rhs[ii];
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            sum -= a[jj] * u[jajj];
        }
        ui = itcom3_1.omega * sum - omm1 * u[ii];
        d__1 = ui - u[ii];
        sumd += d__1 * d__1;
        u[ii] = ui;
    }
    goto L90;

    /*     *************** SYMMETRIC SECTION ************************ */

L40:
    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        sum = rhs[ii];
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            sum -= a[jj] * u[jajj];
        }
        ui = itcom3_1.omega * sum - omm1 * u[ii];
        d__1 = ui - u[ii];
        sumd += d__1 * d__1;
        u[ii] = ui;
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            rhs[jajj] -= a[jj] * ui;
        }
    }

L90:
    itcom3_1.delnnm = sqrt(sumd);
    return 0;
} /* pfsor1_ */

/* Subroutine */
int pjac_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *u, doublereal *rhs)
{
    /* Local variables */
    static integer ii, jj;
    static doublereal uii;
    static integer jajj, ibgn, iend;
    static doublereal rhsii;

/*     ... THIS SUBROUTINE PERFORMS ONE JACOBI ITERATION. */

/* ... PARAMETER LIST: */

/*          N      DIMENSION OF MATRIX */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          U      ESTIMATE OF SOLUTION OF A MATRIX PROBLEM */
/*          RHS    ON INPUT: CONTAINS THE RIGHT HAND SIDE OF */
/*                    A MATRIX PROBLEM */
/*                 ON OUTPUT: CONTAINS A*U + RHS */

    if (itcom1_1.isym == 0)
        goto L30;

    /*     *************** NON - SYMMETRIC SECTION **************** */

    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        rhsii = rhs[ii];
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            rhsii -= a[jj] * u[jajj];
        }
        rhs[ii] = rhsii;
    }
    return 0;

    /*     ************** SYMMETRIC SECTION ********************** */

L30:
    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        if (ibgn >= iend)
            continue;

        rhsii = rhs[ii];
        uii = u[ii];
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            rhsii -= a[jj] * u[jajj];
            rhs[jajj] -= a[jj] * uii;
        }
        rhs[ii] = rhsii;
    }
    return 0;
} /* pjac_ */

/* Subroutine */
int pmult_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *u, doublereal *w)
{
    /* Local variables */
    static integer ii, jj;
    static doublereal uii, wii, sum;
    static integer jajj, ibgn, iend;

/*     ... THIS SUBROUTINE PERFORMS ONE MATRIX-VECTOR MULTIPLICATION. */

/* ... PARAMETER LIST: */

/*          N      DIMENSION OF MATRIX */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          U      LATEST ESTIMATE OF SOLUTION */
/*          W      ON RETURN W CONTAINS A*U */

    if (*n <= 0)
        return 0;

    if (itcom1_1.isym == 0)
        goto L40;

    /*     *************** NON - SYMMETRIC SECTION ********************** */

    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        sum = 0.;
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            sum += a[jj] * u[jajj];
        }
        w[ii] = sum;
    }
    return 0;

    /*     ***************** SYMMETRIC SECTION ************************** */

L40:
    vfill_(n, w, &c_b21);
    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        uii = u[ii];
        wii = w[ii];
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            wii += a[jj] * u[jajj];
            w[jajj] += a[jj] * uii;
        }
        w[ii] = wii;
    }
    return 0;
} /* pmult_ */

/* Subroutine */
int prbndx_(integer *n, integer *nblack, integer *ia, integer *ja, integer *p,
            integer *ip, integer *level, integer *nout, integer *ier)
{
    /* Local variables */
    static integer i, j, k, old, ibgn, iend, nred, last, next, typ, first, young, curtyp, nxttyp;

    (void)level; (void)nout;
/* ************************************************************** */

/*     THIS SUBROUTINE COMPUTES THE RED-BLACK PERMUTATION */
/*     VECTORS P ( AND ITS INVERSE IP ) IF POSSIBLE. */

/*     THE ALGORITHM IS TO MARK THE FIRST NODE AS RED (ARBITRARY). */
/*     ALL OF ITS ADJACENT NODES ARE MARKED BLACK AND PLACED IN */
/*     A STACK.  THE REMAINDER OF THE CODE PULLS THE FIRST NODE */
/*     OFF THE TOP OF THE STACK AND TRIES TO TYPE ITS ADJACENT NODES. */
/*     THE TYPING OF THE ADJACENT POINT IS A FIVE WAY CASE STATEMENT */
/*     WHICH IS WELL COMMENTED BELOW (SEE DO LOOP 100). */

/*     THE ARRAY P IS USED BOTH TO KEEP TRACK OF THE COLOR OF A NODE */
/*     (RED NODE IS POSITIVE, BLACK IS NEGATIVE) BUT ALSO THE FATHER */
/*     NODE THAT CAUSED THE COLOR MARKING OF THAT POINT.  SINCE */
/*     COMPLETE INFORMATION ON THE ADJACENCY STRUCTURE IS HARD TO COME */
/*     BY THIS FORMS A LINK TO ENABLE THE COLOR CHANGE OF A PARTIAL */
/*     TREE WHEN A RECOVERABLE COLOR CONFLICT OCCURS. */

/*     THE ARRAY IP IS USED AS A STACK TO POINT TO THE SET OF NODES */
/*     LEFT TO BE TYPED THAT ARE KNOWN TO BE ADJACENT TO THE CURRENT */
/*     FATHER NODE. */

/* ********************************************************************* */

/*     INPUT PARAMETERS */

/*        N      NUMBER OF NODES.  (INTEGER, SCALAR) */

/*        IA,JA  ADJACENCY STRUCTURE ARRAYS.  CAN BE EITHER THE */
/*               SYMMETRIC OR NONSYMMETRIC FORM.  IT IS ASSUMED */
/*               THAT FOR EVERY ROW WHERE ONLY ONE ELEMENT IS */
/*               STORED THAT ELEMENT CORRESPONDS TO THE DIAGONAL */
/*               ENTRY.  THE DIAGONAL DOES NOT HAVE TO BE THE FIRST */
/*               ENTRY STORED.  (INTEGER, ARRAYS) */
/*        LEVEL  SWITCH FOR PRINTING */
/*        NOUT OUTPUT TAPE NUMBER */

/*     OUTPUT PARAMETERS */

/*        NBLACK NUMBER OF BLACK NODES.  NUMBER OF RED NODES IS */
/*               N - NBLACK.  (INTEGER, SCALAR) */

/*        P, IP  PERMUTATION AND INVERSE PERMUTATION VECTORS. */
/*               (INTEGER, ARRAYS EACH OF LENGTH N) */

/*        IER    ERROR FLAG. (INTEGER, SCALAR) */

/*               IER = 0, NORMAL RETURN.  INDEXING PERFORMED */
/*                        SUCCESSFULLY */
/*               IER =201, RED-BLACK INDEXING NOT POSSIBLE. */

/* ******************************************************************** */

    *ier = 0;

    /*        IF ( N .LE. 0 ) GO TO 8000 */

    for (i = 0; i < *n; ++i) {
        p[i] = 0;
        ip[i] = 0;
    }

    /* ... HANDLE THE FIRST SET OF POINTS UNTIL SOME ADJACENT POINTS ARE FOUND */

    first = 0;

L20:
    p[first] = first + 1;
    if (ia[first + 1] - ia[first] > 1)
        goto L40;

    /* ... SEARCH FOR NEXT ENTRY THAT HAS NOT BEEN MARKED */

    if (first == *n-1)
        goto L130;

    ibgn = first + 1;
    for (i = ibgn; i < *n; ++i) {
        if (p[i] == 0) {
            first = i;
            goto L20;
        }
    }
    goto L130;

    /* ... FIRST SET OF ADJACENT POINTS FOUND */

L40:
    next = 0;
    last = 0;
    ip[0] = first + 1;

    /* ... LOOP OVER LABELED POINTS INDICATED IN THE STACK STORED IN */
    /* ... THE ARRAY IP */

L50:
    k = ip[next] - 1;
    curtyp = p[k];
    nxttyp = -curtyp;
    ibgn = ia[k] - 1;
    iend = ia[k + 1] - 1;
    for (i = ibgn; i < iend; ++i) {
        j = ja[i] - 1;
        typ = p[j];
        if (j == k)
            continue;

        /* ================================================================== */

        /*     THE FOLLOWING IS A FIVE WAY CASE STATEMENT DEALING WITH THE */
        /*     LABELING OF THE ADJACENT NODE. */

        /* ... CASE I.  IF THE ADJACENT NODE HAS ALREADY BEEN LABELED WITH */
        /*              LABEL EQUAL TO NXTTYP, THEN SKIP TO THE NEXT ADJACENT NODE. */

        else if (typ == nxttyp)
            continue;

        /* ... CASE II.  IF THE ADJACENT NODE HAS NOT BEEN LABELED YET LABEL */
        /*               IT WITH NXTTYP AND ENTER IT IN THE STACK */

        else if (typ == 0) {
            ++last;
            ip[last] = j + 1;
            p[j] = nxttyp;
            continue;
        }

        /* ... CASE III.  IF THE ADJACENT NODE HAS ALREADY BEEN LABELED WITH */
        /*                OPPOSITE COLOR AND THE SAME FATHER SEED, THEN THERE */
        /*                IS AN IRRECOVERABLE COLOR CONFLICT. */

        else if (typ == curtyp) { /* ...... TYPE CONFLICT */
            *ier = 201;
            return 0;
        }

        /* ... CASE IV.  IF THE ADJACENT NODE HAS THE RIGHT COLOR AND A DIFFERENT */
        /*               FATHER NODE, THEN CHANGE ALL NODES OF THE YOUNGEST FATHE */
        /*               NODE TO POINT TO THE OLDEST FATHER SEED AND RETAIN THE */
        /*               SAME COLORS. */

        else if (typ * nxttyp >= 1) {
            old = min(abs(typ),abs(nxttyp));
            young = max(abs(typ),abs(nxttyp));
            for (j = young-1; j < *n; ++j) {
                if (abs(p[j]) == young)
                    p[j] = old*p[j] >= 0 ? old : -old;
            }
            curtyp = p[k];
            nxttyp = -curtyp;
            continue;
        }

        /* ... CASE V.  IF THE ADJACENT NODE HAS THE WRONG COLOR AND A DIFFERENT */
        /*              FATHER NODE, THEN CHANGE ALL NODES OF THE YOUNGEST FATHER */
        /*              NODE TO POINT TO THE OLDEST FATHER NODE ALONG WITH */
        /*              CHANGING THEIR COLORS.  SINCE UNTIL THIS TIME THE */
        /*              YOUNGEST FATHER NODE TREE HAS BEEN INDEPENDENT NO OTHER */
        /*              COLOR CONFLICTS WILL ARISE FROM THIS CHANGE. */

        else {
            old = min(abs(typ),abs(nxttyp));
            young = max(abs(typ),abs(nxttyp));
            for (j = young-1; j < *n; ++j) {
                if (abs(p[j]) == young)
                    p[j] = old*p[j] <= 0 ? old : -old;
            }
            curtyp = p[k];
            nxttyp = -curtyp;
            continue;
        }

        /* ... END OF CASE STATEMENT */

        /* ================================================================== */
    }

    /* ... ADVANCE TO NEXT NODE IN THE STACK */

    ++next;
    if (next <= last)
        goto L50;

    /* ... ALL NODES IN THE STACK HAVE BEEN REMOVED */

    /* ... CHECK FOR NODES NOT LABELED.  IF ANY ARE FOUND */
    /* ... START THE LABELING PROCESS AGAIN AT THE FIRST */
    /* ... NODE FOUND THAT IS NOT LABELED. */

    ibgn = first + 1;
    for (i = ibgn; i < *n; ++i) {
        if (p[i] == 0) {
            first = i;
            goto L20;
        }
    }

    /* =================================================================== */

    /* ... ALL NODES ARE NOW TYPED EITHER RED OR BLACK */

    /* ... GENERATE PERMUTATION VECTORS */

L130:
    nred = 0;
    *nblack = 0;
    for (i = 0; i < *n; ++i) {
        if (p[i] < 0) /* BLACK POINT */
        {
            ++(*nblack);
            j = *n - *nblack;
            ip[j] = i + 1;
            p[i] = j + 1;
        }
        else /* RED POINT */
        {
            ++nred;
            ip[nred-1] = i + 1;
            p[i] = nred;
        }
    }

    /* ... SUCCESSFUL RED-BLACK ORDERING COMPLETED */

    return 0;

    /* ........ ERROR TRAPS */

    /* ...... N .LE. 0 */

    /* 8000    IER = 200 */
    /*        GO TO 9000 */
} /* prbndx_ */

/* Subroutine */
int prsblk_(integer *nb, integer *nr, integer *ia, integer *ja, doublereal *a, doublereal *ur, doublereal *vb)
{
    /* Local variables */
    static integer i, j, jaj, inr;
    static doublereal uri, sum;
    static integer ibgn, iend;

/* ... COMPUTE A BLACK-RS SWEEP ON A RED VECTOR INTO A BLACK VECTOR */

/* ... PARAMETER LIST: */

/*         NB      NUMBER OF BLACK POINTS */
/*         NR      NUMBER OF RED POINTS */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          UR     ESTIMATE OF RED SOLUTION VECTOR */
/*          VB     OUTPUT: PRESENT ESTIMATE OF BLACK SOLUTION */
/*                    VECTOR */

    if (itcom1_1.isym == 0)
        goto L30;

    /*     *************** NON - SYMMETRIC SECTION ********************** */

    for (i = 0; i < *nb; ++i) {
        inr = i + *nr;
        ibgn = ia[inr] - 1;
        iend = ia[inr + 1] - 1;
        sum = vb[i];
        if (ibgn >= iend)
            continue;

        for (j = ibgn; j < iend; ++j) {
            jaj = ja[j] - 1;
            sum -= a[j] * ur[jaj];
        }
        vb[i] = sum;
    }
    return 0;

    /*     ***************** SYMMETRIC SECTION ************************** */

L30:
    for (i = 0; i < *nr; ++i) {
        ibgn = ia[i] - 1;
        iend = ia[i + 1] - 1;
        uri = ur[i];
        for (j = ibgn; j < iend; ++j) {
            jaj = ja[j] - *nr - 1;
            vb[jaj] -= a[j] * uri;
        }
    }

    return 0;
} /* prsblk_ */

/* Subroutine */
int prsred_(integer *nb, integer *nr, integer *ia, integer *ja, doublereal *a, doublereal *ub, doublereal *vr)
{
    /* Local variables */
    static integer ii, jj;
    static doublereal sum;
    static integer jajj, ibgn, iend;

/* ... COMPUTES A RED-RS SWEEP ON A BLACK VECTOR INTO A RED VECTOR. */
/*                                                                  */
/* ... PARAMETER LIST:                                              */
/*                                                                  */
/*         NB      NUMBER OF BLACK POINTS (unused!?)                */
/*         NR      NUMBER OF RED POINTS                             */
/*         IA,JA   INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION   */
/*         A       D.P. ARRAY OF SPARSE MATRIX REPRESENTATION       */
/*         UB      PRESENT ESTIMATE OF BLACK SOLUTION VECTOR        */
/*         VR      OUTPUT: PRESENT ESTIMATE OF RED SOLUTION VECTOR  */

    for (ii = 0; ii < *nr; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        if (ibgn >= iend)
            continue;

        sum = vr[ii];
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - *nr - 1;
            sum -= a[jj] * ub[jajj];
        }
        vr[ii] = sum;
    }

    return 0;
} /* prsred_ */

/* Subroutine */
int pssor1_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *u,
            doublereal *rhs, doublereal *fr, doublereal *br)
{
    /* Local variables */
    static integer i, ii, jj;
    static doublereal uii, sum, omm1;
    static integer jajj, ibgn, iend;

/*     ... COMPUTES COMPLETE SSOR SWEEP ON U.  U IS OVERWRITTEN */
/*     ... WITH THE NEW ITERANT, FR AND BR WILL CONTAIN */
/*     ... THE FORWARD AND BACKWARD RESIDUALS ON OUTPUT. */

/* ... PARAMETER LIST: */

/*          N      ORDER OF SYSTEM */
/*          OMEGA  RELAXATION FACTOR */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          U      ESTIMATE OF SOLUTION */
/*          RHS    RIGHT HAND SIDE OF MATRIX PROBLEM */
/*          FR,BR  OUTPUT: FORWARD AND BACKWARD RESIDUALS RESPECTIVELY */

    omm1 = itcom3_1.omega - 1.;
    if (itcom1_1.isym == 0)
        goto L40;

    /*     *************** NON - SYMMETRIC SECTION ********************** */

    /*     ... FORWARD SWEEP */

    for (ii = 0; ii < *n; ++ii) {
        br[ii] = u[ii];
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        sum = rhs[ii];
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            sum -= a[jj] * u[jajj];
        }
        uii = itcom3_1.omega * sum - omm1 * u[ii];
        fr[ii] = uii - u[ii];
        u[ii] = uii;
    }
    goto L90;

    /*     ***************** SYMMETRIC SECTION ************************** */

    /*     ... FORWARD SWEEP */

L40:
    for (ii = 0; ii < *n; ++ii) {
        br[ii] = u[ii];
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        sum = rhs[ii];
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            sum -= a[jj] * u[jajj];
        }
        uii = itcom3_1.omega * sum - omm1 * u[ii];
        fr[ii] = uii - u[ii];
        u[ii] = uii;
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            rhs[jajj] -= a[jj] * uii;
        }
    }

    /*     ... BACKWARD SWEEP */

L90:
    for (i = 0; i < *n; ++i) {
        ii = *n - i - 1;
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        uii = rhs[ii];
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            uii -= a[jj] * u[jajj];
        }
        u[ii] = itcom3_1.omega * uii - omm1 * u[ii];
        br[ii] = u[ii] - br[ii];
    }

    return 0;
} /* pssor1_ */

/* Subroutine */
int pstop_(integer *n, doublereal *u, doublereal *dnrm, doublereal *ccon, integer *iflag, logical *q1)
{
    /* Local variables */
    static doublereal tl, tr, con;
    static doublereal uold;

/*     THIS SUBROUTINE PERFORMS A TEST TO SEE IF THE ITERATIVE */
/*     METHOD HAS CONVERGED TO A SOLUTION INSIDE THE ERROR */
/*     TOLERANCE, ZETA. */

/* ... PARAMETER LIST: */

/*          N      ORDER OF SYSTEM */
/*          U      PRESENT SOLUTION ESTIMATE */
/*          DNRM   INNER PRODUCT OF PSEUDO-RESIDUALS AT PRECEDING */
/*                    ITERATION */
/*          CON    STOPPING TEST PARAMETER (= CCON) */
/*          IFLAG  STOPPING TEST INTEGER FLAG */
/*                    IFLAG = 0,  SOR ITERATION ZERO */
/*                    IFLAG = 1,  NON-RS METHOD */
/*                    IFLAG = 2,  RS METHOD */
/*          Q1     STOPPING TEST LOGICAL FLAG */

    con = *ccon;
    itcom2_1.halt = FALSE_;

    /*     SPECIAL PROCEDURE FOR ZEROTH ITERATION */

    if (itcom1_1.in < 1) {
        *q1 = FALSE_;
        itcom3_1.udnm = 1.;
        itcom3_1.stptst = 1e3;
        if (*iflag <= 0)
            return 0;
    }

    /* ... TEST IF UDNM NEEDS TO BE RECOMPUTED */

    if (!*q1 && (itcom1_1.in <= 5 || itcom1_1.in % 5 == 0)) {
        uold = itcom3_1.udnm;
        itcom3_1.udnm = itpackddot_(n, u, &c__1, u, &c__1);
        if (itcom3_1.udnm == 0.)
            itcom3_1.udnm = 1.;

        if (itcom1_1.in > 5 && abs(itcom3_1.udnm - uold) <= itcom3_1.udnm * itcom3_1.zeta)
            *q1 = TRUE_;
    }

    /* ... COMPUTE STOPPING TEST */

    tr = sqrt(itcom3_1.udnm);
    tl = 1.;
    if (con == 1.)
        goto L40;

    if (*iflag != 2) {
        tl = sqrt(*dnrm);
        tr *= 1. - con;
    }
    else {
        tl = sqrt(*dnrm * 2.);
        tr *= 1. - con * con;
    }
L40:
    itcom3_1.stptst = tl / tr;
    if (tl >= tr * itcom3_1.zeta)
        return 0;

    itcom2_1.halt = TRUE_;

    return 0;
} /* pstop_ */

doublereal pvtbv_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *v)
{
    /* Local variables */
    static integer ii, jj;
    static doublereal sum;
    static integer jajj, ibgn, iend;
    static doublereal sumr;

/*     THIS FUNCTION COMPUTES  (V**T)*A*V. */

/* ... PARAMETER LIST: */

/*          N      DIMENSION OF MATRIX */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          V      D.P. VECTOR OF LENGTH N */

    sum = 0.;
    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        if (ibgn >= iend)
            continue;

        sumr = 0.;
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            sumr -= a[jj] * v[jajj];
        }
        sum += v[ii] * sumr;
    }

    if (itcom1_1.isym == 0)
        sum *= 2.;

    return sum;
} /* pvtbv_ */

/* Subroutine */
int sbagn_(integer *n, integer *nz, integer *ia, integer *ja, doublereal *a,
           integer *iwork, integer *level, integer *nout, integer* ierr)
{
    /* Local variables */
    static integer i, j, ier, ntn, nto, now, nadd;

    (void)level; (void)nout;
/* ... THE ROUTINES SBINI, SBSIJ, AND SBEND CREATE A SPARSE */
/*     MATRIX STRUCTURE BY MEANS OF A LINKED LIST WHICH IS */
/*     DESTROYED BY SBEND. SBAGN CREATES A NEW LINKED LIST */
/*     SO THAT ELEMENTS MAY BE ADDED TO THE MATRIX AFTER SBEND */
/*     HAS BEEN CALLED. SBAGN SHOULD BE CALLED WITH THE APPRO- */
/*     PRIATE PARAMETERS, AND THEN SBSIJ AND SBEND CAN BE CALLED */
/*     TO ADD THE ELEMENTS AND COMPLETE THE SPARSE MATRIX STRUC- */
/*     TURE. */

/* ... PARAMETER LIST: */

/*           N       ORDER OF THE SYSTEM */
/*           NZ      MAXIMUM NUMBER OF NON-ZERO ELEMENTS */
/*                   IN THE SYSTEM */
/*           IA, JA  INTEGER ARRAYS OF THE SPARSE */
/*                   MATRIX STRUCTURE */
/*           A       D.P. ARRAY OF THE SPARSE MATRIX */
/*                   STRUCTURE */
/*           IWORK   WORK ARRAY OF DIMENSION NZ */
/*           LEVEL   OUTPUT LEVEL CONTROL (= LEVELL) */
/*           NOUT  OUTPUT FILE NUMBER */
/*           IER     ERROR FLAG (= IERR). POSSIBLE RETURNS ARE */
/*                      IER = 0, SUCCESSFUL COMPLETION */
/*                          = 703, NZ TOO SMALL - NO MORE */
/*                                 ELEMENTS CAN BE ADDED */

    now = ia[*n] - 1;
    nadd = *nz - now;
    ier = 0;
    if (nadd <= 0)
        ier = 703;

    if (ier != 0)
        goto L90;

    /* ... SHIFT ELEMENTS OF A AND JA DOWN AND ADD ZERO FILL */

    nto = now;
    ntn = *nz;
    for (i = 0; i < now; ++i) {
        --nto; --ntn;
        ja[ntn] = ja[nto];
        a[ntn] = a[nto];
    }
    for (i = 0; i < nadd; ++i) {
        ja[i] = 0;
        a[i] = 0.;
    }

    /* ... UPDATE IA TO REFLECT DOWNWARD SHIFT IN A AND JA */

    for (i = 0; i <= *n; ++i)
        ia[i] += nadd;

    /* ... CREATE LINKED LIST */

    for (i = nadd; i < *nz; ++i)
        iwork[i] = i + 2;

    for (i = 0; i < nadd; ++i)
        iwork[i] = 0;

    for (i = 0; i < *n; ++i) {
        j = ia[i + 1] - 2;
        iwork[j] = -i - 1;
    }

    /* ... INDICATE IN LAST POSITION OF IA HOW MANY SPACES */
    /*     ARE LEFT IN A AND JA FOR ADDITION OF ELEMENTS */

    ia[*n] = nadd;
    return 0;

    /* ... ERROR RETURN */

L90:
    *ierr = ier;
    return 0;
} /* sbagn_ */

/* Subroutine */
int sbelm_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *rhs, integer *iw,
           doublereal *rw, doublereal *tol, integer *isym, integer *level, integer *nout, integer *ier)
{
    /* Local variables */
    static doublereal di;
    static integer ii, jj, kk, ibgn, iend, jjdi, icnt;

    (void)level; (void)nout;
/* ... SBELM IS DESIGNED TO REMOVE ROWS AND COLUMNS OF THE MATRIX */
/* ... WHERE DABS(A(I,J))/A(I,I) .LE. TOL FOR J = 1 TO N AND A(I,I) */
/* ... .GT. 0. THIS IS TO TAKE CARE OF MATRICES ARISING */
/* ... FROM FINITE ELEMENT DISCRETIZATIONS OF PDE^S WITH DIRICHLET */
/* ... BOUNDARY CONDITIONS.  ANY SUCH ROWS AND CORRESPONDING COLUMNS */
/* ... ARE THEN SET TO THE IDENTITY AFTER CORRECTING RHS. */

/* ... PARAMETER LIST: */

/*          N      DIMENSION OF MATRIX */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          RHS    RIGHT HAND SIDE OF MATRIX PROBLEM */
/*          IW,RW  WORK ARRAYS OF LENGTH N */
/*          TOL    TOLERANCE FACTOR */
/*          ISYM   FLAG FOR TYPE OF STORAGE FOR SYSTEM */
/*                 (0: SYMMETRIC, 1:NONSYMMETRIC) */
/*          LEVEL  PRINTING SWITCH FOR ERROR CONDITION */
/*          NOUT OUTPUT TAPE NUMBER */
/*          IER    ERROR FLAG: NONZERO VALUE ON RETURN MEANS */
/*                    101 : DIAGONAL ENTRY NOT POSITIVE */
/*                    102 : THERE IS NO DIAGONAL ENTRY IN ROW */

/* ********************************************************************** */

/*     UPDATE.  SBELM HAS BEEN REWRITTEN TO SPEED UP THE LOCATION OF */
/*              OF ROWS WHICH ARE TO BE ELIMINATED.  THIS IS DONE BY */
/*              FIRST STORING THE LARGEST ELEMENT OF EACH ROW IN */
/*              THE ARRAY RW.  THE DIAGONAL ENTRY IS THEN COMPARED */
/*              WITH THE CORRESPONDING ELEMENT IN RW.  IF IT IS */
/*              DECIDED TO ELIMINATE THE ROW THEN IT IS MARKED FOR */
/*              ELIMINATION. */

/*              WHEN A ROW IS TO BE ELIMINATED ITS DIAGONAL ENTRY */
/*              IS STORED IN  RW  AND  IW IS MARKED BY A NONZERO */
/*              (WHICH IS THIS ROW NUMBER) */

/*              ROWS WHICH HAVE ONLY DIAGONAL ENTRIES ARE NOT */
/*              ALTERED. */

/* ********************************************************************* */

    /*        IF (N .GE. 1) GO TO 10 */
    /*           IER = 100 */
    /*           RETURN */
    /* 10     CONTINUE */

    /* ... STORE THE LARGEST (DABSOLUTE VALUE) OFF DIAGONAL ENTRY FOR */
    /* ... ROW II IN RW(II). */

    *ier = 0;
    icnt = 0;
    for (ii = 0; ii < *n; ++ii) {
        rw[ii] = 0.;
        iw[ii] = 0;
    }
    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        if (ibgn >= iend)
            goto L140;

        for (jj = ibgn; jj < iend; ++jj) {
            kk = ja[jj] - 1;
            if (kk == ii)
                continue;

            rw[ii] = max(rw[ii],abs(a[jj]));
            if (*isym != 0)
                continue;

            rw[kk] = max(rw[kk],abs(a[jj]));
        }
    }

    /* ... FOR II = 1 TO N FIND THE DIAGONAL ENTRY IN ROW II */

    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        for (jj = ibgn; jj < iend; ++jj) {
            if (ja[jj] != ii + 1)
                continue;

            di = a[jj];
            jjdi = jj + 1;
            if (di > 0.)
                goto L50;

            *ier = 101;
            return 0;
        }
        goto L140;
L50:

        /* ... CHECK THE SIZE OF THE LARGEST OFF DIAGONAL ELEMENT */
        /* ... ( STORED IN RW(II) ) AGAINST THE DIAGONAL ELEMENT DII. */

        if (rw[ii] == 0.) {
            if (1. / di > *tol)
                continue;
        }
        else if (rw[ii] / di > *tol)
            continue;

        /* ... THE OFF DIAGONAL ELEMENTS ARE SMALL COMPARED TO THE DIAGONAL */
        /* ... THEREFORE MARK IT FOR ELIMINATION AND PERFORM INITIAL PROCESSING */

        ++icnt;
        iw[ii] = ii + 1;
        rw[ii] = di;
        a[jjdi - 1] = 1.;
        rhs[ii] /= di;
    }

    /* ... ELIMINATE THE ROWS AND COLUMNS INDICATED BY THE NONZERO */
    /* ... ENTRIES IN IW.  THERE ARE ICNT OF THEM */

    if (icnt == 0)
        return 0;

    /* ... THE ELIMINATION IS AS FOLLOWS: */

    /*     FOR II = 1 TO N DO */
    /*        IF ( IW(II) .NE. 0 ) THEN */
    /*           SET DIAGONAL VALUE TO 1.0  ( ALREADY DONE ) */
    /*           SET RHS(II) = RHS(II) / RW(II)   ( ALREADY DONE ) */
    /*           FIND NONZERO OFFDIAGONAL ENTRIES  KK */
    /*           IF ( IW(KK) .EQ. 0 ) FIX UP RHS(KK)  WHEN USING SYMMETRIC ST */
    /*           SET A(II,KK) = 0.0 */
    /*        ELSE ( I.E.  IW(II) .EQ. 0  ) */
    /*           FIND NONZERO OFFDIAGONAL ENTRIES   KK */
    /*           IF ( IW(KK) .NE. 0 ) FIX UP RHS(II) */
    /*                                AND SET A(II,KK) = 0.0 */
    /*        END IF */
    /*     END DO */

    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        if (iw[ii] == 0)
            goto L100;

        /* ... THE II-TH ROW IS TO BE ELIMINATED */

        for (jj = ibgn; jj < iend; ++jj) {
            kk = ja[jj] - 1;
            if (kk == ii)
                continue;

            if (iw[kk] == 0 && *isym == 0)
                rhs[kk] -= a[jj] * rhs[ii];

            a[jj] = 0.;
        }
        continue;

        /* ... THE II-TH ROW IS KEPT.  CHECK THE OFF-DIAGONAL ENTRIES */

L100:
        for (jj = ibgn; jj < iend; ++jj) {
            kk = ja[jj] - 1;
            if (kk != ii && iw[kk] != 0) {
                rhs[ii] -= a[jj] * rhs[kk];
                a[jj] = 0.;
            }
        }
    }

    return 0;

    /* ... ERROR TRAPS -- NO DIAGONAL ENTRY IN ROW II (ROW MAY BE EMPTY). */

L140:
    *ier = 102;

    return 0;
} /* sbelm_ */

/* Subroutine */
int sbend_(integer *n, integer *nz, integer *ia, integer *ja, doublereal *a, integer *iwork)
{
    /* Local variables */
    static integer i, l, jaj;
    static doublereal val;
    static integer top, ideg, link, next, hlink, mhlink, ohlink, nulink, maxtop;

/* *********************************************************************** */

/*     SBEND IS THE THIRD OF A SUITE OF SUBROUTINES TO AID THE */
/*     USER TO CONSTRUCT THE  IA, JA, A DATA STRUCTURE USED IN */
/*     ITPACK. */

/*     SBEND RESTRUCTURES THE LINKED LIST DATA STRUCTURE BUILT BY */
/*     SBINI AND SBSIJ INTO THE FINAL DATA STRUCTURE REQUIRE BY */
/*     ITPACK.  THE RESTRUCTURING CAN TAKE PLACE IN THE MINIMUM */
/*     AMOUNT OF MEMORY REQUIRED TO HOLD THE NONZERO STRUCTURE OF */
/*     THE SPARSE MATRIX BUT WILL RUN QUICKER IF MORE STORAGE */
/*     IS ALLOWED. */

/*     SBEND IS BASED ON SUBROUTINE BUILD OF THE SPARSE MATRIX */
/*     PACKAGE SPARSPAK DEVELOPED BY ALAN GEORGE AND JOSEPH LUI */
/*     OF THE UNIVERSITY OF WATERLOO, WATERLOO, ONTARIO. */

/* ... PARAMETERS */

/* ...... INPUT */

/*     N       THE ORDER OF THE LINEAR SYSTEM */

/*     NZ      THE LENGTH OF THE ARRAYS JA, IWORK, AND A. */

/* ...... INPUT/OUTPUT */

/*     IA      INTEGER ARRAY OF LENGTH N+1.  THE FIRST N ENTRIES */
/*             POINT TO THE BEGINNING OF THE LINKED LIST FOR EACH */
/*             ROW.  IA(N+1)-1 IS THE TOP OF THE LINKED LISTS */
/*             CONTAINED IN JA, IWORK, AND A.  ON OUTPUT IA WILL */
/*             POINT TO THE FIRST ENTRY OF EACH ROW IN THE FINAL */
/*             DATA STRUCTURE. */

/*     JA      INTEGER ARRAY OF LENGTH NZ.  ON INPUT JA STORES THE */
/*             COLUMN NUMBERS OF THE NONZERO ENTRIES AS INDICATED */
/*             BY THE LINKED LISTS.  ON OUTPUT JA STORES THE */
/*             COLUMN NUMBERS IN ROW ORDERED FORM. */

/*     A       D.P. ARRAY OF LENGTH NZ.  ON INPUT A STORES THE */
/*             VALUE OF THE NOZERO ENTRIES AS INDICATED BY THE */
/*             LINKED LISTS.  ON OUTPUT A STORES THE VALUES IN */
/*             ROW ORDERED FORM. */

/*     IWORK    INTEGER ARRAY OF LENGTH NZ.  ON INPUT IWORK STORES THE */
/*             THE LINKS OF THE LINKED LISTS.  ON OUTPUT IT IS */
/*             DESTROYED. */

/* *********************************************************************** */

    /* ... INITIALIZATION */

    /* ...... THE VARIABLES NEXT AND TOP RESPECTIVELY POINT TO THE */
    /*        NEXT AVAILABLE ENTRY FOR THE FINAL DATA STRUCTURE AND */
    /*        THE TOP OF THE REMAINDER OF THE LINKED LISTS. */

    next = 0;
    top = ia[*n];
    maxtop = *nz - ia[*n];

    /* *********************************************************************** */

    /* ... CONVERT EACH ROW INTO FINAL FORM */

    for (i = 0; i < *n; ++i) {
        ideg = 0;
        nulink = ia[i] - 1;

        /* ... LOOP OVER EACH NODE IN THE LINKED LIST OF ROW I */

L10:
        link = nulink;
        if (link < 0)
            goto L80;

        nulink = iwork[link] - 1;
        jaj = ja[link];
        val = a[link];

        /* ... CHECK TO SEE IF A COLLISION BETWEEN THE LINKED LISTS */
        /*     AND THE FINAL FORM HAS OCCURRED. */

        if (next >= top && link != top)
            goto L20;

        /* ... COLLISION HAS NOT OCCURRED.  FREE THE SPACE FOR THE TRIPLE */
        /*     (JA(LINK), A(LINK), IWORK(LINK)) */

        ja[link] = 0;
        a[link] = 0.;
        iwork[link] = 0;

        /* ... SPECIAL CASE TO MOVE  TOP  DOWN IF LINK .EQ. TOP */

        if (link == top)
            goto L60;

        goto L70;

        /* *********************************************************************** */

        /* ... COLLISION HAS OCCURRED.  CLEAR OFF SOME SPACE FOR THE CURRENT */
        /*     ENTRY BY MOVING THE TRIPLE ( JA(TOP),A(TOP),IWORK(TOP) ) */
        /*     DOWNWARDS TO THE FREED TRIPLE ( JA(LINK),A(LINK),IWORK(LINK) ). */
        /*     THEN ADJUST THE LINK FIELDS. */

        /* ...... PATCH UP THE LINKED LIST FOR THE CURRENT ROW I.  THEN */
        /*        TRAVERSE THE LINKED LIST CONTAINING TOP UNTIL THE POINTER */
        /*        POINTER BACK TO IA IS FOUND. */

L20:
        ia[i] = link + 1;
        hlink = top;

L30:
        hlink = iwork[hlink] - 1;
        if (hlink >= 0)
            goto L30;

        /* ...... NOW FOLLOW THE LINKED LIST BACK TO TOP KEEPING TRACK */
        /*        OF THE OLD LINK. */

        /* ......... SPECIAL CASE IF IA(-HLINK) = TOP */

        mhlink = -hlink - 2;
        if (ia[mhlink] != top + 1)
            goto L40;

        iwork[link] = iwork[top];
        ja[link] = ja[top];
        a[link] = a[top];
        ia[mhlink] = link + 1;
        if (nulink == top)
            nulink = link;

        goto L60;

        /* ......... USUAL CASE. */

L40:
        hlink = ia[mhlink] - 1;
L50:
        ohlink = hlink;
        hlink = iwork[ohlink] - 1;
        if (hlink != top)
            goto L50;

        iwork[link] = iwork[top];
        ja[link] = ja[top];
        a[link] = a[top];
        if (ohlink != link)
            iwork[ohlink] = link + 1;

        if (nulink == top)
            nulink = link;

        /* ... COLLAPSE TOP OF LINK LIST BY AS MUCH AS POSSIBLE */

L60:
        while (++top < maxtop && iwork[top] == 0) ;

        /* *********************************************************************** */

        /* ... PUT THE CURRENT TRIPLE INTO THE FINAL DATA STRUCTURE */

L70:
        ja[next] = jaj;
        a[next] = val;
        ++next;
        ++ideg;
        goto L10;

        /* ... FINAL STRUCTURE FOR ROW I IS COMPLETE.  LINKED LIST IS */
        /*     DESTROYED AND WILL BE RECAPTURED AS NECESSARY BY THE */
        /*     LOOP ON LABEL 60 */

L80:
        ia[i] = ideg;
    }

    /* *********************************************************************** */

    /* ... FINALIZE THE DATA STRUCTURE BY BUILDING THE FINAL VERSION OF */
    /*     IA. */

    l = ia[0] + 1;
    ia[0] = 1;
    for (i = 0; i < *n; ++i) {
        ideg = ia[i + 1];
        ia[i + 1] = l;
        l += ideg;
    }

    /* ... FINAL IA, JA, A DATA STRUCTURE BUILT. */

    return 0;
} /* sbend_ */

/* Subroutine */
int sbini_(integer *n, integer *nz, integer *ia, integer *ja, doublereal *a, integer *iwork)
{
    /* Local variables */
    static integer i;

/* *********************************************************************** */

/*     SBINI IS THE FIRST OF A SUITE OF THREE SUBROUTINES TO AID */
/*     THE USER TO CONSTRUCT THE IA, JA, A DATA STRUCTURE USED */
/*     IN ITPACK. */

/*     SBINI INITIALIZES THE ARRAYS IA, JA, IWORK, AND A.  THE OTHER */
/*     SUBROUTINES IN THE SUITE ARE SBSIJ ( WHICH BUILDS A LINKED */
/*     LIST REPRESENTATION OF THE MATRIX STRUCTURE ) AND SBEND ( WHICH */
/*     RESTRUCTURE THE LINKED LIST FORM INTO THE FINAL FORM ). */

/* ... PARAMETERS */

/* ...... INPUT */

/*     N          THE ORDER OF THE LINEAR SYSTEM */

/*     NZ         THE MAXIMUM NUMBER OF NONZEROES ALLOWED IN THE */
/*                LINEAR SYSTEM. */

/* ...... OUTPUT */

/*     IA         INTEGER ARRAY OF LENGTH N+1.  SBINI SETS THIS ARRAY */
/*                TO -I FOR I = 1 THRU N.  IA(N+1) IS SET TO NZ. */

/*     JA         INTEGER ARRAY OF LENGTH NZ.  INITIALIZED TO ZERO HERE. */

/*     A          D.P. ARRAY OF LENGTH NZ.  INITIALIZED TO ZERO HERE. */

/*     IWORK       INTEGER ARRAY OF LENGTH NZ.  INITIALIZED TO ZERO HERE. */

/* *********************************************************************** */

    for (i = 0; i < *n; ++i)
        ia[i] = -i - 1;

    ia[*n] = *nz;

    ivfill_(nz, ja, &c__0);
    ivfill_(nz, iwork, &c__0);
    vfill_(nz, a, &c_b21);

    return 0;
} /* sbini_ */

/* Subroutine */
int sbsij_(integer *n, integer *nz, integer *ia, integer *ja, doublereal *a,
           integer *iwork, integer *ii, integer *jj, doublereal * vall,
           integer *mode, integer *level, integer *nout, integer* ierr)
{
    /* Local variables */
    static integer i, j, ier;
    static doublereal val;
    static integer link;
    static doublereal temp;
    static integer next;

    (void)nz; (void)level; (void)nout;

/* *********************************************************************** */

/*     SBSIJ IS THE SECOND OF A SUITE OF THREE SUBROUTINES TO AID IN */
/*     THE CONSTRUCTION OF THE IA, JA, A DATA STRUCTURE USED IN */
/*     ITPACK. */

/*     SBSIJ TAKES THE INDIVIDUAL ENTRIES OF THE SPARSE MATRIX AS */
/*     GIVEN TO IT AT EACH CALL VIA  (I,J,VAL) AND INSERTS IT INTO */
/*     A LINKED LIST REPRESENTATION OF THE SPARSE MATRIX. */

/*     EACH ROW OF THE SPARSE MATRIX IS ASSOCIATED WITH A CIRCULAR */
/*     LINKED LIST BEGINNING AT IA(I).  THE LAST ENTERED ELEMENT IN */
/*     EACH LIST POINTS BACK TO IA(I) WITH THE VALUE -I.  THE LINKS */
/*     ARE STORED IN THE ARRAY IWORK, WHILE JA AND A STORE THE COLUMN */
/*     NUMBER AND VALUE IN PARALLEL TO IWORK.  THE LINKED LISTED ARE */
/*     STORED BEGINNING AT ENTRY NZ AND WORKING BACKWARDS TOWARDS 1. */

/* ... PARAMETERS */

/* ...... INPUT */

/*     N       THE ORDER OF THE LINEAR SYSTEM */

/*     NZ      THE LENGTH OF THE ARRAYS  JA, A, AND IWORK */

/*     I, J    THE ROW AND COLUMN NUMBERS OF THE ENTRY OF THE SPARSE */
/*             LINEAR SYSTEM TO BE ENTERED IN THE DATA STRUCTURE(=II,JJ) */

/*     VAL     THE NONZERO VALUE ASSOCIATED WITH (I,J)  (= VALL) */

/*     MODE    IF THE (I,J) ENTRY HAS ALREADY BEEN SET, MODE SPECIFIES */
/*             THE WAY IN WHICH THE ENTRY IS TO BE TREATED. */
/*             IF   MODE .LT. 0  LET THE VALUE REMAIN AS IS */
/*                       .EQ. 0  RESET IT TO THE NEW VALUE */
/*                       .GT. 0  ADD THE NEW VALUE TO THE OLD VALUE */

/*     NOUT  OUTPUT FILE NUMBER */

/*     LEVEL   OUTPUT FILE SWITCH */

/* ... INPUT/OUTPUT */

/*     IA      INTEGER ARRAY OF LENGTH N+1.  THE FIRST N ENTRIES */
/*             POINT TO THE BEGINNING OF THE LINKED LIST FOR EACH */
/*             ROW.  IA(N+1) POINTS TO THE NEXT ENTRY AVAILABLE FOR */
/*             STORING THE CURRENT ENTRY INTO THE LINKED LIST. */

/*     JA      INTEGER ARRAY OF LENGTH NZ.  JA STORES THE COLUMN */
/*             NUMBERS OF THE NONZERO ENTRIES. */

/*     A       D.P. ARRAY OF LENGTH NZ.  A STORES THE VALUE OF THE */
/*             NONZERO ENTRIES. */

/*     IWORK   INTEGER ARRAY OF LENGTH NZ. IWORK STORES THE LINKS. */

/*     IER     ERROR FLAG.(= IERR)  POSSIBLE RETURNS ARE */
/*             IER =    0   SUCCESSFUL COMPLETION */
/*                 =  700   ENTRY WAS ALREADY SET,  VALUE HANDLED */
/*                          AS SPECIFIED BY MODE. */
/*                 =  701   IMPROPER VALUE OF EITHER I OR J INDEX */
/*                 =  702   NO ROOM REMAINING, NZ TOO SMALL. */

/* *********************************************************************** */

    /* ... CHECK THE VALIDITY OF THE (I,J) ENTRY */

    i = *ii - 1;
    j = *jj - 1;
    val = *vall;
    ier = 0;
    if (i < 0 || i >= *n)
        ier = 701;

    if (j < 0 || j >= *n)
        ier = 701;

    if (ier != 0)
        goto L130;

    /* ... TRAVERSE THE LINK LIST POINTED TO BY IA(I) UNTIL EITHER */
    /* ... THE J ENTRY OR THE END OF THE LIST HAS BEEN FOUND. */

    link = ia[i] - 1;

    /* ...... SPECIAL CASE FOR THE FIRST ENTRY IN THE ROW */

    if (link >= 0)
        goto L30;

    next = ia[*n] - 1;
    if (next < 0)
        goto L110;

    ia[i] = next + 1;
    ja[next] = j + 1;
    a[next] = val;
    iwork[next] = -i - 1;
    ia[*n] = next;
    goto L130;

    /* ... FOLLOW THE LINK LIST UNTIL J OR THE END OF THE LIST IS FOUND */

L30:
    if (ja[link] == j + 1)
        goto L40;

    if (iwork[link] <= 0)
        goto L100;

    link = iwork[link] - 1;
    goto L30;

    /* : */
    /* ... ENTRY (I,J) ALREADY HAS BEEN SET.  RESET VALUE DEPENDING ON MODE */

L40:
    ier = 700;
    if (*mode < 0)
        goto L130;

    if (*mode < 1) {
        a[link] = val;
        goto L130;
    }
    temp = a[link] + val;
    a[link] = temp;
    goto L130;

    /* ... ENTRY (I,J) HAS NOT BEEN SET.  ENTER IT INTO THE LINKED LIST */

L100:
    next = ia[*n] - 1;
    if (next >= 0) {
        iwork[link] = next + 1;
        ja[next] = j + 1;
        a[next] = val;
        iwork[next] = -i - 1;
        ia[*n] = next;
        goto L130;
    }

    /* *********************************************************************** */

    /* ... ERROR TRAP FOR NO ROOM REMAINING */

L110:
    ier = 702;

L130:
    *ierr = ier;
    return 0;
} /* sbsij_ */

/* Subroutine */
int scal_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *rhs,
          doublereal *u, doublereal *d, integer *level, integer *nout, integer *ier)
{
    /* Local variables */
    static integer i, j;
    static doublereal di;
    static integer ii, jj, im1, jadd, jajj, ibgn, iend, jjpi;

    (void)level; (void)nout;
/* ... ORIGINAL MATRIX IS SCALED TO A UNIT DIAGONAL MATRIX.  RHS */
/* ... AND U ARE SCALED ACCORDINGLY.  THE MATRIX IS THEN SPLIT AND */
/* ... IA, JA, AND A RESHUFFLED. */

/* ... PARAMETER LIST: */

/*          N      DIMENSION OF MATRIX */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          RHS    RIGHT HAND SIDE OF MATRIX PROBLEM */
/*          U      LATEST ESTIMATE OF SOLUTION */
/*          D      OUTPUT VECTOR CONTAINING THE SQUARE ROOTS */
/*                    OF THE DIAGONAL ENTRIES */
/*          LEVEL  PRINTING SWITCH FOR ERROR CONDITION */
/*          NOUT OUTPUT TAPE NUMBER */
/*          IER    ERROR FLAG: ON RETURN NONZERO VALUES MEAN */
/*                    401 : THE ITH DIAGONAL ELEMENT IS .LE. 0. */
/*                    402 : NO DIAGONAL ELEMENT IN ROW I */

    /* ... EXTRACT SQUARE ROOT OF THE DIAGONAL OUT OF A AND SCALE U AND RHS */

    *ier = 0;
    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        for (jj = ibgn; jj < iend; ++jj) {
            if (ja[jj] != ii + 1)
                continue;

            di = a[jj];
            if (di > 0.)
                goto L70;

            if (di != 0.) {
                *ier = 401;
                return 0;
            }
            *ier = 401;
            return 0;
        }
        *ier = 402;
        return 0;

L70:
        di = sqrt((abs(di)));
        rhs[ii] /= di;
        u[ii] *= di;
        d[ii] = di;
    }

    /* ... SHIFT MATRIX TO ELIMINATE DIAGONAL ENTRIES */

    if (*n > 1)
    for (i = 0; i < *n; ++i) {
        im1 = i;
        ii = *n - i - 1;
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        jadd = ibgn + iend + 1;
        for (j = ibgn; j < iend; ++j) {
            jj = jadd - j - 2;
            jjpi = jj + im1;
            if (ja[jj] == ii + 1)
                im1 = i + 1;

            a[jjpi] = a[jj];
            ja[jjpi] = ja[jj];
        }
        ia[ii + 1] = ia[ii + 1] + i;
    }

    ia[0] += *n;

    /* ... SCALE SHIFTED MATRIX AND STORE D ARRAY IN FIRST N ENTRIES OF A */

    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        di = d[ii];
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj] - 1;
            a[jj] /= di * d[jajj];
        }
        a[ii] = di;
    }

    return 0;
} /* scal_ */

/* Subroutine */
int sum3_(integer *n, doublereal *c1, doublereal *x1, doublereal *c2, doublereal *x2, doublereal *c3, doublereal *x3)
{
    /* Local variables */
    static integer i;

/* ... COMPUTES X3 = C1*X1 + C2*X2 + C3*X3 */

/* ... PARAMETER LIST: */

/*          N        INTEGER LENGTH OF VECTORS X1, X2, X3 */
/*          C1,C2,C3 D.P. CONSTANTS */
/*          X1,X2,X3 D.P. VECTORS SUCH THAT */
/*                   X3(I) = C1*X1(I) + C2*X2(I) + C3*X3(I) */
/*                   X3(I) = C1*X1(I) + C2*X2(I)  IF C3 = 0. */

    if (*n <= 0)
        return 0;

    if (*c3 != 0.) {
        for (i = 0; i < *n; ++i)
            x3[i] = *c1 * x1[i] + *c2 * x2[i] + *c3 * x3[i];
        return 0;
    }

    for (i = 0; i < *n; ++i)
        x3[i] = *c1 * x1[i] + *c2 * x2[i];

    return 0;
} /* sum3_ */

doublereal tau_(integer *ii)
{
    /* Initialized data */
    static doublereal t[8] = { 1.5,1.8,1.85,1.9,1.94,1.96,1.975,1.985 };

/* ... THIS SUBROUTINE SETS TAU(II) FOR THE SOR METHOD. */

    /*          II     NUMBER OF TIMES PARAMETERS HAVE BEEN CHANGED */

    if (*ii <= 8)
        return t[*ii - 1];
    else
        return 1.992;
} /* tau_ */

doublereal timer_(real* dummy)
{
/* ... TIMER IS A ROUTINE TO RETURN THE EXECUTION TIME IN SECONDS. */

/* ********************************************* */
/* **                                         ** */
/* **   THIS ROUTINE IS NOT PORTABLE.         ** */
/* **                                         ** */
/* ********************************************* */

    (void)dummy;
    return (doublereal)time(0L);
} /* timer_ */

logical tstchg_(integer *ibmth)
{
    /* Local variables */
    static integer ip;

/*     THIS FUNCTION PERFORMS A TEST TO DETERMINE IF PARAMETERS */
/*     SHOULD BE CHANGED FOR SEMI-ITERATION ACCELERATED METHODS. */

/* ... PARAMETER LIST: */

/*          IBMTH  INDICATOR OF BASIC METHOD BEING ACCELERATED BY SI */
/*                      IBMTH = 1,   JACOBI */
/*                            = 2,   REDUCED SYSTEM */
/*                            = 3,   SSOR */

    ip = itcom1_1.in - itcom1_1.is;
    if (*ibmth == 2)
        ip <<= 1;

    if (itcom1_1.in == 0)
        goto L10;

    if (ip < 3)
        goto L20;

    itcom3_1.qa = sqrt(abs(itcom3_1.delnnm / itcom3_1.delsnm));
    itcom3_1.qt = sqrt(abs(pow_di(&itcom3_1.rrr, &ip))) * 2. / (pow_di(&itcom3_1.rrr, &ip) + 1.);
    if (itcom3_1.qa >= 1. || itcom3_1.qa < pow_dd(&itcom3_1.qt, &itcom3_1.ff))

        goto L20;

    /* ... TEST PASSES -- CHANGE PARAMETERS */

L10:
    return TRUE_;

    /* ... TEST FAILS -- DO NOT CHANGE PARAMETERS */

L20:
    return FALSE_;
} /* tstchg_ */

/* Subroutine */
int unscal_(integer *n, integer *ia, integer *ja, doublereal *a, doublereal *rhs, doublereal *u, doublereal *d)
{
    /* Local variables */
    static doublereal di;
    static integer ii, jj, is, jajj, ibgn, iend, jjpi, inew;

/* ... THIS SUBROUTINE REVERSES THE PROCESS OF SCAL. */

/* ... PARAMETER LIST: */

/*          N      DIMENSION OF MATRIX */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          RHS    RIGHT HAND SIDE OF MATRIX PROBLEM */
/*          U      LATEST ESTIMATE OF SOLUTION */
/*          D      VECTOR CONTAINING THE SQUARE ROOTS */
/*                    OF THE DIAGONAL ENTRIES */

    /* ... EXTRACT DIAGONAL FROM SCALED A AND UNSCALE U AND RHS */

    for (ii = 0; ii < *n; ++ii) {
        di = a[ii];
        u[ii] /= di;
        rhs[ii] *= di;
        d[ii] = di;
    }

    /* ... UNSCALE A */

    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        di = d[ii];
        for (jj = ibgn; jj < iend; ++jj) {
            jajj = ja[jj];
            a[jj] *= di * d[jajj-1];
        }
    }

    /* ... INSERT DIAGONAL BACK INTO A */

    for (ii = 0; ii < *n; ++ii) {
        ibgn = ia[ii] - 1;
        iend = ia[ii + 1] - 1;
        is = *n - ii - 1;
        inew = ibgn - is - 1;
        a[inew] = d[ii] * d[ii];
        ja[inew] = ii + 1;
        if (is != 0)
            for (jj = ibgn; jj < iend; ++jj) {
                jjpi = jj - is;
                a[jjpi] = a[jj];
                ja[jjpi] = ja[jj];
            }
        ia[ii] = inew + 1;
    }

    return 0;
} /* unscal_ */

/* Subroutine */
int vevmw_(integer *n, doublereal *v, doublereal *w)
{
    /* Local variables */
    static integer i, m;

/* ... VEVMW COMPUTES V = V - W */

/* ... PARAMETER LIST: */

/*          N      INTEGER LENGTH OF VECTORS V AND W */
/*          V      D.P. VECTOR */
/*          W      D.P. VECTOR SUCH THAT   V(I) = V(I) - W(I) */

    if (*n <= 0)
        return 0;

    m = *n % 4;

    for (i = 0; i < m; ++i)
        v[i] -= w[i];

    for (i = m; i < *n; i += 4) {
        v[i] -= w[i];
        v[i + 1] -= w[i + 1];
        v[i + 2] -= w[i + 2];
        v[i + 3] -= w[i + 3];
    }
    return 0;
} /* vevmw_ */

/* Subroutine */
int vevpw_(integer *n, doublereal *v, doublereal *w)
{
    /* Local variables */
    static integer i, m;

/* ... VPW COMPUTES    V = V + W */

/* ... PARAMETER LIST: */

/*          N      LENGTH OF VECTORS V AND W */
/*          V      D.P. VECTOR */
/*          W      D.P. VECTOR SUCH THAT   V(I) = V(I) + W(I) */

    if (*n <= 0)
        return 0;

    m = *n % 4;
    for (i = 0; i < m; ++i)
        v[i] += w[i];

    for (i = m; i < *n; i += 4) {
        v[i] += w[i];
        v[i + 1] += w[i + 1];
        v[i + 2] += w[i + 2];
        v[i + 3] += w[i + 3];
    }

    return 0;
} /* vevpw_ */

/* Subroutine */
int vfill_(integer *n, doublereal *v, doublereal *val)
{
    /* Local variables */
    static integer i, m;

/*     FILLS A VECTOR, V, WITH A CONSTANT VALUE, VAL. */

/* ... PARAMETER LIST: */

/*          N      INTEGER LENGTH OF VECTOR V */
/*          V      D.P. VECTOR */
/*          VAL    D.P. CONSTANT THAT FILLS FIRST N LOCATIONS OF V */

    if (*n <= 0)
        return 0;

    /*     CLEAN UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 10 */

    m = *n % 10;
    for (i = 0; i < m; ++i)
        v[i] = *val;

    for (i = m; i < *n; i += 10) {
        v[i] = *val;
        v[i + 1] = *val;
        v[i + 2] = *val;
        v[i + 3] = *val;
        v[i + 4] = *val;
        v[i + 5] = *val;
        v[i + 6] = *val;
        v[i + 7] = *val;
        v[i + 8] = *val;
        v[i + 9] = *val;
    }

    return 0;
} /* vfill_ */

/* Subroutine */
int vout_(integer *n, doublereal *v, integer *iswt, integer *nout)
{
/*     THIS SUBROUTINE EFFECTS PRINTING OF RESIDUAL AND SOLUTION */
/*     VECTORS - CALLED FROM PERROR */

/* ... PARAMETER LIST: */

/*          V      VECTOR OF LENGTH N */
/*          ISWT   LABELLING INFORMATION */
/*          NOUT OUTPUT DEVICE NUMBER */

    /*        IF (N .LE. 0) RETURN */
  (void)n; (void)v; (void)iswt; (void)nout;

    return 0;
} /* vout_ */

/* Subroutine */
int wevmw_(integer *n, doublereal *v, doublereal *w)
{
    /* Local variables */
    static integer i, m;

/* ... WEVMW COMPUTES W = V - W */

/* ... PARAMETER LIST: */

/*          N      INTEGER LENGTH OF VECTORS V AND W */
/*          V      D.P. VECTOR */
/*          W      D.P. VECTOR SUCH THAT   W(I) = V(I) - W(I) */

    if (*n <= 0)
        return 0;

    m = *n % 4;
    for (i = 0; i < m; ++i)
        w[i] = v[i] - w[i];

    for (i = m; i < *n; i += 4) {
        w[i] = v[i] - w[i];
        w[i + 1] = v[i + 1] - w[i + 1];
        w[i + 2] = v[i + 2] - w[i + 2];
        w[i + 3] = v[i + 3] - w[i + 3];
    }

    return 0;
} /* wevmw_ */

/* Subroutine */
int zbrent_(integer *n, doublereal *tri, doublereal *eps, integer *nsig,
            doublereal *aa, doublereal *bb, integer *maxfnn, integer *ier)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal half = .5;
    static doublereal one = 1.;
    static doublereal three = 3.;
    static doublereal ten = 10.;

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    static doublereal a, b, c, d, e, p, q, r, s, t, fa, fb, fc;
    static integer ic;
    static doublereal rm, tol, rone, temp;
    static integer maxfn;

/*   MODIFIED IMSL ROUTINE NAME   - ZBRENT */

/* ----------------------------------------------------------------------- */

/*   COMPUTER            - CDC/SINGLE */

/*   LATEST REVISION     - JANUARY 1, 1978 */

/*   PURPOSE             - ZERO OF A FUNCTION WHICH CHANGES SIGN IN A */
/*                           GIVEN INTERVAL (BRENT ALGORITHM) */

/*   USAGE               - CALL ZBRENT (F,EPS,NSIG,A,B,MAXFN,IER) */

/*   ARGUMENTS    TRI    - A TRIDIAGONAL MATRIX OF ORDER N */
/*                EPS    - FIRST CONVERGENCE CRITERION (INPUT).  A ROOT, */
/*                           B, IS ACCEPTED IF DABS(F(B)) IS LESS THAN OR */
/*                           EQUAL TO EPS.  EPS MAY BE SET TO ZERO. */
/*                NSIG   - SECOND CONVERGENCE CRITERION (INPUT).  A ROOT, */
/*                           B, IS ACCEPTED IF THE CURRENT APPROXIMATION */
/*                           AGREES WITH THE TRUE SOLUTION TO NSIG */
/*                           SIGNIFICANT DIGITS. */
/*                A,B    - ON INPUT, THE USER MUST SUPPLY TWO POINTS, A */
/*                           AND B, SUCH THAT F(A) AND F(B) ARE OPPOSITE */
/*                           IN SIGN. (= AA, BB) */
/*                           ON OUTPUT, BOTH A AND B ARE ALTERED.  B */
/*                           WILL CONTAIN THE BEST APPROXIMATION TO THE */
/*                           ROOT OF F. SEE REMARK 1. */
/*                MAXFN  - ON INPUT, MAXFN SHOULD CONTAIN AN UPPER BOUND */
/*                           ON THE NUMBER OF FUNCTION EVALUATIONS */
/*                           REQUIRED FOR CONVERGENCE.  ON OUTPUT, MAXFN */
/*                           WILL CONTAIN THE ACTUAL NUMBER OF FUNCTION */
/*                           EVALUATIONS USED. (= MAXFNN) */
/*                IER    - ERROR PARAMETER. (OUTPUT) */
/*                         TERMINAL ERROR */
/*                           IER = 501 INDICATES THE ALGORITHM FAILED TO */
/*                             CONVERGE IN MAXFN EVALUATIONS. */
/*                           IER = 502 INDICATES F(A) AND F(B) HAVE THE */
/*                             SAME SIGN. */

/*   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32 */
/*                       - SINGLE/H36,H48,H60 */

/*   NOTATION            - INFORMATION ON SPECIAL NOTATION AND */
/*                           CONVENTIONS IS AVAILABLE IN THE MANUAL */
/*                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP */

/*   REMARKS  1.  LET F(X) BE THE CHARACTERISTIC FUNCTION OF THE MATRIX */
/*                TRI EVALUATED AT X. FUNCTION DETERM EVALUATES F(X). */
/*                ON EXIT FROM ZBRENT, WHEN IER=0, A AND B SATISFY THE */
/*                FOLLOWING, */
/*                F(A)*F(B) .LE.0, */
/*                DABS(F(B)) .LE. DABS(F(A)), AND */
/*                EITHER DABS(F(B)) .LE. EPS OR */
/*                DABS(A-B) .LE. MAX(DABS(B),0.1)*10.0**(-NSIG). */
/*                THE PRESENCE OF 0.1 IN THIS ERROR CRITERION CAUSES */
/*                LEADING ZEROES TO THE RIGHT OF THE DECIMAL POINT TO BE */
/*                COUNTED AS SIGNIFICANT DIGITS. SCALING MAY BE REQUIRED */
/*                IN ORDER TO ACCURATELY DETERMINE A ZERO OF SMALL */
/*                MAGNITUDE. */
/*            2.  ZBRENT IS GUARANTEED TO REACH CONVERGENCE WITHIN */
/*                K = (DLOG((B-A)/D)+1.0)**2 FUNCTION EVALUATIONS WHERE */
/*                  D=MIN(OVER X IN (A,B) OF */
/*                    MAX(DABS(X),0.1)*10.0**(-NSIG)). */
/*                THIS IS AN UPPER BOUND ON THE NUMBER OF EVALUATIONS. */
/*                RARELY DOES THE ACTUAL NUMBER OF EVALUATIONS USED BY */
/*                ZBRENT EXCEED DSQRT(K). D CAN BE COMPUTED AS FOLLOWS, */
/*                  P = DBLE(AMIN1(DABS(A),DABS(B))) */
/*                  P = DMAX1(0.1,P) */
/*                  IF ((A-0.1)*(B-0.1).LT.0.0) P = 0.1 */
/*                  D = P*10.0**(-NSIG) */

/*   COPYRIGHT           - 1977 BY IMSL, INC. ALL RIGHTS RESERVED. */

/*   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN */
/*                           APPLIED TO THIS CODE. NO OTHER WARRANTY, */
/*                           EXPRESSED OR IMPLIED, IS APPLICABLE. */

/* ----------------------------------------------------------------------- */

    a = *aa;
    b = *bb;
    maxfn = *maxfnn;
    *ier = 0;
    i__1 = -(*nsig);
    t = pow_di(&ten, &i__1);
    ic = 2;
    fa = determ_(n, tri, &a);
    fb = determ_(n, tri, &b);
    s = b;

    /*                                  TEST FOR SAME SIGN */

    if (fa * fb > zero)
        goto L110;

L10:
    c = a;
    fc = fa;
    d = b - c;
    e = d;
L20:
    if (abs(fc) < abs(fb)) {
        a = b;
        b = c;
        c = a;
        fa = fb;
        fb = fc;
        fc = fa;
    }

    tol = t * max(abs(b),.1);
    rm = (c - b) * half;

    /*                                  TEST FOR FIRST CONVERGENCE CRITERIA */

    if (abs(fb) <= *eps)
        goto L80;

    /*                                  TEST FOR SECOND CONVERGENCE CRITERIA */

    if (abs(c - b) <= tol)
        goto L80;

    /*                                  CHECK EVALUATION COUNTER */

    if (ic >= maxfn)
        goto L90;

    /*                                  IS BISECTION FORCED */

    if (abs(e) < tol)
        goto L60;

    if (abs(fa) <= abs(fb))
        goto L60;

    s = fb / fa;

    /*                                  LINEAR INTERPOLATION */

    if (a == c) {
        p = (c - b) * s;
        q = one - s;
    }

    /*                                  INVERSE QUADRATIC INTERPOLATION */

    else {
        q = fa / fc;
        r = fb / fc;
        rone = r - one;
        p = s * ((c - b) * q * (q - r) - (b - a) * rone);
        q = (q - one) * rone * (s - one);
    }

    if (p > zero)
        q = -q;

    if (p < zero)
        p = -p;

    s = e;
    e = d;

    /*                                  IF DABS(P/Q).GE.75*DABS(C-B) THEN */
    /*                                     FORCE BISECTION */

    if (p + p >= three * rm * q)
        goto L60;

    /*                                  IF DABS(P/Q).GE..5*DABS(S) THEN FORCE */
    /*                                     BISECTION. S = THE VALUE OF P/Q */
    /*                                     ON THE STEP BEFORE THE LAST ONE */

    if (p + p < abs(s * q)) {
        d = p / q;
        goto L70;
    }

    /*                                  BISECTION */

L60:
    e = rm;
    d = e;

    /*                                  INCREMENT B */

L70:
    a = b;
    fa = fb;
    temp = d;
    if (abs(temp) <= half * tol) {
        d__1 = half * tol;
        temp = d_sign(&d__1, &rm);
    }
    b += temp;
    s = b;
    fb = determ_(n, tri, &s);
    ++ic;
    if (fb * fc <= zero)
        goto L20;
    else
        goto L10;

    /*                                  CONVERGENCE OF B */

L80:
    a = c;
    maxfn = ic;
    goto L130;

    /*                                  MAXFN EVALUATIONS */

L90:
    *ier = 501;
    a = c;
    maxfn = ic;
    goto L130;

    /*                                  TERMINAL ERROR - F(A) AND F(B) HAVE */
    /*                                  THE SAME SIGN */

L110:
    *ier = 502;
    maxfn = ic;
L130:
    *aa = a;
    *bb = b;
    *maxfnn = maxfn;
    return 0;
} /* zbrent_ */
