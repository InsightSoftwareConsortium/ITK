/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itpack.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/**
 * \file itpack.cxx
 * \brief Routines from ITPACK numerical library.
 *
 * In this file routines from ITPACK are defined in the itk::fem::itpack
 * namespace. This is done by calling the original ITPACK code
 * (file dsrc2c.c) which was converted from Fortran using f2c.
 * See "ftp://netlib.bell-labs.com/netlib/f2c" for more info.
 * 
 * Besides that we also define some functions that the ITPACK uses and should
 * normally be defined in f2c.lib. But since we don't have that library
 * we redefine these function here, so that ITPACK code is happy.
 *
 * ITPACK was converted with the following command line options:
 *    f2c -C++P dsrc2c.f
 *
 * This produced two files:
 *   - dsrc2c.c was converted to itpack_dsrc2c.c by removing the
 *     #include "f2c" and all "#ifdef __cplusplus extern C  #endif" parts.
 *   - dsrc2c.P was used to obtain function prototypes which are stored
 *     in itpack.h file.
 *
 * To use ITPACK in your ITK code, simply include the header "itpack.h",
 * and link to the corresponding library.
 *
 * \note All ITPACK functions reside in namespace itpack.
 */




/* Turn off warnings in f2c generated code */
#if defined(_MSC_VER)
#if defined(__ICL)
#pragma warning(disable: 239 264 1011 )
#else
#pragma warning(disable: 4101 4244 4554 4756 4723)
#endif
#endif


/*
 * Required includes and typedefs for code created with f2c
 */
#include "itkNumericTraits.h"
#include "itpack_f2c.h"
#include <math.h>

namespace itk {
namespace fem {
namespace itpack {

/**
 * \namespace itk::fem::itpack
 * \brief ITPACK numeric library is stored in this namespace.
 */



/*
 * Definitions of above functions
 */

double getDoublePrecision()
{
  return NumericTraits<double>::min();
}

double pow_dd(doublereal *ap, doublereal *bp)
{
  return(pow(*ap, *bp) );
}

double pow_di(doublereal *ap, integer *bp)
{
double pow, x;
integer n;
unsigned long u;

pow = 1;
x = *ap;
n = *bp;

if(n != 0)
    {
    if(n < 0)
        {
        n = -n;
        x = 1/x;
        }
    for(u = n; ; )
        {
        if(u & 01)
            pow *= x;
        if(u >>= 1)
            x *= x;
        else
            break;
        }
    }
return(pow);
}

double d_lg10(doublereal *x)
{
return( 0.43429448190325182765 * log(*x) );
}

double sqrt(doublereal x)
{
  return( ::sqrt(x) );
}

double log(doublereal x)
{
  return( ::log(x) );
}

double d_sign(doublereal *a, doublereal *b)
{
double x;
x = (*a >= 0 ? *a : - *a);
return( *b >= 0 ? x : -x);
}

integer i_sign(integer *a, integer *b)
{
integer x;
x = (*a >= 0 ? *a : - *a);
return( *b >= 0 ? x : -x);
}


/*
 * The following couple of functions have something to
 * do with I/O. Since we don't want ITPACK to output
 * anything, the functions do nothing.
 */
integer do_fio(ftnint *, char *, ftnlen )
{
  return 0;
}

integer e_wsfe(void)
{
  return 0;
}

integer s_wsfe(cilist *)
{
  return 0;
}

doublereal etime_(float *tarray)
{
  tarray[0]=0.0;
  tarray[1]=0.0;
  return 0.0;
}




/*
 * Required macros for for code obtained with f2c
 */
#define TRUE_ (1)
#define FALSE_ (0)

#define max(a,b) ((a) >= (b) ? (a) : (b))
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define abs(x) ((x) >= 0 ? (x) : -(x))




/*
 * Original ITPACK code converted from Fortran with f2c
 */

//////////////////////////////////////////////////////////////////////////

/* ../dsrc2c.f -- translated by f2c (version 20020208).
   You must link the resulting object file with the libraries:
    -lf2c -lm   (in that order)
*/


/* Common Block Declarations */

struct {
    integer in, is, isym, itmax, level, nout;
} itcom1_;

#define itcom1_1 itcom1_

struct {
    logical adapt, betadt, caseii, halt, partad;
} itcom2_;

#define itcom2_1 itcom2_

struct {
    doublereal bdelnm, betab, cme, delnnm, delsnm, ff, gamma, omega, qa, qt, 
        rho, rrr, sige, sme, specr, spr, drelpr, stptst, udnm, zeta;
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

/* Subroutine */ int jcg_(integer *nn, integer *ia, integer *ja, doublereal *
    a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, 
    doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr)
{
    /* Format strings */
    static char fmt_10[] = "(\0020\002///1x,\002BEGINNING OF ITPACK SOLUTION\
 MODULE  JCG\002)";
    static char fmt_40[] = "(\0020\002,\002*** W A R N I N G ************\
\002/\0020\002,\002    IN ITPACK ROUTINE JCG\002/\002 \002,\002    RPARM(1) =\
\002,d10.3,\002 (ZETA)\002/\002 \002,\002    A VALUE THIS SMALL MAY HINDER C\
ONVERGENCE \002/\002 \002,\002    SINCE MACHINE PRECISION DRELPR =\002,d10.3/\
\002 \002,\002    ZETA RESET TO \002,d10.3)";
    static char fmt_60[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE JCG \002/\002 \002\
,\002    INVALID MATRIX DIMENSION, N =\002,i8)";
    static char fmt_80[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE JCG \002/\002 \002\
,\002    ERROR DETECTED IN SUBROUTINE  SBELM \002/\002 \002,\002    WHICH RE\
MOVES ROWS AND COLUMNS OF SYSTEM \002/\002 \002,\002    WHEN DIAGONAL ENTRY \
TOO LARGE  \002/\002 \002,\002    IER = \002,i5,5x,\002 RPARM(8) = \002,d10.\
3,\002 (TOL)\002)";
    static char fmt_100[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE JCG \002/\002 \002,\
\002    NOT ENOUGH WORKSPACE AT \002,i10/\002 \002,\002    SET IPARM(8) =\
\002,i10,\002 (NW)\002)";
    static char fmt_120[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE JCG \002/\002 \002,\
\002    ERROR DETECTED IN SUBROUTINE  PRBNDX\002/\002 \002,\002    WHICH COM\
PUTES THE RED-BLACK INDEXING\002/\002 \002,\002    IER = \002,i5,\002 IPARM(\
9) = \002,i5,\002 (NB)\002)";
    static char fmt_140[] = "(/10x,\002ORDER OF BLACK SUBSYSTEM = \002,i5\
,\002 (NB)\002)";
    static char fmt_150[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE JCG \002/\002 \002,\
\002    ERROR DETECTED IN SUBROUTINE  PERMAT\002/\002 \002,\002    WHICH DOE\
S THE RED-BLACK PERMUTATION\002/\002 \002,\002    IER = \002,i5)";
    static char fmt_180[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE JCG \002/\002 \002,\
\002    ERROR DETECTED IN SUBROUTINE  SCAL  \002/\002 \002,\002    WHICH SCA\
LES THE SYSTEM   \002/\002 \002,\002    IER = \002,i5)";
    static char fmt_200[] = "(///1x,\002IN THE FOLLOWING, RHO AND GAMMA AR\
E\002,\002 ACCELERATION PARAMETERS\002)";
    static char fmt_210[] = "(1x,\002CME IS THE ESTIMATE OF THE LARGEST EIGE\
NVALUE OF\002,\002 THE JACOBI MATRIX\002)";
    static char fmt_270[] = "(\0020\002,\002*** W A R N I N G ***********\
*\002/\0020\002,\002    IN ITPACK ROUTINE JCG\002/\002 \002,\002    FAILURE \
TO CONVERGE IN\002,i5,\002 ITERATIONS\002)";
    static char fmt_300[] = "(/1x,\002JCG  HAS CONVERGED IN \002,i5,\002 ITE\
RATIONS\002)";
    static char fmt_320[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE JCG \002/\002 \002,\
\002    ERROR DETECTED IN SUBROUTINE  PERMAT\002/\002 \002,\002    WHICH UND\
OES THE RED-BLACK PERMUTATION   \002/\002 \002,\002    IER = \002,i5)";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */

    /* Local variables */
    extern /* Subroutine */ int pjac_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *), scal_(integer *, 
        integer *, integer *, doublereal *, doublereal *, doublereal *, 
        doublereal *, integer *, integer *, integer *);
    static doublereal temp;
    static integer loop;
    static doublereal time1, time2;
    static real timi1, timj1, timi2, timj2;
    static integer n;
    extern /* Subroutine */ int itjcg_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *, doublereal *, 
        doublereal *, doublereal *, doublereal *), sbelm_(integer *, 
        integer *, integer *, doublereal *, doublereal *, integer *, 
        doublereal *, doublereal *, integer *, integer *, integer *, 
        integer *);
    static integer idgts;
    extern /* Subroutine */ int vfill_(integer *, doublereal *, doublereal *),
         dcopy_(integer *, doublereal *, integer *, doublereal *, integer 
        *);
    extern doublereal timer_(real *);
    static real dummy;
    static integer n3;
    extern /* Subroutine */ int vevmw_(integer *, doublereal *, doublereal *);
    static doublereal digit1, digit2;
    static integer itmax1, nb;
    extern /* Subroutine */ int echall_(integer *, integer *, integer *, 
        doublereal *, doublereal *, integer *, doublereal *, integer *), 
        pervec_(integer *, doublereal *, integer *), ivfill_(integer *, 
        integer *, integer *);
    static integer ierper;
    extern /* Subroutine */ int echout_(integer *, doublereal *, integer *), 
        permat_(integer *, integer *, integer *, doublereal *, integer *, 
        integer *, integer *, integer *, integer *, integer *), unscal_(
        integer *, integer *, integer *, doublereal *, doublereal *, 
        doublereal *, doublereal *);
    static integer ib1, ib2, ib3, ib4, ib5;
    extern /* Subroutine */ int prbndx_(integer *, integer *, integer *, 
        integer *, integer *, integer *, integer *, integer *, integer *),
         perror_(integer *, integer *, integer *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *, 
        doublereal *, integer *);
    static integer ier;
    static doublereal tol;

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___7 = { 0, 0, 0, fmt_40, 0 };
    static cilist io___12 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___14 = { 0, 0, 0, fmt_80, 0 };
    static cilist io___20 = { 0, 0, 0, fmt_100, 0 };
    static cilist io___23 = { 0, 0, 0, fmt_120, 0 };
    static cilist io___24 = { 0, 0, 0, fmt_140, 0 };
    static cilist io___25 = { 0, 0, 0, fmt_150, 0 };
    static cilist io___26 = { 0, 0, 0, fmt_180, 0 };
    static cilist io___27 = { 0, 0, 0, fmt_200, 0 };
    static cilist io___28 = { 0, 0, 0, fmt_210, 0 };
    static cilist io___33 = { 0, 0, 0, fmt_270, 0 };
    static cilist io___34 = { 0, 0, 0, fmt_300, 0 };
    static cilist io___36 = { 0, 0, 0, fmt_320, 0 };



/*     ITPACK 2C MAIN SUBROUTINE  JCG  (JACOBI CONJUGATE GRADIENT) */
/*     EACH OF THE MAIN SUBROUTINES: */
/*           JCG, JSI, SOR, SSORCG, SSORSI, RSCG, RSSI */
/*     CAN BE USED INDEPENDENTLY OF THE OTHERS */

/* ... FUNCTION: */

/*          THIS SUBROUTINE, JCG, DRIVES THE JACOBI CONJUGATE */
/*          GRADIENT ALGORITHM. */

/* ... PARAMETER LIST: */

/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX. (= NN) */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF */
/*                 THE SPARSE MATRIX REPRESENTATION. */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE */
/*                 MATRIX REPRESENTATION. */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE */
/*                 OF THE MATRIX PROBLEM. */
/*          U      INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U CONTAINS THE */
/*                 INITIAL GUESS TO THE SOLUTION. ON OUTPUT, IT CONTAINS */
/*                 THE LATEST ESTIMATE TO THE SOLUTION. */
/*          IWKSP  INTEGER VECTOR WORKSPACE OF LENGTH 3*N */
/*          NW     INPUT INTEGER.  LENGTH OF AVAILABLE WKSP.  ON OUTPUT, */
/*                 IPARM(8) IS AMOUNT USED. */
/*          WKSP   D.P. VECTOR USED FOR WORKING SPACE.  JACOBI CONJUGATE */
/*                 GRADIENT NEEDS THIS TO BE IN LENGTH AT LEAST */
/*                 4*N + 2*ITMAX,  IF ISYM = 0  (SYMMETRIC STORAGE) */
/*                 4*N + 4*ITMAX,  IF ISYM = 1  (NONSYMMETRIC STORAGE) */
/*                 HERE ITMAX = IPARM(1) AND ISYM = IPARM(5) */
/*                 (ITMAX IS THE MAXIMUM ALLOWABLE NUMBER OF ITERATIONS) */
/*          IPARM  INTEGER VECTOR OF LENGTH 12.  ALLOWS USER TO SPECIFY */
/*                 SOME INTEGER PARAMETERS WHICH AFFECT THE METHOD. */
/*          RPARM  D.P. VECTOR OF LENGTH 12. ALLOWS USER TO SPECIFY SOME */
/*                 D.P. PARAMETERS WHICH AFFECT THE METHOD. */
/*          IER    OUTPUT INTEGER.  ERROR FLAG. (= IERR) */

/* ... JCG SUBPROGRAM REFERENCES: */

/*          FROM ITPACK    BISRCH, CHGCON, DETERM, DFAULT, ECHALL, */
/*                         ECHOUT, EIGVNS, EIGVSS, EQRT1S, ITERM, TIMER, */
/*                         ITJCG, IVFILL, PARCON, PERMAT, */
/*                         PERROR, PERVEC, PJAC, PMULT, PRBNDX, */
/*                         PSTOP, QSORT, DAXPY, SBELM, SCAL, DCOPY, */
/*                         DDOT, SUM3, UNSCAL, VEVMW, VFILL, VOUT, */
/*                         WEVMW, ZBRENT */
/*          SYSTEM         DABS, DLOG10, DBLE(AMAX0), DMAX1, MOD, DSQRT */

/*     VERSION:  ITPACK 2C (MARCH 1982) */

/*     CODE WRITTEN BY:  DAVID KINCAID, ROGER GRIMES, JOHN RESPESS */
/*                       CENTER FOR NUMERICAL ANALYSIS */
/*                       UNIVERSITY OF TEXAS */
/*                       AUSTIN, TX  78712 */
/*                       (512) 471-1242 */

/*     FOR ADDITIONAL DETAILS ON THE */
/*          (A) SUBROUTINE SEE TOMS ARTICLE 1982 */
/*          (B) ALGORITHM  SEE CNA REPORT 150 */

/*     BASED ON THEORY BY:  DAVID YOUNG, DAVID KINCAID, LOU HAGEMAN */

/*     REFERENCE THE BOOK:  APPLIED ITERATIVE METHODS */
/*                          L. HAGEMAN, D. YOUNG */
/*                          ACADEMIC PRESS, 1981 */

/*     ************************************************** */
/*     *               IMPORTANT NOTE                   * */
/*     *                                                * */
/*     *      WHEN INSTALLING ITPACK ROUTINES ON A      * */
/*     *  DIFFERENT COMPUTER, RESET SOME OF THE VALUES  * */
/*     *  IN  SUBROUTNE DFAULT.   MOST IMPORTANT ARE    * */
/*     *                                                * */
/*     *   DRELPR      MACHINE RELATIVE PRECISION       * */
/*     *   RPARM(1)    STOPPING CRITERION               * */
/*     *                                                * */
/*     *   ALSO CHANGE SYSTEM-DEPENDENT ROUTINE         * */
/*     *   SECOND USED IN TIMER                         * */
/*     *                                                * */
/*     ************************************************** */

/*     SPECIFICATIONS FOR ARGUMENTS */


/*     SPECIFICATIONS FOR LOCAL VARIABLES */


/* **** BEGIN: ITPACK COMMON */




/* **** END  : ITPACK COMMON */

/* ... VARIABLES IN COMMON BLOCK - ITCOM1 */

/*     IN     - ITERATION NUMBER */
/*     IS     - ITERATION NUMBER WHEN PARAMETERS LAST CHANGED */
/*     ISYM   - SYMMETRIC/NONSYMMETRIC STORAGE FORMAT SWITCH */
/*     ITMAX  - MAXIMUM NUMBER OF ITERATIONS ALLOWED */
/*     LEVEL  - LEVEL OF OUTPUT CONTROL SWITCH */
/*     NOUT   - OUTPUT UNIT NUMBER */

/* ... VARIABLES IN COMMON BLOCK - ITCOM2 */

/*     ADAPT  - FULLY ADAPTIVE PROCEDURE SWITCH */
/*     BETADT - SWITCH FOR ADAPTIVE DETERMINATION OF BETA */
/*     CASEII - ADAPTIVE PROCEDURE CASE SWITCH */
/*     HALT   - STOPPING TEST SWITCH */
/*     PARTAD - PARTIALLY ADAPTIVE PROCEDURE SWITCH */

/* ... VARIABLES IN COMMON BLOCK - ITCOM3 */

/*     BDELNM - TWO NORM OF B TIMES DELTA-SUPER-N */
/*     BETAB  - ESTIMATE FOR THE SPECTRAL RADIUS OF LU MATRIX */
/*     CME    - ESTIMATE OF LARGEST EIGENVALUE */
/*     DELNNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION N */
/*     DELSNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION S */
/*     FF     - ADAPTIVE PROCEDURE DAMPING FACTOR */
/*     GAMMA  - ACCELERATION PARAMETER */
/*     OMEGA  - OVERRELAXATION PARAMETER FOR SOR AND SSOR */
/*     QA     - PSEUDO-RESIDUAL RATIO */
/*     QT     - VIRTUAL SPECTRAL RADIUS */
/*     RHO    - ACCELERATION PARAMETER */
/*     RRR    - ADAPTIVE PARAMETER */
/*     SIGE   - PARAMETER SIGMA-SUB-E */
/*     SME    - ESTIMATE OF SMALLEST EIGENVALUE */
/*     SPECR  - SPECTRAL RADIUS ESTIMATE FOR SSOR */
/*     DRELPR - MACHINE RELATIVE PRECISION */
/*     STPTST - STOPPING PARAMETER */
/*     UDNM   - TWO NORM OF U */
/*     ZETA   - STOPPING CRITERION */

/* ... INITIALIZE COMMON BLOCKS */

    /* Parameter adjustments */
    --u;
    --rhs;
    --ia;
    --ja;
    --a;
    --iwksp;
    --wksp;
    --iparm;
    --rparm;

    /* Function Body */
    itcom1_1.level = iparm[2];
    itcom1_1.nout = iparm[4];
    if (itcom1_1.level >= 1) {
    io___1.ciunit = itcom1_1.nout;
    s_wsfe(&io___1);
    e_wsfe();
    }
    ier = 0;
    if (iparm[1] <= 0) {
    return 0;
    }
    n = *nn;
    if (iparm[11] == 0) {
    timj1 = timer_(&dummy);
    }
    if (itcom1_1.level >= 3) {
    goto L20;
    }
    echout_(&iparm[1], &rparm[1], &c__1);
    goto L30;
L20:
    echall_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iparm[1], &rparm[1], &c__1);
L30:
    temp = itcom3_1.drelpr * 500.;
    if (itcom3_1.zeta >= temp) {
    goto L50;
    }
    if (itcom1_1.level >= 1) {
    io___7.ciunit = itcom1_1.nout;
    s_wsfe(&io___7);
    do_fio(&c__1, (char *)&itcom3_1.zeta, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.drelpr, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&temp, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    itcom3_1.zeta = temp;
L50:
    time1 = rparm[9];
    time2 = rparm[10];
    digit1 = rparm[11];
    digit2 = rparm[12];

/* ... VERIFY N */

    if (n > 0) {
    goto L70;
    }
    ier = 11;
    if (itcom1_1.level >= 0) {
    io___12.ciunit = itcom1_1.nout;
    s_wsfe(&io___12);
    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L370;
L70:

/* ... REMOVE ROWS AND COLUMNS IF REQUESTED */

    if (iparm[10] == 0) {
    goto L90;
    }
    tol = rparm[8];
    ivfill_(&n, &iwksp[1], &c__0);
    vfill_(&n, &wksp[1], &c_b21);
    sbelm_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iwksp[1], &wksp[1], &tol, &
        itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L90;
    }
    if (itcom1_1.level >= 0) {
    io___14.ciunit = itcom1_1.nout;
    s_wsfe(&io___14);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&tol, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    goto L370;

/* ... INITIALIZE WKSP BASE ADDRESSES. */

L90:
    ib1 = 1;
    ib2 = ib1 + n;
    ib3 = ib2 + n;
    ib4 = ib3 + n;
    ib5 = ib4 + n;
    iparm[8] = (n << 2) + (itcom1_1.itmax << 1);
    if (itcom1_1.isym != 0) {
    iparm[8] += itcom1_1.itmax << 1;
    }
    if (*nw >= iparm[8]) {
    goto L110;
    }
    ier = 12;
    if (itcom1_1.level >= 0) {
    io___20.ciunit = itcom1_1.nout;
    s_wsfe(&io___20);
    do_fio(&c__1, (char *)&(*nw), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iparm[8], (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L370;

/* ... PERMUTE TO  RED-BLACK SYSTEM IF REQUESTED */

L110:
    nb = iparm[9];
    if (nb < 0) {
    goto L170;
    }
    n3 = n * 3;
    ivfill_(&n3, &iwksp[1], &c__0);
    prbndx_(&n, &nb, &ia[1], &ja[1], &iwksp[1], &iwksp[ib2], &itcom1_1.level, 
        &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L130;
    }
    if (itcom1_1.level >= 0) {
    io___23.ciunit = itcom1_1.nout;
    s_wsfe(&io___23);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L370;

/* ... PERMUTE MATRIX AND RHS */

L130:
    if (itcom1_1.level >= 2) {
    io___24.ciunit = itcom1_1.nout;
    s_wsfe(&io___24);
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    permat_(&n, &ia[1], &ja[1], &a[1], &iwksp[1], &iwksp[ib3], &itcom1_1.isym,
         &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L160;
    }
    if (itcom1_1.level >= 0) {
    io___25.ciunit = itcom1_1.nout;
    s_wsfe(&io___25);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L370;
L160:
    pervec_(&n, &rhs[1], &iwksp[1]);
    pervec_(&n, &u[1], &iwksp[1]);

/* ... SCALE LINEAR SYSTEM, U, AND RHS BY THE SQUARE ROOT OF THE */
/* ... DIAGONAL ELEMENTS. */

L170:
    vfill_(&iparm[8], &wksp[1], &c_b21);
    scal_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1], &
        itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L190;
    }
    if (itcom1_1.level >= 0) {
    io___26.ciunit = itcom1_1.nout;
    s_wsfe(&io___26);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L370;
L190:
    if (itcom1_1.level <= 2) {
    goto L220;
    }
    io___27.ciunit = itcom1_1.nout;
    s_wsfe(&io___27);
    e_wsfe();
    if (itcom2_1.adapt) {
    io___28.ciunit = itcom1_1.nout;
    s_wsfe(&io___28);
    e_wsfe();
    }
L220:
    if (iparm[11] != 0) {
    goto L230;
    }
    timi1 = timer_(&dummy);

/* ... COMPUTE INITIAL PSEUDO-RESIDUAL */

L230:
    dcopy_(&n, &rhs[1], &c__1, &wksp[ib2], &c__1);
    pjac_(&n, &ia[1], &ja[1], &a[1], &u[1], &wksp[ib2]);
    vevmw_(&n, &wksp[ib2], &u[1]);

/* ... ITERATION SEQUENCE */

    itmax1 = itcom1_1.itmax + 1;
    i__1 = itmax1;
    for (loop = 1; loop <= i__1; ++loop) {
    itcom1_1.in = loop - 1;
    if (itcom1_1.in % 2 == 1) {
        goto L240;
    }

/* ... CODE FOR THE EVEN ITERATIONS. */

/*     U           = U(IN)             WKSP(IB2) = DEL(IN) */
/*     WKSP(IB1)   = U(IN-1)           WKSP(IB3) = DEL(IN-1) */

    itjcg_(&n, &ia[1], &ja[1], &a[1], &u[1], &wksp[ib1], &wksp[ib2], &
        wksp[ib3], &wksp[ib4], &wksp[ib5]);

    if (itcom2_1.halt) {
        goto L280;
    }
    goto L250;

/* ... CODE FOR THE ODD ITERATIONS. */

/*     U           = U(IN-1)           WKSP(IB2) = DEL(IN-1) */
/*     WKSP(IB1)   = U(IN)             WKSP(IB3) = DEL(IN) */

L240:
    itjcg_(&n, &ia[1], &ja[1], &a[1], &wksp[ib1], &u[1], &wksp[ib3], &
        wksp[ib2], &wksp[ib4], &wksp[ib5]);

    if (itcom2_1.halt) {
        goto L280;
    }
L250:
    ;
    }

/* ... ITMAX HAS BEEN REACHED */

    if (iparm[11] != 0) {
    goto L260;
    }
    timi2 = timer_(&dummy);
    time1 = (doublereal) (timi2 - timi1);
L260:
    ier = 13;
    if (itcom1_1.level >= 1) {
    io___33.ciunit = itcom1_1.nout;
    s_wsfe(&io___33);
    do_fio(&c__1, (char *)&itcom1_1.itmax, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    if (iparm[3] == 0) {
    rparm[1] = itcom3_1.stptst;
    }
    goto L310;

/* ... METHOD HAS CONVERGED */

L280:
    if (iparm[11] != 0) {
    goto L290;
    }
    timi2 = timer_(&dummy);
    time1 = (doublereal) (timi2 - timi1);
L290:
    if (itcom1_1.level >= 1) {
    io___34.ciunit = itcom1_1.nout;
    s_wsfe(&io___34);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    e_wsfe();
    }

/* ... PUT SOLUTION INTO U IF NOT ALREADY THERE. */

L310:
    if (itcom1_1.in % 2 == 1) {
    dcopy_(&n, &wksp[ib1], &c__1, &u[1], &c__1);
    }

/* ... UNSCALE THE MATRIX, SOLUTION, AND RHS VECTORS. */

    unscal_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1]);

/* ... UN-PERMUTE MATRIX,RHS, AND SOLUTION */

    if (iparm[9] < 0) {
    goto L340;
    }
    permat_(&n, &ia[1], &ja[1], &a[1], &iwksp[ib2], &iwksp[ib3], &
        itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ierper);
    if (ierper == 0) {
    goto L330;
    }
    if (itcom1_1.level >= 0) {
    io___36.ciunit = itcom1_1.nout;
    s_wsfe(&io___36);
    do_fio(&c__1, (char *)&ierper, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    if (ier == 0) {
    ier = ierper;
    }
    goto L370;
L330:
    pervec_(&n, &rhs[1], &iwksp[ib2]);
    pervec_(&n, &u[1], &iwksp[ib2]);

/* ... OPTIONAL ERROR ANALYSIS */

L340:
    idgts = iparm[12];
    if (idgts < 0) {
    goto L350;
    }
    if (iparm[2] <= 0) {
    idgts = 0;
    }
    perror_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1], &digit1, &
        digit2, &idgts);

/* ... SET RETURN PARAMETERS IN IPARM AND RPARM */

L350:
    iparm[8] -= itcom1_1.itmax - itcom1_1.in << 1;
    if (iparm[11] != 0) {
    goto L360;
    }
    timj2 = timer_(&dummy);
    time2 = (doublereal) (timj2 - timj1);
L360:
    if (itcom1_1.isym != 0) {
    iparm[8] -= itcom1_1.itmax - itcom1_1.in << 1;
    }
    if (iparm[3] != 0) {
    goto L370;
    }
    iparm[1] = itcom1_1.in;
    iparm[9] = nb;
    rparm[2] = itcom3_1.cme;
    rparm[3] = itcom3_1.sme;
    rparm[9] = time1;
    rparm[10] = time2;
    rparm[11] = digit1;
    rparm[12] = digit2;

L370:
    *ierr = ier;
    if (itcom1_1.level >= 3) {
    echall_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iparm[1], &rparm[1], &
        c__2);
    }

    return 0;
} /* jcg_ */

/* Subroutine */ int jsi_(integer *nn, integer *ia, integer *ja, doublereal *
    a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, 
    doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr)
{
    /* Format strings */
    static char fmt_10[] = "(\0020\002///1x,\002BEGINNING OF ITPACK SOLUTION\
 MODULE  JSI\002)";
    static char fmt_40[] = "(\0020\002,\002*** W A R N I N G ************\
\002/\0020\002,\002    IN ITPACK ROUTINE JSI\002/\002 \002,\002    RPARM(1) =\
\002,d10.3,\002 (ZETA)\002/\002 \002,\002    A VALUE THIS SMALL MAY HINDER C\
ONVERGENCE \002/\002 \002,\002    SINCE MACHINE PRECISION DRELPR =\002,d10.3/\
\002 \002,\002    ZETA RESET TO \002,d10.3)";
    static char fmt_60[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE JSI \002/\002 \002\
,\002    INVALID MATRIX DIMENSION, N =\002,i8)";
    static char fmt_80[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE JSI \002/\002 \002\
,\002    ERROR DETECTED IN SUBROUTINE  SBELM \002/\002 \002,\002    WHICH RE\
MOVES ROWS AND COLUMNS OF SYSTEM \002/\002 \002,\002    WHEN DIAGONAL ENTRY \
TOO LARGE  \002/\002 \002,\002    IER = \002,i5,5x,\002 RPARM(8) = \002,d10.\
3,\002 (TOL)\002)";
    static char fmt_100[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE JSI \002/\002 \002,\
\002    NOT ENOUGH WORKSPACE AT \002,i10/\002 \002,\002    SET IPARM(8) =\
\002,i10,\002 (NW)\002)";
    static char fmt_120[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE JSI \002/\002 \002,\
\002    ERROR DETECTED IN SUBROUTINE  PRBNDX\002/\002 \002,\002    WHICH COM\
PUTES THE RED-BLACK INDEXING\002/\002 \002,\002    IER = \002,i5,\002 IPARM(\
9) = \002,i5,\002 (NB)\002)";
    static char fmt_140[] = "(/10x,\002ORDER OF BLACK SUBSYSTEM = \002,i5\
,\002 (NB)\002)";
    static char fmt_150[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE JSI \002/\002 \002,\
\002    ERROR DETECTED IN SUBROUTINE  PERMAT\002/\002 \002,\002    WHICH DOE\
S THE RED-BLACK PERMUTATION\002/\002 \002,\002    IER = \002,i5)";
    static char fmt_180[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE JSI \002/\002 \002,\
\002    ERROR DETECTED IN SUBROUTINE  SCAL  \002/\002 \002,\002    WHICH SCA\
LES THE SYSTEM   \002/\002 \002,\002    IER = \002,i5)";
    static char fmt_200[] = "(///1x,\002IN THE FOLLOWING, RHO AND GAMMA AR\
E\002,\002 ACCELERATION PARAMETERS\002)";
    static char fmt_260[] = "(\0020\002,\002*** W A R N I N G ***********\
*\002/\0020\002,\002    IN ITPACK ROUTINE JSI\002/\002 \002,\002    FAILURE \
TO CONVERGE IN\002,i5,\002 ITERATIONS\002)";
    static char fmt_290[] = "(/1x,\002JSI  HAS CONVERGED IN \002,i5,\002 ITE\
RATIONS\002)";
    static char fmt_310[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE JSI \002/\002 \002,\
\002    ERROR DETECTED IN SUBROUTINE  PERMAT\002/\002 \002,\002    WHICH UND\
OES THE RED-BLACK PERMUTATION   \002/\002 \002,\002    IER = \002,i5)";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(), do_fio(integer *, char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int scal_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *, doublereal *, integer *,
         integer *, integer *);
    static integer icnt;
    static doublereal temp;
    static integer loop;
    static doublereal time1, time2;
    static real timi1, timj1, timi2, timj2;
    static integer n;
    extern /* Subroutine */ int sbelm_(integer *, integer *, integer *, 
        doublereal *, doublereal *, integer *, doublereal *, doublereal *,
         integer *, integer *, integer *, integer *);
    static integer idgts;
    extern /* Subroutine */ int vfill_(integer *, doublereal *, doublereal *),
         dcopy_(integer *, doublereal *, integer *, doublereal *, integer 
        *);
    extern doublereal timer_(real *);
    extern /* Subroutine */ int itjsi_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *, doublereal *, 
        doublereal *, integer *);
    static real dummy;
    static integer n3;
    static doublereal digit1, digit2;
    static integer itmax1, nb;
    extern /* Subroutine */ int echall_(integer *, integer *, integer *, 
        doublereal *, doublereal *, integer *, doublereal *, integer *), 
        pervec_(integer *, doublereal *, integer *), ivfill_(integer *, 
        integer *, integer *);
    static integer ierper;
    extern /* Subroutine */ int echout_(integer *, doublereal *, integer *), 
        permat_(integer *, integer *, integer *, doublereal *, integer *, 
        integer *, integer *, integer *, integer *, integer *), unscal_(
        integer *, integer *, integer *, doublereal *, doublereal *, 
        doublereal *, doublereal *);
    static integer ib1, ib2, ib3;
    extern /* Subroutine */ int prbndx_(integer *, integer *, integer *, 
        integer *, integer *, integer *, integer *, integer *, integer *),
         perror_(integer *, integer *, integer *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *, 
        doublereal *, integer *);
    static integer ier;
    static doublereal tol;

    /* Fortran I/O blocks */
    static cilist io___39 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___45 = { 0, 0, 0, fmt_40, 0 };
    static cilist io___50 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___52 = { 0, 0, 0, fmt_80, 0 };
    static cilist io___56 = { 0, 0, 0, fmt_100, 0 };
    static cilist io___59 = { 0, 0, 0, fmt_120, 0 };
    static cilist io___60 = { 0, 0, 0, fmt_140, 0 };
    static cilist io___61 = { 0, 0, 0, fmt_150, 0 };
    static cilist io___62 = { 0, 0, 0, fmt_180, 0 };
    static cilist io___63 = { 0, 0, 0, fmt_200, 0 };
    static cilist io___69 = { 0, 0, 0, fmt_260, 0 };
    static cilist io___70 = { 0, 0, 0, fmt_290, 0 };
    static cilist io___72 = { 0, 0, 0, fmt_310, 0 };



/*     ITPACK 2C MAIN SUBROUTINE  JSI  (JACOBI SEMI-ITERATIVE) */
/*     EACH OF THE MAIN SUBROUTINES: */
/*           JCG, JSI, SOR, SSORCG, SSORSI, RSCG, RSSI */
/*     CAN BE USED INDEPENDENTLY OF THE OTHERS */

/* ... FUNCTION: */

/*          THIS SUBROUTINE, JSI, DRIVES THE JACOBI SEMI- */
/*          ITERATION ALGORITHM. */

/* ... PARAMETER LIST: */

/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX. (= NN) */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF */
/*                 THE SPARSE MATRIX REPRESENTATION. */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE */
/*                 MATRIX REPRESENTATION. */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE */
/*                 OF THE MATRIX PROBLEM. */
/*          U      INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U CONTAINS THE */
/*                 INITIAL GUESS TO THE SOLUTION. ON OUTPUT, IT CONTAINS */
/*                 THE LATEST ESTIMATE TO THE SOLUTION. */
/*          IWKSP  INTEGER VECTOR WORKSPACE OF LENGTH 3*N */
/*          NW     INPUT INTEGER.  LENGTH OF AVAILABLE WKSP.  ON OUTPUT, */
/*                 IPARM(8) IS AMOUNT USED. */
/*          WKSP   D.P. VECTOR USED FOR WORKING SPACE.  JACOBI SI */
/*                 NEEDS THIS TO BE IN LENGTH AT LEAST */
/*                 2*N */
/*          IPARM  INTEGER VECTOR OF LENGTH 12.  ALLOWS USER TO SPECIFY */
/*                 SOME INTEGER PARAMETERS WHICH AFFECT THE METHOD. */
/*          RPARM  D.P. VECTOR OF LENGTH 12.  ALLOWS USER TO SPECIFY SOME */
/*                 D.P. PARAMETERS WHICH AFFECT THE METHOD. */
/*          IER    OUTPUT INTEGER.  ERROR FLAG. (= IERR) */

/* ... JSI SUBPROGRAM REFERENCES: */

/*          FROM ITPACK   BISRCH, CHEBY, CHGSI, CHGSME, DFAULT, ECHALL, */
/*                        ECHOUT, ITERM, TIMER, ITJSI, IVFILL, PAR */
/*                        PERMAT, PERROR, PERVEC, PJAC, PMULT, PRBNDX, */
/*                        PSTOP, PVTBV, QSORT, DAXPY, SBELM, SCAL, */
/*                        DCOPY, DDOT, SUM3, TSTCHG, UNSCAL, VEVMW, */
/*                        VFILL, VOUT, WEVMW */
/*          SYSTEM        DABS, DLOG10, DBLE(AMAX0), DMAX1, DBLE(FLOAT), */
/*                        MOD,DSQRT */

/*     VERSION:  ITPACK 2C (MARCH 1982) */

/*     CODE WRITTEN BY:  DAVID KINCAID, ROGER GRIMES, JOHN RESPESS */
/*                       CENTER FOR NUMERICAL ANALYSIS */
/*                       UNIVERSITY OF TEXAS */
/*                       AUSTIN, TX  78712 */
/*                       (512) 471-1242 */

/*     FOR ADDITIONAL DETAILS ON THE */
/*          (A) SUBROUTINE SEE TOMS ARTICLE 1982 */
/*          (B) ALGORITHM  SEE CNA REPORT 150 */

/*     BASED ON THEORY BY:  DAVID YOUNG, DAVID KINCAID, LOU HAGEMAN */

/*     REFERENCE THE BOOK:  APPLIED ITERATIVE METHODS */
/*                          L. HAGEMAN, D. YOUNG */
/*                          ACADEMIC PRESS, 1981 */

/*     ************************************************** */
/*     *               IMPORTANT NOTE                   * */
/*     *                                                * */
/*     *      WHEN INSTALLING ITPACK ROUTINES ON A      * */
/*     *  DIFFERENT COMPUTER, RESET SOME OF THE VALUES  * */
/*     *  IN  SUBROUTNE DFAULT.   MOST IMPORTANT ARE    * */
/*     *                                                * */
/*     *   DRELPR      MACHINE RELATIVE PRECISION       * */
/*     *   RPARM(1)    STOPPING CRITERION               * */
/*     *                                                * */
/*     *   ALSO CHANGE SYSTEM-DEPENDENT ROUTINE         * */
/*     *   SECOND USED IN TIMER                         * */
/*     *                                                * */
/*     ************************************************** */

/*     SPECIFICATIONS FOR ARGUMENTS */


/*     SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/* ... VARIABLES IN COMMON BLOCK - ITCOM1 */

/*     IN     - ITERATION NUMBER */
/*     IS     - ITERATION NUMBER WHEN PARAMETERS LAST CHANGED */
/*     ISYM   - SYMMETRIC/NONSYMMETRIC STORAGE FORMAT SWITCH */
/*     ITMAX  - MAXIMUM NUMBER OF ITERATIONS ALLOWED */
/*     LEVEL  - LEVEL OF OUTPUT CONTROL SWITCH */
/*     NOUT   - OUTPUT UNIT NUMBER */

/* ... VARIABLES IN COMMON BLOCK - ITCOM2 */

/*     ADAPT  - FULLY ADAPTIVE PROCEDURE SWITCH */
/*     BETADT - SWITCH FOR ADAPTIVE DETERMINATION OF BETA */
/*     CASEII - ADAPTIVE PROCEDURE CASE SWITCH */
/*     HALT   - STOPPING TEST SWITCH */
/*     PARTAD - PARTIALLY ADAPTIVE PROCEDURE SWITCH */

/* ... VARIABLES IN COMMON BLOCK - ITCOM3 */

/*     BDELNM - TWO NORM OF B TIMES DELTA-SUPER-N */
/*     BETAB  - ESTIMATE FOR THE SPECTRAL RADIUS OF LU MATRIX */
/*     CME    - ESTIMATE OF LARGEST EIGENVALUE */
/*     DELNNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION N */
/*     DELSNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION S */
/*     FF     - ADAPTIVE PROCEDURE DAMPING FACTOR */
/*     GAMMA  - ACCELERATION PARAMETER */
/*     OMEGA  - OVERRELAXATION PARAMETER FOR SOR AND SSOR */
/*     QA     - PSEUDO-RESIDUAL RATIO */
/*     QT     - VIRTUAL SPECTRAL RADIUS */
/*     RHO    - ACCELERATION PARAMETER */
/*     RRR    - ADAPTIVE PARAMETER */
/*     SIGE   - PARAMETER SIGMA-SUB-E */
/*     SME    - ESTIMATE OF SMALLEST EIGENVALUE */
/*     SPECR  - SPECTRAL RADIUS ESTIMATE FOR SSOR */
/*     DRELPR - MACHINE RELATIVE PRECISION */
/*     STPTST - STOPPING PARAMETER */
/*     UDNM   - TWO NORM OF U */
/*     ZETA   - STOPPING CRITERION */

/* ... INITIALIZE COMMON BLOCKS */

    /* Parameter adjustments */
    --u;
    --rhs;
    --ia;
    --ja;
    --a;
    --iwksp;
    --wksp;
    --iparm;
    --rparm;

    /* Function Body */
    itcom1_1.level = iparm[2];
    itcom1_1.nout = iparm[4];
    if (itcom1_1.level >= 1) {
    io___39.ciunit = itcom1_1.nout;
    s_wsfe(&io___39);
    e_wsfe();
    }
    ier = 0;
    if (iparm[1] <= 0) {
    return 0;
    }
    n = *nn;
    if (iparm[11] == 0) {
    timj1 = timer_(&dummy);
    }
    if (itcom1_1.level >= 3) {
    goto L20;
    }
    echout_(&iparm[1], &rparm[1], &c__2);
    goto L30;
L20:
    echall_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iparm[1], &rparm[1], &c__1);
L30:
    temp = itcom3_1.drelpr * 500.;
    if (itcom3_1.zeta >= temp) {
    goto L50;
    }
    if (itcom1_1.level >= 1) {
    io___45.ciunit = itcom1_1.nout;
    s_wsfe(&io___45);
    do_fio(&c__1, (char *)&itcom3_1.zeta, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.drelpr, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&temp, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    itcom3_1.zeta = temp;
L50:
    time1 = rparm[9];
    time2 = rparm[10];
    digit1 = rparm[11];
    digit2 = rparm[12];

/* ... VERIFY N */

    if (n > 0) {
    goto L70;
    }
    ier = 21;
    if (itcom1_1.level >= 0) {
    io___50.ciunit = itcom1_1.nout;
    s_wsfe(&io___50);
    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L360;
L70:

/* ... REMOVE ROWS AND COLUMNS IF REQUESTED */

    if (iparm[10] == 0) {
    goto L90;
    }
    tol = rparm[8];
    ivfill_(&n, &iwksp[1], &c__0);
    vfill_(&n, &wksp[1], &c_b21);
    sbelm_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iwksp[1], &wksp[1], &tol, &
        itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L90;
    }
    if (itcom1_1.level >= 0) {
    io___52.ciunit = itcom1_1.nout;
    s_wsfe(&io___52);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&tol, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    goto L360;

/* ... INITIALIZE WKSP BASE ADDRESSES. */

L90:
    ib1 = 1;
    ib2 = ib1 + n;
    ib3 = ib2 + n;
    iparm[8] = n << 1;
    if (*nw >= iparm[8]) {
    goto L110;
    }
    ier = 22;
    if (itcom1_1.level >= 0) {
    io___56.ciunit = itcom1_1.nout;
    s_wsfe(&io___56);
    do_fio(&c__1, (char *)&(*nw), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iparm[8], (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L360;

/* ... PERMUTE TO  RED-BLACK SYSTEM IF REQUESTED */

L110:
    nb = iparm[9];
    if (nb < 0) {
    goto L170;
    }
    n3 = n * 3;
    ivfill_(&n3, &iwksp[1], &c__0);
    prbndx_(&n, &nb, &ia[1], &ja[1], &iwksp[1], &iwksp[ib2], &itcom1_1.level, 
        &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L130;
    }
    if (itcom1_1.level >= 0) {
    io___59.ciunit = itcom1_1.nout;
    s_wsfe(&io___59);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L360;

/* ... PERMUTE MATRIX AND RHS */

L130:
    if (itcom1_1.level >= 2) {
    io___60.ciunit = itcom1_1.nout;
    s_wsfe(&io___60);
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    permat_(&n, &ia[1], &ja[1], &a[1], &iwksp[1], &iwksp[ib3], &itcom1_1.isym,
         &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L160;
    }
    if (itcom1_1.level >= 0) {
    io___61.ciunit = itcom1_1.nout;
    s_wsfe(&io___61);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L360;
L160:
    pervec_(&n, &rhs[1], &iwksp[1]);
    pervec_(&n, &u[1], &iwksp[1]);

/* ... SCALE LINEAR SYSTEM, U, AND RHS BY THE SQUARE ROOT OF THE */
/* ... DIAGONAL ELEMENTS. */

L170:
    vfill_(&iparm[8], &wksp[1], &c_b21);
    scal_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1], &
        itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L190;
    }
    if (itcom1_1.level >= 0) {
    io___62.ciunit = itcom1_1.nout;
    s_wsfe(&io___62);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L360;
L190:
    if (itcom1_1.level <= 2) {
    goto L210;
    }
    io___63.ciunit = itcom1_1.nout;
    s_wsfe(&io___63);
    e_wsfe();
L210:
    if (iparm[11] != 0) {
    goto L220;
    }
    timi1 = timer_(&dummy);

/* ... ITERATION SEQUENCE */

L220:
    itmax1 = itcom1_1.itmax + 1;
    i__1 = itmax1;
    for (loop = 1; loop <= i__1; ++loop) {
    itcom1_1.in = loop - 1;
    if (itcom1_1.in % 2 == 1) {
        goto L230;
    }

/* ... CODE FOR THE EVEN ITERATIONS. */

/*     U           = U(IN) */
/*     WKSP(IB1)   = U(IN-1) */

    itjsi_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[ib1], &wksp[
        ib2], &icnt);

    if (itcom2_1.halt) {
        goto L270;
    }
    goto L240;

/* ... CODE FOR THE ODD ITERATIONS. */

/*     U           = U(IN-1) */
/*     WKSP(IB1)   = U(IN) */

L230:
    itjsi_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &wksp[ib1], &u[1], &wksp[
        ib2], &icnt);

    if (itcom2_1.halt) {
        goto L270;
    }
L240:
    ;
    }

/* ... ITMAX HAS BEEN REACHED */

    if (iparm[11] != 0) {
    goto L250;
    }
    timi2 = timer_(&dummy);
    time1 = (doublereal) (timi2 - timi1);
L250:
    ier = 23;
    if (itcom1_1.level >= 1) {
    io___69.ciunit = itcom1_1.nout;
    s_wsfe(&io___69);
    do_fio(&c__1, (char *)&itcom1_1.itmax, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    if (iparm[3] == 0) {
    rparm[1] = itcom3_1.stptst;
    }
    goto L300;

/* ... METHOD HAS CONVERGED */

L270:
    if (iparm[11] != 0) {
    goto L280;
    }
    timi2 = timer_(&dummy);
    time1 = (doublereal) (timi2 - timi1);
L280:
    if (itcom1_1.level >= 1) {
    io___70.ciunit = itcom1_1.nout;
    s_wsfe(&io___70);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    e_wsfe();
    }

/* ... PUT SOLUTION INTO U IF NOT ALREADY THERE. */

L300:
    if (itcom1_1.in % 2 == 1) {
    dcopy_(&n, &wksp[ib1], &c__1, &u[1], &c__1);
    }

/* ... UNSCALE THE MATRIX, SOLUTION, AND RHS VECTORS. */

    unscal_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1]);

/* ... UN-PERMUTE MATRIX,RHS, AND SOLUTION */

    if (iparm[9] < 0) {
    goto L330;
    }
    permat_(&n, &ia[1], &ja[1], &a[1], &iwksp[ib2], &iwksp[ib3], &
        itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ierper);
    if (ierper == 0) {
    goto L320;
    }
    if (itcom1_1.level >= 0) {
    io___72.ciunit = itcom1_1.nout;
    s_wsfe(&io___72);
    do_fio(&c__1, (char *)&ierper, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    if (ier == 0) {
    ier = ierper;
    }
    goto L360;
L320:
    pervec_(&n, &rhs[1], &iwksp[ib2]);
    pervec_(&n, &u[1], &iwksp[ib2]);

/* ... OPTIONAL ERROR ANALYSIS */

L330:
    idgts = iparm[12];
    if (idgts < 0) {
    goto L340;
    }
    if (iparm[2] <= 0) {
    idgts = 0;
    }
    perror_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1], &digit1, &
        digit2, &idgts);

/* ... SET RETURN PARAMETERS IN IPARM AND RPARM */

L340:
    if (iparm[11] != 0) {
    goto L350;
    }
    timj2 = timer_(&dummy);
    time2 = (doublereal) (timj2 - timj1);
L350:
    if (iparm[3] != 0) {
    goto L360;
    }
    iparm[1] = itcom1_1.in;
    iparm[9] = nb;
    rparm[2] = itcom3_1.cme;
    rparm[3] = itcom3_1.sme;
    rparm[9] = time1;
    rparm[10] = time2;
    rparm[11] = digit1;
    rparm[12] = digit2;

L360:
    *ierr = ier;
    if (itcom1_1.level >= 3) {
    echall_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iparm[1], &rparm[1], &
        c__2);
    }

    return 0;
} /* jsi_ */

/* Subroutine */ int sor_(integer *nn, integer *ia, integer *ja, doublereal *
    a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, 
    doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr)
{
    /* Format strings */
    static char fmt_10[] = "(\0020\002///1x,\002BEGINNING OF ITPACK SOLUTION\
 MODULE  SOR\002)";
    static char fmt_40[] = "(\0020\002,\002*** W A R N I N G ************\
\002/\0020\002,\002    IN ITPACK ROUTINE SOR\002/\002 \002,\002    RPARM(1) =\
\002,d10.3,\002 (ZETA)\002/\002 \002,\002    A VALUE THIS SMALL MAY HINDER C\
ONVERGENCE \002/\002 \002,\002    SINCE MACHINE PRECISION DRELPR =\002,d10.3/\
\002 \002,\002    ZETA RESET TO \002,d10.3)";
    static char fmt_60[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SOR \002/\002 \002\
,\002    INVALID MATRIX DIMENSION, N =\002,i8)";
    static char fmt_80[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SOR \002/\002 \002\
,\002    ERROR DETECTED IN SUBROUTINE  SBELM \002/\002 \002,\002    WHICH RE\
MOVES ROWS AND COLUMNS OF SYSTEM \002/\002 \002,\002    WHEN DIAGONAL ENTRY \
TOO LARGE  \002/\002 \002,\002    IER = \002,i5,5x,\002 RPARM(8) = \002,d10.\
3,\002 (TOL)\002)";
    static char fmt_100[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SOR \002/\002 \002,\
\002    NOT ENOUGH WORKSPACE AT \002,i10/\002 \002,\002    SET IPARM(8) =\
\002,i10,\002 (NW)\002)";
    static char fmt_120[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SOR \002/\002 \002,\
\002    ERROR DETECTED IN SUBROUTINE  PRBNDX\002/\002 \002,\002    WHICH COM\
PUTES THE RED-BLACK INDEXING\002/\002 \002,\002    IER = \002,i5,\002 IPARM(\
9) = \002,i5,\002 (NB)\002)";
    static char fmt_140[] = "(/10x,\002ORDER OF BLACK SUBSYSTEM = \002,i5\
,\002 (NB)\002)";
    static char fmt_150[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SOR \002/\002 \002,\
\002    ERROR DETECTED IN SUBROUTINE  PERMAT\002/\002 \002,\002    WHICH DOE\
S THE RED-BLACK PERMUTATION\002/\002 \002,\002    IER = \002,i5)";
    static char fmt_180[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SOR \002/\002 \002,\
\002    ERROR DETECTED IN SUBROUTINE  SCAL  \002/\002 \002,\002    WHICH SCA\
LES THE SYSTEM   \002/\002 \002,\002    IER = \002,i5)";
    static char fmt_200[] = "(///1x,\002CME IS THE ESTIMATE OF THE LARGEST E\
IGENVALUE OF\002,\002 THE JACOBI MATRIX\002)";
    static char fmt_210[] = "(1x,\002OMEGA IS THE RELAXATION FACTOR\002)";
    static char fmt_260[] = "(\0020\002,\002*** W A R N I N G ***********\
*\002/\0020\002,\002    IN ITPACK ROUTINE SOR\002/\002 \002,\002    FAILURE \
TO CONVERGE IN\002,i5,\002 ITERATIONS\002)";
    static char fmt_290[] = "(/1x,\002SOR  HAS CONVERGED IN \002,i5,\002 ITE\
RATIONS\002)";
    static char fmt_310[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SOR \002/\002 \002,\
\002    ERROR DETECTED IN SUBROUTINE  PERMAT\002/\002 \002,\002    WHICH UND\
OES THE RED-BLACK PERMUTATION   \002/\002 \002,\002    IER = \002,i5)";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(), do_fio(integer *, char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int scal_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *, doublereal *, integer *,
         integer *, integer *);
    static doublereal temp;
    static integer loop;
    static doublereal time1, time2;
    static real timi1, timj1, timi2, timj2;
    static integer n;
    extern /* Subroutine */ int sbelm_(integer *, integer *, integer *, 
        doublereal *, doublereal *, integer *, doublereal *, doublereal *,
         integer *, integer *, integer *, integer *);
    static integer idgts;
    extern /* Subroutine */ int vfill_(integer *, doublereal *, doublereal *);
    extern doublereal timer_(real *);
    static real dummy;
    extern /* Subroutine */ int itsor_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *, doublereal *);
    static integer n3;
    static doublereal digit1, digit2;
    static integer itmax1, nb;
    extern /* Subroutine */ int echall_(integer *, integer *, integer *, 
        doublereal *, doublereal *, integer *, doublereal *, integer *), 
        pervec_(integer *, doublereal *, integer *), ivfill_(integer *, 
        integer *, integer *);
    static integer ierper;
    extern /* Subroutine */ int echout_(integer *, doublereal *, integer *), 
        permat_(integer *, integer *, integer *, doublereal *, integer *, 
        integer *, integer *, integer *, integer *, integer *), unscal_(
        integer *, integer *, integer *, doublereal *, doublereal *, 
        doublereal *, doublereal *);
    static integer ib1, ib2, ib3;
    extern /* Subroutine */ int prbndx_(integer *, integer *, integer *, 
        integer *, integer *, integer *, integer *, integer *, integer *),
         perror_(integer *, integer *, integer *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *, 
        doublereal *, integer *);
    static integer ier;
    static doublereal tol;

    /* Fortran I/O blocks */
    static cilist io___75 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___81 = { 0, 0, 0, fmt_40, 0 };
    static cilist io___86 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___88 = { 0, 0, 0, fmt_80, 0 };
    static cilist io___92 = { 0, 0, 0, fmt_100, 0 };
    static cilist io___95 = { 0, 0, 0, fmt_120, 0 };
    static cilist io___96 = { 0, 0, 0, fmt_140, 0 };
    static cilist io___97 = { 0, 0, 0, fmt_150, 0 };
    static cilist io___98 = { 0, 0, 0, fmt_180, 0 };
    static cilist io___99 = { 0, 0, 0, fmt_200, 0 };
    static cilist io___100 = { 0, 0, 0, fmt_210, 0 };
    static cilist io___105 = { 0, 0, 0, fmt_260, 0 };
    static cilist io___106 = { 0, 0, 0, fmt_290, 0 };
    static cilist io___108 = { 0, 0, 0, fmt_310, 0 };



/*     ITPACK 2C MAIN SUBROUTINE  SOR  (SUCCESSIVE OVERRELATION) */
/*     EACH OF THE MAIN SUBROUTINES: */
/*           JCG, JSI, SOR, SSORCG, SSORSI, RSCG, RSSI */
/*     CAN BE USED INDEPENDENTLY OF THE OTHERS */

/* ... FUNCTION: */

/*          THIS SUBROUTINE, SOR, DRIVES THE  SUCCESSIVE */
/*          OVERRELAXATION ALGORITHM. */

/* ... PARAMETER LIST: */

/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX. (= NN) */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF */
/*                 THE SPARSE MATRIX REPRESENTATION. */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE */
/*                 MATRIX REPRESENTATION */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE */
/*                 OF THE MATRIX PROBLEM. */
/*          U      INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U CONTAINS THE */
/*                 INITIAL GUESS TO THE SOLUTION. ON OUTPUT, IT CONTAINS */
/*                 THE LATEST ESTIMATE TO THE SOLUTION. */
/*          IWKSP  INTEGER VECTOR WORKSPACE OF LENGTH 3*N */
/*          NW     INPUT INTEGER.  LENGTH OF AVAILABLE WKSP.  ON OUTPUT, */
/*                 IPARM(8) IS AMOUNT USED. */
/*          WKSP   D.P. VECTOR USED FOR WORKING SPACE.  SOR NEEDS THIS */
/*                 TO BE IN LENGTH AT LEAST  N */
/*          IPARM  INTEGER VECTOR OF LENGTH 12.  ALLOWS USER TO SPECIFY */
/*                 SOME INTEGER PARAMETERS WHICH AFFECT THE METHOD. */
/*          RPARM  D.P. VECTOR OF LENGTH 12. ALLOWS USER TO SPECIFY SOME */
/*                 D.P. PARAMETERS WHICH AFFECT THE METHOD. */
/*          IER    OUTPUT INTEGER.  ERROR FLAG. (= IERR) */

/* ... SOR SUBPROGRAM REFERENCES: */

/*          FROM ITPACK   BISRCH, DFAULT, ECHALL, ECHOUT, IPSTR, ITERM, */
/*                        TIMER, ITSOR, IVFILL, PERMAT, PERROR, */
/*                        PERVEC, PFSOR1, PMULT, PRBNDX, PSTOP, QSORT, */
/*                        SBELM, SCAL, DCOPY, DDOT, TAU, UNSCAL, VFILL, */
/*                        VOUT, WEVMW */
/*          SYSTEM        DABS, DLOG10, DBLE(AMAX0), DMAX1, DBLE(FLOAT), */
/*                        DSQRT */

/*     VERSION:  ITPACK 2C (MARCH 1982) */

/*     CODE WRITTEN BY:  DAVID KINCAID, ROGER GRIMES, JOHN RESPESS */
/*                       CENTER FOR NUMERICAL ANALYSIS */
/*                       UNIVERSITY OF TEXAS */
/*                       AUSTIN, TX  78712 */
/*                       (512) 471-1242 */

/*     FOR ADDITIONAL DETAILS ON THE */
/*          (A) SUBROUTINE SEE TOMS ARTICLE 1982 */
/*          (B) ALGORITHM  SEE CNA REPORT 150 */

/*     BASED ON THEORY BY:  DAVID YOUNG, DAVID KINCAID, LOU HAGEMAN */

/*     REFERENCE THE BOOK:  APPLIED ITERATIVE METHODS */
/*                          L. HAGEMAN, D. YOUNG */
/*                          ACADEMIC PRESS, 1981 */

/*     ************************************************** */
/*     *               IMPORTANT NOTE                   * */
/*     *                                                * */
/*     *      WHEN INSTALLING ITPACK ROUTINES ON A      * */
/*     *  DIFFERENT COMPUTER, RESET SOME OF THE VALUES  * */
/*     *  IN  SUBROUTNE DFAULT.   MOST IMPORTANT ARE    * */
/*     *                                                * */
/*     *   DRELPR      MACHINE RELATIVE PRECISION       * */
/*     *   RPARM(1)    STOPPING CRITERION               * */
/*     *                                                * */
/*     *   ALSO CHANGE SYSTEM-DEPENDENT ROUTINE         * */
/*     *   SECOND USED IN TIMER                         * */
/*     *                                                * */
/*     ************************************************** */

/*     SPECIFICATIONS FOR ARGUMENTS */


/*     SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/* ... VARIABLES IN COMMON BLOCK - ITCOM1 */

/*     IN     - ITERATION NUMBER */
/*     IS     - ITERATION NUMBER WHEN PARAMETERS LAST CHANGED */
/*     ISYM   - SYMMETRIC/NONSYMMETRIC STORAGE FORMAT SWITCH */
/*     ITMAX  - MAXIMUM NUMBER OF ITERATIONS ALLOWED */
/*     LEVEL  - LEVEL OF OUTPUT CONTROL SWITCH */
/*     NOUT   - OUTPUT UNIT NUMBER */

/* ... VARIABLES IN COMMON BLOCK - ITCOM2 */

/*     ADAPT  - FULLY ADAPTIVE PROCEDURE SWITCH */
/*     BETADT - SWITCH FOR ADAPTIVE DETERMINATION OF BETA */
/*     CASEII - ADAPTIVE PROCEDURE CASE SWITCH */
/*     HALT   - STOPPING TEST SWITCH */
/*     PARTAD - PARTIALLY ADAPTIVE PROCEDURE SWITCH */

/* ... VARIABLES IN COMMON BLOCK - ITCOM3 */

/*     BDELNM - TWO NORM OF B TIMES DELTA-SUPER-N */
/*     BETAB  - ESTIMATE FOR THE SPECTRAL RADIUS OF LU MATRIX */
/*     CME    - ESTIMATE OF LARGEST EIGENVALUE */
/*     DELNNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION N */
/*     DELSNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION S */
/*     FF     - ADAPTIVE PROCEDURE DAMPING FACTOR */
/*     GAMMA  - ACCELERATION PARAMETER */
/*     OMEGA  - OVERRELAXATION PARAMETER FOR SOR AND SSOR */
/*     QA     - PSEUDO-RESIDUAL RATIO */
/*     QT     - VIRTUAL SPECTRAL RADIUS */
/*     RHO    - ACCELERATION PARAMETER */
/*     RRR    - ADAPTIVE PARAMETER */
/*     SIGE   - PARAMETER SIGMA-SUB-E */
/*     SME    - ESTIMATE OF SMALLEST EIGENVALUE */
/*     SPECR  - SPECTRAL RADIUS ESTIMATE FOR SSOR */
/*     DRELPR - MACHINE RELATIVE PRECISION */
/*     STPTST - STOPPING PARAMETER */
/*     UDNM   - TWO NORM OF U */
/*     ZETA   - STOPPING CRITERION */

/* ... INITIALIZE COMMON BLOCKS */

    /* Parameter adjustments */
    --u;
    --rhs;
    --ia;
    --ja;
    --a;
    --iwksp;
    --wksp;
    --iparm;
    --rparm;

    /* Function Body */
    itcom1_1.level = iparm[2];
    itcom1_1.nout = iparm[4];
    if (itcom1_1.level >= 1) {
    io___75.ciunit = itcom1_1.nout;
    s_wsfe(&io___75);
    e_wsfe();
    }
    ier = 0;
    if (iparm[1] <= 0) {
    return 0;
    }
    n = *nn;
    if (iparm[11] == 0) {
    timj1 = timer_(&dummy);
    }
    if (itcom1_1.level >= 3) {
    goto L20;
    }
    echout_(&iparm[1], &rparm[1], &c__3);
    goto L30;
L20:
    echall_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iparm[1], &rparm[1], &c__1);
L30:
    temp = itcom3_1.drelpr * 500.;
    if (itcom3_1.zeta >= temp) {
    goto L50;
    }
    if (itcom1_1.level >= 1) {
    io___81.ciunit = itcom1_1.nout;
    s_wsfe(&io___81);
    do_fio(&c__1, (char *)&itcom3_1.zeta, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.drelpr, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&temp, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    itcom3_1.zeta = temp;
L50:
    time1 = rparm[9];
    time2 = rparm[10];
    digit1 = rparm[11];
    digit2 = rparm[12];

/* ... VERIFY N */

    if (n > 0) {
    goto L70;
    }
    ier = 31;
    if (itcom1_1.level >= 0) {
    io___86.ciunit = itcom1_1.nout;
    s_wsfe(&io___86);
    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L360;
L70:

/* ... REMOVE ROWS AND COLUMNS IF REQUESTED */

    if (iparm[10] == 0) {
    goto L90;
    }
    tol = rparm[8];
    ivfill_(&n, &iwksp[1], &c__0);
    vfill_(&n, &wksp[1], &c_b21);
    sbelm_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iwksp[1], &wksp[1], &tol, &
        itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L90;
    }
    if (itcom1_1.level >= 0) {
    io___88.ciunit = itcom1_1.nout;
    s_wsfe(&io___88);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&tol, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    goto L360;

/* ... INITIALIZE WKSP BASE ADDRESSES. */

L90:
    ib1 = 1;
    ib2 = ib1 + n;
    ib3 = ib2 + n;
    iparm[8] = n;
    if (*nw >= iparm[8]) {
    goto L110;
    }
    ier = 32;
    if (itcom1_1.level >= 0) {
    io___92.ciunit = itcom1_1.nout;
    s_wsfe(&io___92);
    do_fio(&c__1, (char *)&(*nw), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iparm[8], (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L360;

/* ... PERMUTE TO  RED-BLACK SYSTEM IF REQUESTED */

L110:
    nb = iparm[9];
    if (nb < 0) {
    goto L170;
    }
    n3 = n * 3;
    ivfill_(&n3, &iwksp[1], &c__0);
    prbndx_(&n, &nb, &ia[1], &ja[1], &iwksp[1], &iwksp[ib2], &itcom1_1.level, 
        &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L130;
    }
    if (itcom1_1.level >= 0) {
    io___95.ciunit = itcom1_1.nout;
    s_wsfe(&io___95);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L360;

/* ... PERMUTE MATRIX AND RHS */

L130:
    if (itcom1_1.level >= 2) {
    io___96.ciunit = itcom1_1.nout;
    s_wsfe(&io___96);
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    permat_(&n, &ia[1], &ja[1], &a[1], &iwksp[1], &iwksp[ib3], &itcom1_1.isym,
         &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L160;
    }
    if (itcom1_1.level >= 0) {
    io___97.ciunit = itcom1_1.nout;
    s_wsfe(&io___97);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L360;
L160:
    pervec_(&n, &rhs[1], &iwksp[1]);
    pervec_(&n, &u[1], &iwksp[1]);

/* ... SCALE LINEAR SYSTEM, U, AND RHS BY THE SQUARE ROOT OF THE */
/* ... DIAGONAL ELEMENTS. */

L170:
    vfill_(&iparm[8], &wksp[1], &c_b21);
    scal_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1], &
        itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L190;
    }
    if (itcom1_1.level >= 0) {
    io___98.ciunit = itcom1_1.nout;
    s_wsfe(&io___98);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L360;
L190:
    if (itcom1_1.level <= 2) {
    goto L220;
    }
    if (itcom2_1.adapt) {
    io___99.ciunit = itcom1_1.nout;
    s_wsfe(&io___99);
    e_wsfe();
    }
    io___100.ciunit = itcom1_1.nout;
    s_wsfe(&io___100);
    e_wsfe();
L220:
    if (iparm[11] != 0) {
    goto L230;
    }
    timi1 = timer_(&dummy);

/* ... ITERATION SEQUENCE */

L230:
    itmax1 = itcom1_1.itmax + 1;
    i__1 = itmax1;
    for (loop = 1; loop <= i__1; ++loop) {
    itcom1_1.in = loop - 1;

/* ... CODE FOR ONE ITERATION. */

/*     U           = U(IN) */

    itsor_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[ib1]);

    if (itcom2_1.halt) {
        goto L270;
    }
/* L240: */
    }

/* ... ITMAX HAS BEEN REACHED */

    if (iparm[11] != 0) {
    goto L250;
    }
    timi2 = timer_(&dummy);
    time1 = (doublereal) (timi2 - timi1);
L250:
    if (itcom1_1.level >= 1) {
    io___105.ciunit = itcom1_1.nout;
    s_wsfe(&io___105);
    do_fio(&c__1, (char *)&itcom1_1.itmax, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    ier = 33;
    if (iparm[3] == 0) {
    rparm[1] = itcom3_1.stptst;
    }
    goto L300;

/* ... METHOD HAS CONVERGED */

L270:
    if (iparm[11] != 0) {
    goto L280;
    }
    timi2 = timer_(&dummy);
    time1 = (doublereal) (timi2 - timi1);
L280:
    if (itcom1_1.level >= 1) {
    io___106.ciunit = itcom1_1.nout;
    s_wsfe(&io___106);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    e_wsfe();
    }

/* ... UNSCALE THE MATRIX, SOLUTION, AND RHS VECTORS. */

L300:
    unscal_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1]);

/* ... UN-PERMUTE MATRIX,RHS, AND SOLUTION */

    if (iparm[9] < 0) {
    goto L330;
    }
    permat_(&n, &ia[1], &ja[1], &a[1], &iwksp[ib2], &iwksp[ib3], &
        itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ierper);
    if (ierper == 0) {
    goto L320;
    }
    if (itcom1_1.level >= 0) {
    io___108.ciunit = itcom1_1.nout;
    s_wsfe(&io___108);
    do_fio(&c__1, (char *)&ierper, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    if (ier == 0) {
    ier = ierper;
    }
    goto L360;
L320:
    pervec_(&n, &rhs[1], &iwksp[ib2]);
    pervec_(&n, &u[1], &iwksp[ib2]);

/* ... OPTIONAL ERROR ANALYSIS */

L330:
    idgts = iparm[12];
    if (idgts < 0) {
    goto L340;
    }
    if (iparm[2] <= 0) {
    idgts = 0;
    }
    perror_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1], &digit1, &
        digit2, &idgts);

/* ... SET RETURN PARAMETERS IN IPARM AND RPARM */

L340:
    if (iparm[11] != 0) {
    goto L350;
    }
    timj2 = timer_(&dummy);
    time2 = (doublereal) (timj2 - timj1);
L350:
    if (iparm[3] != 0) {
    goto L360;
    }
    iparm[1] = itcom1_1.in;
    iparm[9] = nb;
    rparm[2] = itcom3_1.cme;
    rparm[3] = itcom3_1.sme;
    rparm[5] = itcom3_1.omega;
    rparm[9] = time1;
    rparm[10] = time2;
    rparm[11] = digit1;
    rparm[12] = digit2;

L360:
    *ierr = ier;
    if (itcom1_1.level >= 3) {
    echall_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iparm[1], &rparm[1], &
        c__2);
    }

    return 0;
} /* sor_ */

/* Subroutine */ int ssorcg_(integer *nn, integer *ia, integer *ja, 
    doublereal *a, doublereal *rhs, doublereal *u, integer *iwksp, 
    integer *nw, doublereal *wksp, integer *iparm, doublereal *rparm, 
    integer *ierr)
{
    /* Format strings */
    static char fmt_10[] = "(\0020\002///1x,\002BEGINNING OF ITPACK SOLUTION\
 MODULE  SSORCG\002)";
    static char fmt_40[] = "(\0020\002,\002*** W A R N I N G ************\
\002/\0020\002,\002    IN ITPACK ROUTINE SSORCG\002/\002 \002,\002    RPARM(\
1) =\002,d10.3,\002 (ZETA)\002/\002 \002,\002    A VALUE THIS SMALL MAY HIND\
ER CONVERGENCE \002/\002 \002,\002    SINCE MACHINE PRECISION DRELPR =\002,d\
10.3/\002 \002,\002    ZETA RESET TO \002,d10.3)";
    static char fmt_60[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SSORCG \002/\002\
 \002,\002    INVALID MATRIX DIMENSION, N =\002,i8)";
    static char fmt_80[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SSORCG \002/\002\
 \002,\002    ERROR DETECTED IN SUBROUTINE  SBELM \002/\002 \002,\002    WHI\
CH REMOVES ROWS AND COLUMNS OF SYSTEM \002/\002 \002,\002    WHEN DIAGONAL E\
NTRY TOO LARGE  \002/\002 \002,\002    IER = \002,i5,5x,\002 RPARM(8) = \002\
,d10.3,\002 (TOL)\002)";
    static char fmt_100[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SSORCG \002/\002\
 \002,\002    NOT ENOUGH WORKSPACE AT \002,i10/\002 \002,\002    SET IPARM(8\
) =\002,i10,\002 (NW)\002)";
    static char fmt_120[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SSORCG \002/\002\
 \002,\002    ERROR DETECTED IN SUBROUTINE  PRBNDX\002/\002 \002,\002    WHI\
CH COMPUTES THE RED-BLACK INDEXING\002/\002 \002,\002    IER = \002,i5,\002 \
IPARM(9) = \002,i5,\002 (NB)\002)";
    static char fmt_140[] = "(/10x,\002ORDER OF BLACK SUBSYSTEM = \002,i5\
,\002 (NB)\002)";
    static char fmt_150[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SSORCG \002/\002\
 \002,\002    ERROR DETECTED IN SUBROUTINE  PERMAT\002/\002 \002,\002    WHI\
CH DOES THE RED-BLACK PERMUTATION\002/\002 \002,\002    IER = \002,i5)";
    static char fmt_180[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SSORCG \002/\002\
 \002,\002    ERROR DETECTED IN SUBROUTINE  SCAL  \002/\002 \002,\002    WHI\
CH SCALES THE SYSTEM   \002/\002 \002,\002    IER = \002,i5)";
    static char fmt_200[] = "(///1x,\002IN THE FOLLOWING, RHO AND GAMMA AR\
E\002,\002 ACCELERATION PARAMETERS\002)";
    static char fmt_210[] = "(1x,\002S-PRIME IS AN INITIAL ESTIMATE FOR NEW \
CME\002)";
    static char fmt_290[] = "(\0020\002,\002*** W A R N I N G ***********\
*\002/\0020\002,\002    IN ITPACK ROUTINE SSORCG\002/\002 \002,\002    FAILU\
RE TO CONVERGE IN\002,i5,\002 ITERATIONS\002)";
    static char fmt_320[] = "(/1x,\002SSORCG  HAS CONVERGED IN \002,i5,\002 \
ITERATIONS\002)";
    static char fmt_340[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SSORCG \002/\002\
 \002,\002    ERROR DETECTED IN SUBROUTINE  PERMAT\002/\002 \002,\002    WHI\
CH UNDOES THE RED-BLACK PERMUTATION   \002/\002 \002,\002    IER = \002,i5)";

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(), do_fio(integer *, char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int scal_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *, doublereal *, integer *,
         integer *, integer *), omeg_(doublereal *, integer *);
    static doublereal temp;
    static integer loop;
    static doublereal time1, time2;
    static real timi1, timj1, timi2, timj2;
    static integer n;
    extern doublereal pbeta_(integer *, integer *, integer *, doublereal *, 
        doublereal *, doublereal *, doublereal *);
    extern /* Subroutine */ int sbelm_(integer *, integer *, integer *, 
        doublereal *, doublereal *, integer *, doublereal *, doublereal *,
         integer *, integer *, integer *, integer *);
    static integer idgts;
    extern /* Subroutine */ int vfill_(integer *, doublereal *, doublereal *),
         dcopy_(integer *, doublereal *, integer *, doublereal *, integer 
        *);
    extern doublereal timer_(real *);
    extern /* Subroutine */ int pfsor_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *);
    static real dummy;
    static integer n3;
    extern /* Subroutine */ int vevmw_(integer *, doublereal *, doublereal *);
    static doublereal digit1, digit2;
    static integer itmax1, nb;
    extern /* Subroutine */ int echall_(integer *, integer *, integer *, 
        doublereal *, doublereal *, integer *, doublereal *, integer *);
    static doublereal betnew;
    extern /* Subroutine */ int ivfill_(integer *, integer *, integer *);
    static integer ierper;
    extern /* Subroutine */ int echout_(integer *, doublereal *, integer *), 
        permat_(integer *, integer *, integer *, doublereal *, integer *, 
        integer *, integer *, integer *, integer *, integer *), pervec_(
        integer *, doublereal *, integer *), itsrcg_(integer *, integer *,
         integer *, doublereal *, doublereal *, doublereal *, doublereal *
        , doublereal *, doublereal *, doublereal *, doublereal *, 
        doublereal *, doublereal *);
    static integer ib1, ib2, ib3, ib4, ib5, ib6, ib7;
    extern /* Subroutine */ int prbndx_(integer *, integer *, integer *, 
        integer *, integer *, integer *, integer *, integer *, integer *),
         unscal_(integer *, integer *, integer *, doublereal *, 
        doublereal *, doublereal *, doublereal *), perror_(integer *, 
        integer *, integer *, doublereal *, doublereal *, doublereal *, 
        doublereal *, doublereal *, doublereal *, integer *);
    static integer ier;
    static doublereal tol;

    /* Fortran I/O blocks */
    static cilist io___111 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___117 = { 0, 0, 0, fmt_40, 0 };
    static cilist io___122 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___124 = { 0, 0, 0, fmt_80, 0 };
    static cilist io___132 = { 0, 0, 0, fmt_100, 0 };
    static cilist io___135 = { 0, 0, 0, fmt_120, 0 };
    static cilist io___136 = { 0, 0, 0, fmt_140, 0 };
    static cilist io___137 = { 0, 0, 0, fmt_150, 0 };
    static cilist io___138 = { 0, 0, 0, fmt_180, 0 };
    static cilist io___139 = { 0, 0, 0, fmt_200, 0 };
    static cilist io___140 = { 0, 0, 0, fmt_210, 0 };
    static cilist io___146 = { 0, 0, 0, fmt_290, 0 };
    static cilist io___147 = { 0, 0, 0, fmt_320, 0 };
    static cilist io___149 = { 0, 0, 0, fmt_340, 0 };



/*     ITPACK 2C MAIN SUBROUTINE  SSORCG  (SYMMETRIC SUCCESSIVE OVER- */
/*                                        RELAXATION CONJUGATE GRADIENT) */
/*     EACH OF THE MAIN SUBROUTINES: */
/*           JCG, JSI, SOR, SSORCG, SSORSI, RSCG, RSSI */
/*     CAN BE USED INDEPENDENTLY OF THE OTHERS */

/* ... FUNCTION: */

/*          THIS SUBROUTINE, SSORCG, DRIVES THE  SYMMETRIC SOR-CG */
/*          ALGORITHM. */

/* ... PARAMETER LIST: */

/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX. (= NN) */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF */
/*                 THE SPARSE MATRIX REPRESENTATION. */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE */
/*                 MATRIX REPRESENTATION. */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE */
/*                 OF THE MATRIX PROBLEM. */
/*          U      INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U CONTAINS THE */
/*                 INITIAL GUESS TO THE SOLUTION. ON OUTPUT, IT CONTAINS */
/*                 THE LATEST ESTIMATE TO THE SOLUTION. */
/*          IWKSP  INTEGER VECTOR WORKSPACE OF LENGTH 3*N */
/*          NW     INPUT INTEGER.  LENGTH OF AVAILABLE WKSP.  ON OUTPUT, */
/*                 IPARM(8) IS AMOUNT USED. */
/*          WKSP   D.P. VECTOR USED FOR WORKING SPACE.  SSOR-CG */
/*                 NEEDS TO BE IN LENGTH AT LEAST */
/*                 6*N + 2*ITMAX,  IF IPARM(5)=0  (SYMMETRIC STORAGE) */
/*                 6*N + 4*ITMAX,  IF IPARM(5)=1  (NONSYMMETRIC STORAGE) */
/*          IPARM  INTEGER VECTOR OF LENGTH 12.  ALLOWS USER TO SPECIFY */
/*                 SOME INTEGER PARAMETERS WHICH AFFECT THE METHOD.  IF */
/*          RPARM  D.P. VECTOR OF LENGTH 12. ALLOWS USER TO SPECIFY SOME */
/*                 D.P. PARAMETERS WHICH AFFECT THE METHOD. */
/*          IER    OUTPUT INTEGER.  ERROR FLAG. (= IERR) */

/* ... SSORCG SUBPROGRAM REFERENCES: */

/*          FROM ITPACK    BISRCH, CHGCON, DETERM, DFAULT, ECHALL, */
/*                         ECHOUT, EIGVNS, EIGVSS, EQRT1S, ITERM, TIMER, */
/*                         ITSRCG, IVFILL, OMEG, OMGCHG, OMGSTR, */
/*                         PARCON, PBETA, PBSOR, PERMAT, PERROR, */
/*                         PERVEC, PFSOR, PJAC, PMULT, PRBNDX, PSTOP, PVT */
/*                         QSORT, SBELM, SCAL, DCOPY, DDOT, SUM3, */
/*                         UNSCAL, VEVMW, VEVPW, VFILL, VOUT, WEVMW, */
/*                         ZBRENT */
/*          SYSTEM         DABS, DLOG, DLOG10, DBLE(AMAX0), DMAX1, AMIN1, */
/*                         MOD, DSQRT */

/*     VERSION:  ITPACK 2C (MARCH 1982) */

/*     CODE WRITTEN BY:  DAVID KINCAID, ROGER GRIMES, JOHN RESPESS */
/*                       CENTER FOR NUMERICAL ANALYSIS */
/*                       UNIVERSITY OF TEXAS */
/*                       AUSTIN, TX  78712 */
/*                       (512) 471-1242 */

/*     FOR ADDITIONAL DETAILS ON THE */
/*          (A) SUBROUTINE SEE TOMS ARTICLE 1982 */
/*          (B) ALGORITHM  SEE CNA REPORT 150 */

/*     BASED ON THEORY BY:  DAVID YOUNG, DAVID KINCAID, LOU HAGEMAN */

/*     REFERENCE THE BOOK:  APPLIED ITERATIVE METHODS */
/*                          L. HAGEMAN, D. YOUNG */
/*                          ACADEMIC PRESS, 1981 */

/*     ************************************************** */
/*     *               IMPORTANT NOTE                   * */
/*     *                                                * */
/*     *      WHEN INSTALLING ITPACK ROUTINES ON A      * */
/*     *  DIFFERENT COMPUTER, RESET SOME OF THE VALUES  * */
/*     *  IN  SUBROUTNE DFAULT.   MOST IMPORTANT ARE    * */
/*     *                                                * */
/*     *   DRELPR      MACHINE RELATIVE PRECISION       * */
/*     *   RPARM(1)    STOPPING CRITERION               * */
/*     *                                                * */
/*     *   ALSO CHANGE SYSTEM-DEPENDENT ROUTINE         * */
/*     *   SECOND USED IN TIMER                         * */
/*     *                                                * */
/*     ************************************************** */

/*     SPECIFICATIONS FOR ARGUMENTS */


/*     SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/* ... VARIABLES IN COMMON BLOCK - ITCOM1 */

/*     IN     - ITERATION NUMBER */
/*     IS     - ITERATION NUMBER WHEN PARAMETERS LAST CHANGED */
/*     ISYM   - SYMMETRIC/NONSYMMETRIC STORAGE FORMAT SWITCH */
/*     ITMAX  - MAXIMUM NUMBER OF ITERATIONS ALLOWED */
/*     LEVEL  - LEVEL OF OUTPUT CONTROL SWITCH */
/*     NOUT   - OUTPUT UNIT NUMBER */

/* ... VARIABLES IN COMMON BLOCK - ITCOM2 */

/*     ADAPT  - FULLY ADAPTIVE PROCEDURE SWITCH */
/*     BETADT - SWITCH FOR ADAPTIVE DETERMINATION OF BETA */
/*     CASEII - ADAPTIVE PROCEDURE CASE SWITCH */
/*     HALT   - STOPPING TEST SWITCH */
/*     PARTAD - PARTIALLY ADAPTIVE PROCEDURE SWITCH */

/* ... VARIABLES IN COMMON BLOCK - ITCOM3 */

/*     BDELNM - TWO NORM OF B TIMES DELTA-SUPER-N */
/*     BETAB  - ESTIMATE FOR THE SPECTRAL RADIUS OF LU MATRIX */
/*     CME    - ESTIMATE OF LARGEST EIGENVALUE */
/*     DELNNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION N */
/*     DELSNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION S */
/*     FF     - ADAPTIVE PROCEDURE DAMPING FACTOR */
/*     GAMMA  - ACCELERATION PARAMETER */
/*     OMEGA  - OVERRELAXATION PARAMETER FOR SOR AND SSOR */
/*     QA     - PSEUDO-RESIDUAL RATIO */
/*     QT     - VIRTUAL SPECTRAL RADIUS */
/*     RHO    - ACCELERATION PARAMETER */
/*     RRR    - ADAPTIVE PARAMETER */
/*     SIGE   - PARAMETER SIGMA-SUB-E */
/*     SME    - ESTIMATE OF SMALLEST EIGENVALUE */
/*     SPECR  - SPECTRAL RADIUS ESTIMATE FOR SSOR */
/*     DRELPR - MACHINE RELATIVE PRECISION */
/*     STPTST - STOPPING PARAMETER */
/*     UDNM   - TWO NORM OF U */
/*     ZETA   - STOPPING CRITERION */

/* ... INITIALIZE COMMON BLOCKS */

    /* Parameter adjustments */
    --u;
    --rhs;
    --ia;
    --ja;
    --a;
    --iwksp;
    --wksp;
    --iparm;
    --rparm;

    /* Function Body */
    itcom1_1.level = iparm[2];
    itcom1_1.nout = iparm[4];
    if (iparm[9] >= 0) {
    iparm[6] = 2;
    }
    if (itcom1_1.level >= 1) {
    io___111.ciunit = itcom1_1.nout;
    s_wsfe(&io___111);
    e_wsfe();
    }
    ier = 0;
    if (iparm[1] <= 0) {
    return 0;
    }
    n = *nn;
    if (iparm[11] == 0) {
    timj1 = timer_(&dummy);
    }
    if (itcom1_1.level >= 3) {
    goto L20;
    }
    echout_(&iparm[1], &rparm[1], &c__4);
    goto L30;
L20:
    echall_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iparm[1], &rparm[1], &c__1);
L30:
    temp = itcom3_1.drelpr * 500.;
    if (itcom3_1.zeta >= temp) {
    goto L50;
    }
    if (itcom1_1.level >= 1) {
    io___117.ciunit = itcom1_1.nout;
    s_wsfe(&io___117);
    do_fio(&c__1, (char *)&itcom3_1.zeta, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.drelpr, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&temp, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    itcom3_1.zeta = temp;
L50:
    time1 = rparm[9];
    time2 = rparm[10];
    digit1 = rparm[11];
    digit2 = rparm[12];

/* ... VERIFY N */

    if (n > 0) {
    goto L70;
    }
    ier = 41;
    if (itcom1_1.level >= 0) {
    io___122.ciunit = itcom1_1.nout;
    s_wsfe(&io___122);
    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L390;
L70:

/* ... REMOVE ROWS AND COLUMNS IF REQUESTED */

    if (iparm[10] == 0) {
    goto L90;
    }
    tol = rparm[8];
    ivfill_(&n, &iwksp[1], &c__0);
    vfill_(&n, &wksp[1], &c_b21);
    sbelm_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iwksp[1], &wksp[1], &tol, &
        itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L90;
    }
    if (itcom1_1.level >= 0) {
    io___124.ciunit = itcom1_1.nout;
    s_wsfe(&io___124);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&tol, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    goto L390;

/* ... INITIALIZE WKSP BASE ADDRESSES. */

L90:
    ib1 = 1;
    ib2 = ib1 + n;
    ib3 = ib2 + n;
    ib4 = ib3 + n;
    ib5 = ib4 + n;
    ib6 = ib5 + n;
    ib7 = ib6 + n;
    iparm[8] = n * 6 + (itcom1_1.itmax << 1);
    if (itcom1_1.isym != 0) {
    iparm[8] += itcom1_1.itmax << 1;
    }
    if (*nw >= iparm[8]) {
    goto L110;
    }
    ier = 42;
    if (itcom1_1.level >= 0) {
    io___132.ciunit = itcom1_1.nout;
    s_wsfe(&io___132);
    do_fio(&c__1, (char *)&(*nw), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iparm[8], (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L390;

/* ... PERMUTE TO  RED-BLACK SYSTEM IF REQUESTED */

L110:
    nb = iparm[9];
    if (nb < 0) {
    goto L170;
    }
    n3 = n * 3;
    ivfill_(&n3, &iwksp[1], &c__0);
    prbndx_(&n, &nb, &ia[1], &ja[1], &iwksp[1], &iwksp[ib2], &itcom1_1.level, 
        &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L130;
    }
    if (itcom1_1.level >= 0) {
    io___135.ciunit = itcom1_1.nout;
    s_wsfe(&io___135);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L390;

/* ... PERMUTE MATRIX AND RHS */

L130:
    if (itcom1_1.level >= 2) {
    io___136.ciunit = itcom1_1.nout;
    s_wsfe(&io___136);
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    permat_(&n, &ia[1], &ja[1], &a[1], &iwksp[1], &iwksp[ib3], &itcom1_1.isym,
         &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L160;
    }
    if (itcom1_1.level >= 0) {
    io___137.ciunit = itcom1_1.nout;
    s_wsfe(&io___137);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L390;
L160:
    pervec_(&n, &rhs[1], &iwksp[1]);
    pervec_(&n, &u[1], &iwksp[1]);

/* ... SCALE LINEAR SYSTEM, U, AND RHS BY THE SQUARE ROOT OF THE */
/* ... DIAGONAL ELEMENTS. */

L170:
    vfill_(&iparm[8], &wksp[1], &c_b21);
    scal_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1], &
        itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L190;
    }
    if (itcom1_1.level >= 0) {
    io___138.ciunit = itcom1_1.nout;
    s_wsfe(&io___138);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L390;
L190:
    if (itcom1_1.level <= 2) {
    goto L220;
    }
    io___139.ciunit = itcom1_1.nout;
    s_wsfe(&io___139);
    e_wsfe();
    io___140.ciunit = itcom1_1.nout;
    s_wsfe(&io___140);
    e_wsfe();
L220:
    if (iparm[11] != 0) {
    goto L230;
    }
    timi1 = timer_(&dummy);

/* ... SPECIAL PROCEDURE FOR FULLY ADAPTIVE CASE. */

L230:
    if (! itcom2_1.adapt) {
    goto L250;
    }
    if (! itcom2_1.betadt) {
    goto L240;
    }
    vfill_(&n, &wksp[ib1], &c_b286);
    betnew = pbeta_(&n, &ia[1], &ja[1], &a[1], &wksp[ib1], &wksp[ib2], &wksp[
        ib3]) / (doublereal) ((real) n);
/* Computing MAX */
    d__1 = max(itcom3_1.betab,.25);
    itcom3_1.betab = max(d__1,betnew);
L240:
    omeg_(&c_b21, &c__1);
    itcom1_1.is = 0;

/* ... INITIALIZE FORWARD PSEUDO-RESIDUAL */

L250:
    dcopy_(&n, &rhs[1], &c__1, &wksp[ib1], &c__1);
    dcopy_(&n, &u[1], &c__1, &wksp[ib2], &c__1);
    pfsor_(&n, &ia[1], &ja[1], &a[1], &wksp[ib2], &wksp[ib1]);
    vevmw_(&n, &wksp[ib2], &u[1]);

/* ... ITERATION SEQUENCE */

    itmax1 = itcom1_1.itmax + 1;
    i__1 = itmax1;
    for (loop = 1; loop <= i__1; ++loop) {
    itcom1_1.in = loop - 1;
    if (itcom1_1.in % 2 == 1) {
        goto L260;
    }

/* ... CODE FOR THE EVEN ITERATIONS. */

/*     U           = U(IN)       WKSP(IB2) = C(IN) */
/*     WKSP(IB1)   = U(IN-1)     WKSP(IB3) = C(IN-1) */

    itsrcg_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[ib1], &wksp[
        ib2], &wksp[ib3], &wksp[ib4], &wksp[ib5], &wksp[ib6], &wksp[
        ib7]);

    if (itcom2_1.halt) {
        goto L300;
    }
    goto L270;

/* ... CODE FOR THE ODD ITERATIONS. */

/*     U           = U(IN-1)     WKSP(IB2) = C(IN-1) */
/*     WKSP(IB1)   = U(IN)       WKSP(IB3) =C(IN) */

L260:
    itsrcg_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &wksp[ib1], &u[1], &wksp[
        ib3], &wksp[ib2], &wksp[ib4], &wksp[ib5], &wksp[ib6], &wksp[
        ib7]);

    if (itcom2_1.halt) {
        goto L300;
    }
L270:
    ;
    }

/* ... ITMAX HAS BEEN REACHED */

    if (iparm[11] != 0) {
    goto L280;
    }
    timi2 = timer_(&dummy);
    time1 = (doublereal) (timi2 - timi1);
L280:
    if (itcom1_1.level >= 1) {
    io___146.ciunit = itcom1_1.nout;
    s_wsfe(&io___146);
    do_fio(&c__1, (char *)&itcom1_1.itmax, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    ier = 43;
    if (iparm[3] == 0) {
    rparm[1] = itcom3_1.stptst;
    }
    goto L330;

/* ... METHOD HAS CONVERGED */

L300:
    if (iparm[11] != 0) {
    goto L310;
    }
    timi2 = timer_(&dummy);
    time1 = (doublereal) (timi2 - timi1);
L310:
    if (itcom1_1.level >= 1) {
    io___147.ciunit = itcom1_1.nout;
    s_wsfe(&io___147);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    e_wsfe();
    }

/* ... PUT SOLUTION INTO U IF NOT ALREADY THERE. */

L330:
    if (itcom1_1.in % 2 == 1) {
    dcopy_(&n, &wksp[ib1], &c__1, &u[1], &c__1);
    }

/* ... UNSCALE THE MATRIX, SOLUTION, AND RHS VECTORS. */

    unscal_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1]);

/* ... UN-PERMUTE MATRIX,RHS, AND SOLUTION */

    if (iparm[9] < 0) {
    goto L360;
    }
    permat_(&n, &ia[1], &ja[1], &a[1], &iwksp[ib2], &iwksp[ib3], &
        itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ierper);
    if (ierper == 0) {
    goto L350;
    }
    if (itcom1_1.level >= 0) {
    io___149.ciunit = itcom1_1.nout;
    s_wsfe(&io___149);
    do_fio(&c__1, (char *)&ierper, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    if (ier == 0) {
    ier = ierper;
    }
    goto L390;
L350:
    pervec_(&n, &rhs[1], &iwksp[ib2]);
    pervec_(&n, &u[1], &iwksp[ib2]);

/* ... OPTIONAL ERROR ANALYSIS */

L360:
    idgts = iparm[12];
    if (idgts < 0) {
    goto L370;
    }
    if (iparm[2] <= 0) {
    idgts = 0;
    }
    perror_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1], &digit1, &
        digit2, &idgts);

/* ... SET RETURN PARAMETERS IN IPARM AND RPARM */

L370:
    if (iparm[11] != 0) {
    goto L380;
    }
    timj2 = timer_(&dummy);
    time2 = (doublereal) (timj2 - timj1);
L380:
    iparm[8] -= itcom1_1.itmax - itcom1_1.in << 1;
    if (itcom1_1.isym != 0) {
    iparm[8] -= itcom1_1.itmax - itcom1_1.in << 1;
    }
    if (iparm[3] != 0) {
    goto L390;
    }
    iparm[1] = itcom1_1.in;
    iparm[9] = nb;
    rparm[2] = itcom3_1.cme;
    rparm[3] = itcom3_1.sme;
    rparm[5] = itcom3_1.omega;
    rparm[6] = itcom3_1.specr;
    rparm[7] = itcom3_1.betab;
    rparm[9] = time1;
    rparm[10] = time2;
    rparm[11] = digit1;
    rparm[12] = digit2;

L390:
    *ierr = ier;
    if (itcom1_1.level >= 3) {
    echall_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iparm[1], &rparm[1], &
        c__2);
    }

    return 0;
} /* ssorcg_ */

/* Subroutine */ int ssorsi_(integer *nn, integer *ia, integer *ja, 
    doublereal *a, doublereal *rhs, doublereal *u, integer *iwksp, 
    integer *nw, doublereal *wksp, integer *iparm, doublereal *rparm, 
    integer *ierr)
{
    /* Format strings */
    static char fmt_10[] = "(\0020\002///1x,\002BEGINNING OF ITPACK SOLUTION\
 MODULE  SSORSI\002)";
    static char fmt_40[] = "(\0020\002,\002*** W A R N I N G ************\
\002/\0020\002,\002    IN ITPACK ROUTINE SSORSI\002/\002 \002,\002    RPARM(\
1) =\002,d10.3,\002 (ZETA)\002/\002 \002,\002    A VALUE THIS SMALL MAY HIND\
ER CONVERGENCE \002/\002 \002,\002    SINCE MACHINE PRECISION DRELPR =\002,d\
10.3/\002 \002,\002    ZETA RESET TO \002,d10.3)";
    static char fmt_60[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SSORSI \002/\002\
 \002,\002    INVALID MATRIX DIMENSION, N =\002,i8)";
    static char fmt_80[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SSORSI \002/\002\
 \002,\002    ERROR DETECTED IN SUBROUTINE  SBELM \002/\002 \002,\002    WHI\
CH REMOVES ROWS AND COLUMNS OF SYSTEM \002/\002 \002,\002    WHEN DIAGONAL E\
NTRY TOO LARGE  \002/\002 \002,\002    IER = \002,i5,5x,\002 RPARM(8) = \002\
,d10.3,\002 (TOL)\002)";
    static char fmt_100[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SSORSI \002/\002\
 \002,\002    NOT ENOUGH WORKSPACE AT \002,i10/\002 \002,\002    SET IPARM(8\
) =\002,i10,\002 (NW)\002)";
    static char fmt_120[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SSORSI \002/\002\
 \002,\002    ERROR DETECTED IN SUBROUTINE  PRBNDX\002/\002 \002,\002    WHI\
CH COMPUTES THE RED-BLACK INDEXING\002/\002 \002,\002    IER = \002,i5,\002 \
IPARM(9) = \002,i5,\002 (NB)\002)";
    static char fmt_140[] = "(/10x,\002ORDER OF BLACK SUBSYSTEM = \002,i5\
,\002 (NB)\002)";
    static char fmt_150[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SSORSI \002/\002\
 \002,\002    ERROR DETECTED IN SUBROUTINE  PERMAT\002/\002 \002,\002    WHI\
CH DOES THE RED-BLACK PERMUTATION\002/\002 \002,\002    IER = \002,i5)";
    static char fmt_180[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SSORSI \002/\002\
 \002,\002    ERROR DETECTED IN SUBROUTINE  SCAL  \002/\002 \002,\002    WHI\
CH SCALES THE SYSTEM   \002/\002 \002,\002    IER = \002,i5)";
    static char fmt_200[] = "(///1x,\002IN THE FOLLOWING, RHO AND GAMMA AR\
E\002,\002 ACCELERATION PARAMETERS\002)";
    static char fmt_280[] = "(\0020\002,\002*** W A R N I N G ***********\
*\002/\0020\002,\002    IN ITPACK ROUTINE SSORSI\002/\002 \002,\002    FAILU\
RE TO CONVERGE IN\002,i5,\002 ITERATIONS\002)";
    static char fmt_310[] = "(/1x,\002SSORSI  HAS CONVERGED IN \002,i5,\002 \
ITERATIONS\002)";
    static char fmt_330[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE SSORSI \002/\002\
 \002,\002    ERROR DETECTED IN SUBROUTINE  PERMAT\002/\002 \002,\002    WHI\
CH UNDOES THE RED-BLACK PERMUTATION   \002/\002 \002,\002    IER = \002,i5)";

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(), do_fio(integer *, char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int scal_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *, doublereal *, integer *,
         integer *, integer *), omeg_(doublereal *, integer *);
    static doublereal temp;
    static integer loop;
    static doublereal time1, time2;
    static real timi1, timj1, timi2, timj2;
    static integer n;
    extern doublereal pbeta_(integer *, integer *, integer *, doublereal *, 
        doublereal *, doublereal *, doublereal *);
    extern /* Subroutine */ int sbelm_(integer *, integer *, integer *, 
        doublereal *, doublereal *, integer *, doublereal *, doublereal *,
         integer *, integer *, integer *, integer *);
    static integer idgts;
    extern /* Subroutine */ int vfill_(integer *, doublereal *, doublereal *),
         dcopy_(integer *, doublereal *, integer *, doublereal *, integer 
        *);
    extern doublereal timer_(real *);
    static real dummy;
    static integer n3;
    static doublereal digit1, digit2;
    static integer itmax1, nb;
    extern /* Subroutine */ int echall_(integer *, integer *, integer *, 
        doublereal *, doublereal *, integer *, doublereal *, integer *);
    static doublereal betnew;
    extern /* Subroutine */ int ivfill_(integer *, integer *, integer *);
    static integer ierper;
    extern /* Subroutine */ int echout_(integer *, doublereal *, integer *), 
        permat_(integer *, integer *, integer *, doublereal *, integer *, 
        integer *, integer *, integer *, integer *, integer *), pervec_(
        integer *, doublereal *, integer *), unscal_(integer *, integer *,
         integer *, doublereal *, doublereal *, doublereal *, doublereal *
        );
    static integer ib1, ib2, ib3, ib4, ib5;
    extern /* Subroutine */ int prbndx_(integer *, integer *, integer *, 
        integer *, integer *, integer *, integer *, integer *, integer *),
         perror_(integer *, integer *, integer *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *, 
        doublereal *, integer *), itsrsi_(integer *, integer *, integer *,
         doublereal *, doublereal *, doublereal *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *);
    static integer ier;
    static doublereal tol;

    /* Fortran I/O blocks */
    static cilist io___152 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___158 = { 0, 0, 0, fmt_40, 0 };
    static cilist io___163 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___165 = { 0, 0, 0, fmt_80, 0 };
    static cilist io___171 = { 0, 0, 0, fmt_100, 0 };
    static cilist io___174 = { 0, 0, 0, fmt_120, 0 };
    static cilist io___175 = { 0, 0, 0, fmt_140, 0 };
    static cilist io___176 = { 0, 0, 0, fmt_150, 0 };
    static cilist io___177 = { 0, 0, 0, fmt_180, 0 };
    static cilist io___178 = { 0, 0, 0, fmt_200, 0 };
    static cilist io___184 = { 0, 0, 0, fmt_280, 0 };
    static cilist io___185 = { 0, 0, 0, fmt_310, 0 };
    static cilist io___187 = { 0, 0, 0, fmt_330, 0 };



/*     ITPACK 2C MAIN SUBROUTINE  SSORSI  (SYMMETRIC SUCCESSIVE RELAX- */
/*                                         ATION SEMI-ITERATION) */
/*     EACH OF THE MAIN SUBROUTINES: */
/*           JCG, JSI, SOR, SSORCG, SSORSI, RSCG, RSSI */
/*     CAN BE USED INDEPENDENTLY OF THE OTHERS */

/* ... FUNCTION: */

/*          THIS SUBROUTINE, SSORSI, DRIVES THE  SYMMETRIC SOR-SI */
/*          ALGORITHM. */

/* ... PARAMETER LIST: */

/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX. (= NN) */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF */
/*                 THE SPARSE MATRIX REPRESENTATION. */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE */
/*                 MATRIX REPRESENTATION. */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE */
/*                 OF THE MATRIX PROBLEM. */
/*          U      INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U CONTAINS THE */
/*                 INITIAL GUESS TO THE SOLUTION. ON OUTPUT, IT CONTAINS */
/*                 THE LATEST ESTIMATE TO THE SOLUTION. */
/*          IWKSP  INTEGER VECTOR WORKSPACE OF LENGTH 3*N */
/*          NW     INPUT INTEGER.  LENGTH OF AVAILABLE WKSP.  ON OUTPUT, */
/*                 IPARM(8) IS AMOUNT USED. */
/*          WKSP   D.P. VECTOR USED FOR WORKING SPACE.  SSORSI */
/*                 NEEDS THIS TO BE IN LENGTH AT LEAST  5*N */
/*          IPARM  INTEGER VECTOR OF LENGTH 12.  ALLOWS USER TO SPECIFY */
/*                 SOME INTEGER PARAMETERS WHICH AFFECT THE METHOD.  IF */
/*          RPARM  D.P. VECTOR OF LENGTH 12. ALLOWS USER TO SPECIFY SOME */
/*                 D.P. PARAMETERS WHICH AFFECT THE METHOD. */
/*          IER    OUTPUT INTEGER.  ERROR FLAG. (= IERR) */

/* ... SSORSI SUBPROGRAM REFERENCES: */

/*          FROM ITPACK    BISRCH, CHEBY, CHGSI, DFAULT, ECHALL, ECHOUT, */
/*                         ITERM, TIMER, ITSRSI, IVFILL, OMEG, */
/*                         OMGSTR, PARSI, PBETA, PERMAT, PERROR, */
/*                         PERVEC, PFSOR, PMULT, PRBNDX, PSSOR1, */
/*                         PSTOP, PVTBV, QSORT, SBELM, SCAL, DCOPY, */
/*                         DDOT, SUM3, TSTCHG, UNSCAL, VEVPW, VFILL, */
/*                         VOUT, WEVMW */
/*          SYSTEM         DABS, DLOG, DLOG10, DBLE(AMAX0), DMAX1, DBLE(F */
/*                         MOD, DSQRT */

/*     VERSION:  ITPACK 2C (MARCH 1982) */

/*     CODE WRITTEN BY:  DAVID KINCAID, ROGER GRIMES, JOHN RESPESS */
/*                       CENTER FOR NUMERICAL ANALYSIS */
/*                       UNIVERSITY OF TEXAS */
/*                       AUSTIN, TX  78712 */
/*                       (512) 471-1242 */

/*     FOR ADDITIONAL DETAILS ON THE */
/*          (A) SUBROUTINE SEE TOMS ARTICLE 1982 */
/*          (B) ALGORITHM  SEE CNA REPORT 150 */

/*     BASED ON THEORY BY:  DAVID YOUNG, DAVID KINCAID, LOU HAGEMAN */

/*     REFERENCE THE BOOK:  APPLIED ITERATIVE METHODS */
/*                          L. HAGEMAN, D. YOUNG */
/*                          ACADEMIC PRESS, 1981 */

/*     ************************************************** */
/*     *               IMPORTANT NOTE                   * */
/*     *                                                * */
/*     *      WHEN INSTALLING ITPACK ROUTINES ON A      * */
/*     *  DIFFERENT COMPUTER, RESET SOME OF THE VALUES  * */
/*     *  IN  SUBROUTNE DFAULT.   MOST IMPORTANT ARE    * */
/*     *                                                * */
/*     *   DRELPR      MACHINE RELATIVE PRECISION       * */
/*     *   RPARM(1)    STOPPING CRITERION               * */
/*     *                                                * */
/*     *   ALSO CHANGE SYSTEM-DEPENDENT ROUTINE         * */
/*     *   SECOND USED IN TIMER                         * */
/*     *                                                * */
/*     ************************************************** */

/*     SPECIFICATIONS FOR ARGUMENTS */


/*     SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/* ... VARIABLES IN COMMON BLOCK - ITCOM1 */

/*     IN     - ITERATION NUMBER */
/*     ISYM   - SYMMETRIC/NONSYMMETRIC STORAGE FORMAT SWITCH */
/*     IS     - ITERATION NUMBER WHEN PARAMETERS LAST CHANGED */
/*     ITMAX  - MAXIMUM NUMBER OF ITERATIONS ALLOWED */
/*     LEVEL  - LEVEL OF OUTPUT CONTROL SWITCH */
/*     NOUT   - OUTPUT UNIT NUMBER */

/* ... VARIABLES IN COMMON BLOCK - ITCOM2 */

/*     ADAPT  - FULLY ADAPTIVE PROCEDURE SWITCH */
/*     BETADT - SWITCH FOR ADAPTIVE DETERMINATION OF BETA */
/*     CASEII - ADAPTIVE PROCEDURE CASE SWITCH */
/*     HALT   - STOPPING TEST SWITCH */
/*     PARTAD - PARTIALLY ADAPTIVE PROCEDURE SWITCH */

/* ... VARIABLES IN COMMON BLOCK - ITCOM3 */

/*     BDELNM - TWO NORM OF B TIMES DELTA-SUPER-N */
/*     BETAB  - ESTIMATE FOR THE SPECTRAL RADIUS OF LU MATRIX */
/*     CME    - ESTIMATE OF LARGEST EIGENVALUE */
/*     DELNNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION N */
/*     DELSNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION S */
/*     FF     - ADAPTIVE PROCEDURE DAMPING FACTOR */
/*     GAMMA  - ACCELERATION PARAMETER */
/*     OMEGA  - OVERRELAXATION PARAMETER FOR SOR AND SSOR */
/*     QA     - PSEUDO-RESIDUAL RATIO */
/*     QT     - VIRTUAL SPECTRAL RADIUS */
/*     RHO    - ACCELERATION PARAMETER */
/*     RRR    - ADAPTIVE PARAMETER */
/*     SIGE   - PARAMETER SIGMA-SUB-E */
/*     SME    - ESTIMATE OF SMALLEST EIGENVALUE */
/*     SPECR  - SPECTRAL RADIUS ESTIMATE FOR SSOR */
/*     DRELPR - MACHINE RELATIVE PRECISION */
/*     STPTST - STOPPING PARAMETER */
/*     UDNM   - TWO NORM OF U */
/*     ZETA   - STOPPING CRITERION */

/* ... INITIALIZE COMMON BLOCKS */

    /* Parameter adjustments */
    --u;
    --rhs;
    --ia;
    --ja;
    --a;
    --iwksp;
    --wksp;
    --iparm;
    --rparm;

    /* Function Body */
    itcom1_1.level = iparm[2];
    itcom1_1.nout = iparm[4];
    if (iparm[9] >= 0) {
    iparm[6] = 2;
    }
    if (itcom1_1.level >= 1) {
    io___152.ciunit = itcom1_1.nout;
    s_wsfe(&io___152);
    e_wsfe();
    }
    ier = 0;
    if (iparm[1] <= 0) {
    return 0;
    }
    n = *nn;
    if (iparm[11] == 0) {
    timj1 = timer_(&dummy);
    }
    if (itcom1_1.level >= 3) {
    goto L20;
    }
    echout_(&iparm[1], &rparm[1], &c__5);
    goto L30;
L20:
    echall_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iparm[1], &rparm[1], &c__1);
L30:
    temp = itcom3_1.drelpr * 500.;
    if (itcom3_1.zeta >= temp) {
    goto L50;
    }
    if (itcom1_1.level >= 1) {
    io___158.ciunit = itcom1_1.nout;
    s_wsfe(&io___158);
    do_fio(&c__1, (char *)&itcom3_1.zeta, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.drelpr, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&temp, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    itcom3_1.zeta = temp;
L50:
    time1 = rparm[9];
    time2 = rparm[10];
    digit1 = rparm[11];
    digit2 = rparm[12];

/* ... VERIFY N */

    if (n > 0) {
    goto L70;
    }
    ier = 51;
    if (itcom1_1.level >= 0) {
    io___163.ciunit = itcom1_1.nout;
    s_wsfe(&io___163);
    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L380;
L70:

/* ... REMOVE ROWS AND COLUMNS IF REQUESTED */

    if (iparm[10] == 0) {
    goto L90;
    }
    tol = rparm[8];
    ivfill_(&n, &iwksp[1], &c__0);
    vfill_(&n, &wksp[1], &c_b21);
    sbelm_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iwksp[1], &wksp[1], &tol, &
        itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L90;
    }
    if (itcom1_1.level >= 0) {
    io___165.ciunit = itcom1_1.nout;
    s_wsfe(&io___165);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&tol, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    goto L380;

/* ... INITIALIZE WKSP BASE ADDRESSES. */

L90:
    ib1 = 1;
    ib2 = ib1 + n;
    ib3 = ib2 + n;
    ib4 = ib3 + n;
    ib5 = ib4 + n;
    iparm[8] = n * 5;
    if (*nw >= iparm[8]) {
    goto L110;
    }
    ier = 52;
    if (itcom1_1.level >= 0) {
    io___171.ciunit = itcom1_1.nout;
    s_wsfe(&io___171);
    do_fio(&c__1, (char *)&(*nw), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iparm[8], (ftnlen)sizeof(integer));
    e_wsfe();
    }

/* ... PERMUTE TO  RED-BLACK SYSTEM IF REQUESTED */

L110:
    nb = iparm[9];
    if (nb < 0) {
    goto L170;
    }
    n3 = n * 3;
    ivfill_(&n3, &iwksp[1], &c__0);
    prbndx_(&n, &nb, &ia[1], &ja[1], &iwksp[1], &iwksp[ib2], &itcom1_1.level, 
        &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L130;
    }
    if (itcom1_1.level >= 0) {
    io___174.ciunit = itcom1_1.nout;
    s_wsfe(&io___174);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L380;

/* ... PERMUTE MATRIX AND RHS */

L130:
    if (itcom1_1.level >= 2) {
    io___175.ciunit = itcom1_1.nout;
    s_wsfe(&io___175);
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    permat_(&n, &ia[1], &ja[1], &a[1], &iwksp[1], &iwksp[ib3], &itcom1_1.isym,
         &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L160;
    }
    if (itcom1_1.level >= 0) {
    io___176.ciunit = itcom1_1.nout;
    s_wsfe(&io___176);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L380;
L160:
    pervec_(&n, &rhs[1], &iwksp[1]);
    pervec_(&n, &u[1], &iwksp[1]);

/* ... SCALE LINEAR SYSTEM, U, AND RHS BY THE SQUARE ROOT OF THE */
/* ... DIAGONAL ELEMENTS. */

L170:
    vfill_(&iparm[8], &wksp[1], &c_b21);
    scal_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1], &
        itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L190;
    }
    if (itcom1_1.level >= 0) {
    io___177.ciunit = itcom1_1.nout;
    s_wsfe(&io___177);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L380;
L190:
    if (itcom1_1.level <= 2) {
    goto L210;
    }
    io___178.ciunit = itcom1_1.nout;
    s_wsfe(&io___178);
    e_wsfe();
L210:
    if (iparm[11] != 0) {
    goto L220;
    }
    timi1 = timer_(&dummy);

/* ... SPECIAL PROCEDURE FOR FULLY ADAPTIVE CASE. */

L220:
    if (! itcom2_1.adapt) {
    goto L240;
    }
    if (! itcom2_1.betadt) {
    goto L230;
    }
    vfill_(&n, &wksp[ib1], &c_b286);
    betnew = pbeta_(&n, &ia[1], &ja[1], &a[1], &wksp[ib1], &wksp[ib2], &wksp[
        ib3]) / (doublereal) ((real) n);
/* Computing MAX */
    d__1 = max(itcom3_1.betab,.25);
    itcom3_1.betab = max(d__1,betnew);
L230:
    omeg_(&c_b21, &c__1);
    itcom1_1.is = 0;

/* ... ITERATION SEQUENCE */

L240:
    itmax1 = itcom1_1.itmax + 1;
    i__1 = itmax1;
    for (loop = 1; loop <= i__1; ++loop) {
    itcom1_1.in = loop - 1;
    if (itcom1_1.in % 2 == 1) {
        goto L250;
    }

/* ... CODE FOR THE EVEN ITERATIONS. */

/*     U           = U(IN) */
/*     WKSP(IB1)   = U(IN-1) */

    itsrsi_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[ib1], &wksp[
        ib2], &wksp[ib3], &wksp[ib4], &wksp[ib5]);

    if (itcom2_1.halt) {
        goto L290;
    }
    goto L260;

/* ... CODE FOR THE ODD ITERATIONS. */

/*     U           = U(IN-1) */
/*     WKSP(IB1)   = U(IN) */

L250:
    itsrsi_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &wksp[ib1], &u[1], &wksp[
        ib2], &wksp[ib3], &wksp[ib4], &wksp[ib5]);

    if (itcom2_1.halt) {
        goto L290;
    }
L260:
    ;
    }

/* ... ITMAX HAS BEEN REACHED */

    if (iparm[11] != 0) {
    goto L270;
    }
    timi2 = timer_(&dummy);
    time1 = (doublereal) (timi2 - timi1);
L270:
    if (itcom1_1.level >= 1) {
    io___184.ciunit = itcom1_1.nout;
    s_wsfe(&io___184);
    do_fio(&c__1, (char *)&itcom1_1.itmax, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    ier = 53;
    if (iparm[3] == 0) {
    rparm[1] = itcom3_1.stptst;
    }
    goto L320;

/* ... METHOD HAS CONVERGED */

L290:
    if (iparm[11] != 0) {
    goto L300;
    }
    timi2 = timer_(&dummy);
    time1 = (doublereal) (timi2 - timi1);
L300:
    if (itcom1_1.level >= 1) {
    io___185.ciunit = itcom1_1.nout;
    s_wsfe(&io___185);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    e_wsfe();
    }

/* ... PUT SOLUTION INTO U IF NOT ALREADY THERE. */

L320:
    if (itcom1_1.in % 2 == 1) {
    dcopy_(&n, &wksp[ib1], &c__1, &u[1], &c__1);
    }

/* ... UNSCALE THE MATRIX, SOLUTION, AND RHS VECTORS. */

    unscal_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1]);

/* ... UN-PERMUTE MATRIX,RHS, AND SOLUTION */

    if (iparm[9] < 0) {
    goto L350;
    }
    permat_(&n, &ia[1], &ja[1], &a[1], &iwksp[ib2], &iwksp[ib3], &
        itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ierper);
    if (ierper == 0) {
    goto L340;
    }
    if (itcom1_1.level >= 0) {
    io___187.ciunit = itcom1_1.nout;
    s_wsfe(&io___187);
    do_fio(&c__1, (char *)&ierper, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    if (ier == 0) {
    ier = ierper;
    }
    goto L380;
L340:
    pervec_(&n, &rhs[1], &iwksp[ib2]);
    pervec_(&n, &u[1], &iwksp[ib2]);

/* ... OPTIONAL ERROR ANALYSIS */

L350:
    idgts = iparm[12];
    if (idgts < 0) {
    goto L360;
    }
    if (iparm[2] <= 0) {
    idgts = 0;
    }
    perror_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1], &digit1, &
        digit2, &idgts);

/* ... SET RETURN PARAMETERS IN IPARM AND RPARM */

L360:
    if (iparm[11] != 0) {
    goto L370;
    }
    timj2 = timer_(&dummy);
    time2 = (doublereal) (timj2 - timj1);
L370:
    if (iparm[3] != 0) {
    goto L380;
    }
    iparm[1] = itcom1_1.in;
    iparm[9] = nb;
    rparm[2] = itcom3_1.cme;
    rparm[3] = itcom3_1.sme;
    rparm[5] = itcom3_1.omega;
    rparm[6] = itcom3_1.specr;
    rparm[7] = itcom3_1.betab;
    rparm[9] = time1;
    rparm[10] = time2;
    rparm[11] = digit1;
    rparm[12] = digit2;

L380:
    *ierr = ier;
    if (itcom1_1.level >= 3) {
    echall_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iparm[1], &rparm[1], &
        c__2);
    }

    return 0;
} /* ssorsi_ */

/* Subroutine */ int rscg_(integer *nn, integer *ia, integer *ja, doublereal *
    a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, 
    doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr)
{
    /* Format strings */
    static char fmt_10[] = "(\0020\002///1x,\002BEGINNING OF ITPACK SOLUTION\
 MODULE  RSCG\002)";
    static char fmt_40[] = "(\0020\002,\002*** W A R N I N G ************\
\002/\0020\002,\002    IN ITPACK ROUTINE RSCG\002/\002 \002,\002    RPARM(1)\
 =\002,d10.3,\002 (ZETA)\002/\002 \002,\002    A VALUE THIS SMALL MAY HINDER\
 CONVERGENCE \002/\002 \002,\002    SINCE MACHINE PRECISION DRELPR =\002,d10\
.3/\002 \002,\002    ZETA RESET TO \002,d10.3)";
    static char fmt_60[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE RSCG \002/\002 \002,\
\002    INVALID MATRIX DIMENSION, N =\002,i8)";
    static char fmt_80[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE RSCG \002/\002 \002,\
\002    ERROR DETECTED IN SUBROUTINE  SBELM \002/\002 \002,\002    WHICH REM\
OVES ROWS AND COLUMNS OF SYSTEM \002/\002 \002,\002    WHEN DIAGONAL ENTRY T\
OO LARGE  \002/\002 \002,\002    IER = \002,i5,5x,\002 RPARM(8) = \002,d10.3,\
\002 (TOL)\002)";
    static char fmt_100[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE RSCG \002/\002 \
\002,\002    ERROR DETECTED IN SUBROUTINE  PRBNDX\002/\002 \002,\002    WHIC\
H COMPUTES THE RED-BLACK INDEXING\002/\002 \002,\002    IER = \002,i5,\002 I\
PARM(9) = \002,i5,\002 (NB)\002)";
    static char fmt_120[] = "(/10x,\002ERROR DETECTED IN RED-BLACK SUBSYSTEM\
 INDEX\002/10x,\002IER =\002,i5,\002 IPARM(9) =\002,i5,\002 (NB)\002)";
    static char fmt_140[] = "(/10x,\002 IPARM(9) = \002,i5,\002 IMPLIES MATR\
IX IS DIAGONAL\002/10x,\002 NB RESET TO \002,i5)";
    static char fmt_160[] = "(/10x,\002ORDER OF BLACK SUBSYSTEM = \002,i5\
,\002 (NB)\002)";
    static char fmt_170[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE RSCG \002/\002 \
\002,\002    ERROR DETECTED IN SUBROUTINE  PERMAT\002/\002 \002,\002    WHIC\
H DOES THE RED-BLACK PERMUTATION\002/\002 \002,\002    IER = \002,i5)";
    static char fmt_200[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE RSCG \002/\002 \
\002,\002    NOT ENOUGH WORKSPACE AT \002,i10/\002 \002,\002    SET IPARM(8)\
 =\002,i10,\002 (NW)\002)";
    static char fmt_220[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE RSCG \002/\002 \
\002,\002    ERROR DETECTED IN SUBROUTINE  SCAL  \002/\002 \002,\002    WHIC\
H SCALES THE SYSTEM   \002/\002 \002,\002    IER = \002,i5)";
    static char fmt_240[] = "(///1x,\002IN THE FOLLOWING, RHO AND GAMMA AR\
E\002,\002 ACCELERATION PARAMETERS\002)";
    static char fmt_250[] = "(1x,\002CME IS THE ESTIMATE OF THE LARGEST EIGE\
NVALUE OF\002,\002 THE JACOBI MATRIX\002)";
    static char fmt_320[] = "(\0020\002,\002*** W A R N I N G ***********\
*\002/\0020\002,\002    IN ITPACK ROUTINE RSCG\002/\002 \002,\002    FAILURE\
 TO CONVERGE IN\002,i5,\002 ITERATIONS\002)";
    static char fmt_350[] = "(/1x,\002RSCG  HAS CONVERGED IN \002,i5,\002 IT\
ERATIONS\002)";
    static char fmt_380[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE RSCG \002/\002 \
\002,\002    ERROR DETECTED IN SUBROUTINE  PERMAT\002/\002 \002,\002    WHIC\
H UNDOES THE RED-BLACK PERMUTATION   \002/\002 \002,\002    IER = \002,i5)";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(), do_fio(integer *, char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int scal_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *, doublereal *, integer *,
         integer *, integer *);
    static doublereal temp;
    static integer loop;
    static doublereal time1, time2;
    static real timi1, timj1, timi2, timj2;
    static integer n;
    extern /* Subroutine */ int sbelm_(integer *, integer *, integer *, 
        doublereal *, doublereal *, integer *, doublereal *, doublereal *,
         integer *, integer *, integer *, integer *);
    static integer idgts;
    extern /* Subroutine */ int vfill_(integer *, doublereal *, doublereal *),
         dcopy_(integer *, doublereal *, integer *, doublereal *, integer 
        *);
    extern doublereal timer_(real *);
    static real dummy;
    static integer n3;
    extern /* Subroutine */ int vevmw_(integer *, doublereal *, doublereal *);
    static doublereal digit1, digit2;
    static integer itmax1, nb;
    extern /* Subroutine */ int echall_(integer *, integer *, integer *, 
        doublereal *, doublereal *, integer *, doublereal *, integer *);
    static integer nr;
    extern /* Subroutine */ int pervec_(integer *, doublereal *, integer *), 
        ivfill_(integer *, integer *, integer *);
    static integer ierper;
    extern /* Subroutine */ int echout_(integer *, doublereal *, integer *), 
        permat_(integer *, integer *, integer *, doublereal *, integer *, 
        integer *, integer *, integer *, integer *, integer *), unscal_(
        integer *, integer *, integer *, doublereal *, doublereal *, 
        doublereal *, doublereal *), itrscg_(integer *, integer *, 
        integer *, integer *, doublereal *, doublereal *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *);
    static integer ib1, ib2, ib3, ib4, ib5, jb3;
    extern /* Subroutine */ int prbndx_(integer *, integer *, integer *, 
        integer *, integer *, integer *, integer *, integer *, integer *),
         prsred_(integer *, integer *, integer *, integer *, doublereal *,
         doublereal *, doublereal *), prsblk_(integer *, integer *, 
        integer *, integer *, doublereal *, doublereal *, doublereal *), 
        perror_(integer *, integer *, integer *, doublereal *, doublereal 
        *, doublereal *, doublereal *, doublereal *, doublereal *, 
        integer *);
    static integer ier;
    static doublereal tol;
    static integer nrp1;

    /* Fortran I/O blocks */
    static cilist io___190 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___196 = { 0, 0, 0, fmt_40, 0 };
    static cilist io___201 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___203 = { 0, 0, 0, fmt_80, 0 };
    static cilist io___209 = { 0, 0, 0, fmt_100, 0 };
    static cilist io___210 = { 0, 0, 0, fmt_120, 0 };
    static cilist io___211 = { 0, 0, 0, fmt_140, 0 };
    static cilist io___212 = { 0, 0, 0, fmt_160, 0 };
    static cilist io___213 = { 0, 0, 0, fmt_170, 0 };
    static cilist io___219 = { 0, 0, 0, fmt_200, 0 };
    static cilist io___220 = { 0, 0, 0, fmt_220, 0 };
    static cilist io___221 = { 0, 0, 0, fmt_240, 0 };
    static cilist io___222 = { 0, 0, 0, fmt_250, 0 };
    static cilist io___227 = { 0, 0, 0, fmt_320, 0 };
    static cilist io___228 = { 0, 0, 0, fmt_350, 0 };
    static cilist io___230 = { 0, 0, 0, fmt_380, 0 };



/*     ITPACK 2C MAIN SUBROUTINE  RSCG  (REDUCED SYSTEM CONJUGATE */
/*                                       GRADIENT) */
/*     EACH OF THE MAIN SUBROUTINES: */
/*           JCG, JSI, SOR, SSORCG, SSORSI, RSCG, RSSI */
/*     CAN BE USED INDEPENDENTLY OF THE OTHERS */

/* ... FUNCTION: */

/*          THIS SUBROUTINE, RSCG, DRIVES THE  REDUCED SYSTEM CG */
/*          ALGORITHM. */

/* ... PARAMETER LIST: */

/*          N     INPUT INTEGER.  DIMENSION OF THE MATRIX. (= NN) */
/*                 IN THE RED-BLACK MATRIX. */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF */
/*                 THE SPARSE MATRIX REPRESENTATION. */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE */
/*                 MATRIX REPRESENTATION. */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE */
/*                 OF THE MATRIX PROBLEM. */
/*          U      INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U CONTAINS THE */
/*                 INITIAL GUESS TO THE SOLUTION. ON OUTPUT, IT CONTAINS */
/*                 THE LATEST ESTIMATE TO THE SOLUTION. */
/*          IWKSP  INTEGER VECTOR WORKSPACE OF LENGTH 3*N */
/*          NW     INPUT INTEGER.  LENGTH OF AVAILABLE WKSP.  ON OUTPUT, */
/*                 IPARM(8) IS AMOUNT USED. */
/*          WKSP   D.P. VECTOR USED FOR WORKING SPACE.  RSCG NEEDS */
/*                 THIS TO BE IN LENGTH AT LEAST */
/*                 N+3*NB+2*ITMAX, IF IPARM(5)=0  (SYMMETRIC STORAGE) */
/*                 N+3*NB+4*ITMAX, IF IPARM(5)=1  (NONSYMMETRIC STORAGE) */
/*                 HERE NB IS THE ORDER OF THE BLACK SUBSYSTEM */
/*          IPARM  INTEGER VECTOR OF LENGTH 12.  ALLOWS USER TO SPECIFY */
/*                 SOME INTEGER PARAMETERS WHICH AFFECT THE METHOD.  IF */
/*          RPARM  D.P. VECTOR OF LENGTH 12. ALLOWS USER TO SPECIFY SOME */
/*                 D.P. PARAMETERS WHICH AFFECT THE METHOD. */
/*          IER    OUTPUT INTEGER. ERROR FLAG. (= IERR) */

/* ... RSCG SUBPROGRAM REFERENCES: */

/*          FROM ITPACK    BISRCH, CHGCON, DETERM, DFAULT, ECHALL, */
/*                         ECHOUT, EIGVNS, EIGVSS, EQRT1S, ITERM, TIMER */
/*                         ITRSCG, IVFILL, PARCON, PERMAT, */
/*                         PERROR, PERVEC, PMULT, PRBNDX, PRSBLK, */
/*                         PRSRED, PSTOP, QSORT, SBELM, SCAL, DCOPY, */
/*                         DDOT, SUM3, UNSCAL, VFILL, VOUT, WEVMW, */
/*                         ZBRENT */
/*          SYSTEM         DABS, DLOG10, DBLE(AMAX0), DMAX1, MOD, DSQRT */

/*     VERSION:  ITPACK 2C (MARCH 1982) */

/*     CODE WRITTEN BY:  DAVID KINCAID, ROGER GRIMES, JOHN RESPESS */
/*                       CENTER FOR NUMERICAL ANALYSIS */
/*                       UNIVERSITY OF TEXAS */
/*                       AUSTIN, TX  78712 */
/*                       (512) 471-1242 */

/*     FOR ADDITIONAL DETAILS ON THE */
/*          (A) SUBROUTINE SEE TOMS ARTICLE 1982 */
/*          (B) ALGORITHM  SEE CNA REPORT 150 */

/*     BASED ON THEORY BY:  DAVID YOUNG, DAVID KINCAID, LOU HAGEMAN */

/*     REFERENCE THE BOOK:  APPLIED ITERATIVE METHODS */
/*                          L. HAGEMAN, D. YOUNG */
/*                          ACADEMIC PRESS, 1981 */

/*     ************************************************** */
/*     *               IMPORTANT NOTE                   * */
/*     *                                                * */
/*     *      WHEN INSTALLING ITPACK ROUTINES ON A      * */
/*     *  DIFFERENT COMPUTER, RESET SOME OF THE VALUES  * */
/*     *  IN  SUBROUTNE DFAULT.   MOST IMPORTANT ARE    * */
/*     *                                                * */
/*     *   DRELPR      MACHINE RELATIVE PRECISION       * */
/*     *   RPARM(1)    STOPPING CRITERION               * */
/*     *                                                * */
/*     *   ALSO CHANGE SYSTEM-DEPENDENT ROUTINE         * */
/*     *   SECOND USED IN TIMER                         * */
/*     *                                                * */
/*     ************************************************** */

/*     SPECIFICATIONS FOR ARGUMENTS */


/*     SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/* ... VARIABLES IN COMMON BLOCK - ITCOM1 */

/*     IN     - ITERATION NUMBER */
/*     IS     - ITERATION NUMBER WHEN PARAMETERS LAST CHANGED */
/*     ISYM   - SYMMETRIC/NONSYMMETRIC STORAGE FORMAT SWITCH */
/*     ITMAX  - MAXIMUM NUMBER OF ITERATIONS ALLOWED */
/*     LEVEL  - LEVEL OF OUTPUT CONTROL SWITCH */
/*     NOUT   - OUTPUT UNIT NUMBER */

/* ... VARIABLES IN COMMON BLOCK - ITCOM2 */

/*     ADAPT  - FULLY ADAPTIVE PROCEDURE SWITCH */
/*     BETADT - SWITCH FOR ADAPTIVE DETERMINATION OF BETA */
/*     CASEII - ADAPTIVE PROCEDURE CASE SWITCH */
/*     HALT   - STOPPING TEST SWITCH */
/*     PARTAD - PARTIALLY ADAPTIVE PROCEDURE SWITCH */

/* ... VARIABLES IN COMMON BLOCK - ITCOM3 */

/*     BDELNM - TWO NORM OF B TIMES DELTA-SUPER-N */
/*     BETAB  - ESTIMATE FOR THE SPECTRAL RADIUS OF LU MATRIX */
/*     CME    - ESTIMATE OF LARGEST EIGENVALUE */
/*     DELNNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION N */
/*     DELSNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION S */
/*     FF     - ADAPTIVE PROCEDURE DAMPING FACTOR */
/*     GAMMA  - ACCELERATION PARAMETER */
/*     OMEGA  - OVERRELAXATION PARAMETER FOR SOR AND SSOR */
/*     QA     - PSEUDO-RESIDUAL RATIO */
/*     QT     - VIRTUAL SPECTRAL RADIUS */
/*     RHO    - ACCELERATION PARAMETER */
/*     RRR    - ADAPTIVE PARAMETER */
/*     SIGE   - PARAMETER SIGMA-SUB-E */
/*     SME    - ESTIMATE OF SMALLEST EIGENVALUE */
/*     SPECR  - SPECTRAL RADIUS ESTIMATE FOR SSOR */
/*     DRELPR - MACHINE RELATIVE PRECISION */
/*     STPTST - STOPPING PARAMETER */
/*     UDNM   - TWO NORM OF U */
/*     ZETA   - STOPPING CRITERION */

/* ... INITIALIZE COMMON BLOCKS */

    /* Parameter adjustments */
    --u;
    --rhs;
    --ia;
    --ja;
    --a;
    --iwksp;
    --wksp;
    --iparm;
    --rparm;

    /* Function Body */
    itcom1_1.level = iparm[2];
    itcom1_1.nout = iparm[4];
    if (itcom1_1.level >= 1) {
    io___190.ciunit = itcom1_1.nout;
    s_wsfe(&io___190);
    e_wsfe();
    }
    ier = 0;
    if (iparm[1] <= 0) {
    return 0;
    }
    n = *nn;
    if (iparm[11] == 0) {
    timj1 = timer_(&dummy);
    }
    if (itcom1_1.level >= 3) {
    goto L20;
    }
    echout_(&iparm[1], &rparm[1], &c__6);
    goto L30;
L20:
    echall_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iparm[1], &rparm[1], &c__1);
L30:
    temp = itcom3_1.drelpr * 500.;
    if (itcom3_1.zeta >= temp) {
    goto L50;
    }
    if (itcom1_1.level >= 1) {
    io___196.ciunit = itcom1_1.nout;
    s_wsfe(&io___196);
    do_fio(&c__1, (char *)&itcom3_1.zeta, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.drelpr, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&temp, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    itcom3_1.zeta = temp;
L50:
    time1 = rparm[9];
    time2 = rparm[10];
    digit1 = rparm[11];
    digit2 = rparm[12];

/* ... VERIFY N */

    if (n > 0) {
    goto L70;
    }
    ier = 61;
    if (itcom1_1.level >= 0) {
    io___201.ciunit = itcom1_1.nout;
    s_wsfe(&io___201);
    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L430;
L70:

/* ... REMOVE ROWS AND COLUMNS IF REQUESTED */

    if (iparm[10] == 0) {
    goto L90;
    }
    tol = rparm[8];
    ivfill_(&n, &iwksp[1], &c__0);
    vfill_(&n, &wksp[1], &c_b21);
    sbelm_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iwksp[1], &wksp[1], &tol, &
        itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L90;
    }
    if (itcom1_1.level >= 0) {
    io___203.ciunit = itcom1_1.nout;
    s_wsfe(&io___203);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&tol, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    goto L430;

/* ... INITIALIZE WKSP BASE ADDRESSES. */

L90:
    ib1 = 1;
    ib2 = ib1 + n;
    jb3 = ib2 + n;

/* ... PERMUTE TO  RED-BLACK SYSTEM IF POSSIBLE */

    nb = iparm[9];
    if (nb >= 0) {
    goto L110;
    }
    n3 = n * 3;
    ivfill_(&n3, &iwksp[1], &c__0);
    prbndx_(&n, &nb, &ia[1], &ja[1], &iwksp[1], &iwksp[ib2], &itcom1_1.level, 
        &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L110;
    }
    if (itcom1_1.level >= 0) {
    io___209.ciunit = itcom1_1.nout;
    s_wsfe(&io___209);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L430;
L110:
    if (nb >= 0 && nb <= n) {
    goto L130;
    }
    ier = 64;
    if (itcom1_1.level >= 1) {
    io___210.ciunit = itcom1_1.nout;
    s_wsfe(&io___210);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L430;
L130:
    if (nb != 0 && nb != n) {
    goto L150;
    }
    nb = n / 2;
    if (itcom1_1.level >= 2 && iparm[9] >= 0) {
    io___211.ciunit = itcom1_1.nout;
    s_wsfe(&io___211);
    do_fio(&c__1, (char *)&iparm[9], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }

/* ... PERMUTE MATRIX AND RHS */

L150:
    if (iparm[9] >= 0) {
    goto L190;
    }
    if (itcom1_1.level >= 2) {
    io___212.ciunit = itcom1_1.nout;
    s_wsfe(&io___212);
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    permat_(&n, &ia[1], &ja[1], &a[1], &iwksp[1], &iwksp[jb3], &itcom1_1.isym,
         &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L180;
    }
    if (itcom1_1.level >= 0) {
    io___213.ciunit = itcom1_1.nout;
    s_wsfe(&io___213);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L430;
L180:
    pervec_(&n, &rhs[1], &iwksp[1]);
    pervec_(&n, &u[1], &iwksp[1]);

/* ... FINISH WKSP BASE ADDRESSES */

L190:
    ib3 = ib2 + nb;
    ib4 = ib3 + nb;
    ib5 = ib4 + nb;
    nr = n - nb;
    nrp1 = nr + 1;
    iparm[8] = n + nb * 3 + (itcom1_1.itmax << 1);
    if (itcom1_1.isym != 0) {
    iparm[8] += itcom1_1.itmax << 1;
    }
    if (*nw >= iparm[8]) {
    goto L210;
    }
    ier = 62;
    if (itcom1_1.level >= 0) {
    io___219.ciunit = itcom1_1.nout;
    s_wsfe(&io___219);
    do_fio(&c__1, (char *)&(*nw), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iparm[8], (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L430;

/* ... SCALE LINEAR SYSTEM, U, AND RHS BY THE SQUARE ROOT OF THE */
/* ... DIAGONAL ELEMENTS. */

L210:
    vfill_(&iparm[8], &wksp[1], &c_b21);
    scal_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1], &
        itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L230;
    }
    if (itcom1_1.level >= 0) {
    io___220.ciunit = itcom1_1.nout;
    s_wsfe(&io___220);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L430;
L230:
    if (itcom1_1.level <= 2) {
    goto L260;
    }
    io___221.ciunit = itcom1_1.nout;
    s_wsfe(&io___221);
    e_wsfe();
    if (itcom2_1.adapt) {
    io___222.ciunit = itcom1_1.nout;
    s_wsfe(&io___222);
    e_wsfe();
    }
L260:
    if (iparm[11] != 0) {
    goto L270;
    }
    timi1 = timer_(&dummy);

/* ... INITIALIZE FORWARD PSEUDO-RESIDUAL */

L270:
    if (n > 1) {
    goto L280;
    }
    u[1] = rhs[1];
    goto L330;
L280:
    dcopy_(&nr, &rhs[1], &c__1, &wksp[ib1], &c__1);
    prsred_(&nb, &nr, &ia[1], &ja[1], &a[1], &u[nrp1], &wksp[ib1]);
    dcopy_(&nb, &rhs[nrp1], &c__1, &wksp[ib2], &c__1);
    prsblk_(&nb, &nr, &ia[1], &ja[1], &a[1], &wksp[ib1], &wksp[ib2]);
    vevmw_(&nb, &wksp[ib2], &u[nrp1]);

/* ... ITERATION SEQUENCE */

    itmax1 = itcom1_1.itmax + 1;
    i__1 = itmax1;
    for (loop = 1; loop <= i__1; ++loop) {
    itcom1_1.in = loop - 1;
    if (itcom1_1.in % 2 == 1) {
        goto L290;
    }

/* ... CODE FOR THE EVEN ITERATIONS. */

/*     U           = U(IN)       WKSP(IB2) = D(IN) */
/*     WKSP(IB1)   = U(IN-1)     WKSP(IB3) = D(IN-1) */

    itrscg_(&n, &nb, &ia[1], &ja[1], &a[1], &u[1], &wksp[ib1], &wksp[ib2],
         &wksp[ib3], &wksp[ib4], &wksp[ib5]);

    if (itcom2_1.halt) {
        goto L330;
    }
    goto L300;

/* ... CODE FOR THE ODD ITERATIONS. */

/*     U           = U(IN-1)     WKSP(IB2) = D(IN-1) */
/*     WKSP(IB1)   = U(IN)       WKSP(IB3) = D(IN) */

L290:
    itrscg_(&n, &nb, &ia[1], &ja[1], &a[1], &wksp[ib1], &u[1], &wksp[ib3],
         &wksp[ib2], &wksp[ib4], &wksp[ib5]);

    if (itcom2_1.halt) {
        goto L330;
    }
L300:
    ;
    }

/* ... ITMAX HAS BEEN REACHED */

    if (iparm[11] != 0) {
    goto L310;
    }
    timi2 = timer_(&dummy);
    time1 = (doublereal) (timi2 - timi1);
L310:
    if (itcom1_1.level >= 1) {
    io___227.ciunit = itcom1_1.nout;
    s_wsfe(&io___227);
    do_fio(&c__1, (char *)&itcom1_1.itmax, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    ier = 63;
    if (iparm[3] == 0) {
    rparm[1] = itcom3_1.stptst;
    }
    goto L360;

/* ... METHOD HAS CONVERGED */

L330:
    if (iparm[11] != 0) {
    goto L340;
    }
    timi2 = timer_(&dummy);
    time1 = (doublereal) (timi2 - timi1);
L340:
    if (itcom1_1.level >= 1) {
    io___228.ciunit = itcom1_1.nout;
    s_wsfe(&io___228);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    e_wsfe();
    }

/* ... PUT SOLUTION INTO U IF NOT ALREADY THERE. */

L360:
    if (n == 1) {
    goto L370;
    }
    if (itcom1_1.in % 2 == 1) {
    dcopy_(&n, &wksp[ib1], &c__1, &u[1], &c__1);
    }
    dcopy_(&nr, &rhs[1], &c__1, &u[1], &c__1);
    prsred_(&nb, &nr, &ia[1], &ja[1], &a[1], &u[nrp1], &u[1]);

/* ... UNSCALE THE MATRIX, SOLUTION, AND RHS VECTORS. */

L370:
    unscal_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1]);

/* ... UN-PERMUTE MATRIX,RHS, AND SOLUTION */

    if (iparm[9] >= 0) {
    goto L400;
    }
    permat_(&n, &ia[1], &ja[1], &a[1], &iwksp[ib2], &iwksp[jb3], &
        itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ierper);
    if (ierper == 0) {
    goto L390;
    }
    if (itcom1_1.level >= 0) {
    io___230.ciunit = itcom1_1.nout;
    s_wsfe(&io___230);
    do_fio(&c__1, (char *)&ierper, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    if (ier == 0) {
    ier = ierper;
    }
    goto L430;
L390:
    pervec_(&n, &rhs[1], &iwksp[ib2]);
    pervec_(&n, &u[1], &iwksp[ib2]);

/* ... OPTIONAL ERROR ANALYSIS */

L400:
    idgts = iparm[12];
    if (idgts < 0) {
    goto L410;
    }
    if (iparm[2] <= 0) {
    idgts = 0;
    }
    perror_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1], &digit1, &
        digit2, &idgts);

/* ... SET RETURN PARAMETERS IN IPARM AND RPARM */

L410:
    if (iparm[11] != 0) {
    goto L420;
    }
    timj2 = timer_(&dummy);
    time2 = (doublereal) (timj2 - timj1);
L420:
    iparm[8] -= itcom1_1.itmax - itcom1_1.in << 1;
    if (itcom1_1.isym != 0) {
    iparm[8] -= itcom1_1.itmax - itcom1_1.in << 1;
    }
    if (iparm[3] != 0) {
    goto L430;
    }
    iparm[1] = itcom1_1.in;
    iparm[9] = nb;
    rparm[2] = itcom3_1.cme;
    rparm[3] = itcom3_1.sme;
    rparm[9] = time1;
    rparm[10] = time2;
    rparm[11] = digit1;
    rparm[12] = digit2;

L430:
    *ierr = ier;
    if (itcom1_1.level >= 3) {
    echall_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iparm[1], &rparm[1], &
        c__2);
    }

    return 0;
} /* rscg_ */

/* Subroutine */ int rssi_(integer *nn, integer *ia, integer *ja, doublereal *
    a, doublereal *rhs, doublereal *u, integer *iwksp, integer *nw, 
    doublereal *wksp, integer *iparm, doublereal *rparm, integer *ierr)
{
    /* Format strings */
    static char fmt_10[] = "(\0020\002///1x,\002BEGINNING OF ITPACK SOLUTION\
 MODULE  RSSI\002)";
    static char fmt_40[] = "(\0020\002,\002*** W A R N I N G ************\
\002/\0020\002,\002    IN ITPACK ROUTINE RSSI\002/\002 \002,\002    RPARM(1)\
 =\002,d10.3,\002 (ZETA)\002/\002 \002,\002    A VALUE THIS SMALL MAY HINDER\
 CONVERGENCE \002/\002 \002,\002    SINCE MACHINE PRECISION DRELPR =\002,d10\
.3/\002 \002,\002    ZETA RESET TO \002,d10.3)";
    static char fmt_60[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE RSSI \002/\002 \002,\
\002    INVALID MATRIX DIMENSION, N =\002,i8)";
    static char fmt_80[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE RSSI \002/\002 \002,\
\002    ERROR DETECTED IN SUBROUTINE  SBELM \002/\002 \002,\002    WHICH REM\
OVES ROWS AND COLUMNS OF SYSTEM \002/\002 \002,\002    WHEN DIAGONAL ENTRY T\
OO LARGE  \002/\002 \002,\002    IER = \002,i5,5x,\002 RPARM(8) = \002,d10.3,\
\002 (TOL)\002)";
    static char fmt_100[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE RSSI \002/\002 \
\002,\002    ERROR DETECTED IN SUBROUTINE  PRBNDX\002/\002 \002,\002    WHIC\
H COMPUTES THE RED-BLACK INDEXING\002/\002 \002,\002    IER = \002,i5,\002 I\
PARM(9) = \002,i5,\002 (NB)\002)";
    static char fmt_120[] = "(/10x,\002ERROR DETECTED IN RED-BLACK SUBSYSTEM\
 INDEX\002/10x,\002IER =\002,i5,\002 IPARM(9) =\002,i5,\002 (NB)\002)";
    static char fmt_140[] = "(/10x,\002 IPARM(9) = \002,i5,\002 IMPLIES MATR\
IX IS DIAGONAL\002/10x,\002 NB RESET TO \002,i5)";
    static char fmt_160[] = "(/10x,\002ORDER OF BLACK SUBSYSTEM = \002,i5\
,\002 (NB)\002)";
    static char fmt_170[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE RSSI \002/\002 \
\002,\002    ERROR DETECTED IN SUBROUTINE  PERMAT\002/\002 \002,\002    WHIC\
H DOES THE RED-BLACK PERMUTATION\002/\002 \002,\002    IER = \002,i5)";
    static char fmt_200[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE RSSI \002/\002 \
\002,\002    NOT ENOUGH WORKSPACE AT \002,i10/\002 \002,\002    SET IPARM(8)\
 =\002,i10,\002 (NW)\002)";
    static char fmt_220[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE RSSI \002/\002 \
\002,\002    ERROR DETECTED IN SUBROUTINE  SCAL  \002/\002 \002,\002    WHIC\
H SCALES THE SYSTEM   \002/\002 \002,\002    IER = \002,i5)";
    static char fmt_240[] = "(///1x,\002IN THE FOLLOWING, RHO AND GAMMA AR\
E\002,\002 ACCELERATION PARAMETERS\002)";
    static char fmt_310[] = "(\0020\002,\002*** W A R N I N G ***********\
*\002/\0020\002,\002    IN ITPACK ROUTINE RSSI\002/\002 \002,\002    FAILURE\
 TO CONVERGE IN\002,i5,\002 ITERATIONS\002)";
    static char fmt_340[] = "(/1x,\002RSSI  HAS CONVERGED IN \002,i5,\002 IT\
ERATIONS\002)";
    static char fmt_370[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    CALLED FROM ITPACK ROUTINE RSSI \002/\002 \
\002,\002    ERROR DETECTED IN SUBROUTINE  PERMAT\002/\002 \002,\002    WHIC\
H UNDOES THE RED-BLACK PERMUTATION   \002/\002 \002,\002    IER = \002,i5)";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(), do_fio(integer *, char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int scal_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *, doublereal *, integer *,
         integer *, integer *);
    static doublereal temp;
    static integer loop;
    static doublereal time1, time2;
    static real timi1, timj1, timi2, timj2;
    static integer n;
    extern /* Subroutine */ int sbelm_(integer *, integer *, integer *, 
        doublereal *, doublereal *, integer *, doublereal *, doublereal *,
         integer *, integer *, integer *, integer *);
    static integer idgts;
    extern /* Subroutine */ int vfill_(integer *, doublereal *, doublereal *),
         dcopy_(integer *, doublereal *, integer *, doublereal *, integer 
        *);
    extern doublereal timer_(real *);
    static real dummy;
    static integer n3;
    static doublereal digit1, digit2;
    static integer itmax1, nb;
    extern /* Subroutine */ int echall_(integer *, integer *, integer *, 
        doublereal *, doublereal *, integer *, doublereal *, integer *);
    static integer nr;
    extern /* Subroutine */ int pervec_(integer *, doublereal *, integer *), 
        ivfill_(integer *, integer *, integer *);
    static integer ierper;
    extern /* Subroutine */ int echout_(integer *, doublereal *, integer *), 
        permat_(integer *, integer *, integer *, doublereal *, integer *, 
        integer *, integer *, integer *, integer *, integer *), unscal_(
        integer *, integer *, integer *, doublereal *, doublereal *, 
        doublereal *, doublereal *);
    static integer ib1, ib2;
    extern /* Subroutine */ int prbndx_(integer *, integer *, integer *, 
        integer *, integer *, integer *, integer *, integer *, integer *);
    static integer jb3;
    extern /* Subroutine */ int prsred_(integer *, integer *, integer *, 
        integer *, doublereal *, doublereal *, doublereal *), perror_(
        integer *, integer *, integer *, doublereal *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *, integer *)
        , itrssi_(integer *, integer *, integer *, integer *, doublereal *
        , doublereal *, doublereal *, doublereal *, doublereal *);
    static integer ier;
    static doublereal tol;
    static integer nrp1;

    /* Fortran I/O blocks */
    static cilist io___233 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___239 = { 0, 0, 0, fmt_40, 0 };
    static cilist io___244 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___246 = { 0, 0, 0, fmt_80, 0 };
    static cilist io___252 = { 0, 0, 0, fmt_100, 0 };
    static cilist io___253 = { 0, 0, 0, fmt_120, 0 };
    static cilist io___254 = { 0, 0, 0, fmt_140, 0 };
    static cilist io___255 = { 0, 0, 0, fmt_160, 0 };
    static cilist io___256 = { 0, 0, 0, fmt_170, 0 };
    static cilist io___259 = { 0, 0, 0, fmt_200, 0 };
    static cilist io___260 = { 0, 0, 0, fmt_220, 0 };
    static cilist io___261 = { 0, 0, 0, fmt_240, 0 };
    static cilist io___266 = { 0, 0, 0, fmt_310, 0 };
    static cilist io___267 = { 0, 0, 0, fmt_340, 0 };
    static cilist io___269 = { 0, 0, 0, fmt_370, 0 };



/*     ITPACK 2C MAIN SUBROUTINE  RSSI  (REDUCED SYSTEM SEMI-ITERATIVE) */
/*     EACH OF THE MAIN SUBROUTINES: */
/*           JCG, JSI, SOR, SSORCG, SSORSI, RSCG, RSSI */
/*     CAN BE USED INDEPENDENTLY OF THE OTHERS */

/* ... FUNCTION: */

/*          THIS SUBROUTINE, RSSI, DRIVES THE  REDUCED SYSTEM SI */
/*          ALGORITHM. */

/* ... PARAMETER LIST: */

/*          N     INPUT INTEGER.  DIMENSION OF THE MATRIX. (= NN) */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF */
/*                 THE SPARSE MATRIX REPRESENTATION. */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE */
/*                 MATRIX REPRESENTATION. */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE */
/*                 OF THE MATRIX PROBLEM. */
/*          U      INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U CONTAINS THE */
/*                 INITIAL GUESS TO THE SOLUTION. ON OUTPUT, IT CONTAINS */
/*                 THE LATEST ESTIMATE TO THE SOLUTION. */
/*          IWKSP  INTEGER VECTOR WORKSPACE OF LENGTH 3*N */
/*          NW     INPUT INTEGER.  LENGTH OF AVAILABLE WKSP.  ON OUTPUT, */
/*                 IPARM(8) IS AMOUNT USED. */
/*          WKSP   D.P. VECTOR USED FOR WORKING SPACE.  RSSI */
/*                 NEEDS THIS TO BE IN LENGTH AT LEAST  N + NB */
/*                 HERE NB IS THE ORDER OF THE BLACK SUBSYSTEM */
/*          IPARM  INTEGER VECTOR OF LENGTH 12.  ALLOWS USER TO SPECIFY */
/*                 SOME INTEGER PARAMETERS WHICH AFFECT THE METHOD.  IF */
/*          RPARM  D.P. VECTOR OF LENGTH 12. ALLOWS USER TO SPECIFY SOME */
/*                 D.P. PARAMETERS WHICH AFFECT THE METHOD. */
/*          IER     OUTPUT INTEGER.  ERROR FLAG. (= IERR) */

/* ... RSSI SUBPROGRAM REFERENCES: */

/*          FROM ITPACK    BISRCH, CHEBY, CHGSI, DFAULT, ECHALL, */
/*                         ECHOUT, ITERM, TIMER, ITRSSI, IVFILL, */
/*                         PARSI, PERMAT, PERROR, PERVEC, PMULT, */
/*                         PRBNDX, PRSBLK, PRSRED, PSTOP, QSORT, */
/*                         DAXPY, SBELM, SCAL, DCOPY, DDOT, SUM3, */
/*                         TSTCHG, UNSCAL, VEVMW, VFILL, VOUT, */
/*                         WEVMW */
/*          SYSTEM         DABS, DLOG10, DBLE(AMAX0), DMAX1, DBLE(FLOAT), */
/*                         DSQRT */

/*     VERSION:  ITPACK 2C (MARCH 1982) */

/*     CODE WRITTEN BY:  DAVID KINCAID, ROGER GRIMES, JOHN RESPESS */
/*                       CENTER FOR NUMERICAL ANALYSIS */
/*                       UNIVERSITY OF TEXAS */
/*                       AUSTIN, TX  78712 */
/*                       (512) 471-1242 */

/*     FOR ADDITIONAL DETAILS ON THE */
/*          (A) SUBROUTINE SEE TOMS ARTICLE 1982 */
/*          (B) ALGORITHM  SEE CNA REPORT 150 */

/*     BASED ON THEORY BY:  DAVID YOUNG, DAVID KINCAID, LOU HAGEMAN */

/*     REFERENCE THE BOOK:  APPLIED ITERATIVE METHODS */
/*                          L. HAGEMAN, D. YOUNG */
/*                          ACADEMIC PRESS, 1981 */

/*     ************************************************** */
/*     *               IMPORTANT NOTE                   * */
/*     *                                                * */
/*     *      WHEN INSTALLING ITPACK ROUTINES ON A      * */
/*     *  DIFFERENT COMPUTER, RESET SOME OF THE VALUES  * */
/*     *  IN  SUBROUTNE DFAULT.   MOST IMPORTANT ARE    * */
/*     *                                                * */
/*     *   DRELPR      MACHINE RELATIVE PRECISION       * */
/*     *   RPARM(1)    STOPPING CRITERION               * */
/*     *                                                * */
/*     *   ALSO CHANGE SYSTEM-DEPENDENT ROUTINE         * */
/*     *   SECOND USED IN TIMER                         * */
/*     *                                                * */
/*     ************************************************** */

/*     SPECIFICATIONS FOR ARGUMENTS */


/*     SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/* ... VARIABLES IN COMMON BLOCK - ITCOM1 */

/*     IN     - ITERATION NUMBER */
/*     IS     - ITERATION NUMBER WHEN PARAMETERS LAST CHANGED */
/*     ISYM   - SYMMETRIC/NONSYMMETRIC STORAGE FORMAT SWITCH */
/*     ITMAX  - MAXIMUM NUMBER OF ITERATIONS ALLOWED */
/*     LEVEL  - LEVEL OF OUTPUT CONTROL SWITCH */
/*     NOUT   - OUTPUT UNIT NUMBER */

/* ... VARIABLES IN COMMON BLOCK - ITCOM2 */

/*     ADAPT  - FULLY ADAPTIVE PROCEDURE SWITCH */
/*     BETADT - SWITCH FOR ADAPTIVE DETERMINATION OF BETA */
/*     CASEII - ADAPTIVE PROCEDURE CASE SWITCH */
/*     HALT   - STOPPING TEST SWITCH */
/*     PARTAD - PARTIALLY ADAPTIVE PROCEDURE SWITCH */

/* ... VARIABLES IN COMMON BLOCK - ITCOM3 */

/*     BDELNM - TWO NORM OF B TIMES DELTA-SUPER-N */
/*     BETAB  - ESTIMATE FOR THE SPECTRAL RADIUS OF LU MATRIX */
/*     CME    - ESTIMATE OF LARGEST EIGENVALUE */
/*     DELNNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION N */
/*     DELSNM - INNER PRODUCT OF PSEUDO-RESIDUAL AT ITERATION S */
/*     FF     - ADAPTIVE PROCEDURE DAMPING FACTOR */
/*     GAMMA  - ACCELERATION PARAMETER */
/*     OMEGA  - OVERRELAXATION PARAMETER FOR SOR AND SSOR */
/*     QA     - PSEUDO-RESIDUAL RATIO */
/*     QT     - VIRTUAL SPECTRAL RADIUS */
/*     RHO    - ACCELERATION PARAMETER */
/*     RRR    - ADAPTIVE PARAMETER */
/*     SIGE   - PARAMETER SIGMA-SUB-E */
/*     SME    - ESTIMATE OF SMALLEST EIGENVALUE */
/*     SPECR  - SPECTRAL RADIUS ESTIMATE FOR SSOR */
/*     DRELPR - MACHINE RELATIVE PRECISION */
/*     STPTST - STOPPING PARAMETER */
/*     UDNM   - TWO NORM OF U */
/*     ZETA   - STOPPING CRITERION */

/* ... INITIALIZE COMMON BLOCKS */

    /* Parameter adjustments */
    --u;
    --rhs;
    --ia;
    --ja;
    --a;
    --iwksp;
    --wksp;
    --iparm;
    --rparm;

    /* Function Body */
    itcom1_1.level = iparm[2];
    itcom1_1.nout = iparm[4];
    if (itcom1_1.level >= 1) {
    io___233.ciunit = itcom1_1.nout;
    s_wsfe(&io___233);
    e_wsfe();
    }
    ier = 0;
    if (iparm[1] <= 0) {
    return 0;
    }
    n = *nn;
    if (iparm[11] == 0) {
    timj1 = timer_(&dummy);
    }
    if (itcom1_1.level >= 3) {
    goto L20;
    }
    echout_(&iparm[1], &rparm[1], &c__7);
    goto L30;
L20:
    echall_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iparm[1], &rparm[1], &c__1);
L30:
    temp = itcom3_1.drelpr * 500.;
    if (itcom3_1.zeta >= temp) {
    goto L50;
    }
    if (itcom1_1.level >= 1) {
    io___239.ciunit = itcom1_1.nout;
    s_wsfe(&io___239);
    do_fio(&c__1, (char *)&itcom3_1.zeta, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.drelpr, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&temp, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    itcom3_1.zeta = temp;
L50:
    time1 = rparm[9];
    time2 = rparm[10];
    digit1 = rparm[11];
    digit2 = rparm[12];

/* ... VERIFY N */

    if (n > 0) {
    goto L70;
    }
    ier = 71;
    if (itcom1_1.level >= 0) {
    io___244.ciunit = itcom1_1.nout;
    s_wsfe(&io___244);
    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L420;
L70:

/* ... REMOVE ROWS AND COLUMNS IF REQUESTED */

    if (iparm[10] == 0) {
    goto L90;
    }
    tol = rparm[8];
    ivfill_(&n, &iwksp[1], &c__0);
    vfill_(&n, &wksp[1], &c_b21);
    sbelm_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iwksp[1], &wksp[1], &tol, &
        itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L90;
    }
    if (itcom1_1.level >= 0) {
    io___246.ciunit = itcom1_1.nout;
    s_wsfe(&io___246);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&tol, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }

/* ... INITIALIZE WKSP BASE ADDRESSES. */

L90:
    ib1 = 1;
    ib2 = ib1 + n;
    jb3 = ib2 + n;

/* ... PERMUTE TO  RED-BLACK SYSTEM IF POSSIBLE */

    nb = iparm[9];
    if (nb >= 0) {
    goto L110;
    }
    n3 = n * 3;
    ivfill_(&n3, &iwksp[1], &c__0);
    prbndx_(&n, &nb, &ia[1], &ja[1], &iwksp[1], &iwksp[ib2], &itcom1_1.level, 
        &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L110;
    }
    if (itcom1_1.level >= 0) {
    io___252.ciunit = itcom1_1.nout;
    s_wsfe(&io___252);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L420;
L110:
    if (nb >= 0 && nb <= n) {
    goto L130;
    }
    ier = 74;
    if (itcom1_1.level >= 1) {
    io___253.ciunit = itcom1_1.nout;
    s_wsfe(&io___253);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L420;
L130:
    if (nb != 0 && nb != n) {
    goto L150;
    }
    nb = n / 2;
    if (itcom1_1.level >= 2 && iparm[9] >= 0) {
    io___254.ciunit = itcom1_1.nout;
    s_wsfe(&io___254);
    do_fio(&c__1, (char *)&iparm[9], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }

/* ... PERMUTE MATRIX AND RHS */

L150:
    if (iparm[9] >= 0) {
    goto L190;
    }
    if (itcom1_1.level >= 2) {
    io___255.ciunit = itcom1_1.nout;
    s_wsfe(&io___255);
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    permat_(&n, &ia[1], &ja[1], &a[1], &iwksp[1], &iwksp[jb3], &itcom1_1.isym,
         &itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L180;
    }
    if (itcom1_1.level >= 0) {
    io___256.ciunit = itcom1_1.nout;
    s_wsfe(&io___256);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L420;
L180:
    pervec_(&n, &rhs[1], &iwksp[1]);
    pervec_(&n, &u[1], &iwksp[1]);

/* ... INITIALIZE WKSP BASE ADDRESSES */

L190:
    nr = n - nb;

    nrp1 = nr + 1;
    iparm[8] = n + nb;
    if (*nw >= iparm[8]) {
    goto L210;
    }
    ier = 72;
    if (itcom1_1.level >= 0) {
    io___259.ciunit = itcom1_1.nout;
    s_wsfe(&io___259);
    do_fio(&c__1, (char *)&(*nw), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iparm[8], (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L420;

/* ... SCALE LINEAR SYSTEM, U, AND RHS BY THE SQUARE ROOT OF THE */
/* ... DIAGONAL ELEMENTS. */

L210:
    vfill_(&iparm[8], &wksp[1], &c_b21);
    scal_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1], &
        itcom1_1.level, &itcom1_1.nout, &ier);
    if (ier == 0) {
    goto L230;
    }
    if (itcom1_1.level >= 0) {
    io___260.ciunit = itcom1_1.nout;
    s_wsfe(&io___260);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L420;
L230:
    if (itcom1_1.level <= 2) {
    goto L250;
    }
    io___261.ciunit = itcom1_1.nout;
    s_wsfe(&io___261);
    e_wsfe();
L250:
    if (iparm[11] != 0) {
    goto L260;
    }
    timi1 = timer_(&dummy);

/* ... ITERATION SEQUENCE */

L260:
    if (n > 1) {
    goto L270;
    }
    u[1] = rhs[1];
    goto L320;
L270:
    itmax1 = itcom1_1.itmax + 1;
    i__1 = itmax1;
    for (loop = 1; loop <= i__1; ++loop) {
    itcom1_1.in = loop - 1;
    if (itcom1_1.in % 2 == 1) {
        goto L280;
    }

/* ... CODE FOR THE EVEN ITERATIONS. */

/*     U           = U(IN) */
/*     WKSP(IB1)   = U(IN-1) */

    itrssi_(&n, &nb, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[ib1], &
        wksp[ib2]);

    if (itcom2_1.halt) {
        goto L320;
    }
    goto L290;

/* ... CODE FOR THE ODD ITERATIONS. */

/*     U           = U(IN-1) */
/*     WKSP(IB1)   = U(IN) */

L280:
    itrssi_(&n, &nb, &ia[1], &ja[1], &a[1], &rhs[1], &wksp[ib1], &u[1], &
        wksp[ib2]);

    if (itcom2_1.halt) {
        goto L320;
    }
L290:
    ;
    }

/* ... ITMAX HAS BEEN REACHED */

    if (iparm[11] != 0) {
    goto L300;
    }
    timi2 = timer_(&dummy);
    time1 = (doublereal) (timi2 - timi1);
L300:
    if (itcom1_1.level >= 1) {
    io___266.ciunit = itcom1_1.nout;
    s_wsfe(&io___266);
    do_fio(&c__1, (char *)&itcom1_1.itmax, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    ier = 73;
    if (iparm[3] == 0) {
    rparm[1] = itcom3_1.stptst;
    }
    goto L350;

/* ... METHOD HAS CONVERGED */

L320:
    if (iparm[11] != 0) {
    goto L330;
    }
    timi2 = timer_(&dummy);
    time1 = (doublereal) (timi2 - timi1);
L330:
    if (itcom1_1.level >= 1) {
    io___267.ciunit = itcom1_1.nout;
    s_wsfe(&io___267);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    e_wsfe();
    }

/* ... PUT SOLUTION INTO U IF NOT ALREADY THERE. */

L350:
    if (n == 1) {
    goto L360;
    }
    if (itcom1_1.in % 2 == 1) {
    dcopy_(&n, &wksp[ib1], &c__1, &u[1], &c__1);
    }
    dcopy_(&nr, &rhs[1], &c__1, &u[1], &c__1);
    prsred_(&nb, &nr, &ia[1], &ja[1], &a[1], &u[nrp1], &u[1]);

/* ... UNSCALE THE MATRIX, SOLUTION, AND RHS VECTORS. */

L360:
    unscal_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1]);

/* ... UN-PERMUTE MATRIX,RHS, AND SOLUTION */

    if (iparm[9] >= 0) {
    goto L390;
    }
    permat_(&n, &ia[1], &ja[1], &a[1], &iwksp[ib2], &iwksp[jb3], &
        itcom1_1.isym, &itcom1_1.level, &itcom1_1.nout, &ierper);
    if (ierper == 0) {
    goto L380;
    }
    if (itcom1_1.level >= 0) {
    io___269.ciunit = itcom1_1.nout;
    s_wsfe(&io___269);
    do_fio(&c__1, (char *)&ierper, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    if (ier == 0) {
    ier = ierper;
    }
    goto L420;
L380:
    pervec_(&n, &rhs[1], &iwksp[ib2]);
    pervec_(&n, &u[1], &iwksp[ib2]);

/* ... OPTIONAL ERROR ANALYSIS */

L390:
    idgts = iparm[12];
    if (idgts < 0) {
    goto L400;
    }
    if (iparm[2] <= 0) {
    idgts = 0;
    }
    perror_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &u[1], &wksp[1], &digit1, &
        digit2, &idgts);

/* ... SET RETURN PARAMETERS IN IPARM AND RPARM */

L400:
    if (iparm[11] != 0) {
    goto L410;
    }
    timj2 = timer_(&dummy);
    time2 = (doublereal) (timj2 - timj1);
L410:
    if (iparm[3] != 0) {
    goto L420;
    }
    iparm[1] = itcom1_1.in;
    iparm[9] = nb;
    rparm[2] = itcom3_1.cme;
    rparm[3] = itcom3_1.sme;
    rparm[9] = time1;
    rparm[10] = time2;
    rparm[11] = digit1;
    rparm[12] = digit2;

L420:
    *ierr = ier;
    if (itcom1_1.level >= 3) {
    echall_(&n, &ia[1], &ja[1], &a[1], &rhs[1], &iparm[1], &rparm[1], &
        c__2);
    }

    return 0;
} /* rssi_ */

/* Subroutine */ int itjcg_(integer *nn, integer *ia, integer *ja, doublereal 
    *a, doublereal *u, doublereal *u1, doublereal *d__, doublereal *d1, 
    doublereal *dtwd, doublereal *tri)
{
    extern /* Subroutine */ int pjac_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *);
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
        integer *);
    static doublereal dnrm;
    static integer n;
    extern /* Subroutine */ int vfill_(integer *, doublereal *, doublereal *),
         iterm_(integer *, doublereal *, doublereal *, doublereal *, 
        integer *);
    static doublereal c1, c2, c3, c4, dtnrm;
    static logical q1;
    extern /* Subroutine */ int pstop_(integer *, doublereal *, doublereal *, 
        doublereal *, integer *, logical *), chgcon_(doublereal *, 
        doublereal *, doublereal *, integer *);
    static doublereal gamold;
    extern /* Subroutine */ int parcon_(doublereal *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *, 
        doublereal *, integer *);
    static doublereal rhoold, rhotmp, con;
    extern /* Subroutine */ int sum3_(integer *, doublereal *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *);


/* ... FUNCTION: */

/*          THIS SUBROUTINE, ITJCG, PERFORMS ONE ITERATION OF THE */
/*          JACOBI CONJUGATE GRADIENT ALGORITHM.  IT IS CALLED BY JCG. */

/* ... PARAMETER LIST: */

/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX. (= NN) */
/*          IA,JA  INPUT INTEGER VECTORS.  CONTAINS INFORMATION DEFINING */
/*                 THE SPARSE MATRIX REPRESENTATION. */
/*          A      INPUT D.P. VECTOR. CONTAINS THE NONZERO VALUES OF THE */
/*                 LINEAR SYSTEM. */
/*          U      INPUT D.P. VECTOR.  CONTAINS THE VALUE OF THE */
/*                 SOLUTION VECTOR AT THE END OF IN ITERATIONS. */
/*          U1     INPUT/OUTPUT D.P. VECTOR.  ON INPUT, IT CONTAINS */
/*                 THE VALUE OF THE SOLUTION AT THE END OF THE IN-1 */
/*                 ITERATION.  ON OUTPUT, IT WILL CONTAIN THE NEWEST */
/*                 ESTIMATE FOR THE SOLUTION VECTOR. */
/*          D      INPUT D.P. VECTOR.  CONTAINS THE PSEUDO-RESIDUAL */
/*                 VECTOR AFTER IN ITERATIONS. */
/*          D1     INPUT/OUTPUT D.P. VECTOR.  ON INPUT, D1 CONTAINS */
/*                 THE PSEUDO-RESIDUAL VECTOR AFTER IN-1 ITERATIONS.  ON */
/*                 OUTPUT, IT WILL CONTAIN THE NEWEST PSEUDO-RESIDUAL */
/*                 VECTOR. */
/*          DTWD   D.P. ARRAY.  USED IN THE COMPUTATIONS OF THE */
/*                 ACCELERATION PARAMETER GAMMA AND THE NEW PSEUDO- */
/*                 RESIDUAL. */
/*          TRI    D.P. ARRAY.  STORES THE TRIDIAGONAL MATRIX ASSOCIATED */
/*                 WITH THE EIGENVALUES OF THE CONJUGATE GRADIENT */
/*                 POLYNOMIAL. */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* ... SPECIFICATIONS FOR FUNCTION SUBPROGRAMS */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN SUBROUTINE JCG */

/* ... COMPUTE NEW ESTIMATE FOR CME IF ADAPT = .TRUE. */

    /* Parameter adjustments */
    --dtwd;
    --d1;
    --d__;
    --u1;
    --u;
    --ia;
    --ja;
    --a;
    tri -= 3;

    /* Function Body */
    if (itcom2_1.adapt) {
    chgcon_(&tri[3], &gamold, &rhoold, &c__1);
    }

/* ... TEST FOR STOPPING */

    n = *nn;
    itcom3_1.delnnm = ddot_(&n, &d__[1], &c__1, &d__[1], &c__1);
    dnrm = itcom3_1.delnnm;
    con = itcom3_1.cme;
    pstop_(&n, &u[1], &dnrm, &con, &c__1, &q1);
    if (itcom2_1.halt) {
    goto L30;
    }

/* ... COMPUTE RHO AND GAMMA - ACCELERATION PARAMETERS */

    vfill_(&n, &dtwd[1], &c_b21);
    pjac_(&n, &ia[1], &ja[1], &a[1], &d__[1], &dtwd[1]);
    dtnrm = ddot_(&n, &d__[1], &c__1, &dtwd[1], &c__1);
    if (itcom1_1.isym == 0) {
    goto L10;
    }
    rhotmp = ddot_(&n, &dtwd[1], &c__1, &d1[1], &c__1);
    parcon_(&dtnrm, &c1, &c2, &c3, &c4, &gamold, &rhotmp, &c__1);
    rhoold = rhotmp;
    goto L20;
L10:
    parcon_(&dtnrm, &c1, &c2, &c3, &c4, &gamold, &rhoold, &c__1);

/* ... COMPUTE U(IN+1) AND D(IN+1) */

L20:
    sum3_(&n, &c1, &d__[1], &c2, &u[1], &c3, &u1[1]);
    sum3_(&n, &c1, &dtwd[1], &c4, &d__[1], &c3, &d1[1]);

/* ... OUTPUT INTERMEDIATE INFORMATION */

L30:
    iterm_(&n, &a[1], &u[1], &dtwd[1], &c__1);

    return 0;
} /* itjcg_ */

/* Subroutine */ int itjsi_(integer *nn, integer *ia, integer *ja, doublereal 
    *a, doublereal *rhs, doublereal *u, doublereal *u1, doublereal *d__, 
    integer *icnt)
{
    extern /* Subroutine */ int pjac_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *);
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
        integer *);
    static doublereal dnrm;
    static integer n;
    extern /* Subroutine */ int chgsi_(doublereal *, integer *), dcopy_(
        integer *, doublereal *, integer *, doublereal *, integer *), 
        parsi_(doublereal *, doublereal *, doublereal *, integer *), 
        iterm_(integer *, doublereal *, doublereal *, doublereal *, 
        integer *);
    static doublereal c1, c2, c3, dtnrm;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *, 
        integer *, doublereal *, integer *);
    extern doublereal pvtbv_(integer *, integer *, integer *, doublereal *, 
        doublereal *);
    static logical q1;
    extern /* Subroutine */ int vevmw_(integer *, doublereal *, doublereal *),
         pstop_(integer *, doublereal *, doublereal *, doublereal *, 
        integer *, logical *);
    extern logical chgsme_(doublereal *, integer *);
    static doublereal oldnrm;
    extern logical tstchg_(integer *);
    static doublereal con;
    extern /* Subroutine */ int sum3_(integer *, doublereal *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *);


/* ... FUNCTION: */

/*          THIS SUBROUTINE, ITJSI, PERFORMS ONE ITERATION OF THE */
/*          JACOBI SEMI-ITERATIVE ALGORITHM.  IT IS CALLED BY JSI. */

/* ... PARAMETER LIST: */

/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX. (= NN) */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF */
/*                 THE SPARSE MATRIX REPRESENTATION. */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE */
/*                 MATRIX REPRESENTATION. */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE */
/*                 OF THE MATRIX PROBLEM. */
/*          U      INPUT D.P. VECTOR.  CONTAINS THE ESTIMATE FOR THE */
/*                 SOLUTION VECTOR AFTER IN ITERATIONS. */
/*          U1     INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U1 CONTAINS THE */
/*                 SOLUTION VECTOR AFTER IN-1 ITERATIONS.  ON OUTPUT, */
/*                 IT WILL CONTAIN THE NEWEST ESTIMATE FOR THE SOLUTION */
/*                 VECTOR. */
/*          D      D.P. ARRAY.  D IS USED FOR THE COMPUTATION OF THE */
/*                 PSEUDO-RESIDUAL ARRAY FOR THE CURRENT ITERATION. */
/*          ICNT   NUMBER OF ITERATIONS SINCE LAST CHANGE OF SME */

/* ... SPECIFICATIONS OF ARGUMENTS */


/* ... SPECIFICATIONS OF LOCAL VARIABLES */


/* ... SPECIFICATIONS OF FUNCTION SUBPROGRAMS */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN SUBROUTINE JSI */

    /* Parameter adjustments */
    --d__;
    --u1;
    --u;
    --rhs;
    --ia;
    --ja;
    --a;

    /* Function Body */
    n = *nn;
    if (itcom1_1.in == 0) {
    *icnt = 0;
    }

/* ... COMPUTE PSEUDO-RESIDUALS */

    dcopy_(&n, &rhs[1], &c__1, &d__[1], &c__1);
    pjac_(&n, &ia[1], &ja[1], &a[1], &u[1], &d__[1]);
    vevmw_(&n, &d__[1], &u[1]);

/* ... STOPPING AND ADAPTIVE CHANGE TESTS */

    oldnrm = itcom3_1.delnnm;
    itcom3_1.delnnm = ddot_(&n, &d__[1], &c__1, &d__[1], &c__1);
    dnrm = itcom3_1.delnnm;
    con = itcom3_1.cme;
    pstop_(&n, &u[1], &dnrm, &con, &c__1, &q1);
    if (itcom2_1.halt) {
    goto L40;
    }
    if (! itcom2_1.adapt) {
    goto L30;
    }
    if (! tstchg_(&c__1)) {
    goto L10;
    }

/* ... CHANGE ITERATIVE PARAMETERS (CME) */

    dtnrm = pvtbv_(&n, &ia[1], &ja[1], &a[1], &d__[1]);
    chgsi_(&dtnrm, &c__1);
    if (! itcom2_1.adapt) {
    goto L30;
    }
    goto L20;

/* ... TEST IF SME NEEDS TO BE CHANGED AND CHANGE IF NECESSARY. */

L10:
    if (itcom2_1.caseii) {
    goto L30;
    }
    if (! chgsme_(&oldnrm, icnt)) {
    goto L30;
    }
    *icnt = 0;

/* ... COMPUTE U(IN+1) AFTER CHANGE OF PARAMETERS */

L20:
    dcopy_(&n, &u[1], &c__1, &u1[1], &c__1);
    daxpy_(&n, &itcom3_1.gamma, &d__[1], &c__1, &u1[1], &c__1);
    goto L40;

/* ... COMPUTE U(IN+1) WITHOUT CHANGE OF PARAMETERS */

L30:
    parsi_(&c1, &c2, &c3, &c__1);
    sum3_(&n, &c1, &d__[1], &c2, &u[1], &c3, &u1[1]);

/* ... OUTPUT INTERMEDIATE INFORMATION */

L40:
    iterm_(&n, &a[1], &u[1], &d__[1], &c__2);

    return 0;
} /* itjsi_ */

/* Subroutine */ int itsor_(integer *nn, integer *ia, integer *ja, doublereal 
    *a, doublereal *rhs, doublereal *u, doublereal *wk)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */

    /* Local variables */
    static doublereal dnrm, h__;
    static integer n, iphat;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
        doublereal *, integer *), iterm_(integer *, doublereal *, 
        doublereal *, doublereal *, integer *);
    extern integer ipstr_(doublereal *);
    static logical q1;
    extern /* Subroutine */ int pstop_(integer *, doublereal *, doublereal *, 
        doublereal *, integer *, logical *);
    static doublereal spcrm1;
    extern /* Subroutine */ int pfsor1_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *);
    static logical change;
    static integer ip;
    static doublereal omegap;
    static integer ipstar;
    extern doublereal tau_(integer *);
    static integer iss;


/* ... FUNCTION: */

/*          THIS SUBROUTINE, ITSOR, PERFORMS ONE ITERATION OF THE */
/*          SUCCESSIVE OVERRELAXATION ALGORITHM.  IT IS CALLED BY SOR. */

/* ... PARAMETER LIST: */

/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX. (= NN) */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF */
/*                 THE SPARSE MATRIX REPRESENTATION. */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE */
/*                 MATRIX REPRESENTATION. */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE */
/*                 OF THE MATRIX PROBLEM. */
/*          U      INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U CONTAINS THE */
/*                 SOLUTION VECTOR AFTER IN ITERATIONS.  ON OUTPUT, */
/*                 IT WILL CONTAIN THE NEWEST ESTIMATE FOR THE SOLUTION */
/*                 VECTOR. */
/*          WK     D.P. ARRAY.  WORK VECTOR OF LENGTH N. */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */



/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN SUBROUTINE SOR */

/* ... SET INITIAL PARAMETERS NOT ALREADY SET */

    /* Parameter adjustments */
    --wk;
    --u;
    --rhs;
    --ia;
    --ja;
    --a;

    /* Function Body */
    n = *nn;
    if (itcom1_1.in != 0) {
    goto L20;
    }
    pstop_(&n, &u[1], &c_b21, &c_b21, &c__0, &q1);
    if (itcom2_1.adapt) {
    goto L10;
    }
    change = FALSE_;
    ip = 0;
    iphat = 2;
    iss = 0;
    goto L30;

L10:
    change = TRUE_;
    ip = 0;
    omegap = itcom3_1.omega;
    itcom3_1.omega = 1.;
    iss = 0;
    iphat = 2;
    ipstar = 4;
    if (omegap <= 1.) {
    change = FALSE_;
    }

/* ... RESET OMEGA, IPHAT, AND IPSTAR (CIRCLE A IN FLOWCHART) */

L20:
    if (! change) {
    goto L30;
    }
    change = FALSE_;
    ++itcom1_1.is;
    ip = 0;
    iss = 0;
/* Computing MIN */
    d__1 = omegap, d__2 = tau_(&itcom1_1.is);
    itcom3_1.omega = min(d__1,d__2);
/* Computing MAX */
    i__1 = 3, i__2 = (integer) ((real) ((itcom3_1.omega - 1.) / (2. - 
        itcom3_1.omega)));
    iphat = max(i__1,i__2);
    ipstar = ipstr_(&itcom3_1.omega);

/* ... COMPUTE U (IN + 1) AND NORM OF DEL(S,P) - CIRCLE B IN FLOW CHART */

L30:
    itcom3_1.delsnm = itcom3_1.delnnm;
    spcrm1 = itcom3_1.specr;
    dcopy_(&n, &rhs[1], &c__1, &wk[1], &c__1);
    pfsor1_(&n, &ia[1], &ja[1], &a[1], &u[1], &wk[1]);
    if (itcom3_1.delnnm == 0.) {
    goto L40;
    }
    if (itcom1_1.in != 0) {
    itcom3_1.specr = itcom3_1.delnnm / itcom3_1.delsnm;
    }
    if (ip < iphat) {
    goto L70;
    }

/* ... STOPPING TEST, SET H */

    if (itcom3_1.specr >= 1.) {
    goto L70;
    }
    if (! (itcom3_1.specr > itcom3_1.omega - 1.)) {
    goto L40;
    }
    h__ = itcom3_1.specr;
    goto L50;
L40:
    ++iss;
    h__ = itcom3_1.omega - 1.;

/* ... PERFORM STOPPING TEST. */

L50:
/* Computing 2nd power */
    d__1 = itcom3_1.delnnm;
    dnrm = d__1 * d__1;
    pstop_(&n, &u[1], &dnrm, &h__, &c__1, &q1);
    if (itcom2_1.halt) {
    goto L70;
    }

/* ... METHOD HAS NOT CONVERGED YET, TEST FOR CHANGING OMEGA */

    if (! itcom2_1.adapt) {
    goto L70;
    }
    if (ip < ipstar) {
    goto L70;
    }
    if (itcom3_1.omega > 1.) {
    goto L60;
    }
    itcom3_1.cme = sqrt((abs(itcom3_1.specr)));
    omegap = 2. / (sqrt((d__1 = 1. - itcom3_1.specr, abs(d__1))) + 1.);
    change = TRUE_;
    goto L70;
L60:
    if (iss != 0) {
    goto L70;
    }
    d__1 = itcom3_1.omega - 1.;
    if (itcom3_1.specr <= pow_dd(&d__1, &itcom3_1.ff)) {
    goto L70;
    }
    if (itcom3_1.specr + 5e-5 <= spcrm1) {
    goto L70;
    }

/* ... CHANGE PARAMETERS */

    itcom3_1.cme = (itcom3_1.specr + itcom3_1.omega - 1.) / (sqrt((abs(
        itcom3_1.specr))) * itcom3_1.omega);
    omegap = 2. / (sqrt((d__1 = 1. - itcom3_1.cme * itcom3_1.cme, abs(d__1))) 
        + 1.);
    change = TRUE_;

/* ... OUTPUT INTERMEDIATE INFORMATION */

L70:
    iterm_(&n, &a[1], &u[1], &wk[1], &c__3);
    ++ip;

    return 0;
} /* itsor_ */

/* Subroutine */ int itsrcg_(integer *nn, integer *ia, integer *ja, 
    doublereal *a, doublereal *rhs, doublereal *u, doublereal *u1, 
    doublereal *c__, doublereal *c1, doublereal *d__, doublereal *dl, 
    doublereal *wk, doublereal *tri)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    extern /* Subroutine */ int pjac_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *), omeg_(doublereal *, 
        integer *);
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
        integer *);
    static doublereal dnrm;
    static integer n;
    extern doublereal pbeta_(integer *, integer *, integer *, doublereal *, 
        doublereal *, doublereal *, doublereal *);
    extern /* Subroutine */ int vfill_(integer *, doublereal *, doublereal *),
         dcopy_(integer *, doublereal *, integer *, doublereal *, integer 
        *), iterm_(integer *, doublereal *, doublereal *, doublereal *, 
        integer *), pbsor_(integer *, integer *, integer *, doublereal *, 
        doublereal *, doublereal *), pfsor_(integer *, integer *, integer 
        *, doublereal *, doublereal *, doublereal *);
    extern doublereal pvtbv_(integer *, integer *, integer *, doublereal *, 
        doublereal *);
    static logical q1;
    extern /* Subroutine */ int vevmw_(integer *, doublereal *, doublereal *);
    static doublereal t1, t2, t3, t4;
    extern /* Subroutine */ int vevpw_(integer *, doublereal *, doublereal *),
         wevmw_(integer *, doublereal *, doublereal *), pstop_(integer *, 
        doublereal *, doublereal *, doublereal *, integer *, logical *);
    static doublereal gamold;
    extern logical omgchg_(integer *);
    extern /* Subroutine */ int chgcon_(doublereal *, doublereal *, 
        doublereal *, integer *), parcon_(doublereal *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *, 
        doublereal *, integer *);
    static doublereal betnew, rhoold, rhotmp;
    extern logical omgstr_(integer *);
    static doublereal con;
    extern /* Subroutine */ int sum3_(integer *, doublereal *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *);


/* ... FUNCTION: */

/*          THIS SUBROUTINE, ITSRCG, PERFORMS ONE ITERATION OF THE */
/*          SYMMETRIC SOR CONJUGATE GRADIENT ALGORITHM.  IT IS CALLED BY */
/*          SSORCG. */

/* ... PARAMETER LIST: */

/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX. (= NN) */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF */
/*                 THE SPARSE MATRIX REPRESENTATION. */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE */
/*                 MATRIX REPRESENTATION. */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE */
/*                 OF THE MATRIX PROBLEM. */
/*          U      INPUT D.P. VECTOR.  CONTAINS THE ESTIMATE OF THE */
/*                 SOLUTION VECTOR AFTER IN ITERATIONS. */
/*          U1     INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U1 CONTAINS THE */
/*                 THE ESTIMATE FOR THE SOLUTION AFTER IN-1 ITERATIONS. */
/*                 ON OUTPUT, U1 CONTAINS THE UPDATED ESTIMATE. */
/*          C      INPUT D.P. VECTOR.  CONTAINS THE FORWARD RESIDUAL */
/*                 AFTER IN ITERATIONS. */
/*          C1     INPUT/OUTPUT D.P. VECTOR.  ON INPUT, C1 CONTAINS */
/*                 THE FORWARD RESIDUAL AFTER IN-1 ITERATIONS.  ON */
/*                 OUTPUT, C1 CONTAINS THE UPDATED FORWARD RESIDUAL. */
/*          D      D.P. VECTOR.  IS USED TO COMPUTE THE BACKWARD PSEUDO- */
/*                 RESIDUAL VECTOR FOR THE CURRENT ITERATION. */
/*          DL     D.P. VECTOR.  IS USED IN THE COMPUTATIONS OF THE */
/*                 ACCELERATION PARAMETERS. */
/*          WK     D.P. VECTOR.  WORKING SPACE OF LENGTH N. */
/*          TRI    D.P. VECTOR. STORES THE TRIDIAGONAL MATRIX ASSOCIATED */
/*                 WITH THE CONJUGATE GRADIENT ACCELERATION. */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* ... SPECIFICATIONS FOR FUNCTION SUBPROGRAMS */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN SUBROUTINE SSORCG */

/* ... CALCULATE S-PRIME FOR ADAPTIVE PROCEDURE. */

    /* Parameter adjustments */
    --wk;
    --dl;
    --d__;
    --c1;
    --c__;
    --u1;
    --u;
    --rhs;
    --ia;
    --ja;
    --a;
    tri -= 3;

    /* Function Body */
    n = *nn;
    if (itcom2_1.adapt || itcom2_1.partad) {
    chgcon_(&tri[3], &gamold, &rhoold, &c__3);
    }

/* ... COMPUTE BACKWARD RESIDUAL */

    dcopy_(&n, &rhs[1], &c__1, &wk[1], &c__1);
    dcopy_(&n, &c__[1], &c__1, &d__[1], &c__1);
    vevpw_(&n, &d__[1], &u[1]);
    pbsor_(&n, &ia[1], &ja[1], &a[1], &d__[1], &wk[1]);
    vevmw_(&n, &d__[1], &u[1]);

/* ... COMPUTE ACCELERATION PARAMETERS AND THEN U(IN+1) (IN U1) */

    dcopy_(&n, &d__[1], &c__1, &dl[1], &c__1);
    vfill_(&n, &wk[1], &c_b21);
    pfsor_(&n, &ia[1], &ja[1], &a[1], &dl[1], &wk[1]);
    wevmw_(&n, &d__[1], &dl[1]);
    itcom3_1.delnnm = ddot_(&n, &c__[1], &c__1, &c__[1], &c__1);
    if (itcom3_1.delnnm == 0.) {
    goto L30;
    }
    dnrm = ddot_(&n, &c__[1], &c__1, &dl[1], &c__1);
    if (dnrm == 0.) {
    goto L30;
    }
    if (itcom1_1.isym == 0) {
    goto L10;
    }
    rhotmp = ddot_(&n, &c__[1], &c__1, &c1[1], &c__1) - ddot_(&n, &dl[1], &
        c__1, &c1[1], &c__1);
    parcon_(&dnrm, &t1, &t2, &t3, &t4, &gamold, &rhotmp, &c__3);
    rhoold = rhotmp;
    goto L20;
L10:
    parcon_(&dnrm, &t1, &t2, &t3, &t4, &gamold, &rhoold, &c__3);
L20:
    sum3_(&n, &t1, &d__[1], &t2, &u[1], &t3, &u1[1]);

/* ... TEST FOR STOPPING */

L30:
    itcom3_1.bdelnm = ddot_(&n, &d__[1], &c__1, &d__[1], &c__1);
    dnrm = itcom3_1.bdelnm;
    con = itcom3_1.specr;
    pstop_(&n, &u[1], &dnrm, &con, &c__1, &q1);
    if (itcom2_1.halt) {
    goto L100;
    }

/* ... IF NON- OR PARTIALLY-ADAPTIVE, COMPUTE C(IN+1) AND EXIT. */

    if (itcom2_1.adapt) {
    goto L40;
    }
    d__1 = -t1;
    sum3_(&n, &d__1, &dl[1], &t2, &c__[1], &t3, &c1[1]);
    goto L100;

/* ... FULLY ADAPTIVE PROCEDURE */

L40:
    if (omgstr_(&c__1)) {
    goto L90;
    }
    if (omgchg_(&c__1)) {
    goto L50;
    }

/* ... PARAMETERS HAVE BEEN UNCHANGED.  COMPUTE C(IN+1) AND EXIT. */

    d__1 = -t1;
    sum3_(&n, &d__1, &dl[1], &t2, &c__[1], &t3, &c1[1]);
    goto L100;

/* ... IT HAS BEEN DECIDED TO CHANGE PARAMETERS */
/*        (1) COMPUTE NEW BETAB IF BETADT = .TRUE. */

L50:
    if (! itcom2_1.betadt) {
    goto L60;
    }
    betnew = pbeta_(&n, &ia[1], &ja[1], &a[1], &d__[1], &wk[1], &c1[1]) / 
        itcom3_1.bdelnm;
/* Computing MAX */
    d__1 = max(itcom3_1.betab,.25);
    itcom3_1.betab = max(d__1,betnew);

/* ...    (2) COMPUTE NEW CME, OMEGA, AND SPECR */

L60:
    if (itcom2_1.caseii) {
    goto L70;
    }
    dnrm = pvtbv_(&n, &ia[1], &ja[1], &a[1], &d__[1]);
    goto L80;
L70:
    vfill_(&n, &wk[1], &c_b21);
    pjac_(&n, &ia[1], &ja[1], &a[1], &d__[1], &wk[1]);
    dnrm = ddot_(&n, &wk[1], &c__1, &wk[1], &c__1);
L80:
    omeg_(&dnrm, &c__3);

/* ...    (3) COMPUTE NEW FORWARD RESIDUAL SINCE OMEGA HAS BEEN CHANGED. */

L90:
    dcopy_(&n, &rhs[1], &c__1, &wk[1], &c__1);
    dcopy_(&n, &u1[1], &c__1, &c1[1], &c__1);
    pfsor_(&n, &ia[1], &ja[1], &a[1], &c1[1], &wk[1]);
    vevmw_(&n, &c1[1], &u1[1]);

/* ... OUTPUT INTERMEDIATE RESULTS. */

L100:
    iterm_(&n, &a[1], &u[1], &wk[1], &c__4);

    return 0;
} /* itsrcg_ */

/* Subroutine */ int itsrsi_(integer *nn, integer *ia, integer *ja, 
    doublereal *a, doublereal *rhs, doublereal *u, doublereal *u1, 
    doublereal *c__, doublereal *d__, doublereal *ctwd, doublereal *wk)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    extern /* Subroutine */ int pjac_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *), omeg_(doublereal *, 
        integer *);
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
        integer *);
    static doublereal dnrm;
    static integer n;
    extern doublereal pbeta_(integer *, integer *, integer *, doublereal *, 
        doublereal *, doublereal *, doublereal *);
    extern /* Subroutine */ int chgsi_(doublereal *, integer *), vfill_(
        integer *, doublereal *, doublereal *), dcopy_(integer *, 
        doublereal *, integer *, doublereal *, integer *), parsi_(
        doublereal *, doublereal *, doublereal *, integer *), iterm_(
        integer *, doublereal *, doublereal *, doublereal *, integer *);
    static doublereal c1, c2, c3;
    extern /* Subroutine */ int pfsor_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *);
    extern doublereal pvtbv_(integer *, integer *, integer *, doublereal *, 
        doublereal *);
    static logical q1;
    extern /* Subroutine */ int vevmw_(integer *, doublereal *, doublereal *),
         pstop_(integer *, doublereal *, doublereal *, doublereal *, 
        integer *, logical *), vevpw_(integer *, doublereal *, doublereal 
        *), pssor1_(integer *, integer *, integer *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *);
    static doublereal betnew;
    extern logical tstchg_(integer *), omgstr_(integer *);
    static doublereal con;
    extern /* Subroutine */ int sum3_(integer *, doublereal *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *);


/* ... FUNCTION: */

/*          THIS SUBROUTINE, ITSRSI, PERFORMS ONE ITERATION OF THE */
/*          SYMMETRIC SOR SEMI-ITERATION ALGORITHM.  IT IS CALLED BY */
/*          SSORSI. */

/* ... PARAMETER LIST: */

/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX. (= NN) */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF */
/*                 THE SPARSE MATRIX REPRESENTATION. */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE */
/*                 MATRIX REPRESENTATION. */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE */
/*                 OF THE MATRIX PROBLEM. */
/*          U      INPUT D.P. VECTOR.  CONTAINS THE ESTIMATE OF THE */
/*                 SOLUTION VECTOR AFTER IN ITERATIONS. */
/*          U1     INPUT/OUTPUT D.P. VECTOR.  ON INPUT, U1 CONTAINS THE */
/*                 THE ESTIMATE FOR THE SOLUTION AFTER IN-1 ITERATIONS. */
/*                 ON OUTPUT, U1 CONTAINS THE UPDATED ESTIMATE. */
/*          C      D.P. VECTOR.  IS USED TO COMPUTE THE FORWARD PSEUDO- */
/*                 RESIDUAL VECTOR FOR THE CURRENT ITERATION. */
/*          D      D.P. VECTOR.  IS USED TO COMPUTE THE BACKWARD PSEUDO- */
/*                 RESIDUAL VECTOR FOR THE CURRENT ITERATION. */
/*          CTWD   D.P. VECTOR.  IS USED IN THE COMPUTATIONS OF THE */
/*                 ACCELERATION PARAMETERS. */
/*          WK     D.P. VECTOR.  WORKING SPACE OF LENGTH N. */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* ... SPECIFICATIONS FOR FUNCTION SUBPROGRAMS */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN SUBROUTINE SSORSI */

/* ... COMPUTE PSEUDO-RESIDUALS (FORWARD AND BACKWARD) */

    /* Parameter adjustments */
    --wk;
    --ctwd;
    --d__;
    --c__;
    --u1;
    --u;
    --rhs;
    --ia;
    --ja;
    --a;

    /* Function Body */
    n = *nn;
    dcopy_(&n, &rhs[1], &c__1, &wk[1], &c__1);
    dcopy_(&n, &u[1], &c__1, &ctwd[1], &c__1);
    pssor1_(&n, &ia[1], &ja[1], &a[1], &ctwd[1], &wk[1], &c__[1], &d__[1]);

/* ... COMPUTE U(IN+1) -- CONTAINED IN THE VECTOR U1. */

    parsi_(&c1, &c2, &c3, &c__3);
    sum3_(&n, &c1, &d__[1], &c2, &u[1], &c3, &u1[1]);

/* ... TEST FOR STOPPING */

    itcom3_1.bdelnm = ddot_(&n, &d__[1], &c__1, &d__[1], &c__1);
    dnrm = itcom3_1.bdelnm;
    con = itcom3_1.specr;
    pstop_(&n, &u[1], &dnrm, &con, &c__1, &q1);
    if (itcom2_1.halt || ! (itcom2_1.adapt || itcom2_1.partad)) {
    goto L40;
    }

/* ... ADAPTIVE PROCEDURE */

    if (omgstr_(&c__1)) {
    goto L40;
    }
    itcom3_1.delnnm = ddot_(&n, &c__[1], &c__1, &c__[1], &c__1);
    if (itcom1_1.in == itcom1_1.is) {
    itcom3_1.delsnm = itcom3_1.delnnm;
    }
    if (itcom1_1.in == 0 || ! tstchg_(&c__1)) {
    goto L40;
    }

/* ... IT HAS BEEN DECIDED TO CHANGE PARAMETERS. */
/* ...    (1) COMPUTE CTWD */

    dcopy_(&n, &d__[1], &c__1, &ctwd[1], &c__1);
    vfill_(&n, &wk[1], &c_b21);
    pfsor_(&n, &ia[1], &ja[1], &a[1], &ctwd[1], &wk[1]);
    vevpw_(&n, &ctwd[1], &c__[1]);
    vevmw_(&n, &ctwd[1], &d__[1]);

/* ...    (2) COMPUTE NEW SPECTRAL RADIUS FOR CURRENT OMEGA. */

    dnrm = ddot_(&n, &c__[1], &c__1, &ctwd[1], &c__1);
    chgsi_(&dnrm, &c__3);
    if (! itcom2_1.adapt) {
    goto L40;
    }

/* ...    (3) COMPUTE NEW BETAB IF BETADT = .TRUE. */

    if (! itcom2_1.betadt) {
    goto L10;
    }
    betnew = pbeta_(&n, &ia[1], &ja[1], &a[1], &d__[1], &wk[1], &ctwd[1]) / 
        itcom3_1.bdelnm;
/* Computing MAX */
    d__1 = max(itcom3_1.betab,.25);
    itcom3_1.betab = max(d__1,betnew);

/* ...    (4) COMPUTE NEW CME, OMEGA, AND SPECR. */

L10:
    if (itcom2_1.caseii) {
    goto L20;
    }
    dnrm = pvtbv_(&n, &ia[1], &ja[1], &a[1], &d__[1]);
    goto L30;
L20:
    vfill_(&n, &wk[1], &c_b21);
    pjac_(&n, &ia[1], &ja[1], &a[1], &d__[1], &wk[1]);
    dnrm = ddot_(&n, &wk[1], &c__1, &wk[1], &c__1);
L30:
    omeg_(&dnrm, &c__3);

/* ... OUTPUT INTERMEDIATE INFORMATION */

L40:
    iterm_(&n, &a[1], &u[1], &wk[1], &c__5);

    return 0;
} /* itsrsi_ */

/* Subroutine */ int itrscg_(integer *n, integer *nnb, integer *ia, integer *
    ja, doublereal *a, doublereal *ub, doublereal *ub1, doublereal *db, 
    doublereal *db1, doublereal *wb, doublereal *tri)
{
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
        integer *);
    static doublereal dnrm;
    extern /* Subroutine */ int vfill_(integer *, doublereal *, doublereal *),
         iterm_(integer *, doublereal *, doublereal *, doublereal *, 
        integer *);
    static doublereal c1, c2, c3, c4;
    static logical q1;
    extern /* Subroutine */ int pstop_(integer *, doublereal *, doublereal *, 
        doublereal *, integer *, logical *);
    static integer nb, nr;
    extern /* Subroutine */ int chgcon_(doublereal *, doublereal *, 
        doublereal *, integer *);
    static doublereal gamold;
    extern /* Subroutine */ int parcon_(doublereal *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *, 
        doublereal *, integer *);
    static doublereal rhoold;
    extern /* Subroutine */ int prsblk_(integer *, integer *, integer *, 
        integer *, doublereal *, doublereal *, doublereal *), prsred_(
        integer *, integer *, integer *, integer *, doublereal *, 
        doublereal *, doublereal *);
    static doublereal rhotmp, con;
    static integer nrp1;
    extern /* Subroutine */ int sum3_(integer *, doublereal *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *);


/* ... FUNCTION: */

/*          THIS SUBROUTINE, ITRSCG, PERFORMS ONE ITERATION OF THE */
/*          REDUCED SYSTEM CONJUGATE GRADIENT ALGORITHM.  IT IS */
/*          CALLED BY RSCG. */

/* ... PARAMETER LIST: */

/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX. */
/*          NB     INPUT INTEGER.  CONTAINS THE NUMBER OF BLACK POINTS */
/*                 IN THE RED-BLACK MATRIX. (= NNB) */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF */
/*                 THE SPARSE MATRIX REPRESENTATION. */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE */
/*                 MATRIX REPRESENTATION. */
/*          UB     INPUT D.P. VECTOR.  CONTAINS THE ESTIMATE FOR THE */
/*                 SOLUTION ON THE BLACK POINTS AFTER IN ITERATIONS. */
/*          UB1    INPUT/OUTPUT D.P. VECTOR.  ON INPUT, UB1 CONTAINS THE */
/*                 SOLUTION VECTOR AFTER IN-1 ITERATIONS.  ON OUTPUT, */
/*                 IT WILL CONTAIN THE NEWEST ESTIMATE FOR THE SOLUTION */
/*                 VECTOR.  THIS IS ONLY FOR THE BLACK POINTS. */
/*          DB     INPUT D.P. ARRAY.  DB CONTAINS THE VALUE OF THE */
/*                 CURRENT PSEUDO-RESIDUAL ON THE BLACK POINTS. */
/*          DB1    INPUT/OUTPUT D.P. ARRAY.  DB1 CONTAINS THE PSEUDO- */
/*                 RESIDUAL ON THE BLACK POINTS FOR THE IN-1 ITERATION */
/*                 ON INPUT.  ON OUTPUT, IT IS FOR THE IN+1 ITERATION. */
/*          WB     D.P. ARRAY.  WB IS USED FOR COMPUTATIONS INVOLVING */
/*                 BLACK VECTORS. */
/*          TRI    D.P. ARRAY.  STORES THE TRIDIAGONAL MATRIX ASSOCIATED */
/*                 WITH CONJUGATE GRADIENT ACCELERATION. */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* ... SPECIFICATIONS FOR FUNCTION SUBPROGRAMS */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN SUBROUTINE RSCG */

/* ... COMPUTE NEW ESTIMATE FOR CME IF ADAPT = .TRUE. */

    /* Parameter adjustments */
    --db1;
    --ub1;
    --ub;
    --wb;
    --db;
    --ia;
    --ja;
    --a;
    tri -= 3;

    /* Function Body */
    nb = *nnb;
    nr = *n - nb;
    nrp1 = nr + 1;
    if (itcom2_1.adapt) {
    chgcon_(&tri[3], &gamold, &rhoold, &c__2);
    }

/* ... TEST FOR STOPPING */

    itcom3_1.delnnm = ddot_(&nb, &db[1], &c__1, &db[1], &c__1);
    dnrm = itcom3_1.delnnm;
    con = itcom3_1.cme;
    pstop_(&nb, &ub[nrp1], &dnrm, &con, &c__2, &q1);
    if (itcom2_1.halt) {
    goto L30;
    }

/* ... COMPUTE ACCELERATION PARAMETERS */

    vfill_(&nr, &ub1[1], &c_b21);
    prsred_(&nb, &nr, &ia[1], &ja[1], &a[1], &db[1], &ub1[1]);
    vfill_(&nb, &wb[1], &c_b21);
    prsblk_(&nb, &nr, &ia[1], &ja[1], &a[1], &ub1[1], &wb[1]);
    dnrm = ddot_(&nb, &db[1], &c__1, &wb[1], &c__1);
    if (itcom1_1.isym == 0) {
    goto L10;
    }
    rhotmp = ddot_(&nb, &wb[1], &c__1, &db1[1], &c__1);
    parcon_(&dnrm, &c1, &c2, &c3, &c4, &gamold, &rhotmp, &c__2);
    rhoold = rhotmp;
    goto L20;
L10:
    parcon_(&dnrm, &c1, &c2, &c3, &c4, &gamold, &rhoold, &c__2);

/* ... COMPUTE UB(IN+1) AND DB(IN+1) */

L20:
    sum3_(&nb, &c1, &db[1], &c2, &ub[nrp1], &c3, &ub1[nrp1]);
    sum3_(&nb, &c1, &wb[1], &c4, &db[1], &c3, &db1[1]);

/* ... OUTPUT INTERMEDIATE INFORMATION */

L30:
    iterm_(&nb, &a[nrp1], &ub[nrp1], &wb[1], &c__6);

    return 0;
} /* itrscg_ */

/* Subroutine */ int itrssi_(integer *n, integer *nnb, integer *ia, integer *
    ja, doublereal *a, doublereal *rhs, doublereal *ub, doublereal *ub1, 
    doublereal *db)
{
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
        integer *);
    static doublereal dnrm;
    extern /* Subroutine */ int chgsi_(doublereal *, integer *), vfill_(
        integer *, doublereal *, doublereal *), dcopy_(integer *, 
        doublereal *, integer *, doublereal *, integer *), parsi_(
        doublereal *, doublereal *, doublereal *, integer *), iterm_(
        integer *, doublereal *, doublereal *, doublereal *, integer *);
    static doublereal c1, c2, const__, c3;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *, 
        integer *, doublereal *, integer *);
    static logical q1;
    extern /* Subroutine */ int vevmw_(integer *, doublereal *, doublereal *),
         pstop_(integer *, doublereal *, doublereal *, doublereal *, 
        integer *, logical *);
    static integer nb, nr;
    extern logical tstchg_(integer *);
    extern /* Subroutine */ int prsblk_(integer *, integer *, integer *, 
        integer *, doublereal *, doublereal *, doublereal *), prsred_(
        integer *, integer *, integer *, integer *, doublereal *, 
        doublereal *, doublereal *);
    static integer nrp1;
    extern /* Subroutine */ int sum3_(integer *, doublereal *, doublereal *, 
        doublereal *, doublereal *, doublereal *, doublereal *);


/* ... FUNCTION: */

/*          THIS SUBROUTINE, ITRSSI, PERFORMS ONE ITERATION OF THE */
/*          REDUCED SYSTEM SEMI-ITERATION ALGORITHM.  IT IS */
/*          CALLED BY RSSI. */

/* ... PARAMETER LIST: */

/*          N      INPUT INTEGER.  DIMENSION OF THE MATRIX. */
/*          NB     INPUT INTEGER.  CONTAINS THE NUMBER OF BLACK POINTS */
/*                 IN THE RED-BLACK MATRIX. (= NNB) */
/*          IA,JA  INPUT INTEGER VECTORS.  THE TWO INTEGER ARRAYS OF */
/*                 THE SPARSE MATRIX REPRESENTATION. */
/*          A      INPUT D.P. VECTOR.  THE D.P. ARRAY OF THE SPARSE */
/*                 MATRIX REPRESENTATION. */
/*          RHS    INPUT D.P. VECTOR.  CONTAINS THE RIGHT HAND SIDE */
/*                 OF THE MATRIX PROBLEM. */
/*          UB     INPUT D.P. VECTOR.  CONTAINS THE ESTIMATE FOR THE */
/*                 SOLUTION ON THE BLACK POINTS AFTER IN ITERATIONS. */
/*          UB1    INPUT/OUTPUT D.P. VECTOR.  ON INPUT, UB1 CONTAINS THE */
/*                 SOLUTION VECTOR AFTER IN-1 ITERATIONS.  ON OUTPUT, */
/*                 IT WILL CONTAIN THE NEWEST ESTIMATE FOR THE SOLUTION */
/*                 VECTOR.  THIS IS ONLY FOR THE BLACK POINTS. */
/*          DB     INPUT D.P. ARRAY.  DB CONTAINS THE VALUE OF THE */
/*                 CURRENT PSEUDO-RESIDUAL ON THE BLACK POINTS. */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* ... SPECIFICATIONS FOR FUNCTION SUBPROGRAMS */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN SUBROUTINE RSSI */

/* ... COMPUTE UR(IN) INTO UB */

    /* Parameter adjustments */
    --ub1;
    --ub;
    --rhs;
    --db;
    --ia;
    --ja;
    --a;

    /* Function Body */
    nb = *nnb;
    nr = *n - nb;
    nrp1 = nr + 1;
    dcopy_(&nr, &rhs[1], &c__1, &ub[1], &c__1);
    prsred_(&nb, &nr, &ia[1], &ja[1], &a[1], &ub[nrp1], &ub[1]);

/* ... COMPUTE PSEUDO-RESIDUAL, DB(IN) */

    dcopy_(&nb, &rhs[nrp1], &c__1, &db[1], &c__1);
    prsblk_(&nb, &nr, &ia[1], &ja[1], &a[1], &ub[1], &db[1]);
    vevmw_(&nb, &db[1], &ub[nrp1]);

/* ... TEST FOR STOPPING */

    itcom3_1.delnnm = ddot_(&nb, &db[1], &c__1, &db[1], &c__1);
    dnrm = itcom3_1.delnnm;
    const__ = itcom3_1.cme;
    pstop_(&nb, &ub[nrp1], &dnrm, &const__, &c__2, &q1);
    if (itcom2_1.halt) {
    goto L20;
    }
    if (! itcom2_1.adapt) {
    goto L10;
    }

/* ... TEST TO CHANGE PARAMETERS */

    if (! tstchg_(&c__2)) {
    goto L10;
    }

/* ... CHANGE PARAMETERS */

    vfill_(&nr, &ub1[1], &c_b21);
    prsred_(&nb, &nr, &ia[1], &ja[1], &a[1], &db[1], &ub1[1]);
    dnrm = ddot_(&nr, &ub1[1], &c__1, &ub1[1], &c__1);
    chgsi_(&dnrm, &c__2);
    if (! itcom2_1.adapt) {
    goto L10;
    }

/* ... COMPUTE UB(N+1) AFTER CHANGING PARAMETERS */

    dcopy_(&nb, &ub[nrp1], &c__1, &ub1[nrp1], &c__1);
    daxpy_(&nb, &itcom3_1.gamma, &db[1], &c__1, &ub1[nrp1], &c__1);
    goto L20;

/* ... COMPUTE UB(N+1) WITHOUT CHANGE OF PARAMETERS */

L10:
    parsi_(&c1, &c2, &c3, &c__2);
    sum3_(&nb, &c1, &db[1], &c2, &ub[nrp1], &c3, &ub1[nrp1]);

/* ... OUTPUT INTERMEDIATE INFORMATION */

L20:
    iterm_(&nb, &a[nrp1], &ub[nrp1], &db[1], &c__7);

    return 0;
} /* itrssi_ */

integer bisrch_(integer *n, integer *k, integer *l)
{
    /* System generated locals */
    integer ret_val;

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

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


    /* Parameter adjustments */
    --k;

    /* Function Body */
    jleft = 1;
    jright = *n;
    if (*n == 2) {
    goto L40;
    }
    jmid = (*n + 1) / 2;

L10:
    if (*l >= k[jmid]) {
    goto L20;
    }

/* ...... L .GE. K(LEFT)  AND  L .LT. K(JMID) */

    jright = jmid;
    goto L30;

/* ...... L .GE. K(JMID)  AND  L .LT. K(JRIGHT) */

L20:
    jleft = jmid;

/* ...... TEST FOR CONVERGENCE */

L30:
    if (jright - jleft == 1) {
    goto L40;
    }
    jmid = jleft + (jright - jleft + 1) / 2;
    goto L10;

/* ...... BISECTION SEARCH FINISHED */

L40:
    ret_val = jleft;

    return ret_val;
} /* bisrch_ */

doublereal cheby_(doublereal *qa, doublereal *qt, doublereal *rrr, integer *
    ip, doublereal *cme, doublereal *sme)
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2, d__3;

    /* Builtin functions */

    /* Local variables */
    static doublereal x, y, z__;


/*     COMPUTES THE SOLUTION TO THE CHEBYSHEV EQUATION */

/* ... PARAMETER LIST: */

/*          QA     RATIO OF PSEUDO-RESIDUALS */
/*          QT     VIRTUAL SPECTRAL RADIUS */
/*          RRR    ADAPTIVE PARAMETER */
/*          IP     NUMBER OF ITERATIONS SINCE LAST CHANGE OF */
/*                     PARAMETERS */
/*          CME,   ESTIMATES FOR THE LARGEST AND SMALLEST EIGEN- */
/*          SME      VALUES OF THE ITERATION MATRIX */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* Computing 2nd power */
    d__2 = *qa;
/* Computing 2nd power */
    d__3 = *qt;
    z__ = (*qa + sqrt((d__1 = d__2 * d__2 - d__3 * d__3, abs(d__1)))) * .5 * (
        pow_di(rrr, ip) + 1.);
    d__1 = 1. / (doublereal) ((real) (*ip));
    x = pow_dd(&z__, &d__1);
    y = (x + *rrr / x) / (*rrr + 1.);

    ret_val = (*cme + *sme + y * (2. - *cme - *sme)) * .5;

    return ret_val;
} /* cheby_ */

/* Subroutine */ int chgcon_(doublereal *tri, doublereal *gamold, doublereal *
    rhoold, integer *ibmth)
{
    /* Format strings */
    static char fmt_70[] = "(/10x,\002DIFFICULTY IN COMPUTATION OF MAXIMUM E\
IGENVALUE\002/15x,\002OF ITERATION MATRIX\002/10x,\002SUBROUTINE ZBRENT RETU\
RNED IER =\002,i5)";
    static char fmt_90[] = "(/10x,\002DIFFICULTY IN COMPUTATION OF MAXIMUM E\
IGENVALUE\002/15x,\002OF ITERATION MATRIX\002/10x,\002SUBROUTINE EQRT1S RETU\
RNED IER =\002,i5)";
    static char fmt_140[] = "(/10x,\002ESTIMATE OF MAXIMUM EIGENVALUE OF JAC\
OBI   \002/15x,\002MATRIX (CME) NOT ACCURATE\002/10x,\002ADAPTIVE PROCEDURE \
TURNED OFF AT ITERATION \002,i5/10x,\002FINAL ESTIMATE OF MAXIMUM EIGENVALUE\
 =\002,d15.7/)";

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();
    double sqrt(doublereal);

    /* Local variables */
    static doublereal cmold, start;
    static integer ip;
    extern doublereal eigvns_(integer *, doublereal *, doublereal *, 
        doublereal *, integer *);
    static integer ib2, ib3;
    extern doublereal eigvss_(integer *, doublereal *, doublereal *, 
        doublereal *, integer *, integer *);
    static doublereal end;
    static integer ier;

    /* Fortran I/O blocks */
    static cilist io___357 = { 0, 0, 0, fmt_70, 0 };
    static cilist io___360 = { 0, 0, 0, fmt_90, 0 };
    static cilist io___361 = { 0, 0, 0, fmt_140, 0 };



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

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN MAIN SUBROUTINE */

    /* Parameter adjustments */
    tri -= 3;

    /* Function Body */
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
/* Computing 2nd power */
    d__1 = itcom3_1.cme;
    start = d__1 * d__1;
    ip = itcom1_1.in;
    goto L40;

/* ... SSOR CG */

L30:
    if (itcom2_1.adapt) {
    start = itcom3_1.spr;
    }
    if (! itcom2_1.adapt) {
    start = itcom3_1.specr;
    }
    ip = itcom1_1.in - itcom1_1.is;

/* ... DEFINE THE MATRIX */

L40:
    if (ip >= 2) {
    goto L60;
    }
    if (ip == 1) {
    goto L50;
    }

/* ... IP = 0 */

    end = 0.;
    cmold = 0.;
    goto L110;

/* ... IP = 1 */

L50:
    end = 1. - 1. / itcom3_1.gamma;
    tri[3] = end;
    tri[4] = 0.;
    goto L110;

/* ... IP > 1 */

L60:
    if (ip > 2 && (d__1 = start - cmold, abs(d__1)) <= itcom3_1.zeta * start) 
        {
    goto L120;
    }
    cmold = start;

/* ... COMPUTE THE LARGEST EIGENVALUE */

    tri[(ip << 1) + 1] = 1. - 1. / itcom3_1.gamma;
    tri[(ip << 1) + 2] = (itcom3_1.rho - 1.) / (itcom3_1.rho * *rhoold * 
        itcom3_1.gamma * *gamold);
    if (itcom1_1.isym != 0) {
    goto L80;
    }
    end = eigvss_(&ip, &tri[3], &start, &itcom3_1.zeta, &itcom1_1.itmax, &ier)
        ;
    if (ier == 0) {
    goto L100;
    }
    if (itcom1_1.level >= 2) {
    io___357.ciunit = itcom1_1.nout;
    s_wsfe(&io___357);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L100;
L80:
    ib2 = ip + 1;
    ib3 = ib2 + ip / 2 + 1;
    end = eigvns_(&ip, &tri[3], &tri[(ib2 << 1) + 1], &tri[(ib3 << 1) + 1], &
        ier);
    if (ier == 0) {
    goto L100;
    }
    if (itcom1_1.level >= 2) {
    io___360.ciunit = itcom1_1.nout;
    s_wsfe(&io___360);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
L100:
    if (ier != 0) {
    goto L130;
    }

/* ... SET SPECTRAL RADIUS FOR THE VARIOUS METHODS */

L110:
    if (*ibmth == 1) {
    itcom3_1.cme = end;
    }
    if (*ibmth == 2) {
    itcom3_1.cme = sqrt((abs(end)));
    }
    if (*ibmth == 3 && itcom2_1.adapt) {
    itcom3_1.spr = end;
    }
    if (*ibmth == 3 && ! itcom2_1.adapt) {
    itcom3_1.specr = end;
    }
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
    if (itcom1_1.level >= 2) {
    io___361.ciunit = itcom1_1.nout;
    s_wsfe(&io___361);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&start, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }

    return 0;
} /* chgcon_ */

/* Subroutine */ int chgsi_(doublereal *dtnrm, integer *ibmth)
{
    /* Format strings */
    static char fmt_90[] = "(/30x,\002PARAMETERS WERE CHANGED AT ITERATION N\
O.\002,i5/35x,\002SOLUTION TO CHEBYSHEV EQN.       =\002,d15.7/35x,\002SOLUT\
ION TO RAYLEIGH QUOTIENT    =\002,d15.7/35x,\002NEW ESTIMATE FOR CME        \
     =\002,d15.7/35x,\002NEW ESTIMATE FOR GAMMA           =\002,d15.7/35x\
,\002NEW ESTIMATE FOR SPECTRAL RADIUS =\002,d15.7/)";
    static char fmt_110[] = "(/10x,\002ESTIMATE OF MAXIMUM EIGENVALUE OF JAC\
OBI   \002/15x,\002MATRIX (CME) TOO LARGE\002/10x,\002ADAPTIVE PROCEDURE TUR\
NED OFF AT ITERATION \002,i5/10x,\002FINAL ESTIMATE OF MAXIMUM EIGENVALUE \
=\002,d15.7/)";
    static char fmt_100[] = "(/30x,\002PARAMETERS WERE CHANGED AT ITERATION \
NO.\002,i5/35x,\002SOLUTION TO CHEBYSHEV EQN.       =\002,d15.7/35x,\002SOLU\
TION TO RAYLEIGH QUOTIENT    =\002,d15.7/35x,\002NEW ESTIMATE FOR CME       \
      =\002,d15.7/35x,\002NEW ESTIMATE FOR SPECTRAL RADIUS =\002,d15.7/)";

    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();

    /* Local variables */
    extern doublereal cheby_(doublereal *, doublereal *, doublereal *, 
        integer *, doublereal *, doublereal *);
    static doublereal cmold, zm1, zm2;

    /* Fortran I/O blocks */
    static cilist io___365 = { 0, 0, 0, fmt_90, 0 };
    static cilist io___366 = { 0, 0, 0, fmt_110, 0 };
    static cilist io___367 = { 0, 0, 0, fmt_90, 0 };
    static cilist io___368 = { 0, 0, 0, fmt_110, 0 };
    static cilist io___369 = { 0, 0, 0, fmt_100, 0 };



/* ... COMPUTES NEW CHEBYSHEV ACCELERATION PARAMETERS ADAPTIVELY. */

/* ... PARAMETER LIST: */

/*          DTNRM  NUMERATOR OF RAYLEIGH QUOTIENT */
/*          IBMTH  INDICATOR OF BASIC METHOD BEING ACCELERATED BY SI */
/*                      IBMTH = 1,   JACOBI */
/*                            = 2,   REDUCED SYSTEM */
/*                            = 3,   SYMMETRIC SOR */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* ... SPECIFICATIONS FOR FUNCTION SUBPROGRAMS */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN MAIN SUBROUTINE */

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
    if (itcom1_1.in == 0) {
    zm1 = itcom3_1.cme;
    }
    if (itcom1_1.in != 0) {
    i__1 = itcom1_1.in - itcom1_1.is;
    zm1 = cheby_(&itcom3_1.qa, &itcom3_1.qt, &itcom3_1.rrr, &i__1, &
        itcom3_1.cme, &itcom3_1.sme);
    }

/* ... RAYLEIGH QUOTIENT */

    zm2 = *dtnrm / itcom3_1.delnnm;

/* ... COMPUTATION OF ITERATIVE PARAMETERS */

    cmold = itcom3_1.cme;
/* Computing MAX */
    d__1 = max(zm1,zm2);
    itcom3_1.cme = max(d__1,cmold);
    if (itcom3_1.cme >= 1.) {
    goto L20;
    }
    if (itcom2_1.caseii) {
    itcom3_1.sme = -itcom3_1.cme;
    }
    itcom3_1.sige = (itcom3_1.cme - itcom3_1.sme) / (2. - itcom3_1.cme - 
        itcom3_1.sme);
    itcom3_1.gamma = 2. / (2. - itcom3_1.cme - itcom3_1.sme);
    itcom3_1.rrr = (1. - sqrt((d__1 = 1. - itcom3_1.sige * itcom3_1.sige, abs(
        d__1)))) / (sqrt((d__2 = 1. - itcom3_1.sige * itcom3_1.sige, abs(
        d__2))) + 1.);
    itcom1_1.is = itcom1_1.in;
    itcom3_1.delsnm = itcom3_1.delnnm;
    itcom3_1.rho = 1.;
    if (itcom1_1.level >= 2) {
    io___365.ciunit = itcom1_1.nout;
    s_wsfe(&io___365);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&zm1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&zm2, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.gamma, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    return 0;

/* ... ADAPTIVE PROCEDURE FAILED FOR JACOBI SI */

L20:
    itcom3_1.cme = cmold;
    itcom2_1.adapt = FALSE_;
    if (itcom1_1.level >= 2) {
    io___366.ciunit = itcom1_1.nout;
    s_wsfe(&io___366);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    return 0;

/*     ----------------------------- */
/* ... REDUCED SYSTEM SEMI-ITERATIVE */
/*     ----------------------------- */

/* ... CHEBYSHEV EQUATION */

L30:
    if (itcom1_1.in == 0) {
    zm1 = itcom3_1.cme;
    }
    if (itcom1_1.in != 0) {
    i__1 = itcom1_1.in - itcom1_1.is << 1;
    zm1 = cheby_(&itcom3_1.qa, &itcom3_1.qt, &itcom3_1.rrr, &i__1, &c_b21,
         &c_b21);
    }

/* ... RAYLEIGH QUOTIENT */

    zm2 = sqrt((d__1 = *dtnrm / itcom3_1.delnnm, abs(d__1)));

/* ... COMPUTATION OF NEW ITERATIVE PARAMETERS */

    cmold = itcom3_1.cme;
/* Computing MAX */
    d__1 = max(zm1,zm2);
    itcom3_1.cme = max(d__1,cmold);
    if (itcom3_1.cme >= 1.) {
    goto L40;
    }
    itcom3_1.sige = itcom3_1.cme * itcom3_1.cme / (2. - itcom3_1.cme * 
        itcom3_1.cme);
    itcom3_1.gamma = 2. / (2. - itcom3_1.cme * itcom3_1.cme);
    itcom3_1.rrr = (1. - sqrt((d__1 = 1. - itcom3_1.cme * itcom3_1.cme, abs(
        d__1)))) / (sqrt((d__2 = 1. - itcom3_1.cme * itcom3_1.cme, abs(
        d__2))) + 1.);
    itcom1_1.is = itcom1_1.in;
    itcom3_1.delsnm = itcom3_1.delnnm;
    itcom3_1.rho = 1.;
    if (itcom1_1.level >= 2) {
    io___367.ciunit = itcom1_1.nout;
    s_wsfe(&io___367);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&zm1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&zm2, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.gamma, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    return 0;

/* ... ADAPTIVE PROCEDURE FAILED FOR REDUCED SYSTEM SI */

L40:
    itcom3_1.cme = cmold;
    itcom2_1.adapt = FALSE_;
    if (itcom1_1.level >= 2) {
    io___368.ciunit = itcom1_1.nout;
    s_wsfe(&io___368);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    return 0;

/*     ----------------------------- */
/* ... SYMMETRIC SOR SEMI-ITERATIVE */
/*     ---------------------------- */

L50:
    if (itcom3_1.specr == 0.) {
    itcom3_1.specr = .171572875;
    }
    if (itcom1_1.in == 0) {
    goto L60;
    }
    i__1 = itcom1_1.in - itcom1_1.is;
    zm1 = cheby_(&itcom3_1.qa, &itcom3_1.qt, &itcom3_1.rrr, &i__1, &
        itcom3_1.specr, &c_b21);
    goto L70;
L60:
    zm1 = itcom3_1.specr;
    itcom3_1.spr = itcom3_1.specr;

/* ... RAYLEIGH QUOTIENT */

L70:
    zm2 = *dtnrm / itcom3_1.delnnm;

/* ... COMPUTATION OF NEW ESTIMATE FOR SPECTRAL RADIUS */

    if (itcom2_1.adapt) {
    goto L80;
    }

/* ... PARTIALLY ADAPTIVE SSOR SI */

/* Computing MAX */
    d__1 = max(zm1,zm2);
    itcom3_1.specr = max(d__1,itcom3_1.specr);
    itcom1_1.is = itcom1_1.in + 1;
    itcom3_1.delsnm = itcom3_1.delnnm;
    if (itcom1_1.level >= 2) {
    io___369.ciunit = itcom1_1.nout;
    s_wsfe(&io___369);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&zm1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&zm2, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.specr, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    return 0;

/* ... FULLY ADAPTIVE SSOR SI */

L80:
/* Computing MAX */
    d__1 = max(zm1,zm2);
    itcom3_1.spr = max(d__1,itcom3_1.spr);
    return 0;

/* ... FORMAT STATEMENTS */




} /* chgsi_ */

logical chgsme_(doublereal *oldnrm, integer *icnt)
{
    /* Format strings */
    static char fmt_20[] = "(/30x,\002ESTIMATE OF SMALLEST EIGENVALUE OF JAC\
OBI\002/37x,\002MATRIX (SME) CHANGED AT ITERATION \002,i5/35x,\002FIRST ESTI\
MATE OF SME            =\002,d15.7/35x,\002SECOND ESTIMATE OF SME           =\
\002,d15.7/35x,\002NEW ESTIMATE OF SME              =\002,d15.7/)";

    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2, d__3;
    logical ret_val;

    /* Builtin functions */
    double sqrt(doublereal), pow_di(doublereal *, integer *), pow_dd(
        doublereal *, doublereal *);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();

    /* Local variables */
    static doublereal q, z__;
    static integer ip;
    static doublereal rn, wp, sm1, sm2;

    /* Fortran I/O blocks */
    static cilist io___377 = { 0, 0, 0, fmt_20, 0 };



/* ... THIS FUNCTION TESTS FOR JACOBI SI WHETHER SME SHOULD BE CHANGED */
/* ... WHEN CASEII = .FALSE..  IF THE TEST IS POSITIVE THE NEW VALUE */
/* ... OF SME IS COMPUTED. */

/* ... PARAMETER LIST: */

/*          OLDNRM SQUARE OF THE NORM OF THE PSEUDO-RESIDUAL */
/*                    AT THE LAST ITERATION */
/*          ICNT   NUMBER OF ITERATIONS SINCE LAST CHANGE OF */
/*                    PARAMETERS */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN MAIN SUBROUTINE */

    ret_val = FALSE_;
    rn = sqrt(itcom3_1.delnnm / *oldnrm);
    if (! (itcom3_1.qa > 1. && rn > 1.)) {
    return ret_val;
    }
    if (itcom1_1.in <= itcom1_1.is + 2) {
    return ret_val;
    }

    ++(*icnt);
    if (*icnt < 3) {
    return ret_val;
    }

/* ... CHANGE SME IN J-SI ADAPTIVE PROCEDURE */

    ret_val = TRUE_;
    sm1 = 0.;
    sm2 = 0.;
    if (itcom3_1.sme >= itcom3_1.cme) {
    goto L10;
    }

/* ... COMPUTE SM1 */

    ip = itcom1_1.in - itcom1_1.is;
    q = itcom3_1.qa * (pow_di(&itcom3_1.rrr, &ip) + 1.) / (sqrt(pow_di(&
        itcom3_1.rrr, &ip)) * 2.);
/* Computing 2nd power */
    d__2 = q;
    d__1 = q + sqrt(d__2 * d__2 - 1.);
    d__3 = 1. / (doublereal) ((real) ip);
    z__ = pow_dd(&d__1, &d__3);
/* Computing 2nd power */
    d__1 = z__;
    wp = (d__1 * d__1 + 1.) / (z__ * 2.);
    sm1 = (itcom3_1.cme + itcom3_1.sme - wp * (itcom3_1.cme - itcom3_1.sme)) *
         .5;

/* ... COMPUTE SM2 */

    i__1 = ip - 1;
    q = rn * (pow_di(&itcom3_1.rrr, &ip) + 1.) / ((pow_di(&itcom3_1.rrr, &
        i__1) + 1.) * sqrt(itcom3_1.rrr));
/* Computing 2nd power */
    d__1 = q;
    wp = (d__1 * d__1 + 1.) / (q * 2.);
    sm2 = (itcom3_1.cme + itcom3_1.sme - wp * (itcom3_1.cme - itcom3_1.sme)) *
         .5;

L10:
/* Computing MIN */
    d__1 = sm1 * 1.25, d__2 = sm2 * 1.25, d__1 = min(d__1,d__2), d__1 = min(
        d__1,itcom3_1.sme);
    itcom3_1.sme = min(d__1,-1.);
    itcom3_1.sige = (itcom3_1.cme - itcom3_1.sme) / (2. - itcom3_1.cme - 
        itcom3_1.sme);
    itcom3_1.gamma = 2. / (2. - itcom3_1.cme - itcom3_1.sme);
/* Computing 2nd power */
    d__1 = itcom3_1.sige;
/* Computing 2nd power */
    d__2 = itcom3_1.sige;
    itcom3_1.rrr = (1. - sqrt(1. - d__1 * d__1)) / (sqrt(1. - d__2 * d__2) + 
        1.);
    itcom1_1.is = itcom1_1.in;
    itcom3_1.delsnm = itcom3_1.delnnm;
    itcom3_1.rho = 1.;

    if (itcom1_1.level >= 2) {
    io___377.ciunit = itcom1_1.nout;
    s_wsfe(&io___377);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&sm1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&sm2, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.sme, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }


    return ret_val;
} /* chgsme_ */

/* Subroutine */ int daxpy_(integer *n, doublereal *da, doublereal *dx, 
    integer *incx, doublereal *dy, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, m, ix, iy, ns, mp1;


/*     OVERWRITE DOUBLE PRECISION DY WITH DOUBLE PRECISION DA*DX + DY. */

    /* Parameter adjustments */
    --dy;
    --dx;

    /* Function Body */
    if (*n <= 0 || *da == 0.) {
    return 0;
    }
    if (*incx == *incy) {
    if ((i__1 = *incx - 1) < 0) {
        goto L10;
    } else if (i__1 == 0) {
        goto L30;
    } else {
        goto L70;
    }
    }
L10:

/*        CODE FOR NONEQUAL OR NONPOSITIVE INCREMENTS. */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
    ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
    iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    dy[iy] += *da * dx[ix];
    ix += *incx;
    iy += *incy;
/* L20: */
    }
    return 0;

/*        CODE FOR BOTH INCREMENTS EQUAL TO 1 */

/*        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 4. */

L30:
    m = *n - (*n / 4 << 2);
    if (m == 0) {
    goto L50;
    }
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
    dy[i__] += *da * dx[i__];
/* L40: */
    }
    if (*n < 4) {
    return 0;
    }
L50:
    mp1 = m + 1;
    i__1 = *n;
    for (i__ = mp1; i__ <= i__1; i__ += 4) {
    dy[i__] += *da * dx[i__];
    dy[i__ + 1] += *da * dx[i__ + 1];
    dy[i__ + 2] += *da * dx[i__ + 2];
    dy[i__ + 3] += *da * dx[i__ + 3];
/* L60: */
    }
    return 0;

/*        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS. */

L70:
    ns = *n * *incx;
    i__1 = ns;
    i__2 = *incx;
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
    dy[i__] = *da * dx[i__] + dy[i__];
/* L80: */
    }
    return 0;
} /* daxpy_ */

/* Subroutine */ int dcopy_(integer *n, doublereal *dx, integer *incx, 
    doublereal *dy, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, m, ix, iy, ns, mp1;


/*     COPY DOUBLE PRECISION DX TO DOUBLE PRECISION DY. */

    /* Parameter adjustments */
    --dy;
    --dx;

    /* Function Body */
    if (*n <= 0) {
    return 0;
    }
    if (*incx == *incy) {
    if ((i__1 = *incx - 1) < 0) {
        goto L10;
    } else if (i__1 == 0) {
        goto L30;
    } else {
        goto L70;
    }
    }
L10:

/*        CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS. */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
    ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
    iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    dy[iy] = dx[ix];
    ix += *incx;
    iy += *incy;
/* L20: */
    }
    return 0;

/*        CODE FOR BOTH INCREMENTS EQUAL TO 1 */

/*        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 7. */

L30:
    m = *n - *n / 7 * 7;
    if (m == 0) {
    goto L50;
    }
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
    dy[i__] = dx[i__];
/* L40: */
    }
    if (*n < 7) {
    return 0;
    }
L50:
    mp1 = m + 1;
    i__1 = *n;
    for (i__ = mp1; i__ <= i__1; i__ += 7) {
    dy[i__] = dx[i__];
    dy[i__ + 1] = dx[i__ + 1];
    dy[i__ + 2] = dx[i__ + 2];
    dy[i__ + 3] = dx[i__ + 3];
    dy[i__ + 4] = dx[i__ + 4];
    dy[i__ + 5] = dx[i__ + 5];
    dy[i__ + 6] = dx[i__ + 6];
/* L60: */
    }
    return 0;

/*        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS. */

L70:
    ns = *n * *incx;
    i__1 = ns;
    i__2 = *incx;
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
    dy[i__] = dx[i__];
/* L80: */
    }
    return 0;
} /* dcopy_ */

doublereal ddot_(integer *n, doublereal *dx, integer *incx, doublereal *dy, 
    integer *incy)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal ret_val;

    /* Local variables */
    static integer i__, m, ix, iy, ns, mp1;


/*     RETURNS THE DOT PRODUCT OF DOUBLE PRECISION DX AND DY. */

    /* Parameter adjustments */
    --dy;
    --dx;

    /* Function Body */
    ret_val = 0.;
    if (*n <= 0) {
    return ret_val;
    }
    if (*incx == *incy) {
    if ((i__1 = *incx - 1) < 0) {
        goto L10;
    } else if (i__1 == 0) {
        goto L30;
    } else {
        goto L70;
    }
    }
L10:

/*         CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS. */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
    ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
    iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    ret_val += dx[ix] * dy[iy];
    ix += *incx;
    iy += *incy;
/* L20: */
    }
    return ret_val;

/*        CODE FOR BOTH INCREMENTS EQUAL TO 1. */

/*        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5. */

L30:
    m = *n - *n / 5 * 5;
    if (m == 0) {
    goto L50;
    }
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
    ret_val += dx[i__] * dy[i__];
/* L40: */
    }
    if (*n < 5) {
    return ret_val;
    }
L50:
    mp1 = m + 1;
    i__1 = *n;
    for (i__ = mp1; i__ <= i__1; i__ += 5) {
    ret_val = ret_val + dx[i__] * dy[i__] + dx[i__ + 1] * dy[i__ + 1] + 
        dx[i__ + 2] * dy[i__ + 2] + dx[i__ + 3] * dy[i__ + 3] + dx[
        i__ + 4] * dy[i__ + 4];
/* L60: */
    }
    return ret_val;

/*         CODE FOR POSITIVE EQUAL INCREMENTS .NE.1. */

L70:
    ns = *n * *incx;
    i__1 = ns;
    i__2 = *incx;
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
    ret_val += dx[i__] * dy[i__];
/* L80: */
    }
    return ret_val;
} /* ddot_ */

doublereal determ_(integer *n, doublereal *tri, doublereal *xlmda)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    static integer icnt, l;
    static doublereal d1, d2, d3;
    static integer nm1;


/*     THIS SUBROUTINE COMPUTES THE DETERMINANT OF A SYMMETRIC */
/*     TRIDIAGONAL MATRIX GIVEN BY TRI. DET(TRI - XLMDA*I) = 0 */

/* ... PARAMETER LIST */

/*          N      ORDER OF TRIDIAGONAL SYSTEM */
/*          TRI    SYMMETRIC TRIDIAGONAL MATRIX OF ORDER N */
/*          XLMDA  ARGUMENT FOR CHARACTERISTIC EQUATION */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


    /* Parameter adjustments */
    tri -= 3;

    /* Function Body */
    nm1 = *n - 1;
    d2 = tri[(*n << 1) + 1] - *xlmda;
    d1 = d2 * (tri[(nm1 << 1) + 1] - *xlmda) - tri[(*n << 1) + 2];
    if (*n == 2) {
    goto L20;
    }

/* ... BEGINNING OF LOOP */

    i__1 = nm1;
    for (icnt = 2; icnt <= i__1; ++icnt) {
    l = nm1 - icnt + 2;
    d3 = d2;
    d2 = d1;
    d1 = (tri[(l - 1 << 1) + 1] - *xlmda) * d2 - d3 * tri[(l << 1) + 2];
/* L10: */
    }

/* ... DETERMINANT COMPUTED */

L20:
    ret_val = d1;

    return ret_val;
} /* determ_ */

/* Subroutine */ int dfault_(integer *iparm, doublereal *rparm)
{

/* ... THIS SUBROUTINE SETS THE DEFAULT VALUES OF IPARM AND RPARM. */

/* ... PARAMETER LIST: */

/*          IPARM */
/*           AND */
/*          RPARM  ARRAYS SPECIFYING OPTIONS AND TOLERANCES */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

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

    /* Parameter adjustments */
    --rparm;
    --iparm;

    /* Function Body */
    //itcom3_1.drelpr = 7.11e-15;
    itcom3_1.drelpr = getDoublePrecision();

    iparm[1] = 100;
    iparm[2] = 0;
    iparm[3] = 0;
    iparm[4] = 6;
    iparm[5] = 0;
    iparm[6] = 1;
    iparm[7] = 1;
    iparm[8] = 0;
    iparm[9] = -1;
    iparm[10] = 0;
    iparm[11] = 0;
    iparm[12] = 0;

    rparm[1] = 5e-6;
    rparm[2] = 0.;
    rparm[3] = 0.;
    rparm[4] = .75;
    rparm[5] = 1.;
    rparm[6] = 0.;
    rparm[7] = .25;
    rparm[8] = itcom3_1.drelpr * 100.;
    rparm[9] = 0.;
    rparm[10] = 0.;
    rparm[11] = 0.;
    rparm[12] = 0.;

    return 0;
} /* dfault_ */

/* Subroutine */ int echall_(integer *nn, integer *ia, integer *ja, 
    doublereal *a, doublereal *rhs, integer *iparm, doublereal *rparm, 
    integer *icall)
{
    /* Format strings */
    static char fmt_10[] = "(///30x,\002THE LINEAR SYSTEM IS AS FOLLOWS\002)";
    static char fmt_20[] = "(/2x,\002IA ARRAY\002)";
    static char fmt_30[] = "(2x,10(2x,i8))";
    static char fmt_40[] = "(/2x,\002JA ARRAY\002)";
    static char fmt_50[] = "(/2x,\002 A ARRAY\002)";
    static char fmt_60[] = "(2x,5(2x,d20.13))";
    static char fmt_70[] = "(/2x,\002RHS ARRAY\002)";
    static char fmt_90[] = "(///30x,\002INITIAL ITERATIVE PARAMETERS\002)";
    static char fmt_110[] = "(///30x,\002FINAL ITERATIVE PARAMETERS\002)";
    static char fmt_130[] = "(35x,\002IPARM(1)  =\002,i15,4x,\002(ITMAX)\002\
/35x,\002IPARM(2)  =\002,i15,4x,\002(LEVEL) \002/35x,\002IPARM(3)  =\002,i15\
,4x,\002(IRESET)\002/35x,\002IPARM(4)  =\002,i15,4x,\002(NOUT)  \002/35x,\
\002IPARM(5)  =\002,i15,4x,\002(ISYM)  \002/35x,\002IPARM(6)  =\002,i15,4x\
,\002(IADAPT)\002)";
    static char fmt_140[] = "(35x,\002IPARM(7)  =\002,i15,4x,\002(ICASE)\002\
/35x,\002IPARM(8)  =\002,i15,4x,\002(NWKSP)\002/35x,\002IPARM(9)  =\002,i15,\
4x,\002(NB)    \002/35x,\002IPARM(10) =\002,i15,4x,\002(IREMOVE)\002/35x,\
\002IPARM(11) =\002,i15,4x,\002(ITIME)\002/35x,\002IPARM(12) =\002,i15,4x\
,\002(IDGTS)\002)";
    static char fmt_150[] = "(35x,\002RPARM(1)  =\002,d15.8,4x,\002(ZETA) \
 \002/35x,\002RPARM(2)  =\002,d15.8,4x,\002(CME)   \002/35x,\002RPARM(3)  \
=\002,d15.8,4x,\002(SME)   \002/35x,\002RPARM(4)  =\002,d15.8,4x,\002(FF)    \
\002/35x,\002RPARM(5)  =\002,d15.8,4x,\002(OMEGA) \002/35x,\002RPARM(6)  \
=\002,d15.8,4x,\002(SPECR) \002)";
    static char fmt_160[] = "(35x,\002RPARM(7)  =\002,d15.8,4x,\002(BETAB)\
 \002/35x,\002RPARM(8)  =\002,d15.8,4x,\002(TOL)\002/35x,\002RPARM(9)  =\002\
,d15.8,4x,\002(TIME1)\002/35x,\002RPARM(10) =\002,d15.8,4x,\002(TIME2)\002/3\
5x,\002RPARM(11) =\002,d15.8,4x,\002(DIGIT1)\002/35x,\002RPARM(12) =\002,d15\
.8,4x,\002(DIGIT2)\002)";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(), do_fio(integer *, char *, ftnlen);

    /* Local variables */
    static integer nzro, i__, n, np1;

    /* Fortran I/O blocks */
    static cilist io___405 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___406 = { 0, 0, 0, fmt_20, 0 };
    static cilist io___407 = { 0, 0, 0, fmt_30, 0 };
    static cilist io___409 = { 0, 0, 0, fmt_40, 0 };
    static cilist io___410 = { 0, 0, 0, fmt_30, 0 };
    static cilist io___411 = { 0, 0, 0, fmt_50, 0 };
    static cilist io___412 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___413 = { 0, 0, 0, fmt_70, 0 };
    static cilist io___414 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___415 = { 0, 0, 0, fmt_90, 0 };
    static cilist io___416 = { 0, 0, 0, fmt_110, 0 };
    static cilist io___417 = { 0, 0, 0, fmt_130, 0 };
    static cilist io___418 = { 0, 0, 0, fmt_140, 0 };
    static cilist io___419 = { 0, 0, 0, fmt_150, 0 };
    static cilist io___420 = { 0, 0, 0, fmt_160, 0 };



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

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN MAIN SUBROUTINE */

    /* Parameter adjustments */
    --rhs;
    --ia;
    --ja;
    --a;
    --iparm;
    --rparm;

    /* Function Body */
    if (*icall != 1) {
    goto L100;
    }
    n = *nn;
    np1 = n + 1;
    nzro = ia[np1] - 1;

/* ... INITIALIZE ITPACK COMMON */

    itcom3_1.zeta = rparm[1];
    itcom3_1.cme = rparm[2];
    itcom3_1.sme = rparm[3];
    itcom3_1.ff = rparm[4];
    itcom3_1.omega = rparm[5];
    itcom3_1.specr = rparm[6];
    itcom3_1.betab = rparm[7];
    itcom1_1.itmax = iparm[1];
    itcom1_1.level = iparm[2];
    itcom1_1.isym = iparm[5];

    itcom2_1.adapt = FALSE_;
    itcom2_1.partad = FALSE_;
    itcom2_1.betadt = FALSE_;
    if (iparm[6] == 1 || iparm[6] == 3) {
    itcom2_1.adapt = TRUE_;
    }
    if (iparm[6] == 1) {
    itcom2_1.betadt = TRUE_;
    }
    if (iparm[6] == 2) {
    itcom2_1.partad = TRUE_;
    }

    itcom2_1.caseii = FALSE_;
    if (iparm[7] == 2) {
    itcom2_1.caseii = TRUE_;
    }
    if (itcom2_1.caseii) {
    itcom3_1.sme = -itcom3_1.cme;
    }
    if (! itcom2_1.caseii && itcom3_1.sme == 0.) {
    itcom3_1.sme = -1.;
    }
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

    if (itcom1_1.level <= 4) {
    goto L80;
    }

/*     THIS SECTION OF ECHALL CAUSES PRINTING OF THE LINEAR SYSTEM AND */
/*     THE ITERATIVE PARAMETERS */

    io___405.ciunit = itcom1_1.nout;
    s_wsfe(&io___405);
    e_wsfe();
    io___406.ciunit = itcom1_1.nout;
    s_wsfe(&io___406);
    e_wsfe();
    io___407.ciunit = itcom1_1.nout;
    s_wsfe(&io___407);
    i__1 = np1;
    for (i__ = 1; i__ <= i__1; ++i__) {
    do_fio(&c__1, (char *)&ia[i__], (ftnlen)sizeof(integer));
    }
    e_wsfe();
    io___409.ciunit = itcom1_1.nout;
    s_wsfe(&io___409);
    e_wsfe();
    io___410.ciunit = itcom1_1.nout;
    s_wsfe(&io___410);
    i__1 = nzro;
    for (i__ = 1; i__ <= i__1; ++i__) {
    do_fio(&c__1, (char *)&ja[i__], (ftnlen)sizeof(integer));
    }
    e_wsfe();
    io___411.ciunit = itcom1_1.nout;
    s_wsfe(&io___411);
    e_wsfe();
    io___412.ciunit = itcom1_1.nout;
    s_wsfe(&io___412);
    i__1 = nzro;
    for (i__ = 1; i__ <= i__1; ++i__) {
    do_fio(&c__1, (char *)&a[i__], (ftnlen)sizeof(doublereal));
    }
    e_wsfe();
    io___413.ciunit = itcom1_1.nout;
    s_wsfe(&io___413);
    e_wsfe();
    io___414.ciunit = itcom1_1.nout;
    s_wsfe(&io___414);
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    do_fio(&c__1, (char *)&rhs[i__], (ftnlen)sizeof(doublereal));
    }
    e_wsfe();
L80:
    io___415.ciunit = itcom1_1.nout;
    s_wsfe(&io___415);
    e_wsfe();
    goto L120;
L100:
    io___416.ciunit = itcom1_1.nout;
    s_wsfe(&io___416);
    e_wsfe();
L120:
    io___417.ciunit = itcom1_1.nout;
    s_wsfe(&io___417);
    do_fio(&c__1, (char *)&iparm[1], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom1_1.level, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iparm[3], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom1_1.nout, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom1_1.isym, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iparm[6], (ftnlen)sizeof(integer));
    e_wsfe();
    io___418.ciunit = itcom1_1.nout;
    s_wsfe(&io___418);
    do_fio(&c__1, (char *)&iparm[7], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iparm[8], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iparm[9], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iparm[10], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iparm[11], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&iparm[12], (ftnlen)sizeof(integer));
    e_wsfe();
    io___419.ciunit = itcom1_1.nout;
    s_wsfe(&io___419);
    do_fio(&c__1, (char *)&itcom3_1.zeta, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.sme, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.ff, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.omega, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.specr, (ftnlen)sizeof(doublereal));
    e_wsfe();
    io___420.ciunit = itcom1_1.nout;
    s_wsfe(&io___420);
    do_fio(&c__1, (char *)&itcom3_1.betab, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&rparm[8], (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&rparm[9], (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&rparm[10], (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&rparm[11], (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&rparm[12], (ftnlen)sizeof(doublereal));
    e_wsfe();

    return 0;
} /* echall_ */

/* Subroutine */ int echout_(integer *iparm, doublereal *rparm, integer *
    imthd)
{
    /* Format strings */
    static char fmt_10[] = "(///30x,\002INITIAL ITERATIVE PARAMETERS\002,3x\
,\002RELEVANT SWITCHES\002/35x,\002ISYM   =\002,i15,8x,\002IPARM(5)\002/35x\
,\002ITMAX  =\002,i15,8x,\002IPARM(1)\002/35x,\002ZETA   =\002,d15.8,8x,\002\
RPARM(1)\002/35x,\002ADAPT  =\002,l15,8x,\002IPARM(6)\002/35x,\002CASEII \
=\002,l15,8x,\002IPARM(7)\002)";
    static char fmt_30[] = "(35x,\002FF     =\002,d15.8,8x,\002RPARM(4)\002/\
35x,\002CME    =\002,d15.8,8x,\002RPARM(2)\002/35x,\002SME    =\002,d15.8,8x,\
\002RPARM(3)\002///)";
    static char fmt_50[] = "(35x,\002PARTAD =\002,l15,8x,\002IPARM(6)\002/35\
x,\002FF     =\002,d15.8,8x,\002RPARM(4)\002/35x,\002CME    =\002,d15.8,8x\
,\002RPARM(2)\002/35x,\002OMEGA  =\002,d15.8,8x,\002RPARM(5)\002/35x,\002SPE\
CR  =\002,d15.8,8x,\002RPARM(6)\002/35x,\002BETAB  =\002,d15.8,8x,\002RPARM(\
7)\002/35x,\002BETADT =\002,l15,8x,\002IPARM(6)\002///)";
    static char fmt_70[] = "(35x,\002PARTAD =\002,l15,8x,\002IPARM(6)\002/35\
x,\002CME    =\002,d15.8,8x,\002RPARM(2)\002/35x,\002OMEGA  =\002,d15.8,8x\
,\002RPARM(5)\002/35x,\002SPECR  =\002,d15.8,8x,\002RPARM(6)\002/35x,\002BET\
AB  =\002,d15.8,8x,\002RPARM(7)\002/35x,\002BETADT =\002,l15,8x,\002IPARM(6\
)\002///)";
    static char fmt_90[] = "(35x,\002CME    =\002,d15.8,8x,\002RPARM(2)\002/\
//)";

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();

    /* Fortran I/O blocks */
    static cilist io___421 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___422 = { 0, 0, 0, fmt_30, 0 };
    static cilist io___423 = { 0, 0, 0, fmt_50, 0 };
    static cilist io___424 = { 0, 0, 0, fmt_70, 0 };
    static cilist io___425 = { 0, 0, 0, fmt_90, 0 };



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

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN MAIN SUBROUTINE */

/* ... INITIALIZE ITPACK COMMON */

    /* Parameter adjustments */
    --rparm;
    --iparm;

    /* Function Body */
    itcom3_1.zeta = rparm[1];
    itcom3_1.cme = rparm[2];
    itcom3_1.sme = rparm[3];
    itcom3_1.ff = rparm[4];
    itcom3_1.omega = rparm[5];
    itcom3_1.specr = rparm[6];
    itcom3_1.betab = rparm[7];
    itcom1_1.itmax = iparm[1];
    itcom1_1.level = iparm[2];
    itcom1_1.isym = iparm[5];

    itcom2_1.adapt = FALSE_;
    itcom2_1.partad = FALSE_;
    itcom2_1.betadt = FALSE_;
    if (iparm[6] == 1 || iparm[6] == 3) {
    itcom2_1.adapt = TRUE_;
    }
    if (iparm[6] == 1) {
    itcom2_1.betadt = TRUE_;
    }
    if (iparm[6] == 2) {
    itcom2_1.partad = TRUE_;
    }

    itcom2_1.caseii = FALSE_;
    if (iparm[7] == 2) {
    itcom2_1.caseii = TRUE_;
    }
    if (itcom2_1.caseii) {
    itcom3_1.sme = -itcom3_1.cme;
    }
    if (! itcom2_1.caseii && itcom3_1.sme == 0.) {
    itcom3_1.sme = -1.;
    }
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
    if (itcom1_1.level <= 2) {
    return 0;
    }

/* ... THIS SECTION OF ECHOUT ECHOES THE INPUT VALUES FOR THE INITIAL */
/*     ITERATIVE PARAMETERS */

    io___421.ciunit = itcom1_1.nout;
    s_wsfe(&io___421);
    do_fio(&c__1, (char *)&itcom1_1.isym, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom1_1.itmax, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom3_1.zeta, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom2_1.adapt, (ftnlen)sizeof(logical));
    do_fio(&c__1, (char *)&itcom2_1.caseii, (ftnlen)sizeof(logical));
    e_wsfe();
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
    io___422.ciunit = itcom1_1.nout;
    s_wsfe(&io___422);
    do_fio(&c__1, (char *)&itcom3_1.ff, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.sme, (ftnlen)sizeof(doublereal));
    e_wsfe();
    return 0;

/* ... SSORSI */

L40:
    io___423.ciunit = itcom1_1.nout;
    s_wsfe(&io___423);
    do_fio(&c__1, (char *)&itcom2_1.partad, (ftnlen)sizeof(logical));
    do_fio(&c__1, (char *)&itcom3_1.ff, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.omega, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.specr, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.betab, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom2_1.betadt, (ftnlen)sizeof(logical));
    e_wsfe();
    return 0;

/* ... SSORCG */

L60:
    io___424.ciunit = itcom1_1.nout;
    s_wsfe(&io___424);
    do_fio(&c__1, (char *)&itcom2_1.partad, (ftnlen)sizeof(logical));
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.omega, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.specr, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.betab, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom2_1.betadt, (ftnlen)sizeof(logical));
    e_wsfe();
    return 0;

/* ... JCG, RSCG */

L80:
    if (itcom2_1.adapt) {
    return 0;
    }
    io___425.ciunit = itcom1_1.nout;
    s_wsfe(&io___425);
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    e_wsfe();

L100:
    return 0;
} /* echout_ */

doublereal eigvns_(integer *n, doublereal *tri, doublereal *d__, doublereal *
    e2, integer *ier)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val, d__1;

    /* Local variables */
    static integer i__;
    extern /* Subroutine */ int eqrt1s_(doublereal *, doublereal *, integer *,
         integer *, integer *, integer *);


/*     COMPUTES THE LARGEST EIGENVALUE OF A SYMMETRIC TRIDIAGONAL MATRIX */
/*     FOR CONJUGATE GRADIENT ACCELERATION. */

/* ... PARAMETER LIST: */

/*          N      ORDER OF TRIDIAGONAL SYSTEM */
/*          TRI    SYMMETRIC TRIDIAGONAL MATRIX OF ORDER N */
/*          D      ARRAY FOR EQRT1S (NEGATIVE DIAGONAL ELEMENTS) */
/*          E2     ARRAY FOR EQRT1S (SUPER DIAGONAL ELEMENTS) */
/*          IER    ERROR FLAG: ON RETURN, IER=0 INDICATES THAT */
/*                    THE LARGEST EIGENVALUE OF TRI WAS FOUND. */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


    /* Parameter adjustments */
    --e2;
    --d__;
    tri -= 3;

    /* Function Body */
    ret_val;

    d__[1] = -tri[3];
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
    d__[i__] = -tri[(i__ << 1) + 1];
    e2[i__] = (d__1 = tri[(i__ << 1) + 2], abs(d__1));
/* L10: */
    }

    eqrt1s_(&d__[1], &e2[1], n, &c__1, &c__0, ier);
    ret_val = -d__[1];

    return ret_val;
} /* eigvns_ */

doublereal eigvss_(integer *n, doublereal *tri, doublereal *start, doublereal 
    *zeta, integer *itmax, integer *ier)
{
    /* System generated locals */
    doublereal ret_val, d__1;

    /* Builtin functions */

    /* Local variables */
    static integer nsig, itmp;
    static doublereal a, b;
    static integer maxfn;
    extern /* Subroutine */ int zbrent_(integer *, doublereal *, doublereal *,
         integer *, doublereal *, doublereal *, integer *, integer *);
    static doublereal eps;


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

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


    /* Parameter adjustments */
    tri -= 3;

    /* Function Body */
    ret_val;
    d__1 = abs(*zeta);
    itmp = (integer) ((real) (-d_lg10(&d__1)));
    nsig = max(itmp,4);
    maxfn = max(*itmax,50);

/*     EPS = DMIN1(ZETA,0.5D-4) */

    eps = 0.;
    a = *start;
    b = 1.;
    zbrent_(n, &tri[3], &eps, &nsig, &a, &b, &maxfn, ier);
    ret_val = b;

    return ret_val;
} /* eigvss_ */

/* Subroutine */ int eqrt1s_(doublereal *d__, doublereal *e2, integer *nn, 
    integer *m, integer *isw, integer *ierr)
{
    /* Format strings */
    static char fmt_80[] = "(\0020\002,\002*** W A R N I N G ************\
\002/\002 \002,\002    IN ITPACK ROUTINE EQRT1S  \002/\002 \002,\002    PARA\
METER ISW = 1 BUT MATRIX   \002/\002 \002,\002    NOT POSITIVE DEFINITE\002)";
    static char fmt_150[] = "(\0020\002,\002*** W A R N I N G ***********\
*\002/\0020\002,\002    IN ITPACK ROUTINE EQRT1S  \002/\002 \002,\002    SUC\
CESSIVE ITERATES TO THE\002,i10/\002 \002,\002    EIGENVALUE WERE NOT MONOTO\
NE INCREASING \002)";

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);
    integer s_wsfe(cilist *), e_wsfe(), do_fio(integer *, char *, ftnlen);

    /* Local variables */
    static doublereal dlam, f;
    static integer i__, j, k, n;
    static doublereal p, q, r__, s, delta;
    static integer k1, ii, jj;
    static doublereal ep, qp;
    static integer ier;
    static doublereal err, tot;

    /* Fortran I/O blocks */
    static cilist io___446 = { 0, 0, 0, fmt_80, 0 };
    static cilist io___452 = { 0, 0, 0, fmt_80, 0 };
    static cilist io___453 = { 0, 0, 0, fmt_150, 0 };



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
/*                           MATRIX. (= NN) */
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

/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCK IN MAIN SUBROUTINE */

/*                                  SPECIFICATIONS FOR ARGUMENTS */


/*                                  SPECIFICATIONS FOR LOCAL VARIABLES */


/*                                  DRELPR = MACHINE PRECISION */
/*                                  FIRST EXECUTABLE STATEMENT */

    /* Parameter adjustments */
    --e2;
    --d__;

    /* Function Body */
    n = *nn;
    ier = 0;
    dlam = 0.;
    err = 0.;
    s = 0.;

/*                                  LOOK FOR SMALL SUB-DIAGONAL ENTRIES */
/*                                  DEFINE INITIAL SHIFT FROM LOWER */
/*                                  GERSCHGORIN BOUND. */

    tot = d__[1];
    q = 0.;
    j = 0;
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    p = q;
    if (i__ == 1) {
        goto L10;
    }
    if (p > itcom3_1.drelpr * ((d__1 = d__[i__], abs(d__1)) + (d__2 = d__[
        i__ - 1], abs(d__2)))) {
        goto L20;
    }
L10:
    e2[i__] = 0.;

/*                                  COUNT IF E2(I) HAS UNDERFLOWED */

L20:
    if (e2[i__] == 0.) {
        ++j;
    }
    q = 0.;
    if (i__ != n) {
        q = sqrt((d__1 = e2[i__ + 1], abs(d__1)));
    }
/* Computing MIN */
    d__1 = d__[i__] - p - q;
    tot = min(d__1,tot);
/* L30: */
    }
    if (*isw == 1 && tot < 0.) {
    goto L50;
    }
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    d__[i__] -= tot;
/* L40: */
    }
    goto L60;
L50:
    tot = 0.;
L60:
    i__1 = *m;
    for (k = 1; k <= i__1; ++k) {

/*                                  NEXT QR TRANSFORMATION */

L70:
    tot += s;
    delta = d__[n] - s;
    i__ = n;
    f = (d__1 = itcom3_1.drelpr * tot, abs(d__1));
    if (dlam < f) {
        dlam = f;
    }
    if (delta > dlam) {
        goto L90;
    }
    if (delta >= -dlam) {
        goto L170;
    }
    ier = 602;
    if (itcom1_1.level >= 1) {
        io___446.ciunit = itcom1_1.nout;
        s_wsfe(&io___446);
        e_wsfe();
    }
    goto L210;

/*                                  REPLACE SMALL SUB-DIAGONAL SQUARES */
/*                                  BY ZERO TO REDUCE THE INCIDENCE OF */
/*                                  UNDERFLOWS */

L90:
    if (k == n) {
        goto L110;
    }
    k1 = k + 1;
    i__2 = n;
    for (j = k1; j <= i__2; ++j) {
/* Computing 2nd power */
        d__1 = itcom3_1.drelpr * (d__[j] + d__[j - 1]);
        if (e2[j] <= d__1 * d__1) {
        e2[j] = 0.;
        }
/* L100: */
    }
L110:
    f = e2[n] / delta;
    qp = delta + f;
    p = 1.;
    if (k == n) {
        goto L140;
    }
    k1 = n - k;
    i__2 = k1;
    for (ii = 1; ii <= i__2; ++ii) {
        i__ = n - ii;
        q = d__[i__] - s - f;
        r__ = q / qp;
        p = p * r__ + 1.;
        ep = f * r__;
        d__[i__ + 1] = qp + ep;
        delta = q - ep;
        if (delta > dlam) {
        goto L120;
        }
        if (delta >= -dlam) {
        goto L170;
        }
        ier = 602;
        if (itcom1_1.level >= 0) {
        io___452.ciunit = itcom1_1.nout;
        s_wsfe(&io___452);
        e_wsfe();
        }
        goto L210;
L120:
        f = e2[i__] / q;
        qp = delta + f;
        e2[i__ + 1] = qp * ep;
/* L130: */
    }
L140:
    d__[k] = qp;
    s = qp / p;
    if (tot + s > tot) {
        goto L70;
    }
    ier = 601;
    e2[1] = (doublereal) k;
    if (itcom1_1.level >= 1) {
        io___453.ciunit = itcom1_1.nout;
        s_wsfe(&io___453);
        do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
        e_wsfe();
    }

/*                                  SET ERROR -- IRREGULAR END */
/*                                  DEFLATE MINIMUM DIAGONAL ELEMENT */

    s = 0.;
    delta = qp;
    i__2 = n;
    for (j = k; j <= i__2; ++j) {
        if (d__[j] > delta) {
        goto L160;
        }
        i__ = j;
        delta = d__[j];
L160:
        ;
    }

/*                                  CONVERGENCE */

L170:
    if (i__ < n) {
        e2[i__ + 1] = e2[i__] * f / qp;
    }
    if (i__ == k) {
        goto L190;
    }
    k1 = i__ - k;
    i__2 = k1;
    for (jj = 1; jj <= i__2; ++jj) {
        j = i__ - jj;
        d__[j + 1] = d__[j] - s;
        e2[j + 1] = e2[j];
/* L180: */
    }
L190:
    d__[k] = tot;
    err += abs(delta);
    e2[k] = err;
/* L200: */
    }
    if (ier == 0) {
    goto L220;
    }
L210:
L220:
    *ierr = ier;
    return 0;
} /* eqrt1s_ */

integer ipstr_(doublereal *omega)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Builtin functions */
    double pow_di(doublereal *, integer *);

    /* Local variables */
    static integer ip;
    static doublereal wm1;


/*     FINDS THE SMALLEST INTEGER, IPSTR, GREATER THAN 5 SUCH THAT */
/*          IPSTR * (OMEGA-1)**(IPSTR-1) .LE. 0.50. IPSTR WILL BE SET */
/*          IN LOOP. */

/* ... PARAMETER LIST: */

/*          OMEGA  RELAXATION FACTOR FOR SOR METHOD */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


    wm1 = *omega - 1.;

    for (ip = 6; ip <= 940; ++ip) {
    i__1 = ip - 1;
    if ((doublereal) ((real) ip) * pow_di(&wm1, &i__1) > .5) {
        goto L10;
    }
    ret_val = ip;
    return ret_val;
L10:
    ;
    }
    ret_val = 940;
    return ret_val;

} /* ipstr_ */

/* Subroutine */ int iterm_(integer *nn, doublereal *a, doublereal *u, 
    doublereal *wk, integer *imthdd)
{
    /* Format strings */
    static char fmt_20[] = "(////15x,\002INTERMEDIATE OUTPUT AFTER EACH ITER\
ATION\002//\002 NUMBER OF\002,5x,\002CONVERGENCE\002,7x,\002CME \002,11x,\
\002RHO\002,12x,\002GAMMA\002/\002 ITERATIONS\002,4x,\002TEST \002//)";
    static char fmt_40[] = "(4x,i5,3x,4d15.7)";
    static char fmt_60[] = "(////15x,\002INTERMEDIATE OUTPUT AFTER EACH ITER\
ATION\002//\002 NUMBER OF\002,4x,\002CONVERGENCE\002,7x,\002PARAMETER CHANGE\
 TEST\002,10x,\002RHO\002,12x,\002GAMMA\002/\002 ITERATIONS\002,3x,\002TEST\
 \002,11x,\002LHS(QA)\002,7x,\002RHS(QT**FF)\002//)";
    static char fmt_80[] = "(4x,i5,3x,5d15.7)";
    static char fmt_100[] = "(4x,i5,3x,d15.7,30x,2d15.7)";
    static char fmt_120[] = "(////15x,\002INTERMEDIATE OUTPUT AFTER EACH ITE\
RATION\002//\002 NUMBER OF\002,4x,\002CONVERGENCE\002,7x,\002PARAMETER CHANG\
E TEST\002,10x,\002RHO\002/\002 ITERATIONS\002,3x,\002TEST \002,11x,\002LHS(\
QA)\002,7x,\002RHS(QT**FF)\002//)";
    static char fmt_140[] = "(4x,i5,3x,5d15.7)";
    static char fmt_160[] = "(4x,i5,3x,d15.7,30x,d15.7)";
    static char fmt_180[] = "(////15x,\002INTERMEDIATE OUTPUT AFTER EACH ITE\
RATION\002//\002 NUMBER OF\002,4x,\002CONVERGENCE\002,6x,\002CME \002,9x,\
\002OMEGA\002,7x,\002SPECTRAL\002/\002 ITERATIONS\002,3x,\002TEST\002,38x\
,\002RADIUS\002//)";
    static char fmt_200[] = "(4x,i5,3x,4d14.7)";
    static char fmt_220[] = "(////15x,\002INTERMEDIATE OUTPUT AFTER EACH ITE\
RATION\002//\002 NUMBER OF\002,4x,\002CONVERGENCE\002,3x,\002 SPECTRAL\002,6\
x,\002S-PRIME\002,9x,\002RHO\002,10x,\002GAMMA\002/\002 ITERATIONS\002,3x\
,\002TEST \002,10x,\002RADIUS\002//)";
    static char fmt_240[] = "(4x,i5,3x,5d14.7)";
    static char fmt_260[] = "(\0020\002,2x,\002ESTIMATE OF SOLUTION AT ITERA\
TION \002,i5)";
    static char fmt_280[] = "(\0020\002,2x,\002ESTIMATE OF SOLUTION AT BLACK\
 POINTS \002,\002AT ITERATION \002,i5)";
    static char fmt_310[] = "(2x,5(2x,d20.13))";
    static char fmt_320[] = "(//)";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(), do_fio(integer *, char *, ftnlen);
    double pow_dd(doublereal *, doublereal *);

    /* Local variables */
    static doublereal qtff;
    static integer i__, n, imthd, ip;

    /* Fortran I/O blocks */
    static cilist io___459 = { 0, 0, 0, fmt_20, 0 };
    static cilist io___460 = { 0, 0, 0, fmt_40, 0 };
    static cilist io___461 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___464 = { 0, 0, 0, fmt_80, 0 };
    static cilist io___465 = { 0, 0, 0, fmt_100, 0 };
    static cilist io___466 = { 0, 0, 0, fmt_120, 0 };
    static cilist io___467 = { 0, 0, 0, fmt_140, 0 };
    static cilist io___468 = { 0, 0, 0, fmt_160, 0 };
    static cilist io___469 = { 0, 0, 0, fmt_180, 0 };
    static cilist io___470 = { 0, 0, 0, fmt_200, 0 };
    static cilist io___471 = { 0, 0, 0, fmt_220, 0 };
    static cilist io___472 = { 0, 0, 0, fmt_240, 0 };
    static cilist io___473 = { 0, 0, 0, fmt_260, 0 };
    static cilist io___474 = { 0, 0, 0, fmt_280, 0 };
    static cilist io___476 = { 0, 0, 0, fmt_310, 0 };
    static cilist io___477 = { 0, 0, 0, fmt_320, 0 };



/*     THIS ROUTINE PRODUCES THE ITERATION SUMMARY LINE AT THE END */
/*     OF EACH ITERATION. IF LEVEL = 5, THE LATEST APPROXIMATION */
/*     TO THE SOLUTION WILL BE PRINTED. */

/* ... PARAMETER LIST: */

/*          NN     ORDER OF SYSTEM OR, FOR REDUCED SYSTEM */
/*                    ROUTINES, ORDER OF BLACK SUBSYSTEM */
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

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

    /* Parameter adjustments */
    --wk;
    --u;
    --a;

    /* Function Body */
    n = *nn;
    imthd = *imthdd;

/* ... PRINT VARIOUS PARAMETERS AFTER EACH ITERATION */

    if (itcom1_1.level < 2) {
    return 0;
    }
    switch (imthd) {
    case 1:  goto L10;
    case 2:  goto L110;
    case 3:  goto L170;
    case 4:  goto L210;
    case 5:  goto L50;
    case 6:  goto L10;
    case 7:  goto L110;
    }
L10:
    if (itcom1_1.in > 0) {
    goto L30;
    }

/* ... PRINT HEADER FOR JCG AND RSCG */

    io___459.ciunit = itcom1_1.nout;
    s_wsfe(&io___459);
    e_wsfe();

/* ... PRINT SUMMARY LINE */

L30:
    io___460.ciunit = itcom1_1.nout;
    s_wsfe(&io___460);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom3_1.stptst, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.rho, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.gamma, (ftnlen)sizeof(doublereal));
    e_wsfe();
    if (itcom1_1.level >= 4) {
    goto L250;
    }

    return 0;

L50:
    if (itcom1_1.in > 0) {
    goto L70;
    }

/* ... PRINT HEADER FOR SSOR-SI */

    io___461.ciunit = itcom1_1.nout;
    s_wsfe(&io___461);
    e_wsfe();

/* ... PRINT SUMMARY LINE */

L70:
    ip = itcom1_1.in - itcom1_1.is;
    if (imthd == 7) {
    ip <<= 1;
    }
    if (ip < 3) {
    goto L90;
    }
    qtff = pow_dd(&itcom3_1.qt, &itcom3_1.ff);
    io___464.ciunit = itcom1_1.nout;
    s_wsfe(&io___464);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom3_1.stptst, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.qa, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&qtff, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.rho, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.gamma, (ftnlen)sizeof(doublereal));
    e_wsfe();
    if (itcom1_1.level >= 4) {
    goto L250;
    }
    return 0;

L90:
    io___465.ciunit = itcom1_1.nout;
    s_wsfe(&io___465);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom3_1.stptst, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.rho, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.gamma, (ftnlen)sizeof(doublereal));
    e_wsfe();
    if (itcom1_1.level >= 4) {
    goto L250;
    }
    return 0;

L110:
    if (itcom1_1.in > 0) {
    goto L130;
    }

/* ... PRINT HEADER FOR J-SI AND RS-SI */

    io___466.ciunit = itcom1_1.nout;
    s_wsfe(&io___466);
    e_wsfe();

/* ... PRINT SUMMARY LINE */

L130:
    ip = itcom1_1.in - itcom1_1.is;
    if (imthd == 7) {
    ip <<= 1;
    }
    if (ip < 3) {
    goto L150;
    }
    qtff = pow_dd(&itcom3_1.qt, &itcom3_1.ff);
    io___467.ciunit = itcom1_1.nout;
    s_wsfe(&io___467);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom3_1.stptst, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.qa, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&qtff, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.rho, (ftnlen)sizeof(doublereal));
    e_wsfe();
    if (itcom1_1.level >= 4) {
    goto L250;
    }
    return 0;

L150:
    io___468.ciunit = itcom1_1.nout;
    s_wsfe(&io___468);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom3_1.stptst, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.rho, (ftnlen)sizeof(doublereal));
    e_wsfe();
    if (itcom1_1.level >= 4) {
    goto L250;
    }
    return 0;

/* ... PRINT VARIOUS PARAMETERS AFTER EACH ITERATION FOR SOR. */

L170:
    if (itcom1_1.in > 0) {
    goto L190;
    }

/* ... PRINT HEADER FOR SOR */

    io___469.ciunit = itcom1_1.nout;
    s_wsfe(&io___469);
    e_wsfe();

/* ... PRINT SUMMARY LINE FOR SOR */

L190:
    io___470.ciunit = itcom1_1.nout;
    s_wsfe(&io___470);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom3_1.stptst, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.omega, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.specr, (ftnlen)sizeof(doublereal));
    e_wsfe();
    if (itcom1_1.level >= 4) {
    goto L250;
    }

    return 0;

/* ... PRINT VARIOUS PARAMETERS AFTER EACH ITERATION FOR SSOR-CG. */

L210:
    if (itcom1_1.in > 0) {
    goto L230;
    }

/* ... PRINT HEADER FOR SSOR-CG */

    io___471.ciunit = itcom1_1.nout;
    s_wsfe(&io___471);
    e_wsfe();

/* ... PRINT SUMMARY LINE FOR SSOR-CG */

L230:
    io___472.ciunit = itcom1_1.nout;
    s_wsfe(&io___472);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom3_1.stptst, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.specr, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.spr, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.rho, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.gamma, (ftnlen)sizeof(doublereal));
    e_wsfe();
    if (itcom1_1.level >= 4) {
    goto L250;
    }
    return 0;

L250:
    if (imthd > 5) {
    goto L270;
    }
    io___473.ciunit = itcom1_1.nout;
    s_wsfe(&io___473);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    e_wsfe();
    goto L290;
L270:
    io___474.ciunit = itcom1_1.nout;
    s_wsfe(&io___474);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    e_wsfe();
L290:
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    wk[i__] = u[i__] / a[i__];
/* L300: */
    }
    io___476.ciunit = itcom1_1.nout;
    s_wsfe(&io___476);
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    do_fio(&c__1, (char *)&wk[i__], (ftnlen)sizeof(doublereal));
    }
    e_wsfe();
    io___477.ciunit = itcom1_1.nout;
    s_wsfe(&io___477);
    e_wsfe();

    return 0;
} /* iterm_ */

/* Subroutine */ int ivfill_(integer *n, integer *iv, integer *ival)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, m, mp1;


/*     FILLS AN INTEGER VECTOR, IV, WITH AN INTEGER VALUE, IVAL. */

/* ... PARAMETER LIST: */

/*          N      INTEGER LENGTH OF VECTOR IV */
/*          IV     INTEGER VECTOR */
/*          IVAL   INTEGER CONSTANT THAT FILLS FIRST N LOCATIONS OF IV */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


    /* Parameter adjustments */
    --iv;

    /* Function Body */
    if (*n <= 0) {
    return 0;
    }

/*     CLEAN UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 10 */

    m = *n % 10;
    if (m == 0) {
    goto L20;
    }
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
    iv[i__] = *ival;
/* L10: */
    }
    if (*n < 10) {
    return 0;
    }

L20:
    mp1 = m + 1;
    i__1 = *n;
    for (i__ = mp1; i__ <= i__1; i__ += 10) {
    iv[i__] = *ival;
    iv[i__ + 1] = *ival;
    iv[i__ + 2] = *ival;
    iv[i__ + 3] = *ival;
    iv[i__ + 4] = *ival;
    iv[i__ + 5] = *ival;
    iv[i__ + 6] = *ival;
    iv[i__ + 7] = *ival;
    iv[i__ + 8] = *ival;
    iv[i__ + 9] = *ival;
/* L30: */
    }

    return 0;
} /* ivfill_ */

/* Subroutine */ int omeg_(doublereal *dnrm, integer *iflag)
{
    /* Format strings */
    static char fmt_20[] = "(/30x,\002PARAMETERS WERE CHANGED AT ITERATION N\
O.\002,i5/35x,\002NEW ESTIMATE OF BETAB            =\002,d15.7/35x,\002SOLUT\
ION TO CHEBYSHEV EQN.       =\002,d15.7/35x,\002SOLUTION TO RAYLEIGH QUOTIEN\
T    =\002,d15.7/35x,\002NEW ESTIMATE FOR CME             =\002,d15.7/35x\
,\002NEW ESTIMATE FOR OMEGA           =\002,d15.7/35x,\002NEW ESTIMATE FOR S\
PECTRAL RADIUS =\002,d15.7/)";

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();

    /* Local variables */
    static doublereal temp, zm1, zm2;

    /* Fortran I/O blocks */
    static cilist io___484 = { 0, 0, 0, fmt_20, 0 };
    static cilist io___485 = { 0, 0, 0, fmt_20, 0 };



/*     COMPUTES NEW VALUES FOR  CME, OMEGA, AND SPECR FOR */
/*     FULLY ADAPTIVE SSOR METHODS. */

/* ... PARAMETER LIST: */

/*          DNRM   NUMERATOR OF RAYLEIGH QUOTIENT */
/*          IFLAG  INDICATOR OF APPROPRIATE ENTRY POINT */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN MAIN SUBROUTINE */

    zm1 = 0.;
    zm2 = 0.;
    if (*iflag == 1) {
    goto L10;
    }

/* ... IFLAG .NE. 1, COMPUTE NEW ESTIMATE FOR CME */

/* Computing 2nd power */
    d__1 = itcom3_1.omega;
    zm1 = ((1. - itcom3_1.spr) * (itcom3_1.betab * (d__1 * d__1) + 1.) - 
        itcom3_1.omega * (2. - itcom3_1.omega)) / (itcom3_1.omega * (
        itcom3_1.omega - 1. - itcom3_1.spr));

    if (! itcom2_1.caseii) {
    zm2 = *dnrm / itcom3_1.bdelnm;
    }
    if (itcom2_1.caseii) {
    zm2 = sqrt((d__1 = *dnrm / itcom3_1.bdelnm, abs(d__1)));
    }
/* Computing MAX */
    d__1 = max(itcom3_1.cme,zm1);
    itcom3_1.cme = max(d__1,zm2);

/* ... IFLAG = 1, OR CONTINUATION OF IFLAG .NE. 1 */

/*        COMPUTE NEW VALUES OF OMEGA AND SPECR BASED ON CME AND BETAB */

L10:
    itcom1_1.is = itcom1_1.in + 1;
    itcom3_1.delsnm = itcom3_1.delnnm;
    if (itcom3_1.cme >= itcom3_1.betab * 4.) {
    goto L30;
    }

/* ... CME .LT. 4.D0*BETAB */

    temp = sqrt((d__1 = 1. - itcom3_1.cme * 2. + itcom3_1.betab * 4., abs(
        d__1)));
/* Computing MAX */
    d__1 = 2. / (temp + 1.);
    itcom3_1.omega = max(d__1,1.);
    temp = (1. - itcom3_1.cme) / temp;
    itcom3_1.specr = (1. - temp) / (temp + 1.);
    if ((d__1 = itcom3_1.omega - 1., abs(d__1)) < itcom3_1.drelpr) {
    itcom3_1.specr = 0.;
    }
    if (itcom1_1.level >= 2) {
    io___484.ciunit = itcom1_1.nout;
    s_wsfe(&io___484);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom3_1.betab, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&zm1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&zm2, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.omega, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.specr, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }

    return 0;

/* ... CME .GE. 4.D0*BETAB */

/* ... OMEGA-STAR WILL BE CHOSEN */

L30:
    itcom3_1.cme = sqrt((abs(itcom3_1.betab))) * 2.;
    itcom3_1.omega = 2. / (sqrt((d__1 = 1. - itcom3_1.betab * 4., abs(d__1))) 
        + 1.);
    itcom3_1.specr = itcom3_1.omega - 1.;
    itcom2_1.adapt = FALSE_;
    itcom2_1.partad = FALSE_;
    if (itcom1_1.level >= 2) {
    io___485.ciunit = itcom1_1.nout;
    s_wsfe(&io___485);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom3_1.betab, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&zm1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&zm2, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.omega, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.specr, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }

    return 0;
} /* omeg_ */

logical omgchg_(integer *)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3, d__4, d__5, d__6;
    logical ret_val;

    /* Builtin functions */
    double sqrt(doublereal), log(doublereal);

    /* Local variables */
    static doublereal del1, del2;


/* ... THIS FUNCTION TESTS TO SEE WHETHER OMEGA SHOULD BE CHANGED */
/* ... FOR SSOR CG METHOD. */

/* ... PARAMETER LIST: */

/*          NDUMMY ARBITRARY INTEGER PARAMETER */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */



/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN MAIN SUBROUTINE */

/* ... STATEMENT FUNCTION PHI(X) */


    ret_val = FALSE_;
    if (itcom1_1.in - itcom1_1.is < 3) {
    return ret_val;
    }
    if (itcom3_1.specr == 0.) {
    goto L10;
    }
    if (itcom3_1.specr >= itcom3_1.spr) {
    return ret_val;
    }
    d__3 = itcom3_1.specr / itcom3_1.spr;
    del1 = -log((d__6 = (1. - sqrt((d__1 = 1. - itcom3_1.specr, abs(d__1)))) /
         (1. + sqrt((d__2 = 1. - itcom3_1.specr, abs(d__2)))) / ((1. - 
        sqrt((d__4 = 1. - d__3, abs(d__4)))) / (1. + sqrt((d__5 = 1. - 
        d__3, abs(d__5))))), abs(d__6)));
    del2 = -log((d__3 = (1. - sqrt((d__1 = 1. - itcom3_1.spr, abs(d__1)))) / (
        1. + sqrt((d__2 = 1. - itcom3_1.spr, abs(d__2)))), abs(d__3)));
    if (del1 / del2 >= itcom3_1.ff) {
    return ret_val;
    }

L10:
    ret_val = TRUE_;

    return ret_val;
} /* omgchg_ */

logical omgstr_(integer *)
{
    /* Format strings */
    static char fmt_20[] = "(/30x,\002OMEGA-STAR, AN ALTERNATE ESTIMATE O\
F\002,\002 OMEGA, WAS CHOSEN AT ITERATION\002,i5/35x,\002NEW ESTIMATE FOR CM\
E             =\002,d15.7/35x,\002NEW ESTIMATE FOR OMEGA           =\002,d15\
.7/35x,\002NEW ESTIMATE FOR SPECTRAL RADIUS =\002,d15.7/)";

    /* System generated locals */
    doublereal d__1, d__2, d__3, d__4;
    logical ret_val;

    /* Builtin functions */
    double sqrt(doublereal), log(doublereal);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();

    /* Local variables */
    static doublereal temp, temp1, omstar;

    /* Fortran I/O blocks */
    static cilist io___491 = { 0, 0, 0, fmt_20, 0 };



/*     TESTS FOR FULLY ADAPTIVE SSOR METHODS WHETHER OMEGA-STAR */
/*     SHOULD BE USED FOR OMEGA AND THE ADAPTIVE PROCESS TURNED */
/*     OFF. */

/* ... PARAMETER LIST: */

/*          NDUMMY ARBITRARY INTEGER PARAMETER */

/* ... SPECIFICATION FOR ARGUMENT */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */



/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN MAIN SUBROUTINE */

/* ... STATEMENT FUNCTION PHI(X) */


    ret_val = FALSE_;
    if (itcom3_1.betab >= .25 || ! itcom2_1.adapt) {
    return ret_val;
    }
    omstar = 2. / (sqrt((d__1 = 1. - itcom3_1.betab * 4., abs(d__1))) + 1.);

/* ... TEST TO CHOSE OMEGA-STAR */

    if (omstar <= 1. || itcom3_1.specr <= 0.) {
    goto L10;
    }
    d__1 = omstar - 1.;
    temp = log((d__4 = (1. - sqrt((d__2 = 1. - d__1, abs(d__2)))) / (1. + 
        sqrt((d__3 = 1. - d__1, abs(d__3)))), abs(d__4)));
    temp1 = log((d__3 = (1. - sqrt((d__1 = 1. - itcom3_1.specr, abs(d__1)))) /
         (1. + sqrt((d__2 = 1. - itcom3_1.specr, abs(d__2)))), abs(d__3)))
        ;
    if (temp / temp1 < itcom3_1.ff) {
    return ret_val;
    }

/* ... OMEGA-STAR WAS CHOSEN */

L10:
    itcom3_1.omega = omstar;
    itcom3_1.specr = itcom3_1.omega - 1.;
    ret_val = TRUE_;
    itcom2_1.adapt = FALSE_;
    itcom2_1.partad = FALSE_;
    itcom3_1.cme = sqrt((abs(itcom3_1.betab))) * 2.;
    d__1 = 1. - itcom3_1.specr;
/* Computing 2nd power */
    d__4 = (1. - sqrt((d__2 = 1. - d__1, abs(d__2)))) / (1. + sqrt((d__3 = 1. 
        - d__1, abs(d__3))));
    itcom3_1.rrr = d__4 * d__4;
    itcom3_1.gamma = 2. / (2. - itcom3_1.specr);
    itcom3_1.sige = itcom3_1.specr / (2. - itcom3_1.specr);
    itcom3_1.rho = 1.;
    itcom1_1.is = itcom1_1.in + 1;
    itcom3_1.delsnm = itcom3_1.delnnm;
    if (itcom1_1.level >= 2) {
    io___491.ciunit = itcom1_1.nout;
    s_wsfe(&io___491);
    do_fio(&c__1, (char *)&itcom1_1.in, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&itcom3_1.cme, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.omega, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&itcom3_1.specr, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }

    return ret_val;
} /* omgstr_ */

/* Subroutine */ int parcon_(doublereal *dtnrm, doublereal *c1, doublereal *
    c2, doublereal *c3, doublereal *c4, doublereal *gamold, doublereal *
    rhotmp, integer *ibmth)
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

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN MAIN SUBROUTINE */

    ip = itcom1_1.in - itcom1_1.is;

/* ... SET RHOOLD AND GAMOLD */

    rhoold = itcom3_1.rho;
    *gamold = itcom3_1.gamma;

/* ... COMPUTE GAMMA (IN+1) */

/* ... FOR JACOBI OR REDUCED SYSTEM CG */

    if (*ibmth <= 2) {
    itcom3_1.gamma = 1. / (1. - *dtnrm / itcom3_1.delnnm);
    }

/* ... FOR SSOR CG */

    if (*ibmth == 3) {
    itcom3_1.gamma = itcom3_1.delnnm / *dtnrm;
    }

/* ... COMPUTE RHO (IN+1) */

    itcom3_1.rho = 1.;
    if (ip == 0) {
    goto L20;
    }
    if (itcom1_1.isym == 0) {
    goto L10;
    }
    itcom3_1.rho = 1. / (1. - itcom3_1.gamma * *rhotmp / itcom3_1.delsnm);
    goto L20;
L10:
    itcom3_1.rho = 1. / (1. - itcom3_1.gamma * itcom3_1.delnnm / (*gamold * 
        itcom3_1.delsnm * rhoold));

/* ... COMPUTE CONSTANTS C1, C2, C3, AND C4 */

L20:
    itcom3_1.delsnm = itcom3_1.delnnm;
    *rhotmp = rhoold;
    *c1 = itcom3_1.rho * itcom3_1.gamma;
    *c2 = itcom3_1.rho;
    *c3 = 1. - itcom3_1.rho;
    *c4 = itcom3_1.rho * (1. - itcom3_1.gamma);

    return 0;
} /* parcon_ */

/* Subroutine */ int parsi_(doublereal *c1, doublereal *c2, doublereal *c3, 
    integer *ibmth)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

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

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN MAIN SUBROUTINE */

    ip = itcom1_1.in - itcom1_1.is;
    if (ip == 0) {
    goto L30;
    }
    if (ip == 1) {
    goto L10;
    }
    itcom3_1.rho = 1. / (1. - itcom3_1.sige * itcom3_1.sige * itcom3_1.rho * 
        .25);
    goto L20;
L10:
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
    if (itcom2_1.caseii) {
    itcom3_1.sme = -itcom3_1.cme;
    }
    itcom3_1.gamma = 2. / (2. - itcom3_1.cme - itcom3_1.sme);
    itcom3_1.sige = (itcom3_1.cme - itcom3_1.sme) / (2. - itcom3_1.cme - 
        itcom3_1.sme);
    goto L70;

/* ... REDUCED SYSTEM SI */

L50:
    itcom3_1.gamma = 2. / (2. - itcom3_1.cme * itcom3_1.cme);
    itcom3_1.sige = itcom3_1.cme * itcom3_1.cme / (2. - itcom3_1.cme * 
        itcom3_1.cme);
    itcom3_1.rrr = (1. - sqrt((d__1 = 1. - itcom3_1.cme * itcom3_1.cme, abs(
        d__1)))) / (sqrt((d__2 = 1. - itcom3_1.cme * itcom3_1.cme, abs(
        d__2))) + 1.);
    goto L70;

/* ... SSORSI */

L60:
    itcom3_1.gamma = 2. / (2. - itcom3_1.specr);
    itcom3_1.sige = itcom3_1.specr / (2. - itcom3_1.specr);
    itcom3_1.rrr = (1. - sqrt((d__1 = 1. - itcom3_1.sige * itcom3_1.sige, abs(
        d__1)))) / (sqrt((d__2 = 1. - itcom3_1.sige * itcom3_1.sige, abs(
        d__2))) + 1.);

L70:
    itcom3_1.rho = 1.;
    *c1 = itcom3_1.gamma;
    *c2 = 1.;
    *c3 = 0.;

    return 0;
} /* parsi_ */

doublereal pbeta_(integer *nn, integer *ia, integer *ja, doublereal *a, 
    doublereal *v, doublereal *w1, doublereal *w2)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal ret_val;

    /* Local variables */
    static integer jajj, ibgn, iend, itmp;
    static doublereal temp1, temp2;
    static integer i__, k, n, ii, jj, nm1, jai;
    static doublereal sum;


/*     ... COMPUTES THE NUMERATOR FOR THE COMPUTATION OF BETAB IN */
/*     ...  SSOR METHODS. */

/* ... PARAMETER LIST: */

/*          N      DIMENSION OF MATRIX (= NN) */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          W1,W2  WORKSPACE VECTORS OF LENGTH N */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCK IN MAIN SUBROUTINE */

    /* Parameter adjustments */
    --w2;
    --w1;
    --v;
    --ia;
    --ja;
    --a;

    /* Function Body */
    n = *nn;
    ret_val = 0.;
    if (itcom1_1.isym == 0) {
    goto L110;
    }

/*     ************** NON - SYMMETRIC SECTION ******************** */

    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    w1[i__] = v[i__];
/* L10: */
    }
    temp1 = 0.;
    temp2 = 0.;
    itmp = 2;
    ibgn = ia[1];
    iend = ia[itmp] - 1;
    if (iend < ibgn) {
    goto L30;
    }
    i__1 = iend;
    for (i__ = ibgn; i__ <= i__1; ++i__) {
    jai = ja[i__];
    temp1 -= a[i__] * w1[jai];
/* L20: */
    }
L30:
    w1[1] = temp1;
    w2[1] = 0.;
    nm1 = n - 1;
    i__1 = nm1;
    for (k = 2; k <= i__1; ++k) {
    temp1 = 0.;
    temp2 = 0.;
    ibgn = ia[k];
    iend = ia[k + 1] - 1;
    if (iend < ibgn) {
        goto L60;
    }
    i__2 = iend;
    for (i__ = ibgn; i__ <= i__2; ++i__) {
        jai = ja[i__];
        if (jai > k) {
        goto L40;
        }
        temp2 -= a[i__] * w1[jai];
        goto L50;
L40:
        temp1 -= a[i__] * w1[jai];
L50:
        ;
    }
L60:
    w1[k] = temp1;
    w2[k] = temp2;
/* L70: */
    }
    temp2 = 0.;
    ibgn = ia[n];
    iend = ia[n + 1] - 1;
    if (iend < ibgn) {
    goto L90;
    }
    i__1 = iend;
    for (i__ = ibgn; i__ <= i__1; ++i__) {
    jai = ja[i__];
    temp2 -= a[i__] * w1[jai];
/* L80: */
    }
L90:
    w2[n] = temp2;
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    ret_val += v[i__] * w2[i__];
/* L100: */
    }
    return ret_val;

/*     **************** SYMMETRIC SECTION ************************* */

L110:
    i__1 = n;
    for (ii = 1; ii <= i__1; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    sum = 0.;
    if (ibgn > iend) {
        goto L130;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        sum -= a[jj] * v[jajj];
/* L120: */
    }
    ret_val += sum * sum;
L130:
    ;
    }
    return ret_val;

} /* pbeta_ */

/* Subroutine */ int pbsor_(integer *nn, integer *ia, integer *ja, doublereal 
    *a, doublereal *u, doublereal *rhs)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer jajj, ibgn, iend, i__, n, ii, jj;
    static doublereal ui, sum, omm1;
    static integer npl1;


/*     ... THIS SUBROUTINE COMPUTES A BACKWARD SOR SWEEP. */

/* ... PARAMETER LIST: */

/*          N      ORDER OF SYSTEM (= NN) */
/*          OMEGA  RELAXATION FACTOR */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          U      LATEST ESTIMATE OF SOLUTION */
/*          RHS    RIGHT HAND SIDE OF MATRIX PROBLEM */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCK IN MAIN SUBROUTINE */

    /* Parameter adjustments */
    --rhs;
    --u;
    --ia;
    --ja;
    --a;

    /* Function Body */
    n = *nn;
    npl1 = n + 1;
    omm1 = itcom3_1.omega - 1.;
    if (itcom1_1.isym == 0) {
    goto L40;
    }

/*     *************** NON - SYMMETRIC SECTION ********************** */

    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    ii = npl1 - i__;
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    sum = rhs[ii];
    if (ibgn > iend) {
        goto L20;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        sum -= a[jj] * u[jajj];
/* L10: */
    }
L20:
    u[ii] = itcom3_1.omega * sum - omm1 * u[ii];
/* L30: */
    }
    return 0;

/*     ***************** SYMMETRIC SECTION ************************** */

L40:
    i__1 = n;
    for (ii = 1; ii <= i__1; ++ii) {
    ui = u[ii];
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    if (ibgn > iend) {
        goto L60;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        rhs[jajj] -= a[jj] * ui;
/* L50: */
    }
L60:
    ;
    }

    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    ii = npl1 - i__;
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    sum = rhs[ii];
    if (ibgn > iend) {
        goto L80;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        sum -= a[jj] * u[jajj];
/* L70: */
    }
L80:
    u[ii] = itcom3_1.omega * sum - omm1 * u[ii];
/* L90: */
    }
    return 0;

} /* pbsor_ */

/* Subroutine */ int permat_(integer *nn, integer *ia, integer *ja, 
    doublereal *a, integer *p, integer *newia, integer *isym, integer *
    level, integer *nout, integer *ierr)
{
    /* Format strings */
    static char fmt_100[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    IN ITPACK ROUTINE PERMAT  \002/\002 \002,\002\
    NO ENTRY IN ROW \002,i10,\002 OF ORIGINAL MATRIX \002)";
    static char fmt_120[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    IN ITPACK ROUTINE PRBNDX  \002/\002 \002,\002\
    NO ENTRY IN ROW \002,i10,\002 OF PERMUTED MATRIX \002)";
    static char fmt_140[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    IN ITPACK ROUTINE QSORT   \002/\002 \002,\002\
    ERROR IN SORTING PERMUTED ROW \002,i12/\002 \002,\002    CALLED FROM ITP\
ACK ROUTINE PRBNDX   \002)";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();

    /* Local variables */
    static integer ibgn, iend;
    static doublereal save;
    static integer nels;
    static doublereal temp;
    static integer next, i__, j, k, n;
    extern /* Subroutine */ int qsort_(integer *, integer *, doublereal *, 
        integer *);
    static integer ip, jp;
    extern integer bisrch_(integer *, integer *, integer *);
    static integer jaj, ier, ipp, npl1;

    /* Fortran I/O blocks */
    static cilist io___536 = { 0, 0, 0, fmt_100, 0 };
    static cilist io___537 = { 0, 0, 0, fmt_120, 0 };
    static cilist io___538 = { 0, 0, 0, fmt_140, 0 };



/* ********************************************************************* */

/* ... SUBROUTINE PERMAT TAKES THE SPARSE MATRIX REPRESENTATION */
/*     OF THE MATRIX STORED IN THE ARRAYS IA, JA, AND A AND */
/*     PERMUTES BOTH ROWS AND COLUMNS OVERWRITING THE PREVIOUS */
/*     STRUCTURE. */

/* ... PARAMETER LIST: */

/*         N      ORDER OF SYSTEM (= NN) */
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


/* ... INTERNAL VARIABLES */



/* ********************************************************************* */

/* ... PREPROCESSING PHASE */

/* ...... DETERMINE THE NUMBER OF NONZEROES IN THE ROWS OF THE PERMUTED */
/*        MATRIX AND STORE THAT IN NEWIA.  THEN SWEEP THRU NEWIA TO MAKE */
/*        NEWIA(I) POINT TO THE BEGINNING OF EACH ROW IN THE PERMUTED */
/*        DATA STRUCTURE.  ALSO NEGATE ALL THE ENTRIES IN JA TO INDICATE */
/*        THAT THOSE ENTRIES HAVE NOT BEEN MOVED YET. */

    /* Parameter adjustments */
    --newia;
    --p;
    --ia;
    --ja;
    --a;

    /* Function Body */
    n = *nn;
    ier = 0;
    npl1 = n + 1;
    nels = ia[npl1] - 1;
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    newia[i__] = 0;
/* L10: */
    }
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    ip = p[i__];
    ibgn = ia[i__];
    iend = ia[i__ + 1] - 1;
    if (ibgn > iend) {
        goto L90;
    }
    i__2 = iend;
    for (j = ibgn; j <= i__2; ++j) {
        ipp = ip;
        jaj = ja[j];
        jp = p[jaj];
        if (*isym == 0 && ip > jp) {
        ipp = jp;
        }
        ++newia[ipp];
        ja[j] = -jaj;
/* L20: */
    }
/* L30: */
    }
    ibgn = 1;
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    k = ibgn + newia[i__];
    newia[i__] = ibgn;
    ibgn = k;
/* L40: */
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

    i__1 = nels;
    for (j = 1; j <= i__1; ++j) {
    if (ja[j] > 0) {
        goto L70;
    }
    jaj = -ja[j];
    save = a[j];
    next = j;
    ja[j] = jaj;

L50:
    jp = p[jaj];
    i__ = bisrch_(&npl1, &ia[1], &next);
    ip = p[i__];
    ipp = ip;
    if (*isym != 0 || ip <= jp) {
        goto L60;
    }
    ipp = jp;
    jp = ip;
L60:
    next = newia[ipp];

    temp = save;
    save = a[next];
    a[next] = temp;

    jaj = -ja[next];
    ja[next] = jp;
    ++newia[ipp];
    if (jaj > 0) {
        goto L50;
    }

L70:
    ;
    }

/* ...... THE MATRIX IS NOW PERMUTED BUT THE ROWS MAY NOT BE IN */
/*        ORDER.  THE REMAINDER OF THIS SUBROUTINE PERFORMS */
/*        A QUICK SORT ON EACH ROW TO SORT THE ENTRIES IN */
/*        COLUMN ORDER.  THE IA ARRAY IS ALSO CORRECTED FROM */
/*        INFORMATION STORED IN THE NEWIA ARRAY.  NEWIA(I) NOW */
/*        POINTS TO THE FIRST ENTRY OF ROW I+1. */

    ia[1] = 1;
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    ia[i__ + 1] = newia[i__];
    k = ia[i__ + 1] - ia[i__];
    if (k == 1) {
        goto L80;
    }
    if (k < 1) {
        goto L110;
    }

    ibgn = ia[i__];
    qsort_(&k, &ja[ibgn], &a[ibgn], &ier);
    if (ier != 0) {
        goto L130;
    }

L80:
    ;
    }

/* ...... END OF MATRIX PERMUTATION */

    goto L150;

/* ... ERROR TRAPS */

/* ...... NO ENTRY IN ROW I IN THE ORIGINAL SYSTEM */

L90:
    ier = 301;
    if (*level >= 0) {
    io___536.ciunit = *nout;
    s_wsfe(&io___536);
    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L150;

/* ...... NO ENTRY IN ROW I IN THE PERMUTED SYSTEM */

L110:
    ier = 302;
    if (*level >= 0) {
    io___537.ciunit = *nout;
    s_wsfe(&io___537);
    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L150;

/* ...... ERROR RETURN FROM SUBROUTINE QSORT */

L130:
    ier = 303;
    if (*level >= 0) {
    io___538.ciunit = *nout;
    s_wsfe(&io___538);
    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
    e_wsfe();
    }

L150:
    *ierr = ier;
    return 0;
} /* permat_ */

/* Subroutine */ int perror_(integer *nn, integer *ia, integer *ja, 
    doublereal *a, doublereal *rhs, doublereal *u, doublereal *w, 
    doublereal *digtt1, doublereal *digtt2, integer *idgtts)
{
    /* Format strings */
    static char fmt_30[] = "(/6x,\002APPROX. NO. OF DIGITS (EST. REL. ERROR)\
 =\002,f5.1,2x,\002(DIGIT1)\002/3x,\002APPROX. NO. OF DIGITS (EST. REL. RESI\
DUAL) =\002,f5.1,2x,\002(DIGIT2)\002)";

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double d_lg10(doublereal *);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();

    /* Local variables */
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
        integer *);
    static doublereal bnrm, temp, rnrm;
    extern /* Subroutine */ int vout_(integer *, doublereal *, integer *, 
        integer *);
    static integer n, idgts;
    extern /* Subroutine */ int pmult_(integer *, integer *, integer *, 
        doublereal *, doublereal *, doublereal *), wevmw_(integer *, 
        doublereal *, doublereal *);
    static doublereal digit1, digit2;

    /* Fortran I/O blocks */
    static cilist io___546 = { 0, 0, 0, fmt_30, 0 };



/*     PERROR COMPUTES THE RESIDUAL, R = RHS - A*U.  THE USER */
/*     ALSO HAS THE OPTION OF PRINTING THE RESIDUAL AND/OR THE */
/*     UNKNOWN VECTOR DEPENDING ON IDGTS. */

/* ... PARAMETER LIST: */

/*          N      DIMENSION OF MATRIX (= NN) */
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

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* ... SPECIFICATIONS FOR FUNCTION SUBPROGRAMS */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCK IN MAIN SUBROUTINE */

    /* Parameter adjustments */
    --w;
    --u;
    --rhs;
    --ia;
    --ja;
    --a;

    /* Function Body */
    n = *nn;
    idgts = *idgtts;
    digit1 = 0.;
    digit2 = 0.;
    if (n <= 0) {
    goto L40;
    }

    d__1 = abs(itcom3_1.drelpr);
    digit1 = -d_lg10(&d__1);
    if (itcom3_1.stptst > 0.) {
    d__1 = abs(itcom3_1.stptst);
    digit1 = -d_lg10(&d__1);
    }
    bnrm = ddot_(&n, &rhs[1], &c__1, &rhs[1], &c__1);
    if (bnrm == 0.) {
    goto L10;
    }
    pmult_(&n, &ia[1], &ja[1], &a[1], &u[1], &w[1]);
    wevmw_(&n, &rhs[1], &w[1]);
    rnrm = ddot_(&n, &w[1], &c__1, &w[1], &c__1);
    temp = rnrm / bnrm;
    if (temp == 0.) {
    goto L10;
    }
    d__1 = abs(temp);
    digit2 = -d_lg10(&d__1) / 2.;
    goto L20;

L10:
    d__1 = abs(itcom3_1.drelpr);
    digit2 = -d_lg10(&d__1);

L20:
    if (idgts < 1 || itcom1_1.level <= 0) {
    goto L40;
    }
    io___546.ciunit = itcom1_1.nout;
    s_wsfe(&io___546);
    do_fio(&c__1, (char *)&digit1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&digit2, (ftnlen)sizeof(doublereal));
    e_wsfe();

    if (idgts <= 1 || idgts > 4) {
    goto L40;
    }
    if (idgts != 3) {
    vout_(&n, &u[1], &c__2, &itcom1_1.nout);
    }
    if (idgts >= 3) {
    vout_(&n, &w[1], &c__1, &itcom1_1.nout);
    }

L40:
    *digtt1 = digit1;
    *digtt2 = digit2;
    return 0;
} /* perror_ */

/* Subroutine */ int pervec_(integer *n, doublereal *v, integer *p)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static doublereal save, temp;
    static integer next, ii, now;


/*     THIS SUBROUTINE PERMUTES A D.P. VECTOR AS DICTATED BY THE */
/*     PERMUTATION VECTOR, P.  IF P(I) = J, THEN V(J) GETS V(I). */

/* ... PARAMETER LIST: */

/*          V      D.P. VECTOR OF LENGTH N */
/*          P     INTEGER PERMUTATION VECTOR */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


    /* Parameter adjustments */
    --p;
    --v;

    /* Function Body */
    if (*n <= 0) {
    return 0;
    }

    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
    if (p[ii] < 0) {
        goto L20;
    }

    next = p[ii];
    save = v[ii];

L10:
    if (p[next] < 0) {
        goto L20;
    }
    temp = save;
    save = v[next];
    v[next] = temp;

    now = next;
    next = p[now];
    p[now] = -next;
    goto L10;

L20:
    ;
    }

    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
    p[ii] = -p[ii];
/* L30: */
    }

    return 0;
} /* pervec_ */

/* Subroutine */ int pfsor_(integer *nn, integer *ia, integer *ja, doublereal 
    *a, doublereal *u, doublereal *rhs)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer jajj, ibgn, iend, n, ii, jj;
    static doublereal ui, sum, omm1;


/*         THIS SUBROUTINE COMPUTES A FORWARD SOR SWEEP. */

/* ... PARAMETER LIST: */

/*         N       ORDER OF SYSTEM (= NN) */
/*          OMEGA  RELAXATION FACTOR */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          U      LATEST ESTIMATE OF SOLUTION */
/*          RHS    RIGHT HAND SIDE OF MATRIX PROBLEM */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCK IN MAIN SUBROUTINE */

    /* Parameter adjustments */
    --rhs;
    --u;
    --ia;
    --ja;
    --a;

    /* Function Body */
    n = *nn;
    omm1 = itcom3_1.omega - 1.;
    if (itcom1_1.isym == 0) {
    goto L40;
    }

/*     *********** NON - SYMMETRIC SECTION ********************* */

    i__1 = n;
    for (ii = 1; ii <= i__1; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    sum = rhs[ii];
    if (ibgn > iend) {
        goto L20;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        sum -= a[jj] * u[jajj];
/* L10: */
    }
L20:
    ui = itcom3_1.omega * sum - omm1 * u[ii];
    u[ii] = ui;
/* L30: */
    }
    return 0;

/*     ************* SYMMETRIC SECTION ************************* */

L40:
    i__1 = n;
    for (ii = 1; ii <= i__1; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    sum = rhs[ii];
    if (ibgn > iend) {
        goto L60;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        sum -= a[jj] * u[jajj];
/* L50: */
    }
L60:
    ui = itcom3_1.omega * sum - omm1 * u[ii];
    u[ii] = ui;
    if (ibgn > iend) {
        goto L80;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        rhs[jajj] -= a[jj] * ui;
/* L70: */
    }
L80:
    ;
    }
    return 0;

} /* pfsor_ */

/* Subroutine */ int pfsor1_(integer *nn, integer *ia, integer *ja, 
    doublereal *a, doublereal *u, doublereal *rhs)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer jajj, ibgn, iend;
    static doublereal sumd;
    static integer n, ii, jj;
    static doublereal ui, sum, omm1;


/*         THIS SUBROUTINE COMPUTES A FORWARD SOR SWEEP ON U AND */
/*         COMPUTES THE NORM OF THE PSEUDO-RESIDUAL VECTOR. */

/* ... PARAMETER LIST: */

/*          N      ORDER OF SYSTEM (= NN) */
/*          OMEGA  RELAXATION FACTOR */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          U      LATEST ESTIMATE OF SOLUTION */
/*          RHS    RIGHT HAND SIDE OF MATRIX PROBLEM */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCK IN MAIN SUBROUTINE */

    /* Parameter adjustments */
    --rhs;
    --u;
    --ia;
    --ja;
    --a;

    /* Function Body */
    n = *nn;
    omm1 = itcom3_1.omega - 1.;
    sumd = 0.;
    if (itcom1_1.isym == 0) {
    goto L40;
    }

/*     **************** NON - SYMMETRIC SECTION ****************** */

    i__1 = n;
    for (ii = 1; ii <= i__1; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    sum = rhs[ii];
    if (ibgn > iend) {
        goto L20;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        sum -= a[jj] * u[jajj];
/* L10: */
    }
L20:
    ui = itcom3_1.omega * sum - omm1 * u[ii];
/* Computing 2nd power */
    d__1 = ui - u[ii];
    sumd += d__1 * d__1;
    u[ii] = ui;
/* L30: */
    }
    goto L90;

/*     *************** SYMMETRIC SECTION ************************ */

L40:
    i__1 = n;
    for (ii = 1; ii <= i__1; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    sum = rhs[ii];
    if (ibgn > iend) {
        goto L60;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        sum -= a[jj] * u[jajj];
/* L50: */
    }
L60:
    ui = itcom3_1.omega * sum - omm1 * u[ii];
/* Computing 2nd power */
    d__1 = ui - u[ii];
    sumd += d__1 * d__1;
    u[ii] = ui;
    if (ibgn > iend) {
        goto L80;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        rhs[jajj] -= a[jj] * ui;
/* L70: */
    }
L80:
    ;
    }

L90:
    itcom3_1.delnnm = sqrt(sumd);
    return 0;

} /* pfsor1_ */

/* Subroutine */ int pjac_(integer *nn, integer *ia, integer *ja, doublereal *
    a, doublereal *u, doublereal *rhs)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer jajj, ibgn, iend, n;
    static doublereal rhsii;
    static integer ii, jj;
    static doublereal uii;


/*     ... THIS SUBROUTINE PERFORMS ONE JACOBI ITERATION. */

/* ... PARAMETER LIST: */

/*          N      DIMENSION OF MATRIX (= NN) */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          U      ESTIMATE OF SOLUTION OF A MATRIX PROBLEM */
/*          RHS    ON INPUT: CONTAINS THE RIGHT HAND SIDE OF */
/*                    A MATRIX PROBLEM */
/*                 ON OUTPUT: CONTAINS A*U + RHS */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCK IN MAIN SUBROUTINE */

    /* Parameter adjustments */
    --rhs;
    --u;
    --ia;
    --ja;
    --a;

    /* Function Body */
    n = *nn;
    if (itcom1_1.isym == 0) {
    goto L30;
    }

/*     *************** NON - SYMMETRIC SECTION **************** */

    i__1 = n;
    for (ii = 1; ii <= i__1; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    if (ibgn > iend) {
        goto L20;
    }
    rhsii = rhs[ii];
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        rhsii -= a[jj] * u[jajj];
/* L10: */
    }
    rhs[ii] = rhsii;
L20:
    ;
    }
    return 0;

/*     ************** SYMMETRIC SECTION ********************** */

L30:
    i__1 = n;
    for (ii = 1; ii <= i__1; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    if (ibgn > iend) {
        goto L50;
    }
    rhsii = rhs[ii];
    uii = u[ii];
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        rhsii -= a[jj] * u[jajj];
        rhs[jajj] -= a[jj] * uii;
/* L40: */
    }
    rhs[ii] = rhsii;
L50:
    ;
    }
    return 0;

} /* pjac_ */

/* Subroutine */ int pmult_(integer *nn, integer *ia, integer *ja, doublereal 
    *a, doublereal *u, doublereal *w)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer jajj, ibgn, iend, n;
    extern /* Subroutine */ int vfill_(integer *, doublereal *, doublereal *);
    static integer ii, jj;
    static doublereal uii, wii, sum;


/*     ... THIS SUBROUTINE PERFORMS ONE MATRIX-VECTOR MULTIPLICATION. */

/* ... PARAMETER LIST: */

/*          N      DIMENSION OF MATRIX (= NN) */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          U      LATEST ESTIMATE OF SOLUTION */
/*          W      ON RETURN W CONTAINS A*U */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCK IN MAIN SUBROUTINE */

    /* Parameter adjustments */
    --w;
    --u;
    --ia;
    --ja;
    --a;

    /* Function Body */
    n = *nn;
    if (n <= 0) {
    return 0;
    }
    if (itcom1_1.isym == 0) {
    goto L40;
    }

/*     *************** NON - SYMMETRIC SECTION ********************** */

    i__1 = n;
    for (ii = 1; ii <= i__1; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    sum = 0.;
    if (ibgn > iend) {
        goto L20;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        sum += a[jj] * u[jajj];
/* L10: */
    }
L20:
    w[ii] = sum;
/* L30: */
    }
    return 0;

/*     ***************** SYMMETRIC SECTION ************************** */

L40:
    vfill_(&n, &w[1], &c_b21);
    i__1 = n;
    for (ii = 1; ii <= i__1; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    uii = u[ii];
    wii = w[ii];
    if (ibgn > iend) {
        goto L60;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        wii += a[jj] * u[jajj];
        w[jajj] += a[jj] * uii;
/* L50: */
    }
L60:
    w[ii] = wii;
/* L70: */
    }
    return 0;

} /* pmult_ */

/* Subroutine */ int prbndx_(integer *nn, integer *nblack, integer *ia, 
    integer *ja, integer *p, integer *ip, integer *level, integer *nout, 
    integer *ier)
{
    /* Format strings */
    static char fmt_170[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    IN ITPACK ROUTINE PRBNDX  \002/\002 \002,\002\
    RED-BLACK INDEXING NOT POSSIBLE\002)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;

    /* Builtin functions */

    /* Local variables */
    static integer ibgn, iend, nred, last, next, type__, i__, j, k, n, first, 
        young, curtyp, nxttyp, old;

    /* Fortran I/O blocks */
    static cilist io___603 = { 0, 0, 0, fmt_170, 0 };



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

/*        N      NUMBER OF NODES.  (INTEGER, SCALAR) (= NN) */

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



/* ----------------------------------------------------------------------- */

    /* Parameter adjustments */
    --ip;
    --p;
    --ia;
    --ja;

    /* Function Body */
    n = *nn;
    *ier = 0;

/*        IF ( N .LE. 0 ) GO TO 8000 */

    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    p[i__] = 0;
    ip[i__] = 0;
/* L10: */
    }

/* ... HANDLE THE FIRST SET OF POINTS UNTIL SOME ADJACENT POINTS */
/* ... ARE FOUND */

    first = 1;

L20:
    p[first] = first;
    if (ia[first + 1] - ia[first] > 1) {
    goto L40;
    }

/* ... SEARCH FOR NEXT ENTRY THAT HAS NOT BEEN MARKED */

    if (first == n) {
    goto L130;
    }
    ibgn = first + 1;
    i__1 = n;
    for (i__ = ibgn; i__ <= i__1; ++i__) {
    if (p[i__] != 0) {
        goto L30;
    }
    first = i__;
    goto L20;
L30:
    ;
    }
    goto L130;

/* ... FIRST SET OF ADJACENT POINTS FOUND */

L40:
    next = 1;
    last = 1;
    ip[1] = first;

/* ... LOOP OVER LABELED POINTS INDICATED IN THE STACK STORED IN */
/* ... THE ARRAY IP */

L50:
    k = ip[next];
    curtyp = p[k];
    nxttyp = -curtyp;
    ibgn = ia[k];
    iend = ia[k + 1] - 1;
    if (ibgn > iend) {
    goto L110;
    }
    i__1 = iend;
    for (i__ = ibgn; i__ <= i__1; ++i__) {
    j = ja[i__];
    type__ = p[j];
    if (j == k) {
        goto L100;
    }

/* ================================================================== */

/*     THE FOLLOWING IS A FIVE WAY CASE STATEMENT DEALING WITH THE */
/*     LABELING OF THE ADJACENT NODE. */

/* ... CASE I.  IF THE ADJACENT NODE HAS ALREADY BEEN LABELED WITH */
/*              LABEL EQUAL TO NXTTYP, THEN SKIP TO THE NEXT ADJACENT */
/*              NODE. */

    if (type__ == nxttyp) {
        goto L100;
    }

/* ... CASE II.  IF THE ADJACENT NODE HAS NOT BEEN LABELED YET LABEL */
/*               IT WITH NXTTYP AND ENTER IT IN THE STACK */

    if (type__ != 0) {
        goto L60;
    }
    ++last;
    ip[last] = j;
    p[j] = nxttyp;
    goto L100;

/* ... CASE III.  IF THE ADJACENT NODE HAS ALREADY BEEN LABELED WITH */
/*                OPPOSITE COLOR AND THE SAME FATHER SEED, THEN THERE */
/*                IS AN IRRECOVERABLE COLOR CONFLICT. */

L60:
    if (type__ == curtyp) {
        goto L160;
    }

/* ... CASE IV.  IF THE ADJACENT NODE HAS THE RIGHT COLOR AND A DIFFERENT */
/*               FATHER NODE, THEN CHANGE ALL NODES OF THE YOUNGEST FATHE */
/*               NODE TO POINT TO THE OLDEST FATHER SEED AND RETAIN THE */
/*               SAME COLORS. */

    if (type__ * nxttyp < 1) {
        goto L80;
    }
/* Computing MIN */
    i__2 = abs(type__), i__3 = abs(nxttyp);
    old = min(i__2,i__3);
/* Computing MAX */
    i__2 = abs(type__), i__3 = abs(nxttyp);
    young = max(i__2,i__3);
    i__2 = n;
    for (j = young; j <= i__2; ++j) {
        if ((i__3 = p[j], abs(i__3)) == young) {
        p[j] = i_sign(&old, &p[j]);
        }
/* L70: */
    }
    curtyp = p[k];
    nxttyp = -curtyp;
    goto L100;

/* ... CASE V.  IF THE ADJACENT NODE HAS THE WRONG COLOR AND A DIFFERENT */
/*              FATHER NODE, THEN CHANGE ALL NODES OF THE YOUNGEST FATHER */
/*              NODE TO POINT TO THE OLDEST FATHER NODE ALONG WITH */
/*              CHANGING THEIR COLORS.  SINCE UNTIL THIS TIME THE */
/*              YOUNGEST FATHER NODE TREE HAS BEEN INDEPENDENT NO OTHER */
/*              COLOR CONFLICTS WILL ARISE FROM THIS CHANGE. */

L80:
/* Computing MIN */
    i__2 = abs(type__), i__3 = abs(nxttyp);
    old = min(i__2,i__3);
/* Computing MAX */
    i__2 = abs(type__), i__3 = abs(nxttyp);
    young = max(i__2,i__3);
    i__2 = n;
    for (j = young; j <= i__2; ++j) {
        if ((i__3 = p[j], abs(i__3)) == young) {
        i__4 = -p[j];
        p[j] = i_sign(&old, &i__4);
        }
/* L90: */
    }
    curtyp = p[k];
    nxttyp = -curtyp;

/* ... END OF CASE STATEMENT */

/* ================================================================== */

L100:
    ;
    }

/* ... ADVANCE TO NEXT NODE IN THE STACK */

L110:
    ++next;
    if (next <= last) {
    goto L50;
    }

/* ... ALL NODES IN THE STACK HAVE BEEN REMOVED */

/* ... CHECK FOR NODES NOT LABELED.  IF ANY ARE FOUND */
/* ... START THE LABELING PROCESS AGAIN AT THE FIRST */
/* ... NODE FOUND THAT IS NOT LABELED. */

    ibgn = first + 1;
    i__1 = n;
    for (i__ = ibgn; i__ <= i__1; ++i__) {
    if (p[i__] != 0) {
        goto L120;
    }
    first = i__;
    goto L20;
L120:
    ;
    }

/* =================================================================== */

/* ... ALL NODES ARE NOW TYPED EITHER RED OR BLACK */

/* ... GENERATE PERMUTATION VECTORS */

L130:
    nred = 0;
    *nblack = 0;
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    if (p[i__] < 0) {
        goto L140;
    }

/*       RED POINT */

    ++nred;
    ip[nred] = i__;
    p[i__] = nred;
    goto L150;

/*     BLACK POINT */

L140:
    ++(*nblack);
    j = n - *nblack + 1;
    ip[j] = i__;
    p[i__] = j;

L150:
    ;
    }

/* ... SUCCESSFUL RED-BLACK ORDERING COMPLETED */

    goto L180;

/* ........ ERROR TRAPS */

/* ...... N .LE. 0 */

/* 8000    IER = 200 */
/*        GO TO 9000 */

/* ...... TYPE CONFLICT */

L160:
    *ier = 201;
    if (*level >= 0) {
    io___603.ciunit = *nout;
    s_wsfe(&io___603);
    e_wsfe();
    }

/* ... RETURN */

L180:
    return 0;
} /* prbndx_ */

/* Subroutine */ int prsblk_(integer *nnb, integer *nnr, integer *ia, integer 
    *ja, doublereal *a, doublereal *ur, doublereal *vb)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer ibgn, iend, i__, j, nb, nr, jaj, inr;
    static doublereal uri, sum;


/* ... COMPUTE A BLACK-RS SWEEP ON A RED VECTOR INTO A BLACK VECTOR */

/* ... PARAMETER LIST: */

/*         NB      NUMBER OF BLACK POINTS (= NNB) */
/*         NR      NUMBER OF RED POINTS (= NNR) */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          UR     ESTIMATE OF RED SOLUTION VECTOR */
/*          VB     OUTPUT: PRESENT ESTIMATE OF BLACK SOLUTION */
/*                    VECTOR */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCK IN MAIN SUBROUTINE */

    /* Parameter adjustments */
    --vb;
    --ur;
    --ia;
    --ja;
    --a;

    /* Function Body */
    nb = *nnb;
    nr = *nnr;
    if (itcom1_1.isym == 0) {
    goto L30;
    }

/*     *************** NON - SYMMETRIC SECTION ********************** */

    i__1 = nb;
    for (i__ = 1; i__ <= i__1; ++i__) {
    inr = i__ + nr;
    ibgn = ia[inr];
    iend = ia[inr + 1] - 1;
    sum = vb[i__];
    if (ibgn > iend) {
        goto L20;
    }
    i__2 = iend;
    for (j = ibgn; j <= i__2; ++j) {
        jaj = ja[j];
        sum -= a[j] * ur[jaj];
/* L10: */
    }
    vb[i__] = sum;
L20:
    ;
    }
    return 0;

/*     ***************** SYMMETRIC SECTION ************************** */

L30:
    i__1 = nr;
    for (i__ = 1; i__ <= i__1; ++i__) {
    ibgn = ia[i__];
    iend = ia[i__ + 1] - 1;
    if (ibgn > iend) {
        goto L50;
    }
    uri = ur[i__];
    i__2 = iend;
    for (j = ibgn; j <= i__2; ++j) {
        jaj = ja[j] - nr;
        vb[jaj] -= a[j] * uri;
/* L40: */
    }
L50:
    ;
    }

    return 0;
} /* prsblk_ */

/* Subroutine */ int prsred_(integer *, integer *nnr, integer *ia, integer 
    *ja, doublereal *a, doublereal *ub, doublereal *vr)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer jajj, ibgn, iend, /*nb,*/ ii, jj, nr;
    static doublereal sum;


/* ... COMPUTES A RED-RS SWEEP ON A BLACK VECTOR INTO A RED VECTOR. */

/* ... PARAMETER LIST: */

/*         NB      NUMBER OF BLACK POINTS (= NNR) */
/*         NR      NUMBER OF RED POINTS (= NNB) */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          UB     PRESENT ESTIMATE OF BLACK SOLUTION VECTOR */
/*          VR     OUTPUT: PRESENT ESTIMATE OF RED SOLUTION VECTOR */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


    /* Parameter adjustments */
    --ub;
    --vr;
    --ia;
    --ja;
    --a;

    /* Function Body */
//    nb = *nnb;
    nr = *nnr;
    i__1 = nr;
    for (ii = 1; ii <= i__1; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    if (ibgn > iend) {
        goto L20;
    }
    sum = vr[ii];
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj] - nr;
        sum -= a[jj] * ub[jajj];
/* L10: */
    }
    vr[ii] = sum;
L20:
    ;
    }

    return 0;
} /* prsred_ */

/* Subroutine */ int pssor1_(integer *nn, integer *ia, integer *ja, 
    doublereal *a, doublereal *u, doublereal *rhs, doublereal *fr, 
    doublereal *br)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer jajj, ibgn, iend, i__, n, ii, jj;
    static doublereal uii, sum, omm1;
    static integer npl1;


/*     ... COMPUTES COMPLETE SSOR SWEEP ON U.  U IS OVERWRITTEN */
/*     ... WITH THE NEW ITERANT, FR AND BR WILL CONTAIN */
/*     ... THE FORWARD AND BACKWARD RESIDUALS ON OUTPUT. */

/* ... PARAMETER LIST: */

/*         N       ORDER OF SYSTEM (= NN) */
/*          OMEGA  RELAXATION FACTOR */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          U      ESTIMATE OF SOLUTION */
/*          RHS    RIGHT HAND SIDE OF MATRIX PROBLEM */
/*          FR,BR  OUTPUT: FORWARD AND BACKWARD RESIDUALS RESPECTIVELY */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCK IN MAIN SUBROUTINE */

    /* Parameter adjustments */
    --br;
    --fr;
    --rhs;
    --u;
    --ia;
    --ja;
    --a;

    /* Function Body */
    n = *nn;
    npl1 = n + 1;
    omm1 = itcom3_1.omega - 1.;
    if (itcom1_1.isym == 0) {
    goto L40;
    }

/*     *************** NON - SYMMETRIC SECTION ********************** */

/*     ... FORWARD SWEEP */

    i__1 = n;
    for (ii = 1; ii <= i__1; ++ii) {
    br[ii] = u[ii];
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    sum = rhs[ii];
    if (ibgn > iend) {
        goto L20;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        sum -= a[jj] * u[jajj];
/* L10: */
    }
L20:
    uii = itcom3_1.omega * sum - omm1 * u[ii];
    fr[ii] = uii - u[ii];
    u[ii] = uii;
/* L30: */
    }
    goto L90;

/*     ***************** SYMMETRIC SECTION ************************** */

/*     ... FORWARD SWEEP */

L40:
    i__1 = n;
    for (ii = 1; ii <= i__1; ++ii) {
    br[ii] = u[ii];
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    sum = rhs[ii];
    if (ibgn > iend) {
        goto L60;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        sum -= a[jj] * u[jajj];
/* L50: */
    }
L60:
    uii = itcom3_1.omega * sum - omm1 * u[ii];
    fr[ii] = uii - u[ii];
    u[ii] = uii;
    if (ibgn > iend) {
        goto L80;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        rhs[jajj] -= a[jj] * uii;
/* L70: */
    }
L80:
    ;
    }

/*     ... BACKWARD SWEEP */

L90:
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    ii = npl1 - i__;
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    uii = rhs[ii];
    if (ibgn > iend) {
        goto L110;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        uii -= a[jj] * u[jajj];
/* L100: */
    }
L110:
    u[ii] = itcom3_1.omega * uii - omm1 * u[ii];
    br[ii] = u[ii] - br[ii];
/* L120: */
    }

    return 0;

} /* pssor1_ */

/* Subroutine */ int pstop_(integer *n, doublereal *u, doublereal *dnrm, 
    doublereal *ccon, integer *iflag, logical *q1)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */

    /* Local variables */
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
        integer *);
    static doublereal uold, tl, tr, con;


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

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* ... SPECIFICATIONS FOR ARGUMENT SUBROUTINES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCK IN MAIN SUBROUTINE */

    /* Parameter adjustments */
    --u;

    /* Function Body */
    con = *ccon;
    itcom2_1.halt = FALSE_;

/*     SPECIAL PROCEDURE FOR ZEROTH ITERATION */

    if (itcom1_1.in >= 1) {
    goto L10;
    }
    *q1 = FALSE_;
    itcom3_1.udnm = 1.;
    itcom3_1.stptst = 1e3;
    if (*iflag <= 0) {
    return 0;
    }

/* ... TEST IF UDNM NEEDS TO BE RECOMPUTED */

L10:
    if (*q1) {
    goto L20;
    }
    if (itcom1_1.in > 5 && itcom1_1.in % 5 != 0) {
    goto L20;
    }
    uold = itcom3_1.udnm;
    itcom3_1.udnm = ddot_(n, &u[1], &c__1, &u[1], &c__1);
    if (itcom3_1.udnm == 0.) {
    itcom3_1.udnm = 1.;
    }
    if (itcom1_1.in > 5 && (d__1 = itcom3_1.udnm - uold, abs(d__1)) <= 
        itcom3_1.udnm * itcom3_1.zeta) {
    *q1 = TRUE_;
    }

/* ... COMPUTE STOPPING TEST */

L20:
    tr = sqrt(itcom3_1.udnm);
    tl = 1.;
    if (con == 1.) {
    goto L40;
    }
    if (*iflag == 2) {
    goto L30;
    }
    tl = sqrt(*dnrm);
    tr *= 1. - con;
    goto L40;
L30:
    tl = sqrt(*dnrm * 2.);
    tr *= 1. - con * con;
L40:
    itcom3_1.stptst = tl / tr;
    if (tl >= tr * itcom3_1.zeta) {
    return 0;
    }
    itcom2_1.halt = TRUE_;

    return 0;
} /* pstop_ */

doublereal pvtbv_(integer *n, integer *ia, integer *ja, doublereal *a, 
    doublereal *v)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal ret_val;

    /* Local variables */
    static integer jajj, ibgn, iend;
    static doublereal sumr;
    static integer ii, jj;
    static doublereal sum;


/*     THIS FUNCTION COMPUTES  (V**T)*A*V. */

/* ... PARAMETER LIST: */

/*          N      DIMENSION OF MATRIX */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          V      D.P. VECTOR OF LENGTH N */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCK IN MAIN SUBROUTINE */

    /* Parameter adjustments */
    --v;
    --ia;
    --ja;
    --a;

    /* Function Body */
    ret_val;
    sum = 0.;
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    if (ibgn > iend) {
        goto L20;
    }
    sumr = 0.;
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        sumr -= a[jj] * v[jajj];
/* L10: */
    }
    sum += v[ii] * sumr;
L20:
    ;
    }

    if (itcom1_1.isym == 0) {
    sum *= 2.;
    }
    ret_val = sum;

    return ret_val;
} /* pvtbv_ */

/* Subroutine */ int qsort_(integer *nn, integer *key, doublereal *data, 
    integer *error)
{
    /* Initialized data */

    static integer tiny = 9;
    static integer stklen = 30;

    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static logical done;
    static integer left, llen, rlen, lfrh2;
    static doublereal d__;
    static integer i__, j, k, n, v, stack[30], right;
    extern /* Subroutine */ int ivfill_(integer *, integer *, integer *);
    static integer jm1, ip1, top;


/*     ================================================================== */

/*     Q U I C K S O R T */

/*         IN THE STYLE OF THE CACM PAPER BY BOB SEDGEWICK, OCTOBER 1978 */

/*     INPUT: */
/*         N    -- NUMBER OF ELEMENTS TO BE SORTED (= NN) */
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


/*     ------------------------ */


    /* Parameter adjustments */
    --data;
    --key;

    /* Function Body */

/*     ----------------------------------- */

/*     ... PROGRAM IS A DIRECT TRANSLATION INTO FORTRAN OF SEDGEWICK^S */
/*         PROGRAM 2, WHICH IS NON-RECURSIVE, IGNORES FILES OF LENGTH */
/*         LESS THAN 'TINY' DURING PARTITIONING, AND USES MEDIAN OF THREE */
/*         PARTITIONING. */

    n = *nn;
    if (n == 1) {
    return 0;
    }
    if (n <= 0) {
    goto L240;
    }

    *error = 0;
    top = 1;
    left = 1;
    right = n;
    done = n <= tiny;

    if (done) {
    goto L150;
    }
    ivfill_(&stklen, stack, &c__0);

/*     =========================================================== */
/*     QUICKSORT -- PARTITION THE FILE UNTIL NO SUBFILE REMAINS OF */
/*     LENGTH GREATER THAN 'TINY' */
/*     =========================================================== */

/*     ... WHILE NOT DONE DO ... */

L10:
    if (done) {
    goto L150;
    }

/*         ... FIND MEDIAN OF LEFT, RIGHT AND MIDDLE ELEMENTS OF CURRENT */
/*             SUBFILE, WHICH IS  KEY(LEFT), ..., KEY(RIGHT) */

    lfrh2 = (left + right) / 2;
    k = key[lfrh2];
    d__ = data[lfrh2];
    key[lfrh2] = key[left];
    data[lfrh2] = data[left];
    key[left] = k;
    data[left] = d__;

    if (key[left + 1] <= key[right]) {
    goto L20;
    }
    k = key[left + 1];
    d__ = data[left + 1];
    key[left + 1] = key[right];
    data[left + 1] = data[right];
    key[right] = k;
    data[right] = d__;

L20:
    if (key[left] <= key[right]) {
    goto L30;
    }
    k = key[left];
    d__ = data[left];
    key[left] = key[right];
    data[left] = data[right];
    key[right] = k;
    data[right] = d__;

L30:
    if (key[left + 1] <= key[left]) {
    goto L40;
    }
    k = key[left + 1];
    d__ = data[left + 1];
    key[left + 1] = key[left];
    data[left + 1] = data[left];
    key[left] = k;
    data[left] = d__;

L40:
    v = key[left];

/*         ... V IS NOW THE MEDIAN VALUE OF THE THREE KEYS.  NOW MOVE */
/*             FROM THE LEFT AND RIGHT ENDS SIMULTANEOUSLY, EXCHANGING */
/*             KEYS AND DATA UNTIL ALL KEYS LESS THAN  V  ARE PACKED TO */
/*             THE LEFT, ALL KEYS LARGER THAN  V  ARE PACKED TO THE */
/*             RIGHT. */

    i__ = left + 1;
    j = right;

/*         LOOP */
/*             REPEAT I = I+1 UNTIL KEY(I) >= V; */
/*             REPEAT J = J-1 UNTIL KEY(J) <= V; */
/*         EXIT IF J < I; */
/*             << EXCHANGE KEYS I AND J >> */
/*         END */

L50:
L60:
    ++i__;
    if (key[i__] < v) {
    goto L60;
    }

L70:
    --j;
    if (key[j] > v) {
    goto L70;
    }

    if (j < i__) {
    goto L80;
    }
    k = key[i__];
    d__ = data[i__];
    key[i__] = key[j];
    data[i__] = data[j];
    key[j] = k;
    data[j] = d__;
    goto L50;

L80:
    k = key[left];
    d__ = data[left];
    key[left] = key[j];
    data[left] = data[j];
    key[j] = k;
    data[j] = d__;

/*         ... WE HAVE NOW PARTITIONED THE FILE INTO TWO SUBFILES, */
/*             ONE IS (LEFT ... J-1)  AND THE OTHER IS (I...RIGHT). */
/*             PROCESS THE SMALLER NEXT.  STACK THE LARGER ONE. */

    llen = j - left;
    rlen = right - i__ + 1;
    if (max(llen,rlen) > tiny) {
    goto L100;
    }

/*             ... BOTH SUBFILES ARE TINY, SO UNSTACK NEXT LARGER FILE */

    if (top == 1) {
    goto L90;
    }
    top += -2;
    left = stack[top - 1];
    right = stack[top];
    goto L10;

L90:
    done = TRUE_;

    goto L10;

/*             ... ELSE ONE OR BOTH SUBFILES ARE LARGE */

L100:
    if (min(llen,rlen) > tiny) {
    goto L120;
    }

/*             ... ONE SUBFILE IS SMALL, ONE LARGE.  IGNORE THE SMALL ONE */

    if (llen > rlen) {
    goto L110;
    }
    left = i__;
    goto L10;

L110:
    right = j - 1;

    goto L10;

/*         ... ELSE BOTH ARE LARGER THAN TINY.  ONE MUST BE STACKED. */

L120:
    if (top >= stklen) {
    goto L240;
    }
    if (llen > rlen) {
    goto L130;
    }
    stack[top - 1] = i__;
    stack[top] = right;
    right = j - 1;
    goto L140;

L130:
    stack[top - 1] = left;
    stack[top] = j - 1;
    left = i__;

L140:
    top += 2;

    goto L10;

/*     ------------------------------------------------------------ */
/*     INSERTION SORT THE ENTIRE FILE, WHICH CONSISTS OF A LIST */
/*     OF 'TINY' SUBFILES, LOCALLY OUT OF ORDER, GLOBALLY IN ORDER. */
/*     ------------------------------------------------------------ */

/*     ... FIRST, FIND LARGEST ELEMENT IN 'KEY' */

L150:
    i__ = n - 1;
/* Computing MAX */
    i__1 = 0, i__2 = n - tiny;
    left = max(i__1,i__2);
    k = key[n];
    j = n;

L160:
    if (i__ <= left) {
    goto L180;
    }
    if (key[i__] <= k) {
    goto L170;
    }
    k = key[i__];
    j = i__;

L170:
    --i__;
    goto L160;

L180:
    if (j == n) {
    goto L190;
    }

/*     ... LARGEST ELEMENT WILL BE IN  KEY(N) */

    key[j] = key[n];
    key[n] = k;
    d__ = data[n];
    data[n] = data[j];
    data[j] = d__;

/*     ... INSERTION SORT ... FOR I := N-1 STEP -1 TO 1 DO ... */

L190:
    i__ = n - 1;
    ip1 = n;

L200:
    if (key[i__] <= key[ip1]) {
    goto L220;
    }

/*             ... OUT OF ORDER ... MOVE UP TO CORRECT PLACE */

    k = key[i__];
    d__ = data[i__];
    j = ip1;
    jm1 = i__;

/*             ... REPEAT ... UNTIL 'CORRECT PLACE FOR K FOUND' */

L210:
    key[jm1] = key[j];
    data[jm1] = data[j];
    jm1 = j;
    ++j;
    if (key[j] < k) {
    goto L210;
    }

    key[jm1] = k;
    data[jm1] = d__;

L220:
    ip1 = i__;
    --i__;
    if (i__ > 0) {
    goto L200;
    }

L230:
    return 0;

L240:
    *error = 1;
    goto L230;

} /* qsort_ */

/* Subroutine */ int sbagn_(integer *n, integer *nz, integer *ia, integer *ja,
     doublereal *a, integer *iwork, integer *levell, integer *noutt, 
    integer *ierr)
{
    /* Format strings */
    static char fmt_10[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    IN ITPACK ROUTINE SBAGN   \002/\002 \002,\002 \
   IER = \002,i10/\002 \002,\002    NZ TOO SMALL - NO ROOM FOR NEW ENTRY\002)"
        ;

    /* System generated locals */
    integer i__1;

    /* Builtin functions */

    /* Local variables */
    static integer nadd, nout, i__, j, level, naddp1, np1, ier, ntn, nto, now;

    /* Fortran I/O blocks */
    static cilist io___667 = { 0, 0, 0, fmt_10, 0 };



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
/*           NOUT  OUTPUT FILE NUMBER (= NOUTT) */
/*           IER     ERROR FLAG (= IERR). POSSIBLE RETURNS ARE */
/*                      IER = 0, SUCCESSFUL COMPLETION */
/*                          = 703, NZ TOO SMALL - NO MORE */
/*                                 ELEMENTS CAN BE ADDED */

/* ... SPECIFICTIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* ... INITIALIZE LOCAL VARIABLES AND MAKE ERROR CHECK */

    /* Parameter adjustments */
    --iwork;
    --a;
    --ia;
    --ja;

    /* Function Body */
    now = ia[*n + 1] - 1;
    nadd = *nz - now;
    ier = 0;
    level = *levell;
    nout = *noutt;
    if (nadd <= 0) {
    ier = 703;
    }
    if (ier == 0) {
    goto L20;
    }
    if (level >= 0) {
    io___667.ciunit = nout;
    s_wsfe(&io___667);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L90;

/* ... SHIFT ELEMENTS OF A AND JA DOWN AND ADD ZERO FILL */

L20:
    nto = now;
    ntn = *nz;
    i__1 = now;
    for (i__ = 1; i__ <= i__1; ++i__) {
    ja[ntn] = ja[nto];
    a[ntn] = a[nto];
    --nto;
    --ntn;
/* L30: */
    }
    i__1 = nadd;
    for (i__ = 1; i__ <= i__1; ++i__) {
    ja[i__] = 0;
    a[i__] = 0.;
/* L40: */
    }

/* ... UPDATE IA TO REFLECT DOWNWARD SHIFT IN A AND JA */

    np1 = *n + 1;
    i__1 = np1;
    for (i__ = 1; i__ <= i__1; ++i__) {
    ia[i__] += nadd;
/* L50: */
    }

/* ... CREATE LINKED LIST */

    naddp1 = nadd + 1;
    i__1 = *nz;
    for (i__ = naddp1; i__ <= i__1; ++i__) {
    iwork[i__] = i__ + 1;
/* L60: */
    }
    i__1 = nadd;
    for (i__ = 1; i__ <= i__1; ++i__) {
    iwork[i__] = 0;
/* L70: */
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    j = ia[i__ + 1] - 1;
    iwork[j] = -i__;
/* L80: */
    }

/* ... INDICATE IN LAST POSITION OF IA HOW MANY SPACES */
/*     ARE LEFT IN A AND JA FOR ADDITION OF ELEMENTS */

    ia[*n + 1] = nadd;
    return 0;

/* ... ERROR RETURN */

L90:
    *ierr = ier;
    return 0;
} /* sbagn_ */

/* Subroutine */ int sbelm_(integer *nn, integer *ia, integer *ja, doublereal 
    *a, doublereal *rhs, integer *iw, doublereal *rw, doublereal *tol, 
    integer *isym, integer *level, integer *nout, integer *ier)
{
    /* Format strings */
    static char fmt_30[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    IN ITPACK ROUTINE SBELM   \002/\002 \002,\002 \
   DIAGONAL ELEMENT\002,i10,\002 NOT POSITIVE  \002/\002 \002,\002    CURREN\
T VALUE = \002,d15.8)";
    static char fmt_150[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    IN ITPACK ROUTINE SBELM   \002/\002 \002,\002\
    NO DIAGONAL ENTRY IN ROW  \002,i10)";

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */

    /* Local variables */
    static integer ibgn, iend, jjdi, icnt, n;
    static doublereal di;
    static integer ii, jj, kk;

    /* Fortran I/O blocks */
    static cilist io___683 = { 0, 0, 0, fmt_30, 0 };
    static cilist io___684 = { 0, 0, 0, fmt_150, 0 };



/* ... SBELM IS DESIGNED TO REMOVE ROWS AND COLUMNS OF THE MATRIX */
/* ... WHERE DABS(A(I,J))/A(I,I) .LE. TOL FOR J = 1 TO N AND A(I,I) */
/* ... .GT. 0. THIS IS TO TAKE CARE OF MATRICES ARISING */
/* ... FROM FINITE ELEMENT DISCRETIZATIONS OF PDE^S WITH DIRICHLET */
/* ... BOUNDARY CONDITIONS.  ANY SUCH ROWS AND CORRESPONDING COLUMNS */
/* ... ARE THEN SET TO THE IDENTITY AFTER CORRECTING RHS. */

/* ... PARAMETER LIST: */

/*          N      DIMENSION OF MATRIX (= NN) */
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

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


    /* Parameter adjustments */
    --rw;
    --iw;
    --rhs;
    --ia;
    --ja;
    --a;

    /* Function Body */
    n = *nn;

/*        IF (N .GE. 1) GO TO 10 */
/*           IER = 100 */
/*           RETURN */
/* 10     CONTINUE */

/* ... STORE THE LARGEST (DABSOLUTE VALUE) OFF DIAGONAL ENTRY FOR */
/* ... ROW II IN RW(II). */

    *ier = 0;
    icnt = 0;
    i__1 = n;
    for (ii = 1; ii <= i__1; ++ii) {
    rw[ii] = 0.;
    iw[ii] = 0;
/* L10: */
    }
    i__1 = n;
    for (ii = 1; ii <= i__1; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    if (ibgn > iend) {
        goto L140;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        kk = ja[jj];
        if (kk == ii) {
        goto L20;
        }
/* Computing MAX */
        d__2 = rw[ii], d__3 = (d__1 = a[jj], abs(d__1));
        rw[ii] = max(d__2,d__3);
        if (*isym != 0) {
        goto L20;
        }
/* Computing MAX */
        d__2 = rw[kk], d__3 = (d__1 = a[jj], abs(d__1));
        rw[kk] = max(d__2,d__3);
L20:
        ;
    }
    }

/* ... FOR II = 1 TO N FIND THE DIAGONAL ENTRY IN ROW II */

    i__2 = n;
    for (ii = 1; ii <= i__2; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    i__1 = iend;
    for (jj = ibgn; jj <= i__1; ++jj) {
        if (ja[jj] != ii) {
        goto L40;
        }
        di = a[jj];
        jjdi = jj;
        if (di > 0.) {
        goto L50;
        }
        *ier = 101;
        if (*level >= 0) {
        io___683.ciunit = *nout;
        s_wsfe(&io___683);
        do_fio(&c__1, (char *)&ii, (ftnlen)sizeof(integer));
        do_fio(&c__1, (char *)&di, (ftnlen)sizeof(doublereal));
        e_wsfe();
        }
        return 0;
L40:
        ;
    }
    goto L140;
L50:

/* ... CHECK THE SIZE OF THE LARGEST OFF DIAGONAL ELEMENT */
/* ... ( STORED IN RW(II) ) AGAINST THE DIAGONAL ELEMENT DII. */

    if (rw[ii] != 0.) {
        goto L60;
    }
    if (1. / di <= *tol) {
        goto L70;
    }
    goto L80;
L60:
    if (rw[ii] / di > *tol) {
        goto L80;
    }

/* ... THE OFF DIAGONAL ELEMENTS ARE SMALL COMPARED TO THE DIAGONAL */
/* ... THEREFORE MARK IT FOR ELIMINATION AND PERFORM INITIAL */
/* ... PROCESSING */

L70:
    ++icnt;
    iw[ii] = ii;
    rw[ii] = di;
    a[jjdi] = 1.;
    rhs[ii] /= di;

L80:
    ;
    }

/* ... ELIMINATE THE ROWS AND COLUMNS INDICATED BY THE NONZERO */
/* ... ENTRIES IN IW.  THERE ARE ICNT OF THEM */

    if (icnt == 0) {
    goto L130;
    }

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

    i__2 = n;
    for (ii = 1; ii <= i__2; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    if (iw[ii] == 0) {
        goto L100;
    }

/* ... THE II-TH ROW IS TO BE ELIMINATED */

    i__1 = iend;
    for (jj = ibgn; jj <= i__1; ++jj) {
        kk = ja[jj];
        if (kk == ii) {
        goto L90;
        }
        if (iw[kk] == 0 && *isym == 0) {
        rhs[kk] -= a[jj] * rhs[ii];
        }
        a[jj] = 0.;
L90:
        ;
    }
    goto L120;

/* ... THE II-TH ROW IS KEPT.  CHECK THE OFF-DIAGONAL ENTRIES */

L100:
    i__1 = iend;
    for (jj = ibgn; jj <= i__1; ++jj) {
        kk = ja[jj];
        if (kk == ii || iw[kk] == 0) {
        goto L110;
        }
        rhs[ii] -= a[jj] * rhs[kk];
        a[jj] = 0.;
L110:
        ;
    }

L120:
    ;
    }

L130:
    return 0;

/* ... ERROR TRAPS -- NO DIAGONAL ENTRY IN ROW II (ROW MAY BE EMPTY). */

L140:
    *ier = 102;
    if (*level >= 0) {
    io___684.ciunit = *nout;
    s_wsfe(&io___684);
    do_fio(&c__1, (char *)&ii, (ftnlen)sizeof(integer));
    e_wsfe();
    }

    return 0;
} /* sbelm_ */

/* Subroutine */ int sbend_(integer *n, integer *nz, integer *ia, integer *ja,
     doublereal *a, integer *iwork)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer ideg, link, next, i__, l, hlink, mhlink, ohlink, nulink, 
        maxtop, jaj;
    static doublereal val;
    static integer top;


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



/* *********************************************************************** */

/* ... INITIALIZATION */

/* ...... THE VARIABLES NEXT AND TOP RESPECTIVELY POINT TO THE */
/*        NEXT AVAILABLE ENTRY FOR THE FINAL DATA STRUCTURE AND */
/*        THE TOP OF THE REMAINDER OF THE LINKED LISTS. */

    /* Parameter adjustments */
    --iwork;
    --a;
    --ja;
    --ia;

    /* Function Body */
    next = 1;
    top = ia[*n + 1] + 1;
    maxtop = *nz - ia[*n + 1] + 1;

/* *********************************************************************** */

/* ... CONVERT EACH ROW INTO FINAL FORM */

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    ideg = 0;
    nulink = ia[i__];

/* ... LOOP OVER EACH NODE IN THE LINKED LIST OF ROW I */

L10:
    link = nulink;
    if (link <= 0) {
        goto L80;
    }
    nulink = iwork[link];
    jaj = ja[link];
    val = a[link];

/* ... CHECK TO SEE IF A COLLISION BETWEEN THE LINKED LISTS */
/*     AND THE FINAL FORM HAS OCCURRED. */

    if (next >= top && link != top) {
        goto L20;
    }

/* ... COLLISION HAS NOT OCCURRED.  FREE THE SPACE FOR THE TRIPLE */
/*     (JA(LINK), A(LINK), IWORK(LINK)) */

    ja[link] = 0;
    a[link] = 0.;
    iwork[link] = 0;

/* ... SPECIAL CASE TO MOVE  TOP  DOWN IF LINK .EQ. TOP */

    if (link == top) {
        goto L60;
    }
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
    ia[i__] = link;
    hlink = top;

L30:
    hlink = iwork[hlink];
    if (hlink > 0) {
        goto L30;
    }

/* ...... NOW FOLLOW THE LINKED LIST BACK TO TOP KEEPING TRACK */
/*        OF THE OLD LINK. */

/* ......... SPECIAL CASE IF IA(-HLINK) = TOP */

    mhlink = -hlink;
    if (ia[mhlink] != top) {
        goto L40;
    }

    iwork[link] = iwork[top];
    ja[link] = ja[top];
    a[link] = a[top];
    ia[mhlink] = link;
    if (nulink == top) {
        nulink = link;
    }
    goto L60;

/* ......... USUAL CASE. */

L40:
    hlink = ia[mhlink];
L50:
    ohlink = hlink;
    hlink = iwork[ohlink];
    if (hlink != top) {
        goto L50;
    }

    iwork[link] = iwork[top];
    ja[link] = ja[top];
    a[link] = a[top];
    if (ohlink != link) {
        iwork[ohlink] = link;
    }
    if (nulink == top) {
        nulink = link;
    }

/* ... COLLAPSE TOP OF LINK LIST BY AS MUCH AS POSSIBLE */

L60:
    ++top;
    if (top >= maxtop) {
        goto L70;
    }
    if (iwork[top] != 0) {
        goto L70;
    }
    goto L60;

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
    ia[i__] = ideg;

/* L90: */
    }

/* *********************************************************************** */

/* ... FINALIZE THE DATA STRUCTURE BY BUILDING THE FINAL VERSION OF */
/*     IA. */

    l = ia[1] + 1;
    ia[1] = 1;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    ideg = ia[i__ + 1];
    ia[i__ + 1] = l;
    l += ideg;
/* L100: */
    }

/* ... FINAL IA, JA, A DATA STRUCTURE BUILT. */

    return 0;
} /* sbend_ */

/* Subroutine */ int sbini_(integer *n, integer *nz, integer *ia, integer *ja,
     doublereal *a, integer *iwork)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;
    extern /* Subroutine */ int vfill_(integer *, doublereal *, doublereal *),
         ivfill_(integer *, integer *, integer *);


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


/* *********************************************************************** */

    /* Parameter adjustments */
    --iwork;
    --a;
    --ja;
    --ia;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    ia[i__] = -i__;
/* L10: */
    }
    ia[*n + 1] = *nz;

    ivfill_(nz, &ja[1], &c__0);
    ivfill_(nz, &iwork[1], &c__0);
    vfill_(nz, &a[1], &c_b21);

    return 0;
} /* sbini_ */

/* Subroutine */ int sbsij_(integer *n, integer *, integer *ia, integer *ja,
     doublereal *a, integer *iwork, integer *ii, integer *jj, doublereal *
    vall, integer *mode, integer *levell, integer *noutt, integer *ierr)
{
    /* Format strings */
    static char fmt_10[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    IN ITPACK ROUTINE SBSIJ   \002/\002 \002,\002 \
   IER = \002,i10/\002 \002,\002    ( \002,i10,\002 , \002,i10,\002 )\002\
/\002 \002,\002    IMPROPER VALUE FOR I OR J \002)";
    static char fmt_50[] = "(\0020\002,\002*** W A R N I N G ************\
\002/\0020\002,\002    IN ITPACK ROUTINE SBSIJ   \002/\002 \002,\002    IER \
= \002,i10/\002 \002,\002    ( \002,i10,\002 , \002,i10,\002 )\002/\002 \002,\
\002    ENTRY ALREADY SET AND IS LEFT AS \002,d15.8)";
    static char fmt_70[] = "(\0020\002,\002*** W A R N I N G ************\
\002/\0020\002,\002    IN ITPACK ROUTINE SBSIJ   \002/\002 \002,\002    IER \
= \002,i10/\002 \002,\002    ( \002,i10,\002 , \002,i10,\002 )\002/\002 \002,\
\002    ENTRY ALREADY SET - CURRENT VALUE OF\002,d15.8/\002 \002,\002       \
                         RESET TO\002,d15.8)";
    static char fmt_90[] = "(\0020\002,\002*** W A R N I N G ************\
\002/\0020\002,\002    IN ITPACK ROUTINE SBSIJ   \002/\002 \002,\002    IER \
= \002,i10/\002 \002,\002    ( \002,i10,\002 , \002,i10,\002 )\002/\002 \002,\
\002    ENTRY ALREADY SET - CURRENT VALUE OF\002,d15.8/\002 \002,\002       \
                         RESET TO\002,d15.8)";
    static char fmt_120[] = "(\0020\002,\002*** F A T A L     E R R O R ****\
********\002/\0020\002,\002    IN ITPACK ROUTINE SBSIJ   \002/\002 \002,\002\
    IER = \002,i10/\002 \002,\002    NZ TOO SMALL - NO ROOM FOR NEW ENTRY\
\002)";

    /* Builtin functions */

    /* Local variables */
    static integer link;
    static doublereal temp;
    static integer next, nout, i__, j, level, ier;
    static doublereal val;
    static integer npl1;

    /* Fortran I/O blocks */
    static cilist io___705 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___709 = { 0, 0, 0, fmt_50, 0 };
    static cilist io___710 = { 0, 0, 0, fmt_70, 0 };
    static cilist io___712 = { 0, 0, 0, fmt_90, 0 };
    static cilist io___713 = { 0, 0, 0, fmt_120, 0 };



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

/*     NOUT  OUTPUT FILE NUMBER (= NOUTT) */

/*     LEVEL   OUTPUT FILE SWITCH (= LEVELL) */
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



/* *********************************************************************** */

/* ... CHECK THE VALIDITY OF THE (I,J) ENTRY */

    /* Parameter adjustments */
    --iwork;
    --a;
    --ja;
    --ia;

    /* Function Body */
    i__ = *ii;
    j = *jj;
    val = *vall;
    level = *levell;
    nout = *noutt;
    ier = 0;
    if (i__ <= 0 || i__ > *n) {
    ier = 701;
    }
    if (j <= 0 || j > *n) {
    ier = 701;
    }
    if (ier == 0) {
    goto L20;
    }
    if (level >= 0) {
    io___705.ciunit = nout;
    s_wsfe(&io___705);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&j, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L130;

/* ... TRAVERSE THE LINK LIST POINTED TO BY IA(I) UNTIL EITHER */
/* ... THE J ENTRY OR THE END OF THE LIST HAS BEEN FOUND. */

L20:
    npl1 = *n + 1;
    link = ia[i__];

/* ...... SPECIAL CASE FOR THE FIRST ENTRY IN THE ROW */

    if (link > 0) {
    goto L30;
    }
    next = ia[npl1];
    if (next < 1) {
    goto L110;
    }

    ia[i__] = next;
    ja[next] = j;
    a[next] = val;
    iwork[next] = -i__;
    ia[npl1] = next - 1;
    goto L130;

/* ... FOLLOW THE LINK LIST UNTIL J OR THE END OF THE LIST IS FOUND */

L30:
    if (ja[link] == j) {
    goto L40;
    }
    if (iwork[link] <= 0) {
    goto L100;
    }
    link = iwork[link];
    goto L30;

/* : */
/* ... ENTRY (I,J) ALREADY HAS BEEN SET.  RESET VALUE DEPENDING ON MODE */

L40:
    ier = 700;
    if (*mode >= 0) {
    goto L60;
    }
    if (level >= 1) {
    io___709.ciunit = nout;
    s_wsfe(&io___709);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&j, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&a[link], (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    goto L130;
L60:
    if (*mode >= 1) {
    goto L80;
    }
    if (level >= 1) {
    io___710.ciunit = nout;
    s_wsfe(&io___710);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&j, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&a[link], (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&val, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    a[link] = val;
    goto L130;
L80:
    temp = a[link] + val;
    if (level >= 1) {
    io___712.ciunit = nout;
    s_wsfe(&io___712);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&j, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&a[link], (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&temp, (ftnlen)sizeof(doublereal));
    e_wsfe();
    }
    a[link] = temp;
    goto L130;

/* ... ENTRY (I,J) HAS NOT BEEN SET.  ENTER IT INTO THE LINKED LIST */

L100:
    next = ia[npl1];
    if (next < 1) {
    goto L110;
    }

    iwork[link] = next;
    ja[next] = j;
    a[next] = val;
    iwork[next] = -i__;
    ia[npl1] = next - 1;
    goto L130;

/* *********************************************************************** */

/* ... ERROR TRAP FOR NO ROOM REMAINING */

L110:
    ier = 702;
    if (level >= 0) {
    io___713.ciunit = nout;
    s_wsfe(&io___713);
    do_fio(&c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsfe();
    }

L130:
    *ierr = ier;
    return 0;
} /* sbsij_ */

/* Subroutine */ int scal_(integer *nn, integer *ia, integer *ja, doublereal *
    a, doublereal *rhs, doublereal *u, doublereal *d__, integer *level, 
    integer *nout, integer *ier)
{
    /* Format strings */
    static char fmt_10[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    IN ITPACK ROUTINE SCAL    \002/\002 \002,\002 \
   DIAGONAL ENTRY IN ROW \002,i10,\002 NEGATIVE\002)";
    static char fmt_30[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    IN ITPACK ROUTINE SCAL    \002/\002 \002,\002 \
   DIAGONAL ENTRY IN ROW \002,i10,\002 IS ZERO\002)";
    static char fmt_60[] = "(\0020\002,\002*** F A T A L     E R R O R *****\
*******\002/\0020\002,\002    IN ITPACK ROUTINE SCAL    \002/\002 \002,\002 \
   NO DIAGONAL ENTRY IN ROW\002,i10)";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */

    /* Local variables */
    static integer jadd, jajj, ibgn, iend, jjpi, i__, j, n;
    static doublereal di;
    static integer ii, jj, im1, np1;

    /* Fortran I/O blocks */
    static cilist io___720 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___721 = { 0, 0, 0, fmt_30, 0 };
    static cilist io___722 = { 0, 0, 0, fmt_60, 0 };



/* ... ORIGINAL MATRIX IS SCALED TO A UNIT DIAGONAL MATRIX.  RHS */
/* ... AND U ARE SCALED ACCORDINGLY.  THE MATRIX IS THEN SPLIT AND */
/* ... IA, JA, AND A RESHUFFLED. */

/* ... PARAMETER LIST: */

/*          N      DIMENSION OF MATRIX (= NN) */
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

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* ... EXTRACT SQUARE ROOT OF THE DIAGONAL OUT OF A AND SCALE U AND RHS */

    /* Parameter adjustments */
    --d__;
    --u;
    --rhs;
    --ia;
    --ja;
    --a;

    /* Function Body */
    n = *nn;
    *ier = 0;
    i__1 = n;
    for (ii = 1; ii <= i__1; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    if (ibgn > iend) {
        goto L50;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        if (ja[jj] != ii) {
        goto L40;
        }
        di = a[jj];
        if (di > 0.) {
        goto L70;
        }
        if (di == 0.) {
        goto L20;
        }
        *ier = 401;
        if (*level >= 0) {
        io___720.ciunit = *nout;
        s_wsfe(&io___720);
        do_fio(&c__1, (char *)&ii, (ftnlen)sizeof(integer));
        e_wsfe();
        }
        return 0;
L20:
        *ier = 401;
        if (*level >= 0) {
        io___721.ciunit = *nout;
        s_wsfe(&io___721);
        e_wsfe();
        }
        return 0;
L40:
        ;
    }
L50:
    *ier = 402;
    if (*level >= 0) {
        io___722.ciunit = *nout;
        s_wsfe(&io___722);
        do_fio(&c__1, (char *)&ii, (ftnlen)sizeof(integer));
        e_wsfe();
    }
    return 0;

L70:
    di = sqrt((abs(di)));
    rhs[ii] /= di;
    u[ii] *= di;
    d__[ii] = di;
/* L80: */
    }

/* ... SHIFT MATRIX TO ELIMINATE DIAGONAL ENTRIES */

    if (n == 1) {
    goto L110;
    }
    np1 = n + 1;
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    im1 = i__ - 1;
    ii = np1 - i__;
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    jadd = ibgn + iend;
    i__2 = iend;
    for (j = ibgn; j <= i__2; ++j) {
        jj = jadd - j;
        jjpi = jj + im1;
        if (ja[jj] == ii) {
        im1 = i__;
        }
        a[jjpi] = a[jj];
        ja[jjpi] = ja[jj];
/* L90: */
    }
    ia[ii + 1] = ia[ii + 1] + i__ - 1;
/* L100: */
    }
L110:
    ia[1] += n;

/* ... SCALE SHIFTED MATRIX AND STORE D ARRAY IN FIRST N ENTRIES OF A */

    i__1 = n;
    for (ii = 1; ii <= i__1; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    di = d__[ii];
    if (ibgn > iend) {
        goto L130;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        a[jj] /= di * d__[jajj];
/* L120: */
    }
L130:
    a[ii] = di;
/* L140: */
    }

    return 0;
} /* scal_ */

/* Subroutine */ int sum3_(integer *n, doublereal *c1, doublereal *x1, 
    doublereal *c2, doublereal *x2, doublereal *c3, doublereal *x3)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;


/* ... COMPUTES X3 = C1*X1 + C2*X2 + C3*X3 */

/* ... PARAMETER LIST: */

/*          N        INTEGER LENGTH OF VECTORS X1, X2, X3 */
/*          C1,C2,C3 D.P. CONSTANTS */
/*          X1,X2,X3 D.P. VECTORS SUCH THAT */
/*                   X3(I) = C1*X1(I) + C2*X2(I) + C3*X3(I) */
/*                   X3(I) = C1*X1(I) + C2*X2(I)  IF C3 = 0. */

/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */


    /* Parameter adjustments */
    --x3;
    --x2;
    --x1;

    /* Function Body */
    if (*n <= 0) {
    return 0;
    }
    if (abs(*c3) == 0.) {
    goto L20;
    }

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    x3[i__] = *c1 * x1[i__] + *c2 * x2[i__] + *c3 * x3[i__];
/* L10: */
    }
    return 0;

/* ... COMPUTE X3 = C1*X1 + C2*X2 */

L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
    x3[i__] = *c1 * x1[i__] + *c2 * x2[i__];
/* L30: */
    }

    return 0;
} /* sum3_ */

doublereal tau_(integer *ii)
{
    /* Initialized data */

    static doublereal t[8] = { 1.5,1.8,1.85,1.9,1.94,1.96,1.975,1.985 };

    /* System generated locals */
    doublereal ret_val;


/* ... THIS SUBROUTINE SETS TAU(II) FOR THE SOR METHOD. */

/* ... PARAMETER LIST: */

/*          II     NUMBER OF TIMES PARAMETERS HAVE BEEN CHANGED */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */



    ret_val = 1.992;
    if (*ii <= 8) {
    ret_val = t[*ii - 1];
    }

    return ret_val;
} /* tau_ */

doublereal timer_(real *)
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    static real total, tarray[2];


/* ... TIMER IS A ROUTINE TO RETURN THE EXECUTION TIME IN */
/* ... SECONDS. */

/* ... PARAMETERS -- */

/*          TIMDMY   DUMMY ARGUMENT */


/* ********************************************* */
/* **                                         ** */
/* **   THIS ROUTINE IS NOT PORTABLE.         ** */
/* **                                         ** */
/* ********************************************* */


/* ... CRAY Y-MP. */

/*     TIMER = SECOND () */

/* ... UNIX ETIME FACILITY. */

    total = etime_(tarray);
    ret_val = total;

/* ... IBM RISC SYSTEM/6000. */

/*     TIMER = FLOAT(MCLOCK())/100.0 */

    return ret_val;
} /* timer_ */

logical tstchg_(integer *ibmth)
{
    /* System generated locals */
    doublereal d__1;
    logical ret_val;

    /* Builtin functions */

    /* Local variables */
    static integer ip;


/*     THIS FUNCTION PERFORMS A TEST TO DETERMINE IF PARAMETERS */
/*     SHOULD BE CHANGED FOR SEMI-ITERATION ACCELERATED METHODS. */

/* ... PARAMETER LIST: */

/*          IBMTH  INDICATOR OF BASIC METHOD BEING ACCELERATED BY SI */
/*                      IBMTH = 1,   JACOBI */
/*                            = 2,   REDUCED SYSTEM */
/*                            = 3,   SSOR */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCKS IN MAIN SUBROUTINE */

    ip = itcom1_1.in - itcom1_1.is;
    if (*ibmth == 2) {
    ip <<= 1;
    }

    if (itcom1_1.in == 0) {
    goto L10;
    }
    if (ip < 3) {
    goto L20;
    }

    itcom3_1.qa = sqrt((d__1 = itcom3_1.delnnm / itcom3_1.delsnm, abs(d__1)));
    itcom3_1.qt = sqrt((d__1 = pow_di(&itcom3_1.rrr, &ip), abs(d__1))) * 2. / 
        (pow_di(&itcom3_1.rrr, &ip) + 1.);
    if (itcom3_1.qa >= 1. || itcom3_1.qa < pow_dd(&itcom3_1.qt, &itcom3_1.ff))
         {
    goto L20;
    }

/* ... TEST PASSES -- CHANGE PARAMETERS */

L10:
    ret_val = TRUE_;
    return ret_val;

/* ... TEST FAILS -- DO NOT CHANGE PARAMETERS */

L20:
    ret_val = FALSE_;
    return ret_val;

} /* tstchg_ */

/* Subroutine */ int unscal_(integer *n, integer *ia, integer *ja, doublereal 
    *a, doublereal *rhs, doublereal *u, doublereal *d__)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Local variables */
    static integer jajj, ibgn, iend, jjpi, inew;
    static doublereal di;
    static integer ii, jj, is;


/* ... THIS SUBROUTINE REVERSES THE PROCESS OF SCAL. */

/* ... PARAMETER LIST: */

/*          N      DIMENSION OF MATRIX */
/*          IA,JA  INTEGER ARRAYS OF SPARSE MATRIX REPRESENTATION */
/*          A      D.P. ARRAY OF SPARSE MATRIX REPRESENTATION */
/*          RHS    RIGHT HAND SIDE OF MATRIX PROBLEM */
/*          U      LATEST ESTIMATE OF SOLUTION */
/*          D      VECTOR CONTAINING THE SQUARE ROOTS */
/*                    OF THE DIAGONAL ENTRIES */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


/* ... EXTRACT DIAGONAL FROM SCALED A AND UNSCALE U AND RHS */

    /* Parameter adjustments */
    --d__;
    --u;
    --rhs;
    --ia;
    --ja;
    --a;

    /* Function Body */
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
    di = a[ii];
    u[ii] /= di;
    rhs[ii] *= di;
    d__[ii] = di;
/* L10: */
    }

/* ... UNSCALE A */

    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    if (ibgn > iend) {
        goto L30;
    }
    di = d__[ii];
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jajj = ja[jj];
        a[jj] = a[jj] * di * d__[jajj];
/* L20: */
    }
L30:
    ;
    }

/* ... INSERT DIAGONAL BACK INTO A */

    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
    ibgn = ia[ii];
    iend = ia[ii + 1] - 1;
    is = *n - ii;
    inew = ibgn - is - 1;
/* Computing 2nd power */
    d__1 = d__[ii];
    a[inew] = d__1 * d__1;
    ja[inew] = ii;
    if (is == 0 || ibgn > iend) {
        goto L50;
    }
    i__2 = iend;
    for (jj = ibgn; jj <= i__2; ++jj) {
        jjpi = jj - is;
        a[jjpi] = a[jj];
        ja[jjpi] = ja[jj];
/* L40: */
    }
L50:
    ia[ii] = inew;
/* L60: */
    }

    return 0;
} /* unscal_ */

/* Subroutine */ int vevmw_(integer *n, doublereal *v, doublereal *w)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, m, mp1;


/* ... VEVMW COMPUTES V = V - W */

/* ... PARAMETER LIST: */

/*          N      INTEGER LENGTH OF VECTORS V AND W */
/*          V      D.P. VECTOR */
/*          W      D.P. VECTOR SUCH THAT   V(I) = V(I) - W(I) */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


    /* Parameter adjustments */
    --w;
    --v;

    /* Function Body */
    if (*n <= 0) {
    return 0;
    }
    m = *n % 4;

    if (m == 0) {
    goto L20;
    }
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
    v[i__] -= w[i__];
/* L10: */
    }
    if (*n < 4) {
    return 0;
    }

L20:
    mp1 = m + 1;
    i__1 = *n;
    for (i__ = mp1; i__ <= i__1; i__ += 4) {
    v[i__] -= w[i__];
    v[i__ + 1] -= w[i__ + 1];
    v[i__ + 2] -= w[i__ + 2];
    v[i__ + 3] -= w[i__ + 3];
/* L30: */
    }
    return 0;

} /* vevmw_ */

/* Subroutine */ int vevpw_(integer *n, doublereal *v, doublereal *w)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, m, mp1;


/* ... VPW COMPUTES    V = V + W */

/* ... PARAMETER LIST: */

/*          N      LENGTH OF VECTORS V AND W */
/*          V      D.P. VECTOR */
/*          W      D.P. VECTOR SUCH THAT   V(I) = V(I) + W(I) */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


    /* Parameter adjustments */
    --w;
    --v;

    /* Function Body */
    if (*n <= 0) {
    return 0;
    }

    m = *n % 4;
    if (m == 0) {
    goto L20;
    }
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
    v[i__] += w[i__];
/* L10: */
    }
    if (*n < 4) {
    return 0;
    }

L20:
    mp1 = m + 1;
    i__1 = *n;
    for (i__ = mp1; i__ <= i__1; i__ += 4) {
    v[i__] += w[i__];
    v[i__ + 1] += w[i__ + 1];
    v[i__ + 2] += w[i__ + 2];
    v[i__ + 3] += w[i__ + 3];
/* L30: */
    }

    return 0;
} /* vevpw_ */

/* Subroutine */ int vfill_(integer *n, doublereal *v, doublereal *val)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, m, mp1;


/*     FILLS A VECTOR, V, WITH A CONSTANT VALUE, VAL. */

/* ... PARAMETER LIST: */

/*          N      INTEGER LENGTH OF VECTOR V */
/*          V      D.P. VECTOR */
/*          VAL    D.P. CONSTANT THAT FILLS FIRST N LOCATIONS OF V */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


    /* Parameter adjustments */
    --v;

    /* Function Body */
    if (*n <= 0) {
    return 0;
    }

/*     CLEAN UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 10 */

    m = *n % 10;
    if (m == 0) {
    goto L20;
    }
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
    v[i__] = *val;
/* L10: */
    }
    if (*n < 10) {
    return 0;
    }

L20:
    mp1 = m + 1;
    i__1 = *n;
    for (i__ = mp1; i__ <= i__1; i__ += 10) {
    v[i__] = *val;
    v[i__ + 1] = *val;
    v[i__ + 2] = *val;
    v[i__ + 3] = *val;
    v[i__ + 4] = *val;
    v[i__ + 5] = *val;
    v[i__ + 6] = *val;
    v[i__ + 7] = *val;
    v[i__ + 8] = *val;
    v[i__ + 9] = *val;
/* L30: */
    }

    return 0;
} /* vfill_ */

/* Subroutine */ int vout_(integer *n, doublereal *v, integer *iswt, integer *
    noutt)
{
    /* Format strings */
    static char fmt_10[] = "(//5x,\002RESIDUAL VECTOR\002)";
    static char fmt_20[] = "(//5x,\002SOLUTION VECTOR\002)";
    static char fmt_30[] = "(10x,8i15)";
    static char fmt_40[] = "(10x,120(\002-\002)/)";
    static char fmt_50[] = "(4x,i5,\002+  \002,8d15.5)";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */

    /* Local variables */
    static integer nout, i__, j, k, kupper, jm1;

    /* Fortran I/O blocks */
    static cilist io___755 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___756 = { 0, 0, 0, fmt_20, 0 };
    static cilist io___757 = { 0, 0, 0, fmt_30, 0 };
    static cilist io___759 = { 0, 0, 0, fmt_40, 0 };
    static cilist io___762 = { 0, 0, 0, fmt_50, 0 };



/*     THIS SUBROUTINE EFFECTS PRINTING OF RESIDUAL AND SOLUTION */
/*     VECTORS - CALLED FROM PERROR */

/* ... PARAMETER LIST: */

/*          V      VECTOR OF LENGTH N */
/*          ISWT   LABELLING INFORMATION */
/*          NOUT OUTPUT DEVICE NUMBER (= NOUTT) */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


    /* Parameter adjustments */
    --v;

    /* Function Body */
    nout = *noutt;

/*        IF (N .LE. 0) RETURN */

    kupper = min(*n,8);
    if (*iswt == 1) {
    io___755.ciunit = nout;
    s_wsfe(&io___755);
    e_wsfe();
    }
    if (*iswt == 2) {
    io___756.ciunit = nout;
    s_wsfe(&io___756);
    e_wsfe();
    }
    io___757.ciunit = nout;
    s_wsfe(&io___757);
    i__1 = kupper;
    for (i__ = 1; i__ <= i__1; ++i__) {
    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
    }
    e_wsfe();
    io___759.ciunit = nout;
    s_wsfe(&io___759);
    e_wsfe();

    i__1 = *n;
    for (j = 1; j <= i__1; j += 8) {
/* Computing MIN */
    i__2 = j + 7;
    kupper = min(i__2,*n);
    jm1 = j - 1;
    io___762.ciunit = nout;
    s_wsfe(&io___762);
    do_fio(&c__1, (char *)&jm1, (ftnlen)sizeof(integer));
    i__2 = kupper;
    for (k = j; k <= i__2; ++k) {
        do_fio(&c__1, (char *)&v[k], (ftnlen)sizeof(doublereal));
    }
    e_wsfe();
/* L60: */
    }

    return 0;
} /* vout_ */

/* Subroutine */ int wevmw_(integer *n, doublereal *v, doublereal *w)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, m, mp1;


/* ... WEVMW COMPUTES W = V - W */

/* ... PARAMETER LIST: */

/*          N      INTEGER LENGTH OF VECTORS V AND W */
/*          V      D.P. VECTOR */
/*          W      D.P. VECTOR SUCH THAT   W(I) = V(I) - W(I) */

/* ... SPECIFICATIONS FOR ARGUMENTS */


/* ... SPECIFICATIONS FOR LOCAL VARIABLES */


    /* Parameter adjustments */
    --w;
    --v;

    /* Function Body */
    if (*n <= 0) {
    return 0;
    }
    m = *n % 4;
    if (m == 0) {
    goto L20;
    }
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
    w[i__] = v[i__] - w[i__];
/* L10: */
    }
    if (*n < 4) {
    return 0;
    }

L20:
    mp1 = m + 1;
    i__1 = *n;
    for (i__ = mp1; i__ <= i__1; i__ += 4) {
    w[i__] = v[i__] - w[i__];
    w[i__ + 1] = v[i__ + 1] - w[i__ + 1];
    w[i__ + 2] = v[i__ + 2] - w[i__ + 2];
    w[i__ + 3] = v[i__ + 3] - w[i__ + 3];
/* L30: */
    }

    return 0;
} /* wevmw_ */

/* Subroutine */ int zbrent_(integer *n, doublereal *tri, doublereal *eps, 
    integer *nsig, doublereal *aa, doublereal *bb, integer *maxfnn, 
    integer *ier)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal half = .5;
    static doublereal one = 1.;
    static doublereal three = 3.;
    static doublereal ten = 10.;

    /* Format strings */
    static char fmt_100[] = "(\0020\002,\002*** W A R N I N G ***********\
*\002/\0020\002,\002    IN ITPACK ROUTINE ZBRENT  \002/\002 \002,\002    ALG\
ORITHM FAILED TO CONVERGE   \002/\002 \002,\002    IN\002,i6,\002 ITERATIONS \
\002)";
    static char fmt_120[] = "(\0020\002,\002*** W A R N I N G ***********\
*\002/\0020\002,\002    IN ITPACK ROUTINE ZBRENT  \002/\002 \002,\002    F(A\
) AND F(B) HAVE SAME SIGN   \002)";

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */

    /* Local variables */
    static doublereal rone, temp, a, b, c__, d__, e, p, q, s, t, r__;
    static integer maxfn;
    static doublereal fa, fb, fc;
    static integer ic;
    static doublereal rm;
    extern doublereal determ_(integer *, doublereal *, doublereal *);
    static doublereal tol;

    /* Fortran I/O blocks */
    static cilist io___791 = { 0, 0, 0, fmt_100, 0 };
    static cilist io___792 = { 0, 0, 0, fmt_120, 0 };



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

/* *** BEGIN: ITPACK COMMON */




/* *** END  : ITPACK COMMON */

/*     DESCRIPTION OF VARIABLES IN COMMON BLOCK IN MAIN SUBROUTINE */

/*                                  SPECIFICATIONS FOR ARGUMENTS */


/*                                  SPECIFICATIONS FOR LOCAL VARIABLES */

    /* Parameter adjustments */
    tri -= 3;

    /* Function Body */

/*                                  FIRST EXECUTABLE STATEMENT */

    a = *aa;
    b = *bb;
    maxfn = *maxfnn;
    *ier = 0;
    i__1 = -(*nsig);
    t = pow_di(&ten, &i__1);
    ic = 2;
    fa = determ_(n, &tri[3], &a);
    fb = determ_(n, &tri[3], &b);
    s = b;

/*                                  TEST FOR SAME SIGN */

    if (fa * fb > zero) {
    goto L110;
    }
L10:
    c__ = a;
    fc = fa;
    d__ = b - c__;
    e = d__;
L20:
    if (abs(fc) >= abs(fb)) {
    goto L30;
    }
    a = b;
    b = c__;
    c__ = a;
    fa = fb;
    fb = fc;
    fc = fa;
L30:
/* Computing MAX */
    d__1 = abs(b);
    tol = t * max(d__1,.1);
    rm = (c__ - b) * half;

/*                                  TEST FOR FIRST CONVERGENCE CRITERIA */

    if (abs(fb) <= *eps) {
    goto L80;
    }

/*                                  TEST FOR SECOND CONVERGENCE CRITERIA */

    if ((d__1 = c__ - b, abs(d__1)) <= tol) {
    goto L80;
    }

/*                                  CHECK EVALUATION COUNTER */

    if (ic >= maxfn) {
    goto L90;
    }

/*                                  IS BISECTION FORCED */

    if (abs(e) < tol) {
    goto L60;
    }
    if (abs(fa) <= abs(fb)) {
    goto L60;
    }
    s = fb / fa;
    if (a != c__) {
    goto L40;
    }

/*                                  LINEAR INTERPOLATION */

    p = (c__ - b) * s;
    q = one - s;
    goto L50;

/*                                  INVERSE QUADRATIC INTERPOLATION */

L40:
    q = fa / fc;
    r__ = fb / fc;
    rone = r__ - one;
    p = s * ((c__ - b) * q * (q - r__) - (b - a) * rone);
    q = (q - one) * rone * (s - one);
L50:
    if (p > zero) {
    q = -q;
    }
    if (p < zero) {
    p = -p;
    }
    s = e;
    e = d__;

/*                                  IF DABS(P/Q).GE.75*DABS(C-B) THEN */
/*                                     FORCE BISECTION */

    if (p + p >= three * rm * q) {
    goto L60;
    }

/*                                  IF DABS(P/Q).GE..5*DABS(S) THEN FORCE */
/*                                     BISECTION. S = THE VALUE OF P/Q */
/*                                     ON THE STEP BEFORE THE LAST ONE */

    if (p + p >= (d__1 = s * q, abs(d__1))) {
    goto L60;
    }
    d__ = p / q;
    goto L70;

/*                                  BISECTION */

L60:
    e = rm;
    d__ = e;

/*                                  INCREMENT B */

L70:
    a = b;
    fa = fb;
    temp = d__;
    if (abs(temp) <= half * tol) {
    d__1 = half * tol;
    temp = d_sign(&d__1, &rm);
    }
    b += temp;
    s = b;
    fb = determ_(n, &tri[3], &s);
    ++ic;
    if (fb * fc <= zero) {
    goto L20;
    }
    goto L10;

/*                                  CONVERGENCE OF B */

L80:
    a = c__;
    maxfn = ic;
    goto L130;

/*                                  MAXFN EVALUATIONS */

L90:
    *ier = 501;
    a = c__;
    maxfn = ic;
    if (itcom1_1.level >= 1) {
    io___791.ciunit = itcom1_1.nout;
    s_wsfe(&io___791);
    do_fio(&c__1, (char *)&maxfn, (ftnlen)sizeof(integer));
    e_wsfe();
    }
    goto L130;

/*                                  TERMINAL ERROR - F(A) AND F(B) HAVE */
/*                                  THE SAME SIGN */

L110:
    *ier = 502;
    maxfn = ic;
    if (itcom1_1.level >= 1) {
    io___792.ciunit = itcom1_1.nout;
    s_wsfe(&io___792);
    e_wsfe();
    }
L130:
    *aa = a;
    *bb = b;
    *maxfnn = maxfn;
    return 0;
} /* zbrent_ */

//////////////////////////////////////////////////////////////////////////



}}} // end namespace itk::fem::itpack
