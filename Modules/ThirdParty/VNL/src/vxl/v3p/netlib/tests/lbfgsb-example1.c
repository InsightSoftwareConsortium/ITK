/* lbfgsb-example1.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
        on Microsoft Windows system, link with libf2c.lib;
        on Linux or Unix systems, link with .../path/to/libf2c.a -lm
        or, if you install libf2c.a in a standard place, with -lf2c -lm
        -- in that order, at the end of the command line, as in
                cc *.o -lf2c -lm
        Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

                http://www.netlib.org/f2c/libf2c.zip
*/

#include "v3p_netlib.h"

/*                             DRIVER 1 */
/*     -------------------------------------------------------------- */
/*                SIMPLE DRIVER FOR L-BFGS-B (version 2.1) */
/*     -------------------------------------------------------------- */

/*        L-BFGS-B is a code for solving large nonlinear optimization */
/*             problems with simple bounds on the variables. */

/*        The code can also be used for unconstrained problems and is */
/*        as efficient for these problems as the earlier limited memory */
/*                          code L-BFGS. */

/*        This is the simplest driver in the package. It uses all the */
/*                    default settings of the code. */


/*     References: */

/*        [1] R. H. Byrd, P. Lu, J. Nocedal and C. Zhu, ``A limited */
/*        memory algorithm for bound constrained optimization'', */
/*        SIAM J. Scientific Computing 16 (1995), no. 5, pp. 1190--1208. */

/*        [2] C. Zhu, R.H. Byrd, P. Lu, J. Nocedal, ``L-BFGS-B: FORTRAN */
/*        Subroutines for Large Scale Bound Constrained Optimization'' */
/*        Tech. Report, NAM-11, EECS Department, Northwestern University, */
/*        1994. */


/*          (Postscript files of these papers are available via anonymous */
/*           ftp to eecs.nwu.edu in the directory pub/lbfgs/lbfgs_bcm.) */

/*                              *  *  * */

/*        NEOS, November 1994. (Latest revision June 1996.) */
/*        Optimization Technology Center. */
/*        Argonne National Laboratory and Northwestern University. */
/*        Written by */
/*                           Ciyou Zhu */
/*        in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */

/*     NOTE: The user should adapt the subroutine 'timer' if 'etime' is */
/*           not available on the system.  An example for system */
/*           AIX Version 3.2 is available at the end of this driver. */

/*     ************** */
/*<       program driver >*/
/* Main program */ int main()
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    doublereal f, g[1024];
    integer i__;
    doublereal l[1024];
    integer m, n;
    doublereal u[1024], x[1024], t1, t2, wa[42584];
    integer nbd[1024], iwa[3072];
    char task[60] = {0};
    doublereal factr;
    char csave[60];
    doublereal dsave[29];
    integer isave[44];
    logical lsave[4];
    doublereal pgtol;
    integer iprint;

/*     This simple driver demonstrates how to call the L-BFGS-B code to */
/*       solve a sample problem (the extended Rosenbrock function */
/*       subject to bounds on the variables). The dimension n of this */
/*       problem is variable. */
/*<       integer          nmax, mmax >*/
/*<       parameter        (nmax=1024, mmax=17) >*/
/*        nmax is the dimension of the largest problem to be solved. */
/*        mmax is the maximum number of limited memory corrections. */
/*     Declare the variables needed by the code. */
/*       A description of all these variables is given at the end of */
/*       the driver. */
/*<       character*60     task, csave >*/
/*<       logical          lsave(4) >*/
/*<    >*/
/*<    >*/
/*     Declare a few additional variables for this sample problem. */
/*<       double precision t1, t2 >*/
/*<       integer          i >*/
/*     We wish to have output at every iteration. */
/*<       iprint = 1 >*/
    iprint = 1;
/*     We specify the tolerances in the stopping criteria. */
/*<       factr=1.0d+7 >*/
    factr = 1e7;
/*<       pgtol=1.0d-5 >*/
    pgtol = 1e-5;
/*     We specify the dimension n of the sample problem and the number */
/*        m of limited memory corrections stored.  (n and m should not */
/*        exceed the limits nmax and mmax respectively.) */
/*<       n=25 >*/
    n = 25;
/*<       m=5 >*/
    m = 5;
/*     We now provide nbd which defines the bounds on the variables: */
/*                    l   specifies the lower bounds, */
/*                    u   specifies the upper bounds. */
/*     First set bounds on the odd-numbered variables. */
/*<       do 10 i=1,n,2 >*/
    i__1 = n;
    for (i__ = 1; i__ <= i__1; i__ += 2) {
/*<          nbd(i)=2 >*/
        nbd[i__ - 1] = 2;
/*<          l(i)=1.0d0 >*/
        l[i__ - 1] = 1.;
/*<          u(i)=1.0d2 >*/
        u[i__ - 1] = 100.;
/*<   10  continue >*/
/* L10: */
    }
/*     Next set bounds on the even-numbered variables. */
/*<       do 12 i=2,n,2 >*/
    i__1 = n;
    for (i__ = 2; i__ <= i__1; i__ += 2) {
/*<          nbd(i)=2 >*/
        nbd[i__ - 1] = 2;
/*<          l(i)=-1.0d2 >*/
        l[i__ - 1] = -100.;
/*<          u(i)=1.0d2 >*/
        u[i__ - 1] = 100.;
/*<   12   continue >*/
/* L12: */
    }
/*     We now define the starting point. */
/*<       do 14 i=1,n >*/
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          x(i)=3.0d0 >*/
        x[i__ - 1] = 3.;
/*<   14  continue >*/
/* L14: */
    }
/*<       write (6,16) >*/
/*
  16  format(/,5x, 'Solving sample problem.',
     +       /,5x, ' (f = 0.0 at the optimal solution.)',/)
*/
/*<    >*/
/*     We start the iteration by initializing task. */

/*<       task = 'START' >*/
    s_copy(task, "START", (ftnlen)60, (ftnlen)5);
/*        ------- the beginning of the loop ---------- */
/*<  111  continue >*/
L111:
/*     This is the call to the L-BFGS-B code. */
/*<    >*/
    setulb_(&n, &m, x, l, u, nbd, &f, g, &factr, &pgtol, wa, iwa, task, &
            iprint, csave, lsave, isave, dsave);
/*<       if (task(1:2) .eq. 'FG') then >*/
    if (s_cmp(task, "FG", (ftnlen)2, (ftnlen)2) == 0) {
/*        the minimization routine has returned to request the */
/*        function f and gradient g values at the current x. */
/*        Compute function value f for the sample problem. */
/*<          f=.25d0*(x(1)-1.d0)**2 >*/
/* Computing 2nd power */
        d__1 = x[0] - 1.;
        f = d__1 * d__1 * .25;
/*<          do 20 i=2,n >*/
        i__1 = n;
        for (i__ = 2; i__ <= i__1; ++i__) {
/*<             f=f+(x(i)-x(i-1)**2)**2 >*/
/* Computing 2nd power */
            d__2 = x[i__ - 2];
/* Computing 2nd power */
            d__1 = x[i__ - 1] - d__2 * d__2;
            f += d__1 * d__1;
/*<   20     continue >*/
/* L20: */
        }
/*<          f=4.d0*f >*/
        f *= 4.;
/*        Compute gradient g for the sample problem. */
/*<          t1=x(2)-x(1)**2 >*/
/* Computing 2nd power */
        d__1 = x[0];
        t1 = x[1] - d__1 * d__1;
/*<          g(1)=2.d0*(x(1)-1.d0)-1.6d1*x(1)*t1 >*/
        g[0] = (x[0] - 1.) * 2. - x[0] * 16. * t1;
/*<          do 22 i=2,n-1 >*/
        i__1 = n - 1;
        for (i__ = 2; i__ <= i__1; ++i__) {
/*<             t2=t1 >*/
            t2 = t1;
/*<             t1=x(i+1)-x(i)**2 >*/
/* Computing 2nd power */
            d__1 = x[i__ - 1];
            t1 = x[i__] - d__1 * d__1;
/*<             g(i)=8.d0*t2-1.6d1*x(i)*t1 >*/
            g[i__ - 1] = t2 * 8. - x[i__ - 1] * 16. * t1;
/*<   22     continue >*/
/* L22: */
        }
/*<          g(n)=8.d0*t1 >*/
        g[n - 1] = t1 * 8.;
/*          go back to the minimization routine. */
/*<          goto 111 >*/
        goto L111;
/*<       endif >*/
    }

/*<       if (task(1:5) .eq. 'NEW_X')  goto 111 >*/
    if (s_cmp(task, "NEW_X", (ftnlen)5, (ftnlen)5) == 0) {
        goto L111;
    }
/*        the minimization routine has returned with a new iterate, */
/*         and we have opted to continue the iteration. */
/*           ---------- the end of the loop ------------- */
/*     If task is neither FG nor NEW_X we terminate execution. */
/*<       stop >*/
/*<       end >*/
    return 0;
} /* MAIN__ */
