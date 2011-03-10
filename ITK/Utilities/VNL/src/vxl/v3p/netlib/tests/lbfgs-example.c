/* tests/lbfgs-example.f -- translated by f2c (version 20050501).
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

/*     *********************** */
/*     SIMPLE DRIVER FOR LBFGS */
/*     *********************** */

/*     Example of driver for LBFGS routine, using a */
/*     simple test problem. The solution point is at */
/*     X=(1,...,1) and the optimal function value of 0. */

/*                          JORGE NOCEDAL */
/*                        *** July 1990 *** */

/*<       PROGRAM SDRIVE >*/
/* Main program */ int main()
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Local variables */
    doublereal f, g[2000];
    integer j, m, n;
    doublereal w[30014], x[2000], t1, t2, eps, diag[2000], xtol;
    integer iflag, icall;
    logical diagco;
    integer iprint[2];

    v3p_netlib_lbfgs_global_t lbfgs_global;
    v3p_netlib_lbfgs_init(&lbfgs_global);

/*<       PARAMETER(NDIM=2000,MSAVE=7,NWORK=NDIM*(2*MSAVE +1)+2*MSAVE) >*/
/*<       DOUBLE PRECISION X(NDIM),G(NDIM),DIAG(NDIM),W(NWORK) >*/
/*<       DOUBLE PRECISION F,EPS,XTOL,GTOL,T1,T2,STPMIN,STPMAX >*/
/*<       INTEGER IPRINT(2),IFLAG,ICALL,N,M,MP,LP,J >*/
/*<       LOGICAL DIAGCO >*/

/*     The driver for LBFGS must always declare LB2 as EXTERNAL */

/*<       EXTERNAL LB2 >*/
/*<       COMMON /LB3/MP,LP,GTOL,STPMIN,STPMAX >*/

/*<       N=100 >*/
    n = 100;
/*<       M=5 >*/
    m = 5;
/*<       IPRINT(1)= 1 >*/
    iprint[0] = 1;
/*<       IPRINT(2)= 0 >*/
    iprint[1] = 0;

/*     We do not wish to provide the diagonal matrices Hk0, and */
/*     therefore set DIAGCO to FALSE. */

/*<       DIAGCO= .FALSE. >*/
    diagco = FALSE_;
/*<       EPS= 1.0D-5 >*/
    eps = 1e-5;
/*<       XTOL= 1.0D-16 >*/
    xtol = 1e-16;
/*<       ICALL=0 >*/
    icall = 0;
/*<       IFLAG=0 >*/
    iflag = 0;
/*<       DO 10 J=1,N,2 >*/
    i__1 = n;
    for (j = 1; j <= i__1; j += 2) {
/*<          X(J)=-1.2D0 >*/
        x[j - 1] = -1.2;
/*<          X(J+1)=1.D0 >*/
        x[j] = 1.;
/*<  10   CONTINUE >*/
/* L10: */
    }

/*<  20   CONTINUE >*/
L20:
/*<       F= 0.D0 >*/
    f = 0.;
/*<       DO 30 J=1,N,2 >*/
    i__1 = n;
    for (j = 1; j <= i__1; j += 2) {
/*<         T1= 1.D0-X(J) >*/
        t1 = 1. - x[j - 1];
/*<         T2= 1.D1*(X(J+1)-X(J)**2) >*/
/* Computing 2nd power */
        d__1 = x[j - 1];
        t2 = (x[j] - d__1 * d__1) * 10.;
/*<         G(J+1)= 2.D1*T2 >*/
        g[j] = t2 * 20.;
/*<         G(J)= -2.D0*(X(J)*G(J+1)+T1) >*/
        g[j - 1] = (x[j - 1] * g[j] + t1) * -2.;
/*<         F= F+T1**2+T2**2 >*/
/* Computing 2nd power */
        d__1 = t1;
/* Computing 2nd power */
        d__2 = t2;
        f = f + d__1 * d__1 + d__2 * d__2;
/*<  30   CONTINUE >*/
/* L30: */
    }
/*<       CALL LBFGS(N,M,X,F,G,DIAGCO,DIAG,IPRINT,EPS,XTOL,W,IFLAG) >*/
    v3p_netlib_lbfgs_(
      &n, &m, x, &f, g, &diagco, diag, iprint, &eps, &xtol, w, &iflag,
      &lbfgs_global);
/*<       IF(IFLAG.LE.0) GO TO 50 >*/
    if (iflag <= 0) {
        goto L50;
    }
/*<       ICALL=ICALL + 1 >*/
    ++icall;
/*     We allow at most 2000 evaluations of F and G */
/*<       IF(ICALL.GT.2000) GO TO 50 >*/
    if (icall > 2000) {
        goto L50;
    }
/*<       GO TO 20 >*/
    goto L20;
/*<   50  CONTINUE >*/
L50:
/*<       END >*/
    return 0;
} /* MAIN__ */
