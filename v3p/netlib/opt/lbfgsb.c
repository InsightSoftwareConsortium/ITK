/* opt/lbfgsb.f -- translated by f2c (version 20050501).
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

#undef abs
#undef min
#undef max
#include <math.h>
#include <stdio.h>
#define abs(x) ((x) >= 0 ? (x) : -(x))
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))

/* Code below must be fixed to pass a real FILE* value to fprintf if
   this macro is defined.  */
#undef LBFGSB_ENABLE_ITERATE_FILE

static void lbfgsb_printf_vec(const char* name,
                              double const* v,
                              integer len)
{
/*<             write (6,1004) 'G =',(g(i), i = 1, n) >*/
/*
  1004 format (/,a4, 1p, 6(1x,d11.4),/,(4x,1p,6(1x,d11.4)))
*/
  integer i;
  printf("%s =", name);
  for(i = 1; i <= len; ++i)
    {
    printf(" %11.4g", v[i]);
    }
  printf("\n");
}

/* Table of constant values */

static doublereal c_b9 = 0.;
static integer c__1 = 1;
static integer c__11 = 11;
static doublereal c_b275 = .001;
static doublereal c_b276 = .9;
static doublereal c_b277 = .1;

/* ================    L-BFGS-B (version 2.1)   ========================== */
/*<    >*/
/* Subroutine */ int setulb_(integer *n, integer *m, doublereal *x, 
	doublereal *l, doublereal *u, integer *nbd, doublereal *f, doublereal 
	*g, doublereal *factr, doublereal *pgtol, doublereal *wa, integer *
	iwa, char *task, integer *iprint, char *csave, logical *lsave, 
	integer *isave, doublereal *dsave)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer l1, l2, l3, ld, lr, lt, lz, lwa, lsg, lyg, lwn, lss, lws, lwt, 
	    lsy, lwy, lyy, lsnd, lsgo, lygo;
    extern /* Subroutine */ int mainlb_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *, 
	    integer *, char *, integer *, char *, logical *, integer *, 
	    doublereal *, ftnlen, ftnlen);

/*<       character*60     task, csave >*/
/*<       logical          lsave(4) >*/
/*<    >*/
/*<    >*/
/*     ************ */

/*     Subroutine setulb */

/*     This subroutine partitions the working arrays wa and iwa, and */
/*       then uses the limited memory BFGS method to solve the bound */
/*       constrained optimization problem by calling mainlb. */
/*       (The direct method will be used in the subspace minimization.) */

/*     n is an integer variable. */
/*       On entry n is the dimension of the problem. */
/*       On exit n is unchanged. */

/*     m is an integer variable. */
/*       On entry m is the maximum number of variable metric corrections */
/*         used to define the limited memory matrix. */
/*       On exit m is unchanged. */

/*     x is a double precision array of dimension n. */
/*       On entry x is an approximation to the solution. */
/*       On exit x is the current approximation. */

/*     l is a double precision array of dimension n. */
/*       On entry l is the lower bound on x. */
/*       On exit l is unchanged. */

/*     u is a double precision array of dimension n. */
/*       On entry u is the upper bound on x. */
/*       On exit u is unchanged. */

/*     nbd is an integer array of dimension n. */
/*       On entry nbd represents the type of bounds imposed on the */
/*         variables, and must be specified as follows: */
/*         nbd(i)=0 if x(i) is unbounded, */
/*                1 if x(i) has only a lower bound, */
/*                2 if x(i) has both lower and upper bounds, and */
/*                3 if x(i) has only an upper bound. */
/*       On exit nbd is unchanged. */

/*     f is a double precision variable. */
/*       On first entry f is unspecified. */
/*       On final exit f is the value of the function at x. */

/*     g is a double precision array of dimension n. */
/*       On first entry g is unspecified. */
/*       On final exit g is the value of the gradient at x. */

/*     factr is a double precision variable. */
/*       On entry factr >= 0 is specified by the user.  The iteration */
/*         will stop when */

/*         (f^k - f^{k+1})/max{|f^k|,|f^{k+1}|,1} <= factr*epsmch */

/*         where epsmch is the machine precision, which is automatically */
/*         generated by the code. Typical values for factr: 1.d+12 for */
/*         low accuracy; 1.d+7 for moderate accuracy; 1.d+1 for extremely */
/*         high accuracy. */
/*       On exit factr is unchanged. */

/*     pgtol is a double precision variable. */
/*       On entry pgtol >= 0 is specified by the user.  The iteration */
/*         will stop when */

/*                 max{|proj g_i | i = 1, ..., n} <= pgtol */

/*         where pg_i is the ith component of the projected gradient. */
/*       On exit pgtol is unchanged. */

/*     wa is a double precision working array of length */
/*       (2mmax + 4)nmax + 12mmax^2 + 12mmax. */

/*     iwa is an integer working array of length 3nmax. */

/*     task is a working string of characters of length 60 indicating */
/*       the current job when entering and quitting this subroutine. */

/*     iprint is an integer variable that must be set by the user. */
/*       It controls the frequency and type of output generated: */
/*        iprint<0    no output is generated; */
/*        iprint=0    print only one line at the last iteration; */
/*        0<iprint<99 print also f and |proj g| every iprint iterations; */
/*        iprint=99   print details of every iteration except n-vectors; */
/*        iprint=100  print also the changes of active set and final x; */
/*        iprint>100  print details of every iteration including x and g; */
/*       When iprint > 0, the file iterate.dat will be created to */
/*                        summarize the iteration. */

/*     csave is a working string of characters of length 60. */

/*     lsave is a logical working array of dimension 4. */
/*       On exit with 'task' = NEW_X, the following information is */
/*                                                             available: */
/*         If lsave(1) = .true.  then  the initial X has been replaced by */
/*                                     its projection in the feasible set; */
/*         If lsave(2) = .true.  then  the problem is constrained; */
/*         If lsave(3) = .true.  then  each variable has upper and lower */
/*                                     bounds; */

/*     isave is an integer working array of dimension 44. */
/*       On exit with 'task' = NEW_X, the following information is */
/*                                                             available: */
/*         isave(22) = the total number of intervals explored in the */
/*                         search of Cauchy points; */
/*         isave(26) = the total number of skipped BFGS updates before */
/*                         the current iteration; */
/*         isave(30) = the number of current iteration; */
/*         isave(31) = the total number of BFGS updates prior the current */
/*                         iteration; */
/*         isave(33) = the number of intervals explored in the search of */
/*                         Cauchy point in the current iteration; */
/*         isave(34) = the total number of function and gradient */
/*                         evaluations; */
/*         isave(36) = the number of function value or gradient */
/*                                  evaluations in the current iteration; */
/*         if isave(37) = 0  then the subspace argmin is within the box; */
/*         if isave(37) = 1  then the subspace argmin is beyond the box; */
/*         isave(38) = the number of free variables in the current */
/*                         iteration; */
/*         isave(39) = the number of active constraints in the current */
/*                         iteration; */
/*         n + 1 - isave(40) = the number of variables leaving the set of */
/*                           active constraints in the current iteration; */
/*         isave(41) = the number of variables entering the set of active */
/*                         constraints in the current iteration. */

/*     dsave is a double precision working array of dimension 29. */
/*       On exit with 'task' = NEW_X, the following information is */
/*                                                             available: */
/*         dsave(1) = current 'theta' in the BFGS matrix; */
/*         dsave(2) = f(x) in the previous iteration; */
/*         dsave(3) = factr*epsmch; */
/*         dsave(4) = 2-norm of the line search direction vector; */
/*         dsave(5) = the machine precision epsmch generated by the code; */
/*         dsave(7) = the accumulated time spent on searching for */
/*                                                         Cauchy points; */
/*         dsave(8) = the accumulated time spent on */
/*                                                 subspace minimization; */
/*         dsave(9) = the accumulated time spent on line search; */
/*         dsave(11) = the slope of the line search function at */
/*                                  the current point of line search; */
/*         dsave(12) = the maximum relative step length imposed in */
/*                                                           line search; */
/*         dsave(13) = the infinity norm of the projected gradient; */
/*         dsave(14) = the relative step length in the line search; */
/*         dsave(15) = the slope of the line search function at */
/*                                 the starting point of the line search; */
/*         dsave(16) = the square of the 2-norm of the line search */
/*                                                      direction vector. */

/*     Subprograms called: */

/*       L-BFGS-B Library ... mainlb. */


/*     References: */

/*       [1] R. H. Byrd, P. Lu, J. Nocedal and C. Zhu, ``A limited */
/*       memory algorithm for bound constrained optimization'', */
/*       SIAM J. Scientific Computing 16 (1995), no. 5, pp. 1190--1208. */

/*       [2] C. Zhu, R.H. Byrd, P. Lu, J. Nocedal, ``L-BFGS-B: a */
/*       limited memory FORTRAN code for solving bound constrained */
/*       optimization problems'', Tech. Report, NAM-11, EECS Department, */
/*       Northwestern University, 1994. */

/*       (Postscript files of these papers are available via anonymous */
/*        ftp to eecs.nwu.edu in the directory pub/lbfgs/lbfgs_bcm.) */

/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ************ */
/*<    >*/
/*<       if (task .eq. 'START') then >*/
    /* Parameter adjustments */
    --iwa;
    --g;
    --nbd;
    --u;
    --l;
    --x;
    --wa;
    --lsave;
    --isave;
    --dsave;

    /* Function Body */
    if (s_cmp(task, "START", (ftnlen)60, (ftnlen)5) == 0) {
/*<          isave(1)  = m*n >*/
	isave[1] = *m * *n;
/*<          isave(2)  = m**2 >*/
/* Computing 2nd power */
	i__1 = *m;
	isave[2] = i__1 * i__1;
/*<          isave(3)  = 4*m**2 >*/
/* Computing 2nd power */
	i__1 = *m;
	isave[3] = i__1 * i__1 << 2;
/*<          isave(4)  = 1 >*/
	isave[4] = 1;
/*<          isave(5)  = isave(4)  + isave(1) >*/
	isave[5] = isave[4] + isave[1];
/*<          isave(6)  = isave(5)  + isave(1) >*/
	isave[6] = isave[5] + isave[1];
/*<          isave(7)  = isave(6)  + isave(2) >*/
	isave[7] = isave[6] + isave[2];
/*<          isave(8)  = isave(7)  + isave(2) >*/
	isave[8] = isave[7] + isave[2];
/*<          isave(9)  = isave(8)  + isave(2) >*/
	isave[9] = isave[8] + isave[2];
/*<          isave(10) = isave(9)  + isave(2) >*/
	isave[10] = isave[9] + isave[2];
/*<          isave(11) = isave(10) + isave(3) >*/
	isave[11] = isave[10] + isave[3];
/*<          isave(12) = isave(11) + isave(3) >*/
	isave[12] = isave[11] + isave[3];
/*<          isave(13) = isave(12) + n >*/
	isave[13] = isave[12] + *n;
/*<          isave(14) = isave(13) + n >*/
	isave[14] = isave[13] + *n;
/*<          isave(15) = isave(14) + n >*/
	isave[15] = isave[14] + *n;
/*<          isave(16) = isave(15) + n >*/
	isave[16] = isave[15] + *n;
/*<          isave(17) = isave(16) + 8*m >*/
	isave[17] = isave[16] + (*m << 3);
/*<          isave(18) = isave(17) + m >*/
	isave[18] = isave[17] + *m;
/*<          isave(19) = isave(18) + m >*/
	isave[19] = isave[18] + *m;
/*<          isave(20) = isave(19) + m    >*/
	isave[20] = isave[19] + *m;
/*<       endif >*/
    }
/*<       l1   = isave(1) >*/
    l1 = isave[1];
/*<       l2   = isave(2) >*/
    l2 = isave[2];
/*<       l3   = isave(3) >*/
    l3 = isave[3];
/*<       lws  = isave(4) >*/
    lws = isave[4];
/*<       lwy  = isave(5) >*/
    lwy = isave[5];
/*<       lsy  = isave(6) >*/
    lsy = isave[6];
/*<       lss  = isave(7) >*/
    lss = isave[7];
/*<       lyy  = isave(8) >*/
    lyy = isave[8];
/*<       lwt  = isave(9) >*/
    lwt = isave[9];
/*<       lwn  = isave(10) >*/
    lwn = isave[10];
/*<       lsnd = isave(11) >*/
    lsnd = isave[11];
/*<       lz   = isave(12) >*/
    lz = isave[12];
/*<       lr   = isave(13) >*/
    lr = isave[13];
/*<       ld   = isave(14) >*/
    ld = isave[14];
/*<       lt   = isave(15) >*/
    lt = isave[15];
/*<       lwa  = isave(16) >*/
    lwa = isave[16];
/*<       lsg  = isave(17) >*/
    lsg = isave[17];
/*<       lsgo = isave(18) >*/
    lsgo = isave[18];
/*<       lyg  = isave(19) >*/
    lyg = isave[19];
/*<       lygo = isave(20) >*/
    lygo = isave[20];
/*<    >*/
    mainlb_(n, m, &x[1], &l[1], &u[1], &nbd[1], f, &g[1], factr, pgtol, &wa[
	    lws], &wa[lwy], &wa[lsy], &wa[lss], &wa[lyy], &wa[lwt], &wa[lwn], 
	    &wa[lsnd], &wa[lz], &wa[lr], &wa[ld], &wa[lt], &wa[lwa], &wa[lsg],
	     &wa[lsgo], &wa[lyg], &wa[lygo], &iwa[1], &iwa[*n + 1], &iwa[(*n 
	    << 1) + 1], task, iprint, csave, &lsave[1], &isave[22], &dsave[1],
	     (ftnlen)60, (ftnlen)60);
/*<       return >*/
    return 0;
/*<       end >*/
} /* setulb_ */

/* ======================= The end of setulb ============================= */
/*<    >*/
/* Subroutine */ int mainlb_(integer *n, integer *m, doublereal *x, 
	doublereal *l, doublereal *u, integer *nbd, doublereal *f, doublereal 
	*g, doublereal *factr, doublereal *pgtol, doublereal *ws, doublereal *
	wy, doublereal *sy, doublereal *ss, doublereal *yy, doublereal *wt, 
	doublereal *wn, doublereal *snd, doublereal *z__, doublereal *r__, 
	doublereal *d__, doublereal *t, doublereal *wa, doublereal *sg, 
	doublereal *sgo, doublereal *yg, doublereal *ygo, integer *index, 
	integer *iwhere, integer *indx2, char *task, integer *iprint, char *
	csave, logical *lsave, integer *isave, doublereal *dsave, ftnlen 
	task_len, ftnlen csave_len)
{
    /* System generated locals */
    integer ws_dim1, ws_offset, wy_dim1, wy_offset, sy_dim1, sy_offset, 
	    ss_dim1, ss_offset, yy_dim1, yy_offset, wt_dim1, wt_offset, 
	    wn_dim1, wn_offset, snd_dim1, snd_offset, i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__, k;
    doublereal gd, dr, rr, dtd;
    integer col;
    doublereal tol;
    logical wrk;
    doublereal stp, cpu1, cpu2;
    integer head;
    doublereal fold;
    integer nact;
    doublereal ddum;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    integer info;
    doublereal time;
    integer nfgv, ifun, iter, nint;
    char word[3];
    doublereal time1, time2;
    integer iback;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *);
    doublereal gdold;
    integer nfree;
    logical boxed;
    integer itail;
    doublereal theta;
    extern /* Subroutine */ int freev_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, logical *, logical *, 
	    logical *, integer *, integer *), dcopy_(integer *, doublereal *, 
	    integer *, doublereal *, integer *);
    doublereal dnorm;
    extern /* Subroutine */ int timer_(doublereal *), formk_(integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    logical *, doublereal *, doublereal *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *, 
	    integer *);
    integer nskip, iword;
    extern /* Subroutine */ int formt_(integer *, doublereal *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *), subsm_(integer 
	    *, integer *, integer *, integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, doublereal *, doublereal *, doublereal *,
	     doublereal *, integer *, integer *, integer *, doublereal *, 
	    doublereal *, integer *, integer *);
    doublereal xstep, stpmx;
    extern /* Subroutine */ int prn1lb_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, integer *, doublereal *), 
	    prn2lb_(integer *, doublereal *, doublereal *, doublereal *, 
	    integer *, integer *, integer *, integer *, integer *, doublereal 
	    *, integer *, char *, integer *, integer *, doublereal *, 
	    doublereal *, ftnlen), prn3lb_(integer *, doublereal *, 
	    doublereal *, char *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, doublereal *, 
	    doublereal *, integer *, char *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *,
	     ftnlen, ftnlen);
    integer ileave;
    extern /* Subroutine */ int errclb_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, char *, integer *, integer 
	    *, ftnlen);
    doublereal cachyt;
    integer itfile;
    extern /* Subroutine */ int active_(integer *, doublereal *, doublereal *,
	     integer *, doublereal *, integer *, integer *, logical *, 
	    logical *, logical *), cauchy_(integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, doublereal *,
	     integer *, doublereal *, integer *, doublereal *);
    doublereal epsmch;
    extern /* Subroutine */ int cmprlb_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, integer *, integer *, integer *, logical *, 
	    integer *);
    logical updatd;
    doublereal sbtime;
    logical prjctd;
    integer iupdat;
    extern doublereal dpmeps_();
    logical cnstnd;
    doublereal sbgnrm;
    integer nenter;
    doublereal lnscht;
    extern /* Subroutine */ int lnsrlb_(integer *, doublereal *, doublereal *,
	     integer *, doublereal *, doublereal *, doublereal *, doublereal *
	    , doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *, 
	    integer *, integer *, integer *, char *, logical *, logical *, 
	    char *, integer *, doublereal *, ftnlen, ftnlen), matupd_(integer 
	    *, integer *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *, 
	    integer *, integer *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    integer nintol;
    extern /* Subroutine */ int projgr_(integer *, doublereal *, doublereal *,
	     integer *, doublereal *, doublereal *, doublereal *);

    (void)task_len;
    (void)csave_len;

/*<       character*60     task, csave >*/
/*<       logical          lsave(4) >*/
/*<    >*/
/*<    >*/
/*     ************ */

/*     Subroutine mainlb */

/*     This subroutine solves bound constrained optimization problems by */
/*       using the compact formula of the limited memory BFGS updates. */

/*     n is an integer variable. */
/*       On entry n is the number of variables. */
/*       On exit n is unchanged. */

/*     m is an integer variable. */
/*       On entry m is the maximum number of variable metric */
/*          corrections allowed in the limited memory matrix. */
/*       On exit m is unchanged. */

/*     x is a double precision array of dimension n. */
/*       On entry x is an approximation to the solution. */
/*       On exit x is the current approximation. */

/*     l is a double precision array of dimension n. */
/*       On entry l is the lower bound of x. */
/*       On exit l is unchanged. */

/*     u is a double precision array of dimension n. */
/*       On entry u is the upper bound of x. */
/*       On exit u is unchanged. */

/*     nbd is an integer array of dimension n. */
/*       On entry nbd represents the type of bounds imposed on the */
/*         variables, and must be specified as follows: */
/*         nbd(i)=0 if x(i) is unbounded, */
/*                1 if x(i) has only a lower bound, */
/*                2 if x(i) has both lower and upper bounds, */
/*                3 if x(i) has only an upper bound. */
/*       On exit nbd is unchanged. */

/*     f is a double precision variable. */
/*       On first entry f is unspecified. */
/*       On final exit f is the value of the function at x. */

/*     g is a double precision array of dimension n. */
/*       On first entry g is unspecified. */
/*       On final exit g is the value of the gradient at x. */

/*     factr is a double precision variable. */
/*       On entry factr >= 0 is specified by the user.  The iteration */
/*         will stop when */

/*         (f^k - f^{k+1})/max{|f^k|,|f^{k+1}|,1} <= factr*epsmch */

/*         where epsmch is the machine precision, which is automatically */
/*         generated by the code. */
/*       On exit factr is unchanged. */

/*     pgtol is a double precision variable. */
/*       On entry pgtol >= 0 is specified by the user.  The iteration */
/*         will stop when */

/*                 max{|proj g_i | i = 1, ..., n} <= pgtol */

/*         where pg_i is the ith component of the projected gradient. */
/*       On exit pgtol is unchanged. */

/*     ws, wy, sy, and wt are double precision working arrays used to */
/*       store the following information defining the limited memory */
/*          BFGS matrix: */
/*          ws, of dimension n x m, stores S, the matrix of s-vectors; */
/*          wy, of dimension n x m, stores Y, the matrix of y-vectors; */
/*          sy, of dimension m x m, stores S'Y; */
/*          ss, of dimension m x m, stores S'S; */
/* 	   yy, of dimension m x m, stores Y'Y; */
/*          wt, of dimension m x m, stores the Cholesky factorization */
/*                                  of (theta*S'S+LD^(-1)L'); see eq. */
/*                                  (2.26) in [3]. */

/*     wn is a double precision working array of dimension 2m x 2m */
/*       used to store the LEL^T factorization of the indefinite matrix */
/*                 K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ] */
/*                     [L_a -R_z           theta*S'AA'S ] */

/*       where     E = [-I  0] */
/*                     [ 0  I] */

/*     snd is a double precision working array of dimension 2m x 2m */
/*       used to store the lower triangular part of */
/*                 N = [Y' ZZ'Y   L_a'+R_z'] */
/*                     [L_a +R_z  S'AA'S   ] */

/*     z(n),r(n),d(n),t(n),wa(8*m) are double precision working arrays. */
/*       z is used at different times to store the Cauchy point and */
/*       the Newton point. */

/*     sg(m),sgo(m),yg(m),ygo(m) are double precision working arrays. */

/*     index is an integer working array of dimension n. */
/*       In subroutine freev, index is used to store the free and fixed */
/*          variables at the Generalized Cauchy Point (GCP). */

/*     iwhere is an integer working array of dimension n used to record */
/*       the status of the vector x for GCP computation. */
/*       iwhere(i)=0 or -3 if x(i) is free and has bounds, */
/*                 1       if x(i) is fixed at l(i), and l(i) .ne. u(i) */
/*                 2       if x(i) is fixed at u(i), and u(i) .ne. l(i) */
/*                 3       if x(i) is always fixed, i.e.,  u(i)=x(i)=l(i) */
/*                -1       if x(i) is always free, i.e., no bounds on it. */

/*     indx2 is an integer working array of dimension n. */
/*       Within subroutine cauchy, indx2 corresponds to the array iorder. */
/*       In subroutine freev, a list of variables entering and leaving */
/*       the free set is stored in indx2, and it is passed on to */
/*       subroutine formk with this information. */

/*     task is a working string of characters of length 60 indicating */
/*       the current job when entering and leaving this subroutine. */

/*     iprint is an INTEGER variable that must be set by the user. */
/*       It controls the frequency and type of output generated: */
/*        iprint<0    no output is generated; */
/*        iprint=0    print only one line at the last iteration; */
/*        0<iprint<99 print also f and |proj g| every iprint iterations; */
/*        iprint=99   print details of every iteration except n-vectors; */
/*        iprint=100  print also the changes of active set and final x; */
/*        iprint>100  print details of every iteration including x and g; */
/*       When iprint > 0, the file iterate.dat will be created to */
/*                        summarize the iteration. */

/*     csave is a working string of characters of length 60. */

/*     lsave is a logical working array of dimension 4. */

/*     isave is an integer working array of dimension 23. */

/*     dsave is a double precision working array of dimension 29. */


/*     Subprograms called */

/*       L-BFGS-B Library ... cauchy, subsm, lnsrlb, formk, */

/*        errclb, prn1lb, prn2lb, prn3lb, active, projgr, */

/*        freev, cmprlb, matupd, formt. */

/*       Minpack2 Library ... timer, dpmeps. */

/*       Linpack Library ... dcopy, ddot. */


/*     References: */

/*       [1] R. H. Byrd, P. Lu, J. Nocedal and C. Zhu, ``A limited */
/*       memory algorithm for bound constrained optimization'', */
/*       SIAM J. Scientific Computing 16 (1995), no. 5, pp. 1190--1208. */

/*       [2] C. Zhu, R.H. Byrd, P. Lu, J. Nocedal, ``L-BFGS-B: FORTRAN */
/*       Subroutines for Large Scale Bound Constrained Optimization'' */
/*       Tech. Report, NAM-11, EECS Department, Northwestern University, */
/*       1994. */

/*       [3] R. Byrd, J. Nocedal and R. Schnabel "Representations of */
/*       Quasi-Newton Matrices and their use in Limited Memory Methods'', */
/*       Mathematical Programming 63 (1994), no. 4, pp. 129-156. */

/*       (Postscript files of these papers are available via anonymous */
/*        ftp to eecs.nwu.edu in the directory pub/lbfgs/lbfgs_bcm.) */

/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ************ */
/*<       logical          prjctd,cnstnd,boxed,updatd,wrk >*/
/*<       character*3      word >*/
/*<    >*/
/*<    >*/
/*<       double precision one,zero >*/
/*<       parameter        (one=1.0d0,zero=0.0d0) >*/
/*<       if (task .eq. 'START') then >*/
    /* Parameter adjustments */
    --indx2;
    --iwhere;
    --index;
    --t;
    --d__;
    --r__;
    --z__;
    --g;
    --nbd;
    --u;
    --l;
    --x;
    --ygo;
    --yg;
    --sgo;
    --sg;
    --wa;
    snd_dim1 = 2 * *m;
    snd_offset = 1 + snd_dim1;
    snd -= snd_offset;
    wn_dim1 = 2 * *m;
    wn_offset = 1 + wn_dim1;
    wn -= wn_offset;
    wt_dim1 = *m;
    wt_offset = 1 + wt_dim1;
    wt -= wt_offset;
    yy_dim1 = *m;
    yy_offset = 1 + yy_dim1;
    yy -= yy_offset;
    ss_dim1 = *m;
    ss_offset = 1 + ss_dim1;
    ss -= ss_offset;
    sy_dim1 = *m;
    sy_offset = 1 + sy_dim1;
    sy -= sy_offset;
    wy_dim1 = *n;
    wy_offset = 1 + wy_dim1;
    wy -= wy_offset;
    ws_dim1 = *n;
    ws_offset = 1 + ws_dim1;
    ws -= ws_offset;
    --lsave;
    --isave;
    --dsave;

    /* Function Body */
    if (s_cmp(task, "START", (ftnlen)60, (ftnlen)5) == 0) {
/*<          call timer(time1) >*/
	timer_(&time1);
/*        Generate the current machine precision. */
/*<          epsmch = dpmeps() >*/
	epsmch = dpmeps_();
/*        Initialize counters and scalars when task='START'. */
/*           for the limited memory BFGS matrices: */
/*<          col    = 0 >*/
	col = 0;
/*<          head   = 1 >*/
	head = 1;
/*<          theta  = one >*/
	theta = 1.;
/*<          iupdat = 0 >*/
	iupdat = 0;
/*<          updatd = .false. >*/
	updatd = FALSE_;
/*           for operation counts: */
/*<          iter   = 0 >*/
	iter = 0;
/*<          nfgv   = 0 >*/
	nfgv = 0;
/*<          nint   = 0 >*/
	nint = 0;
/*<          nintol = 0 >*/
	nintol = 0;
/*<          nskip  = 0 >*/
	nskip = 0;
/*<          nfree  = n >*/
	nfree = *n;
/*           for stopping tolerance: */
/*<          tol = factr*epsmch >*/
	tol = *factr * epsmch;
/*           for measuring running time: */
/*<          cachyt = 0 >*/
	cachyt = 0.;
/*<          sbtime = 0 >*/
	sbtime = 0.;
/*<          lnscht = 0 >*/
	lnscht = 0.;
/*           'word' records the status of subspace solutions. */
/*<          word = '---' >*/
	s_copy(word, "---", (ftnlen)3, (ftnlen)3);
/*           'info' records the termination information. */
/*<          info = 0 >*/
	info = 0;
/*<          if (iprint .ge. 1) then >*/
	if (*iprint >= 1) {
/*                                open a summary file 'iterate.dat' */
/*<             open (8, file = 'iterate.dat', status = 'unknown') >*/
/*
	    o__1.oerr = 0;
	    o__1.ounit = 8;
	    o__1.ofnmlen = 11;
	    o__1.ofnm = "iterate.dat";
	    o__1.orl = 0;
	    o__1.osta = "unknown";
	    o__1.oacc = 0;
	    o__1.ofm = 0;
	    o__1.oblnk = 0;
	    f_open(&o__1);
*/
/*<             itfile = 8 >*/
	    itfile = 8;
/*<          endif             >*/
	}
/*        Check the input arguments for errors. */
/*< 	 call errclb(n,m,factr,l,u,nbd,task,info,k) >*/
	errclb_(n, m, factr, &l[1], &u[1], &nbd[1], task, &info, &k, (ftnlen)
		60);
/*<          if (task(1:5) .eq. 'ERROR') then >*/
	if (s_cmp(task, "ERROR", (ftnlen)5, (ftnlen)5) == 0) {
/*<    >*/
	    prn3lb_(n, &x[1], f, task, iprint, &info, &itfile, &iter, &nfgv, &
		    nintol, &nskip, &nact, &sbgnrm, &c_b9, &nint, word, &
		    iback, &stp, &xstep, &k, &cachyt, &sbtime, &lnscht, (
		    ftnlen)60, (ftnlen)3);
/*<             return >*/
	    return 0;
/*<          endif >*/
	}
/*<          call prn1lb(n,m,l,u,x,iprint,itfile,epsmch) >*/
	prn1lb_(n, m, &l[1], &u[1], &x[1], iprint, &itfile, &epsmch);
/*        Initialize iwhere & project x onto the feasible set. */
/*<          call active(n,l,u,nbd,x,iwhere,iprint,prjctd,cnstnd,boxed)  >*/
	active_(n, &l[1], &u[1], &nbd[1], &x[1], &iwhere[1], iprint, &prjctd, 
		&cnstnd, &boxed);
/*        The end of the initialization. */
/*<       else >*/
    } else {
/*          restore local variables. */
/*<          prjctd = lsave(1) >*/
	prjctd = lsave[1];
/*<          cnstnd = lsave(2) >*/
	cnstnd = lsave[2];
/*<          boxed  = lsave(3) >*/
	boxed = lsave[3];
/*<          updatd = lsave(4) >*/
	updatd = lsave[4];
/*<          nintol = isave(1) >*/
	nintol = isave[1];
/*<          itfile = isave(3) >*/
	itfile = isave[3];
/*<          iback  = isave(4) >*/
	iback = isave[4];
/*<          nskip  = isave(5) >*/
	nskip = isave[5];
/*<          head   = isave(6) >*/
	head = isave[6];
/*<          col    = isave(7) >*/
	col = isave[7];
/*<          itail  = isave(8) >*/
	itail = isave[8];
/*<          iter   = isave(9) >*/
	iter = isave[9];
/*<          iupdat = isave(10) >*/
	iupdat = isave[10];
/*<          nint   = isave(12) >*/
	nint = isave[12];
/*<          nfgv   = isave(13) >*/
	nfgv = isave[13];
/*<          info   = isave(14) >*/
	info = isave[14];
/*<          ifun   = isave(15) >*/
	ifun = isave[15];
/*<          iword  = isave(16) >*/
	iword = isave[16];
/*<          nfree  = isave(17) >*/
	nfree = isave[17];
/*<          nact   = isave(18) >*/
	nact = isave[18];
/*<          ileave = isave(19) >*/
	ileave = isave[19];
/*<          nenter = isave(20) >*/
	nenter = isave[20];
/*<          theta  = dsave(1) >*/
	theta = dsave[1];
/*<          fold   = dsave(2) >*/
	fold = dsave[2];
/*<          tol    = dsave(3) >*/
	tol = dsave[3];
/*<          dnorm  = dsave(4) >*/
	dnorm = dsave[4];
/*<          epsmch = dsave(5) >*/
	epsmch = dsave[5];
/*<          cpu1   = dsave(6) >*/
	cpu1 = dsave[6];
/*<          cachyt = dsave(7) >*/
	cachyt = dsave[7];
/*<          sbtime = dsave(8) >*/
	sbtime = dsave[8];
/*<          lnscht = dsave(9) >*/
	lnscht = dsave[9];
/*<          time1  = dsave(10) >*/
	time1 = dsave[10];
/*<          gd     = dsave(11) >*/
	gd = dsave[11];
/*<          stpmx  = dsave(12) >*/
	stpmx = dsave[12];
/*<          sbgnrm = dsave(13) >*/
	sbgnrm = dsave[13];
/*<          stp    = dsave(14) >*/
	stp = dsave[14];
/*<          gdold  = dsave(15) >*/
	gdold = dsave[15];
/*<          dtd    = dsave(16) >*/
	dtd = dsave[16];
/*        After returning from the driver go to the point where execution */
/*        is to resume. */
/*<          if (task(1:5) .eq. 'FG_LN') goto 666 >*/
	if (s_cmp(task, "FG_LN", (ftnlen)5, (ftnlen)5) == 0) {
	    goto L666;
	}
/*<          if (task(1:5) .eq. 'NEW_X') goto 777 >*/
	if (s_cmp(task, "NEW_X", (ftnlen)5, (ftnlen)5) == 0) {
	    goto L777;
	}
/*<          if (task(1:5) .eq. 'FG_ST') goto 111 >*/
	if (s_cmp(task, "FG_ST", (ftnlen)5, (ftnlen)5) == 0) {
	    goto L111;
	}
/*<          if (task(1:4) .eq. 'STOP') then >*/
	if (s_cmp(task, "STOP", (ftnlen)4, (ftnlen)4) == 0) {
/*<             if (task(7:9) .eq. 'CPU') then >*/
	    if (s_cmp(task + 6, "CPU", (ftnlen)3, (ftnlen)3) == 0) {
/*                                          restore the previous iterate. */
/*<                call dcopy(n,t,1,x,1) >*/
		dcopy_(n, &t[1], &c__1, &x[1], &c__1);
/*<                call dcopy(n,r,1,g,1) >*/
		dcopy_(n, &r__[1], &c__1, &g[1], &c__1);
/*<                f = fold >*/
		*f = fold;
/*<             endif >*/
	    }
/*<             goto 999 >*/
	    goto L999;
/*<          endif >*/
	}
/*<       endif  >*/
    }
/*     Compute f0 and g0. */
/*<       task = 'FG_START'  >*/
    s_copy(task, "FG_START", (ftnlen)60, (ftnlen)8);
/*          return to the driver to calculate f and g; reenter at 111. */
/*<       goto 1000 >*/
    goto L1000;
/*<  111  continue >*/
L111:
/*<       nfgv = 1 >*/
    nfgv = 1;
/*     Compute the infinity norm of the (-) projected gradient. */
/*<       call projgr(n,l,u,nbd,x,g,sbgnrm) >*/
    projgr_(n, &l[1], &u[1], &nbd[1], &x[1], &g[1], &sbgnrm);
/*<       if (iprint .ge. 1) then >*/
    if (*iprint >= 1) {
/*<          write (6,1002) iter,f,sbgnrm >*/
/*
 1002 format
     +  (/,'At iterate',i5,4x,'f= ',1p,d12.5,4x,'|proj g|= ',1p,d12.5)
*/
        printf("At iterate %5ld    f= %12.5g    |proj g|= %12.5g\n",
               iter, *f, sbgnrm);
/*<          write (itfile,1003) iter,nfgv,sbgnrm,f >*/
/*
 1003 format (2(1x,i4),5x,'-',5x,'-',3x,'-',5x,'-',5x,'-',8x,'-',3x,
     +        1p,2(1x,d10.3))
*/
#ifdef LBFGSB_ENABLE_ITERATE_FILE
        fprintf(0,
                " %4ld %4ld     -     -   -     -     -        -    %10.3g %10.3g\n",
                iter, nfgv, sbgnrm, *f);
#endif
/*<       endif >*/
    }
/*<       if (sbgnrm .le. pgtol) then >*/
    if (sbgnrm <= *pgtol) {
/*                                terminate the algorithm. */
/*<          task = 'CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL' >*/
	s_copy(task, "CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL", (
		ftnlen)60, (ftnlen)48);
/*<          goto 999 >*/
	goto L999;
/*<       endif  >*/
    }
/* ----------------- the beginning of the loop -------------------------- */
/*<  222  continue >*/
L222:
/*<       if (iprint .ge. 99) write (6,1001) iter + 1 >*/
/*
 1001 format (//,'ITERATION ',i5)
*/
    if (*iprint >= 99) {
	i__1 = iter + 1;
        printf("ITERATION %5ld\n", i__1);
    }
/*<       iword = -1 >*/
    iword = -1;

/*<       if (.not. cnstnd .and. col .gt. 0) then  >*/
    if (! cnstnd && col > 0) {
/*                                            skip the search for GCP. */
/*<          call dcopy(n,x,1,z,1) >*/
	dcopy_(n, &x[1], &c__1, &z__[1], &c__1);
/*< 	 wrk = updatd >*/
	wrk = updatd;
/*<          nint = 0 >*/
	nint = 0;
/*<          goto 333 >*/
	goto L333;
/*<       endif >*/
    }
/* ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc */

/*     Compute the Generalized Cauchy Point (GCP). */

/* ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc */
/*<       call timer(cpu1)  >*/
    timer_(&cpu1);
/*<    >*/
    cauchy_(n, &x[1], &l[1], &u[1], &nbd[1], &g[1], &indx2[1], &iwhere[1], &t[
	    1], &d__[1], &z__[1], m, &wy[wy_offset], &ws[ws_offset], &sy[
	    sy_offset], &wt[wt_offset], &theta, &col, &head, &wa[1], &wa[(*m 
	    << 1) + 1], &wa[(*m << 2) + 1], &wa[*m * 6 + 1], &nint, &sg[1], &
	    yg[1], iprint, &sbgnrm, &info, &epsmch);
/*<       if (info .ne. 0) then  >*/
    if (info != 0) {
/*         singular triangular system detected; refresh the lbfgs memory. */
/*<          if(iprint .ge. 1) write (6, 1005) >*/
/*
 1005 format (/, 
     +' Singular triangular system detected;',/,
     +'   refresh the lbfgs memory and restart the iteration.')
*/
	if (*iprint >= 1) {
            printf(" Singular triangular system detected;\n"
                   "   refresh the lbfgs memory and restart the iteration.\n");
	}
/*<          info   = 0 >*/
	info = 0;
/*<          col    = 0 >*/
	col = 0;
/*<          head   = 1 >*/
	head = 1;
/*<          theta  = one >*/
	theta = 1.;
/*<          iupdat = 0 >*/
	iupdat = 0;
/*<          updatd = .false. >*/
	updatd = FALSE_;
/*<          call timer(cpu2)  >*/
	timer_(&cpu2);
/*<          cachyt = cachyt + cpu2 - cpu1 >*/
	cachyt = cachyt + cpu2 - cpu1;
/*<          goto 222 >*/
	goto L222;
/*<       endif >*/
    }
/*<       call timer(cpu2)  >*/
    timer_(&cpu2);
/*<       cachyt = cachyt + cpu2 - cpu1 >*/
    cachyt = cachyt + cpu2 - cpu1;
/*<       nintol = nintol + nint >*/
    nintol += nint;
/*     Count the entering and leaving variables for iter > 0; */
/*     find the index set of free and active variables at the GCP. */
/*<    >*/
    freev_(n, &nfree, &index[1], &nenter, &ileave, &indx2[1], &iwhere[1], &
	    wrk, &updatd, &cnstnd, iprint, &iter);
/*<       nact = n - nfree >*/
    nact = *n - nfree;
/*<  333  continue >*/
L333:
/*     If there are no free variables or B=theta*I, then */
/*                                        skip the subspace minimization. */
/*<       if (nfree .eq. 0 .or. col .eq. 0) goto 555 >*/
    if (nfree == 0 || col == 0) {
	goto L555;
    }
/* ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc */

/*     Subspace minimization. */

/* ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc */
/*<       call timer(cpu1)  >*/
    timer_(&cpu1);
/*     Form  the LEL^T factorization of the indefinite */
/*       matrix    K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ] */
/*                     [L_a -R_z           theta*S'AA'S ] */
/*       where     E = [-I  0] */
/*                     [ 0  I] */
/*<    >*/
    if (wrk) {
	formk_(n, &nfree, &index[1], &nenter, &ileave, &indx2[1], &iupdat, &
		updatd, &wn[wn_offset], &snd[snd_offset], m, &ws[ws_offset], &
		wy[wy_offset], &sy[sy_offset], &theta, &col, &head, &info);
    }
/*<       if (info .ne. 0) then >*/
    if (info != 0) {
/*          nonpositive definiteness in Cholesky factorization; */
/*          refresh the lbfgs memory and restart the iteration. */
/*<          if(iprint .ge. 1) write (6, 1006) >*/
/*
 1006 format (/, 
     +' Nonpositive definiteness in Cholesky factorization in formk;',/,
     +'   refresh the lbfgs memory and restart the iteration.')
 */
	if (*iprint >= 1) {
            printf(" Nonpositive definiteness in Cholesky factorization in formk;\n"
                   "   refresh the lbfgs memory and restart the iteration.\n");
	}
/*<          info   = 0 >*/
	info = 0;
/*<          col    = 0 >*/
	col = 0;
/*<          head   = 1 >*/
	head = 1;
/*<          theta  = one >*/
	theta = 1.;
/*<          iupdat = 0 >*/
	iupdat = 0;
/*<          updatd = .false. >*/
	updatd = FALSE_;
/*<          call timer(cpu2)  >*/
	timer_(&cpu2);
/*<          sbtime = sbtime + cpu2 - cpu1  >*/
	sbtime = sbtime + cpu2 - cpu1;
/*<          goto 222 >*/
	goto L222;
/*<       endif  >*/
    }
/*        compute r=-Z'B(xcp-xk)-Z'g (using wa(2m+1)=W'(xcp-x) */
/*                                                   from 'cauchy'). */
/*<    >*/
    cmprlb_(n, m, &x[1], &g[1], &ws[ws_offset], &wy[wy_offset], &sy[sy_offset]
	    , &wt[wt_offset], &z__[1], &r__[1], &wa[1], &index[1], &theta, &
	    col, &head, &nfree, &cnstnd, &info);
/*<       if (info .ne. 0) goto 444 >*/
    if (info != 0) {
	goto L444;
    }
/*       call the direct method. */
/*<    >*/
    subsm_(n, m, &nfree, &index[1], &l[1], &u[1], &nbd[1], &z__[1], &r__[1], &
	    ws[ws_offset], &wy[wy_offset], &theta, &col, &head, &iword, &wa[1]
	    , &wn[wn_offset], iprint, &info);
/*<  444  continue >*/
L444:
/*<       if (info .ne. 0) then  >*/
    if (info != 0) {
/*          singular triangular system detected; */
/*          refresh the lbfgs memory and restart the iteration. */
/*<          if(iprint .ge. 1) write (6, 1005) >*/
	if (*iprint >= 1) {
            printf(" Singular triangular system detected;\n"
                   "   refresh the lbfgs memory and restart the iteration.\n");
	}
/*<          info   = 0 >*/
	info = 0;
/*<          col    = 0 >*/
	col = 0;
/*<          head   = 1 >*/
	head = 1;
/*<          theta  = one >*/
	theta = 1.;
/*<          iupdat = 0 >*/
	iupdat = 0;
/*<          updatd = .false. >*/
	updatd = FALSE_;
/*<          call timer(cpu2)  >*/
	timer_(&cpu2);
/*<          sbtime = sbtime + cpu2 - cpu1  >*/
	sbtime = sbtime + cpu2 - cpu1;
/*<          goto 222 >*/
	goto L222;
/*<       endif >*/
    }
/*<       call timer(cpu2)  >*/
    timer_(&cpu2);
/*<       sbtime = sbtime + cpu2 - cpu1  >*/
    sbtime = sbtime + cpu2 - cpu1;
/*<  555  continue >*/
L555:
/* ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc */

/*     Line search and optimality tests. */

/* ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc */
/*     Generate the search direction d:=z-x. */
/*<       do 40 i = 1, n >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	 d(i) = z(i) - x(i) >*/
	d__[i__] = z__[i__] - x[i__];
/*<   40  continue >*/
/* L40: */
    }
/*<       call timer(cpu1)  >*/
    timer_(&cpu1);
/*<  666  continue >*/
L666:
/*<    >*/
    lnsrlb_(n, &l[1], &u[1], &nbd[1], &x[1], f, &fold, &gd, &gdold, &g[1], &
	    d__[1], &r__[1], &t[1], &z__[1], &stp, &dnorm, &dtd, &xstep, &
	    stpmx, &iter, &ifun, &iback, &nfgv, &info, task, &boxed, &cnstnd, 
	    csave, &isave[22], &dsave[17], (ftnlen)60, (ftnlen)60);
/*<       if (info .ne. 0 .or. iback .ge. 20) then >*/
    if (info != 0 || iback >= 20) {
/*          restore the previous iterate. */
/*<          call dcopy(n,t,1,x,1) >*/
	dcopy_(n, &t[1], &c__1, &x[1], &c__1);
/*<          call dcopy(n,r,1,g,1) >*/
	dcopy_(n, &r__[1], &c__1, &g[1], &c__1);
/*<          f = fold >*/
	*f = fold;
/*<          if (col .eq. 0) then >*/
	if (col == 0) {
/*             abnormal termination. */
/*<             if (info .eq. 0) then >*/
	    if (info == 0) {
/*<                info = -9 >*/
		info = -9;
/*                restore the actual number of f and g evaluations etc. */
/*<                nfgv = nfgv - 1 >*/
		--nfgv;
/*<                ifun = ifun - 1 >*/
		--ifun;
/*<                iback = iback - 1 >*/
		--iback;
/*<             endif >*/
	    }
/*<             task = 'ABNORMAL_TERMINATION_IN_LNSRCH' >*/
	    s_copy(task, "ABNORMAL_TERMINATION_IN_LNSRCH", (ftnlen)60, (
		    ftnlen)30);
/*<             iter = iter + 1 >*/
	    ++iter;
/*<             goto 999 >*/
	    goto L999;
/*<          else >*/
	} else {
/*             refresh the lbfgs memory and restart the iteration. */
/*<             if(iprint .ge. 1) write (6, 1008) >*/
/*
 1008 format (/,
     +' Bad direction in the line search;',/,
     +'   refresh the lbfgs memory and restart the iteration.')
*/
	    if (*iprint >= 1) {
                printf(" Bad direction in the line search;\n"
                       "   refresh the lbfgs memory and restart the iteration.\n");
	    }
/*<             if (info .eq. 0) nfgv = nfgv - 1 >*/
	    if (info == 0) {
		--nfgv;
	    }
/*<             info   = 0 >*/
	    info = 0;
/*<             col    = 0 >*/
	    col = 0;
/*<             head   = 1 >*/
	    head = 1;
/*<             theta  = one >*/
	    theta = 1.;
/*<             iupdat = 0 >*/
	    iupdat = 0;
/*<             updatd = .false. >*/
	    updatd = FALSE_;
/*<             task   = 'RESTART_FROM_LNSRCH' >*/
	    s_copy(task, "RESTART_FROM_LNSRCH", (ftnlen)60, (ftnlen)19);
/*<             call timer(cpu2) >*/
	    timer_(&cpu2);
/*<             lnscht = lnscht + cpu2 - cpu1 >*/
	    lnscht = lnscht + cpu2 - cpu1;
/*<             goto 222 >*/
	    goto L222;
/*<          endif >*/
	}
/*<       else if (task(1:5) .eq. 'FG_LN') then >*/
    } else if (s_cmp(task, "FG_LN", (ftnlen)5, (ftnlen)5) == 0) {
/*          return to the driver for calculating f and g; reenter at 666. */
/*< 	 goto 1000 >*/
	goto L1000;
/*<       else  >*/
    } else {
/*          calculate and print out the quantities related to the new X. */
/*<          call timer(cpu2)  >*/
	timer_(&cpu2);
/*<          lnscht = lnscht + cpu2 - cpu1 >*/
	lnscht = lnscht + cpu2 - cpu1;
/*<          iter = iter + 1 >*/
	++iter;
/*        Compute the infinity norm of the projected (-)gradient. */
/*<          call projgr(n,l,u,nbd,x,g,sbgnrm) >*/
	projgr_(n, &l[1], &u[1], &nbd[1], &x[1], &g[1], &sbgnrm);
/*        Print iteration information. */
/*<    >*/
	prn2lb_(n, &x[1], f, &g[1], iprint, &itfile, &iter, &nfgv, &nact, &
		sbgnrm, &nint, word, &iword, &iback, &stp, &xstep, (ftnlen)3);
/*<          goto 1000 >*/
	goto L1000;
/*<       endif >*/
    }
/*<  777  continue >*/
L777:
/*     Test for termination. */
/*<       if (sbgnrm .le. pgtol) then >*/
    if (sbgnrm <= *pgtol) {
/*                                terminate the algorithm. */
/*<          task = 'CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL' >*/
	s_copy(task, "CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL", (
		ftnlen)60, (ftnlen)48);
/*<          goto 999 >*/
	goto L999;
/*<       endif  >*/
    }
/*<       ddum = max(abs(fold), abs(f), one) >*/
/* Computing MAX */
    d__1 = abs(fold), d__2 = abs(*f), d__1 = max(d__1,d__2);
    ddum = max(d__1,1.);
/*<       if ((fold - f) .le. tol*ddum) then >*/
    if (fold - *f <= tol * ddum) {
/*                                        terminate the algorithm. */
/*<          task = 'CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH' >*/
	s_copy(task, "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH", (
		ftnlen)60, (ftnlen)47);
/*<          if (iback .ge. 10) info = -5 >*/
	if (iback >= 10) {
	    info = -5;
	}
/*           i.e., to issue a warning if iback>10 in the line search. */
/*<          goto 999 >*/
	goto L999;
/*<       endif  >*/
    }
/*     Compute d=newx-oldx, r=newg-oldg, rr=y'y and dr=y's. */
/*<       do 42 i = 1, n >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          r(i) = g(i) - r(i) >*/
	r__[i__] = g[i__] - r__[i__];
/*<   42  continue >*/
/* L42: */
    }
/*<       rr = ddot(n,r,1,r,1) >*/
    rr = ddot_(n, &r__[1], &c__1, &r__[1], &c__1);
/*<       if (stp .eq. one) then   >*/
    if (stp == 1.) {
/*<          dr = gd - gdold >*/
	dr = gd - gdold;
/*<          ddum = -gdold >*/
	ddum = -gdold;
/*<       else >*/
    } else {
/*<          dr = (gd - gdold)*stp >*/
	dr = (gd - gdold) * stp;
/*< 	 call dscal(n,stp,d,1) >*/
	dscal_(n, &stp, &d__[1], &c__1);
/*<          ddum = -gdold*stp >*/
	ddum = -gdold * stp;
/*<       endif >*/
    }
/*<       if (dr .le. epsmch*ddum) then >*/
    if (dr <= epsmch * ddum) {
/*                            skip the L-BFGS update. */
/*<          nskip = nskip + 1 >*/
	++nskip;
/*<          updatd = .false. >*/
	updatd = FALSE_;
/*<          if (iprint .ge. 1) write (6,1004) dr, ddum >*/
/*
 1004 format ('  ys=',1p,e10.3,'  -gs=',1p,e10.3,' BFGS update SKIPPED')
 */
	if (*iprint >= 1) {
            printf("  ys=%10.3g  -gs=%10.3g BFGS update SKIPPED\n",
                   dr, ddum);
	}
/*<          goto 888 >*/
	goto L888;
/*<       endif  >*/
    }
/* ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc */

/*     Update the L-BFGS matrix. */

/* ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc */
/*<       updatd = .true. >*/
    updatd = TRUE_;
/*<       iupdat = iupdat + 1 >*/
    ++iupdat;
/*     Update matrices WS and WY and form the middle matrix in B. */
/*<    >*/
    matupd_(n, m, &ws[ws_offset], &wy[wy_offset], &sy[sy_offset], &ss[
	    ss_offset], &d__[1], &r__[1], &itail, &iupdat, &col, &head, &
	    theta, &rr, &dr, &stp, &dtd);
/*     Form the upper half of the pds T = theta*SS + L*D^(-1)*L'; */
/*        Store T in the upper triangular of the array wt; */
/*        Cholesky factorize T to J*J' with */
/*           J' stored in the upper triangular of wt. */
/*<       call formt(m,wt,sy,ss,col,theta,info) >*/
    formt_(m, &wt[wt_offset], &sy[sy_offset], &ss[ss_offset], &col, &theta, &
	    info);
/*<       if (info .ne. 0) then  >*/
    if (info != 0) {
/*          nonpositive definiteness in Cholesky factorization; */
/*          refresh the lbfgs memory and restart the iteration. */
/*<          if(iprint .ge. 1) write (6, 1007) >*/
/*
 1007 format (/,
     +' Nonpositive definiteness in Cholesky factorization in formt;',/,
     +'   refresh the lbfgs memory and restart the iteration.')
*/
	if (*iprint >= 1) {
            printf(" Nonpositive definiteness in Cholesky factorization in formt;\n"
                   "   refresh the lbfgs memory and restart the iteration.\n");
	}
/*<          info = 0 >*/
	info = 0;
/*<          col = 0 >*/
	col = 0;
/*<          head = 1 >*/
	head = 1;
/*<          theta = one >*/
	theta = 1.;
/*<          iupdat = 0 >*/
	iupdat = 0;
/*<          updatd = .false. >*/
	updatd = FALSE_;
/*<          goto 222 >*/
	goto L222;
/*<       endif >*/
    }
/*     Now the inverse of the middle matrix in B is */
/*       [  D^(1/2)      O ] [ -D^(1/2)  D^(-1/2)*L' ] */
/*       [ -L*D^(-1/2)   J ] [  0        J'          ] */
/*<  888  continue >*/
L888:
/* -------------------- the end of the loop ----------------------------- */
/*<       goto 222 >*/
    goto L222;
/*<  999  continue >*/
L999:
/*<       call timer(time2) >*/
    timer_(&time2);
/*<       time = time2 - time1 >*/
    time = time2 - time1;
/*<    >*/
    prn3lb_(n, &x[1], f, task, iprint, &info, &itfile, &iter, &nfgv, &nintol, 
	    &nskip, &nact, &sbgnrm, &time, &nint, word, &iback, &stp, &xstep, 
	    &k, &cachyt, &sbtime, &lnscht, (ftnlen)60, (ftnlen)3);
/*<  1000 continue >*/
L1000:
/*     Save local variables. */
/*<       lsave(1)  = prjctd >*/
    lsave[1] = prjctd;
/*<       lsave(2)  = cnstnd >*/
    lsave[2] = cnstnd;
/*<       lsave(3)  = boxed >*/
    lsave[3] = boxed;
/*<       lsave(4)  = updatd >*/
    lsave[4] = updatd;
/*<       isave(1)  = nintol  >*/
    isave[1] = nintol;
/*<       isave(3)  = itfile  >*/
    isave[3] = itfile;
/*<       isave(4)  = iback  >*/
    isave[4] = iback;
/*<       isave(5)  = nskip  >*/
    isave[5] = nskip;
/*<       isave(6)  = head  >*/
    isave[6] = head;
/*<       isave(7)  = col  >*/
    isave[7] = col;
/*<       isave(8)  = itail  >*/
    isave[8] = itail;
/*<       isave(9)  = iter  >*/
    isave[9] = iter;
/*<       isave(10) = iupdat  >*/
    isave[10] = iupdat;
/*<       isave(12) = nint  >*/
    isave[12] = nint;
/*<       isave(13) = nfgv  >*/
    isave[13] = nfgv;
/*<       isave(14) = info  >*/
    isave[14] = info;
/*<       isave(15) = ifun  >*/
    isave[15] = ifun;
/*<       isave(16) = iword  >*/
    isave[16] = iword;
/*<       isave(17) = nfree  >*/
    isave[17] = nfree;
/*<       isave(18) = nact  >*/
    isave[18] = nact;
/*<       isave(19) = ileave  >*/
    isave[19] = ileave;
/*<       isave(20) = nenter  >*/
    isave[20] = nenter;
/*<       dsave(1)  = theta  >*/
    dsave[1] = theta;
/*<       dsave(2)  = fold  >*/
    dsave[2] = fold;
/*<       dsave(3)  = tol  >*/
    dsave[3] = tol;
/*<       dsave(4)  = dnorm  >*/
    dsave[4] = dnorm;
/*<       dsave(5)  = epsmch  >*/
    dsave[5] = epsmch;
/*<       dsave(6)  = cpu1  >*/
    dsave[6] = cpu1;
/*<       dsave(7)  = cachyt  >*/
    dsave[7] = cachyt;
/*<       dsave(8)  = sbtime  >*/
    dsave[8] = sbtime;
/*<       dsave(9)  = lnscht  >*/
    dsave[9] = lnscht;
/*<       dsave(10) = time1  >*/
    dsave[10] = time1;
/*<       dsave(11) = gd  >*/
    dsave[11] = gd;
/*<       dsave(12) = stpmx  >*/
    dsave[12] = stpmx;
/*<       dsave(13) = sbgnrm >*/
    dsave[13] = sbgnrm;
/*<       dsave(14) = stp >*/
    dsave[14] = stp;
/*<       dsave(15) = gdold >*/
    dsave[15] = gdold;
/*<       dsave(16) = dtd   >*/
    dsave[16] = dtd;
/*<  1001 format (//,'ITERATION ',i5) >*/
/*<  1 >*/
/*<  1 >*/
/*<  1004 format ('  ys=',1p,e10.3,'  -gs=',1p,e10.3,' BFGS update SKIPPED') >*/
/*<  1 >*/
/*<  1 >*/
/*<  1 >*/
/*<  1 >*/
/*<       return    >*/
    return 0;
/*<       end >*/
} /* mainlb_ */

/* ======================= The end of mainlb ============================= */
/*<    >*/
/* Subroutine */ int active_(integer *n, doublereal *l, doublereal *u, 
	integer *nbd, doublereal *x, integer *iwhere, integer *iprint, 
	logical *prjctd, logical *cnstnd, logical *boxed)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__, nbdd;

/*<       logical          prjctd, cnstnd, boxed >*/
/*<       integer          n, iprint, nbd(n), iwhere(n) >*/
/*<       double precision x(n), l(n), u(n) >*/
/*     ************ */

/*     Subroutine active */

/*     This subroutine initializes iwhere and projects the initial x to */
/*       the feasible set if necessary. */

/*     iwhere is an integer array of dimension n. */
/*       On entry iwhere is unspecified. */
/*       On exit iwhere(i)=-1  if x(i) has no bounds */
/*                         3   if l(i)=u(i) */
/*                         0   otherwise. */
/*       In cauchy, iwhere is given finer gradations. */


/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ************ */
/*<       integer          nbdd,i >*/
/*<       double precision zero >*/
/*<       parameter        (zero=0.0d0) >*/
/*     Initialize nbdd, prjctd, cnstnd and boxed. */
/*<       nbdd = 0 >*/
    /* Parameter adjustments */
    --iwhere;
    --x;
    --nbd;
    --u;
    --l;

    /* Function Body */
    nbdd = 0;
/*<       prjctd = .false. >*/
    *prjctd = FALSE_;
/*<       cnstnd = .false. >*/
    *cnstnd = FALSE_;
/*<       boxed = .true. >*/
    *boxed = TRUE_;
/*     Project the initial x to the easible set if necessary. */
/*<       do 10 i = 1, n >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          if (nbd(i) .gt. 0) then >*/
	if (nbd[i__] > 0) {
/*<             if (nbd(i) .le. 2 .and. x(i) .le. l(i)) then >*/
	    if (nbd[i__] <= 2 && x[i__] <= l[i__]) {
/*< 	       if (x(i) .lt. l(i)) then >*/
		if (x[i__] < l[i__]) {
/*<                   prjctd = .true. >*/
		    *prjctd = TRUE_;
/*< 	          x(i) = l(i) >*/
		    x[i__] = l[i__];
/*<                endif >*/
		}
/*<                nbdd = nbdd + 1 >*/
		++nbdd;
/*<             else if (nbd(i) .ge. 2 .and. x(i) .ge. u(i)) then >*/
	    } else if (nbd[i__] >= 2 && x[i__] >= u[i__]) {
/*< 	       if (x(i) .gt. u(i)) then >*/
		if (x[i__] > u[i__]) {
/*<                   prjctd = .true. >*/
		    *prjctd = TRUE_;
/*< 	          x(i) = u(i) >*/
		    x[i__] = u[i__];
/*<                endif >*/
		}
/*<                nbdd = nbdd + 1 >*/
		++nbdd;
/*<             endif >*/
	    }
/*<          endif >*/
	}
/*<   10  continue >*/
/* L10: */
    }
/*     Initialize iwhere and assign values to cnstnd and boxed. */
/*<       do 20 i = 1, n >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          if (nbd(i) .ne. 2) boxed = .false. >*/
	if (nbd[i__] != 2) {
	    *boxed = FALSE_;
	}
/*<          if (nbd(i) .eq. 0) then >*/
	if (nbd[i__] == 0) {
/*                                this variable is always free */
/*< 	    iwhere(i) = -1 >*/
	    iwhere[i__] = -1;
/*           otherwise set x(i)=mid(x(i), u(i), l(i)). */
/*<          else >*/
	} else {
/*< 	    cnstnd = .true. >*/
	    *cnstnd = TRUE_;
/*<             if (nbd(i) .eq. 2 .and. u(i) - l(i) .le. zero) then >*/
	    if (nbd[i__] == 2 && u[i__] - l[i__] <= 0.) {
/*                   this variable is always fixed */
/*< 	       iwhere(i) = 3 >*/
		iwhere[i__] = 3;
/*<             else  >*/
	    } else {
/*<                iwhere(i) = 0 >*/
		iwhere[i__] = 0;
/*<             endif >*/
	    }
/*<          endif >*/
	}
/*<   20  continue >*/
/* L20: */
    }
/*<       if (iprint .ge. 0) then >*/
    if (*iprint >= 0) {
/*<    >*/
	if (*prjctd) {
            printf("The initial X is infeasible.  Restart with its projection.\n");
	}
/*<    >*/
	if (! (*cnstnd)) {
            printf("This problem is unconstrained.\n");
	}
/*<       endif >*/
    }
/*<       if (iprint .gt. 0) write (6,1001) nbdd >*/
    if (*iprint > 0) {
        printf("At X0 %9ld variables are exactly at the bounds\n", nbdd);
    }
/*<  1001 format (/,'At X0 ',i9,' variables are exactly at the bounds')  >*/
/*<       return >*/
    return 0;
/*<       end >*/
} /* active_ */

/* ======================= The end of active ============================= */
/*<       subroutine bmv(m, sy, wt, col, v, p, info) >*/
/* Subroutine */ int bmv_(integer *m, doublereal *sy, doublereal *wt, integer 
	*col, doublereal *v, doublereal *p, integer *info)
{
    /* System generated locals */
    integer sy_dim1, sy_offset, wt_dim1, wt_offset, i__1, i__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__, k, i2;
    doublereal sum;
    extern /* Subroutine */ int dtrsl_(doublereal *, integer *, integer *, 
	    doublereal *, integer *, integer *);

/*<       integer m, col, info >*/
/*<       double precision sy(m, m), wt(m, m), v(2*col), p(2*col) >*/
/*     ************ */

/*     Subroutine bmv */

/*     This subroutine computes the product of the 2m x 2m middle matrix */
/* 	in the compact L-BFGS formula of B and a 2m vector v; */
/* 	it returns the product in p. */

/*     m is an integer variable. */
/*       On entry m is the maximum number of variable metric corrections */
/*         used to define the limited memory matrix. */
/*       On exit m is unchanged. */

/*     sy is a double precision array of dimension m x m. */
/*       On entry sy specifies the matrix S'Y. */
/*       On exit sy is unchanged. */

/*     wt is a double precision array of dimension m x m. */
/*       On entry wt specifies the upper triangular matrix J' which is */
/*         the Cholesky factor of (thetaS'S+LD^(-1)L'). */
/*       On exit wt is unchanged. */

/*     col is an integer variable. */
/*       On entry col specifies the number of s-vectors (or y-vectors) */
/*         stored in the compact L-BFGS formula. */
/*       On exit col is unchanged. */

/*     v is a double precision array of dimension 2col. */
/*       On entry v specifies vector v. */
/*       On exit v is unchanged. */

/*     p is a double precision array of dimension 2col. */
/*       On entry p is unspecified. */
/*       On exit p is the product Mv. */

/*     info is an integer variable. */
/*       On entry info is unspecified. */
/*       On exit info = 0       for normal return, */
/*                    = nonzero for abnormal return when the system */
/*                                to be solved by dtrsl is singular. */

/*     Subprograms called: */

/*       Linpack ... dtrsl. */


/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ************ */
/*<       integer          i,k,i2 >*/
/*<       double precision sum >*/
/*<       if (col .eq. 0) return >*/
    /* Parameter adjustments */
    wt_dim1 = *m;
    wt_offset = 1 + wt_dim1;
    wt -= wt_offset;
    sy_dim1 = *m;
    sy_offset = 1 + sy_dim1;
    sy -= sy_offset;
    --p;
    --v;

    /* Function Body */
    if (*col == 0) {
	return 0;
    }
/*     PART I: solve [  D^(1/2)      O ] [ p1 ] = [ v1 ] */
/*                   [ -L*D^(-1/2)   J ] [ p2 ]   [ v2 ]. */
/* 	solve Jp2=v2+LD^(-1)v1. */
/*<       p(col + 1) = v(col + 1) >*/
    p[*col + 1] = v[*col + 1];
/*<       do 20 i = 2, col >*/
    i__1 = *col;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*<          i2 = col + i >*/
	i2 = *col + i__;
/*<          sum = 0.0d0 >*/
	sum = 0.;
/*<          do 10 k = 1, i - 1 >*/
	i__2 = i__ - 1;
	for (k = 1; k <= i__2; ++k) {
/*<             sum = sum + sy(i,k)*v(k)/sy(k,k) >*/
	    sum += sy[i__ + k * sy_dim1] * v[k] / sy[k + k * sy_dim1];
/*<   10     continue >*/
/* L10: */
	}
/*<          p(i2) = v(i2) + sum >*/
	p[i2] = v[i2] + sum;
/*<   20  continue   >*/
/* L20: */
    }
/*     Solve the triangular system */
/*<       call dtrsl(wt,m,col,p(col+1),11,info) >*/
    dtrsl_(&wt[wt_offset], m, col, &p[*col + 1], &c__11, info);
/*<       if (info .ne. 0) return >*/
    if (*info != 0) {
	return 0;
    }
/*     	solve D^(1/2)p1=v1. */
/*<       do 30 i = 1, col >*/
    i__1 = *col;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          p(i) = v(i)/sqrt(sy(i,i)) >*/
	p[i__] = v[i__] / sqrt(sy[i__ + i__ * sy_dim1]);
/*<   30  continue  >*/
/* L30: */
    }
/*     PART II: solve [ -D^(1/2)   D^(-1/2)*L'  ] [ p1 ] = [ p1 ] */
/*                    [  0         J'           ] [ p2 ]   [ p2 ]. */
/*       solve J^Tp2=p2. */
/*<       call dtrsl(wt,m,col,p(col+1),01,info) >*/
    dtrsl_(&wt[wt_offset], m, col, &p[*col + 1], &c__1, info);
/*<       if (info .ne. 0) return >*/
    if (*info != 0) {
	return 0;
    }
/*       compute p1=-D^(-1/2)(p1-D^(-1/2)L'p2) */
/*                 =-D^(-1/2)p1+D^(-1)L'p2. */
/*<       do 40 i = 1, col >*/
    i__1 = *col;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          p(i) = -p(i)/sqrt(sy(i,i)) >*/
	p[i__] = -p[i__] / sqrt(sy[i__ + i__ * sy_dim1]);
/*<   40  continue >*/
/* L40: */
    }
/*<       do 60 i = 1, col >*/
    i__1 = *col;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          sum = 0.d0 >*/
	sum = 0.;
/*<          do 50 k = i + 1, col >*/
	i__2 = *col;
	for (k = i__ + 1; k <= i__2; ++k) {
/*<             sum = sum + sy(k,i)*p(col+k)/sy(i,i) >*/
	    sum += sy[k + i__ * sy_dim1] * p[*col + k] / sy[i__ + i__ * 
		    sy_dim1];
/*<   50     continue >*/
/* L50: */
	}
/*<          p(i) = p(i) + sum >*/
	p[i__] += sum;
/*<   60  continue >*/
/* L60: */
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* bmv_ */

/* ======================== The end of bmv =============================== */
/*<    >*/
/* Subroutine */ int cauchy_(integer *n, doublereal *x, doublereal *l, 
	doublereal *u, integer *nbd, doublereal *g, integer *iorder, integer *
	iwhere, doublereal *t, doublereal *d__, doublereal *xcp, integer *m, 
	doublereal *wy, doublereal *ws, doublereal *sy, doublereal *wt, 
	doublereal *theta, integer *col, integer *head, doublereal *p, 
	doublereal *c__, doublereal *wbp, doublereal *v, integer *nint, 
	doublereal *sg, doublereal *yg, integer *iprint, doublereal *sbgnrm, 
	integer *info, doublereal *epsmch)
{
    /* System generated locals */
    integer wy_dim1, wy_offset, ws_dim1, ws_offset, sy_dim1, sy_offset, 
	    wt_dim1, wt_offset, i__1, i__2;
    doublereal d__1;

    /* Local variables */
    integer i__, j;
    doublereal f1, f2, dt, tj, tl=0, tu=0, tj0;
    integer ibp;
    doublereal dtm;
    extern /* Subroutine */ int bmv_(integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *);
    doublereal wmc, wmp, wmw;
    integer col2;
    doublereal dibp;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    integer iter;
    doublereal zibp, tsum, dibp2;
    logical bnded;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *);
    doublereal neggi;
    integer nfree;
    doublereal bkmin;
    integer nleft;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *), daxpy_(integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *);
    doublereal f2_org__;
    integer nbreak, ibkmin;
    extern /* Subroutine */ int hpsolb_(integer *, doublereal *, integer *, 
	    integer *);
    integer pointr;
    logical xlower, xupper;

/*<    >*/
/*<    >*/
/*     ************ */

/*     Subroutine cauchy */

/*     For given x, l, u, g (with sbgnrm > 0), and a limited memory */
/*       BFGS matrix B defined in terms of matrices WY, WS, WT, and */
/*       scalars head, col, and theta, this subroutine computes the */
/*       generalized Cauchy point (GCP), defined as the first local */
/*       minimizer of the quadratic */

/*                  Q(x + s) = g's + 1/2 s'Bs */

/*       along the projected gradient direction P(x-tg,l,u). */
/*       The routine returns the GCP in xcp. */

/*     n is an integer variable. */
/*       On entry n is the dimension of the problem. */
/*       On exit n is unchanged. */

/*     x is a double precision array of dimension n. */
/*       On entry x is the starting point for the GCP computation. */
/*       On exit x is unchanged. */

/*     l is a double precision array of dimension n. */
/*       On entry l is the lower bound of x. */
/*       On exit l is unchanged. */

/*     u is a double precision array of dimension n. */
/*       On entry u is the upper bound of x. */
/*       On exit u is unchanged. */

/*     nbd is an integer array of dimension n. */
/*       On entry nbd represents the type of bounds imposed on the */
/*         variables, and must be specified as follows: */
/*         nbd(i)=0 if x(i) is unbounded, */
/*                1 if x(i) has only a lower bound, */
/*                2 if x(i) has both lower and upper bounds, and */
/*                3 if x(i) has only an upper bound. */
/*       On exit nbd is unchanged. */

/*     g is a double precision array of dimension n. */
/*       On entry g is the gradient of f(x).  g must be a nonzero vector. */
/*       On exit g is unchanged. */

/*     iorder is an integer working array of dimension n. */
/*       iorder will be used to store the breakpoints in the piecewise */
/*       linear path and free variables encountered. On exit, */
/*         iorder(1),...,iorder(nleft) are indices of breakpoints */
/*                                which have not been encountered; */
/*         iorder(nleft+1),...,iorder(nbreak) are indices of */
/*                                     encountered breakpoints; and */
/*         iorder(nfree),...,iorder(n) are indices of variables which */
/*                 have no bound constraits along the search direction. */

/*     iwhere is an integer array of dimension n. */
/*       On entry iwhere indicates only the permanently fixed (iwhere=3) */
/*       or free (iwhere= -1) components of x. */
/*       On exit iwhere records the status of the current x variables. */
/*       iwhere(i)=-3  if x(i) is free and has bounds, but is not moved */
/*                 0   if x(i) is free and has bounds, and is moved */
/*                 1   if x(i) is fixed at l(i), and l(i) .ne. u(i) */
/*                 2   if x(i) is fixed at u(i), and u(i) .ne. l(i) */
/*                 3   if x(i) is always fixed, i.e.,  u(i)=x(i)=l(i) */
/*                 -1  if x(i) is always free, i.e., it has no bounds. */

/*     t is a double precision working array of dimension n. */
/*       t will be used to store the break points. */

/*     d is a double precision array of dimension n used to store */
/*       the Cauchy direction P(x-tg)-x. */

/*     xcp is a double precision array of dimension n used to return the */
/*       GCP on exit. */

/*     m is an integer variable. */
/*       On entry m is the maximum number of variable metric corrections */
/*         used to define the limited memory matrix. */
/*       On exit m is unchanged. */

/*     ws, wy, sy, and wt are double precision arrays. */
/*       On entry they store information that defines the */
/*                             limited memory BFGS matrix: */
/*         ws(n,m) stores S, a set of s-vectors; */
/*         wy(n,m) stores Y, a set of y-vectors; */
/*         sy(m,m) stores S'Y; */
/*         wt(m,m) stores the */
/*                 Cholesky factorization of (theta*S'S+LD^(-1)L'). */
/*       On exit these arrays are unchanged. */

/*     theta is a double precision variable. */
/*       On entry theta is the scaling factor specifying B_0 = theta I. */
/*       On exit theta is unchanged. */

/*     col is an integer variable. */
/*       On entry col is the actual number of variable metric */
/*         corrections stored so far. */
/*       On exit col is unchanged. */

/*     head is an integer variable. */
/*       On entry head is the location of the first s-vector (or y-vector) */
/*         in S (or Y). */
/*       On exit col is unchanged. */

/*     p is a double precision working array of dimension 2m. */
/*       p will be used to store the vector p = W^(T)d. */

/*     c is a double precision working array of dimension 2m. */
/*       c will be used to store the vector c = W^(T)(xcp-x). */

/*     wbp is a double precision working array of dimension 2m. */
/*       wbp will be used to store the row of W corresponding */
/*         to a breakpoint. */

/*     v is a double precision working array of dimension 2m. */

/*     nint is an integer variable. */
/*       On exit nint records the number of quadratic segments explored */
/*         in searching for the GCP. */

/*     sg and yg are double precision arrays of dimension m. */
/*       On entry sg  and yg store S'g and Y'g correspondingly. */
/*       On exit they are unchanged. */

/*     iprint is an INTEGER variable that must be set by the user. */
/*       It controls the frequency and type of output generated: */
/*        iprint<0    no output is generated; */
/*        iprint=0    print only one line at the last iteration; */
/*        0<iprint<99 print also f and |proj g| every iprint iterations; */
/*        iprint=99   print details of every iteration except n-vectors; */
/*        iprint=100  print also the changes of active set and final x; */
/*        iprint>100  print details of every iteration including x and g; */
/*       When iprint > 0, the file iterate.dat will be created to */
/*                        summarize the iteration. */

/*     sbgnrm is a double precision variable. */
/*       On entry sbgnrm is the norm of the projected gradient at x. */
/*       On exit sbgnrm is unchanged. */

/*     info is an integer variable. */
/*       On entry info is 0. */
/*       On exit info = 0       for normal return, */
/*                    = nonzero for abnormal return when the the system */
/*                              used in routine bmv is singular. */

/*     Subprograms called: */

/*       L-BFGS-B Library ... hpsolb, bmv. */

/*       Linpack ... dscal dcopy, daxpy. */


/*     References: */

/*       [1] R. H. Byrd, P. Lu, J. Nocedal and C. Zhu, ``A limited */
/*       memory algorithm for bound constrained optimization'', */
/*       SIAM J. Scientific Computing 16 (1995), no. 5, pp. 1190--1208. */

/*       [2] C. Zhu, R.H. Byrd, P. Lu, J. Nocedal, ``L-BFGS-B: FORTRAN */
/*       Subroutines for Large Scale Bound Constrained Optimization'' */
/*       Tech. Report, NAM-11, EECS Department, Northwestern University, */
/*       1994. */

/*       (Postscript files of these papers are available via anonymous */
/*        ftp to eecs.nwu.edu in the directory pub/lbfgs/lbfgs_bcm.) */

/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ************ */
/*<       logical          xlower,xupper,bnded >*/
/*<    >*/
/*<    >*/
/*<       double precision one,zero >*/
/*<       parameter        (one=1.0d0,zero=0.0d0) >*/
/*     Check the status of the variables, reset iwhere(i) if necessary; */
/*       compute the Cauchy direction d and the breakpoints t; initialize */
/*       the derivative f1 and the vector p = W'd (for theta = 1). */
/*<       if (sbgnrm .le. zero) then >*/
    /* Parameter adjustments */
    --xcp;
    --d__;
    --t;
    --iwhere;
    --iorder;
    --g;
    --nbd;
    --u;
    --l;
    --x;
    --yg;
    --sg;
    --v;
    --wbp;
    --c__;
    --p;
    wt_dim1 = *m;
    wt_offset = 1 + wt_dim1;
    wt -= wt_offset;
    sy_dim1 = *m;
    sy_offset = 1 + sy_dim1;
    sy -= sy_offset;
    ws_dim1 = *n;
    ws_offset = 1 + ws_dim1;
    ws -= ws_offset;
    wy_dim1 = *n;
    wy_offset = 1 + wy_dim1;
    wy -= wy_offset;

    /* Function Body */
    if (*sbgnrm <= 0.) {
/*<          if (iprint .ge. 0) write (6,*) 'Subgnorm = 0.  GCP = X.' >*/
	if (*iprint >= 0) {
            printf("Subgnorm = 0.  GCP = X.\n");
	}
/*<          call dcopy(n,x,1,xcp,1) >*/
	dcopy_(n, &x[1], &c__1, &xcp[1], &c__1);
/*< 	 return >*/
	return 0;
/*<       endif  >*/
    }
/*<       bnded = .true. >*/
    bnded = TRUE_;
/*<       nfree = n + 1 >*/
    nfree = *n + 1;
/*<       nbreak = 0 >*/
    nbreak = 0;
/*<       ibkmin = 0 >*/
    ibkmin = 0;
/*<       bkmin = zero >*/
    bkmin = 0.;
/*<       col2 = 2*col >*/
    col2 = *col << 1;
/*<       f1 = zero >*/
    f1 = 0.;
/*<       if (iprint .ge. 99) write (6,3010) >*/
/*
 3010 format (/,'---------------- CAUCHY entered-------------------')
 */
    if (*iprint >= 99) {
        printf("---------------- CAUCHY entered-------------------\n");
    }
/*     We set p to zero and build it up as we determine d. */
/*<       do 20 i = 1, col2 >*/
    i__1 = col2;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          p(i) = zero >*/
	p[i__] = 0.;
/*<   20  continue  >*/
/* L20: */
    }
/*     In the following loop we determine for each variable its bound */
/*        status and its breakpoint, and update p accordingly. */
/*        Smallest breakpoint is identified. */
/*<       do 50 i = 1, n  >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          neggi = -g(i)       >*/
	neggi = -g[i__];
/*<          if (iwhere(i) .ne. 3 .and. iwhere(i) .ne. -1) then >*/
	if (iwhere[i__] != 3 && iwhere[i__] != -1) {
/*             if x(i) is not a constant and has bounds, */
/*             compute the difference between x(i) and its bounds. */
/*<             if (nbd(i) .le. 2) tl = x(i) - l(i) >*/
	    if (nbd[i__] <= 2) {
		tl = x[i__] - l[i__];
	    }
/*<             if (nbd(i) .ge. 2) tu = u(i) - x(i) >*/
	    if (nbd[i__] >= 2) {
		tu = u[i__] - x[i__];
	    }
/*           If a variable is close enough to a bound */
/*             we treat it as at bound. */
/*<             xlower = nbd(i) .le. 2 .and. tl .le. zero >*/
	    xlower = nbd[i__] <= 2 && tl <= 0.;
/*<             xupper = nbd(i) .ge. 2 .and. tu .le. zero >*/
	    xupper = nbd[i__] >= 2 && tu <= 0.;
/*              reset iwhere(i). */
/*<             iwhere(i) = 0 >*/
	    iwhere[i__] = 0;
/*<             if (xlower) then >*/
	    if (xlower) {
/*<                if (neggi .le. zero) iwhere(i) = 1 >*/
		if (neggi <= 0.) {
		    iwhere[i__] = 1;
		}
/*<             else if (xupper) then >*/
	    } else if (xupper) {
/*<                if (neggi .ge. zero) iwhere(i) = 2 >*/
		if (neggi >= 0.) {
		    iwhere[i__] = 2;
		}
/*<             else >*/
	    } else {
/*<                if (abs(neggi) .le. zero) iwhere(i) = -3 >*/
		if (abs(neggi) <= 0.) {
		    iwhere[i__] = -3;
		}
/*<             endif >*/
	    }
/*<          endif  >*/
	}
/*<          pointr = head >*/
	pointr = *head;
/*<          if (iwhere(i) .ne. 0 .and. iwhere(i) .ne. -1) then >*/
	if (iwhere[i__] != 0 && iwhere[i__] != -1) {
/*<             d(i) = zero >*/
	    d__[i__] = 0.;
/*<          else >*/
	} else {
/*<             d(i) = neggi >*/
	    d__[i__] = neggi;
/*<             f1 = f1 - neggi*neggi >*/
	    f1 -= neggi * neggi;
/*             calculate p := p - W'e_i* (g_i). */
/*<             do 40 j = 1, col >*/
	    i__2 = *col;
	    for (j = 1; j <= i__2; ++j) {
/*<                p(j) = p(j) +  wy(i,pointr)* neggi >*/
		p[j] += wy[i__ + pointr * wy_dim1] * neggi;
/*<                p(col + j) = p(col + j) + ws(i,pointr)*neggi >*/
		p[*col + j] += ws[i__ + pointr * ws_dim1] * neggi;
/*<                pointr = mod(pointr,m) + 1 >*/
		pointr = pointr % *m + 1;
/*<   40        continue  >*/
/* L40: */
	    }
/*<    >*/
	    if (nbd[i__] <= 2 && nbd[i__] != 0 && neggi < 0.) {
/*                                 x(i) + d(i) is bounded; compute t(i). */
/*<                nbreak = nbreak + 1 >*/
		++nbreak;
/*<                iorder(nbreak) = i >*/
		iorder[nbreak] = i__;
/*<                t(nbreak) = tl/(-neggi) >*/
		t[nbreak] = tl / (-neggi);
/*< 	       if (nbreak .eq. 1 .or. t(nbreak) .lt. bkmin) then >*/
		if (nbreak == 1 || t[nbreak] < bkmin) {
/*< 		  bkmin = t(nbreak) >*/
		    bkmin = t[nbreak];
/*< 		  ibkmin = nbreak >*/
		    ibkmin = nbreak;
/*<                endif >*/
		}
/*<             else if (nbd(i) .ge. 2 .and. neggi .gt. zero) then >*/
	    } else if (nbd[i__] >= 2 && neggi > 0.) {
/*                                 x(i) + d(i) is bounded; compute t(i). */
/*<                nbreak = nbreak + 1 >*/
		++nbreak;
/*<                iorder(nbreak) = i >*/
		iorder[nbreak] = i__;
/*<                t(nbreak) = tu/neggi >*/
		t[nbreak] = tu / neggi;
/*< 	       if (nbreak .eq. 1 .or. t(nbreak) .lt. bkmin) then >*/
		if (nbreak == 1 || t[nbreak] < bkmin) {
/*< 		  bkmin = t(nbreak) >*/
		    bkmin = t[nbreak];
/*< 		  ibkmin = nbreak >*/
		    ibkmin = nbreak;
/*<                endif >*/
		}
/*<             else >*/
	    } else {
/*                x(i) + d(i) is not bounded. */
/*<                nfree = nfree - 1 >*/
		--nfree;
/*<                iorder(nfree) = i >*/
		iorder[nfree] = i__;
/*<                if (abs(neggi) .gt. zero) bnded = .false. >*/
		if (abs(neggi) > 0.) {
		    bnded = FALSE_;
		}
/*<             endif >*/
	    }
/*<          endif >*/
	}
/*<   50  continue  >*/
/* L50: */
    }
/*     The indices of the nonzero components of d are now stored */
/*       in iorder(1),...,iorder(nbreak) and iorder(nfree),...,iorder(n). */
/*       The smallest of the nbreak breakpoints is in t(ibkmin)=bkmin. */
/*<       if (theta .ne. one) then >*/
    if (*theta != 1.) {
/*                   complete the initialization of p for theta not= one. */
/*<          call dscal(col,theta,p(col+1),1) >*/
	dscal_(col, theta, &p[*col + 1], &c__1);
/*<       endif >*/
    }
/*     Initialize GCP xcp = x. */
/*<       call dcopy(n,x,1,xcp,1) >*/
    dcopy_(n, &x[1], &c__1, &xcp[1], &c__1);
/*<       if (nbreak .eq. 0 .and. nfree .eq. n + 1) then >*/
    if (nbreak == 0 && nfree == *n + 1) {
/*                  is a zero vector, return with the initial xcp as GCP. */
/*<          if (iprint .gt. 100) write (6,1010) (xcp(i), i = 1, n) >*/
/*
 1010 format ('Cauchy X =  ',/,(4x,1p,6(1x,d11.4)))
*/
	if (*iprint > 100) {
            i__1 = *n;
            lbfgsb_printf_vec("Cauchy X", xcp, i__1);
	}
/*<          return >*/
	return 0;
/*<       endif     >*/
    }
/*     Initialize c = W'(xcp - x) = 0. */
/*<       do 60 j = 1, col2 >*/
    i__1 = col2;
    for (j = 1; j <= i__1; ++j) {
/*<          c(j) = zero >*/
	c__[j] = 0.;
/*<   60  continue  >*/
/* L60: */
    }
/*     Initialize derivative f2. */
/*<       f2 =  -theta*f1  >*/
    f2 = -(*theta) * f1;
/*<       f2_org  =  f2 >*/
    f2_org__ = f2;
/*<       if (col .gt. 0) then >*/
    if (*col > 0) {
/*<      	 call bmv(m,sy,wt,col,p,v,info) >*/
	bmv_(m, &sy[sy_offset], &wt[wt_offset], col, &p[1], &v[1], info);
/*< 	 if (info .ne. 0) return >*/
	if (*info != 0) {
	    return 0;
	}
/*<      	 f2 = f2 - ddot(col2,v,1,p,1) >*/
	f2 -= ddot_(&col2, &v[1], &c__1, &p[1], &c__1);
/*<       endif >*/
    }
/*<       dtm = -f1/f2 >*/
    dtm = -f1 / f2;
/*<       tsum = zero >*/
    tsum = 0.;
/*<       nint = 1 >*/
    *nint = 1;
/*<    >*/
    if (*iprint >= 99) {
        printf("There are %ld  breakpoints.\n", nbreak);
    }
/*     If there are no breakpoints, locate the GCP and return. */
/*<       if (nbreak .eq. 0) goto 888 >*/
    if (nbreak == 0) {
	goto L888;
    }
/*<       nleft = nbreak >*/
    nleft = nbreak;
/*<       iter = 1 >*/
    iter = 1;
/*<       tj = zero >*/
    tj = 0.;
/* ------------------- the beginning of the loop ------------------------- */
/*<  777  continue >*/
L777:
/*     Find the next smallest breakpoint; */
/*       compute dt = t(nleft) - t(nleft + 1). */
/*<       tj0 = tj >*/
    tj0 = tj;
/*<       if (iter .eq. 1) then >*/
    if (iter == 1) {
/*         Since we already have the smallest breakpoint we need not do */
/*         heapsort yet. Often only one breakpoint is used and the */
/*         cost of heapsort is avoided. */
/*< 	 tj = bkmin >*/
	tj = bkmin;
/*< 	 ibp = iorder(ibkmin) >*/
	ibp = iorder[ibkmin];
/*<       else >*/
    } else {
/*<          if (iter .eq. 2) then >*/
	if (iter == 2) {
/*             Replace the already used smallest breakpoint with the */
/*             breakpoint numbered nbreak > nlast, before heapsort call. */
/*<             if (ibkmin .ne. nbreak) then >*/
	    if (ibkmin != nbreak) {
/*<                t(ibkmin) = t(nbreak) >*/
		t[ibkmin] = t[nbreak];
/*< 	       iorder(ibkmin) = iorder(nbreak) >*/
		iorder[ibkmin] = iorder[nbreak];
/*<             endif  >*/
	    }
/*        Update heap structure of breakpoints */
/*           (if iter=2, initialize heap). */
/*<          endif >*/
	}
/*<          call hpsolb(nleft,t,iorder,iter-2) >*/
	i__1 = iter - 2;
	hpsolb_(&nleft, &t[1], &iorder[1], &i__1);
/*<          tj = t(nleft) >*/
	tj = t[nleft];
/*<          ibp = iorder(nleft)   >*/
	ibp = iorder[nleft];
/*<       endif  >*/
    }
/*<       dt = tj - tj0 >*/
    dt = tj - tj0;
/*<       if (dt .ne. zero .and. iprint .ge. 100) then >*/
    if (dt != 0. && *iprint >= 100) {
/*<          write (6,4011) nint,f1,f2 >*/
/*
 4010 format ('Piece    ',i3,' --f1, f2 at start point ',1p,2(1x,d11.4))
*/
        printf("Piece    %3ld --f1, f2 at start point  %11.4g %11.5g\n",
               *nint, f1, f2);
/*<          write (6,5010) dt >*/
/*
 5010 format ('Distance to the next break point =  ',1p,d11.4)
 */
        printf("Distance to the next break point =  %11.4g", dt);
/*<          write (6,6010) dtm >*/
/*
 6010 format ('Distance to the stationary point =  ',1p,d11.4) 
 */
        printf("Distance to the stationary point =  %11.4g", dtm);
/*<       endif	      >*/
    }
/*     If a minimizer is within this interval, locate the GCP and return. */
/*<       if (dtm .lt. dt) goto 888 >*/
    if (dtm < dt) {
	goto L888;
    }
/*     Otherwise fix one variable and */
/*       reset the corresponding component of d to zero. */
/*<       tsum = tsum + dt >*/
    tsum += dt;
/*<       nleft = nleft - 1 >*/
    --nleft;
/*<       iter = iter + 1 >*/
    ++iter;
/*<       dibp = d(ibp) >*/
    dibp = d__[ibp];
/*<       d(ibp) = zero >*/
    d__[ibp] = 0.;
/*<       if (dibp .gt. zero) then >*/
    if (dibp > 0.) {
/*< 	 zibp = u(ibp) - x(ibp) >*/
	zibp = u[ibp] - x[ibp];
/*< 	 xcp(ibp) = u(ibp) >*/
	xcp[ibp] = u[ibp];
/*<          iwhere(ibp) = 2 >*/
	iwhere[ibp] = 2;
/*<       else >*/
    } else {
/*< 	 zibp = l(ibp) - x(ibp) >*/
	zibp = l[ibp] - x[ibp];
/*< 	 xcp(ibp) = l(ibp) >*/
	xcp[ibp] = l[ibp];
/*<          iwhere(ibp) = 1 >*/
	iwhere[ibp] = 1;
/*<       endif >*/
    }
/*<       if (iprint .ge. 100) write (6,*) 'Variable  ',ibp,'  is fixed.' >*/
    if (*iprint >= 100) {
        printf("Variable  %ld  is fixed.\n", ibp);
    }
/*<       if (nleft .eq. 0 .and. nbreak .eq. n) then >*/
    if (nleft == 0 && nbreak == *n) {
/*                                             all n variables are fixed, */
/*                                                return with xcp as GCP. */
/*< 	 dtm = dt >*/
	dtm = dt;
/*< 	 goto 999 >*/
	goto L999;
/*<       endif >*/
    }
/*     Update the derivative information. */
/*<       nint = nint + 1 >*/
    ++(*nint);
/*<       dibp2 = dibp**2 >*/
/* Computing 2nd power */
    d__1 = dibp;
    dibp2 = d__1 * d__1;
/*     Update f1 and f2. */
/*        temporarily set f1 and f2 for col=0. */
/*<       f1 = f1 + dt*f2 + dibp2 - theta*dibp*zibp >*/
    f1 = f1 + dt * f2 + dibp2 - *theta * dibp * zibp;
/*<       f2 = f2 - theta*dibp2 >*/
    f2 -= *theta * dibp2;
/*<       if (col .gt. 0) then >*/
    if (*col > 0) {
/*                          update c = c + dt*p. */
/*< 	 call daxpy(col2,dt,p,1,c,1) >*/
	daxpy_(&col2, &dt, &p[1], &c__1, &c__[1], &c__1);
/*           choose wbp, */
/*           the row of W corresponding to the breakpoint encountered. */
/*<       	 pointr = head >*/
	pointr = *head;
/*<          do 70 j = 1,col >*/
	i__1 = *col;
	for (j = 1; j <= i__1; ++j) {
/*< 	    wbp(j) = wy(ibp,pointr) >*/
	    wbp[j] = wy[ibp + pointr * wy_dim1];
/*< 	    wbp(col + j) = theta*ws(ibp,pointr) >*/
	    wbp[*col + j] = *theta * ws[ibp + pointr * ws_dim1];
/*<             pointr = mod(pointr,m) + 1 >*/
	    pointr = pointr % *m + 1;
/*<   70     continue  >*/
/* L70: */
	}
/*           compute (wbp)Mc, (wbp)Mp, and (wbp)M(wbp)'. */
/*<          call bmv(m,sy,wt,col,wbp,v,info) >*/
	bmv_(m, &sy[sy_offset], &wt[wt_offset], col, &wbp[1], &v[1], info);
/*< 	 if (info .ne. 0) return >*/
	if (*info != 0) {
	    return 0;
	}
/*< 	 wmc = ddot(col2,c,1,v,1) >*/
	wmc = ddot_(&col2, &c__[1], &c__1, &v[1], &c__1);
/*< 	 wmp = ddot(col2,p,1,v,1)  >*/
	wmp = ddot_(&col2, &p[1], &c__1, &v[1], &c__1);
/*< 	 wmw = ddot(col2,wbp,1,v,1) >*/
	wmw = ddot_(&col2, &wbp[1], &c__1, &v[1], &c__1);
/*           update p = p - dibp*wbp. */
/*<        	 call daxpy(col2,-dibp,wbp,1,p,1) >*/
	d__1 = -dibp;
	daxpy_(&col2, &d__1, &wbp[1], &c__1, &p[1], &c__1);
/*           complete updating f1 and f2 while col > 0. */
/*<       	 f1 = f1 + dibp*wmc >*/
	f1 += dibp * wmc;
/*<       	 f2 = f2 + 2.0d0*dibp*wmp - dibp2*wmw >*/
	f2 = f2 + dibp * 2. * wmp - dibp2 * wmw;
/*<       endif >*/
    }
/*<       f2 = max(epsmch*f2_org,f2) >*/
/* Computing MAX */
    d__1 = *epsmch * f2_org__;
    f2 = max(d__1,f2);
/*<       if (nleft .gt. 0) then >*/
    if (nleft > 0) {
/*<          dtm = -f1/f2 >*/
	dtm = -f1 / f2;
/*<          goto 777 >*/
	goto L777;
/*                 to repeat the loop for unsearched intervals. */
/*<       else if(bnded) then >*/
    } else if (bnded) {
/*<       	 f1 = zero >*/
	f1 = 0.;
/*<       	 f2 = zero >*/
	f2 = 0.;
/*< 	 dtm = zero >*/
	dtm = 0.;
/*<       else >*/
    } else {
/*<          dtm = -f1/f2 >*/
	dtm = -f1 / f2;
/*<       endif  >*/
    }
/* ------------------- the end of the loop ------------------------------- */
/*<  888  continue >*/
L888:
/*<       if (iprint .ge. 99) then >*/
    if (*iprint >= 99) {
/*<          write (6,*) >*/
        printf("\n");
/*<          write (6,*) 'GCP found in this segment' >*/
        printf("GCP found in this segment\n");
/*<          write (6,4010) nint,f1,f2 >*/
/*
 4010 format ('Piece    ',i3,' --f1, f2 at start point ',1p,2(1x,d11.4))
*/
        printf("Piece    %3ld --f1, f2 at start point  %11.4g %11.4g\n",
               *nint, f1, f2);
/*<          write (6,6010) dtm >*/
/*
 6010 format ('Distance to the stationary point =  ',1p,d11.4)
*/
        printf("Distance to the stationary point =  %11.4g\n", dtm);
/*<       endif  >*/
    }
/*<       if (dtm .le. zero) dtm = zero >*/
    if (dtm <= 0.) {
	dtm = 0.;
    }
/*<       tsum = tsum + dtm >*/
    tsum += dtm;
/*     Move free variables (i.e., the ones w/o breakpoints) and */
/*       the variables whose breakpoints haven't been reached. */
/*<       call daxpy(n,tsum,d,1,xcp,1) >*/
    daxpy_(n, &tsum, &d__[1], &c__1, &xcp[1], &c__1);
/*<  999  continue >*/
L999:
/*     Update c = c + dtm*p = W'(x^c - x) */
/*       which will be used in computing r = Z'(B(x^c - x) + g). */
/*<       if (col .gt. 0) call daxpy(col2,dtm,p,1,c,1) >*/
    if (*col > 0) {
	daxpy_(&col2, &dtm, &p[1], &c__1, &c__[1], &c__1);
    }
/*<       if (iprint .gt. 100) write (6,1010) (xcp(i),i = 1,n) >*/
    if (*iprint > 100) {
	i__1 = *n;
        lbfgsb_printf_vec("Cauchy X", xcp, i__1);
    }
/*<       if (iprint .ge. 99) write (6,2010) >*/
/*
 2010 format (/,'---------------- exit CAUCHY----------------------',/)
 */
    if (*iprint >= 99) {
        printf("---------------- exit CAUCHY----------------------\n");
    }
/*<  1010 format ('Cauchy X =  ',/,(4x,1p,6(1x,d11.4))) >*/
/*<  2010 format (/,'---------------- exit CAUCHY----------------------',/) >*/
/*<  3010 format (/,'---------------- CAUCHY entered-------------------') >*/
/*<  4010 format ('Piece    ',i3,' --f1, f2 at start point ',1p,2(1x,d11.4)) >*/
/*<  4 >*/
/*<  5010 format ('Distance to the next break point =  ',1p,d11.4) >*/
/*<  6010 format ('Distance to the stationary point =  ',1p,d11.4)  >*/
/*<       return >*/
    return 0;
/*<       end >*/
} /* cauchy_ */

/* ====================== The end of cauchy ============================== */
/*<    >*/
/* Subroutine */ int cmprlb_(integer *n, integer *m, doublereal *x, 
	doublereal *g, doublereal *ws, doublereal *wy, doublereal *sy, 
	doublereal *wt, doublereal *z__, doublereal *r__, doublereal *wa, 
	integer *index, doublereal *theta, integer *col, integer *head, 
	integer *nfree, logical *cnstnd, integer *info)
{
    /* System generated locals */
    integer ws_dim1, ws_offset, wy_dim1, wy_offset, sy_dim1, sy_offset, 
	    wt_dim1, wt_offset, i__1, i__2;

    /* Local variables */
    integer i__, j, k;
    doublereal a1, a2;
    extern /* Subroutine */ int bmv_(integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *);
    integer pointr;

/*<       logical          cnstnd >*/
/*<       integer          n, m, col, head, nfree, info, index(n) >*/
/*<    >*/
/*     ************ */

/*     Subroutine cmprlb */

/*       This subroutine computes r=-Z'B(xcp-xk)-Z'g by using */
/*         wa(2m+1)=W'(xcp-x) from subroutine cauchy. */

/*     Subprograms called: */

/*       L-BFGS-B Library ... bmv. */


/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ************ */
/*<       integer          i,j,k,pointr >*/
/*<       double precision a1,a2 >*/
/*<       if (.not. cnstnd .and. col .gt. 0) then  >*/
    /* Parameter adjustments */
    --index;
    --r__;
    --z__;
    --g;
    --x;
    --wa;
    wt_dim1 = *m;
    wt_offset = 1 + wt_dim1;
    wt -= wt_offset;
    sy_dim1 = *m;
    sy_offset = 1 + sy_dim1;
    sy -= sy_offset;
    wy_dim1 = *n;
    wy_offset = 1 + wy_dim1;
    wy -= wy_offset;
    ws_dim1 = *n;
    ws_offset = 1 + ws_dim1;
    ws -= ws_offset;

    /* Function Body */
    if (! (*cnstnd) && *col > 0) {
/*<          do 26 i = 1, n >*/
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	    r(i) = -g(i) >*/
	    r__[i__] = -g[i__];
/*<   26     continue >*/
/* L26: */
	}
/*<       else >*/
    } else {
/*<          do 30 i = 1, nfree >*/
	i__1 = *nfree;
	for (i__ = 1; i__ <= i__1; ++i__) {
/*<             k = index(i) >*/
	    k = index[i__];
/*< 	    r(i) = -theta*(z(k) - x(k)) - g(k) >*/
	    r__[i__] = -(*theta) * (z__[k] - x[k]) - g[k];
/*<   30     continue >*/
/* L30: */
	}
/*<      	 call bmv(m,sy,wt,col,wa(2*m+1),wa(1),info) >*/
	bmv_(m, &sy[sy_offset], &wt[wt_offset], col, &wa[(*m << 1) + 1], &wa[
		1], info);
/*<          if (info .ne. 0) then >*/
	if (*info != 0) {
/*<             info = -8 >*/
	    *info = -8;
/*< 	    return >*/
	    return 0;
/*<          endif >*/
	}
/*<      	 pointr = head  >*/
	pointr = *head;
/*<      	 do 34 j = 1, col >*/
	i__1 = *col;
	for (j = 1; j <= i__1; ++j) {
/*<        	    a1 = wa(j) >*/
	    a1 = wa[j];
/*<             a2 = theta*wa(col + j) >*/
	    a2 = *theta * wa[*col + j];
/*< 	    do 32 i = 1, nfree >*/
	    i__2 = *nfree;
	    for (i__ = 1; i__ <= i__2; ++i__) {
/*< 	       k = index(i) >*/
		k = index[i__];
/*< 	       r(i) = r(i) + wy(k,pointr)*a1 + ws(k,pointr)*a2 >*/
		r__[i__] = r__[i__] + wy[k + pointr * wy_dim1] * a1 + ws[k + 
			pointr * ws_dim1] * a2;
/*<   32        continue >*/
/* L32: */
	    }
/*< 	    pointr = mod(pointr,m) + 1 >*/
	    pointr = pointr % *m + 1;
/*<   34     continue >*/
/* L34: */
	}
/*<       endif >*/
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* cmprlb_ */

/* ======================= The end of cmprlb ============================= */
/*<       subroutine errclb(n, m, factr, l, u, nbd, task, info, k) >*/
/* Subroutine */ int errclb_(integer *n, integer *m, doublereal *factr, 
	doublereal *l, doublereal *u, integer *nbd, char *task, integer *info,
	 integer *k, ftnlen task_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__;

    (void)task_len;

/*<       character*60     task >*/
/*<       integer          n, m, info, k, nbd(n) >*/
/*<       double precision factr, l(n), u(n) >*/
/*     ************ */

/*     Subroutine errclb */

/*     This subroutine checks the validity of the input data. */


/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ************ */
/*<       integer          i >*/
/*<       double precision one,zero >*/
/*<       parameter        (one=1.0d0,zero=0.0d0) >*/
/*     Check the input arguments for errors. */
/*<       if (n .le. 0) task = 'ERROR: N .LE. 0' >*/
    /* Parameter adjustments */
    --nbd;
    --u;
    --l;

    /* Function Body */
    if (*n <= 0) {
	s_copy(task, "ERROR: N .LE. 0", (ftnlen)60, (ftnlen)15);
    }
/*<       if (m .le. 0) task = 'ERROR: M .LE. 0' >*/
    if (*m <= 0) {
	s_copy(task, "ERROR: M .LE. 0", (ftnlen)60, (ftnlen)15);
    }
/*<       if (factr .lt. zero) task = 'ERROR: FACTR .LT. 0' >*/
    if (*factr < 0.) {
	s_copy(task, "ERROR: FACTR .LT. 0", (ftnlen)60, (ftnlen)19);
    }
/*     Check the validity of the arrays nbd(i), u(i), and l(i). */
/*<       do 10 i = 1, n >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          if (nbd(i) .lt. 0 .or. nbd(i) .gt. 3) then >*/
	if (nbd[i__] < 0 || nbd[i__] > 3) {
/*                                                   return */
/*<             task = 'ERROR: INVALID NBD' >*/
	    s_copy(task, "ERROR: INVALID NBD", (ftnlen)60, (ftnlen)18);
/*< 	    info = -6 >*/
	    *info = -6;
/*< 	    k = i >*/
	    *k = i__;
/*<          endif >*/
	}
/*< 	 if (nbd(i) .eq. 2) then >*/
	if (nbd[i__] == 2) {
/*< 	    if (l(i) .gt. u(i)) then >*/
	    if (l[i__] > u[i__]) {
/*                                    return */
/*<                task = 'ERROR: NO FEASIBLE SOLUTION' >*/
		s_copy(task, "ERROR: NO FEASIBLE SOLUTION", (ftnlen)60, (
			ftnlen)27);
/*< 	       info = -7 >*/
		*info = -7;
/*< 	       k = i >*/
		*k = i__;
/*<             endif >*/
	    }
/*<          endif >*/
	}
/*<   10  continue >*/
/* L10: */
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* errclb_ */

/* ======================= The end of errclb ============================= */
/*<    >*/
/* Subroutine */ int formk_(integer *n, integer *nsub, integer *ind, integer *
	nenter, integer *ileave, integer *indx2, integer *iupdat, logical *
	updatd, doublereal *wn, doublereal *wn1, integer *m, doublereal *ws, 
	doublereal *wy, doublereal *sy, doublereal *theta, integer *col, 
	integer *head, integer *info)
{
    /* System generated locals */
    integer wn_dim1, wn_offset, wn1_dim1, wn1_offset, ws_dim1, ws_offset, 
	    wy_dim1, wy_offset, sy_dim1, sy_offset, i__1, i__2, i__3;

    /* Local variables */
    integer i__, k, k1, m2, is, js, iy, jy, is1, js1, col2, dend, pend;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    integer upcl;
    doublereal temp1, temp2, temp3, temp4;
    extern /* Subroutine */ int dpofa_(doublereal *, integer *, integer *, 
	    integer *), dcopy_(integer *, doublereal *, integer *, doublereal 
	    *, integer *), dtrsl_(doublereal *, integer *, integer *, 
	    doublereal *, integer *, integer *);
    integer ipntr, jpntr, dbegin, pbegin;

/*<    >*/
/*<    >*/
/*<       logical          updatd >*/
/*     ************ */

/*     Subroutine formk */

/*     This subroutine forms  the LEL^T factorization of the indefinite */

/*       matrix    K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ] */
/*                     [L_a -R_z           theta*S'AA'S ] */
/*                                                    where E = [-I  0] */
/*                                                              [ 0  I] */
/*     The matrix K can be shown to be equal to the matrix M^[-1]N */
/*       occurring in section 5.1 of [1], as well as to the matrix */
/*       Mbar^[-1] Nbar in section 5.3. */

/*     n is an integer variable. */
/*       On entry n is the dimension of the problem. */
/*       On exit n is unchanged. */

/*     nsub is an integer variable */
/*       On entry nsub is the number of subspace variables in free set. */
/*       On exit nsub is not changed. */

/*     ind is an integer array of dimension nsub. */
/*       On entry ind specifies the indices of subspace variables. */
/*       On exit ind is unchanged. */

/*     nenter is an integer variable. */
/*       On entry nenter is the number of variables entering the */
/*         free set. */
/*       On exit nenter is unchanged. */

/*     ileave is an integer variable. */
/*       On entry indx2(ileave),...,indx2(n) are the variables leaving */
/*         the free set. */
/*       On exit ileave is unchanged. */

/*     indx2 is an integer array of dimension n. */
/*       On entry indx2(1),...,indx2(nenter) are the variables entering */
/*         the free set, while indx2(ileave),...,indx2(n) are the */
/*         variables leaving the free set. */
/*       On exit indx2 is unchanged. */

/*     iupdat is an integer variable. */
/*       On entry iupdat is the total number of BFGS updates made so far. */
/*       On exit iupdat is unchanged. */

/*     updatd is a logical variable. */
/*       On entry 'updatd' is true if the L-BFGS matrix is updatd. */
/*       On exit 'updatd' is unchanged. */

/*     wn is a double precision array of dimension 2m x 2m. */
/*       On entry wn is unspecified. */
/*       On exit the upper triangle of wn stores the LEL^T factorization */
/*         of the 2*col x 2*col indefinite matrix */
/*                     [-D -Y'ZZ'Y/theta     L_a'-R_z'  ] */
/*                     [L_a -R_z           theta*S'AA'S ] */

/*     wn1 is a double precision array of dimension 2m x 2m. */
/*       On entry wn1 stores the lower triangular part of */
/*                     [Y' ZZ'Y   L_a'+R_z'] */
/*                     [L_a+R_z   S'AA'S   ] */
/*         in the previous iteration. */
/*       On exit wn1 stores the corresponding updated matrices. */
/*       The purpose of wn1 is just to store these inner products */
/*       so they can be easily updated and inserted into wn. */

/*     m is an integer variable. */
/*       On entry m is the maximum number of variable metric corrections */
/*         used to define the limited memory matrix. */
/*       On exit m is unchanged. */

/*     ws, wy, sy, and wtyy are double precision arrays; */
/*     theta is a double precision variable; */
/*     col is an integer variable; */
/*     head is an integer variable. */
/*       On entry they store the information defining the */
/*                                          limited memory BFGS matrix: */
/*         ws(n,m) stores S, a set of s-vectors; */
/*         wy(n,m) stores Y, a set of y-vectors; */
/*         sy(m,m) stores S'Y; */
/*         wtyy(m,m) stores the Cholesky factorization */
/*                                   of (theta*S'S+LD^(-1)L') */
/*         theta is the scaling factor specifying B_0 = theta I; */
/*         col is the number of variable metric corrections stored; */
/*         head is the location of the 1st s- (or y-) vector in S (or Y). */
/*       On exit they are unchanged. */

/*     info is an integer variable. */
/*       On entry info is unspecified. */
/*       On exit info =  0 for normal return; */
/*                    = -1 when the 1st Cholesky factorization failed; */
/*                    = -2 when the 2st Cholesky factorization failed. */

/*     Subprograms called: */

/*       Linpack ... dcopy, dpofa, dtrsl. */


/*     References: */
/*       [1] R. H. Byrd, P. Lu, J. Nocedal and C. Zhu, ``A limited */
/*       memory algorithm for bound constrained optimization'', */
/*       SIAM J. Scientific Computing 16 (1995), no. 5, pp. 1190--1208. */

/*       [2] C. Zhu, R.H. Byrd, P. Lu, J. Nocedal, ``L-BFGS-B: a */
/*       limited memory FORTRAN code for solving bound constrained */
/*       optimization problems'', Tech. Report, NAM-11, EECS Department, */
/*       Northwestern University, 1994. */

/*       (Postscript files of these papers are available via anonymous */
/*        ftp to eecs.nwu.edu in the directory pub/lbfgs/lbfgs_bcm.) */

/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ************ */
/*<    >*/
/*<       double precision ddot,temp1,temp2,temp3,temp4 >*/
/*<       double precision one,zero >*/
/*<       parameter        (one=1.0d0,zero=0.0d0) >*/
/*     Form the lower triangular part of */
/*               WN1 = [Y' ZZ'Y   L_a'+R_z'] */
/*                     [L_a+R_z   S'AA'S   ] */
/*        where L_a is the strictly lower triangular part of S'AA'Y */
/*              R_z is the upper triangular part of S'ZZ'Y. */
/*<       if (updatd) then >*/
    /* Parameter adjustments */
    --indx2;
    --ind;
    sy_dim1 = *m;
    sy_offset = 1 + sy_dim1;
    sy -= sy_offset;
    wy_dim1 = *n;
    wy_offset = 1 + wy_dim1;
    wy -= wy_offset;
    ws_dim1 = *n;
    ws_offset = 1 + ws_dim1;
    ws -= ws_offset;
    wn1_dim1 = 2 * *m;
    wn1_offset = 1 + wn1_dim1;
    wn1 -= wn1_offset;
    wn_dim1 = 2 * *m;
    wn_offset = 1 + wn_dim1;
    wn -= wn_offset;

    /* Function Body */
    if (*updatd) {
/*<          if (iupdat .gt. m) then  >*/
	if (*iupdat > *m) {
/*                                 shift old part of WN1. */
/*<             do 10 jy = 1, m - 1 >*/
	    i__1 = *m - 1;
	    for (jy = 1; jy <= i__1; ++jy) {
/*<                js = m + jy >*/
		js = *m + jy;
/*< 	       call dcopy(m-jy,wn1(jy+1,jy+1),1,wn1(jy,jy),1) >*/
		i__2 = *m - jy;
		dcopy_(&i__2, &wn1[jy + 1 + (jy + 1) * wn1_dim1], &c__1, &wn1[
			jy + jy * wn1_dim1], &c__1);
/*<  	       call dcopy(m-jy,wn1(js+1,js+1),1,wn1(js,js),1) >*/
		i__2 = *m - jy;
		dcopy_(&i__2, &wn1[js + 1 + (js + 1) * wn1_dim1], &c__1, &wn1[
			js + js * wn1_dim1], &c__1);
/*<  	       call dcopy(m-1,wn1(m+2,jy+1),1,wn1(m+1,jy),1) >*/
		i__2 = *m - 1;
		dcopy_(&i__2, &wn1[*m + 2 + (jy + 1) * wn1_dim1], &c__1, &wn1[
			*m + 1 + jy * wn1_dim1], &c__1);
/*<   10        continue >*/
/* L10: */
	    }
/*<          endif >*/
	}
/*          put new rows in blocks (1,1), (2,1) and (2,2). */
/*<          pbegin = 1 >*/
	pbegin = 1;
/*< 	 pend = nsub >*/
	pend = *nsub;
/*<          dbegin = nsub + 1 >*/
	dbegin = *nsub + 1;
/*< 	 dend = n >*/
	dend = *n;
/*<          iy = col >*/
	iy = *col;
/*<          is = m + col >*/
	is = *m + *col;
/*<          ipntr = head + col - 1 >*/
	ipntr = *head + *col - 1;
/*<          if (ipntr .gt. m) ipntr = ipntr - m	 >*/
	if (ipntr > *m) {
	    ipntr -= *m;
	}
/*<          jpntr = head >*/
	jpntr = *head;
/*<          do 20 jy = 1, col >*/
	i__1 = *col;
	for (jy = 1; jy <= i__1; ++jy) {
/*<             js = m + jy >*/
	    js = *m + jy;
/*<             temp1 = zero >*/
	    temp1 = 0.;
/*< 	    temp2 = zero >*/
	    temp2 = 0.;
/*< 	    temp3 = zero >*/
	    temp3 = 0.;
/*             compute element jy of row 'col' of Y'ZZ'Y */
/*< 	    do 15 k = pbegin, pend >*/
	    i__2 = pend;
	    for (k = pbegin; k <= i__2; ++k) {
/*< 	       k1 = ind(k) >*/
		k1 = ind[k];
/*< 	       temp1 = temp1 + wy(k1,ipntr)*wy(k1,jpntr) >*/
		temp1 += wy[k1 + ipntr * wy_dim1] * wy[k1 + jpntr * wy_dim1];
/*<   15        continue >*/
/* L15: */
	    }
/*             compute elements jy of row 'col' of L_a and S'AA'S */
/*< 	    do 16 k = dbegin, dend >*/
	    i__2 = dend;
	    for (k = dbegin; k <= i__2; ++k) {
/*< 	       k1 = ind(k) >*/
		k1 = ind[k];
/*< 	       temp2 = temp2 + ws(k1,ipntr)*ws(k1,jpntr) >*/
		temp2 += ws[k1 + ipntr * ws_dim1] * ws[k1 + jpntr * ws_dim1];
/*< 	       temp3 = temp3 + ws(k1,ipntr)*wy(k1,jpntr) >*/
		temp3 += ws[k1 + ipntr * ws_dim1] * wy[k1 + jpntr * wy_dim1];
/*<   16        continue >*/
/* L16: */
	    }
/*< 	    wn1(iy,jy) = temp1 >*/
	    wn1[iy + jy * wn1_dim1] = temp1;
/*< 	    wn1(is,js) = temp2 >*/
	    wn1[is + js * wn1_dim1] = temp2;
/*< 	    wn1(is,jy) = temp3 >*/
	    wn1[is + jy * wn1_dim1] = temp3;
/*<             jpntr = mod(jpntr,m) + 1 >*/
	    jpntr = jpntr % *m + 1;
/*<   20     continue >*/
/* L20: */
	}
/*          put new column in block (2,1). */
/*<          jy = col	 >*/
	jy = *col;
/*<          jpntr = head + col - 1 >*/
	jpntr = *head + *col - 1;
/*<          if (jpntr .gt. m) jpntr = jpntr - m >*/
	if (jpntr > *m) {
	    jpntr -= *m;
	}
/*<          ipntr = head >*/
	ipntr = *head;
/*<          do 30 i = 1, col >*/
	i__1 = *col;
	for (i__ = 1; i__ <= i__1; ++i__) {
/*<             is = m + i >*/
	    is = *m + i__;
/*< 	    temp3 = zero >*/
	    temp3 = 0.;
/*             compute element i of column 'col' of R_z */
/*< 	    do 25 k = pbegin, pend >*/
	    i__2 = pend;
	    for (k = pbegin; k <= i__2; ++k) {
/*< 	       k1 = ind(k) >*/
		k1 = ind[k];
/*< 	       temp3 = temp3 + ws(k1,ipntr)*wy(k1,jpntr) >*/
		temp3 += ws[k1 + ipntr * ws_dim1] * wy[k1 + jpntr * wy_dim1];
/*<   25        continue  >*/
/* L25: */
	    }
/*< 	    ipntr = mod(ipntr,m) + 1 >*/
	    ipntr = ipntr % *m + 1;
/*<             wn1(is,jy) = temp3 >*/
	    wn1[is + jy * wn1_dim1] = temp3;
/*<   30     continue >*/
/* L30: */
	}
/*< 	 upcl = col - 1 >*/
	upcl = *col - 1;
/*<       else >*/
    } else {
/*<          upcl = col >*/
	upcl = *col;
/*<       endif >*/
    }
/*       modify the old parts in blocks (1,1) and (2,2) due to changes */
/*       in the set of free variables. */
/*<       ipntr = head	 >*/
    ipntr = *head;
/*<       do 45 iy = 1, upcl >*/
    i__1 = upcl;
    for (iy = 1; iy <= i__1; ++iy) {
/*<          is = m + iy >*/
	is = *m + iy;
/*< 	 jpntr = head >*/
	jpntr = *head;
/*<       	 do 40 jy = 1, iy >*/
	i__2 = iy;
	for (jy = 1; jy <= i__2; ++jy) {
/*< 	    js = m + jy >*/
	    js = *m + jy;
/*< 	    temp1 = zero >*/
	    temp1 = 0.;
/*< 	    temp2 = zero >*/
	    temp2 = 0.;
/*< 	    temp3 = zero >*/
	    temp3 = 0.;
/*< 	    temp4 = zero >*/
	    temp4 = 0.;
/*< 	    do 35 k = 1, nenter >*/
	    i__3 = *nenter;
	    for (k = 1; k <= i__3; ++k) {
/*< 	       k1 = indx2(k) >*/
		k1 = indx2[k];
/*< 	       temp1 = temp1 + wy(k1,ipntr)*wy(k1,jpntr) >*/
		temp1 += wy[k1 + ipntr * wy_dim1] * wy[k1 + jpntr * wy_dim1];
/*< 	       temp2 = temp2 + ws(k1,ipntr)*ws(k1,jpntr) >*/
		temp2 += ws[k1 + ipntr * ws_dim1] * ws[k1 + jpntr * ws_dim1];
/*<   35        continue >*/
/* L35: */
	    }
/*< 	    do 36 k = ileave, n >*/
	    i__3 = *n;
	    for (k = *ileave; k <= i__3; ++k) {
/*< 	       k1 = indx2(k) >*/
		k1 = indx2[k];
/*< 	       temp3 = temp3 + wy(k1,ipntr)*wy(k1,jpntr) >*/
		temp3 += wy[k1 + ipntr * wy_dim1] * wy[k1 + jpntr * wy_dim1];
/*< 	       temp4 = temp4 + ws(k1,ipntr)*ws(k1,jpntr) >*/
		temp4 += ws[k1 + ipntr * ws_dim1] * ws[k1 + jpntr * ws_dim1];
/*<   36        continue >*/
/* L36: */
	    }
/*< 	    wn1(iy,jy) = wn1(iy,jy) + temp1 - temp3  >*/
	    wn1[iy + jy * wn1_dim1] = wn1[iy + jy * wn1_dim1] + temp1 - temp3;
/*< 	    wn1(is,js) = wn1(is,js) - temp2 + temp4  >*/
	    wn1[is + js * wn1_dim1] = wn1[is + js * wn1_dim1] - temp2 + temp4;
/*< 	    jpntr = mod(jpntr,m) + 1 >*/
	    jpntr = jpntr % *m + 1;
/*<   40     continue >*/
/* L40: */
	}
/*<          ipntr = mod(ipntr,m) + 1 >*/
	ipntr = ipntr % *m + 1;
/*<   45  continue >*/
/* L45: */
    }
/*       modify the old parts in block (2,1). */
/*<       ipntr = head       >*/
    ipntr = *head;
/*<       do 60 is = m + 1, m + upcl >*/
    i__1 = *m + upcl;
    for (is = *m + 1; is <= i__1; ++is) {
/*<          jpntr = head  >*/
	jpntr = *head;
/*<          do 55 jy = 1, upcl >*/
	i__2 = upcl;
	for (jy = 1; jy <= i__2; ++jy) {
/*<             temp1 = zero >*/
	    temp1 = 0.;
/*< 	    temp3 = zero >*/
	    temp3 = 0.;
/*< 	    do 50 k = 1, nenter >*/
	    i__3 = *nenter;
	    for (k = 1; k <= i__3; ++k) {
/*< 	       k1 = indx2(k) >*/
		k1 = indx2[k];
/*< 	       temp1 = temp1 + ws(k1,ipntr)*wy(k1,jpntr) >*/
		temp1 += ws[k1 + ipntr * ws_dim1] * wy[k1 + jpntr * wy_dim1];
/*<   50	    continue >*/
/* L50: */
	    }
/*< 	    do 51 k = ileave, n >*/
	    i__3 = *n;
	    for (k = *ileave; k <= i__3; ++k) {
/*< 	       k1 = indx2(k) >*/
		k1 = indx2[k];
/*< 	       temp3 = temp3 + ws(k1,ipntr)*wy(k1,jpntr) >*/
		temp3 += ws[k1 + ipntr * ws_dim1] * wy[k1 + jpntr * wy_dim1];
/*<   51	    continue >*/
/* L51: */
	    }
/*<          if (is .le. jy + m) then >*/
	    if (is <= jy + *m) {
/*< 	       wn1(is,jy) = wn1(is,jy) + temp1 - temp3   >*/
		wn1[is + jy * wn1_dim1] = wn1[is + jy * wn1_dim1] + temp1 - 
			temp3;
/*< 	    else >*/
	    } else {
/*< 	       wn1(is,jy) = wn1(is,jy) - temp1 + temp3   >*/
		wn1[is + jy * wn1_dim1] = wn1[is + jy * wn1_dim1] - temp1 + 
			temp3;
/*< 	    endif >*/
	    }
/*< 	    jpntr = mod(jpntr,m) + 1 >*/
	    jpntr = jpntr % *m + 1;
/*<   55     continue >*/
/* L55: */
	}
/*<          ipntr = mod(ipntr,m) + 1 >*/
	ipntr = ipntr % *m + 1;
/*<   60  continue >*/
/* L60: */
    }
/*     Form the upper triangle of WN = [D+Y' ZZ'Y/theta   -L_a'+R_z' ] */
/*                                     [-L_a +R_z        S'AA'S*theta] */
/*<       m2 = 2*m >*/
    m2 = *m << 1;
/*<       do 70 iy = 1, col >*/
    i__1 = *col;
    for (iy = 1; iy <= i__1; ++iy) {
/*< 	 is = col + iy >*/
	is = *col + iy;
/*< 	 is1 = m + iy >*/
	is1 = *m + iy;
/*<       	 do 65 jy = 1, iy >*/
	i__2 = iy;
	for (jy = 1; jy <= i__2; ++jy) {
/*< 	    js = col + jy >*/
	    js = *col + jy;
/*<             js1 = m + jy >*/
	    js1 = *m + jy;
/*<  	    wn(jy,iy) = wn1(iy,jy)/theta >*/
	    wn[jy + iy * wn_dim1] = wn1[iy + jy * wn1_dim1] / *theta;
/*<  	    wn(js,is) = wn1(is1,js1)*theta >*/
	    wn[js + is * wn_dim1] = wn1[is1 + js1 * wn1_dim1] * *theta;
/*<   65     continue >*/
/* L65: */
	}
/*<       	 do 66 jy = 1, iy - 1 >*/
	i__2 = iy - 1;
	for (jy = 1; jy <= i__2; ++jy) {
/*<  	    wn(jy,is) = -wn1(is1,jy) >*/
	    wn[jy + is * wn_dim1] = -wn1[is1 + jy * wn1_dim1];
/*<   66     continue >*/
/* L66: */
	}
/*<       	 do 67 jy = iy, col >*/
	i__2 = *col;
	for (jy = iy; jy <= i__2; ++jy) {
/*<  	    wn(jy,is) = wn1(is1,jy) >*/
	    wn[jy + is * wn_dim1] = wn1[is1 + jy * wn1_dim1];
/*<   67     continue >*/
/* L67: */
	}
/*<  	 wn(iy,iy) = wn(iy,iy) + sy(iy,iy) >*/
	wn[iy + iy * wn_dim1] += sy[iy + iy * sy_dim1];
/*<   70  continue >*/
/* L70: */
    }
/*     Form the upper triangle of WN= [  LL'            L^-1(-L_a'+R_z')] */
/*                                    [(-L_a +R_z)L'^-1   S'AA'S*theta  ] */
/*        first Cholesky factor (1,1) block of wn to get LL' */
/*                          with L' stored in the upper triangle of wn. */
/*<       call dpofa(wn,m2,col,info) >*/
    dpofa_(&wn[wn_offset], &m2, col, info);
/*<       if (info .ne. 0) then >*/
    if (*info != 0) {
/*< 	 info = -1 >*/
	*info = -1;
/*< 	 return >*/
	return 0;
/*<       endif >*/
    }
/*        then form L^-1(-L_a'+R_z') in the (1,2) block. */
/*<       col2 = 2*col >*/
    col2 = *col << 1;
/*<       do 71 js = col+1 ,col2 >*/
    i__1 = col2;
    for (js = *col + 1; js <= i__1; ++js) {
/*<          call dtrsl(wn,m2,col,wn(1,js),11,info) >*/
	dtrsl_(&wn[wn_offset], &m2, col, &wn[js * wn_dim1 + 1], &c__11, info);
/*<   71  continue >*/
/* L71: */
    }
/*     Form S'AA'S*theta + (L^-1(-L_a'+R_z'))'L^-1(-L_a'+R_z') in the */
/*        upper triangle of (2,2) block of wn. */
/*<       do 72 is = col+1, col2 >*/
    i__1 = col2;
    for (is = *col + 1; is <= i__1; ++is) {
/*<          do 74 js = is, col2 >*/
	i__2 = col2;
	for (js = is; js <= i__2; ++js) {
/*< 	       wn(is,js) = wn(is,js) + ddot(col,wn(1,is),1,wn(1,js),1) >*/
	    wn[is + js * wn_dim1] += ddot_(col, &wn[is * wn_dim1 + 1], &c__1, 
		    &wn[js * wn_dim1 + 1], &c__1);
/*<   74        continue >*/
/* L74: */
	}
/*<   72     continue >*/
/* L72: */
    }
/*     Cholesky factorization of (2,2) block of wn. */
/*<       call dpofa(wn(col+1,col+1),m2,col,info) >*/
    dpofa_(&wn[*col + 1 + (*col + 1) * wn_dim1], &m2, col, info);
/*<       if (info .ne. 0) then >*/
    if (*info != 0) {
/*< 	 info = -2 >*/
	*info = -2;
/*< 	 return >*/
	return 0;
/*<       endif >*/
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* formk_ */

/* ======================= The end of formk ============================== */
/*<       subroutine formt(m, wt, sy, ss, col, theta, info) >*/
/* Subroutine */ int formt_(integer *m, doublereal *wt, doublereal *sy, 
	doublereal *ss, integer *col, doublereal *theta, integer *info)
{
    /* System generated locals */
    integer wt_dim1, wt_offset, sy_dim1, sy_offset, ss_dim1, ss_offset, i__1, 
	    i__2, i__3;

    /* Local variables */
    integer i__, j, k, k1;
    doublereal ddum;
    extern /* Subroutine */ int dpofa_(doublereal *, integer *, integer *, 
	    integer *);

/*<       integer          m, col, info >*/
/*<       double precision theta, wt(m, m), sy(m, m), ss(m, m) >*/
/*     ************ */

/*     Subroutine formt */

/*       This subroutine forms the upper half of the pos. def. and symm. */
/*         T = theta*SS + L*D^(-1)*L', stores T in the upper triangle */
/*         of the array wt, and performs the Cholesky factorization of T */
/*         to produce J*J', with J' stored in the upper triangle of wt. */

/*     Subprograms called: */

/*       Linpack ... dpofa. */


/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ************ */
/*<       integer          i,j,k,k1 >*/
/*<       double precision ddum >*/
/*<       double precision zero >*/
/*<       parameter        (zero=0.0d0) >*/
/*     Form the upper half of  T = theta*SS + L*D^(-1)*L', */
/*        store T in the upper triangle of the array wt. */
/*<       do 52 j = 1, col >*/
    /* Parameter adjustments */
    ss_dim1 = *m;
    ss_offset = 1 + ss_dim1;
    ss -= ss_offset;
    sy_dim1 = *m;
    sy_offset = 1 + sy_dim1;
    sy -= sy_offset;
    wt_dim1 = *m;
    wt_offset = 1 + wt_dim1;
    wt -= wt_offset;

    /* Function Body */
    i__1 = *col;
    for (j = 1; j <= i__1; ++j) {
/*<       	 wt(1,j) = theta*ss(1,j) >*/
	wt[j * wt_dim1 + 1] = *theta * ss[j * ss_dim1 + 1];
/*<   52  continue >*/
/* L52: */
    }
/*<       do 55 i = 2, col >*/
    i__1 = *col;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*< 	 do 54 j = i, col >*/
	i__2 = *col;
	for (j = i__; j <= i__2; ++j) {
/*<             k1 = min(i,j) - 1 >*/
	    k1 = min(i__,j) - 1;
/*<             ddum  = zero >*/
	    ddum = 0.;
/*<             do 53 k = 1, k1 >*/
	    i__3 = k1;
	    for (k = 1; k <= i__3; ++k) {
/*<                ddum  = ddum + sy(i,k)*sy(j,k)/sy(k,k) >*/
		ddum += sy[i__ + k * sy_dim1] * sy[j + k * sy_dim1] / sy[k + 
			k * sy_dim1];
/*<   53        continue >*/
/* L53: */
	    }
/*<             wt(i,j) = ddum + theta*ss(i,j) >*/
	    wt[i__ + j * wt_dim1] = ddum + *theta * ss[i__ + j * ss_dim1];
/*<   54     continue >*/
/* L54: */
	}
/*<   55  continue >*/
/* L55: */
    }
/*     Cholesky factorize T to J*J' with */
/*        J' stored in the upper triangle of wt. */
/*<       call dpofa(wt,m,col,info) >*/
    dpofa_(&wt[wt_offset], m, col, info);
/*<       if (info .ne. 0) then >*/
    if (*info != 0) {
/*<          info = -3 >*/
	*info = -3;
/*<       endif >*/
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* formt_ */

/* ======================= The end of formt ============================== */
/*<    >*/
/* Subroutine */ int freev_(integer *n, integer *nfree, integer *index, 
	integer *nenter, integer *ileave, integer *indx2, integer *iwhere, 
	logical *wrk, logical *updatd, logical *cnstnd, integer *iprint, 
	integer *iter)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__, k, iact;

/*<    >*/
/*<       logical wrk, updatd, cnstnd >*/
/*     ************ */

/*     Subroutine freev */

/*     This subroutine counts the entering and leaving variables when */
/*       iter > 0, and finds the index set of free and active variables */
/*       at the GCP. */

/*     cnstnd is a logical variable indicating whether bounds are present */

/*     index is an integer array of dimension n */
/*       for i=1,...,nfree, index(i) are the indices of free variables */
/*       for i=nfree+1,...,n, index(i) are the indices of bound variables */
/*       On entry after the first iteration, index gives */
/*         the free variables at the previous iteration. */
/*       On exit it gives the free variables based on the determination */
/*         in cauchy using the array iwhere. */

/*     indx2 is an integer array of dimension n */
/*       On entry indx2 is unspecified. */
/*       On exit with iter>0, indx2 indicates which variables */
/*          have changed status since the previous iteration. */
/*       For i= 1,...,nenter, indx2(i) have changed from bound to free. */
/*       For i= ileave+1,...,n, indx2(i) have changed from free to bound. */


/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ************ */
/*<       integer iact,i,k >*/
/*<       nenter = 0 >*/
    /* Parameter adjustments */
    --iwhere;
    --indx2;
    --index;

    /* Function Body */
    *nenter = 0;
/*<       ileave = n + 1 >*/
    *ileave = *n + 1;
/*<       if (iter .gt. 0 .and. cnstnd) then >*/
    if (*iter > 0 && *cnstnd) {
/*                           count the entering and leaving variables. */
/*< 	 do 20 i = 1, nfree >*/
	i__1 = *nfree;
	for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	    k = index(i) >*/
	    k = index[i__];
/*< 	    if (iwhere(k) .gt. 0) then >*/
	    if (iwhere[k] > 0) {
/*< 	       ileave = ileave - 1 >*/
		--(*ileave);
/*< 	       indx2(ileave) = k >*/
		indx2[*ileave] = k;
/*< 	  >*/
		if (*iprint >= 100) {
                    printf("Variable %ld leaves the set of free variables\n", k);
		}
/*<             endif >*/
	    }
/*<   20     continue >*/
/* L20: */
	}
/*< 	 do 22 i = 1 + nfree, n >*/
	i__1 = *n;
	for (i__ = *nfree + 1; i__ <= i__1; ++i__) {
/*< 	    k = index(i) >*/
	    k = index[i__];
/*< 	    if (iwhere(k) .le. 0) then >*/
	    if (iwhere[k] <= 0) {
/*< 	       nenter = nenter + 1 >*/
		++(*nenter);
/*< 	       indx2(nenter) = k >*/
		indx2[*nenter] = k;
/*< 	  >*/
		if (*iprint >= 100) {
                    printf("Variable %ld enters the set of free variables\n", k);
		}
/*<             endif >*/
	    }
/*<   22     continue >*/
/* L22: */
	}
/*<    >*/
	if (*iprint >= 99) {
            i__1 = *n + 1 - *ileave;
            printf("%ld variables leave; %ld variables enter\n", i__1, *nenter);
	}
/*<       endif >*/
    }
/*<       wrk = (ileave .lt. n+1) .or. (nenter .gt. 0) .or. updatd >*/
    *wrk = *ileave < *n + 1 || *nenter > 0 || *updatd;
/*     Find the index set of free and active variables at the GCP. */
/*<       nfree = 0  >*/
    *nfree = 0;
/*<       iact = n + 1 >*/
    iact = *n + 1;
/*<       do 24 i = 1, n >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	 if (iwhere(i) .le. 0) then >*/
	if (iwhere[i__] <= 0) {
/*< 	    nfree = nfree + 1 >*/
	    ++(*nfree);
/*< 	    index(nfree) = i >*/
	    index[*nfree] = i__;
/*<          else >*/
	} else {
/*<             iact = iact - 1 >*/
	    --iact;
/*<             index(iact) = i >*/
	    index[iact] = i__;
/*<          endif >*/
	}
/*<   24  continue >*/
/* L24: */
    }
/*<    >*/
    if (*iprint >= 99) {
	i__1 = *iter + 1;
        printf("%ld variables are free at GCP %ld\n", *nfree, i__1);
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* freev_ */

/* ======================= The end of freev ============================== */
/*<       subroutine hpsolb(n, t, iorder, iheap) >*/
/* Subroutine */ int hpsolb_(integer *n, doublereal *t, integer *iorder, 
	integer *iheap)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__, j, k;
    doublereal out, ddum;
    integer indxin, indxou;

/*<       integer          iheap, n, iorder(n) >*/
/*<       double precision t(n) >*/
/*     ************ */

/*     Subroutine hpsolb */

/*     This subroutine sorts out the least element of t, and puts the */
/*       remaining elements of t in a heap. */

/*     n is an integer variable. */
/*       On entry n is the dimension of the arrays t and iorder. */
/*       On exit n is unchanged. */

/*     t is a double precision array of dimension n. */
/*       On entry t stores the elements to be sorted, */
/*       On exit t(n) stores the least elements of t, and t(1) to t(n-1) */
/*         stores the remaining elements in the form of a heap. */

/*     iorder is an integer array of dimension n. */
/*       On entry iorder(i) is the index of t(i). */
/*       On exit iorder(i) is still the index of t(i), but iorder may be */
/*         permuted in accordance with t. */

/*     iheap is an integer variable specifying the task. */
/*       On entry iheap should be set as follows: */
/*         iheap .eq. 0 if t(1) to t(n) is not in the form of a heap, */
/*         iheap .ne. 0 if otherwise. */
/*       On exit iheap is unchanged. */


/*     References: */
/*       Algorithm 232 of CACM (J. W. J. Williams): HEAPSORT. */

/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */

/*     ************ */
/*<       integer          i,j,k,indxin,indxou >*/
/*<       double precision ddum,out >*/
/*<       if (iheap .eq. 0) then >*/
    /* Parameter adjustments */
    --iorder;
    --t;

    /* Function Body */
    if (*iheap == 0) {
/*        Rearrange the elements t(1) to t(n) to form a heap. */
/*<          do 20 k = 2, n >*/
	i__1 = *n;
	for (k = 2; k <= i__1; ++k) {
/*<             ddum  = t(k) >*/
	    ddum = t[k];
/*<             indxin = iorder(k) >*/
	    indxin = iorder[k];
/*           Add ddum to the heap. */
/*<             i = k >*/
	    i__ = k;
/*<    10       continue >*/
L10:
/*<             if (i.gt.1) then >*/
	    if (i__ > 1) {
/*<                j = i/2 >*/
		j = i__ / 2;
/*<                if (ddum .lt. t(j)) then >*/
		if (ddum < t[j]) {
/*<                   t(i) = t(j) >*/
		    t[i__] = t[j];
/*<                   iorder(i) = iorder(j) >*/
		    iorder[i__] = iorder[j];
/*<                   i = j >*/
		    i__ = j;
/*<                   goto 10  >*/
		    goto L10;
/*<                endif   >*/
		}
/*<             endif   >*/
	    }
/*<             t(i) = ddum >*/
	    t[i__] = ddum;
/*<             iorder(i) = indxin >*/
	    iorder[i__] = indxin;
/*<    20    continue >*/
/* L20: */
	}
/*<       endif >*/
    }
/*     Assign to 'out' the value of t(1), the least member of the heap, */
/*        and rearrange the remaining members to form a heap as */
/*        elements 1 to n-1 of t. */
/*<       if (n .gt. 1) then >*/
    if (*n > 1) {
/*<          i = 1 >*/
	i__ = 1;
/*<          out = t(1) >*/
	out = t[1];
/*<          indxou = iorder(1) >*/
	indxou = iorder[1];
/*<          ddum  = t(n) >*/
	ddum = t[*n];
/*<          indxin  = iorder(n) >*/
	indxin = iorder[*n];
/*        Restore the heap */
/*<    30    continue >*/
L30:
/*<          j = i+i >*/
	j = i__ + i__;
/*<          if (j .le. n-1) then >*/
	if (j <= *n - 1) {
/*<             if (t(j+1) .lt. t(j)) j = j+1 >*/
	    if (t[j + 1] < t[j]) {
		++j;
	    }
/*<             if (t(j) .lt. ddum ) then >*/
	    if (t[j] < ddum) {
/*<                t(i) = t(j) >*/
		t[i__] = t[j];
/*<                iorder(i) = iorder(j) >*/
		iorder[i__] = iorder[j];
/*<                i = j >*/
		i__ = j;
/*<                goto 30 >*/
		goto L30;
/*<             endif  >*/
	    }
/*<          endif  >*/
	}
/*<          t(i) = ddum >*/
	t[i__] = ddum;
/*<          iorder(i) = indxin >*/
	iorder[i__] = indxin;
/*     Put the least member in t(n). */
/*<          t(n) = out >*/
	t[*n] = out;
/*<          iorder(n) = indxou >*/
	iorder[*n] = indxou;
/*<       endif  >*/
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* hpsolb_ */

/* ====================== The end of hpsolb ============================== */
/*<    >*/
/* Subroutine */ int lnsrlb_(integer *n, doublereal *l, doublereal *u, 
	integer *nbd, doublereal *x, doublereal *f, doublereal *fold, 
	doublereal *gd, doublereal *gdold, doublereal *g, doublereal *d__, 
	doublereal *r__, doublereal *t, doublereal *z__, doublereal *stp, 
	doublereal *dnorm, doublereal *dtd, doublereal *xstep, doublereal *
	stpmx, integer *iter, integer *ifun, integer *iback, integer *nfgv, 
	integer *info, char *task, logical *boxed, logical *cnstnd, char *
	csave, integer *isave, doublereal *dsave, ftnlen task_len, ftnlen 
	csave_len)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    double sqrt(doublereal);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__;
    doublereal a1, a2;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *), dcsrch_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, char *, integer *, doublereal *, 
	    ftnlen);
    (void)task_len;
    (void)csave_len;

/*<       character*60     task, csave >*/
/*<       logical          boxed, cnstnd >*/
/*<    >*/
/*<    >*/
/*     ********** */

/*     Subroutine lnsrlb */

/*     This subroutine calls subroutine dcsrch from the Minpack2 library */
/*       to perform the line search.  Subroutine dscrch is safeguarded so */
/*       that all trial points lie within the feasible region. */

/*     Subprograms called: */

/*       Minpack2 Library ... dcsrch. */

/*       Linpack ... dtrsl, ddot. */


/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ********** */
/*<       integer          i >*/
/*<       double           precision ddot,a1,a2 >*/
/*<       double precision one,zero,big >*/
/*<       parameter        (one=1.0d0,zero=0.0d0,big=1.0d+10) >*/
/*<       double precision ftol,gtol,xtol >*/
/*<       parameter        (ftol=1.0d-3,gtol=0.9d0,xtol=0.1d0) >*/
/*<       if (task(1:5) .eq. 'FG_LN') goto 556 >*/
    /* Parameter adjustments */
    --z__;
    --t;
    --r__;
    --d__;
    --g;
    --x;
    --nbd;
    --u;
    --l;
    --isave;
    --dsave;

    /* Function Body */
    if (s_cmp(task, "FG_LN", (ftnlen)5, (ftnlen)5) == 0) {
	goto L556;
    }
/*<       dtd = ddot(n,d,1,d,1) >*/
    *dtd = ddot_(n, &d__[1], &c__1, &d__[1], &c__1);
/*<       dnorm = sqrt(dtd) >*/
    *dnorm = sqrt(*dtd);
/*     Determine the maximum step length. */
/*<       stpmx = big >*/
    *stpmx = 1e10;
/*<       if (cnstnd) then >*/
    if (*cnstnd) {
/*<          if (iter .eq. 0) then >*/
	if (*iter == 0) {
/*<             stpmx = one >*/
	    *stpmx = 1.;
/*<          else >*/
	} else {
/*<             do 43 i = 1, n >*/
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/*<                a1 = d(i) >*/
		a1 = d__[i__];
/*<                if (nbd(i) .ne. 0) then >*/
		if (nbd[i__] != 0) {
/*<                   if (a1 .lt. zero .and. nbd(i) .le. 2) then >*/
		    if (a1 < 0. && nbd[i__] <= 2) {
/*<                      a2 = l(i) - x(i) >*/
			a2 = l[i__] - x[i__];
/*<                      if (a2 .ge. zero) then >*/
			if (a2 >= 0.) {
/*<                         stpmx = zero >*/
			    *stpmx = 0.;
/*<                      else if (a1*stpmx .lt. a2) then >*/
			} else if (a1 * *stpmx < a2) {
/*<                         stpmx = a2/a1 >*/
			    *stpmx = a2 / a1;
/*<                      endif >*/
			}
/*<                   else if (a1 .gt. zero .and. nbd(i) .ge. 2) then >*/
		    } else if (a1 > 0. && nbd[i__] >= 2) {
/*<                      a2 = u(i) - x(i) >*/
			a2 = u[i__] - x[i__];
/*<                      if (a2 .le. zero) then >*/
			if (a2 <= 0.) {
/*<                         stpmx = zero >*/
			    *stpmx = 0.;
/*<                      else if (a1*stpmx .gt. a2) then >*/
			} else if (a1 * *stpmx > a2) {
/*<                         stpmx = a2/a1 >*/
			    *stpmx = a2 / a1;
/*<                      endif >*/
			}
/*<                   endif >*/
		    }
/*<                endif >*/
		}
/*<   43        continue >*/
/* L43: */
	    }
/*<          endif >*/
	}
/*<       endif >*/
    }
/*<       if (iter .eq. 0 .and. .not. boxed) then >*/
    if (*iter == 0 && ! (*boxed)) {
/*<          stp = min(one/dnorm, stpmx) >*/
/* Computing MIN */
	d__1 = 1. / *dnorm;
	*stp = min(d__1,*stpmx);
/*<       else >*/
    } else {
/*< 	 stp = one >*/
	*stp = 1.;
/*<       endif  >*/
    }
/*<       call dcopy(n,x,1,t,1) >*/
    dcopy_(n, &x[1], &c__1, &t[1], &c__1);
/*<       call dcopy(n,g,1,r,1) >*/
    dcopy_(n, &g[1], &c__1, &r__[1], &c__1);
/*<       fold = f >*/
    *fold = *f;
/*<       ifun = 0 >*/
    *ifun = 0;
/*<       iback = 0 >*/
    *iback = 0;
/*<       csave = 'START' >*/
    s_copy(csave, "START", (ftnlen)60, (ftnlen)5);
/*<  556  continue >*/
L556:
/*<       gd = ddot(n,g,1,d,1) >*/
    *gd = ddot_(n, &g[1], &c__1, &d__[1], &c__1);
/*<       if (ifun .eq. 0) then >*/
    if (*ifun == 0) {
/*< 	 gdold=gd >*/
	*gdold = *gd;
/*<          if (gd .ge. zero) then >*/
	if (*gd >= 0.) {
/*                               the directional derivative >=0. */
/*                               Line search is impossible. */
/*<             info = -4 >*/
	    *info = -4;
/*<             return >*/
	    return 0;
/*<          endif >*/
	}
/*<       endif >*/
    }
/*<       call dcsrch(f,gd,stp,ftol,gtol,xtol,zero,stpmx,csave,isave,dsave) >*/
    dcsrch_(f, gd, stp, &c_b275, &c_b276, &c_b277, &c_b9, stpmx, csave, &
	    isave[1], &dsave[1], (ftnlen)60);
/*<       xstep = stp*dnorm >*/
    *xstep = *stp * *dnorm;
/*<       if (csave(1:4) .ne. 'CONV' .and. csave(1:4) .ne. 'WARN') then >*/
    if (s_cmp(csave, "CONV", (ftnlen)4, (ftnlen)4) != 0 && s_cmp(csave, "WARN"
	    , (ftnlen)4, (ftnlen)4) != 0) {
/*< 	 task = 'FG_LNSRCH' >*/
	s_copy(task, "FG_LNSRCH", (ftnlen)60, (ftnlen)9);
/*< 	 ifun = ifun + 1 >*/
	++(*ifun);
/*<          nfgv = nfgv + 1 >*/
	++(*nfgv);
/*<          iback = ifun - 1  >*/
	*iback = *ifun - 1;
/*<          if (stp .eq. one) then >*/
	if (*stp == 1.) {
/*<             call dcopy(n,z,1,x,1) >*/
	    dcopy_(n, &z__[1], &c__1, &x[1], &c__1);
/*<          else >*/
	} else {
/*<             do 41 i = 1, n >*/
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	       x(i) = stp*d(i) + t(i) >*/
		x[i__] = *stp * d__[i__] + t[i__];
/*<   41        continue >*/
/* L41: */
	    }
/*<          endif >*/
	}
/*<       else >*/
    } else {
/*<          task = 'NEW_X' >*/
	s_copy(task, "NEW_X", (ftnlen)60, (ftnlen)5);
/*<       endif >*/
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* lnsrlb_ */

/* ======================= The end of lnsrlb ============================= */
/*<    >*/
/* Subroutine */ int matupd_(integer *n, integer *m, doublereal *ws, 
	doublereal *wy, doublereal *sy, doublereal *ss, doublereal *d__, 
	doublereal *r__, integer *itail, integer *iupdat, integer *col, 
	integer *head, doublereal *theta, doublereal *rr, doublereal *dr, 
	doublereal *stp, doublereal *dtd)
{
    /* System generated locals */
    integer ws_dim1, ws_offset, wy_dim1, wy_offset, sy_dim1, sy_offset, 
	    ss_dim1, ss_offset, i__1, i__2;

    /* Local variables */
    integer j;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *);
    integer pointr;

/*<       integer          n, m, itail, iupdat, col, head >*/
/*<    >*/
/*     ************ */

/*     Subroutine matupd */

/*       This subroutine updates matrices WS and WY, and forms the */
/*         middle matrix in B. */

/*     Subprograms called: */

/*       Linpack ... dcopy, ddot. */


/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ************ */
/*<       integer          j,pointr >*/
/*<       double precision ddot >*/
/*<       double precision one >*/
/*<       parameter        (one=1.0d0) >*/
/*     Set pointers for matrices WS and WY. */
/*<       if (iupdat .le. m) then >*/
    /* Parameter adjustments */
    --r__;
    --d__;
    ss_dim1 = *m;
    ss_offset = 1 + ss_dim1;
    ss -= ss_offset;
    sy_dim1 = *m;
    sy_offset = 1 + sy_dim1;
    sy -= sy_offset;
    wy_dim1 = *n;
    wy_offset = 1 + wy_dim1;
    wy -= wy_offset;
    ws_dim1 = *n;
    ws_offset = 1 + ws_dim1;
    ws -= ws_offset;

    /* Function Body */
    if (*iupdat <= *m) {
/*< 	 col = iupdat >*/
	*col = *iupdat;
/*< 	 itail = mod(head+iupdat-2,m) + 1 >*/
	*itail = (*head + *iupdat - 2) % *m + 1;
/*<       else >*/
    } else {
/*< 	 itail = mod(itail,m) + 1 >*/
	*itail = *itail % *m + 1;
/*< 	 head = mod(head,m) + 1 >*/
	*head = *head % *m + 1;
/*<       endif >*/
    }
/*     Update matrices WS and WY. */
/*<       call dcopy(n,d,1,ws(1,itail),1) >*/
    dcopy_(n, &d__[1], &c__1, &ws[*itail * ws_dim1 + 1], &c__1);
/*<       call dcopy(n,r,1,wy(1,itail),1) >*/
    dcopy_(n, &r__[1], &c__1, &wy[*itail * wy_dim1 + 1], &c__1);
/*     Set theta=yy/ys. */
/*<       theta = rr/dr >*/
    *theta = *rr / *dr;
/*     Form the middle matrix in B. */
/*        update the upper triangle of SS, */
/*                                         and the lower triangle of SY: */
/*<       if (iupdat .gt. m) then >*/
    if (*iupdat > *m) {
/*                              move old information */
/*<          do 50 j = 1, col - 1 >*/
	i__1 = *col - 1;
	for (j = 1; j <= i__1; ++j) {
/*<             call dcopy(j,ss(2,j+1),1,ss(1,j),1) >*/
	    dcopy_(&j, &ss[(j + 1) * ss_dim1 + 2], &c__1, &ss[j * ss_dim1 + 1]
		    , &c__1);
/*<             call dcopy(col-j,sy(j+1,j+1),1,sy(j,j),1) >*/
	    i__2 = *col - j;
	    dcopy_(&i__2, &sy[j + 1 + (j + 1) * sy_dim1], &c__1, &sy[j + j * 
		    sy_dim1], &c__1);
/*<   50     continue >*/
/* L50: */
	}
/*<       endif >*/
    }
/*        add new information: the last row of SY */
/*                                             and the last column of SS: */
/*<       pointr = head >*/
    pointr = *head;
/*<       do 51 j = 1, col - 1 >*/
    i__1 = *col - 1;
    for (j = 1; j <= i__1; ++j) {
/*< 	 sy(col,j) = ddot(n,d,1,wy(1,pointr),1) >*/
	sy[*col + j * sy_dim1] = ddot_(n, &d__[1], &c__1, &wy[pointr * 
		wy_dim1 + 1], &c__1);
/*< 	 ss(j,col) = ddot(n,ws(1,pointr),1,d,1) >*/
	ss[j + *col * ss_dim1] = ddot_(n, &ws[pointr * ws_dim1 + 1], &c__1, &
		d__[1], &c__1);
/*<          pointr = mod(pointr,m) + 1 >*/
	pointr = pointr % *m + 1;
/*<   51  continue >*/
/* L51: */
    }
/*<       if (stp .eq. one) then >*/
    if (*stp == 1.) {
/*<          ss(col,col) = dtd >*/
	ss[*col + *col * ss_dim1] = *dtd;
/*<       else >*/
    } else {
/*<          ss(col,col) = stp*stp*dtd >*/
	ss[*col + *col * ss_dim1] = *stp * *stp * *dtd;
/*<       endif >*/
    }
/*<       sy(col,col) = dr >*/
    sy[*col + *col * sy_dim1] = *dr;
/*<       return >*/
    return 0;
/*<       end >*/
} /* matupd_ */

/* ======================= The end of matupd ============================= */
/*<       subroutine prn1lb(n, m, l, u, x, iprint, itfile, epsmch) >*/
/* Subroutine */ int prn1lb_(integer *n, integer *m, doublereal *l, 
	doublereal *u, doublereal *x, integer *iprint, integer *itfile, 
	doublereal *epsmch)
{
  (void)itfile;
  (void)m;
  (void)epsmch;
/*<       integer n, m, iprint, itfile >*/
/*<       double precision epsmch, x(n), l(n), u(n) >*/
/*     ************ */

/*     Subroutine prn1lb */

/*     This subroutine prints the input data, initial point, upper and */
/*       lower bounds of each variable, machine precision, as well as */
/*       the headings of the output. */


/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ************ */
/*<       integer i >*/
/*<       if (iprint .ge. 0) then >*/
    /* Parameter adjustments */
    --x;
    --u;
    --l;

    /* Function Body */
    if (*iprint >= 0) {
/*
 7001 format ('RUNNING THE L-BFGS-B CODE',/,/,
     + '           * * *',/,/,
     + 'Machine precision =',1p,d10.3)
*/
/*<          write (6,7001) epsmch >*/
#ifdef LBFGSB_ENABLE_ITERATE_FILE
        fprintf(0,
                "RUNNING THE L-BFGS-B CODE\n"
                "\n"
                "           * * *\n"
                "\n"
                "Machine precision = %10.3g\n", *epsmch);
#endif
/*<          write (6,*) 'N = ',n,'    M = ',m >*/
#ifdef LBFGSB_ENABLE_ITERATE_FILE
        fprintf(0, "N = %ld    M = %ld\n", *n, *m);
#endif
/*<          if (iprint .ge. 1) then >*/
	if (*iprint >= 1) {
/*<             write (itfile,2001) epsmch >*/
/*
 2001 format ('RUNNING THE L-BFGS-B CODE',/,/,
     + 'it    = iteration number',/,
     + 'nf    = number of function evaluations',/,
     + 'nint  = number of segments explored during the Cauchy search',/,
     + 'nact  = number of active bounds at the generalized Cauchy point'
     + ,/,
     + 'sub   = manner in which the subspace minimization terminated:'
     + ,/,'        con = converged, bnd = a bound was reached',/,
     + 'itls  = number of iterations performed in the line search',/,
     + 'stepl = step length used',/,
     + 'tstep = norm of the displacement (total step)',/,
     + 'projg = norm of the projected gradient',/,
     + 'f     = function value',/,/,
     + '           * * *',/,/,
     + 'Machine precision =',1p,d10.3)
*/
#ifdef LBFGSB_ENABLE_ITERATE_FILE
            fprintf(0,
                    "RUNNING THE L-BFGS-B CODE\n"
                    "\n"
                    "it    = iteration number\n"
                    "nf    = number of function evaluations\n"
                    "nint  = number of segments explored during the Cauchy search\n"
                    "nact  = number of active bounds at the generalized Cauchy point\n"
                    "sub   = manner in which the subspace minimization terminated:\n"
                    "        con = converged, bnd = a bound was reached\n"
                    "itls  = number of iterations performed in the line search\n"
                    "stepl = step length used\n"
                    "tstep = norm of the displacement (total step)\n"
                    "projg = norm of the projected gradient\n"
                    "f     = function value\n"
                    "\n"
                    "           * * *\n"
                    "\n"
                    "Machine precision = %10.3g\n", *epsmch);
#endif
/*<             write (itfile,*)'N = ',n,'    M = ',m >*/
#ifdef LBFGSB_ENABLE_ITERATE_FILE
            fprintf(0, "N = %ld    M = %ld\n", *n, *m);
#endif
/*< 	    write (itfile,9001) >*/
/*
 9001 format (/,3x,'it',3x,'nf',2x,'nint',2x,'nact',2x,'sub',2x,'itls',
     +        2x,'stepl',4x,'tstep',5x,'projg',8x,'f')
*/
#ifdef LBFGSB_ENABLE_ITERATE_FILE
            fprintf(0, "   it   nf  nint  nact  sub  itls  stepl    tstep     projg        f\n");
#endif
/*<             if (iprint .gt. 100) then >*/
	    if (*iprint > 100) {
/*<                write (6,1004) 'L =',(l(i),i = 1,n) >*/
                lbfgsb_printf_vec("L", l, *n);
/*<                write (6,1004) 'X0 =',(x(i),i = 1,n) >*/
                lbfgsb_printf_vec("X0", x, *n);
/*<                write (6,1004) 'U =',(u(i),i = 1,n) >*/
                lbfgsb_printf_vec("U", u, *n);
/*<             endif  >*/
	    }
/*<          endif >*/
	}
/*<       endif  >*/
    }
/*<  1004 format (/,a4, 1p, 6(1x,d11.4),/,(4x,1p,6(1x,d11.4))) >*/
/*<  2 >*/
/*<  7 >*/
/*<  9 >*/
/*<       return >*/
    return 0;
/*<       end >*/
} /* prn1lb_ */

/* ======================= The end of prn1lb ============================= */
/*<    >*/
/* Subroutine */ int prn2lb_(integer *n, doublereal *x, doublereal *f, 
	doublereal *g, integer *iprint, integer *itfile, integer *iter, 
	integer *nfgv, integer *nact, doublereal *sbgnrm, integer *nint, char 
	*word, integer *iword, integer *iback, doublereal *stp, doublereal *
	xstep, ftnlen word_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer imod;

    (void)itfile;
    (void)word_len;
    (void)nfgv;
    (void)nact;
    (void)nint;
    (void)stp;

/*<       character*3      word >*/
/*<    >*/
/*<       double precision f, sbgnrm, stp, xstep, x(n), g(n) >*/
/*     ************ */

/*     Subroutine prn2lb */

/*     This subroutine prints out new information after a successful */
/*       line search. */


/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ************ */
/*<       integer i,imod >*/
/*           'word' records the status of subspace solutions. */
/*<       if (iword .eq. 0) then >*/
    /* Parameter adjustments */
    --g;
    --x;

    /* Function Body */
    if (*iword == 0) {
/*                            the subspace minimization converged. */
/*< 	 word = 'con' >*/
	s_copy(word, "con", (ftnlen)3, (ftnlen)3);
/*<       else if (iword .eq. 1) then >*/
    } else if (*iword == 1) {
/*                          the subspace minimization stopped at a bound. */
/*<          word = 'bnd' >*/
	s_copy(word, "bnd", (ftnlen)3, (ftnlen)3);
/*<       else if (iword .eq. 5) then >*/
    } else if (*iword == 5) {
/*                             the truncated Newton step has been used. */
/*< 	 word = 'TNT' >*/
	s_copy(word, "TNT", (ftnlen)3, (ftnlen)3);
/*<       else >*/
    } else {
/*<          word = '---' >*/
	s_copy(word, "---", (ftnlen)3, (ftnlen)3);
/*<       endif >*/
    }
/*<       if (iprint .ge. 99) then >*/
    if (*iprint >= 99) {
/*<          write (6,*) 'LINE SEARCH',iback,' times; norm of step = ',xstep >*/
        printf("LINE SEARCH %ld times; norm of step = %g\n", *iback, *xstep);
/*<          write (6,2001) iter,f,sbgnrm >*/
/*
 2001 format
     +  (/,'At iterate',i5,4x,'f= ',1p,d12.5,4x,'|proj g|= ',1p,d12.5)
*/
        printf("At iterate %5ld    f= %12.5g    |proj g|= %12.5g\n",
               *iter, *f, *sbgnrm);
/*<          if (iprint .gt. 100) then	 >*/
	if (*iprint > 100) {
/*<             write (6,1004) 'X =',(x(i), i = 1, n) >*/
            lbfgsb_printf_vec("X", x, *n);
/*<             write (6,1004) 'G =',(g(i), i = 1, n) >*/
            lbfgsb_printf_vec("G", g, *n);
/*<          endif >*/
	}
/*<       else if (iprint .gt. 0) then  >*/
    } else if (*iprint > 0) {
/*<          imod = mod(iter,iprint) >*/
	imod = *iter % *iprint;
/*<          if (imod .eq. 0) write (6,2001) iter,f,sbgnrm >*/
	if (imod == 0) {
/*
  2001 format
  +  (/,'At iterate',i5,4x,'f= ',1p,d12.5,4x,'|proj g|= ',1p,d12.5)
*/
            printf("At iterate %5ld    f= %12.5g    |proj g|= %12.5g\n",
                   *iter, *f, *sbgnrm);
	}
/*<       endif >*/
    }
/*<    >*/
    if (*iprint >= 1) {
/*
<  3001 format(2(1x,i4),2(1x,i5),2x,a3,1x,i4,1p,2(2x,d7.1),1p,2(1x,d10.3)) >
*/
#ifdef LBFGSB_ENABLE_ITERATE_FILE
        fprintf(0, " %4ld %4ld %5ld %5ld  %3s %4ld  %7.1g %7.1g<1p> %10.3g %10.3g\n",
                *iter, *nfgv, *nint, *nact, word, *iback, *stp, *xstep,
                *sbgnrm, *f);
#endif
    }
/*<  1004 format (/,a4, 1p, 6(1x,d11.4),/,(4x,1p,6(1x,d11.4))) >*/
/*<  2 >*/
/*<  3001 format(2(1x,i4),2(1x,i5),2x,a3,1x,i4,1p,2(2x,d7.1),1p,2(1x,d10.3)) >*/
/*<       return >*/
    return 0;
/*<       end >*/
} /* prn2lb_ */

/* ======================= The end of prn2lb ============================= */
/*<    >*/
/* Subroutine */ int prn3lb_(integer *n, doublereal *x, doublereal *f, char *
	task, integer *iprint, integer *info, integer *itfile, integer *iter, 
	integer *nfgv, integer *nintol, integer *nskip, integer *nact, 
	doublereal *sbgnrm, doublereal *time, integer *nint, char *word, 
	integer *iback, doublereal *stp, doublereal *xstep, integer *k, 
	doublereal *cachyt, doublereal *sbtime, doublereal *lnscht, ftnlen 
	task_len, ftnlen word_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    (void)itfile;
    (void)task_len;
    (void)word_len;
    (void)nint;
    (void)word;
    (void)iback;
    (void)stp;
    (void)xstep;

/*<       character*60     task >*/
/*<       character*3      word >*/
/*<    >*/
/*<    >*/
/*     ************ */

/*     Subroutine prn3lb */

/*     This subroutine prints out information when either a built-in */
/*       convergence test is satisfied or when an error message is */
/*       generated. */


/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ************ */
/*<       integer i >*/
/*<       if (task(1:5) .eq. 'ERROR') goto 999 >*/
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (s_cmp(task, "ERROR", (ftnlen)5, (ftnlen)5) == 0) {
	goto L999;
    }
/*<       if (iprint .ge. 0) then >*/
    if (*iprint >= 0) {
/*<          write (6,3003) >*/
/*
 3003 format (/,
     + '           * * *',/,/,
     + 'Tit   = total number of iterations',/,
     + 'Tnf   = total number of function evaluations',/,
     + 'Tnint = total number of segments explored during',
     +           ' Cauchy searches',/,
     + 'Skip  = number of BFGS updates skipped',/,
     + 'Nact  = number of active bounds at final generalized',
     +          ' Cauchy point',/,
     + 'Projg = norm of the final projected gradient',/,
     + 'F     = final function value',/,/,
     + '           * * *')
*/
        printf(
          "           * * *\n"
          "\n"
          "Tit   = total number of iterations\n"
          "Tnf   = total number of function evaluations\n"
          "Tnint = total number of segments explored during Cauchy searches\n"
          "Skip  = number of BFGS updates skipped\n"
          "Nact  = number of active bounds at final generalized Cauchy point\n"
          "Projg = norm of the final projected gradient\n"
          "F     = final function value\n"
          "\n"
          "           * * *\n");
/*<          write (6,3004) >*/
/*
 3004 format (/,3x,'N',3x,'Tit',2x,'Tnf',2x,'Tnint',2x,
     +       'Skip',2x,'Nact',5x,'Projg',8x,'F')
*/
        printf("   N   Tit  Tnf  Tnint  Skip  Nact     Projg        F\n");
/*<          write(6,3005) n,iter,nfgv,nintol,nskip,nact,sbgnrm,f >*/
/*
 3005 format (i5,2(1x,i4),(1x,i6),(2x,i4),(1x,i5),1p,2(2x,d10.3))
*/
        printf(" %4ld %4ld %4ld %6ld %4ld %5ld  %10.3g  %10.3g\n",
               *n, *iter, *nfgv, *nintol, *nskip, *nact, *sbgnrm, *f);
/*<          if (iprint .ge. 100) then >*/
	if (*iprint >= 100) {
/*<             write (6,1004) 'X =',(x(i),i = 1,n) >*/
            lbfgsb_printf_vec("X", x, *n);
/*<          endif   >*/
	}
/*<          if (iprint .ge. 1) write (6,*) ' F =',f >*/
	if (*iprint >= 1) {
            printf("F = %g\n", *f);
	}
/*<       endif  >*/
    }
/*<  999  continue >*/
L999:
/*<       if (iprint .ge. 0) then >*/
    if (*iprint >= 0) {
/*<          write (6,3009) task >*/
        printf("%60s\n", task);
/*<          if (info .ne. 0) then >*/
	if (*info != 0) {
/*<             if (info .eq. -1) write (6,9011) >*/
	    if (*info == -1) {
/*
 9011 format (/,
     +' Matrix in 1st Cholesky factorization in formk is not Pos. Def.')
*/
                printf(" Matrix in 1st Cholesky factorization in formk is not Pos. Def.\n");
	    }
/*<             if (info .eq. -2) write (6,9012) >*/
	    if (*info == -2) {
/*
 9012 format (/,
     +' Matrix in 2st Cholesky factorization in formk is not Pos. Def.')
*/
                printf(" Matrix in 2st Cholesky factorization in formk is not Pos. Def.\n");
	    }
/*<             if (info .eq. -3) write (6,9013) >*/
	    if (*info == -3) {
/*
 9013 format (/,
     +' Matrix in the Cholesky factorization in formt is not Pos. Def.')
*/
                printf(" Matrix in the Cholesky factorization in formk is not Pos. Def.\n");
	    }
/*<             if (info .eq. -4) write (6,9014) >*/
	    if (*info == -4) {
/*
 9014 format (/,
     +' Derivative >= 0, backtracking line search impossible.',/,
     +'   Previous x, f and g restored.',/,
     +' Possible causes: 1 error in function or gradient evaluation;',/,
     +'                  2 rounding errors dominate computation.')
 */
                printf(" Derivative >= 0, backtracking line search impossible.\n"
                       "   Previous x, f and g restored.\n"
                       " Possible causes: 1 error in function or gradient evaluation;\n"
                       "                  2 rounding errors dominate computation.\n");
	    }
/*<             if (info .eq. -5) write (6,9015) >*/
	    if (*info == -5) {
/*
 9015 format (/,
     +' Warning:  more than 10 function and gradient',/,
     +'   evaluations in the last line search.  Termination',/,
     +'   may possibly be caused by a bad search direction.')
*/
                printf(" Warning:  more than 10 function and gradient\n"
                       "   evaluations in the last line search.  Termination\n"
                       "   may possibly be caused by a bad search direction.");
	    }
/*<             if (info .eq. -6) write (6,*)' Input nbd(',k,') is invalid.' >*/
	    if (*info == -6) {
                printf(" Input nbd(%ld) is invalid.\n", *k);
	    }
/*<    >*/
	    if (*info == -7) {
                printf(" l(%ld) > u(%ld).  No feasible solution.\n", *k, *k);
	    }
/*<             if (info .eq. -8) write (6,9018) >*/
	    if (*info == -8) {
/*
 9018 format (/,' The triangular system is singular.')
*/
                printf(" The triangular system is singular.\n");
	    }
/*<             if (info .eq. -9) write (6,9019) >*/
	    if (*info == -9) {
/*
 9019 format (/,
     +' Line search cannot locate an adequate point after 20 function',/
     +,'  and gradient evaluations.  Previous x, f and g restored.',/,
     +' Possible causes: 1 error in function or gradient evaluation;',/,
     +'                  2 rounding error dominate computation.')

*/
                printf(" Line search cannot locate an adequate point after 20 function\n"
                       "  and gradient evaluations.  Previous x, f and g restored.\n"
                       " Possible causes: 1 error in function or gradient evaluation;\n"
                       "                  2 rounding error dominate computation.\n");
	    }
/*<          endif >*/
	}
/*<          if (iprint .ge. 1) write (6,3007) cachyt,sbtime,lnscht >*/
/*
 3007 format (/,' Cauchy                time',1p,e10.3,' seconds.',/ 
     +        ' Subspace minimization time',1p,e10.3,' seconds.',/
     +        ' Line search           time',1p,e10.3,' seconds.')
*/
	if (*iprint >= 1) {
            printf(" Cauchy                time %10.3g seconds.\n"
                   " Subspace minimization time %10.3g seconds.\n"
                   " Line search           time %10.3g seconds.\n",
                   *cachyt, *sbtime, *lnscht);
	}
/*<          write (6,3008) time >*/
/*
 3008 format (/,' Total User time',1p,e10.3,' seconds.',/)
 */
        printf(" Total User time %10.3g seconds.\n", *time);
/*<          if (iprint .ge. 1) then >*/
	if (*iprint >= 1) {
/*<             if (info .eq. -4 .or. info .eq. -9) then >*/
	    if (*info == -4 || *info == -9) {
/*<    >*/
/*
 3002 format(2(1x,i4),2(1x,i5),2x,a3,1x,i4,1p,2(2x,d7.1),6x,'-',10x,'-')
 */
#ifdef LBFGSB_ENABLE_ITERATE_FILE
                fprintf(0,
                        " %4ld %4ld %5ld %5ld  %3s %4ld  %7.1g %7.1g      -         -\n",
                        *iter, *nfgv, *nint, *nact, word, *iback, *stp, *xstep);
#endif
/*<             endif >*/
	    }
/*<             write (itfile,3009) task >*/
#ifdef LBFGSB_ENABLE_ITERATE_FILE
            fprintf(0, "%60s\n", task);
#endif
/*<             if (info .ne. 0) then >*/
	    if (*info != 0) {
/*<                if (info .eq. -1) write (itfile,9011) >*/
/*
 9011 format (/,
     +' Matrix in 1st Cholesky factorization in formk is not Pos. Def.')
 */
		if (*info == -1) {
#ifdef LBFGSB_ENABLE_ITERATE_FILE
                    fprintf(0, " Matrix in 1st Cholesky factorization in formk is not Pos. Def.\n");
#endif
		}
/*<                if (info .eq. -2) write (itfile,9012) >*/
/*
 9012 format (/,
     +' Matrix in 2st Cholesky factorization in formk is not Pos. Def.')
 */
		if (*info == -2) {
#ifdef LBFGSB_ENABLE_ITERATE_FILE
                    fprintf(0, " Matrix in 2st Cholesky factorization in formk is not Pos. Def.\n");
#endif
		}
/*<                if (info .eq. -3) write (itfile,9013) >*/
/*
 9013 format (/,
     +' Matrix in the Cholesky factorization in formt is not Pos. Def.')
 */
		if (*info == -3) {
#ifdef LBFGSB_ENABLE_ITERATE_FILE
                    fprintf(0, " Matrix in the Cholesky factorization in formk is not Pos. Def.\n");
#endif
		}
/*<                if (info .eq. -4) write (itfile,9014) >*/
/*
 9014 format (/,
     +' Derivative >= 0, backtracking line search impossible.',/,
     +'   Previous x, f and g restored.',/,
     +' Possible causes: 1 error in function or gradient evaluation;',/,
     +'                  2 rounding errors dominate computation.')
 */
		if (*info == -4) {
#ifdef LBFGSB_ENABLE_ITERATE_FILE
                    fprintf(0, " Derivative >= 0, backtracking line search impossible.\n"
                            "   Previous x, f and g restored.\n"
                            " Possible causes: 1 error in function or gradient evaluation;\n"
                            "                  2 rounding errors dominate computation.\n");
#endif
		}
/*<                if (info .eq. -5) write (itfile,9015) >*/
/*
 9015 format (/,
     +' Warning:  more than 10 function and gradient',/,
     +'   evaluations in the last line search.  Termination',/,
     +'   may possibly be caused by a bad search direction.')
 */
		if (*info == -5) {
#ifdef LBFGSB_ENABLE_ITERATE_FILE
                    fprintf(0, " Warning:  more than 10 function and gradient\n"
                            "   evaluations in the last line search.  Termination\n"
                            "   may possibly be caused by a bad search direction.");
#endif
		}
/*<                if (info .eq. -8) write (itfile,9018) >*/
/*
 9018 format (/,' The triangular system is singular.')
 */
		if (*info == -8) {
#ifdef LBFGSB_ENABLE_ITERATE_FILE
                    fprintf(0, " The triangular system is singular.\n");
#endif
		}
/*<                if (info .eq. -9) write (itfile,9019) >*/
/*
 9019 format (/,
     +' Line search cannot locate an adequate point after 20 function',/
     +,'  and gradient evaluations.  Previous x, f and g restored.',/,
     +' Possible causes: 1 error in function or gradient evaluation;',/,
     +'                  2 rounding error dominate computation.')
 */
		if (*info == -9) {
#ifdef LBFGSB_ENABLE_ITERATE_FILE
                    fprintf(0, " Line search cannot locate an adequate point after 20 function\n"
                            "  and gradient evaluations.  Previous x, f and g restored.\n"
                            " Possible causes: 1 error in function or gradient evaluation;\n"
                            "                  2 rounding error dominate computation.\n");
#endif
		}
/*<             endif >*/
	    }
/*<             write (itfile,3008) time >*/
#ifdef LBFGSB_ENABLE_ITERATE_FILE
            fprintf(0, " Total User time %10.3g seconds.\n", *time);
#endif
/*<          endif >*/
	}
/*<       endif >*/
    }
/*<  1004 format (/,a4, 1p, 6(1x,d11.4),/,(4x,1p,6(1x,d11.4))) >*/
/*<  3002 format(2(1x,i4),2(1x,i5),2x,a3,1x,i4,1p,2(2x,d7.1),6x,'-',10x,'-') >*/
/*<  3 >*/
/*<  3 >*/
/*<  3005 format (i5,2(1x,i4),(1x,i6),(2x,i4),(1x,i5),1p,2(2x,d10.3)) >*/
/*<  3006 format (i5,2(1x,i4),2(1x,i6),(1x,i4),(1x,i5),7x,'-',10x,'-') >*/
/* L3006: */
/*<  3 >*/
/*<  3008 format (/,' Total User time',1p,e10.3,' seconds.',/) >*/
/*<  3009 format (/,a60) >*/
/*<  9 >*/
/*<  9 >*/
/*<  9 >*/
/*<  9 >*/
/*<  9 >*/
/*<  9018 format (/,' The triangular system is singular.') >*/
/*<  9 >*/
/*<       return >*/
    return 0;
/*<       end >*/
} /* prn3lb_ */

/* ======================= The end of prn3lb ============================= */
/*<       subroutine projgr(n, l, u, nbd, x, g, sbgnrm) >*/
/* Subroutine */ int projgr_(integer *n, doublereal *l, doublereal *u, 
	integer *nbd, doublereal *x, doublereal *g, doublereal *sbgnrm)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Local variables */
    integer i__;
    doublereal gi;

/*<       integer          n, nbd(n) >*/
/*<       double precision sbgnrm, x(n), l(n), u(n), g(n) >*/
/*     ************ */

/*     Subroutine projgr */

/*     This subroutine computes the infinity norm of the projected */
/*       gradient. */


/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ************ */
/*<       integer i >*/
/*<       double precision gi >*/
/*<       double precision one,zero >*/
/*<       parameter        (one=1.0d0,zero=0.0d0) >*/
/*<       sbgnrm = zero >*/
    /* Parameter adjustments */
    --g;
    --x;
    --nbd;
    --u;
    --l;

    /* Function Body */
    *sbgnrm = 0.;
/*<       do 15 i = 1, n >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	gi = g(i) >*/
	gi = g[i__];
/*<         if (nbd(i) .ne. 0) then >*/
	if (nbd[i__] != 0) {
/*<            if (gi .lt. zero) then >*/
	    if (gi < 0.) {
/*<               if (nbd(i) .ge. 2) gi = max((x(i)-u(i)),gi) >*/
		if (nbd[i__] >= 2) {
/* Computing MAX */
		    d__1 = x[i__] - u[i__];
		    gi = max(d__1,gi);
		}
/*<        	   else >*/
	    } else {
/*<               if (nbd(i) .le. 2) gi = min((x(i)-l(i)),gi) >*/
		if (nbd[i__] <= 2) {
/* Computing MIN */
		    d__1 = x[i__] - l[i__];
		    gi = min(d__1,gi);
		}
/*<            endif >*/
	    }
/*<         endif >*/
	}
/*< 	sbgnrm = max(sbgnrm,abs(gi)) >*/
/* Computing MAX */
	d__1 = *sbgnrm, d__2 = abs(gi);
	*sbgnrm = max(d__1,d__2);
/*<   15  continue >*/
/* L15: */
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* projgr_ */

/* ======================= The end of projgr ============================= */
/*<    >*/
/* Subroutine */ int subsm_(integer *n, integer *m, integer *nsub, integer *
	ind, doublereal *l, doublereal *u, integer *nbd, doublereal *x, 
	doublereal *d__, doublereal *ws, doublereal *wy, doublereal *theta, 
	integer *col, integer *head, integer *iword, doublereal *wv, 
	doublereal *wn, integer *iprint, integer *info)
{
    /* System generated locals */
    integer ws_dim1, ws_offset, wy_dim1, wy_offset, wn_dim1, wn_offset, i__1, 
	    i__2;

    /* Local variables */
    integer i__, j, k, m2;
    doublereal dk;
    integer js, jy, ibd=0, col2;
    doublereal temp1, temp2, alpha;
    extern /* Subroutine */ int dtrsl_(doublereal *, integer *, integer *, 
	    doublereal *, integer *, integer *);
    integer pointr;

/*<    >*/
/*<    >*/
/*     ************ */

/*     Subroutine subsm */

/*     Given xcp, l, u, r, an index set that specifies */
/* 	the active set at xcp, and an l-BFGS matrix B */
/* 	(in terms of WY, WS, SY, WT, head, col, and theta), */
/* 	this subroutine computes an approximate solution */
/* 	of the subspace problem */

/*     	(P)   min Q(x) = r'(x-xcp) + 1/2 (x-xcp)' B (x-xcp) */

/*             subject to l<=x<=u */
/* 	  	        x_i=xcp_i for all i in A(xcp) */

/* 	along the subspace unconstrained Newton direction */

/* 	   d = -(Z'BZ)^(-1) r. */

/*       The formula for the Newton direction, given the L-BFGS matrix */
/*       and the Sherman-Morrison formula, is */

/* 	   d = (1/theta)r + (1/theta*2) Z'WK^(-1)W'Z r. */

/*       where */
/*                 K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ] */
/*                     [L_a -R_z           theta*S'AA'S ] */

/*     Note that this procedure for computing d differs */
/*     from that described in [1]. One can show that the matrix K is */
/*     equal to the matrix M^[-1]N in that paper. */

/*     n is an integer variable. */
/*       On entry n is the dimension of the problem. */
/*       On exit n is unchanged. */

/*     m is an integer variable. */
/*       On entry m is the maximum number of variable metric corrections */
/*         used to define the limited memory matrix. */
/*       On exit m is unchanged. */

/*     nsub is an integer variable. */
/*       On entry nsub is the number of free variables. */
/*       On exit nsub is unchanged. */

/*     ind is an integer array of dimension nsub. */
/*       On entry ind specifies the coordinate indices of free variables. */
/*       On exit ind is unchanged. */

/*     l is a double precision array of dimension n. */
/*       On entry l is the lower bound of x. */
/*       On exit l is unchanged. */

/*     u is a double precision array of dimension n. */
/*       On entry u is the upper bound of x. */
/*       On exit u is unchanged. */

/*     nbd is a integer array of dimension n. */
/*       On entry nbd represents the type of bounds imposed on the */
/*         variables, and must be specified as follows: */
/*         nbd(i)=0 if x(i) is unbounded, */
/*                1 if x(i) has only a lower bound, */
/*                2 if x(i) has both lower and upper bounds, and */
/*                3 if x(i) has only an upper bound. */
/*       On exit nbd is unchanged. */

/*     x is a double precision array of dimension n. */
/*       On entry x specifies the Cauchy point xcp. */
/*       On exit x(i) is the minimizer of Q over the subspace of */
/*                                                        free variables. */

/*     d is a double precision array of dimension n. */
/*       On entry d is the reduced gradient of Q at xcp. */
/*       On exit d is the Newton direction of Q. */

/*     ws and wy are double precision arrays; */
/*     theta is a double precision variable; */
/*     col is an integer variable; */
/*     head is an integer variable. */
/*       On entry they store the information defining the */
/*                                          limited memory BFGS matrix: */
/*         ws(n,m) stores S, a set of s-vectors; */
/*         wy(n,m) stores Y, a set of y-vectors; */
/*         theta is the scaling factor specifying B_0 = theta I; */
/*         col is the number of variable metric corrections stored; */
/*         head is the location of the 1st s- (or y-) vector in S (or Y). */
/*       On exit they are unchanged. */

/*     iword is an integer variable. */
/*       On entry iword is unspecified. */
/*       On exit iword specifies the status of the subspace solution. */
/*         iword = 0 if the solution is in the box, */
/*                 1 if some bound is encountered. */

/*     wv is a double precision working array of dimension 2m. */

/*     wn is a double precision array of dimension 2m x 2m. */
/*       On entry the upper triangle of wn stores the LEL^T factorization */
/*         of the indefinite matrix */

/*              K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ] */
/*                  [L_a -R_z           theta*S'AA'S ] */
/*                                                    where E = [-I  0] */
/*                                                              [ 0  I] */
/*       On exit wn is unchanged. */

/*     iprint is an INTEGER variable that must be set by the user. */
/*       It controls the frequency and type of output generated: */
/*        iprint<0    no output is generated; */
/*        iprint=0    print only one line at the last iteration; */
/*        0<iprint<99 print also f and |proj g| every iprint iterations; */
/*        iprint=99   print details of every iteration except n-vectors; */
/*        iprint=100  print also the changes of active set and final x; */
/*        iprint>100  print details of every iteration including x and g; */
/*       When iprint > 0, the file iterate.dat will be created to */
/*                        summarize the iteration. */

/*     info is an integer variable. */
/*       On entry info is unspecified. */
/*       On exit info = 0       for normal return, */
/*                    = nonzero for abnormal return */
/*                                  when the matrix K is ill-conditioned. */

/*     Subprograms called: */

/*       Linpack dtrsl. */


/*     References: */

/*       [1] R. H. Byrd, P. Lu, J. Nocedal and C. Zhu, ``A limited */
/*       memory algorithm for bound constrained optimization'', */
/*       SIAM J. Scientific Computing 16 (1995), no. 5, pp. 1190--1208. */



/*                           *  *  * */

/*     NEOS, November 1994. (Latest revision June 1996.) */
/*     Optimization Technology Center. */
/*     Argonne National Laboratory and Northwestern University. */
/*     Written by */
/*                        Ciyou Zhu */
/*     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal. */


/*     ************ */
/*<       integer          pointr,m2,col2,ibd,jy,js,i,j,k >*/
/*<       double precision alpha,dk,temp1,temp2 >*/
/*<       double precision one,zero >*/
/*<       parameter        (one=1.0d0,zero=0.0d0) >*/
/*<       if (nsub .le. 0) return >*/
    /* Parameter adjustments */
    --d__;
    --x;
    --nbd;
    --u;
    --l;
    wn_dim1 = 2 * *m;
    wn_offset = 1 + wn_dim1;
    wn -= wn_offset;
    --wv;
    wy_dim1 = *n;
    wy_offset = 1 + wy_dim1;
    wy -= wy_offset;
    ws_dim1 = *n;
    ws_offset = 1 + ws_dim1;
    ws -= ws_offset;
    --ind;

    /* Function Body */
    if (*nsub <= 0) {
	return 0;
    }
/*<       if (iprint .ge. 99) write (6,1001) >*/
/*
 1001 format (/,'----------------SUBSM entered-----------------',/)
 */
    if (*iprint >= 99) {
        printf("----------------SUBSM entered-----------------");
    }
/*     Compute wv = W'Zd. */
/*<       pointr = head  >*/
    pointr = *head;
/*<       do 20 i = 1, col >*/
    i__1 = *col;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<      	 temp1 = zero >*/
	temp1 = 0.;
/*< 	 temp2 = zero >*/
	temp2 = 0.;
/*< 	 do 10 j = 1, nsub >*/
	i__2 = *nsub;
	for (j = 1; j <= i__2; ++j) {
/*< 	    k = ind(j) >*/
	    k = ind[j];
/*< 	    temp1 = temp1 + wy(k,pointr)*d(j) >*/
	    temp1 += wy[k + pointr * wy_dim1] * d__[j];
/*< 	    temp2 = temp2 + ws(k,pointr)*d(j) >*/
	    temp2 += ws[k + pointr * ws_dim1] * d__[j];
/*<   10     continue >*/
/* L10: */
	}
/*< 	 wv(i) = temp1 >*/
	wv[i__] = temp1;
/*< 	 wv(col + i) = theta*temp2 >*/
	wv[*col + i__] = *theta * temp2;
/*< 	 pointr = mod(pointr,m) + 1 >*/
	pointr = pointr % *m + 1;
/*<   20  continue >*/
/* L20: */
    }
/*     Compute wv:=K^(-1)wv. */
/*<       m2 = 2*m >*/
    m2 = *m << 1;
/*<       col2 = 2*col >*/
    col2 = *col << 1;
/*<       call dtrsl(wn,m2,col2,wv,11,info) >*/
    dtrsl_(&wn[wn_offset], &m2, &col2, &wv[1], &c__11, info);
/*<       if (info .ne. 0) return >*/
    if (*info != 0) {
	return 0;
    }
/*<       do 25 i = 1, col >*/
    i__1 = *col;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	 wv(i) = -wv(i) >*/
	wv[i__] = -wv[i__];
/*<   25     continue >*/
/* L25: */
    }
/*<       call dtrsl(wn,m2,col2,wv,01,info) >*/
    dtrsl_(&wn[wn_offset], &m2, &col2, &wv[1], &c__1, info);
/*<       if (info .ne. 0) return >*/
    if (*info != 0) {
	return 0;
    }
/*     Compute d = (1/theta)d + (1/theta**2)Z'W wv. */
/*<       pointr = head >*/
    pointr = *head;
/*<       do 40 jy = 1, col >*/
    i__1 = *col;
    for (jy = 1; jy <= i__1; ++jy) {
/*<          js = col + jy >*/
	js = *col + jy;
/*< 	 do 30 i = 1, nsub >*/
	i__2 = *nsub;
	for (i__ = 1; i__ <= i__2; ++i__) {
/*< 	    k = ind(i) >*/
	    k = ind[i__];
/*< 	  >*/
	    d__[i__] = d__[i__] + wy[k + pointr * wy_dim1] * wv[jy] / *theta 
		    + ws[k + pointr * ws_dim1] * wv[js];
/*<   30     continue >*/
/* L30: */
	}
/*< 	 pointr = mod(pointr,m) + 1 >*/
	pointr = pointr % *m + 1;
/*<   40  continue >*/
/* L40: */
    }
/*<       do 50 i = 1, nsub >*/
    i__1 = *nsub;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	 d(i) = d(i)/theta >*/
	d__[i__] /= *theta;
/*<   50  continue >*/
/* L50: */
    }
/*     Backtrack to the feasible region. */
/*<       alpha = one >*/
    alpha = 1.;
/*<       temp1 = alpha	 >*/
    temp1 = alpha;
/*<       do 60 i = 1, nsub >*/
    i__1 = *nsub;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	 k = ind(i) >*/
	k = ind[i__];
/*<          dk = d(i) >*/
	dk = d__[i__];
/*< 	 if (nbd(k) .ne. 0) then >*/
	if (nbd[k] != 0) {
/*<    	    if (dk .lt. zero .and. nbd(k) .le. 2) then >*/
	    if (dk < 0. && nbd[k] <= 2) {
/*< 	       temp2 = l(k) - x(k) >*/
		temp2 = l[k] - x[k];
/*< 	       if (temp2 .ge. zero) then >*/
		if (temp2 >= 0.) {
/*< 		  temp1 = zero >*/
		    temp1 = 0.;
/*< 	       else if (dk*alpha .lt. temp2) then >*/
		} else if (dk * alpha < temp2) {
/*< 		  temp1 = temp2/dk >*/
		    temp1 = temp2 / dk;
/*<  	       endif >*/
		}
/*<    	    else if (dk .gt. zero .and. nbd(k) .ge. 2) then >*/
	    } else if (dk > 0. && nbd[k] >= 2) {
/*< 	       temp2 = u(k) - x(k) >*/
		temp2 = u[k] - x[k];
/*< 	       if (temp2 .le. zero) then >*/
		if (temp2 <= 0.) {
/*< 		  temp1 = zero >*/
		    temp1 = 0.;
/*< 	       else if (dk*alpha .gt. temp2) then >*/
		} else if (dk * alpha > temp2) {
/*< 		  temp1 = temp2/dk >*/
		    temp1 = temp2 / dk;
/*<  	       endif >*/
		}
/*<             endif >*/
	    }
/*<             if (temp1 .lt. alpha) then >*/
	    if (temp1 < alpha) {
/*< 	       alpha = temp1 >*/
		alpha = temp1;
/*< 	       ibd = i >*/
		ibd = i__;
/*<             endif >*/
	    }
/*<          endif >*/
	}
/*<   60  continue >*/
/* L60: */
    }
/*<       if (alpha .lt. one) then >*/
    if (alpha < 1.) {
/*<       	 dk = d(ibd) >*/
	dk = d__[ibd];
/*<       	 k = ind(ibd) >*/
	k = ind[ibd];
/*<       	 if (dk .gt. zero) then >*/
	if (dk > 0.) {
/*<             x(k) = u(k) >*/
	    x[k] = u[k];
/*<             d(ibd) = zero >*/
	    d__[ibd] = 0.;
/*<      	 else if (dk .lt. zero) then >*/
	} else if (dk < 0.) {
/*<             x(k) = l(k) >*/
	    x[k] = l[k];
/*< 	    d(ibd) = zero >*/
	    d__[ibd] = 0.;
/*<      	 endif >*/
	}
/*<       endif >*/
    }
/*<       do 70 i = 1, nsub >*/
    i__1 = *nsub;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	 k = ind(i) >*/
	k = ind[i__];
/*< 	 x(k) = x(k) + alpha*d(i) >*/
	x[k] += alpha * d__[i__];
/*<   70  continue >*/
/* L70: */
    }
/*<       if (iprint .ge. 99) then >*/
    if (*iprint >= 99) {
/*< 	 if (alpha .lt. one) then >*/
	if (alpha < 1.) {
/*<             write (6,1002) alpha >*/
/*
 1002 format ( 'ALPHA = ',f7.5,' backtrack to the BOX')	
*/
            printf("ALPHA = %7.5g backtrack to the BOX\n", alpha);
/*<          else >*/
	} else {
/*<             write (6,*) 'SM solution inside the box' >*/
            printf("SM solution inside the box\n");
/*< 	 end if	 >*/
	}
/*< 	 if (iprint .gt.100) write (6,1003) (x(i),i=1,n) >*/
/*
 1003 format ('Subspace solution X =  ',/,(4x,1p,6(1x,d11.4)))
 */
	if (*iprint > 100) {
	    i__1 = *n;
            lbfgsb_printf_vec("Subspace solution X", x, i__1);
	}
/*<       endif >*/
    }
/*<       if (alpha .lt. one) then >*/
    if (alpha < 1.) {
/*<          iword = 1 >*/
	*iword = 1;
/*<       else >*/
    } else {
/*<          iword = 0 >*/
	*iword = 0;
/*<       endif  >*/
    }
/*<       if (iprint .ge. 99) write (6,1004) >*/
/*
 1004 format (/,'----------------exit SUBSM --------------------',/)
 */
    if (*iprint >= 99) {
        printf("----------------exit SUBSM --------------------");
    }
/*<  1001 format (/,'----------------SUBSM entered-----------------',/) >*/
/*<  1002 format ( 'ALPHA = ',f7.5,' backtrack to the BOX')	 >*/
/*<  1003 format ('Subspace solution X =  ',/,(4x,1p,6(1x,d11.4))) >*/
/*<  1004 format (/,'----------------exit SUBSM --------------------',/) >*/
/*<       return >*/
    return 0;
/*<       end >*/
} /* subsm_ */

/* ====================== The end of subsm =============================== */
/*<    >*/
/* Subroutine */ int dcsrch_(doublereal *f, doublereal *g, doublereal *stp, 
	doublereal *ftol, doublereal *gtol, doublereal *xtol, doublereal *
	stpmin, doublereal *stpmax, char *task, integer *isave, doublereal *
	dsave, ftnlen task_len)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    doublereal fm, gm, fx, fy, gx, gy, fxm, fym, gxm, gym, stx, sty;
    integer stage;
    doublereal finit, ginit, width, ftest, gtest, stmin, stmax, width1;
    logical brackt;
    extern /* Subroutine */ int dcstep_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, logical *, doublereal *,
	     doublereal *);

/*<       character*(*) task >*/
/*<       integer isave(2) >*/
/*<       double precision f,g,stp,ftol,gtol,xtol,stpmin,stpmax >*/
/*<       double precision dsave(13) >*/
/*     ********** */

/*     Subroutine dcsrch */

/*     This subroutine finds a step that satisfies a sufficient */
/*     decrease condition and a curvature condition. */

/*     Each call of the subroutine updates an interval with */
/*     endpoints stx and sty. The interval is initially chosen */
/*     so that it contains a minimizer of the modified function */

/*           psi(stp) = f(stp) - f(0) - ftol*stp*f'(0). */

/*     If psi(stp) <= 0 and f'(stp) >= 0 for some step, then the */
/*     interval is chosen so that it contains a minimizer of f. */

/*     The algorithm is designed to find a step that satisfies */
/*     the sufficient decrease condition */

/*           f(stp) <= f(0) + ftol*stp*f'(0), */

/*     and the curvature condition */

/*           abs(f'(stp)) <= gtol*abs(f'(0)). */

/*     If ftol is less than gtol and if, for example, the function */
/*     is bounded below, then there is always a step which satisfies */
/*     both conditions. */

/*     If no step can be found that satisfies both conditions, then */
/*     the algorithm stops with a warning. In this case stp only */
/*     satisfies the sufficient decrease condition. */

/*     A typical invocation of dcsrch has the following outline: */

/*     task = 'START' */
/*  10 continue */
/*        call dcsrch( ... ) */
/*        if (task .eq. 'FG') then */
/*           Evaluate the function and the gradient at stp */
/*           goto 10 */
/*           end if */

/*     NOTE: The user must no alter work arrays between calls. */

/*     The subroutine statement is */

/*        subroutine dcsrch(f,g,stp,ftol,gtol,xtol,stpmin,stpmax, */
/*                          task,isave,dsave) */
/*     where */

/*       f is a double precision variable. */
/*         On initial entry f is the value of the function at 0. */
/*            On subsequent entries f is the value of the */
/*            function at stp. */
/*         On exit f is the value of the function at stp. */

/* 	g is a double precision variable. */
/*         On initial entry g is the derivative of the function at 0. */
/*            On subsequent entries g is the derivative of the */
/*            function at stp. */
/*         On exit g is the derivative of the function at stp. */

/* 	stp is a double precision variable. */
/*         On entry stp is the current estimate of a satisfactory */
/*            step. On initial entry, a positive initial estimate */
/*            must be provided. */
/*         On exit stp is the current estimate of a satisfactory step */
/*            if task = 'FG'. If task = 'CONV' then stp satisfies */
/*            the sufficient decrease and curvature condition. */

/*       ftol is a double precision variable. */
/*         On entry ftol specifies a nonnegative tolerance for the */
/*            sufficient decrease condition. */
/*         On exit ftol is unchanged. */

/*       gtol is a double precision variable. */
/*         On entry gtol specifies a nonnegative tolerance for the */
/*            curvature condition. */
/*         On exit gtol is unchanged. */

/* 	xtol is a double precision variable. */
/*         On entry xtol specifies a nonnegative relative tolerance */
/*            for an acceptable step. The subroutine exits with a */
/*            warning if the relative difference between sty and stx */
/*            is less than xtol. */
/*         On exit xtol is unchanged. */

/* 	stpmin is a double precision variable. */
/*         On entry stpmin is a nonnegative lower bound for the step. */
/*         On exit stpmin is unchanged. */

/* 	stpmax is a double precision variable. */
/*         On entry stpmax is a nonnegative upper bound for the step. */
/*         On exit stpmax is unchanged. */

/*       task is a character variable of length at least 60. */
/*         On initial entry task must be set to 'START'. */
/*         On exit task indicates the required action: */

/*            If task(1:2) = 'FG' then evaluate the function and */
/*            derivative at stp and call dcsrch again. */

/*            If task(1:4) = 'CONV' then the search is successful. */

/*            If task(1:4) = 'WARN' then the subroutine is not able */
/*            to satisfy the convergence conditions. The exit value of */
/*            stp contains the best point found during the search. */

/*            If task(1:5) = 'ERROR' then there is an error in the */
/*            input arguments. */

/*         On exit with convergence, a warning or an error, the */
/*            variable task contains additional information. */

/*       isave is an integer work array of dimension 2. */

/*       dsave is a double precision work array of dimension 13. */

/*     Subprograms called */

/* 	MINPACK-2 ... dcstep */

/*     MINPACK-1 Project. June 1983. */
/*     Argonne National Laboratory. */
/*     Jorge J. More' and David J. Thuente. */

/*     MINPACK-2 Project. October 1993. */
/*     Argonne National Laboratory and University of Minnesota. */
/*     Brett M. Averick, Richard G. Carter, and Jorge J. More'. */

/*     ********** */
/*<       double precision zero,p5,p66 >*/
/*<       parameter(zero=0.0d0,p5=0.5d0,p66=0.66d0) >*/
/*<       double precision xtrapl,xtrapu >*/
/*<       parameter(xtrapl=1.1d0,xtrapu=4.0d0) >*/
/*<       logical brackt >*/
/*<       integer stage >*/
/*<    >*/
/*     Initialization block. */
/*<       if (task(1:5) .eq. 'START') then >*/
    /* Parameter adjustments */
    --dsave;
    --isave;

    /* Function Body */
    if (s_cmp(task, "START", (ftnlen)5, (ftnlen)5) == 0) {
/*        Check the input arguments for errors. */
/*<          if (stp .lt. stpmin) task = 'ERROR: STP .LT. STPMIN' >*/
	if (*stp < *stpmin) {
	    s_copy(task, "ERROR: STP .LT. STPMIN", task_len, (ftnlen)22);
	}
/*<          if (stp .gt. stpmax) task = 'ERROR: STP .GT. STPMAX' >*/
	if (*stp > *stpmax) {
	    s_copy(task, "ERROR: STP .GT. STPMAX", task_len, (ftnlen)22);
	}
/*<          if (g .ge. zero) task = 'ERROR: INITIAL G .GE. ZERO' >*/
	if (*g >= 0.) {
	    s_copy(task, "ERROR: INITIAL G .GE. ZERO", task_len, (ftnlen)26);
	}
/*<          if (ftol .lt. zero) task = 'ERROR: FTOL .LT. ZERO' >*/
	if (*ftol < 0.) {
	    s_copy(task, "ERROR: FTOL .LT. ZERO", task_len, (ftnlen)21);
	}
/*<          if (gtol .lt. zero) task = 'ERROR: GTOL .LT. ZERO' >*/
	if (*gtol < 0.) {
	    s_copy(task, "ERROR: GTOL .LT. ZERO", task_len, (ftnlen)21);
	}
/*<          if (xtol .lt. zero) task = 'ERROR: XTOL .LT. ZERO' >*/
	if (*xtol < 0.) {
	    s_copy(task, "ERROR: XTOL .LT. ZERO", task_len, (ftnlen)21);
	}
/*<          if (stpmin .lt. zero) task = 'ERROR: STPMIN .LT. ZERO' >*/
	if (*stpmin < 0.) {
	    s_copy(task, "ERROR: STPMIN .LT. ZERO", task_len, (ftnlen)23);
	}
/*<          if (stpmax .lt. stpmin) task = 'ERROR: STPMAX .LT. STPMIN' >*/
	if (*stpmax < *stpmin) {
	    s_copy(task, "ERROR: STPMAX .LT. STPMIN", task_len, (ftnlen)25);
	}
/*        Exit if there are errors on input. */
/*<          if (task(1:5) .eq. 'ERROR') return >*/
	if (s_cmp(task, "ERROR", (ftnlen)5, (ftnlen)5) == 0) {
	    return 0;
	}
/*        Initialize local variables. */
/*<          brackt = .false. >*/
	brackt = FALSE_;
/*<          stage = 1 >*/
	stage = 1;
/*<          finit = f >*/
	finit = *f;
/*<          ginit = g >*/
	ginit = *g;
/*<          gtest = ftol*ginit >*/
	gtest = *ftol * ginit;
/*<          width = stpmax - stpmin >*/
	width = *stpmax - *stpmin;
/*<          width1 = width/p5 >*/
	width1 = width / .5;
/*        The variables stx, fx, gx contain the values of the step, */
/*        function, and derivative at the best step. */
/*        The variables sty, fy, gy contain the value of the step, */
/*        function, and derivative at sty. */
/*        The variables stp, f, g contain the values of the step, */
/*        function, and derivative at stp. */
/*<          stx = zero >*/
	stx = 0.;
/*<          fx = finit >*/
	fx = finit;
/*<          gx = ginit >*/
	gx = ginit;
/*<          sty = zero >*/
	sty = 0.;
/*<          fy = finit >*/
	fy = finit;
/*<          gy = ginit >*/
	gy = ginit;
/*<          stmin = zero >*/
	stmin = 0.;
/*<          stmax = stp + xtrapu*stp >*/
	stmax = *stp + *stp * 4.;
/*<          task = 'FG' >*/
	s_copy(task, "FG", task_len, (ftnlen)2);
/*<          goto 1000 >*/
	goto L1000;
/*<       else >*/
    } else {
/*        Restore local variables. */
/*<          if (isave(1) .eq. 1) then >*/
	if (isave[1] == 1) {
/*<             brackt = .true. >*/
	    brackt = TRUE_;
/*<          else >*/
	} else {
/*<             brackt = .false. >*/
	    brackt = FALSE_;
/*<          endif >*/
	}
/*<          stage = isave(2)  >*/
	stage = isave[2];
/*<          ginit = dsave(1)  >*/
	ginit = dsave[1];
/*<          gtest = dsave(2)  >*/
	gtest = dsave[2];
/*<          gx = dsave(3)  >*/
	gx = dsave[3];
/*<          gy = dsave(4)  >*/
	gy = dsave[4];
/*<          finit = dsave(5)  >*/
	finit = dsave[5];
/*<          fx = dsave(6)  >*/
	fx = dsave[6];
/*<          fy = dsave(7)  >*/
	fy = dsave[7];
/*<          stx = dsave(8)  >*/
	stx = dsave[8];
/*<          sty = dsave(9)  >*/
	sty = dsave[9];
/*<          stmin = dsave(10)  >*/
	stmin = dsave[10];
/*<          stmax = dsave(11)  >*/
	stmax = dsave[11];
/*<          width = dsave(12)  >*/
	width = dsave[12];
/*<          width1 = dsave(13)  >*/
	width1 = dsave[13];
/*<       endif >*/
    }
/*     If psi(stp) <= 0 and f'(stp) >= 0 for some step, then the */
/*     algorithm enters the second stage. */
/*<       ftest = finit + stp*gtest >*/
    ftest = finit + *stp * gtest;
/*<    >*/
    if (stage == 1 && *f <= ftest && *g >= 0.) {
	stage = 2;
    }
/*     Test for warnings. */
/*<    >*/
    if (brackt && (*stp <= stmin || *stp >= stmax)) {
	s_copy(task, "WARNING: ROUNDING ERRORS PREVENT PROGRESS", task_len, (
		ftnlen)41);
    }
/*<    >*/
    if (brackt && stmax - stmin <= *xtol * stmax) {
	s_copy(task, "WARNING: XTOL TEST SATISFIED", task_len, (ftnlen)28);
    }
/*<    >*/
    if (*stp == *stpmax && *f <= ftest && *g <= gtest) {
	s_copy(task, "WARNING: STP = STPMAX", task_len, (ftnlen)21);
    }
/*<    >*/
    if (*stp == *stpmin && (*f > ftest || *g >= gtest)) {
	s_copy(task, "WARNING: STP = STPMIN", task_len, (ftnlen)21);
    }
/*     Test for convergence. */
/*<    >*/
    if (*f <= ftest && abs(*g) <= *gtol * (-ginit)) {
	s_copy(task, "CONVERGENCE", task_len, (ftnlen)11);
    }
/*     Test for termination. */
/*<       if (task(1:4) .eq. 'WARN' .or. task(1:4) .eq. 'CONV') goto 1000 >*/
    if (s_cmp(task, "WARN", (ftnlen)4, (ftnlen)4) == 0 || s_cmp(task, "CONV", 
	    (ftnlen)4, (ftnlen)4) == 0) {
	goto L1000;
    }
/*     A modified function is used to predict the step during the */
/*     first stage if a lower function value has been obtained but */
/*     the decrease is not sufficient. */
/*<       if (stage .eq. 1 .and. f .le. fx .and. f .gt. ftest) then >*/
    if (stage == 1 && *f <= fx && *f > ftest) {
/*        Define the modified function and derivative values. */
/*<          fm = f - stp*gtest >*/
	fm = *f - *stp * gtest;
/*<          fxm = fx - stx*gtest >*/
	fxm = fx - stx * gtest;
/*<          fym = fy - sty*gtest >*/
	fym = fy - sty * gtest;
/*<          gm = g - gtest >*/
	gm = *g - gtest;
/*<          gxm = gx - gtest >*/
	gxm = gx - gtest;
/*<          gym = gy - gtest >*/
	gym = gy - gtest;
/*        Call dcstep to update stx, sty, and to compute the new step. */
/*<    >*/
	dcstep_(&stx, &fxm, &gxm, &sty, &fym, &gym, stp, &fm, &gm, &brackt, &
		stmin, &stmax);
/*        Reset the function and derivative values for f. */
/*<          fx = fxm + stx*gtest >*/
	fx = fxm + stx * gtest;
/*<          fy = fym + sty*gtest >*/
	fy = fym + sty * gtest;
/*<          gx = gxm + gtest >*/
	gx = gxm + gtest;
/*<          gy = gym + gtest >*/
	gy = gym + gtest;
/*<       else >*/
    } else {
/*       Call dcstep to update stx, sty, and to compute the new step. */
/*<    >*/
	dcstep_(&stx, &fx, &gx, &sty, &fy, &gy, stp, f, g, &brackt, &stmin, &
		stmax);
/*<       endif >*/
    }
/*     Decide if a bisection step is needed. */
/*<       if (brackt) then >*/
    if (brackt) {
/*<          if (abs(sty-stx) .ge. p66*width1) stp = stx + p5*(sty - stx) >*/
	if ((d__1 = sty - stx, abs(d__1)) >= width1 * .66) {
	    *stp = stx + (sty - stx) * .5;
	}
/*<          width1 = width >*/
	width1 = width;
/*<          width = abs(sty-stx) >*/
	width = (d__1 = sty - stx, abs(d__1));
/*<       endif >*/
    }
/*     Set the minimum and maximum steps allowed for stp. */
/*<       if (brackt) then >*/
    if (brackt) {
/*<          stmin = min(stx,sty) >*/
	stmin = min(stx,sty);
/*<          stmax = max(stx,sty) >*/
	stmax = max(stx,sty);
/*<       else >*/
    } else {
/*<          stmin = stp + xtrapl*(stp - stx) >*/
	stmin = *stp + (*stp - stx) * 1.1;
/*<          stmax = stp + xtrapu*(stp - stx) >*/
	stmax = *stp + (*stp - stx) * 4.;
/*<       endif >*/
    }
/*     Force the step to be within the bounds stpmax and stpmin. */
/*<       stp = max(stp,stpmin) >*/
    *stp = max(*stp,*stpmin);
/*<       stp = min(stp,stpmax) >*/
    *stp = min(*stp,*stpmax);
/*     If further progress is not possible, let stp be the best */
/*     point obtained during the search. */
/*<    >*/
    if ((brackt && (*stp <= stmin || *stp >= stmax)) ||
        (brackt && stmax - stmin <= *xtol * stmax)) {
	*stp = stx;
    }
/*     Obtain another function and derivative. */
/*<       task = 'FG' >*/
    s_copy(task, "FG", task_len, (ftnlen)2);
/*<  1000 continue >*/
L1000:
/*     Save local variables. */
/*<       if (brackt) then >*/
    if (brackt) {
/*<          isave(1) = 1 >*/
	isave[1] = 1;
/*<       else >*/
    } else {
/*<          isave(1) = 0 >*/
	isave[1] = 0;
/*<       endif >*/
    }
/*<       isave(2) = stage >*/
    isave[2] = stage;
/*<       dsave(1) =  ginit >*/
    dsave[1] = ginit;
/*<       dsave(2) =  gtest >*/
    dsave[2] = gtest;
/*<       dsave(3) =  gx >*/
    dsave[3] = gx;
/*<       dsave(4) =  gy >*/
    dsave[4] = gy;
/*<       dsave(5) =  finit >*/
    dsave[5] = finit;
/*<       dsave(6) =  fx >*/
    dsave[6] = fx;
/*<       dsave(7) =  fy >*/
    dsave[7] = fy;
/*<       dsave(8) =  stx >*/
    dsave[8] = stx;
/*<       dsave(9) =  sty >*/
    dsave[9] = sty;
/*<       dsave(10) = stmin >*/
    dsave[10] = stmin;
/*<       dsave(11) = stmax >*/
    dsave[11] = stmax;
/*<       dsave(12) = width >*/
    dsave[12] = width;
/*<       dsave(13) = width1 >*/
    dsave[13] = width1;
/*<       end >*/
    return 0;
} /* dcsrch_ */

/* ====================== The end of dcsrch ============================== */
/*<    >*/
/* Subroutine */ int dcstep_(doublereal *stx, doublereal *fx, doublereal *dx, 
	doublereal *sty, doublereal *fy, doublereal *dy, doublereal *stp, 
	doublereal *fp, doublereal *dp, logical *brackt, doublereal *stpmin, 
	doublereal *stpmax)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal p, q, r__, s, sgnd, stpc, stpf, stpq, gamma, theta;

/*<       logical brackt >*/
/*<       double precision stx,fx,dx,sty,fy,dy,stp,fp,dp,stpmin,stpmax >*/
/*     ********** */

/*     Subroutine dcstep */

/*     This subroutine computes a safeguarded step for a search */
/*     procedure and updates an interval that contains a step that */
/*     satisfies a sufficient decrease and a curvature condition. */

/*     The parameter stx contains the step with the least function */
/*     value. If brackt is set to .true. then a minimizer has */
/*     been bracketed in an interval with endpoints stx and sty. */
/*     The parameter stp contains the current step. */
/*     The subroutine assumes that if brackt is set to .true. then */

/*           min(stx,sty) < stp < max(stx,sty), */

/*     and that the derivative at stx is negative in the direction */
/*     of the step. */

/*     The subroutine statement is */

/*       subroutine dcstep(stx,fx,dx,sty,fy,dy,stp,fp,dp,brackt, */
/*                         stpmin,stpmax) */

/*     where */

/*       stx is a double precision variable. */
/*         On entry stx is the best step obtained so far and is an */
/*            endpoint of the interval that contains the minimizer. */
/*         On exit stx is the updated best step. */

/*       fx is a double precision variable. */
/*         On entry fx is the function at stx. */
/*         On exit fx is the function at stx. */

/*       dx is a double precision variable. */
/*         On entry dx is the derivative of the function at */
/*            stx. The derivative must be negative in the direction of */
/*            the step, that is, dx and stp - stx must have opposite */
/*            signs. */
/*         On exit dx is the derivative of the function at stx. */

/*       sty is a double precision variable. */
/*         On entry sty is the second endpoint of the interval that */
/*            contains the minimizer. */
/*         On exit sty is the updated endpoint of the interval that */
/*            contains the minimizer. */

/*       fy is a double precision variable. */
/*         On entry fy is the function at sty. */
/*         On exit fy is the function at sty. */

/*       dy is a double precision variable. */
/*         On entry dy is the derivative of the function at sty. */
/*         On exit dy is the derivative of the function at the exit sty. */

/*       stp is a double precision variable. */
/*         On entry stp is the current step. If brackt is set to .true. */
/*            then on input stp must be between stx and sty. */
/*         On exit stp is a new trial step. */

/*       fp is a double precision variable. */
/*         On entry fp is the function at stp */
/*         On exit fp is unchanged. */

/*       dp is a double precision variable. */
/*         On entry dp is the the derivative of the function at stp. */
/*         On exit dp is unchanged. */

/*       brackt is an logical variable. */
/*         On entry brackt specifies if a minimizer has been bracketed. */
/*            Initially brackt must be set to .false. */
/*         On exit brackt specifies if a minimizer has been bracketed. */
/*            When a minimizer is bracketed brackt is set to .true. */

/*       stpmin is a double precision variable. */
/*         On entry stpmin is a lower bound for the step. */
/*         On exit stpmin is unchanged. */

/*       stpmax is a double precision variable. */
/*         On entry stpmax is an upper bound for the step. */
/*         On exit stpmax is unchanged. */

/*     MINPACK-1 Project. June 1983 */
/*     Argonne National Laboratory. */
/*     Jorge J. More' and David J. Thuente. */

/*     MINPACK-2 Project. October 1993. */
/*     Argonne National Laboratory and University of Minnesota. */
/*     Brett M. Averick and Jorge J. More'. */

/*     ********** */
/*<       double precision zero,p66,two,three >*/
/*<       parameter(zero=0.0d0,p66=0.66d0,two=2.0d0,three=3.0d0) >*/
/*<       double precision gamma,p,q,r,s,sgnd,stpc,stpf,stpq,theta >*/
/*<       sgnd = dp*(dx/abs(dx)) >*/
    sgnd = *dp * (*dx / abs(*dx));
/*     First case: A higher function value. The minimum is bracketed. */
/*     If the cubic step is closer to stx than the quadratic step, the */
/*     cubic step is taken, otherwise the average of the cubic and */
/*     quadratic steps is taken. */
/*<       if (fp .gt. fx) then >*/
    if (*fp > *fx) {
/*<          theta = three*(fx - fp)/(stp - stx) + dx + dp >*/
	theta = (*fx - *fp) * 3. / (*stp - *stx) + *dx + *dp;
/*<          s = max(abs(theta),abs(dx),abs(dp)) >*/
/* Computing MAX */
	d__1 = abs(theta), d__2 = abs(*dx), d__1 = max(d__1,d__2), d__2 = abs(
		*dp);
	s = max(d__1,d__2);
/*<          gamma = s*sqrt((theta/s)**2 - (dx/s)*(dp/s)) >*/
/* Computing 2nd power */
	d__1 = theta / s;
	gamma = s * sqrt(d__1 * d__1 - *dx / s * (*dp / s));
/*<          if (stp .lt. stx) gamma = -gamma >*/
	if (*stp < *stx) {
	    gamma = -gamma;
	}
/*<          p = (gamma - dx) + theta >*/
	p = gamma - *dx + theta;
/*<          q = ((gamma - dx) + gamma) + dp >*/
	q = gamma - *dx + gamma + *dp;
/*<          r = p/q >*/
	r__ = p / q;
/*<          stpc = stx + r*(stp - stx) >*/
	stpc = *stx + r__ * (*stp - *stx);
/*<    >*/
	stpq = *stx + *dx / ((*fx - *fp) / (*stp - *stx) + *dx) / 2. * (*stp 
		- *stx);
/*<          if (abs(stpc-stx) .lt. abs(stpq-stx)) then >*/
	if ((d__1 = stpc - *stx, abs(d__1)) < (d__2 = stpq - *stx, abs(d__2)))
		 {
/*<             stpf = stpc >*/
	    stpf = stpc;
/*<          else >*/
	} else {
/*<             stpf = stpc + (stpq - stpc)/two >*/
	    stpf = stpc + (stpq - stpc) / 2.;
/*<          endif >*/
	}
/*<          brackt = .true. >*/
	*brackt = TRUE_;
/*     Second case: A lower function value and derivatives of opposite */
/*     sign. The minimum is bracketed. If the cubic step is farther from */
/*     stp than the secant step, the cubic step is taken, otherwise the */
/*     secant step is taken. */
/*<       else if (sgnd .lt. zero) then >*/
    } else if (sgnd < 0.) {
/*<          theta = three*(fx - fp)/(stp - stx) + dx + dp >*/
	theta = (*fx - *fp) * 3. / (*stp - *stx) + *dx + *dp;
/*<          s = max(abs(theta),abs(dx),abs(dp)) >*/
/* Computing MAX */
	d__1 = abs(theta), d__2 = abs(*dx), d__1 = max(d__1,d__2), d__2 = abs(
		*dp);
	s = max(d__1,d__2);
/*<          gamma = s*sqrt((theta/s)**2 - (dx/s)*(dp/s)) >*/
/* Computing 2nd power */
	d__1 = theta / s;
	gamma = s * sqrt(d__1 * d__1 - *dx / s * (*dp / s));
/*<          if (stp .gt. stx) gamma = -gamma >*/
	if (*stp > *stx) {
	    gamma = -gamma;
	}
/*<          p = (gamma - dp) + theta >*/
	p = gamma - *dp + theta;
/*<          q = ((gamma - dp) + gamma) + dx >*/
	q = gamma - *dp + gamma + *dx;
/*<          r = p/q >*/
	r__ = p / q;
/*<          stpc = stp + r*(stx - stp) >*/
	stpc = *stp + r__ * (*stx - *stp);
/*<          stpq = stp + (dp/(dp - dx))*(stx - stp) >*/
	stpq = *stp + *dp / (*dp - *dx) * (*stx - *stp);
/*<          if (abs(stpc-stp) .gt. abs(stpq-stp)) then >*/
	if ((d__1 = stpc - *stp, abs(d__1)) > (d__2 = stpq - *stp, abs(d__2)))
		 {
/*<             stpf = stpc >*/
	    stpf = stpc;
/*<          else >*/
	} else {
/*<             stpf = stpq >*/
	    stpf = stpq;
/*<          endif >*/
	}
/*<          brackt = .true. >*/
	*brackt = TRUE_;
/*     Third case: A lower function value, derivatives of the same sign, */
/*     and the magnitude of the derivative decreases. */
/*<       else if (abs(dp) .lt. abs(dx)) then >*/
    } else if (abs(*dp) < abs(*dx)) {
/*        The cubic step is computed only if the cubic tends to infinity */
/*        in the direction of the step or if the minimum of the cubic */
/*        is beyond stp. Otherwise the cubic step is defined to be the */
/*        secant step. */
/*<          theta = three*(fx - fp)/(stp - stx) + dx + dp >*/
	theta = (*fx - *fp) * 3. / (*stp - *stx) + *dx + *dp;
/*<          s = max(abs(theta),abs(dx),abs(dp)) >*/
/* Computing MAX */
	d__1 = abs(theta), d__2 = abs(*dx), d__1 = max(d__1,d__2), d__2 = abs(
		*dp);
	s = max(d__1,d__2);
/*        The case gamma = 0 only arises if the cubic does not tend */
/*        to infinity in the direction of the step. */
/*<          gamma = s*sqrt(max(zero,(theta/s)**2-(dx/s)*(dp/s))) >*/
/* Computing MAX */
/* Computing 2nd power */
	d__3 = theta / s;
	d__1 = 0., d__2 = d__3 * d__3 - *dx / s * (*dp / s);
	gamma = s * sqrt((max(d__1,d__2)));
/*<          if (stp .gt. stx) gamma = -gamma >*/
	if (*stp > *stx) {
	    gamma = -gamma;
	}
/*<          p = (gamma - dp) + theta >*/
	p = gamma - *dp + theta;
/*<          q = (gamma + (dx - dp)) + gamma >*/
	q = gamma + (*dx - *dp) + gamma;
/*<          r = p/q >*/
	r__ = p / q;
/*<          if (r .lt. zero .and. gamma .ne. zero) then >*/
	if (r__ < 0. && gamma != 0.) {
/*<             stpc = stp + r*(stx - stp) >*/
	    stpc = *stp + r__ * (*stx - *stp);
/*<          else if (stp .gt. stx) then >*/
	} else if (*stp > *stx) {
/*<             stpc = stpmax >*/
	    stpc = *stpmax;
/*<          else >*/
	} else {
/*<             stpc = stpmin >*/
	    stpc = *stpmin;
/*<          endif >*/
	}
/*<          stpq = stp + (dp/(dp - dx))*(stx - stp) >*/
	stpq = *stp + *dp / (*dp - *dx) * (*stx - *stp);
/*<          if (brackt) then >*/
	if (*brackt) {
/*           A minimizer has been bracketed. If the cubic step is */
/*           closer to stp than the secant step, the cubic step is */
/*           taken, otherwise the secant step is taken. */
/*<             if (abs(stpc-stp) .lt. abs(stpq-stp)) then >*/
	    if ((d__1 = stpc - *stp, abs(d__1)) < (d__2 = stpq - *stp, abs(
		    d__2))) {
/*<                stpf = stpc >*/
		stpf = stpc;
/*<             else >*/
	    } else {
/*<                stpf = stpq >*/
		stpf = stpq;
/*<             endif >*/
	    }
/*<             if (stp .gt. stx) then >*/
	    if (*stp > *stx) {
/*<                stpf = min(stp+p66*(sty-stp),stpf) >*/
/* Computing MIN */
		d__1 = *stp + (*sty - *stp) * .66;
		stpf = min(d__1,stpf);
/*<             else >*/
	    } else {
/*<                stpf = max(stp+p66*(sty-stp),stpf) >*/
/* Computing MAX */
		d__1 = *stp + (*sty - *stp) * .66;
		stpf = max(d__1,stpf);
/*<             endif >*/
	    }
/*<          else >*/
	} else {
/*           A minimizer has not been bracketed. If the cubic step is */
/*           farther from stp than the secant step, the cubic step is */
/*           taken, otherwise the secant step is taken. */
/*<             if (abs(stpc-stp) .gt. abs(stpq-stp)) then >*/
	    if ((d__1 = stpc - *stp, abs(d__1)) > (d__2 = stpq - *stp, abs(
		    d__2))) {
/*<                stpf = stpc >*/
		stpf = stpc;
/*<             else >*/
	    } else {
/*<                stpf = stpq >*/
		stpf = stpq;
/*<             endif >*/
	    }
/*<             stpf = min(stpmax,stpf) >*/
	    stpf = min(*stpmax,stpf);
/*<             stpf = max(stpmin,stpf) >*/
	    stpf = max(*stpmin,stpf);
/*<          endif >*/
	}
/*     Fourth case: A lower function value, derivatives of the same sign, */
/*     and the magnitude of the derivative does not decrease. If the */
/*     minimum is not bracketed, the step is either stpmin or stpmax, */
/*     otherwise the cubic step is taken. */
/*<       else >*/
    } else {
/*<          if (brackt) then >*/
	if (*brackt) {
/*<             theta = three*(fp - fy)/(sty - stp) + dy + dp >*/
	    theta = (*fp - *fy) * 3. / (*sty - *stp) + *dy + *dp;
/*<             s = max(abs(theta),abs(dy),abs(dp)) >*/
/* Computing MAX */
	    d__1 = abs(theta), d__2 = abs(*dy), d__1 = max(d__1,d__2), d__2 = 
		    abs(*dp);
	    s = max(d__1,d__2);
/*<             gamma = s*sqrt((theta/s)**2 - (dy/s)*(dp/s)) >*/
/* Computing 2nd power */
	    d__1 = theta / s;
	    gamma = s * sqrt(d__1 * d__1 - *dy / s * (*dp / s));
/*<             if (stp .gt. sty) gamma = -gamma >*/
	    if (*stp > *sty) {
		gamma = -gamma;
	    }
/*<             p = (gamma - dp) + theta >*/
	    p = gamma - *dp + theta;
/*<             q = ((gamma - dp) + gamma) + dy >*/
	    q = gamma - *dp + gamma + *dy;
/*<             r = p/q >*/
	    r__ = p / q;
/*<             stpc = stp + r*(sty - stp) >*/
	    stpc = *stp + r__ * (*sty - *stp);
/*<             stpf = stpc >*/
	    stpf = stpc;
/*<          else if (stp .gt. stx) then >*/
	} else if (*stp > *stx) {
/*<             stpf = stpmax >*/
	    stpf = *stpmax;
/*<          else >*/
	} else {
/*<             stpf = stpmin >*/
	    stpf = *stpmin;
/*<          endif >*/
	}
/*<       endif >*/
    }
/*     Update the interval which contains a minimizer. */
/*<       if (fp .gt. fx) then >*/
    if (*fp > *fx) {
/*<          sty = stp >*/
	*sty = *stp;
/*<          fy = fp >*/
	*fy = *fp;
/*<          dy = dp >*/
	*dy = *dp;
/*<       else >*/
    } else {
/*<          if (sgnd .lt. zero) then >*/
	if (sgnd < 0.) {
/*<             sty = stx >*/
	    *sty = *stx;
/*<             fy = fx >*/
	    *fy = *fx;
/*<             dy = dx >*/
	    *dy = *dx;
/*<          endif >*/
	}
/*<          stx = stp >*/
	*stx = *stp;
/*<          fx = fp >*/
	*fx = *fp;
/*<          dx = dp >*/
	*dx = *dp;
/*<       endif >*/
    }
/*     Compute the new step. */
/*<       stp = stpf >*/
    *stp = stpf;
/*<       end >*/
    return 0;
} /* dcstep_ */

/* ====================== The end of dcstep ============================== */
/*<       subroutine timer(ttime) >*/
/* Subroutine */ int timer_(doublereal *ttime)
{
#if 0
    real temp;
    extern doublereal etime_(real *);
    real tarray[2];

/*<       double precision ttime >*/
/*     ********* */

/*     Subroutine timer */

/*     This subroutine is used to determine user time. In a typical */
/*     application, the user time for a code segment requires calls */
/*     to subroutine timer to determine the initial and final time. */

/*     The subroutine statement is */

/*       subroutine timer(ttime) */

/*     where */

/*       ttime is an output variable which specifies the user time. */

/*     Argonne National Laboratory and University of Minnesota. */
/*     MINPACK-2 Project. */

/*     Modified October 1990 by Brett M. Averick. */

/*     ********** */
/*<       real temp >*/
/*<       real tarray(2) >*/
/*<       real etime >*/
/*     The first element of the array tarray specifies user time */
/*<       temp = etime(tarray)  >*/
    temp = etime_(tarray);
/*<       ttime = dble(tarray(1)) >*/
    *ttime = (doublereal) tarray[0];
/*<       return >*/
    return 0;
/*<       end >*/
#else
    *ttime = 0;
    return 0;
#endif
} /* timer_ */

/* ====================== The end of timer =============================== */
/*<       double precision function dpmeps() >*/
doublereal dpmeps_()
{
    /* Initialized data */

    static doublereal zero = 0.; /* constant */
    static doublereal one = 1.; /* constant */
    static doublereal two = 2.; /* constant */

    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    doublereal a, b;
    integer i__, it;
    doublereal beta;
    integer irnd;
    doublereal temp, temp1, betah;
    integer ibeta, negep;
    doublereal tempa;
    integer itemp;
    doublereal betain;

/*     ********** */

/*     Subroutine dpeps */

/*     This subroutine computes the machine precision parameter */
/*     dpmeps as the smallest floating point number such that */
/*     1 + dpmeps differs from 1. */

/*     This subroutine is based on the subroutine machar described in */

/*     W. J. Cody, */
/*     MACHAR: A subroutine to dynamically determine machine parameters, */
/*     ACM Transactions on Mathematical Software, 14, 1988, pages 303-311. */

/*     The subroutine statement is: */

/*       subroutine dpeps(dpmeps) */

/*     where */

/*       dpmeps is a double precision variable. */
/*         On entry dpmeps need not be specified. */
/*         On exit dpmeps is the machine precision. */

/*     MINPACK-2 Project. February 1991. */
/*     Argonne National Laboratory and University of Minnesota. */
/*     Brett M. Averick. */

/*     ******* */
/*<       integer i,ibeta,irnd,it,itemp,negep >*/
/*<    >*/
/*<       data zero,one,two /0.0d0,1.0d0,2.0d0/ >*/
/*     determine ibeta, beta ala malcolm. */
/*<       a = one >*/
    a = one;
/*<       b = one >*/
    b = one;
/*<    10 continue >*/
L10:
/*<          a = a + a >*/
    a += a;
/*<          temp = a + one >*/
    temp = a + one;
/*<          temp1 = temp - a >*/
    temp1 = temp - a;
/*<       if (temp1 - one .eq. zero) go to 10 >*/
    if (temp1 - one == zero) {
	goto L10;
    }
/*<    20 continue >*/
L20:
/*<          b = b + b >*/
    b += b;
/*<          temp = a + b >*/
    temp = a + b;
/*<          itemp = int(temp - a) >*/
    itemp = (integer) (temp - a);
/*<       if (itemp .eq. 0) go to 20 >*/
    if (itemp == 0) {
	goto L20;
    }
/*<       ibeta = itemp >*/
    ibeta = itemp;
/*<       beta = dble(ibeta) >*/
    beta = (doublereal) ibeta;
/*     determine it, irnd. */
/*<       it = 0 >*/
    it = 0;
/*<       b = one >*/
    b = one;
/*<    30 continue >*/
L30:
/*<          it = it + 1 >*/
    ++it;
/*<          b = b * beta >*/
    b *= beta;
/*<          temp = b + one >*/
    temp = b + one;
/*<          temp1 = temp - b >*/
    temp1 = temp - b;
/*<       if (temp1 - one .eq. zero) go to 30 >*/
    if (temp1 - one == zero) {
	goto L30;
    }
/*<       irnd = 0 >*/
    irnd = 0;
/*<       betah = beta/two >*/
    betah = beta / two;
/*<       temp = a + betah >*/
    temp = a + betah;
/*<       if (temp - a .ne. zero) irnd = 1 >*/
    if (temp - a != zero) {
	irnd = 1;
    }
/*<       tempa = a + beta >*/
    tempa = a + beta;
/*<       temp = tempa + betah >*/
    temp = tempa + betah;
/*<       if ((irnd .eq. 0) .and. (temp - tempa .ne. zero)) irnd = 2 >*/
    if (irnd == 0 && temp - tempa != zero) {
	irnd = 2;
    }
/*     determine dpmeps. */
/*<       negep = it + 3 >*/
    negep = it + 3;
/*<       betain = one/beta >*/
    betain = one / beta;
/*<       a = one >*/
    a = one;
/*<       do 40 i = 1, negep >*/
    i__1 = negep;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          a = a*betain >*/
	a *= betain;
/*<    40 continue >*/
/* L40: */
    }
/*<    50 continue >*/
L50:
/*<         temp = one + a >*/
    temp = one + a;
/*<         if (temp - one .ne. zero) go to 60 >*/
    if (temp - one != zero) {
	goto L60;
    }
/*<         a = a*beta >*/
    a *= beta;
/*<         go to  50 >*/
    goto L50;
/*<    60 continue >*/
L60:
/*<       dpmeps = a >*/
    ret_val = a;
/*<       if ((ibeta .eq. 2) .or. (irnd .eq. 0)) go to 70 >*/
    if (ibeta == 2 || irnd == 0) {
	goto L70;
    }
/*<       a = (a*(one + a))/two >*/
    a = a * (one + a) / two;
/*<       temp = one + a >*/
    temp = one + a;
/*<       if (temp - one .ne. zero) dpmeps = a >*/
    if (temp - one != zero) {
	ret_val = a;
    }
/*<    70 return >*/
L70:
    return ret_val;
/*<       end >*/
} /* dpmeps_ */

/* ====================== The end of dpmeps ============================== */
/*<       subroutine dtrsl(t,ldt,n,b,job,info) >*/
/* Subroutine */ int dtrsl_(doublereal *t, integer *ldt, integer *n, 
	doublereal *b, integer *job, integer *info)
{
    /* System generated locals */
    integer t_dim1, t_offset, i__1, i__2;

    /* Local variables */
    integer j, jj, case__;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    doublereal temp;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, integer *);

/*<       integer ldt,n,job,info >*/
/*<       double precision t(ldt,1),b(1) >*/


/*     dtrsl solves systems of the form */

/*                   t * x = b */
/*     or */
/*                   trans(t) * x = b */

/*     where t is a triangular matrix of order n. here trans(t) */
/*     denotes the transpose of the matrix t. */

/*     on entry */

/*         t         double precision(ldt,n) */
/*                   t contains the matrix of the system. the zero */
/*                   elements of the matrix are not referenced, and */
/*                   the corresponding elements of the array can be */
/*                   used to store other information. */

/*         ldt       integer */
/*                   ldt is the leading dimension of the array t. */

/*         n         integer */
/*                   n is the order of the system. */

/*         b         double precision(n). */
/*                   b contains the right hand side of the system. */

/*         job       integer */
/*                   job specifies what kind of system is to be solved. */
/*                   if job is */

/*                        00   solve t*x=b, t lower triangular, */
/*                        01   solve t*x=b, t upper triangular, */
/*                        10   solve trans(t)*x=b, t lower triangular, */
/*                        11   solve trans(t)*x=b, t upper triangular. */

/*     on return */

/*         b         b contains the solution, if info .eq. 0. */
/*                   otherwise b is unaltered. */

/*         info      integer */
/*                   info contains zero if the system is nonsingular. */
/*                   otherwise info contains the index of */
/*                   the first zero diagonal element of t. */

/*     linpack. this version dated 08/14/78 . */
/*     g. w. stewart, university of maryland, argonne national lab. */

/*     subroutines and functions */

/*     blas daxpy,ddot */
/*     fortran mod */

/*     internal variables */

/*<       double precision ddot,temp >*/
/*<       integer case,j,jj >*/

/*     begin block permitting ...exits to 150 */

/*        check for zero diagonal elements. */

/*<          do 10 info = 1, n >*/
    /* Parameter adjustments */
    t_dim1 = *ldt;
    t_offset = 1 + t_dim1;
    t -= t_offset;
    --b;

    /* Function Body */
    i__1 = *n;
    for (*info = 1; *info <= i__1; ++(*info)) {
/*     ......exit */
/*<             if (t(info,info) .eq. 0.0d0) go to 150 >*/
	if (t[*info + *info * t_dim1] == 0.) {
	    goto L150;
	}
/*<    10    continue >*/
/* L10: */
    }
/*<          info = 0 >*/
    *info = 0;

/*        determine the task and go to it. */

/*<          case = 1 >*/
    case__ = 1;
/*<          if (mod(job,10) .ne. 0) case = 2 >*/
    if (*job % 10 != 0) {
	case__ = 2;
    }
/*<          if (mod(job,100)/10 .ne. 0) case = case + 2 >*/
    if (*job % 100 / 10 != 0) {
	case__ += 2;
    }
/*<          go to (20,50,80,110), case >*/
    switch (case__) {
	case 1:  goto L20;
	case 2:  goto L50;
	case 3:  goto L80;
	case 4:  goto L110;
    }

/*        solve t*x=b for t lower triangular */

/*<    20    continue >*/
L20:
/*<             b(1) = b(1)/t(1,1) >*/
    b[1] /= t[t_dim1 + 1];
/*<             if (n .lt. 2) go to 40 >*/
    if (*n < 2) {
	goto L40;
    }
/*<             do 30 j = 2, n >*/
    i__1 = *n;
    for (j = 2; j <= i__1; ++j) {
/*<                temp = -b(j-1) >*/
	temp = -b[j - 1];
/*<                call daxpy(n-j+1,temp,t(j,j-1),1,b(j),1) >*/
	i__2 = *n - j + 1;
	daxpy_(&i__2, &temp, &t[j + (j - 1) * t_dim1], &c__1, &b[j], &c__1);
/*<                b(j) = b(j)/t(j,j) >*/
	b[j] /= t[j + j * t_dim1];
/*<    30       continue >*/
/* L30: */
    }
/*<    40       continue >*/
L40:
/*<          go to 140 >*/
    goto L140;

/*        solve t*x=b for t upper triangular. */

/*<    50    continue >*/
L50:
/*<             b(n) = b(n)/t(n,n) >*/
    b[*n] /= t[*n + *n * t_dim1];
/*<             if (n .lt. 2) go to 70 >*/
    if (*n < 2) {
	goto L70;
    }
/*<             do 60 jj = 2, n >*/
    i__1 = *n;
    for (jj = 2; jj <= i__1; ++jj) {
/*<                j = n - jj + 1 >*/
	j = *n - jj + 1;
/*<                temp = -b(j+1) >*/
	temp = -b[j + 1];
/*<                call daxpy(j,temp,t(1,j+1),1,b(1),1) >*/
	daxpy_(&j, &temp, &t[(j + 1) * t_dim1 + 1], &c__1, &b[1], &c__1);
/*<                b(j) = b(j)/t(j,j) >*/
	b[j] /= t[j + j * t_dim1];
/*<    60       continue >*/
/* L60: */
    }
/*<    70       continue >*/
L70:
/*<          go to 140 >*/
    goto L140;

/*        solve trans(t)*x=b for t lower triangular. */

/*<    80    continue >*/
L80:
/*<             b(n) = b(n)/t(n,n) >*/
    b[*n] /= t[*n + *n * t_dim1];
/*<             if (n .lt. 2) go to 100 >*/
    if (*n < 2) {
	goto L100;
    }
/*<             do 90 jj = 2, n >*/
    i__1 = *n;
    for (jj = 2; jj <= i__1; ++jj) {
/*<                j = n - jj + 1 >*/
	j = *n - jj + 1;
/*<                b(j) = b(j) - ddot(jj-1,t(j+1,j),1,b(j+1),1) >*/
	i__2 = jj - 1;
	b[j] -= ddot_(&i__2, &t[j + 1 + j * t_dim1], &c__1, &b[j + 1], &c__1);
/*<                b(j) = b(j)/t(j,j) >*/
	b[j] /= t[j + j * t_dim1];
/*<    90       continue >*/
/* L90: */
    }
/*<   100       continue >*/
L100:
/*<          go to 140 >*/
    goto L140;

/*        solve trans(t)*x=b for t upper triangular. */

/*<   110    continue >*/
L110:
/*<             b(1) = b(1)/t(1,1) >*/
    b[1] /= t[t_dim1 + 1];
/*<             if (n .lt. 2) go to 130 >*/
    if (*n < 2) {
	goto L130;
    }
/*<             do 120 j = 2, n >*/
    i__1 = *n;
    for (j = 2; j <= i__1; ++j) {
/*<                b(j) = b(j) - ddot(j-1,t(1,j),1,b(1),1) >*/
	i__2 = j - 1;
	b[j] -= ddot_(&i__2, &t[j * t_dim1 + 1], &c__1, &b[1], &c__1);
/*<                b(j) = b(j)/t(j,j) >*/
	b[j] /= t[j + j * t_dim1];
/*<   120       continue >*/
/* L120: */
    }
/*<   130       continue >*/
L130:
/*<   140    continue >*/
L140:
/*<   150 continue >*/
L150:
/*<       return >*/
    return 0;
/*<       end >*/
} /* dtrsl_ */

#ifdef __cplusplus
	}
#endif
