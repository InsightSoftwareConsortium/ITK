typedef struct v3p_netlib_lbfgs_global_s v3p_netlib_lbfgs_global_t;

/*: Solves the unconstrained minimization problem min F(x1..xN) */
extern int v3p_netlib_lbfgs_(
  v3p_netlib_integer *n,
  v3p_netlib_integer *m,
  v3p_netlib_doublereal *x,
  v3p_netlib_doublereal *f,
  v3p_netlib_doublereal *g,
  v3p_netlib_logical *diagco,
  v3p_netlib_doublereal *diag,
  v3p_netlib_integer *iprint,
  v3p_netlib_doublereal *eps,
  v3p_netlib_doublereal *xtol,
  v3p_netlib_doublereal *w,
  v3p_netlib_integer *iflag,
  v3p_netlib_lbfgs_global_t* v3p_netlib_lbfgs_global_arg
  );
extern void v3p_netlib_lbfgs_init(
  v3p_netlib_lbfgs_global_t* v3p_netlib_lbfgs_global_arg
  );

/*
C    GTOL is a DOUBLE PRECISION variable with default value 0.9, which
C        controls the accuracy of the line search routine MCSRCH. If the
C        function and gradient evaluations are inexpensive with respect
C        to the cost of the iteration (which is sometimes the case when
C        solving very large problems) it may be advantageous to set GTOL
C        to a small value. A typical small value is 0.1.  Restriction:
C        GTOL should be greater than 1.D-04.
C
C    STPMIN and STPMAX are non-negative DOUBLE PRECISION variables which
C        specify lower and upper bounds for the step in the line search.
C        Their default values are 1.D-20 and 1.D+20, respectively. These
C        values need not be modified unless the exponents are too large
C        for the machine being used, or unless the problem is extremely
C        badly scaled (in which case the exponents should be increased).
*/
struct v3p_netlib_lbfgs_global_s
{
  v3p_netlib_integer mp, lp;
  v3p_netlib_doublereal gtol, stpmin, stpmax;
  v3p_netlib_doublereal stpinit; /* line search default step length, added by awf */

  /* Private data that needs to be persistent across calls during a
     minimization.  It need not be initialized.  */
  v3p_netlib_integer private_info;
  v3p_netlib_integer private_infoc;
  v3p_netlib_integer private_nfev;
  v3p_netlib_integer private_maxfev;
  v3p_netlib_doublereal private_stp;
  v3p_netlib_doublereal private_stp1;
  v3p_netlib_doublereal private_beta;
  v3p_netlib_doublereal private_ftol;
  v3p_netlib_doublereal private_gnorm;
  v3p_netlib_doublereal private_xnorm;
  v3p_netlib_integer private_inmc;
  v3p_netlib_integer private_iscn;
  v3p_netlib_integer private_iycn;
  v3p_netlib_integer private_iter;
  v3p_netlib_integer private_nfun;
  v3p_netlib_integer private_ispt;
  v3p_netlib_integer private_iypt;
  v3p_netlib_integer private_bound;
  v3p_netlib_integer private_point;
  v3p_netlib_logical private_finish;
  v3p_netlib_doublereal private_dg;
  v3p_netlib_doublereal private_fm;
  v3p_netlib_doublereal private_fx;
  v3p_netlib_doublereal private_fy;
  v3p_netlib_doublereal private_dgm;
  v3p_netlib_doublereal private_dgx;
  v3p_netlib_doublereal private_dgy;
  v3p_netlib_doublereal private_fxm;
  v3p_netlib_doublereal private_fym;
  v3p_netlib_doublereal private_stx;
  v3p_netlib_doublereal private_sty;
  v3p_netlib_doublereal private_dgxm;
  v3p_netlib_doublereal private_dgym;
  v3p_netlib_doublereal private_finit;
  v3p_netlib_doublereal private_width;
  v3p_netlib_doublereal private_stmin;
  v3p_netlib_doublereal private_stmax;
  v3p_netlib_logical private_stage1;
  v3p_netlib_doublereal private_width1;
  v3p_netlib_doublereal private_ftest1;
  v3p_netlib_logical private_brackt;
  v3p_netlib_doublereal private_dginit;
  v3p_netlib_doublereal private_dgtest;
};
