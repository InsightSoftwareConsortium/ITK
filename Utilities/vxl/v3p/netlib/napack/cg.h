/*: Minimizes a function using the conjugate gradient method */
extern int v3p_netlib_cg_(
  v3p_netlib_doublereal *x, /*!< (IN/OUT) minimizer, length n; input = starting guess */
  v3p_netlib_doublereal *e, /*!< (OUT) max-norm of gradient */
  v3p_netlib_integer *it,   /*!< (OUT) number of iterations performed */
  v3p_netlib_doublereal *step, /*!< (IN/OUT) step size along search direction */
  v3p_netlib_doublereal v3p_netlib_const *tolerance_on_e,
  v3p_netlib_integer v3p_netlib_const *max_iterations,
  v3p_netlib_integer *n, /*!< (IN) number of unknowns */
  v3p_netlib_integer *m, /*!< (IN) # iterations before calc new seach direction */
  double (*value)(double*,void*),
  void (*grad)(double*,double*,void*),
  void (*both)(double*,double*,double*,void*),
  void (*pre)(double*,double*,void*),
  v3p_netlib_doublereal *h__,
  void* userdata,
  v3p_netlib_integer* error_code /*!< (OUT) error code
                                   0 = NO ERROR,
                                   1 = UNABLE TO OBTAIN DESCENT DIRECTION,
                                   2 = THE FUNCTION DECREASES WITH NO MINIMUM,
                                   3 = PRECONDITIONER NOT POSITIVE DEFINITE */
  );
extern v3p_netlib_doublereal v3p_netlib_fv_(
  v3p_netlib_doublereal *a,
  v3p_netlib_doublereal *x,
  v3p_netlib_doublereal *h__,
  v3p_netlib_integer *n,
  double (*value)(double*,void*),
  void* userdata
  );
extern v3p_netlib_doublereal v3p_netlib_fd_(
  v3p_netlib_doublereal *a,
  v3p_netlib_doublereal *x,
  v3p_netlib_doublereal *h__,
  v3p_netlib_integer *n,
  void (*grad)(double*,double*,void*),
  void* userdata
  );
extern int v3p_netlib_fvd_(
  v3p_netlib_doublereal *v,
  v3p_netlib_doublereal *d__,
  v3p_netlib_doublereal *a,
  v3p_netlib_doublereal *x,
  v3p_netlib_doublereal *h__,
  v3p_netlib_integer *n,
  void (*both)(double*,double*,double*,void*),
  void* userdata
  );
extern int v3p_netlib_cub_(
  v3p_netlib_doublereal *x,
  v3p_netlib_doublereal *a,
  v3p_netlib_doublereal *b,
  v3p_netlib_doublereal *c__,
  v3p_netlib_doublereal *d__,
  v3p_netlib_doublereal *e,
  v3p_netlib_doublereal *f
  );
extern int v3p_netlib_ins_(
  v3p_netlib_doublereal *s,
  v3p_netlib_doublereal *f,
  v3p_netlib_doublereal *a,
  v3p_netlib_doublereal *b,
  v3p_netlib_doublereal *c__,
  v3p_netlib_doublereal *fa,
  v3p_netlib_doublereal *fb,
  v3p_netlib_doublereal *fc,
  v3p_netlib_integer *j,
  v3p_netlib_doublereal *y,
  v3p_netlib_doublereal *z__
  );
