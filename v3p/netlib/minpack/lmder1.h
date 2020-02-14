/*: Minimizes the sum of the squares of m nonlin functions in n variables */
extern int v3p_netlib_lmder1_(
  void (*fcn)(v3p_netlib_integer*,
              v3p_netlib_integer*,
              v3p_netlib_doublereal*,
              v3p_netlib_doublereal*,
              v3p_netlib_doublereal*,
              v3p_netlib_integer*,
              v3p_netlib_integer*,
              void*),
  v3p_netlib_integer *m,
  v3p_netlib_integer *n,
  v3p_netlib_doublereal *x,
  v3p_netlib_doublereal *fvec,
  v3p_netlib_doublereal *fjac,
  v3p_netlib_integer *ldfjac,
  v3p_netlib_doublereal *tol,
  v3p_netlib_integer *info,
  v3p_netlib_integer *ipvt,
  v3p_netlib_doublereal *wa,
  v3p_netlib_integer *lwa,
  void* userdata
  );
