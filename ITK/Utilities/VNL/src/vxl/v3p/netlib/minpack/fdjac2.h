extern int v3p_netlib_fdjac2_(
  void (*fcn)(v3p_netlib_integer*,
              v3p_netlib_integer*,
              v3p_netlib_doublereal*,
              v3p_netlib_doublereal*,
              v3p_netlib_integer*,
              void*),
  v3p_netlib_integer *m,
  v3p_netlib_integer *n,
  v3p_netlib_doublereal *x,
  v3p_netlib_doublereal *fvec,
  v3p_netlib_doublereal *fjac,
  v3p_netlib_integer *ldfjac,
  v3p_netlib_integer *iflag,
  v3p_netlib_doublereal *epsfcn,
  v3p_netlib_doublereal *wa,
  void* userdata
  );
