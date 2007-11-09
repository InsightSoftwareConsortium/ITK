extern int v3p_netlib_adaptquad_(
  v3p_netlib_doublereal (*f)(v3p_netlib_doublereal*),
  v3p_netlib_doublereal *a,
  v3p_netlib_doublereal *b,
  v3p_netlib_doublereal *tol,
  v3p_netlib_doublereal *srmat,
  v3p_netlib_doublereal *integral,
  v3p_netlib_doublereal *errbdd,
  v3p_netlib_integer *m,
  v3p_netlib_integer *state
  );
extern int v3p_netlib_refine_(
  v3p_netlib_doublereal (*f)(v3p_netlib_doublereal*),
  v3p_netlib_integer *p,
  v3p_netlib_doublereal *srmat,
  v3p_netlib_integer *m,
  v3p_netlib_integer *state
  );
extern int v3p_netlib_srule_(
  v3p_netlib_doublereal (*f)(v3p_netlib_doublereal*),
  v3p_netlib_doublereal *a,
  v3p_netlib_doublereal *b,
  v3p_netlib_doublereal *tol0,
  v3p_netlib_doublereal *srvec
  );
