extern void v3p_netlib_dlamch_init();
extern v3p_netlib_doublereal v3p_netlib_dlamch_(
  char *cmach,
  v3p_netlib_ftnlen cmach_len
  );
extern int v3p_netlib_dlamc1_(
  v3p_netlib_integer *beta,
  v3p_netlib_integer *t,
  v3p_netlib_logical *rnd,
  v3p_netlib_logical *ieee1
  );
extern int v3p_netlib_dlamc2_(
  v3p_netlib_integer *beta,
  v3p_netlib_integer *t,
  v3p_netlib_logical *rnd,
  v3p_netlib_doublereal *eps,
  v3p_netlib_integer *emin,
  v3p_netlib_doublereal *rmin,
  v3p_netlib_integer *emax,
  v3p_netlib_doublereal *rmax
  );
extern v3p_netlib_doublereal v3p_netlib_dlamc3_(
  v3p_netlib_doublereal *a,
  v3p_netlib_doublereal *b
  );
extern int v3p_netlib_dlamc4_(
  v3p_netlib_integer *emin,
  v3p_netlib_doublereal *start,
  v3p_netlib_integer *base
  );
extern int v3p_netlib_dlamc5_(
  v3p_netlib_integer *beta,
  v3p_netlib_integer *p,
  v3p_netlib_integer *emin,
  v3p_netlib_logical *ieee,
  v3p_netlib_integer *emax,
  v3p_netlib_doublereal *rmax
  );
