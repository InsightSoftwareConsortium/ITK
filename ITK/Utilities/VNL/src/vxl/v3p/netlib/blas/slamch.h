extern void v3p_netlib_slamch_init();
extern v3p_netlib_E_f v3p_netlib_slamch_(
  char *cmach,
  v3p_netlib_ftnlen cmach_len
  );
extern int v3p_netlib_slamc1_(
  v3p_netlib_integer *beta,
  v3p_netlib_integer *t,
  v3p_netlib_logical *rnd,
  v3p_netlib_logical *ieee1
  );
extern int v3p_netlib_slamc2_(
  v3p_netlib_integer *beta,
  v3p_netlib_integer *t,
  v3p_netlib_logical *rnd,
  v3p_netlib_real *eps,
  v3p_netlib_integer *emin,
  v3p_netlib_real *rmin,
  v3p_netlib_integer *emax,
  v3p_netlib_real *rmax
  );
extern v3p_netlib_E_f v3p_netlib_slamc3_(
  v3p_netlib_real *a,
  v3p_netlib_real *b
  );
extern int v3p_netlib_slamc4_(
  v3p_netlib_integer *emin,
  v3p_netlib_real *start,
  v3p_netlib_integer *base
  );
extern int v3p_netlib_slamc5_(
  v3p_netlib_integer *beta,
  v3p_netlib_integer *p,
  v3p_netlib_integer *emin,
  v3p_netlib_logical *ieee,
  v3p_netlib_integer *emax,
  v3p_netlib_real *rmax
  );
