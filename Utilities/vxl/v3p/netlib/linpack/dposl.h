/*: Solves the symmetric positive definite system a * x = b dpoco or
    dpofa output. */
extern int v3p_netlib_dposl_(
  v3p_netlib_doublereal v3p_netlib_const *a,
  v3p_netlib_integer v3p_netlib_const *lda,
  v3p_netlib_integer v3p_netlib_const *n,
  v3p_netlib_doublereal *b
  );
