extern int v3p_netlib_zgees_(
  char *jobvs,
  char *sort,
  v3p_netlib_logical (*select)(v3p_netlib_doublecomplex*),
  v3p_netlib_integer *n,
  v3p_netlib_doublecomplex *a,
  v3p_netlib_integer *lda,
  v3p_netlib_integer *sdim,
  v3p_netlib_doublecomplex *w,
  v3p_netlib_doublecomplex *vs,
  v3p_netlib_integer *ldvs,
  v3p_netlib_doublecomplex *work,
  v3p_netlib_integer *lwork,
  v3p_netlib_doublereal *rwork,
  v3p_netlib_logical *bwork,
  v3p_netlib_integer *info,
  v3p_netlib_ftnlen jobvs_len,
  v3p_netlib_ftnlen sort_len
  );
