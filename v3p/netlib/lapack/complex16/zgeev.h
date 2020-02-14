/*: Computes eigenvalues and eigenvectors of nxn complex general matrix */
extern int v3p_netlib_zgeev_(
  char v3p_netlib_const *jobvl,
  char v3p_netlib_const *jobvr,
  v3p_netlib_integer v3p_netlib_const *n,
  v3p_netlib_doublecomplex *a,
  v3p_netlib_integer v3p_netlib_const *lda,
  v3p_netlib_doublecomplex *w,
  v3p_netlib_doublecomplex *vl,
  v3p_netlib_integer v3p_netlib_const *ldvl,
  v3p_netlib_doublecomplex *vr,
  v3p_netlib_integer v3p_netlib_const *ldvr,
  v3p_netlib_doublecomplex *work,
  v3p_netlib_integer *lwork,
  v3p_netlib_doublereal *rwork,
  v3p_netlib_integer *info,
  v3p_netlib_ftnlen jobvl_len,
  v3p_netlib_ftnlen jobvr_len
  );
