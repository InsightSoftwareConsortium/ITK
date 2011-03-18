/*: Factors a symmetric positive definite matrix and estimates the
    condition of the matrix */
extern int v3p_netlib_dpoco_(
  v3p_netlib_doublereal *a,
  v3p_netlib_integer *lda,
  v3p_netlib_integer *n,
  v3p_netlib_doublereal *rcond,
  v3p_netlib_doublereal *z__,
  v3p_netlib_integer *info
  );
