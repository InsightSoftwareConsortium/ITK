/* Cherry-picked from lapack/util/ into blas/: lsame_ is the only LAPACK routine
   the retained netlib code (BLAS) requires; the rest of LAPACK was removed. */
extern v3p_netlib_logical v3p_netlib_lsame_(
  const char *ca,
  const char *cb,
  v3p_netlib_ftnlen ca_len,
  v3p_netlib_ftnlen cb_len
  );
