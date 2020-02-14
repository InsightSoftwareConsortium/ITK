/*: Computes determinant and inverse of a certain symmetric positive
    definite matrix using dpoco_, dposa_ or dqrdc_ output. */
extern int v3p_netlib_dpodi_(
  v3p_netlib_doublereal *a,
  v3p_netlib_integer v3p_netlib_const *lda,
  v3p_netlib_integer v3p_netlib_const *n,
  v3p_netlib_doublereal *det,
  v3p_netlib_integer v3p_netlib_const *job
  );
