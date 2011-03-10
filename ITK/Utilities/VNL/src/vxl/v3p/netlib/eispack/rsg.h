/*: Computes eigenvalues and eigenvectors of a real symmetric generalized eigenproblem  ax = lambda bx.  */
extern int v3p_netlib_rsg_(
  v3p_netlib_integer v3p_netlib_const *nm, /*!< (IN) leading dimension of matrices */
  v3p_netlib_integer v3p_netlib_const *n, /*!< (IN) order of the square matrices a and b */
  v3p_netlib_doublereal *a, /*!< (IN) real symmetric matrix */
  v3p_netlib_doublereal *b, /*!< (IN) positive definite real symm matrix */
  v3p_netlib_doublereal *w, /*!< (OUT) eigenvalues in ascending order */
  v3p_netlib_integer v3p_netlib_const *matz, /*!< (IN) set nonzero if eigenvectors wanted */
  v3p_netlib_doublereal *z__, /*!< (OUT) eigenvectors */
  v3p_netlib_doublereal *fv1, v3p_netlib_doublereal *fv2, /*!< scratch */
  v3p_netlib_integer *ierr /*!< (OUT) normal completion code is 0 */
  );
