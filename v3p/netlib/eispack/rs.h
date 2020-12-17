/*: Computes eigenvalues and eigenvectors of a real symmetric matrix */
extern int v3p_netlib_rs_(
  v3p_netlib_integer v3p_netlib_const *nm, /*!< (IN) leading dimension of matrices */
  v3p_netlib_integer v3p_netlib_const *n, /*!< (IN) order of the square matrix a */
  v3p_netlib_doublereal *a, /*!< (IN) real symmetric matrix */
  v3p_netlib_doublereal *w, /*!< (OUT) eigenvalues in ascending order */
  v3p_netlib_integer v3p_netlib_const *matz, /*!< (IN) set nonzero if eigenvectors wanted */
  v3p_netlib_doublereal *z__, /*!< (OUT) eigenvectors */
  v3p_netlib_doublereal *fv1, v3p_netlib_doublereal *fv2, /*!< scratch */
  v3p_netlib_integer v3p_netlib_const *ierr /*!< (OUT) normal completion code is 0 */
  );
