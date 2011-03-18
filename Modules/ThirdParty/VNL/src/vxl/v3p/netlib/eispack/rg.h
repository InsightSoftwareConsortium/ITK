/*: Finds eigenvalues and eigenvectors of a general matrix */
extern int v3p_netlib_rg_(
  v3p_netlib_integer v3p_netlib_const *nm, /*!< (IN) leading dimension of matrices */
  v3p_netlib_integer v3p_netlib_const *n, /*!< (IN) order of the square matrix a */
  v3p_netlib_doublereal *a, /*!< (IN) real general matrix */
  v3p_netlib_doublereal *wr, /*!< (OUT) real part of eigenvalues */
  v3p_netlib_doublereal *wi, /*!< (OUT) imaginary part of eigenvalues */
  v3p_netlib_integer v3p_netlib_const *matz, /*!< (IN) set nonzero if eigenvectors wanted */
  v3p_netlib_doublereal *z__, /*!< (OUT) eigenvectors */
  v3p_netlib_integer *iv1, v3p_netlib_doublereal *fv1, /*!< scratch */
  v3p_netlib_integer *ierr /*!< (OUT) normal completion code is 0 */
  );
