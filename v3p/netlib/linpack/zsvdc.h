/*: Computes singular values and vectors of an mxn matrix (double_complex version) */
extern int v3p_netlib_zsvdc_(
  v3p_netlib_doublecomplex *x, v3p_netlib_integer v3p_netlib_const *ldx, /*!< (IN) matrix, m rows, n columns, stored row-wise */
  v3p_netlib_integer v3p_netlib_const *m, v3p_netlib_integer v3p_netlib_const *n,
  v3p_netlib_doublecomplex *singular_values, /*!< (OUT) in descending order of magnitude */
  v3p_netlib_doublecomplex *errors, /*!< (OUT) superdiagonal of u^T*x*v (normally 0) */
  v3p_netlib_doublecomplex *u, v3p_netlib_integer v3p_netlib_const *ldu, /*!< (OUT) left singular vectors */
  v3p_netlib_doublecomplex *v, v3p_netlib_integer v3p_netlib_const *ldv, /*!< (OUT) right singular vectors */
  v3p_netlib_doublecomplex *work, /*!< (IN/OUT) scratch work area of length m */
  v3p_netlib_integer v3p_netlib_const *job, /*!< (IN) 2-digit number. First digit refers to u; 0 = do not compute, 1 = all m; 2 = only min(m,n) */
  v3p_netlib_integer *info /*!< (OUT) singular values [info] and up are correct */
  );
