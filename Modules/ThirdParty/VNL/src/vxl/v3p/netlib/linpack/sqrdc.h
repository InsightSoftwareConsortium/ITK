/*: Computes QR factorisation of an n x p float matrix */
extern int v3p_netlib_sqrdc_(
  v3p_netlib_real *x, v3p_netlib_integer v3p_netlib_const *ldx, /*!< (IN/OUT) matrix, n rows, p columns, stored row-wise */
  v3p_netlib_integer v3p_netlib_const *n, v3p_netlib_integer v3p_netlib_const *p,
  v3p_netlib_real *qraux, /*!< (OUT) further info necessary to recover R part from x */
  v3p_netlib_integer *jpvt,  /*!< (IN/OUT) length p; selection of pivot columns: */
                             /*   ==0 ==> any; >0 ==> initial column; <0 ==> final */
  v3p_netlib_real *work,  /*!< (IN/OUT) scratch work area of length p */
  v3p_netlib_integer v3p_netlib_const *job /*!< (IN) if == 0, no pivoting is done */
  );
