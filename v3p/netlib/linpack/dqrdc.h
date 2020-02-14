/*: Computes QR factorisation of an n x p double matrix */
extern int v3p_netlib_dqrdc_(
  v3p_netlib_doublereal *x, v3p_netlib_integer v3p_netlib_const *ldx, /*!< (IN/OUT) matrix, n rows, p columns, stored row-wise */
  v3p_netlib_integer v3p_netlib_const *n, v3p_netlib_integer v3p_netlib_const *p,
  v3p_netlib_doublereal *qraux, /*!< (OUT) further info necessary to recover R part from x */
  v3p_netlib_integer *jpvt,  /*!< (IN/OUT) length p; selection of pivot columns: */
                             /*   ==0 ==> any; >0 ==> initial column; <0 ==> final */
  v3p_netlib_doublereal *work,  /*!< (IN/OUT) scratch work area of length p */
  v3p_netlib_integer v3p_netlib_const *job /*!< (IN) if == 0, no pivoting is done */
  );
