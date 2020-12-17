/*: Computes coord transf etc from QR factorisation of float matrix */
extern int v3p_netlib_sqrsl_(
  v3p_netlib_real v3p_netlib_const *x, v3p_netlib_integer v3p_netlib_const *ldx, /*!< (IN) output of sqrdc_, n x k matrix */
  v3p_netlib_integer v3p_netlib_const *n, v3p_netlib_integer v3p_netlib_const *k, /*!< (IN) k <= min(n,p) with n,p from sqrdc_ */
  v3p_netlib_real v3p_netlib_const *qraux, /*!< (IN) qraux output of sqrdc_ */
  v3p_netlib_real v3p_netlib_const *y, /*!< (IN) n-vector to operate on */
  v3p_netlib_real *qy,  /*!< (OUT) q*y */
  v3p_netlib_real *qty, /*!< (OUT) q^T*y (conjugate transpose if complex) */
  v3p_netlib_real *b,   /*!< (OUT) solution b of min norm_2(y - x*b) */
  v3p_netlib_real *rsd, /*!< (OUT) least squares residual y - x*b = proj of y on orth complement of columns(x) */
  v3p_netlib_real *xb,  /*!< (OUT) least squares approx of x*b = proj of y on columns(x) */
  v3p_netlib_integer v3p_netlib_const *job, /*!< (IN) decimal acbde: a:compute qy; c:qty; b:qty+b; d:qty+rsd; e:qty+xb */
  v3p_netlib_integer *info /*!< non-zero if r is singular and b is set. */
  );
