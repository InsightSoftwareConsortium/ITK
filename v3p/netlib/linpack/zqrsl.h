/*: Computes coord transf etc from QR factorisation of double complex matrix */
extern int v3p_netlib_zqrsl_(
  v3p_netlib_doublecomplex v3p_netlib_const *x, v3p_netlib_integer v3p_netlib_const *ldx, /*!< (IN) output of zqrdc_, n x k matrix */
  v3p_netlib_integer v3p_netlib_const *n, v3p_netlib_integer v3p_netlib_const *k, /*!< (IN) k <= min(n,p) with n,p from zqrdc_ */
  v3p_netlib_doublecomplex v3p_netlib_const *qraux, /*!< (IN) qraux output of zqrdc_ */
  v3p_netlib_doublecomplex v3p_netlib_const *y, /*!< (IN) n-vector to operate on */
  v3p_netlib_doublecomplex *qy,  /*!< (OUT) q*y */
  v3p_netlib_doublecomplex *qty, /*!< (OUT) q^T*y (conjugate transpose if complex) */
  v3p_netlib_doublecomplex *b,   /*!< (OUT) solution b of min norm_2(y - x*b) */
  v3p_netlib_doublecomplex *rsd, /*!< (OUT) least squares residual y - x*b = proj of y on orth complement of columns(x) */
  v3p_netlib_doublecomplex *xb,  /*!< (OUT) least squares approx of x*b = proj of y on columns(x) */
  v3p_netlib_integer v3p_netlib_const *job, /*!< (IN) decimal acbde: a:compute qy; c:qty; b:qty+b; d:qty+rsd; e:qty+xb */
  v3p_netlib_integer *info /*!< non-zero if r is singular and b is set. */
  );
