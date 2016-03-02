/*: Computes coord transf etc from QR factorisation of double matrix */
extern int v3p_netlib_dqrsl_(
  v3p_netlib_doublereal v3p_netlib_const *x, v3p_netlib_integer v3p_netlib_const *ldx, /*!< (IN) output of dqrdc_, n x k matrix */
  v3p_netlib_integer v3p_netlib_const *n, v3p_netlib_integer v3p_netlib_const *k, /*!< (IN) k <= min(n,p) with n,p from dqrdc_ */
  v3p_netlib_doublereal v3p_netlib_const *qraux, /*!< (IN) qraux output of dqrdc_ */
  v3p_netlib_doublereal v3p_netlib_const *y, /*!< (IN) n-vector to operate on */
  v3p_netlib_doublereal *qy,  /*!< (OUT) q*y */
  v3p_netlib_doublereal *qty, /*!< (OUT) q^T*y (conjugate transpose if complex) */
  v3p_netlib_doublereal *b,   /*!< (OUT) solution b of min norm_2(y - x*b) */
  v3p_netlib_doublereal *rsd, /*!< (OUT) least squares residual y - x*b = proj of y on orth complement of columns(x) */
  v3p_netlib_doublereal *xb,  /*!< (OUT) least squares approx of x*b = proj of y on columns(x) */
  v3p_netlib_integer v3p_netlib_const *job, /*!< (IN) decimal acbde: a:compute qy; c:qty; b:qty+b; d:qty+rsd; e:qty+xb */
  v3p_netlib_integer *info /*!< non-zero if r is singular and b is set. */
  );
