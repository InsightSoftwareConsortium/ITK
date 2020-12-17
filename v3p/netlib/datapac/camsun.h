/*: Computes the float cumulative distribution function value for the chi-squared distribution */
void v3p_netlib_chscdf_(
  v3p_netlib_real v3p_netlib_const *x,      /*!< (IN) value where the cumulative distribution must be evaluated */
  v3p_netlib_integer v3p_netlib_const *nu,  /*!< (IN) # degrees of freedom */
  v3p_netlib_real *cdf                      /*!< (OUT) the function value */
  );
/*: Computes the double cumulative distribution function value for the chi-squared distribution */
void v3p_netlib_dchscdf_(
  v3p_netlib_doublereal v3p_netlib_const *x, /*!< (IN) value where the cumulative distribution must be evaluated */
  v3p_netlib_integer v3p_netlib_const *nu,   /*!< (IN) # degrees of freedom */
  v3p_netlib_doublereal *cdf                 /*!< (OUT) the function value */
  );
