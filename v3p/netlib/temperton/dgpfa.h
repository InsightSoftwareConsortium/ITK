/*: Self-sorting in-place generalized prime factor (complex) double fft */
extern int v3p_netlib_dgpfa_(
  v3p_netlib_doublereal *a, /*!< (IN/OUT) Real part of input/output vectors */
  v3p_netlib_doublereal *b, /*!< (IN/OUT) Imaginary part of input/output vectors */
  v3p_netlib_doublereal v3p_netlib_const *trigs, /*!< (IN) output of dsetgfpa_ (twiddle factors) */
  v3p_netlib_integer v3p_netlib_const *inc, /*!< (IN) increment within each data vector (normally 1) */
  v3p_netlib_integer v3p_netlib_const *jump, /*!< (IN) increment between data vectors */
  v3p_netlib_integer v3p_netlib_const *n, /*!< (IN) length of the transforms; should only have 2,3,5 as prime factors */
  v3p_netlib_integer v3p_netlib_const *lot, /*!< (IN) number of transforms */
  v3p_netlib_integer v3p_netlib_const *isign, /*!< (IN) forward transform: +1; backward: -1 */
  v3p_netlib_integer v3p_netlib_const *npqr /*!< (IN) 3-array with the number of factors of 2,3,5 */
  );
