// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(test_fft1d );
  REGISTER_TEST(test_fft2d );
  REGISTER_TEST(test_math );
  REGISTER_TEST(test_vector );
  REGISTER_TEST(test_matrix );
  REGISTER_TEST(test_rnpoly_roots );
  REGISTER_TEST(test_sparse_matrix );
  REGISTER_TEST(test_sample );
  REGISTER_TEST(test_functions );
  REGISTER_TEST(test_numeric_limits );
  REGISTER_TEST(test_rpoly_roots );
  REGISTER_TEST(test_cpoly_roots );
  REGISTER_TEST(test_resize );
  REGISTER_TEST(test_complex );
  REGISTER_TEST(test_determinant );
  REGISTER_TEST(test_qr );
  REGISTER_TEST(test_qsvd );
  REGISTER_TEST(test_svd );
  REGISTER_TEST(test_real_eigensystem );
  REGISTER_TEST(test_complex_eigensystem );
  REGISTER_TEST(test_symmetric_eigensystem );
  REGISTER_TEST(test_generalized_eigensystem );
  REGISTER_TEST(test_matrix_exp );
}
