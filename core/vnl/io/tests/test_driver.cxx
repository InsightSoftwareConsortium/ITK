#include <testlib/testlib_register.h>

DECLARE( test_bignum_io );
DECLARE( test_diag_matrix_io );
DECLARE( test_matrix_fixed_io );
DECLARE( test_matrix_io );
DECLARE( test_nonlinear_minimizer_io );
DECLARE( test_rational_io );
DECLARE( test_real_npolynomial_io );
DECLARE( test_real_polynomial_io );
DECLARE( test_sparse_matrix_io );
DECLARE( test_sym_matrix_io );
DECLARE( test_vector_fixed_io );
DECLARE( test_vector_io );
DECLARE( golden_test_vnl_io );

void
register_tests()
{
  REGISTER( test_bignum_io );
  REGISTER( test_diag_matrix_io );
  REGISTER( test_matrix_fixed_io );
  REGISTER( test_matrix_io );
  REGISTER( test_nonlinear_minimizer_io );
  REGISTER( test_rational_io );
  REGISTER( test_real_npolynomial_io );
  REGISTER( test_real_polynomial_io );
  REGISTER( test_sparse_matrix_io );
  REGISTER( test_sym_matrix_io );
  REGISTER( test_vector_fixed_io );
  REGISTER( test_vector_io );
  REGISTER( golden_test_vnl_io );
}

DEFINE_MAIN;
