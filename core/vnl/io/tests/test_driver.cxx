#include <vcl_compiler.h>
#include "testlib/testlib_register.h"

DECLARE(test_bignum_io);
DECLARE(test_diag_matrix_io);
DECLARE(test_matrix_fixed_io);
DECLARE(test_matrix_io);
DECLARE(test_nonlinear_minimizer_io);
DECLARE(test_rational_io);
DECLARE(test_real_npolynomial_io);
DECLARE(test_real_polynomial_io);
DECLARE(test_sparse_matrix_io);
DECLARE(test_sym_matrix_io);
DECLARE(test_vector_fixed_io);
DECLARE(test_vector_io);
#if !VXL_LEGACY_FUTURE_REMOVE
DECLARE(golden_test_vnl_io);
#endif

void
register_tests()
{
  REGISTER(test_bignum_io);
  REGISTER(test_diag_matrix_io);
  REGISTER(test_matrix_fixed_io);
  REGISTER(test_matrix_io);
  REGISTER(test_nonlinear_minimizer_io);
  REGISTER(test_rational_io);
  REGISTER(test_real_npolynomial_io);
  REGISTER(test_real_polynomial_io);
  REGISTER(test_sparse_matrix_io);
  REGISTER(test_sym_matrix_io);
  REGISTER(test_vector_fixed_io);
  REGISTER(test_vector_io);
#if !VXL_LEGACY_FUTURE_REMOVE
  REGISTER(golden_test_vnl_io);
#endif
}

DEFINE_MAIN;
