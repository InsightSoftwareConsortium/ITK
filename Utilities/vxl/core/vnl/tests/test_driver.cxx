#include <testlib/testlib_register.h>

DECLARE( test_bignum );
DECLARE( test_complex );
DECLARE( test_inverse );
DECLARE( test_diag_matrix );
DECLARE( test_file_matrix );
DECLARE( test_finite );
DECLARE( test_math );
//DECLARE( test_matlab );
DECLARE( test_matrix );
DECLARE( test_matrix_exp );
DECLARE( test_matrix_fixed );
DECLARE( test_matrix_fixed_ref );
DECLARE( test_numeric_traits );
DECLARE( test_rational );
DECLARE( test_real_polynomial );
DECLARE( test_resize );
DECLARE( test_sample );
DECLARE( test_sym_matrix );
DECLARE( test_transpose );
DECLARE( test_fastops );
DECLARE( test_vector );
DECLARE( test_vector_fixed_ref );
DECLARE( test_gamma );
DECLARE( test_random );
DECLARE( test_arithmetic );
DECLARE( test_hungarian_algorithm );
DECLARE( test_integrant );
DECLARE( test_bessel );

void
register_tests()
{
  REGISTER( test_bignum );
  REGISTER( test_complex );
  REGISTER( test_inverse );
  REGISTER( test_diag_matrix );
  REGISTER( test_file_matrix );
  REGISTER( test_finite );
  REGISTER( test_math );
  //REGISTER( test_matlab );
  REGISTER( test_matrix );
  REGISTER( test_matrix_exp );
  REGISTER( test_matrix_fixed );
  REGISTER( test_matrix_fixed_ref );
  REGISTER( test_numeric_traits );
  REGISTER( test_rational );
  REGISTER( test_real_polynomial );
  REGISTER( test_resize );
  REGISTER( test_sample );
  REGISTER( test_sym_matrix );
  REGISTER( test_transpose );
  REGISTER( test_fastops );
  REGISTER( test_vector );
  REGISTER( test_vector_fixed_ref );
  REGISTER( test_gamma );
  REGISTER( test_random );
  REGISTER( test_arithmetic );
  REGISTER( test_hungarian_algorithm );
  REGISTER( test_integrant );
  REGISTER( test_bessel );
}

DEFINE_MAIN;
