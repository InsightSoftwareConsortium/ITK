#include "testlib/testlib_register.h"

// VNL_EISPACK_REMOVED is defined by CMake under ITK_FUTURE_LEGACY_REMOVE, where
// the eispack-backed vnl_*_eigensystem solvers are excluded from the build.
DECLARE(test_amoeba);
DECLARE(test_cholesky);
DECLARE(test_determinant);
DECLARE(test_rank);
#ifndef VNL_EISPACK_REMOVED
DECLARE(test_generalized_eigensystem);
#endif
DECLARE(test_ldl_cholesky);
DECLARE(test_levenberg_marquardt);
DECLARE(test_matrix_update);
DECLARE(test_minimizers);
DECLARE(test_powell);
DECLARE(test_qr);
DECLARE(test_rational);
#ifndef VNL_EISPACK_REMOVED
DECLARE(test_real_eigensystem);
#endif
DECLARE(test_sparse_matrix);
DECLARE(test_integral);
DECLARE(test_svd);
DECLARE(test_svd_fixed);
#ifndef VNL_EISPACK_REMOVED
DECLARE(test_symmetric_eigensystem);
#endif
DECLARE(test_algo);
DECLARE(test_solve_qp);
DECLARE(test_sparse_lu);
DECLARE(test_bracket_minimum);
DECLARE(test_brent_minimizer);
DECLARE(test_complex_algo);

void
register_tests()
{
  REGISTER(test_amoeba);
  REGISTER(test_cholesky);
  REGISTER(test_determinant);
  REGISTER(test_rank);
#ifndef VNL_EISPACK_REMOVED
  REGISTER(test_generalized_eigensystem);
#endif
  REGISTER(test_ldl_cholesky);
  REGISTER(test_levenberg_marquardt);
  REGISTER(test_matrix_update);
  REGISTER(test_minimizers);
  REGISTER(test_powell);
  REGISTER(test_qr);
#ifndef VNL_EISPACK_REMOVED
  REGISTER(test_real_eigensystem);
#endif
  REGISTER(test_integral);
  REGISTER(test_sparse_matrix);
  REGISTER(test_svd);
  REGISTER(test_svd_fixed);
#ifndef VNL_EISPACK_REMOVED
  REGISTER(test_symmetric_eigensystem);
#endif
  REGISTER(test_algo);
  REGISTER(test_solve_qp);
  REGISTER(test_sparse_lu);
  REGISTER(test_bracket_minimum);
  REGISTER(test_brent_minimizer);
  REGISTER(test_complex_algo);
}

DEFINE_MAIN;
