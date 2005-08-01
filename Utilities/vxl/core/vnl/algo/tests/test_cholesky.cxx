// This is core/vnl/algo/tests/test_cholesky.cxx
#include <testlib/testlib_test.h>
#include <vcl_iostream.h>
#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_cholesky.h>
#include <vnl/algo/vnl_svd.h>
#include <vnl/vnl_random.h>

#include "test_util.h"

void test_cholesky()
{
  vnl_random rng(1000);
  vnl_matrix<double> A(3,3);
  test_util_fill_random(A.begin(), A.end(), rng);
  A = A * A.transpose();

  vnl_matrix<double> I(3,3);
  I.set_identity();

  {
    vnl_cholesky chol(A);
    vnl_svd<double> svd(A);
    vcl_cout << "cholesky inverse:\n" << chol.inverse() << '\n'
             << "svd inverse:\n" << svd.inverse() << '\n';
    testlib_test_assert_near("svd.inverse() ~= cholesky.inverse()",
                             (chol.inverse() - svd.inverse()).fro_norm());
  }
  {
    vnl_cholesky chol(A);
    testlib_test_assert_near("Ai * A - I", (chol.inverse() * A - I).fro_norm());
    testlib_test_assert_near("Ai * A - I", (A * chol.inverse() - I).fro_norm());
  }
  {
    vnl_cholesky chol(A, vnl_cholesky::estimate_condition);
    testlib_test_assert_near("Ai * A - I", (chol.inverse() * A - I).fro_norm());
    testlib_test_assert_near("Ai * A - I", (A * chol.inverse() - I).fro_norm());
  }
}

TESTMAIN(test_cholesky);
