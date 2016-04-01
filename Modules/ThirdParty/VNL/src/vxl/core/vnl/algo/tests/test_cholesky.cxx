// This is core/vnl/algo/tests/test_cholesky.cxx
#include <iostream>
#include <testlib/testlib_test.h>
#include <vcl_compiler.h>
#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_cholesky.h>
#include <vnl/vnl_inverse.h>
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
    std::cout << "cholesky inverse:\n" << chol.inverse() << '\n'
             << "direct inverse:\n" << vnl_inverse(A) << '\n';
    testlib_test_assert_near("vnl_inverse() ~= cholesky.inverse()",
                             (chol.inverse() - vnl_inverse(A)).fro_norm());
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

  {
    vnl_vector<double> b(3),x0(3),x;
    test_util_fill_random(x0.begin(), x0.end(), rng);
    b=A*x0;
    vnl_cholesky chol(A);
    x=chol.solve(b);
    testlib_test_assert_near("Solve Ax=b",(x-x0).one_norm(),0,1e-6);
  }
}

TESTMAIN(test_cholesky);
