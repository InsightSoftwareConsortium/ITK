// This is core/vnl/algo/tests/test_ldl_cholesky.cxx
#include <iostream>
#include <testlib/testlib_test.h>
#include <vcl_compiler.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_diag_matrix.h>
#include <vnl/algo/vnl_ldl_cholesky.h>
#include <vnl/vnl_inverse.h>
#include <vnl/vnl_random.h>

#include "test_util.h"

void test_ldl_cholesky()
{
  vnl_random rng(1000);
  vnl_matrix<double> A(3,3);
  test_util_fill_random(A.begin(), A.end(), rng);
  A = A * A.transpose();

  vnl_matrix<double> I(3,3);
  I.set_identity();

  {
    vnl_ldl_cholesky chol(A);
    vnl_matrix<double> A2 = chol.lower_triangle() * vnl_diag_matrix<double>(chol.diagonal()) * chol.upper_triangle();
    TEST_NEAR("LDL'=A",(A-A2).fro_norm(), 0.0, 1e-12);
  }
  {
    // Test the rank-1 update
    vnl_vector<double> v(3);
    vnl_matrix<double> Mv(3,1);
    test_util_fill_random(v.begin(), v.end(), rng);
    Mv.set_column(0,v);
    vnl_ldl_cholesky chol(A);
    chol.rank1_update(v);
    vnl_matrix<double> A2 = A + Mv*Mv.transpose();
    vnl_matrix<double> A3 = chol.lower_triangle() * vnl_diag_matrix<double>(chol.diagonal()) * chol.upper_triangle();
    TEST_NEAR("Rank 1 update",(A2-A3).fro_norm(), 0.0, 1e-12);
  }
  {
    // Test the rank 2 update
    vnl_matrix<double> W(3,2);
    test_util_fill_random(W.begin(), W.end(), rng);
    vnl_ldl_cholesky chol(A);
    chol.update(W);
    vnl_matrix<double> A2 = A + W*W.transpose();
    vnl_matrix<double> A3 = chol.lower_triangle() * vnl_diag_matrix<double>(chol.diagonal()) * chol.upper_triangle();
    TEST_NEAR("Rank 2 update",(A2-A3).fro_norm(), 0.0, 1e-12);
  }
  {
    // Test the rank 4 update
    vnl_matrix<double> W(3,4);
    test_util_fill_random(W.begin(), W.end(), rng);
    vnl_ldl_cholesky chol(A);
    chol.update(W);
    std::cout<<"Adding: "<<W*W.transpose()<<std::endl;
    vnl_matrix<double> A2 = A + W*W.transpose();
    vnl_matrix<double> A3 = chol.lower_triangle() * vnl_diag_matrix<double>(chol.diagonal()) * chol.upper_triangle();
    TEST_NEAR("Rank 2 update",(A2-A3).fro_norm(), 0.0, 1e-12);
  }

  {
    vnl_ldl_cholesky chol(A);
    std::cout << "cholesky inverse:\n" << chol.inverse() << '\n'
             << "vnl_inverse:\n" << vnl_inverse(A) << '\n';
    TEST_NEAR("vnl_inverse() ~= cholesky.inverse()", (chol.inverse() - vnl_inverse(A)).fro_norm(), 0.0, 1e-12);
  }
  {
    vnl_ldl_cholesky chol(A);
    TEST_NEAR("Ai * A - I", (chol.inverse() * A - I).fro_norm(), 0.0, 1e-12);
    TEST_NEAR("Ai * A - I", (A * chol.inverse() - I).fro_norm(), 0.0, 1e-12);
  }
  {
    vnl_ldl_cholesky chol(A, vnl_ldl_cholesky::estimate_condition);
    TEST_NEAR("Ai * A - I", (chol.inverse() * A - I).fro_norm(), 0.0, 1e-12);
    TEST_NEAR("Ai * A - I", (A * chol.inverse() - I).fro_norm(), 0.0, 1e-12);
  }

  {
    vnl_vector<double> b(3),x0(3),x;
    test_util_fill_random(x0.begin(), x0.end(), rng);
    b=A*x0;
    vnl_ldl_cholesky chol(A);
    x=chol.solve(b);
    TEST_NEAR("Solve Ax=b",(x-x0).one_norm(),0,1e-6);
  }
  {
    vnl_vector<double> b(3),x0(3),x;
    test_util_fill_random(x0.begin(), x0.end(), rng);
    vnl_ldl_cholesky chol(A);
    b=chol.lower_triangle()*x0;
    x=b;
    chol.solve_lx(x);
    TEST_NEAR("Solve Lx=b",(x-x0).one_norm(),0,1e-6);
  }
  {
    vnl_ldl_cholesky chol(A);
    vnl_vector<double> v(3);
    test_util_fill_random(v.begin(), v.end(), rng);

    double res1 = chol.xt_m_inv_x(v);
    double res2 = dot_product(v,chol.inverse()*v);

    TEST_NEAR("x' * inv(M) * x",res1,res2,1e-12);
  }
  {
    vnl_ldl_cholesky chol(A);
    vnl_vector<double> v(3);
    test_util_fill_random(v.begin(), v.end(), rng);

    double res1 = chol.xt_m_x(v);
    double res2 = dot_product(v,A*v);

    TEST_NEAR("x' * M * x",res1,res2,1e-12);
  }
}

TESTMAIN(test_ldl_cholesky);
