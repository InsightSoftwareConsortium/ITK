// This is core/vnl/algo/tests/test_algo.cxx
#include <complex>
#include "testlib/testlib_test.h"
//:
// \file
// \brief test miscellaneous classes and functions in vnl/algo.
// This file contains short tests for several algorithms in vnl/algo
// that are not tested more extensively in separate test files.
// Currently, the following classes or functions are tested here:
// - vnl_svd_economy
// - vnl_matrix_inverse
// - vnl_conjugate_gradient
// - vnl_lbfgs
// - vnl_powell
// - vnl_lsqr
// - vnl_discrete_diff_fwd
// - vnl_discrete_diff_sym
//
// \author Peter Vanroose, KULeuven/ESAT.
// \date 20 September 2003

#include <vnl/algo/vnl_conjugate_gradient.h>
#include <vnl/algo/vnl_discrete_diff.h>
#include <vnl/algo/vnl_lbfgs.h>
#include <vnl/algo/vnl_lbfgsb.h>
#include <vnl/algo/vnl_lsqr.h>
#include <vnl/algo/vnl_matrix_inverse.h>
#include <vnl/algo/vnl_powell.h>
#include <vnl/algo/vnl_svd_economy.h>
#include <vnl/algo/vnl_svd.h>
#include "vnl/vnl_sparse_matrix_linear_system.h"
#include "vnl/vnl_least_squares_function.h"

static void
test_matrix_inverse()
{
  double data[] = { 1., -1., 1., -1., 1., 1., -1., -1., 1., 1., 1., 1., 1., -1., -1., 1. };
  const vnl_matrix<double> m(data, 4, 4);
  vnl_svd_economy<double> svde(m);
  vnl_matrix<double> V = svde.V();
  vnl_svd<double> svd(m);
  vnl_matrix<double> V0 = svd.V();
  TEST_NEAR("vnl_svd_economy", V[0][1], V0[0][1], 1e-6);

  const vnl_matrix<double> inv{ vnl_matrix_inverse<double>(m).as_matrix() };
  vnl_matrix<double> identity(4, 4);
  identity.set_identity();
  TEST_NEAR("vnl_matrix_inverse", (m * inv - identity).array_inf_norm(), 0, 1e-6);
}

class F_test_powell : public vnl_cost_function
{
public:
  // Local min near (0,0) is at (1,1) and has value 1.
  F_test_powell()
    : vnl_cost_function(2)
  {}
  double
  f(const vnl_vector<double> & x) override
  {
    const double u = x[0] - x[1] * x[1];
    const double v = x[1] - 1;
    return u * u + v * v + 1;
  }
  void
  gradf(const vnl_vector<double> & x, vnl_vector<double> & g) override
  {
    g[0] = 2 * x[0] - 2 * x[1] * x[1];
    g[1] = 4 * x[1] * x[1] * x[1] - 4 * x[0] * x[1] + 2 * x[1] - 2;
  }
};


class F_broken : public vnl_cost_function
{
public:
  F_broken()
    : vnl_cost_function(1)
  {}
  double
  f(const vnl_vector<double> &) override
  {
    return 0;
  }
  void
  gradf(const vnl_vector<double> &, vnl_vector<double> & gradient) override
  {
    gradient[0] = 1;
  }
};


static void
test_powell()
{
  F_test_powell f; // local minimum is 1 in (1,1).
  vnl_vector<double> x(2);
  x[0] = x[1] = 0.0;
  vnl_conjugate_gradient cg(f);
  cg.minimize(x);
  TEST_NEAR("vnl_conjugate_gradient", x[0], 1.0, 1e-5);

  F_broken fb;
  vnl_vector<double> x2(1, 0);
  vnl_conjugate_gradient cg2(fb);
  const bool rv = cg2.minimize(x2);
  TEST("vnl_conjugate_gradient on broken function should fail", rv, false);


  vnl_lbfgs lbfgs(f);
  x[0] = x[1] = 0.0;
  lbfgs.minimize(x);
  TEST_NEAR("vnl_lbfgs", x[1], 1.0, 1e-6);

  {
    // Local min near (0,0) with (x,y) bounded to [-0.5,+0.5] is
    //   at (0.25, 0.5) with value 1.25.
    vnl_lbfgsb lbfgsb(f);
    x[0] = x[1] = 0.0;
    vnl_vector<double> l(2);
    l[0] = -0.5;
    l[1] = -0.5;
    vnl_vector<double> u(2);
    u[0] = +0.5;
    u[1] = +0.5;
    vnl_vector<long> nbd(2);
    nbd[0] = 3;
    nbd[1] = 3;
    lbfgsb.set_lower_bound(l);
    lbfgsb.set_upper_bound(u);
    lbfgsb.set_bound_selection(nbd);
    lbfgsb.minimize(x);
    TEST_NEAR("vnl_lbfgsb", x[0], 0.25, 1e-6);
  }

  vnl_powell powell(&f);
  x[0] = x[1] = 0.0;
  powell.minimize(x);
  TEST_NEAR("vnl_powell", f.f(x), 1.0, 1e-6);
}

static void
test_lsqr()
{
  vnl_sparse_matrix<double> A(2, 2);
  vnl_vector<double> b(2);
  A(0, 0) = 2;
  A(0, 1) = 3;
  A(1, 0) = 4;
  A(1, 1) = 5;
  b[0] = 5;
  b[1] = 9;
  vnl_sparse_matrix_linear_system<double> ls(A, b);
  vnl_vector<double> x(2);
  x[0] = x[1] = 0.0;
  vnl_lsqr lsqr(ls);
  lsqr.minimize(x);
  TEST_NEAR("vnl_lsqr", x[1], 1.0, 1e-6);
}

class F_test_discrete_diff : public vnl_least_squares_function
{
public:
  F_test_discrete_diff()
    : vnl_least_squares_function(2, 2, no_gradient)
  {}
  void
  f(const vnl_vector<double> & x, vnl_vector<double> & fx) override
  {
    fx[0] = x[0] - x[1] * x[1];
    fx[1] = x[1] - 1;
  }
};

static void
test_discrete_diff()
{
  F_test_discrete_diff f;
  const double h = 0.1;
  vnl_vector<double> x(2);
  x[0] = 5.0;
  x[1] = 9.0;
  vnl_matrix<double> J(2, 2);
  vnl_discrete_diff_fwd(&f, h, x, J);
  TEST_NEAR("vnl_discrete_diff_fwd", J(0, 1), -18.1, 1e-6);
  vnl_discrete_diff_sym(&f, h, x, J);
  TEST_NEAR("vnl_discrete_diff_sym", J(0, 1), -18, 1e-6);
}

void
test_algo()
{
  test_matrix_inverse();
  test_powell();
  test_lsqr();
  test_discrete_diff();
}

TESTMAIN(test_algo);
