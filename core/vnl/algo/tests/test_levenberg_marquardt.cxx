// @author fsm
#include <cmath>
#include <iostream>
#include <cassert>
#include "vnl/vnl_double_2.h"

#include "testlib/testlib_test.h"
#include "vnl/vnl_least_squares_function.h"
#include <vnl/algo/vnl_levenberg_marquardt.h>

struct vnl_rosenbrock : public vnl_least_squares_function
{
  vnl_rosenbrock(bool with_grad)
    : vnl_least_squares_function(2, 2, with_grad ? use_gradient : no_gradient)
  {}

  void
  f(vnl_vector<double> const & x, vnl_vector<double> & y) override
  {
    TEST("size of x", x.size(), 2);
    TEST("size of y", y.size(), 2);
    y[0] = 10 * (x[1] - x[0] * x[0]);
    y[1] = 1 - x[0];
  }

  void
  gradf(vnl_vector<double> const & x, vnl_matrix<double> & J) override
  {
    TEST("size of x", x.size(), 2);
    TEST("size of J", J.rows() == 2 && J.cols() == 2, true);
    J[0][0] = -20 * x[0];
    J[0][1] = 10;
    J[1][0] = -1;
    J[1][1] = 0;
  }
};

struct linear_est : public vnl_least_squares_function
{
  linear_est(vnl_matrix<double> const & A, vnl_vector<double> const & b, bool with_grad)
    : vnl_least_squares_function(A.cols(), A.rows(), with_grad ? use_gradient : no_gradient)
    , A_(A)
    , b_(b)
  {
    assert(A.rows() == b.size());
  }

  void
  f(vnl_vector<double> const & x, vnl_vector<double> & y) override
  {
    y = A_ * x - b_;
  }

  void
  gradf(vnl_vector<double> const & /*x*/, vnl_matrix<double> & J) override
  {
    J = A_;
  }

  vnl_matrix<double> A_;
  vnl_vector<double> b_;
};

static void
do_rosenbrock_test(bool with_grad)
{
  vnl_rosenbrock f(with_grad);

  vnl_double_2 x0(2.7, -1.3);
  std::cout << "x0 = " << x0 << std::endl;

  vnl_levenberg_marquardt lm(f);

  vnl_vector<double> x1 = x0.as_vector();
  if (f.has_gradient())
    lm.minimize_using_gradient(x1);
  else
    lm.minimize_without_gradient(x1);
  lm.diagnose_outcome(std::cout);
  std::cout << "x1 = " << x1 << std::endl;

  double err = std::abs(x1[0] - 1) + std::abs(x1[1] - 1);
  std::cout << "err = " << err << std::endl;
  TEST_NEAR("converged to (1, 1)", err, 0.0, 1e-10);
}

static void
do_linear_test(bool with_grad)
{
  vnl_matrix<double> A(6, 2, 1.0);
  vnl_vector<double> b(6);

  A(0, 1) = 10;
  A(1, 1) = 15;
  A(2, 1) = 5.1;
  A(3, 1) = 20.2;
  A(4, 1) = -0.3;
  A(5, 1) = 25;

  b(0) = 10;
  b(1) = 15.5;
  b(2) = 4.5;
  b(3) = 21;
  b(4) = 1;
  b(5) = 24.3;

  linear_est f(A, b, with_grad);
  vnl_levenberg_marquardt lm(f);
  vnl_vector<double> x(2, -1000.0); // init can be far off
  // since obj function is linear
  // high precision can be achieved
  lm.set_x_tolerance(1e-12);
  lm.set_f_tolerance(1e-12);
  lm.set_g_tolerance(1e-12);

  if (f.has_gradient())
    lm.minimize_using_gradient(x);
  else
    lm.minimize_without_gradient(x);
  lm.diagnose_outcome(std::cout);
  std::cout << "x = " << x << std::endl;

  vnl_vector<double> true_x(2);
  true_x[1] = 0.969684757298943;
  true_x[0] = 0.595607200429874;

  TEST_NEAR("converged to true estimate", (true_x - x).two_norm(), 0, 1e-6);

  // now check (inverse of) covariance approximation
  vnl_matrix<double> true_cov(2, 2);
  true_cov(0, 0) = 6;
  true_cov(1, 0) = 75;
  true_cov(0, 1) = 75;
  true_cov(1, 1) = 1384.14;

  vnl_matrix<double> covar = lm.get_JtJ();
  std::cout << "Cov(x) =\n" << covar << std::endl;
  TEST_NEAR("covariance approximation", (true_cov - covar).array_two_norm(), 0, 1e-5);
}

static void
test_levenberg_marquardt()
{
  do_rosenbrock_test(true);
  do_rosenbrock_test(false);

  do_linear_test(true);
  do_linear_test(false);
}

TESTMAIN(test_levenberg_marquardt);
