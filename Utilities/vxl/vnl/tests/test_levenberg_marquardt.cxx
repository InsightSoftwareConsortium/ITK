/*
  fsm
*/
#include <vcl_cmath.h>
#include <vcl_iostream.h>

#include <testlib/testlib_test.h>
#include <vnl/vnl_least_squares_function.h>
#include <vnl/algo/vnl_levenberg_marquardt.h>

struct vnl_rosenbrock : public vnl_least_squares_function
{
  vnl_rosenbrock(bool with_grad): vnl_least_squares_function(2, 2, with_grad ? use_gradient : no_gradient) {}

  void f(vnl_vector<double> const& x, vnl_vector<double>& y) {
    //testlib_test_assert("size of x", x.size() == 2);
    //testlib_test_assert("size of y", y.size() == 2);
    y[0] = 10*(x[1] - x[0]*x[0]);
    y[1] = 1 - x[0];
  }

  void gradf(vnl_vector<double> const& x, vnl_matrix<double> &J) {
    //testlib_test_assert("size of x", x.size() == 2);
    //testlib_test_assert("size of J", J.rows() == 2 && J.cols() == 2);
    J[0][0] = -20 * x[0]; J[0][1] = 10;
    J[1][0] = -1;         J[1][1] = 0;
  }
};

static
void do_test(bool with_grad)
{
  vnl_rosenbrock f(with_grad);

  vnl_vector<double> x0(2);
  x0[0] =  2.7;
  x0[1] = -1.3;
  vcl_cout << "x0 = " << x0 << vcl_endl;

  vnl_levenberg_marquardt lm(f);

  vnl_vector<double> x1 = x0;
  if (f.has_gradient())
    lm.minimize_using_gradient(x1);
  else
    lm.minimize_without_gradient(x1);
  lm.diagnose_outcome(vcl_cout);
  vcl_cout << "x1 = " << x1 << vcl_endl;

  double err = vcl_abs(x1[0] - 1) + vcl_abs(x1[1] - 1);
  vcl_cout << "err = " << err << vcl_endl;
  testlib_test_assert("converged to (1, 1)", err <= 1e-10);
}

static
void test_levenberg_marquardt()
{
  do_test(true);
  do_test(false);
}

TESTMAIN(test_levenberg_marquardt);
