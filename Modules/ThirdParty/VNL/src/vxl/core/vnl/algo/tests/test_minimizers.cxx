#include <vcl_iostream.h>
#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_brent.h>

#include <testlib/testlib_test.h>

struct cubic : public vnl_cost_function {
  cubic() : vnl_cost_function(1) {}

  double f(const vnl_vector<double>& x) {
    vcl_cout << ' ' << x[0];
    return (2 - x[0]) * (2 - x[0]) + 10;
  }
};

void test_minimizers()
{
  cubic c;
  vnl_brent b(&c);
  double x = 77;
  vcl_cout << "brent1: ";
  b.minimize_given_bounds(-100, x, 100, 1e-6, &x);
  vcl_cout << vcl_endl;
  TEST_NEAR("brent1", x, 2, 1e-5);
  vcl_cout << "brent2: ";
  x = 77;
  x = b.minimize(x);
  vcl_cout << vcl_endl;
  TEST_NEAR("brent2", x, 2, 1e-5);
}

TESTMAIN(test_minimizers);
