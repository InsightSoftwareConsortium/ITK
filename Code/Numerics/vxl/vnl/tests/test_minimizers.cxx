

#include <vcl/vcl_iostream.h>
#include <vcl/vcl_cassert.h>

#include <vnl/vnl_complex.h>
#include <vnl/vnl_test.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matops.h>

#include <vnl/algo/vnl_brent.h>

struct cubic : public vnl_cost_function {
  cubic() : vnl_cost_function(1) {}
  
  double f(const vnl_vector<double>& x) {
    cerr << " " << x[0];
    return (2 - x[0]) * (2 - x[0]) + 10;
  }
};

void test_minimizers()
{
  cubic c;
  vnl_brent b(&c);
  double x = 77;
  cerr << "brent1: ";
  b.minimize_given_bounds(-100, x, 100, 1e-6, &x);
  cerr << endl;
  TEST("brent1", fabs(x - 2) < 1e-5, true);
  cerr << "brent2: ";
  x = 77;
  x = b.minimize(x);
  cerr << endl;
  TEST("brent2", fabs(x - 2) < 1e-5, true);
}

TESTMAIN(test_minimizers);
