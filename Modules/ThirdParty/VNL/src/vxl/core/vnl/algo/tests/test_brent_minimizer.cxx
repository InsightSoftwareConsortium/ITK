#include <vcl_iostream.h>
#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_brent_minimizer.h>

#include <testlib/testlib_test.h>

struct brent_f1 : public vnl_cost_function {
  unsigned n_evals;
  brent_f1() : vnl_cost_function(1),n_evals(0) {}

  double f(const vnl_vector<double>& x) { n_evals++;
    return (2 - x[0]) * (2 - x[0]) + 10;
  }
};

struct brent_f2 : public vnl_cost_function {
  unsigned n_evals;
  brent_f2() : vnl_cost_function(1),n_evals(0) {}

  double f(const vnl_vector<double>& x) { n_evals++;
    double y = (2 - x[0]) * (2 - x[0]);
    return y*y + 10;
  }
};

void test_brent_minimizer()
{
  brent_f1 f1;
  vnl_brent_minimizer brent1(f1);

  double x = brent1.minimize(77);
  TEST_NEAR("f1 minimize(77)",x,2,1e-6);
  vcl_cout<<"Number of evaluations: "<<f1.n_evals<<vcl_endl;

  f1.n_evals=0;
  x = brent1.minimize(13);
  TEST_NEAR("f1 minimize(13)",x,2,1e-6);
  vcl_cout<<"Number of evaluations: "<<f1.n_evals<<vcl_endl;

  brent_f2 f2;
  vnl_brent_minimizer brent2(f2);

  f2.n_evals=0;
  x = brent2.minimize(77);
  TEST_NEAR("f2 minimize(77)",x,2,1e-3);
  vcl_cout<<"Number of evaluations: "<<f2.n_evals<<vcl_endl;

  f2.n_evals=0;
  x = brent2.minimize(13);
  TEST_NEAR("f2 minimize(13)",x,2,1e-3);
  vcl_cout<<"Number of evaluations: "<<f2.n_evals<<vcl_endl;

  vnl_vector<double> v(1);
  v[0]=2;
  double f2_a = f2.f(v);
  v[0]=x;
  double f2_b = f2.f(v);
  vcl_cout<<"f2(2)-f2(x)="<<f2_a-f2_b<<vcl_endl;

}

TESTMAIN(test_brent_minimizer);
