#include <vcl_iostream.h>
#include <vcl_cassert.h>
#include <vcl_cmath.h>

#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_amoeba.h>
#include <vnl/vnl_cost_function.h>

#include <testlib/testlib_test.h>

struct testlib_test_amoeba_cubic : public vnl_cost_function {
  testlib_test_amoeba_cubic() : vnl_cost_function(1) {}

  double f(const vnl_vector<double>& x) {
    return (2 - x[0]) * (2 - x[0]) + 10;
  }
};

struct testlib_test_amoeba_cost1 : public vnl_cost_function {
  testlib_test_amoeba_cost1(int n) : vnl_cost_function(n) {}

  double f(const vnl_vector<double>& x) {
    assert((int)x.size()==dim);
    double sum=0;
    for (unsigned int i=0;i<x.size();++i) sum+=(x[i]-i)*(x[i]-i);
    return sum;
  }
};

void test_amoeba()
{
  vcl_cout<<" ================== test_amoeba ===============\n";

  vcl_cout<<"Testing on 1D cubic\n";
  testlib_test_amoeba_cubic c;
  vnl_amoeba amoeba1(c);
  vnl_vector<double> x(1);
  x[0]=77;
  vcl_cout << "amoeba1: ";
  amoeba1.minimize(x);
  TEST("amoeba1", vcl_fabs(x[0] - 2) < 1e-5, true);

  int n = 4;
  vcl_cout<<"Testing on "<<n<<"-D quadratic\n";
  x.set_size(n);
  x.fill(1);
  testlib_test_amoeba_cost1 cost1(n);
  vnl_amoeba amoeba2(cost1);
  amoeba2.minimize(x);

  double err=0;
  for (int i=0;i<n;++i) err+=vcl_fabs(x[i]-i);
  TEST("Quadratic, starting at (1,1,1...)",err<1e-5,true);
  vcl_cout<<"Number of evaluations: "<<amoeba2.get_num_evaluations()<<vcl_endl;

  x.fill(0);
  amoeba2.set_max_iterations(10000);
  amoeba2.minimize(x);
  err=0;
  for (int i=0;i<n;++i) err+=vcl_fabs(x[i]-i);
  TEST("Quadratic, starting at (0,0,0...)",err<1e-5,true);
  vcl_cout<<"Number of evaluations: "<<amoeba2.get_num_evaluations()<<vcl_endl;

  vnl_vector<double> dx(n);
  dx.fill(0.1);
  x.fill(0);
  amoeba2.minimize(x,dx);
  err=0;
  for (int i=0;i<n;++i) err+=vcl_fabs(x[i]-i);
  TEST("Quadratic, starting at (0,0,0...) using minimise(x,dx)",err<1e-5,true);
  vcl_cout<<"Number of evaluations: "<<amoeba2.get_num_evaluations()<<vcl_endl;

  vcl_cout<<"Test static functions\n";
  x.fill(0);
  vnl_amoeba::minimize(cost1,x,dx);
  err=0;
  for (int i=0;i<n;++i) err+=vcl_fabs(x[i]-i);
  TEST("Quadratic, starting at (0,0,0...) using minimise(x,dx)",err<1e-5,true);
}

TESTMAIN(test_amoeba);
