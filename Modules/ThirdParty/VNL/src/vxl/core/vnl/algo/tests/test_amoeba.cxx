#include <iostream>
#include <cmath>
#include <vcl_cassert.h>
#include <vcl_compiler.h>

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
  std::cout<<" ================== test_amoeba ===============\n"

          <<"Testing on 1D cubic\n";
  testlib_test_amoeba_cubic c;
  vnl_amoeba amoeba1(c);
  vnl_vector<double> x(1);
  x[0]=77;
  std::cout << "amoeba1: ";
  amoeba1.minimize(x);
  TEST_NEAR("amoeba1", x[0], 2, 1e-5);

  // Test a quadratic cost function for a varying number of dimensions
  for (unsigned n=1; n<=4; ++n)
  {
    std::cout << "-------------------------------\n"
             << "Testing on "<<n<<"-D quadratic\n";
    x.set_size(n);
    x.fill(1);
    testlib_test_amoeba_cost1 cost1(n);
    vnl_amoeba amoeba2(cost1);
    amoeba2.minimize(x);

    double err=0;
    for (unsigned int i=0; i<n; ++i) err+=std::fabs(x[i]-i);
    TEST_NEAR("Quadratic, starting at (1,1,1...)", err, 0.0, 1e-5); // fails from n=6 onwards: result is 0.379827 for n=6, 0.0812755 for n=7, 7.01668 for n=8.
    std::cout<<"Number of evaluations: "<<amoeba2.get_num_evaluations()<<std::endl;

    x.fill(0);
    amoeba2.set_max_iterations(10000);
    amoeba2.minimize(x);
    err=0;
    for (unsigned int i=0; i<n; ++i) err+=std::fabs(x[i]-i);
    TEST_NEAR("Quadratic, starting at (0,0,0...)", err, 0.0, 1e-5); // fails from n=5 onwards, "random" results
    std::cout<<"Number of evaluations: "<<amoeba2.get_num_evaluations()<<std::endl;

    vnl_vector<double> dx(n);
    dx.fill(0.1);
    x.fill(0);
    amoeba2.minimize(x,dx);
    err=0;
    for (unsigned int i=0; i<n; ++i) err+=std::fabs(x[i]-i);
    TEST_NEAR("Quadratic, starting at (0,0,0...) using minimise(x,dx)",err,0,1e-5); // fails from n=7 onwards, value approx. 5.0074 for n=7, between 5.95 and 6.05 for n=8
    std::cout<<"Number of evaluations: "<<amoeba2.get_num_evaluations()<<std::endl

            <<"Test static functions\n";
    x.fill(0);
    vnl_amoeba::minimize(cost1,x,dx);
    err=0;
    for (unsigned int i=0; i<n; ++i) err+=std::fabs(x[i]-i);
    TEST_NEAR("Quadratic, starting at (0,0,0...) using minimise(x,dx)",err,0,1e-5); // fails from n=6 onwards: result is 0.0000167217 for n=6, 4.80315 for n=7, 6.64319 for n=8.
  }
}

TESTMAIN(test_amoeba);
