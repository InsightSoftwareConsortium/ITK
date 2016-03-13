//: \file
//  \author Kevin de Souza
//  \brief Program to test operation of vnl_powell minimizer.
//  \note Adapted from test_amoeba.cxx

#include <iostream>
#include <cmath>
#include <vcl_cassert.h>
#include <vcl_compiler.h>

#include <vnl/vnl_vector.h>
#include <vnl/vnl_double_2.h>
#include <vnl/algo/vnl_powell.h>
#include <vnl/vnl_cost_function.h>

#include <testlib/testlib_test.h>


//-------------------------------------------------------------------------
// nD quadratic function with minimum at min{x[i]} = i;
//-------------------------------------------------------------------------
class vnl_test_powell_quadratic : public vnl_cost_function
{
 public:
  vnl_test_powell_quadratic(int n) : vnl_cost_function(n) {}

  double f(const vnl_vector<double>& x)
  {
    assert((int)x.size()==dim);
    double sum = 0;
    for (unsigned int i=0; i<x.size(); ++i)
      sum += (x[i]-i)*(x[i]-i);
    return sum;
  }
};


//-------------------------------------------------------------------------
// Function f(x,y) = (10*(y - x^2))^2 + (1-x)^2.
// Minimum at (1,1).
//-------------------------------------------------------------------------
class vnl_test_powell_rosenbrock : public vnl_cost_function
{
 public:
  vnl_test_powell_rosenbrock() : vnl_cost_function(2) {}

  double f(const vnl_vector<double>& x)
  {
    double a = 10*(x[1] - x[0]*x[0]);
    double b = 1 - x[0];
    return a*a + b*b;
  }

  void gradf(const vnl_vector<double>& x, vnl_vector<double>& g)
  {
    double a = 10*(x[1] - x[0]*x[0]);
    double b = 1 - x[0];
    g[0] = 2 * a * (-20*x[0]) - 2 * b;
    g[1] = 20 * a;
  }
};


//-------------------------------------------------------------------------
// Test 2D quadratic function
//-------------------------------------------------------------------------
static void test_quadratic_2d()
{
  std::cout << "---------------------\n"
           << " test_quadratic_2d()\n"
           << "---------------------\n";

  // No. of dimensions
  const unsigned n = 2;

  // Start at (1,...,1)
  {
    vnl_vector<double> x(n);
    x.fill(1);
    vnl_test_powell_quadratic cost1(n);
    vnl_powell powell(&cost1);
    powell.minimize(x);

    double err=0;
    for (unsigned i=0; i<n; ++i) err += std::fabs(x[i]-i);
    TEST_NEAR("Starting at (1,1,1...)", err, 0.0, 1e-5);
    std::cout<<"Number of evaluations: "<<powell.get_num_evaluations()<<std::endl;
  }

  // Start at x[i]=n-i
  {
    vnl_vector<double> x(n);
    for (unsigned i=0; i<n; ++i) x[i] = static_cast<int>(n) - static_cast<int>(i);
    vnl_test_powell_quadratic cost1(n);
    vnl_powell powell(&cost1);
    powell.minimize(x);

    double err=0;
    for (unsigned i=0; i<n; ++i) err += std::fabs(x[i]-i);
    TEST_NEAR("Starting at (1,1,1...)", err, 0.0, 1e-5);
    std::cout<<"Number of evaluations: "<<powell.get_num_evaluations()<<std::endl;
  }
  std::cout << std::endl;
}


//-------------------------------------------------------------------------
// Test quadratic functions with various numbers of dimensions
//-------------------------------------------------------------------------
static void test_quadratic_nd()
{
  // Max. no. of dimensions
  const unsigned max_n = 16;
  for (unsigned n=1; n<max_n; ++n)
  {
    std::cout << "-------------------\n"
             << " test_quadratic_" << n << "d\n"
             << "-------------------\n";

    // Start at (1,1,...,1)
    vnl_vector<double> x(n);
    x.fill(1);
    vnl_test_powell_quadratic cost1(n);
    vnl_powell powell(&cost1);
    powell.minimize(x);

    double err=0;
    for (unsigned i=0; i<n; ++i) err+=std::fabs(x[i]-i);
    TEST_NEAR("Starting at (1,1,1...)", err, 0.0, 1e-5);
    std::cout << "Number of evaluations: " << powell.get_num_evaluations()
             << std::endl << std::endl;
  }
}


//-------------------------------------------------------------------------
// Test rosenbrock 2d function
//-------------------------------------------------------------------------
static void test_rosenbrock_2d()
{
  std::cout << "----------------------\n"
           << " test_rosenbrock_2d()\n"
           << "----------------------\n";
  vnl_test_powell_rosenbrock c;
  vnl_double_2 xmin(1.0, 1.0); // true minimum
  vnl_powell powell(&c);
  vnl_double_2 x0(-2, 2);   // initial x
  vnl_vector<double> x = x0.as_vector();

  powell.minimize(x);
  double r = (x-xmin).magnitude();
  TEST_NEAR("test_rosenbrock_2d", r, 0, 1e-6);
  std::cout << "Number of evaluations: " << powell.get_num_evaluations()
           << std::endl << std::endl;
}


//-------------------------------------------------------------------------
// Main test function
//-------------------------------------------------------------------------
void test_powell()
{
  test_quadratic_2d();
  test_quadratic_nd();
  test_rosenbrock_2d();
}


TESTMAIN(test_powell);
