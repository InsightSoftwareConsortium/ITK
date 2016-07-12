// This is core/vnl/tests/test_complex.cxx
#include <iostream>
#include <cmath>
#include <complex>
#include <testlib/testlib_test.h>
//:
// \file

#include <vcl_compiler.h>

#include <vnl/vnl_vector.h>
#include <vnl/vnl_random.h>
#include <vnl/vnl_math.h>

//: inverse cosine for complex numbers.
// The implementation is at the bottom of this file.
// \author Peter Vanroose, ESAT, KULeuven.
std::complex<double> tc_acos(std::complex<double> x);

// make a vector with random, complex entries :
static void fill_rand(std::complex<double> *b, std::complex<double> *e, vnl_random &rng)
{
  for (std::complex<double> *p=b; p<e; ++p)
    (*p) = std::complex<double>( rng.drand64(-1.0, +1.0), rng.drand64(-1.0, +1.0) );
}

static void test_operators()
{
  std::complex<double> a(-5.);
  std::complex<double> b(7.,-1.);
  std::complex<double> c;
  //Numerically deterministic double precision
  c = a + b; TEST("sum", c, std::complex<double>(2.,-1.));
  c = a - b; TEST("difference", c, std::complex<double>(-12.,1.));
  c = a * b; TEST("product", c, std::complex<double>(-35.,5.));
  //Numerically could have small floating point truncation errors
  c = a / b; TEST_NEAR("quotient", c, std::complex<double>(-0.7,-0.1), 1e-12);
  //Numerically deterministic double precision
  a += b; TEST("+=", a, std::complex<double>(2.,-1.));
  a -= b; TEST("-=", a, std::complex<double>(-5.));
  a *= b; TEST("*=", a, std::complex<double>(-35.,5.));
  //Numerically could have small floating point truncation errors
  std::complex<double> a_pre = a;
  a /= b; TEST_NEAR("/=", a, std::complex<double>(-5.),1e-12);
  std::cout << "a /= b " << a_pre << " /= " << b
           << "\n         --> " << a << " diff: "<< (a-std::complex<double>(-5))
           <<'\n';
}

static void test_vector()
{
  vnl_random rng(9667566);
  vnl_vector<std::complex<double> > a(5); fill_rand(a.begin(), a.end(), rng);
  vnl_vector<std::complex<double> > b(5); fill_rand(b.begin(), b.end(), rng);

  std::cout << "a=" << a << '\n'
           << "b=" << b << '\n'
           << "dot_product(a,b)=" << dot_product(a,b) << '\n'
           << "inner_product(a,b)=" << inner_product(a,b) << '\n';

  std::complex<double> i(0,1);

  TEST_NEAR("inner_product() conjugates correctly", inner_product(i*a,b), i*inner_product(a,b), 1e-12);
  TEST_NEAR("inner_product() conjugates correctly", inner_product(a,i*b),-i*inner_product(a,b), 1e-12);

  TEST_NEAR("dot_product() does not conjugate", dot_product(i*a,b), i*dot_product(a,b), 1e-12);
  TEST_NEAR("dot_product() does not conjugate", dot_product(a,i*b), i*dot_product(a,b), 1e-12);

  double norma=0;
  for (unsigned n=0; n<a.size(); ++n)
    norma += std::real(a[n])*std::real(a[n]) + std::imag(a[n])*std::imag(a[n]);
  norma = std::sqrt(norma);
  TEST_NEAR("correct magnitude", norma, a.magnitude(), 1e-12);
}

static void test_cosine()
{
  vnl_random rng(1234567);
  for (int i=0; i<20; ++i)
  {
    double u = rng.drand32(vnl_math::pi_over_2);
    double v = rng.drand32(2.0);
    std::complex<double> c(u,v);
    std::complex<double> d = std::cos(c);
    std::complex<double> e = tc_acos(d);
    std::cout << c << ' ' << d << ' ' << e << '\n';
    TEST_NEAR("acos", c, e, 1e-12);
  }
}

void test_complex()
{
  test_operators();
  test_vector();
  test_cosine();
}

TESTMAIN(test_complex);

//: inverse cosine for complex numbers.
// implementation by Peter Vanroose, ESAT, KULeuven.
std::complex<double> tc_acos(std::complex<double> x)
{
  double a = std::real(x), b = std::imag(x);
  // special cases:
  if (b==0 && a > 1)
    return std::complex<double>(0.0, std::log(a+std::sqrt(a*a-1))); // == acosh(a)
  else if (b==0 && a >= -1.0)
    return std::acos(a);

  // the general case:
  // the result c + d*i satisfies a = cos(c)*cosh(d), b = -sin(c)*sinh(d)
  // hence $\frac{a^2}{\cos^2(c)} - \frac{b^2}{\sin^2(c)} = 1$.
  double q = (a*a-1)*(a*a-1)+b*b*(b*b+2*a*a+2);
  double t = 0.5*(1+a*a+b*b-std::sqrt(q));
  // this $t = \cos^2(c)$ solves the latter biquadratic equation and lies in [0,1].
  double aa = a/std::sqrt(t), bb = b/std::sqrt(1-t);
  double r_real = std::acos(std::sqrt(t));
  double r_imag = std::log(std::fabs(aa-bb));
  return std::complex<double>(r_real, r_imag);
}
