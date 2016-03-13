// not used? #include <iostream>
#include <cmath>
#include <vcl_compiler.h>
#include <vnl/vnl_double_3.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_analytic_integrant.h>
#include <vnl/algo/vnl_simpson_integral.h>
#include <vnl/algo/vnl_adaptsimpson_integral.h>
#include <testlib/testlib_test.h>

class my_test_integrant : public vnl_analytic_integrant
{
 public:
   double f_(double x) { return x/(1+x*x); }
};

class gaussian_integrant : public vnl_analytic_integrant
{
 public:
  gaussian_integrant(double sr, double sz,  vnl_double_3 p0) : sr_(sr), sz_(sz), p0_(p0)
  {
    oneoversr2_ = 1.0 / sr_ / sr_;
    oneoversz2_ = 1.0 / sz_ / sz_;
    normalizer_ = -sr_ * sr_ / (sz_ * 2 * vnl_math::sqrt2pi);
  }

  void set_varying_params(double theta, double phi)
  {
    theta_ = theta;
    phi_ = phi;
  }

  double f_(double rho)
  {
    double x2 = std::pow( p0_.get(0) + rho * std::sin(theta_) * std::cos(phi_), 2 );
    double y2 = std::pow( p0_.get(1) + rho * std::sin(theta_) * std::sin(phi_), 2 );
    double z2 = std::pow( p0_.get(2) + rho * std::cos(theta_), 2 );
    double term1 = oneoversr2_ * ((x2 + y2) * oneoversr2_ - 2);
    double term2 = std::exp(-(x2+y2)*oneoversr2_/2) * std::exp(-z2*oneoversz2_/2);
    return normalizer_ * term1 * term2;
  }

 protected:
  // fixed parameters
  double sr_;
  double sz_;
  vnl_double_3 p0_;

  // varying parameters
  double theta_;
  double phi_;

  // pre-calculated values to save computing time
  double oneoversr2_;
  double oneoversz2_;
  double normalizer_;
};


void test_integral()
{
  my_test_integrant f;
  vnl_simpson_integral simpson_integral;

  double a = 0;
  double b = 1;

  TEST_NEAR("simpson integral of x/(1+x^2) from  0 to 1 is: ",
            simpson_integral.integral(&f, a, b, 100), 0.5*std::log(2.0), 1e-6);

  vnl_adaptsimpson_integral adaptsimpson_integral;

  TEST_NEAR("adaptive simpson integral of x/(1+x^2) from 0 to 1 is: ",
            adaptsimpson_integral.integral(&f, a, b, 1e-11f), 0.5*std::log(2.0), 1e-6);

  gaussian_integrant filter_fnct(0.04, 0.1, vnl_double_3(0,0,0));
  filter_fnct.set_varying_params(0, 0);

  TEST_NEAR("simpson integral of a filter function from  -10 to 10 is: ",
            simpson_integral.integral(&filter_fnct, -10, 10, 1000), 1, 1e-6);

  TEST_NEAR("adaptive simpson integral of a filter function from -10 to 10 is: ",
            adaptsimpson_integral.integral(&filter_fnct, -10, 10, 1e-6), 1, 1e-6);
}

TESTMAIN( test_integral );
