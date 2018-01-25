// This is core/vnl/vnl_gamma.cxx
#include <iostream>
#include "vnl_gamma.h"
//:
// \file
// \brief Complete and incomplete gamma function approximations
// \author Tim Cootes

#include <vcl_compiler.h>
#include <vcl_cassert.h>

#if defined(__INTEL_COMPILER)
# pragma warning (disable:279) /* controlling expression is constant */
#endif

//: Approximate gamma function
//  Uses 6 parameter Lanczos approximation as described by Viktor Toth
//  (http://www.rskey.org/gamma.htm)
//  Accurate to about 3e-11.
double vnl_log_gamma(double x)
{
  double zp = 2.50662827563479526904;
  zp += 225.525584619175212544/x;
  zp -= 268.295973841304927459/(x+1.0);
  zp += 80.9030806934622512966/(x+2.0);
  zp -= 5.00757863970517583837/(x+3.0);
  zp += 0.0114684895434781459556/(x+4.0);

  double x1 = x+4.65;

  return std::log(zp)+(x-0.5)*std::log(x1)-x1;
}

const int MAX_ITS = 100;
const double MaxRelError = 3.0e-7;
const double vnl_very_small = 1.0e-30;

//: Use series expansion of incomplete gamma function
static double vnl_gamma_series(double a, double x)
{
  if (x>0)
  {
    double a_i=a;
    double term_i=1.0/a;
    double sum = term_i;
    for (int i=1;i<=MAX_ITS;++i)
    {
      a_i+=1;
      term_i *= x/a_i;
      sum += term_i;
      if (std::fabs(term_i) < std::fabs(sum)*MaxRelError)
        return sum*std::exp(-x+a*std::log(x)-vnl_log_gamma(a));
    }
    std::cerr<<"vnl_gamma_series : Failed to converge in "<<MAX_ITS<<" steps\n"
            <<"a = "<<a<<"   x= "<< x <<"\nReturning best guess.\n";
    return sum*std::exp(-x+a*std::log(x)-vnl_log_gamma(a));
  }
  else if (x < 0.0)
    assert(!"vnl_gamma_series - x less than 0");

  return 0.0;
}

//: Incomplete gamma using continued fraction representation
// Use Lentz's algorithm
// Continued fraction with terms a_i/b_i
// a_i = i*(a-i), b_i = (x+a-2i)
static double vnl_gamma_cont_frac(double a, double x)
{
  double b_i=x+1.0-a;
  double c=1.0/vnl_very_small;
  double d=1.0/b_i;
  double cf=d;
  for (int i=1;i<=MAX_ITS;i++)
  {
    double a_i = i*(a-i);
    b_i += 2.0;
    d=a_i*d+b_i;
    if (std::fabs(d) < vnl_very_small) d=vnl_very_small;
    c=b_i+a_i/c;
    if (std::fabs(c) < vnl_very_small) c=vnl_very_small;
    d=1.0/d;
    double delta=d*c;
    cf *= delta;
    if (std::fabs(delta-1.0) < MaxRelError)
      return std::exp(-x+a*std::log(x)-vnl_log_gamma(a))*cf;
  }

  std::cerr<<"vnl_gamma_cont_frac : Failed to converge in "<<MAX_ITS<<" steps\n"
          <<"a = "<<a<<"   x= "<<x<<std::endl;
  return std::exp(-x+a*std::log(x)-vnl_log_gamma(a))*cf;
}

double vnl_gamma_p(double a, double x)
{
  if (x < 0.0 || a <= 0.0)
    assert(!"vnl_gamma_p - Invalid arguments.");

  if (x < a+1.0)
    return vnl_gamma_series(a,x); // Use series representation
  else
    return 1.0 - vnl_gamma_cont_frac(a,x); // Use continued fraction representation
}

double vnl_gamma_q(double a, double x)
{
  if (x < 0.0 || a <= 0.0)
    assert(!"vnl_gamma_q - Invalid arguments.");

  if (x < a+1.0)
    return 1.0-vnl_gamma_series(a,x); // Use series representation
  else
    return vnl_gamma_cont_frac(a,x); // Use continued fraction representation
}

double vnl_digamma(double z)
{
  double t0 = (z-0.5)/(z+4.65)-1.0;
  double tlg = std::log(4.65+z);
  double tc = 2.50662827563479526904;
  double t1 = 225.525584619175212544/z;
  double t2 = -268.295973841304927459/(1+z);
  double t3 = +80.9030806934622512966/(2+z);
  double t4 = -5.00757863970517583837/(3+z);
  double t5 = 0.0114684895434781459556/(4+z);
  double neu = t1/z + t2/(1+z) + t3/(2+z) + t4/(3+z) + t5/(4+z);
  double den = tc + t1 + t2 + t3 + t4 + t5;
  return t0 -(neu/den) + tlg;
}
