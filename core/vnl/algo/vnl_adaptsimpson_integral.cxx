#include <iostream>
#include <cmath>
#include "vnl_adaptsimpson_integral.h"

double
vnl_adaptsimpson_integral::int_fnct_(double * x)
{
  return pfnct_->f_(*x);
}

double
vnl_adaptsimpson_integral::integral(vnl_integrant_fnct * f, double a, double b, double acury)
{
  // set the function
  pfnct_ = f;

  return adaptivesimpson(&vnl_adaptsimpson_integral::int_fnct_, a, b, acury, 0, depth_);
}

double
vnl_adaptsimpson_integral::adaptivesimpson(double (*f)(double *),
                                           double a,
                                           double b,
                                           double eps,
                                           int level,
                                           int level_max)
{
  const double h = b - a;
  double c = 0.5 * (a + b);
  const double one_simpson = h * (f(&a) + 4.0 * f(&c) + f(&b)) / 6.0;
  double d = 0.5 * (a + c);
  double e = 0.5 * (c + b);
  const double two_simpson = h * (f(&a) + 4.0 * f(&d) + 2.0 * f(&c) + 4.0 * f(&e) + f(&b)) / 12.0;
  /* Check for level */
  double result;
  if (level + 1 >= level_max)
  {
    result = two_simpson;
    std::cerr << "Maximum level reached\n";
  }
  else
  {
    /* Check for desired accuracy */
    if (std::fabs(two_simpson - one_simpson) < 15.0 * eps)
      result = two_simpson + (two_simpson - one_simpson) / 15.0;
    /* Divide further */
    else
    {
      const double left_simpson = adaptivesimpson(f, a, c, eps / 2.0, level + 1, level_max);
      const double right_simpson = adaptivesimpson(f, c, b, eps / 2.0, level + 1, level_max);
      result = left_simpson + right_simpson;
    }
  }
  return result;
}
