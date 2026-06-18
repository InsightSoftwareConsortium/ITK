#include "vnl_simpson_integral.h"

double
vnl_simpson_integral::int_fnct_(double * x)
{
  return pfnct_->f_(*x);
}

double
vnl_simpson_integral::integral(vnl_integrant_fnct * f, double a, double b, long n)
{
  // set the function
  pfnct_ = f;

  // Composite Simpson rule (Mathews Algorithm 7.2): n intervals, 2n subintervals.
  const double h = (b - a) / (2 * n);

  double sumeven = 0.0;
  for (long k = 1; k <= n - 1; ++k)
  {
    double x = a + h * 2 * k;
    sumeven += int_fnct_(&x);
  }

  double sumodd = 0.0;
  for (long k = 1; k <= n; ++k)
  {
    double x = a + h * (2 * k - 1);
    sumodd += int_fnct_(&x);
  }

  return h * (int_fnct_(&a) + int_fnct_(&b) + 2 * sumeven + 4 * sumodd) / 3;
}
