#include "v3p_netlib.h"

#include <stdio.h>

double f(double *x)
{
  return (*x)/(1+(*x)*(*x));
}

void test_simpson_integral()
{
  double a = 0;
  double b = 1;
  double res;
  long n = 100;

  v3p_netlib_simpru_(&f, &a, &b, &n, &res);
  printf("simpson integral of x/(1+x^2) from 0 to 1 (%ld grids) is %2.10f\n", n, res);
}

void test_trapezod_integral()
{
  double a = 0;
  double b = 1;
  double res;
  long n = 500;

  v3p_netlib_trapru_(&f, &a, &b, &n, &res);
  printf("trapezod integral of x/(1+x^2) from 0 to 1 (%ld grids) is %f\n", n, res);
}



int main()
{
  test_simpson_integral();
  test_trapezod_integral();
  return 0;
}
