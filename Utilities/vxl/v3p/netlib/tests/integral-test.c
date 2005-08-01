#include "../f2c.h"
#include "../netlib.h"
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
  int n = 100;

  simpru_(&f, &a, &b, &n, &res);
  printf("simpson integral of x/(1+x^2) from 0 to 1 (%d grids) is %2.10f\n", n, res);
}

void test_adapted_simpson_integral()
{
  double a = 0;
  double b = 1;
  double res;
  int n = 100;
  double rmat[1111];
  double tol = 1e-10;
  double errbound;
  int stat;

  adaptquad_(&f, &a, &b, &tol, rmat, &res, &errbound, &n, &stat);
  printf("adapted simpson integral (with tol=%g) of x/(1+x^2) from 0 to 1 (%d grids) is %2.10f\n", tol, n,  res);
  printf("errbound is %f, state is %d\n", errbound,  stat);
}

void test_trapezod_integral()
{
  double a = 0;
  double b = 1;
  double res;
  int n = 500;

  trapru_(&f, &a, &b, &n, &res);
  printf("trapezod integral of x/(1+x^2) from 0 to 1 (%d grids) is %f\n", n, res);
}



int main()
{
  test_simpson_integral();
  test_adapted_simpson_integral();
  test_trapezod_integral();
  return 0;
}
