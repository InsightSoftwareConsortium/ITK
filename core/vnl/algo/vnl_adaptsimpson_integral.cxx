#include "vnl_adaptsimpson_integral.h"
#include <vcl_iostream.h>
#include <vcl_cmath.h>

double vnl_adaptsimpson_integral::int_fnct_(double* x)
{
  return  pfnct_->f_(*x);
}

double vnl_adaptsimpson_integral::integral(vnl_integrant_fnct* f, double a,
                                           double b, double acury)
{
  //set the function
  pfnct_ = f;

  return adaptivesimpson(&vnl_adaptsimpson_integral::int_fnct_, a, b, acury, 0, deepth_);
}

double vnl_adaptsimpson_integral::adaptivesimpson(double(*f)(double*),
                       double a, double b, double eps, int level, int level_max)
{
  double c, d, e, h, result;
  double one_simpson, two_simpson;
  double left_simpson, right_simpson;

  h = b-a;
  c = 0.5*(a+b);
  one_simpson = h*(f(&a)+4.0*f(&c)+f(&b))/6.0;
  d = 0.5*(a+c);
  e = 0.5*(c+b);
  two_simpson = h*(f(&a)+4.0*f(&d)+2.0*f(&c)+4.0*f(&e)+f(&b))/12.0;
  /* Check for level */
  if (level+1 >= level_max) {
    result = two_simpson;
    vcl_cerr<< "Maximum level reached\n";
  }
  else {
    /* Check for desired accuracy */
    if (vcl_fabs(two_simpson-one_simpson) < 15.0*eps)
      result = two_simpson + (two_simpson-one_simpson)/15.0;
    /* Divide further */
    else {
      left_simpson = adaptivesimpson(f,a,c,eps/2.0,level+1,level_max);
      right_simpson = adaptivesimpson(f,c,b,eps/2.0,level+1,level_max);
      result =  left_simpson + right_simpson;
    }
  }
  return result;
}

