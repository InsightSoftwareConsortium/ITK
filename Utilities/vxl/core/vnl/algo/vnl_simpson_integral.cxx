#include "vnl_simpson_integral.h"
#include <vnl/algo/vnl_netlib.h>

double vnl_simpson_integral::int_fnct_(double* x)
{
  return  pfnct_->f_(*x);
}

double vnl_simpson_integral::integral(vnl_integrant_fnct* f, double a, double b, int n)
{

  double res = 0;
 
  //set the function
  pfnct_ = f;
  
  simpru_(&vnl_simpson_integral::int_fnct_, &a, &b, &n, &res);

  return res;
}
