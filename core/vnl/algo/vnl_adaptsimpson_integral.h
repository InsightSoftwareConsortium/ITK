#ifndef VNL_ADAPTSIMPSON_INTEGRAL_H_
#define VNL_ADAPTSIMPSON_INTEGRAL_H_
//:
// \file
// \author Kongbin Kang at Brown
// \date   Jan. 17th, 2005
//
#include <vnl/vnl_definite_integral.h>
#include <vnl/algo/vnl_algo_export.h>

class VNL_ALGO_EXPORT vnl_adaptsimpson_integral : public vnl_definite_integral
{
 private:
  //: used to wrap the function class to an ordinary function.
  static double int_fnct_(double* x);

 protected:

  //: maximum recursion depth
  int depth_;

  //: real computation
  double  adaptivesimpson(double(*f)(double*), double a, double b, double eps, int level, int level_max);

 public:
  vnl_adaptsimpson_integral(int depth = 32) : depth_(depth) {}

  //: a and b are integral limits respectively.
  // n is the number of intervals used in integral.
  // accuracy is the accuracy you want to achieve. Normally accuracy > 1e-11)
  double integral(vnl_integrant_fnct *f, double a, double b, double accuracy);
};

#endif
