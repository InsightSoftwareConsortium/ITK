#ifndef VNL_SIMPSON_INTEGRAL_H_
#define VNL_SIMPSON_INTEGRAL_H_
//:
// \file
// \author Kongbin Kang at Brown
// \date   Jan. 17th, 2005
//
#include <vnl/vnl_definite_integral.h>

class vnl_simpson_integral : public vnl_definite_integral
{
 private:
  //: used to extract integrant functions of the vnl_integrant_fnct.
  static double int_fnct_(double* x);

 public:

  vnl_simpson_integral() {}

  //: a and b are integral limits respectively.
  // n is the number of intervals used in integral.
  // The actual subinterval used is 2* num_intervals_
  double integral(vnl_integrant_fnct *f, double a, double b, long n);
};

#endif
