#ifndef VNL_SIMPSON_INTEGRAL_H_
#define VNL_SIMPSON_INTEGRAL_H_

#if __has_include(<itkConfigure.h>)
#  include <itkConfigure.h>
#  if defined(ITK_FUTURE_LEGACY_REMOVE)
#    error "vnl_simpson_integral was removed; supply a composite Simpson rule (Mathews Algorithm 7.2) directly."
#  elif defined(ITK_LEGACY_REMOVE) && !defined(ITK_LEGACY_SILENT) && !defined(ITK_LEGACY_TEST)
#    if defined(_MSC_VER)
#      pragma message("vnl_simpson_integral is deprecated; supply a composite Simpson rule directly.")
#    else
#      warning "vnl_simpson_integral is deprecated; supply a composite Simpson rule directly."
#    endif
#  endif
#endif
//:
// \file
// \author Kongbin Kang at Brown
// \date   Jan. 17th, 2005
//
#include <vnl/vnl_definite_integral.h>
#include <vnl/algo/vnl_algo_export.h>

class VNL_ALGO_EXPORT vnl_simpson_integral : public vnl_definite_integral
{
private:
  //: used to extract integrand functions of the vnl_integrant_fnct.
  static double
  int_fnct_(double * x);

public:
  vnl_simpson_integral() = default;

  //: a and b are integral limits respectively.
  // n is the number of intervals used in integral.
  // The actual subinterval used is 2* num_intervals_
  double
  integral(vnl_integrant_fnct * f, double a, double b, long n);
};

#endif
