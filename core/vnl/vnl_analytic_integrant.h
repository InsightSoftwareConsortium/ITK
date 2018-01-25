#ifndef VNL_ANALYTIC_INTEGRANT_
#define VNL_ANALYTIC_INTEGRANT_
// :
// \author Kongbin Kang
// \date Jan 13, 2005
// \brief a class to represent an analytic integrand

#include "vnl_integrant_fnct.h"
#include "vnl/vnl_export.h"

class VNL_TEMPLATE_EXPORT vnl_analytic_integrant : public vnl_integrant_fnct
{
 public:
  vnl_analytic_integrant() {}
  ~vnl_analytic_integrant() {}

  // the function every derived class has to implement, which is to evaluate
  // the function value at point x
  virtual double f_(double /*x*/) = 0;
};

#endif
