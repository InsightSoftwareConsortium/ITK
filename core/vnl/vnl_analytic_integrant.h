#ifndef VNL_ANALYTIC_INTEGRANT
#define VNL_ANALYTIC_INTEGRANT

// :
// \author Kongbin Kang
// \date Jan 13, 2005
// \brief a class to represent an analytic integrant

#include "vnl_integrant_fnct.h"

class vnl_analytic_integrant : public vnl_integrant_fnct 
{
 public:
  vnl_analytic_integrant() {}
  ~vnl_analytic_integrant() {}

  // the function every derived class has to implement, which is evalutate
  // the function value at point x
  virtual double f_(double /*x*/) = 0;
};

#endif
