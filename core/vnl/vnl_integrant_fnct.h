#ifndef VNL_INTEGRANT_FNCT_H_
#define VNL_INTEGRANT_FNCT_H_
//:
// \file
// \author Kongbin Kang
// \date Jan 12, 2005
// \brief the abstract class of 1D integrant function used in integral 
// 

class vnl_integrant_fnct
{
 public:
  vnl_integrant_fnct() {}
  virtual ~vnl_integrant_fnct() {}

  virtual double f_(double /*x*/) = 0;
};

#endif
