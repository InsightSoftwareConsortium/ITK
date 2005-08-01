#ifndef VNL_DEFINITE_INTEGRAL_H_
#define VNL_DEFINITE_INTEGRAL_H_
//:
// \file
// \author Kongbin Kang at Brown
// \date Jan 12, 2005
// \brief the abstract 1D integrant function used for definite integral

#include "vnl_integrant_fnct.h"

class vnl_definite_integral
{
  protected:

    static vnl_integrant_fnct *pfnct_;
    
  public:

    vnl_definite_integral() { pfnct_ = 0; }

    void set_fnct(vnl_integrant_fnct* f) { pfnct_ = f; }

#if 0
    //: integration from a to b, in n steps
    virtual double integral(vnl_integrant_fnct *f, float a, float b, int n)=0;
#endif 

    //: dector
    virtual ~vnl_definite_integral() { pfnct_ = 0; }
};

#endif
