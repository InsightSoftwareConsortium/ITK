#ifndef VNL_DEFINITE_INTEGRAL_H_
#define VNL_DEFINITE_INTEGRAL_H_
//:
// \file
// \author Kongbin Kang at Brown
// \date Jan 12, 2005
// \brief the abstract 1D integrand function used for definite integral

#include "vnl_integrant_fnct.h"
#include "vnl/vnl_export.h"
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

class VNL_EXPORT vnl_definite_integral
{
 protected:
  static vnl_integrant_fnct *pfnct_;

 public:
  vnl_definite_integral() { pfnct_ = nullptr; }

  void set_fnct(vnl_integrant_fnct* f) { pfnct_ = f; }

  // destructor
  virtual ~vnl_definite_integral() { pfnct_ = nullptr; }
};

#endif
