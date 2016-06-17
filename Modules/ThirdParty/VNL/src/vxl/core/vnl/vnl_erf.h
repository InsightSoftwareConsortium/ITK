// This is core/vnl/vnl_erf.h
#ifndef vnl_erf_h_
#define vnl_erf_h_
//:
// \file
// \brief Error Function (erf) approximations
// \author Tim Cootes, Ian Scott

#include <vnl/vnl_gamma.h>
#include <vnl/vnl_math.h>
#include "vnl/vnl_export.h"

//: The Error function.
// erf(x) = (2/sqrt(pi)) Integral from 0 to x (exp(-t^2) dt)
// \note the output ranges from -1 to 1, and vnl_erf(0) = 0.
inline double vnl_erf(double x)
{ return (x<0)?-vnl_gamma_p(0.5,x*x):vnl_gamma_p(0.5,x*x); }

//: The Complementary Error function.
// erfc(x) = 1 - erf(x) = 1 - (2/sqrt(pi)) Integral from 0 to x (exp(-t^2) dt)
// This value is useful for large x, when erf(x) ~= 1 and erfc(x) < eps.
// \note the output ranges from 0 to 2, and vnl_erfc(0) = 1.
VNL_EXPORT double vnl_erfc(double x);

//: The Scaled Complementary Error function.
// erfc_scaled(x) = exp(x^2) * erfc(x)
// This value is useful for very large x, where erf and erfc returns
// respectively ~1 and ~0.
// It can be approximated by (1/sqrt(pi)) * (1/x)
inline double vnl_scaled_erfc(double x)
{ return (vnl_math::two_over_sqrtpi/2.)*(1./x); }

#endif // vnl_erf_h_
