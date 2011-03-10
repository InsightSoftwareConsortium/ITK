// This is core/vnl/algo/vnl_gaussian_kernel_1d.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   07 Aug 1997
//
//-----------------------------------------------------------------------------

#include "vnl_gaussian_kernel_1d.h"
#include <vcl_cmath.h>
#include <vnl/vnl_math.h>

// G(x) = 1/(sigma * sqrt(2*pi)) * exp(-0.5 * (x/sigma)^2)
// x(g) = sigma * sqrt(-2 * log(g * sigma * sqrt(2*pi) ) )

// Compute the x value at which a Gaussian becomes lower than cutoff.
static inline
double compute_width(double sigma, double cutoff)
{
  return sigma * vcl_sqrt(-2 * vcl_log(cutoff * sigma * vcl_sqrt(2*vnl_math::pi)));
}

//: Construct a sampled 1D gaussian of standard deviation sigma.
// The vector is normalized so that its sum is 0.5.
vnl_gaussian_kernel_1d::vnl_gaussian_kernel_1d(double sigma, double cutoff):
  vec_((int)vcl_ceil(compute_width(sigma, cutoff)))
{
  int wid = vec_.size();
  inscale_ = 0.5/(sigma * sigma);
  double area = 0;
  for (int i = 0; i < wid; ++i) {
    double v = G(i);
    area += v;
    vec_[i] = v;
  }
  vec_ *= (0.5/area);
}

double vnl_gaussian_kernel_1d::G(double x) const
{
  return vcl_exp(-x*x * inscale_);
}
