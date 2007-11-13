// This is core/vnl/vnl_sample.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author fsm
// \verbatim
//  Modifications
//   2007-03-26 Peter Vanroose - avoid returning log(0.0) by switching params
// \endverbatim

#include "vnl_sample.h"
#include <vnl/vnl_math.h>

#include <vcl_cmath.h>
#include <vxl_config.h>

#if VXL_STDLIB_HAS_DRAND48
# include <stdlib.h> // dont_vxl_filter
#else
// rand() is not always a good random number generator,
// so use a simple congruential random number generator when no drand48 - PVr
static unsigned long vnl_sample_seed = 12345;
#endif

# include <vcl_ctime.h>

void vnl_sample_reseed()
{
#if VXL_STDLIB_HAS_SRAND48
  srand48( vcl_time(0) );
#elif !VXL_STDLIB_HAS_DRAND48
  vnl_sample_seed = (unsigned long)vcl_time(0);
#endif
}

void vnl_sample_reseed(int seed)
{
#if VXL_STDLIB_HAS_SRAND48
  srand48( seed );
#elif !VXL_STDLIB_HAS_DRAND48
  vnl_sample_seed = seed;
#endif
}

//: return a random number uniformly drawn on [a, b)
double vnl_sample_uniform(double a, double b)
{
#if VXL_STDLIB_HAS_DRAND48
  double u = drand48(); // uniform on [0, 1)
#else
  vnl_sample_seed = (vnl_sample_seed*16807)%2147483647L;
  double u = double(vnl_sample_seed)/2147483647L; // uniform on [0, 1)
#endif
  return (1.0 - u)*a + u*b;
}

void vnl_sample_normal_2(double *x, double *y)
{
  double u     = vnl_sample_uniform(1, 0); // not (0,1): should not return 0
  double theta = vnl_sample_uniform(0, 2 * vnl_math::pi);

  double r = vcl_sqrt(-2*vcl_log(u));

  if (x) *x = r * vcl_cos(theta);
  if (y) *y = r * vcl_sin(theta);
}

double vnl_sample_normal(double mean, double sigma)
{
  double x;
  vnl_sample_normal_2(&x, 0);
  return mean + sigma * x;
}
