// This is vxl/vnl/vnl_sample.cxx

/*
  fsm@robots.ox.ac.uk
*/
#include "vnl_sample.h"

#include <vcl_cmath.h>
#include <vcl_cstdlib.h>
#include <vxl_config.h>
#if VXL_STDLIB_HAS_DRAND48
// On SunPro 5.0, <cstdlib> does not
// declare drand48() but <stdlib.h> does.
# include <stdlib.h> // dont_vxl_filter
#include <sys/types.h>
#include <time.h>
#endif


#if !VXL_STDLIB_HAS_DRAND48
// rand() is not always a good random number generator,
// so use the following congruential random number generator - PVr
static unsigned long vnl_sample_seed = 12345;
#endif

# include <vcl_ctime.h>


void vnl_sample_reseed()
{
# if VXL_STDLIB_HAS_DRAND48
  srand48( vcl_time(0) );
# else
  vnl_sample_seed = vcl_time(0);
# endif
}

//: return a random number uniformly drawn on [a, b)
double vnl_sample_uniform(double a, double b) 
{
#if VXL_STDLIB_HAS_DRAND48
  // it's your lucky day.
  double u = drand48(); // uniform on [0, 1)
#else
  // unlucky! perhaps your luck will change if you try
  // a different random number generator.
  vnl_sample_seed = (vnl_sample_seed*16807)%2147483647L;
  double u = double(vnl_sample_seed)/2147483711L;
#endif
  return (1.0 - u)*a + u*b;
}

void vnl_sample_normal_2(double *x, double *y)
{
  double u     = vnl_sample_uniform(0, 1);
  double theta = vnl_sample_uniform(0, 2 * /* pi = */3.14159265358979);
  
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
