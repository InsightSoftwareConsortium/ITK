// This is core/vnl/vnl_sample.cxx

#include <cmath>
#include <ctime>
#include "vnl_sample.h"
#include <vnl/vnl_math.h>
#include "vnl_drand48.h"

void vnl_sample_reseed()
{
  vnl_srand48( std::time(0) );
}

void vnl_sample_reseed(int seed)
{
  vnl_srand48( seed );
}

double vnl_sample_uniform(double a, double b)
{
  double u = vnl_drand48(); // uniform on [0, 1)
  return (1.0 - u)*a + u*b;
}

double vnl_sample_uniform01()
{
  return vnl_sample_uniform(0.0, 1.0); // uniform on [0, 1)
}

void vnl_sample_normal_2(double *x, double *y)
{
  double u     = vnl_sample_uniform(1, 0); // not (0,1): should not return 0
  double theta = vnl_sample_uniform(0, vnl_math::twopi);

  double r = std::sqrt(-2*std::log(u));

  if (x) *x = r * std::cos(theta);
  if (y) *y = r * std::sin(theta);
}

double vnl_sample_normal(double mean, double sigma)
{
  double x;
  vnl_sample_normal_2(&x, nullptr);
  return mean + sigma * x;
}

// Implementation of Bernoulli sampling by Peter Vanroose
int vnl_sample_bernoulli(double q)
{
  // quick return if possible:
  if (q==0.0) return 0;
  if (q==1.0) return 1;
  if (q<0.0 || q>1.0) return -1;
  // q should be the probability of returning 0:
  return (vnl_sample_uniform(0.0, 1.0/q) >= 1.0) ? 1 : 0;
}

// Implementation of binomial sampling by Peter Vanroose
int vnl_sample_binomial(int n, double q)
{
  // Returns a random "k" value, between 0 and n, viz. the sum of n random
  // and independent drawings from a Bernoulli distribution with parameter q.

  if (n <= 0 || q<0.0 || q>1.0) return -1; // That is: when the input makes no sense, return nonsense "-1".
  if (q==0.0) return 0;
  if (q==1.0) return n;
  int k = 0;
  for (int i=n-1; i>=0; --i) {
    k += vnl_sample_bernoulli(q);
  }
  return k;
}
