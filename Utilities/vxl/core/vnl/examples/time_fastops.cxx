// This is core/vnl/examples/time_fastops.cxx
#include <vcl_iostream.h>
#include <vcl_cmath.h> // for vcl_sqrt()
#include <vcl_vector.h>
#include <vul/vul_timer.h>

double vnl_fastops_dot(const double* a, const double* b, unsigned int n);

#ifdef OPTIMIZED
#undef OPTIMIZED
#define OPTIMIZED 1
#else
#define OPTIMIZED 0
#endif
#ifndef METHOD
#define METHOD 4
#endif

int main()
{
  vcl_vector<double> x(1000000), y(1000000);
  for (int i = 0; i < 1000000; ++i)
    x[i] = y[i] = 1.0/vcl_sqrt(double(i+1));

  vul_timer t;
  for (int n = 0; n < 20; ++n)
    vnl_fastops_dot(&x[0], &y[0], x.size());
  vcl_cerr << "Method = " << METHOD << ", Optimized = " << OPTIMIZED << ", "
           << "Result = " << vnl_fastops_dot(&x[0], &y[0], x.size()) << ", ";
  t.print(vcl_cerr);

  return 0;
}

double vnl_fastops_dot(const double* a, const double* b, unsigned int n)
{
  // Method 2 is fastest on the u170 -- weird.
  double accum = 0;
#if METHOD == 1
  const double* aend = a + n;
  while (a != aend)
    accum += *a++ * *b++;
#endif
#if METHOD == 2
  for (unsigned int k = 0; k < n; ++k)
    accum += a[k] * b[k];
#endif
#if METHOD == 3
  while (n--)
    accum += a[n] * b[n];
#endif
#if METHOD == 4
  unsigned int k = n;
  while (k > 0)
    --k, accum += a[k] * b[k];
#endif
  return accum;
}
