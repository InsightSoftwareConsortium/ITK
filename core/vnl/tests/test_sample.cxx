#include <vcl_iostream.h>
#include <testlib/testlib_test.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_sample.h>
#include <vul/vul_get_timestamp.h> // to generate a random number

void test_sample()
{
  unsigned const N = 100000;
  double mu = 1.552;
  double sigma = 3.729;
  vnl_sample_reseed(); // initialise the random seed in a random way

  double X[N];
  for (unsigned i=0; i<N; ++i)
    X[i] = vnl_sample_normal(mu, sigma);

  // sample mean
  double X_bar = 0;
  for (unsigned i=0; i<N; ++i)
    X_bar += X[i];
  X_bar /= N;
  TEST_NEAR("sample mean", X_bar, mu, 0.1);

  // sample standard deviation
  double sigma_bar = 0;
  for (unsigned i=0; i<N; ++i)
    sigma_bar += vnl_math_sqr(X[i] - X_bar);
  sigma_bar = vcl_sqrt(sigma_bar / (N-1));
  TEST_NEAR("sample stddev", sigma_bar, sigma, 0.1);

  int seed; vul_get_timestamp(seed,seed);
  vcl_cout << "seed is " << seed << vcl_endl;
  vnl_sample_reseed(seed);
  double nval0 = vnl_sample_normal(0.0, 1.0);
  double uval0 = vnl_sample_uniform(0.0, 1.0);
  vnl_sample_reseed(seed);
  double nval1 = vnl_sample_normal(0.0, 1.0);
  double uval1 = vnl_sample_uniform(0.0, 1.0);
  TEST_NEAR("seed repeat normal", nval0, nval1, 0);
  TEST_NEAR("seed repeat uniform", uval0, uval1, 0);
}

TESTMAIN(test_sample);
