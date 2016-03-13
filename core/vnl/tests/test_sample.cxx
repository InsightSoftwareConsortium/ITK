#include <iostream>
#include <vcl_compiler.h>
#include <testlib/testlib_test.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_sample.h>
#include <vul/vul_get_timestamp.h> // to generate a random number

static double eps = 0.02;

static void test_sample_uniform()
{
  std::cout << "*************** sample uniform ***************\n";
  unsigned const N = 100000;
  double a = 123.456;
  double b = 543.210;
  vnl_sample_reseed(); // initialise the random seed in a random way

  double X[N];
  for (unsigned i=0; i<N; ++i)
    X[i] = vnl_sample_uniform(a, b);

  // sample mean
  double X_bar = 0;
  for (unsigned i=0; i<N; ++i)
    X_bar += X[i];
  X_bar /= N;
  TEST_NEAR("sample mean", X_bar, (a+b)*0.5, eps*a);

  // sample standard deviation
  double sigma_bar = 0;
  for (unsigned i=0; i<N; ++i)
    sigma_bar += vnl_math::sqr(X[i] - X_bar);
  sigma_bar = std::sqrt(sigma_bar / (N-1));
  TEST_NEAR("sample stddev", sigma_bar, (b-a)/std::sqrt(12.0), eps*a);

  int seed; vul_get_timestamp(seed,seed);
  std::cout << "seed is " << seed << std::endl;
  vnl_sample_reseed(seed);
  double uval0 = vnl_sample_uniform(0.0, 1.0);
  vnl_sample_reseed(seed);
  double uval1 = vnl_sample_uniform(0.0, 1.0);
  TEST_NEAR("seed repeat uniform", uval0, uval1, 0);

  // And now in one go, with the handy "range filling" sampler:
  vnl_sample_uniform(X, X+N, a, b);
  X_bar=0; for (unsigned i=0; i<N; ++i) X_bar += X[i]; X_bar /= N;
  TEST_NEAR("sample mean", X_bar, (a+b)*0.5, eps*a);
  sigma_bar=0; for (unsigned i=0; i<N; ++i) sigma_bar += vnl_math::sqr(X[i] - X_bar);
  TEST_NEAR("sample stddev", std::sqrt(sigma_bar/(N-1)), (b-a)/std::sqrt(12.0), eps*a);
}

static void test_sample_normal()
{
  std::cout << "*************** sample normal ***************\n";
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
  TEST_NEAR("sample mean", X_bar, mu, eps*sigma);

  // sample standard deviation
  double sigma_bar = 0;
  for (unsigned i=0; i<N; ++i)
    sigma_bar += vnl_math::sqr(X[i] - X_bar);
  sigma_bar = std::sqrt(sigma_bar / (N-1));
  TEST_NEAR("sample stddev", sigma_bar, sigma, eps*sigma);

  int seed; vul_get_timestamp(seed,seed);
  std::cout << "seed is " << seed << std::endl;
  vnl_sample_reseed(seed);
  double nval0 = vnl_sample_normal(0.0, 1.0);
  vnl_sample_reseed(seed);
  double nval1 = vnl_sample_normal(0.0, 1.0);
  TEST_NEAR("seed repeat normal", nval0, nval1, 0);

  // And now in one go, with the handy "range filling" sampler:
  vnl_sample_normal(X, X+N, mu, sigma);
  X_bar=0; for (unsigned i=0; i<N; ++i) X_bar += X[i]; X_bar /= N;
  TEST_NEAR("sample mean", X_bar, mu, eps*sigma);
  sigma_bar=0; for (unsigned i=0; i<N; ++i) sigma_bar += vnl_math::sqr(X[i] - X_bar);
  TEST_NEAR("sample stddev", std::sqrt(sigma_bar/(N-1)), sigma, eps*sigma);
}

static void test_sample_binomial()
{
  std::cout << "*************** sample binomial ***************\n";
  unsigned const N = 100000;
  int n = 10;
  double p = 0.4;
  vnl_sample_reseed(); // initialise the random seed in a random way

  int X[N];
  for (unsigned i=0; i<N; ++i)
    X[i] = vnl_sample_binomial(n, p);

  // sample mean
  double X_bar = 0;
  for (unsigned i=0; i<N; ++i)
    X_bar += X[i];
  X_bar /= N;
  TEST_NEAR("sample mean", X_bar, n*(1-p), eps*n);

  // sample standard deviation
  double sigma_bar_sqr = 0;
  for (unsigned i=0; i<N; ++i)
    sigma_bar_sqr += vnl_math::sqr(X[i] - X_bar);
  sigma_bar_sqr /= N-1;
  TEST_NEAR("sample stddev squared", sigma_bar_sqr, p*(1-p)*n, eps*n);

  // And now in one go, with the handy "range filling" sampler:
  vnl_sample_binomial(X, X+N, n, p);
  X_bar=0; for (unsigned i=0; i<N; ++i) X_bar += X[i]; X_bar /= N;
  TEST_NEAR("sample mean", X_bar, n*(1-p), eps*n);
  sigma_bar_sqr=0; for (unsigned i=0; i<N; ++i) sigma_bar_sqr += vnl_math::sqr(X[i] - X_bar);
  TEST_NEAR("sample stddev squared", sigma_bar_sqr /= N-1, p*(1-p)*n, eps*n);
}

static void test_sample_bernoulli()
{
  std::cout << "*************** sample Bernoulli ***************\n";
  unsigned const N = 100000;
  double p = 0.7;
  vnl_sample_reseed(); // initialise the random seed in a random way

  int X[N];
  for (unsigned i=0; i<N; ++i)
    X[i] = vnl_sample_bernoulli(p);

  // sample mean
  double X_bar = 0;
  for (unsigned i=0; i<N; ++i)
    X_bar += X[i];
  X_bar /= N;
  TEST_NEAR("sample mean", X_bar, 1-p, eps);

  // sample standard deviation
  double sigma_bar_sqr = 0;
  for (unsigned i=0; i<N; ++i)
    sigma_bar_sqr += vnl_math::sqr(X[i] - X_bar);
  sigma_bar_sqr /= N-1;
  TEST_NEAR("sample stddev squared", sigma_bar_sqr, p*(1-p), eps);

  // And now in one go, with the handy "range filling" sampler:
  vnl_sample_bernoulli(X, X+N, p);
  X_bar=0; for (unsigned i=0; i<N; ++i) X_bar += X[i]; X_bar /= N;
  TEST_NEAR("sample mean", X_bar, 1-p, eps);
  sigma_bar_sqr=0; for (unsigned i=0; i<N; ++i) sigma_bar_sqr += vnl_math::sqr(X[i] - X_bar);
  TEST_NEAR("sample stddev squared", sigma_bar_sqr /= N-1, p*(1-p), eps);
}

static void test_sample()
{
  test_sample_normal();
  test_sample_uniform();
  test_sample_binomial();
  test_sample_bernoulli();
}

TESTMAIN(test_sample);
