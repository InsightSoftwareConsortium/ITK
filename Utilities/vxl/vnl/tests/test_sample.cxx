/*
  fsm
*/
#include <vcl_iostream.h>
#include <testlib/testlib_test.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_sample.h>

void test_sample()
{
  unsigned const N = 100000;
  double mu = 1.552;
  double sigma = 3.729;

  double* X = new double[N];
  for (unsigned i=0; i<N; ++i)
    X[i] = vnl_sample_normal(mu, sigma);

  // sample mean
  double X_bar = 0;
  for (unsigned i=0; i<N; ++i)
    X_bar += X[i];
  X_bar /= N;
  vcl_cout << "actual mean : " << mu << vcl_endl;
  vcl_cout << "sample mean : " << X_bar << vcl_endl;
  TEST("sample mean", X_bar-mu < 0.1 && mu-X_bar < 0.1, true);

  // sample standard deviation
  double sigma_bar = 0;
  for (unsigned i=0; i<N; ++i)
    sigma_bar += vnl_math_sqr(X[i] - X_bar);
  sigma_bar = vcl_sqrt(sigma_bar / (N-1));
  vcl_cout << "actual standard deviation : " << sigma << vcl_endl;
  vcl_cout << "sample standard deviation : " << sigma_bar << vcl_endl;
  TEST("sample stddev", sigma_bar-sigma < 0.1 && sigma-sigma_bar < 0.1, true);

  const int seed = 7;
  vnl_sample_reseed (seed);
  double nval0 = vnl_sample_normal (0.0, 1.0);
  double uval0 = vnl_sample_uniform (0.0, 1.0);
  vnl_sample_reseed (seed);
  double nval1 = vnl_sample_normal (0.0, 1.0);
  double uval1 = vnl_sample_uniform (0.0, 1.0);
  vcl_cout << "repeat normal: " << nval0 << " " << nval1 << vcl_endl;
  vcl_cout << "repeat uniform: " << uval0 << " " << uval1 << vcl_endl;
  TEST("seed repeat", nval0 == nval1 && uval0 == uval1, true);
  delete [] X;
}

TESTMAIN(test_sample);
