/*
  fsm@robots.ox.ac.uk
*/
#include <vcl_iostream.h>
#include <vnl/vnl_test.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_sample.h>

void testvnl_sample()
{
  unsigned const N = 100000;
  double mu = 1.552;
  double sigma = 3.729;
  
  double X[N];
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
  sigma_bar = sqrt(sigma_bar / (N-1));
  vcl_cout << "actual standard deviation : " << sigma << vcl_endl;
  vcl_cout << "sample standard deviation : " << sigma_bar << vcl_endl;
  TEST("sample stddev", sigma_bar-sigma < 0.1 && sigma-sigma_bar < 0.1, true);
}

TESTMAIN(testvnl_sample);
