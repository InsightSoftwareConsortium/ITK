// This is core/vnl/algo/tests/test_fft.cxx
/*
  fsm
*/
#include <iostream>
#include <vcl_compiler.h>

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_matlab_print.h>
#include <vnl/vnl_random.h>

#include <vnl/algo/vnl_fft_1d.h>
#include <vnl/algo/vnl_fft_2d.h>

#include <testlib/testlib_test.h>

#include "test_util.h"

void test_fft_1d(unsigned int N)
{
  vnl_random rng;
  vnl_vector<std::complex<double> > signal(N);
  test_util_fill_random(signal.begin(), signal.end(), rng);

  vnl_fft_1d<double> fft(N);

  vnl_vector<std::complex<double> > tmp = signal;
  vnl_matlab_print(std::cout, tmp, "tmp");

  fft.fwd_transform(tmp);
  vnl_matlab_print(std::cout, tmp, "tmp");

  fft.bwd_transform(tmp); tmp /= N;
  vnl_matlab_print(std::cout, tmp, "tmp");

  double err = (tmp - signal).two_norm();
  std::cout << "err = " << err << std::endl;
  TEST_NEAR("test fwd-bwd", err, 0.0, 1e-10);
}

void test_fft_2d(unsigned int M, unsigned int N)
{
  vnl_random rng;
  vnl_matrix<std::complex<double> > signal(M, N);
  test_util_fill_random(signal.begin(), signal.end(), rng);

  vnl_fft_2d<double> fft(M, N);

  vnl_matrix<std::complex<double> > tmp = signal;
  vnl_matlab_print(std::cout, tmp, "tmp");

  fft.fwd_transform(tmp);
  vnl_matlab_print(std::cout, tmp, "tmp");

  fft.bwd_transform(tmp); tmp /= (M*N);
  vnl_matlab_print(std::cout, tmp, "tmp");

  double err = (tmp - signal).fro_norm();
  std::cout << "err = " << err << std::endl;
  TEST_NEAR("test fwd-bwd", err, 0.0, 1e-10);
}

void test_fft()
{
  test_fft_1d(24);
  test_fft_2d(25, 30);
}

TESTMAIN (test_fft);
