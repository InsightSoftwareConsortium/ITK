// This is core/vnl/algo/tests/test_fft1d.cxx
#include <cstdlib>
#include <cmath>
#include <iostream>
#include <complex>
#include <vector>
#include <testlib/testlib_test.h>
//:
// \file
// \brief test program for 1D FFT routines.
// \author Veit U.B. Schenk, Oxford RRG.
// \date 20 Mar 1998
//
// Creates 1D arrays and vectors, computes forward fft, then backward fft
// for all (where applicable) constructors of the class
// and computes differences between input and output.
//
// \verbatim
//  Modifications
//   Jan. 2002 - Peter Vanroose - adapted from vnl_fft1d to vnl_fft_1d
//   June 2003 - Peter Vanroose - added tests for the std::vector interface
// \endverbatim

//-----------------------------------------------------------------------------
#include <vcl_compiler.h>
#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_fft_1d.h>

void test_fft_1d(int n)
{
  std::cout << "=================================\n"
           << "Testing vnl_fft_1d for length " << n << '\n'
           << "=================================\n";

  // calculate prime factors for this size array
  //============================================
  vnl_fft_prime_factors<double> oPFx(n);
  if (!oPFx) {
    std::cerr << "cannot decompose X-size " << n << ")into the form (2^P)(3^Q)(5^R)\n";
    std::abort();
  }

  // create a number of arrays for testing the transform
  //====================================================
  vnl_vector<std::complex<double> > fTestArrayConvert(n);
  vnl_vector<std::complex<double> > fTestArrayFwd(n);
  std::vector<std::complex<double> > fTestVecConvert(n);
  std::vector<std::complex<double> > fTestVecFwd(n);
  std::complex<double>* fTestPtrConvert = new std::complex<double>[n];
  std::complex<double>* fTestPtrFwd = new std::complex<double>[n];

  //fill with data
  for (int iC = 0;iC < n;iC ++)
    fTestArrayConvert(iC) = fTestArrayFwd(iC) =
    fTestVecConvert[iC]   = fTestVecFwd[iC] =
    fTestPtrConvert[iC]   = fTestPtrFwd[iC] =
      std::complex<double>(iC-3.5,0.0);

  //============================= super-easy transform =====================
  vnl_fft_1d<double> oFFTSimple(n);
  oFFTSimple.transform(fTestArrayConvert, +1);
  oFFTSimple.fwd_transform(fTestArrayFwd);
  oFFTSimple.transform(fTestVecConvert, +1);
  oFFTSimple.fwd_transform(fTestVecFwd);
  oFFTSimple.transform(fTestPtrConvert, +1);
  oFFTSimple.fwd_transform(fTestPtrFwd);

  // now compare the results
  TEST("test forward vnl_vector", fTestArrayConvert, fTestArrayFwd);
  TEST("test forward std::vector", fTestVecConvert, fTestVecFwd);
  bool test_Ptr=true;
  for (int iC = 0;iC < n;iC ++)
    if (fTestPtrConvert[iC]!=fTestVecFwd[iC] ||
        fTestPtrFwd[iC]!=fTestVecConvert[iC]) { test_Ptr = false; break; }
  TEST("test forward C-array", test_Ptr, true);

  // the whole thing backwards
  //==========================
  oFFTSimple.transform(fTestArrayConvert, -1);
  oFFTSimple.bwd_transform(fTestArrayFwd);
  oFFTSimple.transform(fTestVecConvert, -1);
  oFFTSimple.bwd_transform(fTestVecFwd);
  oFFTSimple.transform(fTestPtrConvert, -1);
  oFFTSimple.bwd_transform(fTestPtrFwd);

  TEST("test backward vnl_vector", fTestArrayConvert, fTestArrayFwd);
  TEST("test backward std::vector", fTestVecConvert, fTestVecFwd);
  test_Ptr=true;
  for (int iC = 0;iC < n;iC ++)
    if (fTestPtrConvert[iC]!=fTestVecFwd[iC] ||
        fTestPtrFwd[iC]!=fTestVecConvert[iC]) {
      std::cout<<"C-array_fwd_bwd["<<iC<<"]="<<fTestPtrFwd[iC]
              <<", C-array_convert["<<iC<<"]="<<fTestPtrConvert[iC]
              <<", std::vector["<<iC<<"]="<<fTestVecFwd[iC]<<'\n';
      test_Ptr = false; break;
    }
  TEST("test backward C-array", test_Ptr, true);

  double fArrayRealError = 0.0, fArrayImagError = 0.0,
         fVecRealError = 0.0,   fVecImagError = 0.0,
         fPtrRealError = 0.0,   fPtrImagError = 0.0,
         fFwdRealError = 0.0,   fFwdImagError = 0.0;

  for (int iC = 0;iC < n;iC ++)
  {
    // divide by n (since by definition fft_bwd(a) == n*....)
    fArrayRealError += std::fabs(std::real(fTestArrayConvert(iC))/n - (iC-3.5));
    fArrayImagError += std::fabs(std::imag(fTestArrayConvert(iC))/n);
    fVecRealError += std::fabs(std::real(fTestVecConvert[iC])/n - (iC-3.5));
    fVecImagError += std::fabs(std::imag(fTestVecConvert[iC])/n);
    fPtrRealError += std::fabs(std::real(fTestPtrConvert[iC])/n - (iC-3.5));
    fPtrImagError += std::fabs(std::imag(fTestPtrConvert[iC])/n);
    fFwdRealError += std::fabs(std::real(fTestPtrFwd[iC])/n - (iC-3.5));
    fFwdImagError += std::fabs(std::imag(fTestPtrFwd[iC])/n);
  }

  TEST_NEAR("vnl_vector absolute error, real part (per element)", fArrayRealError/n, 0.0, 1e-9);
  TEST_NEAR("vnl_vector absolute error, imag part (per element)", fArrayImagError/n, 0.0, 1e-9);
  TEST_NEAR("std::vector absolute error, real part (per element)", fVecRealError/n, 0.0, 1e-9);
  TEST_NEAR("std::vector absolute error, imag part (per element)", fVecImagError/n, 0.0, 1e-9);
  TEST_NEAR("C-array absolute error, real part (per element)", fPtrRealError/n, 0.0, 1e-9);
  TEST_NEAR("C-array absolute error, imag part (per element)", fPtrImagError/n, 0.0, 1e-9);
  TEST_NEAR("C-array fwd absolute error, real part (per element)", fFwdRealError/n, 0.0, 1e-9);
  TEST_NEAR("C-array fwd absolute error, imag part (per element)", fFwdImagError/n, 0.0, 1e-9);

  delete[] fTestPtrConvert;
  delete[] fTestPtrFwd;
}

void test_fft1d()
{
  test_fft_1d(256);
  test_fft_1d(243);
  test_fft_1d(625);

  test_fft_1d(1);
  test_fft_1d(2);
  test_fft_1d(3);
  test_fft_1d(4);
  test_fft_1d(5);
  test_fft_1d(6);
  test_fft_1d(8);
  test_fft_1d(9);
  test_fft_1d(10);
  test_fft_1d(16);
  test_fft_1d(32);
  test_fft_1d(64);
  test_fft_1d(128);

  test_fft_1d(200);
  test_fft_1d(216);
  test_fft_1d(225);
  test_fft_1d(240);
  test_fft_1d(250);
  test_fft_1d(270);
  test_fft_1d(288);

  test_fft_1d(10000);
  test_fft_1d(65536);
}

TESTMAIN(test_fft1d);
