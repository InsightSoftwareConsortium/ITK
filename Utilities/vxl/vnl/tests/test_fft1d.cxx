// This is vxl/vnl/tests/test_fft1d.cxx

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
// Modifications:
// Jan. 2002 - Peter Vanroose - adapted from vnl_fft1d to vnl_fft_1d
// \endverbatim

//-----------------------------------------------------------------------------
#include <vcl_cstdlib.h> // for vcl_abort
#include <vcl_cmath.h> // for vcl_fabs
#include <vcl_iostream.h>
#include <vcl_complex.h>

#include <testlib/testlib_test.h>
#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_fft_1d.h>

const double maxRealErrorPrecision = 1e-5;
const double maxImagErrorPrecision = 1e-5;

void test_fft1d () {
  const int ciArraySizeX = 64;

  // calculate prime factors for this size array
  //============================================
  vnl_fft_prime_factors<double> oPFx (ciArraySizeX);
  if (!oPFx) {
    vcl_cerr << "cannot decompose X-size " << ciArraySizeX << ")into the form (2^P)(3^Q)(5^R)\n";
    vcl_abort();
  }

  // create a number of arrays for testing the transform
  //====================================================
  vnl_vector<vcl_complex<double> > fTestArray(ciArraySizeX);
  vnl_vector<vcl_complex<double> > fTestArrayConvert(ciArraySizeX);
  vnl_vector<vcl_complex<double> > fTestArrayFwd(ciArraySizeX);

  //fill with data
  for (int iC = 0;iC < ciArraySizeX;iC ++) {
      fTestArray(iC) = fTestArrayConvert(iC) = fTestArrayFwd(iC)
                     = vcl_complex<double>(iC,0.0);
    }

  //============================= super-easy transform =====================
  vnl_fft_1d<double> oFFTSimple(ciArraySizeX);
  oFFTSimple.transform(fTestArrayConvert, +1);
  oFFTSimple.fwd_transform(fTestArrayFwd);

  // now compare the results
  testlib_test_assert ("test forward", fTestArrayConvert == fTestArrayFwd);

  // the whole thing backwards
  //==========================
  oFFTSimple.transform(fTestArrayConvert, -1);
  oFFTSimple.bwd_transform(fTestArrayFwd);

  testlib_test_assert ("test backward", fTestArrayConvert == fTestArrayFwd);

  double fRealError = 0.0;
  double fImagError = 0.0;

  for (int iC = 0;iC < ciArraySizeX;iC ++) {
    // divide by ciArraySizeX (since by def fft(a) == n*....)
    fRealError += vcl_fabs(vcl_real(fTestArrayConvert(iC))/ciArraySizeX-iC);
    fImagError += vcl_fabs(vcl_imag(fTestArrayConvert(iC))/ciArraySizeX);
  }

  vcl_cout << "total real absolute error = " << fRealError << " (== " << fRealError/ciArraySizeX << " per element)\n"
           << "total imag absolute error = " << fImagError << " (== " << fImagError/ciArraySizeX << " per element)\n";
  testlib_test_assert ("real error", fRealError/ciArraySizeX < maxRealErrorPrecision);
  testlib_test_assert ("imag error", fImagError/ciArraySizeX < maxImagErrorPrecision);
}

TESTMAIN (test_fft1d);
