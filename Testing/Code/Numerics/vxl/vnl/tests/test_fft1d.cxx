/* -*- c++ -*-
 * test program for 1D FFT routines
 * creates 1D arrays and vectors, computes forward fft, then backward fft 
 * for all (where applicable) constructors of the class
 * and computes differences between input and output.
 ************************************************************/
//
// Class: testvnl_fft2d
// Author: Veit U.B. Schenk, Oxford RRG
// Created: 20 Mar 1998
//
//-----------------------------------------------------------------------------
#include <vcl_cstdlib.h> // for abort
#include <vcl_iostream.h>

#include <vnl/vnl_complex.h>
#include <vnl/vnl_test.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_complex_ops.h>
#include <vnl/algo/vnl_fft1d.h>
#include <vnl/algo/vnl_fftxd_prime_factors.h>

// What type to use for calculations (double or float).
// Believe it or not, the SunPro compiler will emit a differently mangled
// symbol if 'fsm_real' is a typedef. The result is a linker error.
#ifndef VCL_SUNPRO_CC
typedef double fsm_real;
#else
# define fsm_real double
#endif

const fsm_real maxRealErrorPrecision = 1e-5;
const fsm_real maxImagErrorPrecision = 1e-5;

void test_fft1d () {
  const int ciArraySizeX = 64;
  
  /*
   * calculate prime factors for this size array
   **************************************************/
  vnl_fftxd_prime_factors<fsm_real> oPFx (ciArraySizeX); 
  if (!oPFx) {
    vcl_cerr << "cannot decompose X-size " << ciArraySizeX << ")into the form (2^P)(3^Q)(5^R)\n";
    abort();
  }

  /*
   * create a number of arrays for testing the different constructors
   ************************************************************/

  vnl_vector<vnl_complex<fsm_real> > fTestArrayComplex(ciArraySizeX);
  vnl_vector<fsm_real> fTestArray(ciArraySizeX);
  vnl_vector<fsm_real> fZeroArray(ciArraySizeX,0.0); // imag == 0.0
  fsm_real *realArray = new fsm_real[ciArraySizeX];
  fsm_real *imagArray = new fsm_real[ciArraySizeX];
  vnl_complex<fsm_real> *complArray = new vnl_complex<fsm_real>[ciArraySizeX];
  
  //fill with data
  for (int iC = 0;iC < ciArraySizeX;iC ++) {
      fTestArrayComplex(iC) = vnl_complex<fsm_real> (iC,0.0);
      fTestArray(iC) = iC; // for complex default = (iC,0.0)
      realArray[iC] = iC;
      imagArray[iC] = 0.0;
      complArray[iC] = iC; // (default = complex<T>(r,0.0))
    }

  /****************************** super-easy constructors ***************/
  // simplest of all constructors: just a 'real' array
  vnl_fft1d<fsm_real> oFFTSimple (fTestArray);
  // second simplest: a complex array
  vnl_fft1d<fsm_real> oFFTSimpleComplex (fTestArrayComplex, +1);  
  // the following are the constructors that take twiddle factors

  /******************************* take matrices *******************************/
  // real matrix
  vnl_fft1d<fsm_real> oFFTRealMTwiddle(fTestArray, oPFx, +1);
  // real/imag matrices
  vnl_fft1d<fsm_real> oFFTRIMTwiddle (fTestArray, fZeroArray, oPFx, +1);
  // complex matrix
  vnl_fft1d<fsm_real> oFFTComplMTwiddle(fTestArrayComplex, oPFx, +1);
  /******************************* arrays of data *******************************/
  // real data
  vnl_fft1d<fsm_real> oFFTrealDTwiddle(realArray, ciArraySizeX, 
				      oPFx, +1);
  // real/imag data
  vnl_fft1d<fsm_real> oFFTimagDTwiddle(realArray, imagArray, 
				      ciArraySizeX,  
				      oPFx, +1);
  // complex data
  // awf had to dump this as I could not get vc60 to handle it
  //  vnl_fft1d<fsm_real> oFFTcomplDTwiddle(complArray, ciArraySizeX, oPFx, +1);

  // now compare all the results against oFFTSimple
  Assert ("test forward 1", oFFTSimple == oFFTSimpleComplex);  
  Assert ("test forward 2", oFFTSimple == oFFTRealMTwiddle);
  Assert ("test forward 3", oFFTSimple == oFFTRIMTwiddle);
  Assert ("test forward 4", oFFTSimple == oFFTComplMTwiddle);
  Assert ("test forward 5", oFFTSimple == oFFTrealDTwiddle);
  Assert ("test forward 6", oFFTSimple == oFFTimagDTwiddle);
  //awfasabove; Assert ("test forward 7", oFFTSimple == oFFTcomplDTwiddle);

  /*
   * the whole thing backwards
   **************************************************/
  vnl_vector<fsm_real> fBackRealMat = real(VCL_OVERLOAD_CAST(vnl_vector<vnl_complex<fsm_real> >&, oFFTSimple));
  vnl_vector<fsm_real> fBackImagMat = imag(VCL_OVERLOAD_CAST(vnl_vector<vnl_complex<fsm_real> >&, oFFTSimple));
  fsm_real *realBackArray = fBackRealMat.data_block ();
  fsm_real *imagBackArray = fBackImagMat.data_block ();
  vnl_complex<fsm_real> *complBackArray = oFFTSimple.data_block ();

  // second simplest: a complex array
  vnl_fft1d<fsm_real> oFFTBackSimpleComplex (oFFTSimple, -1);  
  // the following are the constructors that take twiddle factors

  /******************************* take matrices *******************************/
  // real/imag matrices
  vnl_fft1d<fsm_real> oFFTBackRIMTwiddle (fBackRealMat, fBackImagMat, 
					  oPFx, -1);
  // complex matrix
  vnl_fft1d<fsm_real> oFFTBackComplMTwiddle(oFFTSimple, oPFx, -1);
  /******************************* arrays of data *******************************/
  // real/imag data
  vnl_fft1d<fsm_real> oFFTBackimagDTwiddle(realBackArray, imagBackArray, 
					   ciArraySizeX, 
					   oPFx, -1);
  // complex data
  vnl_fft1d<fsm_real> oFFTBackcomplDTwiddle(complBackArray, ciArraySizeX, oPFx, -1);

  Assert ("test 1 back", oFFTBackSimpleComplex == oFFTBackRIMTwiddle);
  Assert ("test 2 back", oFFTBackSimpleComplex == oFFTBackComplMTwiddle);
  Assert ("test 3 back", oFFTBackSimpleComplex == oFFTBackimagDTwiddle);  
  Assert ("test 4 back", oFFTBackSimpleComplex == oFFTBackcomplDTwiddle);

  fsm_real fRealError = 0.0;
  fsm_real fImagError = 0.0;
  
  {
    for (int iC = 0;iC < ciArraySizeX;iC ++) {
      // divide by ciArraySizeX (since by def fft(a) == n*....)
      fRealError += fabs(oFFTBackSimpleComplex(iC).real ()/ciArraySizeX-iC);
      fImagError += fabs(oFFTBackSimpleComplex(iC).imag ()/ciArraySizeX);
    }
  }

  vcl_cout << "total real absolute error = " << fRealError << " (== " << fRealError/ciArraySizeX << " per element)\n";
  vcl_cout << "total imag absolute error = " << fImagError << " (== " << fImagError/ciArraySizeX << " per element)\n";
  Assert ("real error", fRealError/ciArraySizeX < maxRealErrorPrecision);
  Assert ("imag error", fImagError/ciArraySizeX < maxImagErrorPrecision);
}

TESTMAIN (test_fft1d);
