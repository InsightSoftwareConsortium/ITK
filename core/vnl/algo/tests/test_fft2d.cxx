// This is core/vnl/algo/tests/test_fft2d.cxx
#include <testlib/testlib_test.h>
//:
// \file
// \brief test program for 2D FFT routines.
// \author Veit U.B. Schenk, Oxford RRG.
// \date 20 Mar 1998
//
// Creates 2D arrays and matrices, computes forward fft, then backward fft
// for all (where applicable) constructors of the class
// and computes differences between input and output.
//
// \verbatim
//  Modifications
//   Jan. 2002 - Peter Vanroose - adapted from vnl_fft2d to vnl_fft_2d
// \endverbatim

//-----------------------------------------------------------------------------
#include <vcl_cstdlib.h>
#include <vcl_iostream.h>
#include <vcl_complex.h>

#include <vnl/vnl_complexify.h>
#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_fft_2d.h>

inline static double function(unsigned i, unsigned j) { return i * j; }

void test_cplx(vnl_fft_prime_factors<double> const &/*prx*/,
               vnl_fft_prime_factors<double> const &/*pry*/,
               vnl_matrix<vcl_complex<double> > const &M,
               int dir)
{
  vnl_matrix<vcl_complex<double> > fft_matrix = M;
  vnl_fft_2d<double> fft(M.rows(), M.cols()); fft.transform(fft_matrix, dir);
  TEST("test rows", fft.rows(), M.rows());
  TEST("test cols", fft.cols(), M.cols());
  TEST("test transform", fft_matrix == M, false);
}

void test_fft2d ()
{
  const unsigned int rows = 64;
  const unsigned int cols = 64;

  // calculate prime factors for this size array
  vnl_fft_prime_factors<double> prx (rows);
  vnl_fft_prime_factors<double> pry (cols);

  if (!prx) {
    vcl_cerr << "cannot decompose X-size " << rows << ") into the form (2^P)(3^Q)(5^R)\n";
    vcl_abort();
  }
  if (!pry) {
    vcl_cerr << "cannot decompose Y-size (" << cols << ") into the form (2^P)(3^Q)(5^R)\n";
    vcl_abort();
  }

  // create arrays for testing the transform

  // data as arrays :
  double real_array[cols*rows];
  double imag_array[cols*rows];

  // fill with stuff :
  for (unsigned i=0; i<rows; ++i) {
    for (unsigned j=0; j<cols; ++j) {
      real_array[i*cols + j] = function(i, j);
      imag_array[i*cols + j] = 0.0;
    }
  }

  // complexify :
  vcl_complex<double> cplx_array[rows*cols];
  vnl_complexify(real_array, imag_array, cplx_array, rows*cols);

  // data as matrices :
  vnl_matrix<vcl_complex<double> > cplx_matrix(cplx_array, rows,cols);

  //--------------------------------------------------------------------------------

  test_cplx(prx, pry, cplx_matrix, +1);
  test_cplx(prx, pry, cplx_matrix, -1);

  //--------------------------------------------------------------------------------

  // verify that backwards * forwards is multiplication by .size().

  vnl_matrix<vcl_complex<double> > fft_matrix = cplx_matrix;
  vnl_fft_2d<double> fft(cplx_matrix.rows(), cplx_matrix.cols());
  fft.fwd_transform(fft_matrix);
  fft.bwd_transform(fft_matrix);

  double error = (fft_matrix - vcl_complex<double>(cplx_matrix.size())*cplx_matrix).fro_norm();
  vcl_cout << "error = " << error << vcl_endl;
  TEST_NEAR("fwd-bwd error", error, 0.0, 1e-7); // increase for float
}

TESTMAIN (test_fft2d);
