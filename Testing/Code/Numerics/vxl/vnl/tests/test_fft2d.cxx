//:
// \file
// \brief test program for 2D FFT routines.
// \author Veit U.B. Schenk, Oxford RRG.
// \date 20 Mar 1998
// Creates 2D arrays and matrices, computes forward fft, then backward fft 
// for all (where applicable) constructors of the class
// and computes differences between input and output.
//

//-----------------------------------------------------------------------------
#include <vcl_cstdlib.h>
#include <vcl_iostream.h>
#include <vcl_cmath.h>
#include <vcl_complex.h>

#include <vnl/vnl_complex_ops.h>
#include <vnl/vnl_test.h>
#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_fft2d.h>
#include <vnl/algo/vnl_fftxd_prime_factors.h>

// What type to use for calculations (double or float).
// Believe it or not, the SunPro compiler will emit a differently mangled
// symbol if 'fsm_real' is a typedef. The result is a linker error.
#ifndef VCL_SUNPRO_CC
typedef double fsm_real;
#else
# define fsm_real double
#endif

static fsm_real function(unsigned i, unsigned j) {
  return i * j;
}

void test_real(vnl_fftxd_prime_factors<fsm_real> const &prx,
               vnl_fftxd_prime_factors<fsm_real> const &pry,
               vnl_matrix<fsm_real> const &M,
               int dir)
{
  vnl_fft2d<fsm_real> fft_matrix(M, prx, pry, dir);
  vnl_fft2d<fsm_real> fft_array (M.begin(), M.rows(), M.cols(), prx, pry, dir);
  vnl_test_assert ("test real", fft_matrix == fft_array);
}

void test_reim(vnl_fftxd_prime_factors<fsm_real> const &prx,
               vnl_fftxd_prime_factors<fsm_real> const &pry,
               vnl_matrix<fsm_real> const &M_real,
               vnl_matrix<fsm_real> const &M_imag,
               int dir)
{
  vnl_fft2d<fsm_real> fft_matrix(M_real, M_imag, prx, pry, dir);
  vnl_fft2d<fsm_real> fft_array (M_real.begin(), M_imag.begin(), M_real.rows(), M_real.cols(), prx, pry, dir);
  vnl_test_assert ("test reim", fft_matrix == fft_array);
}

void test_cplx(vnl_fftxd_prime_factors<fsm_real> const &prx,
               vnl_fftxd_prime_factors<fsm_real> const &pry,
               vnl_matrix<vcl_complex<fsm_real> > const &M,
               int dir)
{
#ifndef VCL_VC
  vnl_fft2d<fsm_real> fft_matrix(M, prx, pry, dir);
  vnl_fft2d<fsm_real> fft_array (M.begin(), M.rows(), M.cols(), prx, pry, dir);
  vnl_test_assert ("test cplx", fft_matrix == fft_array);
#endif
}


void test_fft2d () {
  const unsigned int fsm_rows = 64;
  const unsigned int fsm_cols = 64;

  // calculate prime factors for this size array
  vnl_fftxd_prime_factors<fsm_real> prx (fsm_rows);
  vnl_fftxd_prime_factors<fsm_real> pry (fsm_cols);

  if (!prx) {
    vcl_cerr << "cannot decompose X-size " << fsm_rows << ")into the form (2^P)(3^Q)(5^R)\n";
    abort();
  }
  if (!pry) {
    vcl_cerr << "cannot decompose Y-size (" << fsm_cols << ") into the form (2^P)(3^Q)(5^R)\n";
    abort();
  }

  // create arrays for testing the different constructors

  // data as arrays :
  fsm_real real_array[fsm_cols*fsm_rows];
  fsm_real imag_array[fsm_cols*fsm_rows];

  // fill with stuff :
  for (unsigned i=0; i<fsm_rows; ++i) {
    for (unsigned j=0; j<fsm_cols; ++j) {
      real_array[i*fsm_cols + j] = function(i, j);
      imag_array[i*fsm_cols + j] = 0.0;
    }
  }

  // complexify :
  vcl_complex<fsm_real> cplx_array[fsm_rows*fsm_cols];
  vnl_complexify(real_array, imag_array, cplx_array, fsm_rows*fsm_cols);

  // data as matrices :
  vnl_matrix<vcl_complex<fsm_real> > cplx_matrix(cplx_array, fsm_rows,fsm_cols);
  vnl_matrix<fsm_real> real_matrix(real_array, fsm_rows,fsm_cols);
  vnl_matrix<fsm_real> imag_matrix(imag_array, fsm_rows,fsm_cols);

  //--------------------------------------------------------------------------------

  // simple ***

  //--------------------------------------------------------------------------------

  // verify that the array and matrix constructors give the same result :

  test_real(prx, pry, real_matrix, +1);
  test_real(prx, pry, real_matrix, -1);

  test_reim(prx, pry, real_matrix, imag_matrix, +1);
  test_reim(prx, pry, real_matrix, imag_matrix, -1);

  test_cplx(prx, pry, cplx_matrix, +1);
  test_cplx(prx, pry, cplx_matrix, -1);

  //--------------------------------------------------------------------------------

  // verify that backwards * forwards is multiplication by .size().
  {
#ifndef VCL_VC
    vnl_fft2d<fsm_real> fwd(cplx_matrix, prx, pry, +1);
    vnl_fft2d<fsm_real> bwd(fwd, prx, pry, -1);

    double error = (bwd - vcl_complex<double>(cplx_matrix.size())*cplx_matrix).fro_norm();
    vcl_cout << "error = " << error << vcl_endl;
    vnl_test_assert ("fwd-bwd error", error < 1e-7); // increase for float
#endif
  }
}

TESTMAIN (test_fft2d);
