#ifndef vnl_fft2d_h_
#define vnl_fft2d_h_
//
// .NAME vnl_fft2d - templated 2D FFT class based on Temperton FFT routines
// .HEADER vxl package
// .LIBRARY vnl-algo
// .INCLUDE vnl/algo/vnl_fft2d.h
// .FILE vnl_fft2d.txx
//
// .SECTION Description
//  templated 2D FFT class based on Temperton FFT routines:
//
//  C TEMPERTON 
//  A GENERALIZED PRIME FACTOR FFT ALGORITHM FOR ANY $N = 2^P 3^Q 5^R$
//  SIAM J. SCI. STAT. COMP., MAY 1992.)
//
//  original temperton-code (fortran) converted using f2c with -C++
//  option. Two versions created: float and double.
// 
//  subclassed from vnl_matrix<vnl_complex>, i.e. internally uses
//  vnl_matrix<vnl_complex> for storing the data
// 
//  one super-simple constructor, all it takes as input is an vnl_matrix<float>
//  for which it will compute  the FFT. 
//  For efficiency, all other constructors take as an argument two
//  PrimeFactors<T>-objects  which have to be initialised before the 
//  call do doFFT. These PrimeFactors<T> objects contain the prime-factors
//  of the 'number' (the size of the array) to be FFT'd.
//  (integral part of Temperton's algorithm)
//
// .SECTION Author
//   Veit U.B. Schenk, Oxford RRG, 19 Mar 98
// .SECTION Modifications:
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_complex.h>
#include <vnl/vnl_matrix.h>
#include "vnl_fftxd_prime_factors.h"

template<class T>
class vnl_fft2d : public vnl_matrix<vnl_complex<T> > {
public:
  typedef vnl_matrix<vnl_complex<T> > base;

  vnl_fft2d (const vnl_matrix<T> &real); // always forward
  vnl_fft2d (const vnl_matrix<vnl_complex<T> > &z, int dir); // both forw and backw
  
  // (real,imag)-constructors
  vnl_fft2d (const vnl_matrix<T> &r, const vnl_matrix<T> &i,
	     const vnl_fftxd_prime_factors<T> &oPFx,
	     const vnl_fftxd_prime_factors<T> &oPFy, int dir); // vnl_matrixs r,i
  vnl_fft2d (const vnl_matrix<T> &r, 
	     const vnl_fftxd_prime_factors<T> &oPFx,
	     const vnl_fftxd_prime_factors<T> &oPFy, int dir);  // Imag defaults to 0.0 
  vnl_fft2d (const T *realdata, const T *imagdata, unsigned int iRows, unsigned int iCols, 
	     const vnl_fftxd_prime_factors<T> &oPFx,
	     const vnl_fftxd_prime_factors<T> &oPFy, int dir);  // 'raw' r,i
  vnl_fft2d (const T *data, unsigned int iRows, unsigned int iCols, 
	     const vnl_fftxd_prime_factors<T> &oPFx,
	     const vnl_fftxd_prime_factors<T> &oPFy, int dir);         // 'raw' r. I defaults to 0.0

#ifndef VCL_VC60
  // complex-constructors
  vnl_fft2d (const vnl_matrix<vnl_complex<T> > &c, 
	     const vnl_fftxd_prime_factors<T> &oPFx,
	     const vnl_fftxd_prime_factors<T> &oPFy, int dir);          //  complex vnl_matrix
  vnl_fft2d (const vnl_complex<T> *cdata, unsigned int iRows, unsigned int iCols, 
	     const vnl_fftxd_prime_factors<T> &oPFx,
	     const vnl_fftxd_prime_factors<T> &oPFy, int dir);  // 'raw' complex data
#endif

  // static member function: avoid copy-overhead
  static int doFFT_IP (vnl_complex<T> *cdata, unsigned int rows, unsigned int cols, 
		       const vnl_fftxd_prime_factors<T> &oPFx,
		       const vnl_fftxd_prime_factors<T> &oPFy, int dir);  
  
private:
  int doFFT (const vnl_fftxd_prime_factors<T> &oPFx,
	     const vnl_fftxd_prime_factors<T> &oPFy, int dir);
};
  
#endif // vnl_fft2d_h_
