// -*- c++ -*-
#ifndef vnl_fft1d_h_
#define vnl_fft1d_h_

// .NAME	vnl_fft1d - 1D Fourier transform
// .LIBRARY	vnl-algo
// .HEADER	vxl Package
// .INCLUDE	vnl/algo/vnl_fft1d.h
// .FILE	vnl_fft1d.txx
// .EXAMPLE	../tests/test_fft1d.cxx
//
// .SECTION Description
//
//  templated 1D FFT class based on Temperton FFT routines:
//
//  C TEMPERTON 
//  A GENERALIZED PRIME FACTOR FFT ALGORITHM FOR ANY $N = 2^P 3^Q 5^R$
//  SIAM J. SCI. STAT. COMP., MAY 1992.)
//
//  original temperton-code (fortran) converted using f2c with -C++
//  option. Two versions created: float and double.
// 
//  subclassed from vnl_vector<vnl_complex>, i.e. internally uses
//  vnl_vector<vnl_complex> for storing the data
// 
//  two super-simple constructors, first takes as input is an vnl_vector<T>
//  for which it will compute  the FFT. Second takes vnl_complex_vector_t<T> and
//  direction. Calculates FFT in that direction.
//  For efficiency, all other constructors take as an argument two
//  PrimeFactors<T>-objects  which have to be initialised before the 
//  call do doFFT. These PrimeFactors<T> objects contain the prime-factors
//  of the 'number' (the size of the array) to be FFT'd.
//  (integral part of Temperton's algorithm)
//
// .SECTION Author
//  Veit U.B. Schenk, Oxford RRG, 19 Mar 98
// .SECTION Modifications:
//-----------------------------------------------------------------------------

#include <vnl/vnl_complex.h>
#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_fftxd_prime_factors.h>

template<class T>
class vnl_fft1d : public vnl_vector<vnl_complex<T> > {
  typedef vnl_vector<vnl_complex<T> > base;
public:
  // real-constructors
  vnl_fft1d (const vnl_vector<T> &r); // only real data, always forward
  vnl_fft1d (const vnl_vector<vnl_complex<T> > &z, int dir); // forw & backw
  
  // (real,imag)-constructors
  vnl_fft1d (const vnl_vector<T> &r, const vnl_vector<T> &i,
	     const vnl_fftxd_prime_factors<T> &oPF, int dir); // vnl_vectors r,i
  vnl_fft1d (const vnl_vector<T> &r, 
	     const vnl_fftxd_prime_factors<T> &oPF, int dir);  // Imag defaults to 0.0 
  vnl_fft1d (const T *realdata, const T *imagdata, unsigned len, 
  	     const vnl_fftxd_prime_factors<T>  &, int dir);  // 'raw' r,i
  vnl_fft1d (const T *data, unsigned len, 
	     const vnl_fftxd_prime_factors<T>  &, int dir);         // 'raw' r. I defaults to 0.0
  
  // complex-constructors
#ifndef VCL_VC
  vnl_fft1d (const vnl_vector<vcl_complex<T> > &c, const vnl_fftxd_prime_factors<T> &, int dir);          //  complex vnl_vector
#endif
  vnl_fft1d (const vcl_complex<T> *cdata, unsigned len, const vnl_fftxd_prime_factors<T> &, int dir);  // 'raw' complex data

  // static member function: avoid copy-overhead
  static int doFFT_IP (vnl_complex<T> *cdata, unsigned len, 
		       const vnl_fftxd_prime_factors<T> &, int dir);  
private:
  int doFFT (const vnl_fftxd_prime_factors<T> &, int dir);
};
  
#endif // vnl_fft1d_h_
