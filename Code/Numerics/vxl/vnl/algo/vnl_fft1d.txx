// -*- c++ -*-
// Class: vnl_fft1d
// Author: Veit U.B. Schenk, Oxford RRG, 19 Mar 98
//
//-----------------------------------------------------------------------------

#include "vnl_fft1d.h"

#include <vcl_cstdlib.h>    // abort() - PVr
#include <vcl_iostream.h>

#include <vnl/vnl_complex_ops.h>
#include <vnl/algo/vnl_netlib.h> // dgpfa_(), gpfa_()

//--------------------------------------------------------------------------------

// -- super-easy constructor #1: take real vector
// -- and do the forward transform

template<class T> 
vnl_fft1d<T>::vnl_fft1d (const vnl_vector<T> &R) : base (R.size())
{
  vnl_complexify(R.data_block(), base::data_block(), base::size());
  
  vnl_fftxd_prime_factors<T> oPF (R.size ());
  if (!oPF) {
    vcl_cerr << "vnl_fft1d<T>::vnl_fft1d (const vnl_vector<T> &R): vector size (";
    vcl_cerr << R.size () << ") not of format 2^P*3^Q*5^R\n";
    abort();
  }
  doFFT (oPF, +1);
}

// -- super-easy constructor #2: take complex vector and direction
// -- don't worry about twiddle-factors, just lean back ....
template<class T> 
vnl_fft1d<T>::vnl_fft1d (vnl_vector<vnl_complex<T> > const &Z, int dir) : base (Z)
{
  vnl_fftxd_prime_factors<T> oPF (Z.size ());
  if (!oPF) {
    vcl_cerr << "vnl_fft1d<T>::vnl_fft1d (const vnl_vector<vnl_complex<T> > &Z): vector size (";
    vcl_cerr << Z.size () << ") not of format 2^P*3^Q*5^R\n";
    abort();
  }
  doFFT (oPF, dir);
}
  
// -- create complex from vnl_vector R and i=0.0
template<class T> 
vnl_fft1d<T>::vnl_fft1d (const vnl_vector<T> &R, 
			 const vnl_fftxd_prime_factors<T> &oPF, int dir)
  : base(R.size ())
{
  // copy data into complex format
  vnl_complexify(R.begin(), base::data_block(), base::size());

  // do the transform
  doFFT (oPF, dir);
}

// -- create complex from vnl_vectors r,i
template<class T> 
vnl_fft1d<T>::vnl_fft1d (const vnl_vector<T> &R, const vnl_vector<T> &I, 
			 const vnl_fftxd_prime_factors<T> &oPF, int dir)
  : base(R.size ())
{
  // copy data into complex format
  vnl_complexify(R.begin(), I.begin(), base::data_block(), base::size());

  // do the transform
  doFFT (oPF, dir);
}

// -- create complex from 'raw' r(RData) and default i(0.0)
template<class T> 
vnl_fft1d<T>::vnl_fft1d (const T *RData, unsigned iLen, 
			 const vnl_fftxd_prime_factors<T> &oPF, int dir)
  : base(iLen)
{
  // copy data into complex format
  vnl_complexify(RData, base::data_block(), base::size());

  // do the transform
  doFFT (oPF, dir);
}

// -- create complex from 'raw' r(RData) and 'raw' i(IData)
template<class T> 
vnl_fft1d<T>::vnl_fft1d (const T *RData, const T *IData, unsigned iLen, 
			 const vnl_fftxd_prime_factors<T> &oPF, int dir)
  : base(iLen)
{
  // copy data into complex format
  vnl_complexify(RData, IData, base::data_block(), iLen);

  // do the transform
  doFFT (oPF, dir);
}

#ifndef VCL_VC
// -- init to given complex vnl_vector
template<class T> 
vnl_fft1d<T>::vnl_fft1d (vnl_vector<vnl_complex<T> > const& C, 
			 vnl_fftxd_prime_factors<T> const& oPF, int dir)
  : base(C)
{
  doFFT (oPF, dir);
}
#endif

// -- create new complex given 'raw' complex c(CData)
template<class T> 
vnl_fft1d<T>::vnl_fft1d (vnl_complex<T> const *CData, unsigned iLen, 
			 vnl_fftxd_prime_factors<T> const &oPF, int dir)
  : base(CData,iLen)
{
  doFFT (oPF, dir);
}


/************************************************************
************************************************************
* the actual call of the FFT routine
* 
* the arguments to gpfa are:
*
  // T *real-part
  // T *imag-part 
  // const T *primefactors,
  // int INCREMENT WITHIN EACH DATA VECTOR
  // int INCREMENT BETWEEN DATA VECTORS
  // int ciSize: size of transform (which has to be of form:
  //             ciSize = (2**IP) * (3**IQ) * (5**IR)
  // int NUMBER OF TRANSFORMS
  // int +1/-1 == direction (forward/backward)
*
************************************************************
************************************************************/


// use C++ overloading to find thecorrect FORTRAN routine

inline void cxx_gpfa(double *a, double *b, const double *triggs, 
		     const int&inc, const int&jump, const int&n, 
		     const int&lot, const int&isign, const int *iPvnl_qr, int *info)
{
  dgpfa_(a,b,triggs,inc,jump,n,lot,isign,iPvnl_qr,info);
}

inline void cxx_gpfa(float  *a, float  *b, const float  *triggs, 
		     const int&inc, const int&jump, const int&n, 
		     const int&lot, const int&isign, const int *iPvnl_qr, int *info)
{
  gpfa_(a,b,triggs,inc,jump,n,lot,isign,iPvnl_qr,info);
}


// -- the static member function
// can be called directly by user to avoid copying-overhead
// called internally by all constructors
template<class T> 
int vnl_fft1d<T>::doFFT_IP (vnl_complex<T> *cdata, unsigned iLen, 
			    vnl_fftxd_prime_factors<T> const &oPF, int iDirection)
{
  int info = 0; // return value (should really throw exceptions)

  T *oTReal = (T*)cdata;
  // use stride of 2 (and hope noone messes with the complex-class,
  // relies on the fact that the first element is the real, the
  // second the imaginary part.
  cxx_gpfa (oTReal,(T*)(oTReal+1),oPF.getFactors (),
	       2,0,iLen,1,iDirection,oPF.getPvnl_qr (),&info);
  return info;
}
  
// -- calls the actual fft routine with correct stride for complex data
// -- declared as 'private', since only called from within constructors


template<class T> 
int vnl_fft1d<T>::doFFT (const vnl_fftxd_prime_factors<T> &oPF, int iDirection)
{
  return doFFT_IP (this->data_block (), this->size (), oPF, iDirection);
}

//--------------------------------------------------------------------------------

#define VNL_FFT1D_INSTANTIATE(T) template class vnl_fft1d<T >;
