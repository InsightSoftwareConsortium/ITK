// -*- c++ -*-
// Class: vnl_fft2d
// Author: Veit U.B. Schenk, Oxford RRG, 19 Mar 98
//
//-----------------------------------------------------------------------------

#include "vnl_fft2d.h"

#include <vcl_cstdlib.h>  // abort()
#include <vcl_iostream.h>

#include <vnl/vnl_complex_ops.h>
#include <vnl/algo/vnl_netlib.h> // dgpfa_(), gpfa_()

//--------------------------------------------------------------------------------

// -- super-simple constructor: take vnl_matrix<float>, do the forward FFT
// -- don't have to worry about the prime-factors
template<class T> 
vnl_fft2d<T>::vnl_fft2d (const vnl_matrix<T> &R) : base (R.rows(), R.columns())
{
  vnl_complexify(R.data_block(),
		 base::data_block(), base::size());
  
  vnl_fftxd_prime_factors<T> oPFx (R.rows ());
  vnl_fftxd_prime_factors<T> oPFy (R.cols ());
  if (!oPFx || !oPFy) {
    vcl_cerr << __FILE__ " : image dimensions not of form 2^P*3^Q*5^R" << vcl_endl;
    abort();
  }
  
  doFFT (oPFx, oPFy, +1); // always forward
}

// -- super-simple: takes complex matrix, can do both directions
// -- don't have to worry about the prime-factors
template<class T> 
vnl_fft2d<T>::vnl_fft2d (const vnl_matrix<vnl_complex<T> > &Z, int dir) : base (Z)
{
  vnl_fftxd_prime_factors<T> oPFx (Z.rows ());
  vnl_fftxd_prime_factors<T> oPFy (Z.cols ());
  if (!oPFx || !oPFy) {
    vcl_cerr << __FILE__ " : image dimensions not of form 2^P*3^Q*5^R" << vcl_endl;
    abort();
  }
  
  doFFT (oPFx, oPFy, dir); 
}
  
// -- init with vnl_matrix R (default i=0.0)
template<class T> 
vnl_fft2d<T>::vnl_fft2d (const vnl_matrix<T> &R, 
			 const vnl_fftxd_prime_factors<T> &oPFx, 
			 const vnl_fftxd_prime_factors<T> &oPFy, int dir)
  : base(R.rows(), R.columns())
{
  vnl_complexify(R.data_block(), 
		 base::data_block(), base::size());
  doFFT (oPFx, oPFy, dir);
}


// -- init with vnl_matrices r,i
template<class T> 
vnl_fft2d<T>::vnl_fft2d (const vnl_matrix<T> &R, const vnl_matrix<T> &I, 
			 const vnl_fftxd_prime_factors<T> &oPFx, 
			 const vnl_fftxd_prime_factors<T> &oPFy, int dir)
  : base(R.rows(), R.cols())
{
  vnl_complexify(R.data_block(), I.data_block(), 
		 base::data_block(), base::size());
  doFFT (oPFx, oPFy, dir);
}

// -- init with 'raw' r(RData) and default i(0.0)
template<class T> 
vnl_fft2d<T>::vnl_fft2d (const T *RData, unsigned int iRows, unsigned int iCols, 
			 const vnl_fftxd_prime_factors<T> &oPFx, 
			 const vnl_fftxd_prime_factors<T> &oPFy, int dir)
  : base(iRows, iCols)
{
  vnl_complexify(RData, 
		 base::data_block(), base::size());
  doFFT (oPFx, oPFy, dir);
}

// -- init with 'raw' r(RData) and 'raw' i(IData)
template<class T> 
vnl_fft2d<T>::vnl_fft2d (const T *RData, const T *IData,  // data
			 unsigned int iRows, unsigned int iCols, // dimensions
			 const vnl_fftxd_prime_factors<T> &oPFx, 
			 const vnl_fftxd_prime_factors<T> &oPFy, int dir)
  : base(iRows, iCols)
{
  vnl_complexify(RData, IData, 
		 base::data_block(), base::size());
  doFFT (oPFx, oPFy, dir);
}

#ifndef VCL_VC60
// -- init to given complex vnl_matrix
template<class T> 
vnl_fft2d<T>::vnl_fft2d (const vnl_matrix<vnl_complex<T> > &C, 
			 const vnl_fftxd_prime_factors<T> &oPFx, 
			 const vnl_fftxd_prime_factors<T> &oPFy, int dir)
  : base(C)
{
  doFFT (oPFx, oPFy, dir);
}

// -- create new complex given 'raw' complex c(CData)
template<class T> 
vnl_fft2d<T>::vnl_fft2d (const vnl_complex<T> *CData, 
			 unsigned int iRows, unsigned int iCols, // dimensions
			 const vnl_fftxd_prime_factors<T> &oPFx, 
			 const vnl_fftxd_prime_factors<T> &oPFy, int dir)
  : base(CData,iRows, iCols)
{
  doFFT (oPFx, oPFy, dir);
}
#endif

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

// use C++ overloading to find correct FORTRAN routine

inline void cxx_gpfa(double *a, double *b, const double *triggs, 
		     const int&inc, const int&jump, const int&n, 
		     const int&lot, const int&isign, const int *iPvnl_qr, int *info)
{
  dgpfa_(a,b,triggs,inc,jump,n,lot,isign,iPvnl_qr,info);
}

inline void cxx_gpfa (float  *a, float  *b, const float  *triggs, 
		      const int&inc, const int&jump, const int&n, 
		      const int&lot, const int&isign, const int *iPvnl_qr, int *info)
{
  gpfa_(a,b,triggs,inc,jump,n,lot,isign,iPvnl_qr,info);
}


// -- the static member function
// can be called directly by user to avoid copying-overhead
// called internally by all constructors
template<class T> 
int vnl_fft2d<T>::doFFT_IP (vnl_complex<T> *cdata, unsigned int iRows,
			    unsigned int iColumns, 
			    const vnl_fftxd_prime_factors<T> &oPFx, 
			    const vnl_fftxd_prime_factors<T> &oPFy, int iDirection)
{
  int info = 0; // return value

  T *oTReal = (T*)cdata;
  // use stride of 2 (and hope noone messes with the complex-class,
  // relies on the fact that the first element is the real, the
  // second the imaginary part.
  
  cxx_gpfa (oTReal,(T*)(oTReal+1),oPFx.getFactors (),
	    2*iColumns,2,iRows,iColumns,iDirection,oPFx.getPvnl_qr (),&info);
  
  if (info != -1)
    cxx_gpfa (oTReal,(T*)(oTReal+1),oPFy.getFactors (),
	      2,2*iColumns,iColumns,iRows,iDirection,oPFy.getPvnl_qr (),&info);

  return info;
}
  
// -- calls the actual fft routine with correct stride for complex data
// -- declared as 'private', since only called from within constructors


template<class T> 
int vnl_fft2d<T>::doFFT (const vnl_fftxd_prime_factors<T> &oPFx, 
			 const vnl_fftxd_prime_factors<T> &oPFy, int iDirection)
{
  return doFFT_IP (this->data_block (), this->rows (), this->columns (),
		   oPFx, oPFy, iDirection);
}

//--------------------------------------------------------------------------------

#define VNL_FFT2D_INSTANTIATE(T) template class vnl_fft2d<T >;
