#ifndef vnl_fft_base_h_
#define vnl_fft_base_h_
#ifdef __GNUC__
#pragma interface
#endif
/*
  fsm@robots.ox.ac.uk
*/

//: Base class for in-place ND fast fourier transform.

#include <vcl_complex.h>
#include <vnl/algo/vnl_fft_prime_factors.h>

export template <int D, class T>
struct vnl_fft_base
{
  vnl_fft_base() { }
  
  //: dir = +1/-1 according to direction of transform.
  void transform(vcl_complex<T> *signal, int dir);
  
protected: 
  //: prime factorizations of signal dimensions.
  vnl_fft_prime_factors<T> factors_[D];
};

#endif
