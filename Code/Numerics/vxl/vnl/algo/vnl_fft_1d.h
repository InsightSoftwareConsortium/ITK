#ifndef vnl_fft_1d_h_
#define vnl_fft_1d_h_
#ifdef __GNUC__
#pragma interface
#endif
/*
  fsm@robots.ox.ac.uk
*/

//: In-place 1D fast fourier transform.

#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_fft_base.h>

template <class T>
struct vnl_fft_1d : vnl_fft_base<1, T>
{
  typedef vnl_fft_base<1, T> base;
  
  //: constructor takes length of signal.
  vnl_fft_1d(int N) {
    base::factors_[0].resize(N);
  }

  
  //: dir = +1/-1 according to direction of transform.
  void transform(vnl_vector<vcl_complex<T> > &signal, int dir)
  { base::transform(signal.data_block(), dir); }
  
  //: forward FFT
  void fwd_transform(vnl_vector<vcl_complex<T> > &signal)
  { transform(signal, +1); }
  
  //: backward (inverse) FFT
  void bwd_transform(vnl_vector<vcl_complex<T> > &signal)
  { transform(signal, -1); }
  
  //: return length of signal.
  unsigned size() const { return base::factors_[0].number(); }
  
};

#endif
