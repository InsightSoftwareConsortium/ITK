// This is core/vnl/algo/vnl_fft_2d.h
#ifndef vnl_fft_2d_h_
#define vnl_fft_2d_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief In-place 2D fast Fourier transform
// \author fsm

#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_fft_base.h>

//: In-place 2D fast Fourier transform

template <class T>
struct vnl_fft_2d : public vnl_fft_base<2, T>
{
  typedef vnl_fft_base<2, T> base;

  //: constructor takes size of signal.
  vnl_fft_2d(int M, int N) {
    base::factors_[0].resize(M);
    base::factors_[1].resize(N);
  }

  //: dir = +1/-1 according to direction of transform.
  void transform(vnl_matrix<vcl_complex<T> > &signal, int dir)
  { base::transform(signal.data_block(), dir); }

  //: forward FFT
  void fwd_transform(vnl_matrix<vcl_complex<T> > &signal)
  { transform(signal, +1); }

  //: backward (inverse) FFT
  void bwd_transform(vnl_matrix<vcl_complex<T> > &signal)
  { transform(signal, -1); }

  //: return size of signal.
  unsigned rows() const { return base::factors_[0].number(); }
  unsigned cols() const { return base::factors_[1].number(); }
};

#endif // vnl_fft_2d_h_
