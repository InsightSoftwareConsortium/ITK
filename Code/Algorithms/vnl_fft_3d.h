// This is vxl/vnl/algo/vnl_fft_3d.h
#ifndef vnl_fft_3d_h_
#define vnl_fft_3d_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief In-place 3D fast fourier transform
// \author fsm

#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_fft_base.h>

//: In-place 3D fast fourier transform

template <class T>
struct vnl_fft_3d : public vnl_fft_base<3, T>
{
  typedef vnl_fft_base<3, T> base;

  //: constructor takes size of signal.
  vnl_fft_3d(int M, int N,int Q) {
    base::factors_[0].resize(M);
    base::factors_[1].resize(N);
    base::factors_[2].resize(Q);
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

#endif // vnl_fft_3d_h_
