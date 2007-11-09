// This is core/vnl/algo/vnl_fft_1d.h
#ifndef vnl_fft_1d_h_
#define vnl_fft_1d_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief In-place 1D fast Fourier transform
// \author fsm
//
// \verbatim
//  Modifications
//   19 June 2003 - Peter Vanroose - added cmplx* and vector<cmplx> interfaces
// \endverbatim


#if (defined _MSC_VER) && _MSC_VER == 1200
// disable warning buried deep in the standard library
// warning C4018: '<' : signed/unsigned mismatch: vector(159)
# pragma warning(disable: 4018)
#endif


#include <vcl_vector.h>
#include <vnl/vnl_vector.h>
#include <vnl/algo/vnl_fft_base.h>

//: In-place 1D fast Fourier transform

template <class T>
struct vnl_fft_1d : public vnl_fft_base<1, T>
{
  typedef vnl_fft_base<1, T> base;

  //: constructor takes length of signal.
  vnl_fft_1d(int N) {
    base::factors_[0].resize(N);
  }

  //: return length of signal.
  unsigned int size() const { return base::factors_[0].number(); }

  //: dir = +1/-1 according to direction of transform.
  void transform(vcl_vector<vcl_complex<T> > &signal, int dir)
  { base::transform(&signal[0], dir); }

  //: dir = +1/-1 according to direction of transform.
  void transform(vcl_complex<T> *signal, int dir)
  { base::transform(signal, dir); }

  //: dir = +1/-1 according to direction of transform.
  void transform(vnl_vector<vcl_complex<T> > &signal, int dir)
  { base::transform(signal.data_block(), dir); }

  //: forward FFT
  void fwd_transform(vcl_vector<vcl_complex<T> > &signal)
  { transform(signal, +1); }

  //: forward FFT
  void fwd_transform(vcl_complex<T> *signal)
  { transform(signal, +1); }

  //: forward FFT
  void fwd_transform(vnl_vector<vcl_complex<T> > &signal)
  { transform(signal, +1); }

  //: backward (inverse) FFT
  void bwd_transform(vcl_vector<vcl_complex<T> > &signal)
  { transform(signal, -1); }

  //: backward (inverse) FFT
  void bwd_transform(vcl_complex<T> *signal)
  { transform(signal, -1); }

  //: backward (inverse) FFT
  void bwd_transform(vnl_vector<vcl_complex<T> > &signal)
  { transform(signal, -1); }
};

#endif // vnl_fft_1d_h_
