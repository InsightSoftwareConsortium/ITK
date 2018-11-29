// This is core/vnl/algo/vnl_fft_base.h
#ifndef vnl_fft_base_h_
#define vnl_fft_base_h_
//:
// \file
// \brief In-place n-D fast Fourier transform
// \author fsm

#include <complex>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include <vnl/algo/vnl_algo_export.h>
#include <vnl/algo/vnl_fft_prime_factors.h>

//: Base class for in-place ND fast Fourier transform.

template <int D, class T>
struct VNL_ALGO_EXPORT vnl_fft_base
{
  vnl_fft_base() = default;

  //: dir = +1/-1 according to direction of transform.
  void transform(std::complex<T> *signal, int dir);

 protected:
  //: prime factorizations of signal dimensions.
  vnl_fft_prime_factors<T> factors_[D];
};

#endif // vnl_fft_base_h_
