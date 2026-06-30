// This is core/vnl/algo/vnl_fft_1d.h
#ifndef vnl_fft_1d_h_
#define vnl_fft_1d_h_
//:
// \file
// \brief In-place 1D fast Fourier transform (deprecated PocketFFT-backed shim)
//
// Compatibility alias for the removed Temperton GPFA implementation; the
// transform is computed by PocketFFT (ITKFFT module) with the original vnl
// sign convention (dir=+1 applies exp(+2*pi*i*j*k/N)) and no normalization.
// Unlike the Temperton backend, any signal length is supported.
//
// Requires the ITKFFT module on the include path (for itk_pocketfft.h):
// a consumer that includes this header must depend on ITKFFT.

#if defined(ITK_FUTURE_LEGACY_REMOVE) || defined(ITK_LEGACY_REMOVE)
#  error \
    "vnl_fft_1d has been removed: use the itk::PocketFFT* / itk::Forward1DFFTImageFilter family from the ITKFFT module instead."
#else

#  include <complex>
#  include <vector>
#  ifdef _MSC_VER
#    include <vcl_msvc_warnings.h>
#  endif
#  include <vnl/vnl_vector.h>
#  include "itk_pocketfft.h" // provided by the ITKPocketFFT module include directory

#  if defined(ITK_LEGACY_SILENT)
#    define VNL_FFT_1D_DEPRECATED
#  else
#    define VNL_FFT_1D_DEPRECATED \
      [[deprecated("vnl_fft_1d is deprecated and will be removed with ITK_FUTURE_LEGACY_REMOVE: " \
                   "use the itk::PocketFFT* FFT image filters (ITKFFT) instead.")]]
#  endif

//: In-place 1D fast Fourier transform

template <class T>
struct VNL_FFT_1D_DEPRECATED vnl_fft_1d
{
  //: constructor takes length of signal.
  vnl_fft_1d(int N)
    : size_{ static_cast<unsigned int>(N) }
  {}

  //: return length of signal.
  unsigned int
  size() const
  {
    return size_;
  }

  //: dir = +1/-1 according to direction of transform.
  void
  transform(std::complex<T> * signal, int dir)
  {
    const itk::detail::pocketfft::shape_t  shape{ size_ };
    const itk::detail::pocketfft::stride_t stride{ static_cast<ptrdiff_t>(sizeof(std::complex<T>)) };
    const itk::detail::pocketfft::shape_t  axes{ 0 };
    // vnl dir=+1 is exp(+i...), which is pocketfft's backward direction.
    itk::detail::pocketfft::c2c(shape, stride, stride, axes, dir < 0, signal, signal, static_cast<T>(1));
  }

  //: dir = +1/-1 according to direction of transform.
  void
  transform(std::vector<std::complex<T>> & signal, int dir)
  {
    transform(signal.data(), dir);
  }

  //: dir = +1/-1 according to direction of transform.
  void
  transform(vnl_vector<std::complex<T>> & signal, int dir)
  {
    transform(signal.data_block(), dir);
  }

  //: forward FFT
  void
  fwd_transform(std::vector<std::complex<T>> & signal)
  {
    transform(signal, +1);
  }

  //: forward FFT
  void
  fwd_transform(std::complex<T> * signal)
  {
    transform(signal, +1);
  }

  //: forward FFT
  void
  fwd_transform(vnl_vector<std::complex<T>> & signal)
  {
    transform(signal, +1);
  }

  //: backward (inverse) FFT
  void
  bwd_transform(std::vector<std::complex<T>> & signal)
  {
    transform(signal, -1);
  }

  //: backward (inverse) FFT
  void
  bwd_transform(std::complex<T> * signal)
  {
    transform(signal, -1);
  }

  //: backward (inverse) FFT
  void
  bwd_transform(vnl_vector<std::complex<T>> & signal)
  {
    transform(signal, -1);
  }

private:
  unsigned int size_;
};

#  undef VNL_FFT_1D_DEPRECATED

#endif // ITK_FUTURE_LEGACY_REMOVE
#endif // vnl_fft_1d_h_
