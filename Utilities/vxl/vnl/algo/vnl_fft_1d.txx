#ifndef vnl_fft_1d_txx_
#define vnl_fft_1d_txx_
// -*- c++ -*-

#include "vnl_fft_1d.h"

#undef VNL_FFT_1D_INSTANTIATE
#define VNL_FFT_1D_INSTANTIATE(T) \
template struct vnl_fft_1d<T >

#endif
