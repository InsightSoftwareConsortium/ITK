#ifndef vnl_fft_2d_txx_
#define vnl_fft_2d_txx_
// -*- c++ -*-

#include "vnl_fft_2d.h"

#undef VNL_FFT_2D_INSTANTIATE
#define VNL_FFT_2D_INSTANTIATE(T) \
template struct vnl_fft_2d<T >

#endif
