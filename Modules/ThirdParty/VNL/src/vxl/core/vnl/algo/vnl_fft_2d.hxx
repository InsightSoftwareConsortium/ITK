#ifndef vnl_fft_2d_hxx_
#define vnl_fft_2d_hxx_
// -*- c++ -*-

#include "vnl_fft_2d.h"

#undef VNL_FFT_2D_INSTANTIATE
#define VNL_FFT_2D_INSTANTIATE(T) \
template struct VNL_ALGO_EXPORT vnl_fft_2d<T >

#endif
