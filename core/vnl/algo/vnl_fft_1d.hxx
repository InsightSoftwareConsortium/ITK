#ifndef vnl_fft_1d_hxx_
#define vnl_fft_1d_hxx_
// -*- c++ -*-

#include "vnl_fft_1d.h"

#undef VNL_FFT_1D_INSTANTIATE
#define VNL_FFT_1D_INSTANTIATE(T) \
template struct VNL_ALGO_EXPORT vnl_fft_1d<T >

#endif
