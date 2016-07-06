#ifndef vnl_fft_prime_factors_hxx_
#define vnl_fft_prime_factors_hxx_
/*
  fsm
*/
#include "vnl_fft_prime_factors.h"
#include <vnl/algo/vnl_fft.h>
#include <vcl_cassert.h>

template <class T>
vnl_fft_prime_factors<T>::vnl_fft_prime_factors()
  : trigs_(0)
  , number_(0)
{
}

template <class T>
void vnl_fft_prime_factors<T>::construct(int N)
{
  assert(N>0);
  trigs_ = new T[2*N];
  number_ = N;
  vnl_fft_setgpfa (trigs_, number_, pqr_, &info_);
  // info_ == -1 if cannot split into primes
  if (info_ == -1)
    assert(!"you probably gave a signal size not of the form 2^p 3^q 5^r");
}

template <class T>
void vnl_fft_prime_factors<T>::destruct()
{
  if (trigs_)
    delete [] trigs_;
}

#undef VNL_FFT_PRIME_FACTORS_INSTANTIATE
#define VNL_FFT_PRIME_FACTORS_INSTANTIATE(T) \
template struct VNL_ALGO_EXPORT vnl_fft_prime_factors<T >

#endif
