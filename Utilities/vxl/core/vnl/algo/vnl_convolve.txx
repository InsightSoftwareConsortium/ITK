// This is core/vnl/algo/vnl_convolve.txx
#ifndef vnl_convolve_txx_
#define vnl_convolve_txx_

#include "vnl_convolve.h"
#include <vnl/algo/vnl_fft_1d.h> // this #includes <vcl_complex.h>
#include <vcl_cassert.h>
#include <vcl_iostream.h> // for warning messages

template <class T1, class T2, class U>
inline
vnl_vector<U> vnl_convolve_cyclic_using_fft(vnl_vector<T1> const& v1, vnl_vector<T2> const& v2, U*)
{
  assert (v1.size() == v2.size());
  unsigned int n = v1.size();

  typedef vcl_complex<double> C;
  vnl_vector<C> w1(n, C(0)); for (unsigned i=0; i<n; ++i) w1[i]=v1[i];
  vnl_vector<C> w2(n, C(0)); for (unsigned i=0; i<n; ++i) w2[i]=v2[i];

  vnl_fft_1d<double> fft(n); fft.fwd_transform(w1); fft.fwd_transform(w2);
  for (unsigned int i=0; i<n; ++i) w1[i] *= w2[i];
  fft.bwd_transform(w1);
#ifdef DEBUG
  vcl_cout << w1 << vcl_endl;
#endif

  vnl_vector<U> r(n);
  for (unsigned int i = 0; i<n; ++i)
    r[i] = U(vcl_real(w1[i]) / n); // the imaginary part is certainly zero
#ifdef DEBUG
  for (unsigned int i = 0; i<n; ++i)
    assert(vcl_imag(w1[i]) == 0);
#endif
  return r;
}

template <class T1, class T2, class U>
vnl_vector<U> vnl_convolve_cyclic(vnl_vector<T1> const& v1, vnl_vector<T2> const& v2, U*, bool use_fft)
{
  assert (v1.size() == v2.size());
  unsigned int n = v1.size();

  // Quick return if possible:
  if (n == 0) return vnl_vector<U>(0, U(0));
  if (n == 1) return vnl_vector<U>(1, U(v1[0]*v2[0]));

  if (use_fft)
    return vnl_convolve_cyclic_using_fft(v1, v2, (U*)0);

  vnl_vector<U> ret(n, (U)0); // all elements already initialized to zero
  for (unsigned int k=0; k<n; ++k)
  {
    for (unsigned int i=0; i<=k; ++i)
      ret[k] += U(v1[k-i]) * U(v2[i]);
    for (unsigned int i=k+1; i<n; ++i)
      ret[k] += U(v1[n+k-i]) * U(v2[i]);
  }

  return ret;
}

inline bool has_only_primefactors_2_3_5(unsigned int n)
{
  if (n <= 1) return true;
  while (n%2 == 0) n /= 2;
  while (n%3 == 0) n /= 3;
  while (n%5 == 0) n /= 5;
  return n==1;
}

template <class T1, class T2, class U>
inline
vnl_vector<U> vnl_convolve_using_fft(vnl_vector<T1> const& v1, vnl_vector<T2> const& v2, U*, int n)
{
  if (n+1 < int(v1.size() + v2.size())) n = v1.size() + v2.size() - 1;

  // Make sure n has only prime factors 2, 3 and 5; if necessary, increase n.
  while (!has_only_primefactors_2_3_5(n)) ++n;

  // pad with zeros, so the cyclic convolution is a convolution:
  vnl_vector<U> w1(n, U(0)); for (unsigned i=0; i<v1.size(); ++i) w1[i]=v1[i];
  vnl_vector<U> w2(n, U(0)); for (unsigned i=0; i<v2.size(); ++i) w2[i]=v2[i];
  // convolve, using n-points FFT:
  w1 = vnl_convolve_cyclic_using_fft(w1, w2, (U*)0);
  // return w1, but possibly drop the last few (zero) entries:
  return vnl_vector<U>(v1.size()+v2.size()-1, v1.size()+v2.size()-1, w1.data_block());
}

template <class T>
vnl_vector<T> vnl_convolve(vnl_vector<T> const& v1, vnl_vector<T> const& v2, int use_fft)
{
  // Quick return if possible:
  if (v1.size() == 0 || v2.size() == 0)
    return vnl_vector<T>(0);
  if (v1.size() == 1) return v2*v1[0];
  if (v2.size() == 1) return v1*v2[0];

  if (use_fft != 0)
    return vnl_convolve_using_fft(v1, v2, (T*)0, use_fft);

  unsigned int n = v1.size() + v2.size() - 1;
  vnl_vector<T> ret(n, (T)0); // all elements already initialized to zero
  for (unsigned int k=0; k<v1.size(); ++k)
    for (unsigned int i=0; i<=k && i<v2.size(); ++i)
      ret[k] += v1[k-i] * v2[i];
  for (unsigned int k=v1.size(); k<n; ++k)
    for (unsigned int i=k+1-v1.size(); i<=k && i<v2.size(); ++i)
      ret[k] += v1[k-i] * v2[i];

  return ret;
}

template <class T1, class T2, class U>
vnl_vector<U> vnl_convolve(vnl_vector<T1> const& v1, vnl_vector<T2> const& v2, U*, int use_fft)
{
  // Quick return if possible:
  if (v1.size() == 0 || v2.size() == 0)
    return vnl_vector<U>(0);

  if (use_fft != 0)
    return vnl_convolve_using_fft(v1, v2, (U*)0, use_fft);

  unsigned int n = v1.size() + v2.size() - 1;
  vnl_vector<U> ret(n, (U)0); // all elements already initialized to zero
  for (unsigned int k=0; k<v1.size(); ++k)
    for (unsigned int i=0; i<=k && i<v2.size(); ++i)
      ret[k] += U(v1[k-i]) * U(v2[i]);
  for (unsigned int k=v1.size(); k<n; ++k)
    for (unsigned int i=k+1-v1.size(); i<=k && i<v2.size(); ++i)
      ret[k] += U(v1[k-i]) * U(v2[i]);

  return ret;
}

#undef VNL_CONVOLVE_INSTANTIATE
#define VNL_CONVOLVE_INSTANTIATE_2(T,U) \
template vnl_vector<U > vnl_convolve(vnl_vector<T > const&, vnl_vector<U > const&, U*, int); \
template vnl_vector<U > vnl_convolve_cyclic(vnl_vector<T > const&, vnl_vector<U > const&, U*, bool)

#define VNL_CONVOLVE_INSTANTIATE(T,U) \
VNL_CONVOLVE_INSTANTIATE_2(T,U); \
template vnl_vector<T > vnl_convolve(vnl_vector<T > const&, vnl_vector<T > const&, int)

#endif // vnl_convolve_txx_
