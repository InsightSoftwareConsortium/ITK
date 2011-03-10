// This is core/vnl/vnl_c_vector.txx
#ifndef vnl_c_vector_txx_
#define vnl_c_vector_txx_
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   12 Feb 1998
//
//-----------------------------------------------------------------------------

#include "vnl_c_vector.h"
#include <vcl_cmath.h>     // vcl_sqrt()
#include <vcl_cassert.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_complex_traits.h>
#include <vnl/vnl_numeric_traits.h>

#include <vnl/vnl_sse.h>

template <class T>
T vnl_c_vector<T>::sum(T const* v, unsigned n)
{
  return vnl_sse<T>::sum(v,n);
}

template <class T>
void vnl_c_vector<T>::normalize(T* v, unsigned n)
{
  typedef typename vnl_numeric_traits<T>::abs_t abs_t;
  typedef typename vnl_numeric_traits<abs_t>::real_t real_t;
  abs_t tmp(0);
  for (unsigned i = 0; i < n; ++i)
    tmp += vnl_math_squared_magnitude(v[i]);
  if (tmp!=0)
  {
    tmp = abs_t(real_t(1) / vcl_sqrt(real_t(tmp)));
    for (unsigned i = 0; i < n; ++i)
      v[i] = T(tmp*v[i]);
  }
}

template <class T>
void vnl_c_vector<T>::apply(T const* v, unsigned n, T (*f)(T const&), T* v_out)
{
  for (unsigned i = 0; i < n; ++i)
    v_out[i] = f(v[i]);
}

template <class T>
void vnl_c_vector<T>::apply(T const* v, unsigned n, T (*f)(T), T* v_out)
{
  for (unsigned i = 0; i < n; ++i)
    v_out[i] = f(v[i]);
}

template <class T>
void vnl_c_vector<T>::copy(T const *src, T *dst, unsigned n)
{
  for (unsigned i=0; i<n; ++i)
    dst[i] = src[i];
}

template <class T>
void vnl_c_vector<T>::scale(T const *x, T *y, unsigned n, T const &a_)
{
  T a = a_;
  if (x == y)
    for (unsigned i=0; i<n; ++i)
      y[i] *= a;
  else
    for (unsigned i=0; i<n; ++i)
      y[i] = a*x[i];
}

//----------------------------------------------------------------------------
#ifndef DOXYGEN_SHOULD_SKIP_THIS
#define impl_elmt_wise_commutative(op) \
  if (z == x) \
    for (unsigned i=0; i<n; ++i) \
      z[i] op##= y[i]; \
  else if (z == y) \
    for (unsigned i=0; i<n; ++i) \
      z[i] op##= x[i]; \
  else \
    for (unsigned i=0; i<n; ++i) \
      z[i] = x[i] op y[i];

#define impl_elmt_wise_non_commutative(op) \
  if (z == x) \
    for (unsigned i=0; i<n; ++i) \
      z[i] op##= y[i]; \
  else \
    for (unsigned i=0; i<n; ++i) \
      z[i] = x[i] op y[i];

#define impl_elmt_wise_commutative_a(op) \
  if (z == x) \
    for (unsigned i=0; i<n; ++i) \
      z[i] op##= y; \
  else \
    for (unsigned i=0; i<n; ++i) \
      z[i] = x[i] op y;

#define impl_elmt_wise_non_commutative_a(op) \
  if (z == x) \
    for (unsigned i=0; i<n; ++i) \
      z[i] op##= y; \
  else \
    for (unsigned i=0; i<n; ++i) \
      z[i] = x[i] op y;
#endif // DOXYGEN_SHOULD_SKIP_THIS

template <class T>
void vnl_c_vector<T>::add(T const *x, T const *y, T *z, unsigned n)
{
  impl_elmt_wise_commutative(+);
}

template <class T>
void vnl_c_vector<T>::add(T const *x, T const& y, T *z, unsigned n)
{
  impl_elmt_wise_commutative_a(+);
}

template <class T>
void vnl_c_vector<T>::subtract(T const *x, T const *y, T *z, unsigned n)
{
  impl_elmt_wise_non_commutative(-);
}

template <class T>
void vnl_c_vector<T>::subtract(T const *x, T const& y, T *z, unsigned n)
{
  impl_elmt_wise_commutative_a(-);
}

template <class T>
void vnl_c_vector<T>::multiply(T const *x, T const *y, T *z, unsigned n)
{
  impl_elmt_wise_commutative(*);
}

template <class T>
void vnl_c_vector<T>::multiply(T const *x, T const& y, T *z, unsigned n)
{
  impl_elmt_wise_commutative_a(*);
}

template <class T>
void vnl_c_vector<T>::divide(T const *x, T const *y, T *z, unsigned n)
{
  impl_elmt_wise_non_commutative(/);
}

template <class T>
void vnl_c_vector<T>::divide(T const *x, T const& y, T *z, unsigned n)
{
  impl_elmt_wise_commutative_a(/);
}

#undef impl_elmt_wise_commutative
#undef impl_elmt_wise_noncommutative
//--------------------------------------------------------------------------

template <class T>
void vnl_c_vector<T>::negate(T const *x, T *y, unsigned n)
{
  if (x == y)
    for (unsigned i=0; i<n; ++i)
      y[i] = -y[i];
  else
    for (unsigned i=0; i<n; ++i)
      y[i] = -x[i];
}

template <class T>
void vnl_c_vector<T>::invert(T const *x, T *y, unsigned n)
{
  if (x == y)
    for (unsigned i=0; i<n; ++i)
      y[i] = T(1)/y[i];
  else
    for (unsigned i=0; i<n; ++i)
      y[i] = T(1)/x[i];
}

template <class T>
void vnl_c_vector<T>::saxpy(T const &a_, T const *x, T *y, unsigned n)
{
  T a = a_;
  for (unsigned i=0; i<n; ++i)
    y[i] += a*x[i];
}

template <class T>
void vnl_c_vector<T>::fill(T *x, unsigned n, T const &v_)
{
  T v = v_;
  for (unsigned i=0; i<n; ++i)
    x[i] = v;
}

template <class T>
void vnl_c_vector<T>::reverse(T *x, unsigned n)
{
  for (unsigned i=0; 2*i+1<n; ++i) {
    T tmp = x[i];
    x[i] = x[n-1-i];
    x[n-1-i] = tmp;
  }
}

// non-conjugating "dot" product.
template<class T>
T vnl_c_vector<T>::dot_product(T const *a, T const *b, unsigned n)
{
  return vnl_sse<T>::dot_product(a,b,n);
}

// conjugating "dot" product.
template<class T>
T vnl_c_vector<T>::inner_product(T const *a, T const *b, unsigned n)
{
  T ip(0);
  for (unsigned i=0; i<n; ++i)
    ip += a[i] * vnl_complex_traits<T>::conjugate(b[i]);
  return ip;
}

// conjugates one block of data into another block.
template<class T>
void vnl_c_vector<T>::conjugate(T const *src, T *dst, unsigned n)
{
  for (unsigned i=0; i<n; ++i)
    dst[i] = vnl_complex_traits<T>::conjugate( src[i] );
}

//------------------------------------------------------------------------------

//: Returns max value of the vector.
template<class T>
T vnl_c_vector<T>::max_value(T const *src, unsigned n)
{
  assert(n!=0); // max_value of an empty vector is undefined
  return vnl_sse<T>::max(src,n);
}

//: Returns min value of the vector.
template<class T>
T vnl_c_vector<T>::min_value(T const *src, unsigned n)
{
  assert(n!=0); // min_value of an empty vector is undefined
  return vnl_sse<T>::min(src,n);
}

//: Returns location of max value of the vector.
template<class T>
unsigned vnl_c_vector<T>::arg_max(T const *src, unsigned n)
{
  assert(n!=0); // max value of an empty vector is undefined
  return vnl_sse<T>::arg_max(src,n);
}

//: Returns location of min value of the vector.
template<class T>
unsigned vnl_c_vector<T>::arg_min(T const *src, unsigned n)
{
  assert(n!=0); // min value of an empty vector is undefined
  return vnl_sse<T>::arg_min(src,n);
}

//: Sum of Differences squared.
template<class T>
T vnl_c_vector<T>::euclid_dist_sq(T const *a, T const *b, unsigned n)
{
  return vnl_sse<T>::euclid_dist_sq(a,b,n);
}

template <class T>
T vnl_c_vector<T>::sum_sq_diff_means(T const* v, unsigned n)
{
  T sum(0);
  T sum_sq(0);
  for (unsigned i = 0; i < n; ++i, ++v)
  {
    sum += *v;
    sum_sq += *v * *v;
  }
  typedef typename vnl_numeric_traits<T>::abs_t abs_t;
  return sum_sq - sum*sum / abs_t(n);
}

//------------------------------------------------------------

template <class T, class S>
void vnl_c_vector_two_norm_squared(T const *p, unsigned n, S *out)
{
#if 1
  // IMS: MSVC's optimiser does much better with *p++ than with p[i];
  // consistently about 30% better over vectors from 4 to 20000 dimensions.
  // PVr: with gcc 3.0 on alpha this is even a factor 3 faster!
  S val = 0;
  T const* end = p+n;
  while (p != end)
    val += S(vnl_math_squared_magnitude(*p++));
  *out = val;
#else
  *out = 0;
  for (unsigned i=0; i<n; ++i)
    *out += vnl_math_squared_magnitude(p[i]);
#endif
}

template <class T, class S>
void vnl_c_vector_rms_norm(T const *p, unsigned n, S *out)
{
  vnl_c_vector_two_norm_squared(p, n, out);
  *out /= n;
  typedef typename vnl_numeric_traits<S>::real_t real_t;
  *out = S(vcl_sqrt(real_t(*out)));
}

template <class T, class S>
void vnl_c_vector_one_norm(T const *p, unsigned n, S *out)
{
  *out = 0;
  T const* end = p+n;
  while (p != end)
    *out += vnl_math_abs(*p++);
}

template <class T, class S>
void vnl_c_vector_two_norm(T const *p, unsigned n, S *out)
{
  vnl_c_vector_two_norm_squared(p, n, out);
  typedef typename vnl_numeric_traits<S>::real_t real_t;
  *out = S(vcl_sqrt(real_t(*out)));
}

template <class T, class S>
void vnl_c_vector_inf_norm(T const *p, unsigned n, S *out)
{
  *out = 0;
  T const* end = p+n;
  while (p != end) {
    S v = vnl_math_abs(*p++);
    if (v > *out)
      *out = v;
  }
}


//---------------------------------------------------------------------------


inline void* vnl_c_vector_alloc(int n, int size)
{
  return vnl_sse_alloc(n,size);
}


inline void vnl_c_vector_dealloc(void* v, int n, int size)
{
  vnl_sse_dealloc(v,n,size);
}

template<class T>
T** vnl_c_vector<T>::allocate_Tptr(int n)
{
  return (T**)vnl_c_vector_alloc(n, sizeof (T*));
}

template<class T>
void vnl_c_vector<T>::deallocate(T** v, int n)
{
  vnl_c_vector_dealloc(v, n, sizeof (T*));
}

// "T *" is POD, but "T" might not be.
#include <vcl_new.h>
template <class T> inline void vnl_c_vector_construct(T *p, int n)
{
  for (int i=0; i<n; ++i)
    new (p+i) T();
}

inline void vnl_c_vector_construct(float *, int) { }
inline void vnl_c_vector_construct(double *, int) { }
inline void vnl_c_vector_construct(long double *, int) { }
inline void vnl_c_vector_construct(vcl_complex<float> *, int) { }
inline void vnl_c_vector_construct(vcl_complex<double> *, int) { }
inline void vnl_c_vector_construct(vcl_complex<long double> *, int) { }

#ifdef __BORLANDC__
// The compiler is confused
# pragma option push -w-8057
// Warning W8057 vnl/vnl_c_vector.txx 414:
// Parameter 'p' is never used in function
// vnl_c_vector_destruct<int>(int *,int)
#endif


template <class T> inline void vnl_c_vector_destruct(T *p, int n)
{
  for (int i=0; i<n; ++i)
    (p+i)->~T();
}

#ifdef __BORLANDC__
# pragma option pop
#endif


inline void vnl_c_vector_destruct(float *, int) { }
inline void vnl_c_vector_destruct(double *, int) { }
inline void vnl_c_vector_destruct(long double *, int) { }
inline void vnl_c_vector_destruct(vcl_complex<float> *, int) { }
inline void vnl_c_vector_destruct(vcl_complex<double> *, int) { }
inline void vnl_c_vector_destruct(vcl_complex<long double> *, int) { }

template<class T>
T* vnl_c_vector<T>::allocate_T(int n)
{
  T *p = (T*)vnl_c_vector_alloc(n, sizeof (T));
  vnl_c_vector_construct(p, n);
  return p;
}

template<class T>
void vnl_c_vector<T>::deallocate(T* p, int n)
{
  vnl_c_vector_destruct(p, n);
  vnl_c_vector_dealloc(p, n, sizeof (T));
}

template<class T>
vcl_ostream& print_vector(vcl_ostream& s, T const* v, unsigned size)
{
  if (size != 0) s << v[0];
  for (unsigned i = 1; i < size; ++i)   // For each index in vector
    s << ' ' << v[i];                   // Output data element
  return s;
}

//---------------------------------------------------------------------------

#define VNL_C_VECTOR_INSTANTIATE_norm(T, S) \
template void vnl_c_vector_two_norm_squared(T const *, unsigned, S *); \
template void vnl_c_vector_rms_norm(T const *, unsigned, S *); \
template void vnl_c_vector_one_norm(T const *, unsigned, S *); \
template void vnl_c_vector_two_norm(T const *, unsigned, S *); \
template void vnl_c_vector_inf_norm(T const *, unsigned, S *)

#undef VNL_C_VECTOR_INSTANTIATE_ordered
#define VNL_C_VECTOR_INSTANTIATE_ordered(T) \
VNL_C_VECTOR_INSTANTIATE_norm(T, vnl_c_vector<T >::abs_t); \
template class vnl_c_vector<T >; \
template vcl_ostream& print_vector(vcl_ostream &,T const *,unsigned)


#undef VNL_C_VECTOR_INSTANTIATE_unordered
#define VNL_C_VECTOR_INSTANTIATE_unordered(T) \
VCL_DO_NOT_INSTANTIATE(T vnl_c_vector<T >::max_value(T const *, unsigned), T(0)); \
VCL_DO_NOT_INSTANTIATE(T vnl_c_vector<T >::min_value(T const *, unsigned), T(0)); \
VCL_DO_NOT_INSTANTIATE(unsigned vnl_c_vector<T >::arg_max(T const *, unsigned), 0U); \
VCL_DO_NOT_INSTANTIATE(unsigned vnl_c_vector<T >::arg_min(T const *, unsigned), 0U); \
VNL_C_VECTOR_INSTANTIATE_norm(T, vnl_c_vector<T >::abs_t); \
template class vnl_c_vector<T >; \
VCL_UNINSTANTIATE_SPECIALIZATION(T vnl_c_vector<T >::max_value(T const *, unsigned)); \
VCL_UNINSTANTIATE_SPECIALIZATION(T vnl_c_vector<T >::min_value(T const *, unsigned)); \
VCL_UNINSTANTIATE_SPECIALIZATION(unsigned vnl_c_vector<T >::arg_max(T const *, unsigned)); \
VCL_UNINSTANTIATE_SPECIALIZATION(unsigned vnl_c_vector<T >::arg_min(T const *, unsigned))

#ifndef DOXYGEN_SHOULD_SKIP_THIS
#undef VNL_C_VECTOR_INSTANTIATE
#define VNL_C_VECTOR_INSTANTIATE(T) extern "no such macro; use e.g. VNL_C_VECTOR_INSTANTIATE_ordered instead"
#endif // DOXYGEN_SHOULD_SKIP_THIS

#endif // vnl_c_vector_txx_
