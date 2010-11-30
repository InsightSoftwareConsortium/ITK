// This is core/vnl/vnl_c_na_vector.txx
#ifndef vnl_c_na_vector_txx_
#define vnl_c_na_vector_txx_
//:
// \file
// \author Andrew W. Fitzgibbon, Ian Scott
// \date   3 Nov 2010
//
//-----------------------------------------------------------------------------

#include "vnl_c_na_vector.h"
#include <vcl_cmath.h>     // vcl_sqrt()
#include <vnl/vnl_math.h>
#include <vnl/vnl_na.h>
#include <vnl/vnl_complex_traits.h>
#include <vnl/vnl_numeric_traits.h>


template <class T>
T vnl_c_na_vector<T>::sum(T const* v, unsigned n)
{
  T tot(0);
  bool any_valid(false);
  for (const T* end = v+n; v != end; v++)
  {
    if (!vnl_na_isna(*v))
    {
      tot += *v;
      any_valid=true;
    }
  }
  return any_valid ? tot : vnl_na(T());
}

template <class T>
T vnl_c_na_vector<T>::mean(T const *p, unsigned n)
{
  T tot(0);
  unsigned n_finite=0;
  for (const T* end = p+n; p != end; p++)
    if (!vnl_na_isna(*p))
    {
      tot += *p;
      n_finite++;
    }
  return n_finite ? tot/abs_t(n_finite) : vnl_na(T());
}


//------------------------------------------------------------


template <class T, class S>
void vnl_c_na_vector_two_norm_squared(T const *p, unsigned n, S *out)
{
  S val = 0;
  bool any_valid(false);
  for(T const * end = p+n; p != end; p++)
  {
    if (!vnl_na_isna(*p))
    {
      val += S(vnl_math_squared_magnitude(*p));
      any_valid=true;
    }
  }
  *out = any_valid ? val : vnl_na(T());
}

template <class T, class S>
void vnl_c_na_vector_rms_norm(T const *p, unsigned n, S *out)
{
  S val = 0;
  unsigned n_finite=0;
  for (T const* end = p+n; p != end; p++)
  {
    if (!vnl_na_isna(*p))
    {
      val += S(vnl_math_squared_magnitude(*p));
      n_finite++;
    }
  }
  typedef typename vnl_numeric_traits<S>::real_t real_t;
  *out = n_finite ? S(vcl_sqrt(real_t(val/n_finite))) : vnl_na(T());
}

template <class T, class S>
void vnl_c_na_vector_one_norm(T const *p, unsigned n, S *out)
{
  T val = 0;
  bool any_valid(false);
  for (T const* end = p+n; p != end; p++)
  {
    if (!vnl_na_isna(*p))
    {
      val += vnl_math_abs(*p++);
      any_valid=true;
    }
  }
  *out = any_valid ? val : vnl_na(T());
}

template <class T, class S>
void vnl_c_na_vector_two_norm(T const *p, unsigned n, S *out)
{
  vnl_c_na_vector_two_norm_squared(p, n, out);
  typedef typename vnl_numeric_traits<S>::real_t real_t;
  *out = S(vcl_sqrt(real_t(*out)));
}


template <class T, class S>
void vnl_c_na_vector_inf_norm(T const *p, unsigned n, S *out)
{
  T val = 0;
  bool any_valid(false);
  for (T const* end = p+n; p != end; p++)
  {
    S v = vnl_math_abs(*p);
    if (v > val) // don't need to test for NA, because NA > x is always false.
    {
      v = val;
      any_valid=true;
    }
  }
  *out = any_valid ? val : vnl_na(T());
}


//---------------------------------------------------------------------------

template<class T>
vcl_ostream& print_na_vector(vcl_ostream& s, T const* v, unsigned size)
{
  if (size != 0) vnl_na_insert(s, *v++);
  for (T const* end = v+size-1; v != end; v++)
  {
    s << ' ';
    vnl_na_insert(s, *v);  // Output data element
  }
  return s;
}

//---------------------------------------------------------------------------

#define VNL_C_NA_VECTOR_INSTANTIATE_norm(T, S) \
template void vnl_c_na_vector_two_norm_squared(T const *, unsigned, S *); \
template void vnl_c_na_vector_two_norm(T const *, unsigned, S *); \
template void vnl_c_na_vector_one_norm(T const *, unsigned, S *); \
template void vnl_c_na_vector_rms_norm(T const *, unsigned, S *); \
template void vnl_c_na_vector_inf_norm(T const *, unsigned, S *)

#undef VNL_C_NA_VECTOR_INSTANTIATE_ordered
#define VNL_C_NA_VECTOR_INSTANTIATE_ordered(T) \
VNL_C_NA_VECTOR_INSTANTIATE_norm(T, vnl_c_na_vector<T >::abs_t); \
template class vnl_c_na_vector<T >; \
template vcl_ostream& print_na_vector(vcl_ostream &,T const *,unsigned)


#undef VNL_C_NA_VECTOR_INSTANTIATE_unordered
#define VNL_C_NA_VECTOR_INSTANTIATE_unordered(T)

#if 0
VCL_DO_NOT_INSTANTIATE(T vnl_c_na_vector<T >::max_value(T const *, unsigned), T(0)); \
VCL_DO_NOT_INSTANTIATE(T vnl_c_na_vector<T >::min_value(T const *, unsigned), T(0)); \
VCL_DO_NOT_INSTANTIATE(unsigned vnl_c_na_vector<T >::arg_max(T const *, unsigned), 0U); \
VCL_DO_NOT_INSTANTIATE(unsigned vnl_c_na_vector<T >::arg_min(T const *, unsigned), 0U); \
VNL_C_NA_VECTOR_INSTANTIATE_norm(T, vnl_c_na_vector<T >::abs_t); \
template class vnl_c_na_vector<T >; \
VCL_UNINSTANTIATE_SPECIALIZATION(T vnl_c_na_vector<T >::max_value(T const *, unsigned)); \
VCL_UNINSTANTIATE_SPECIALIZATION(T vnl_c_na_vector<T >::min_value(T const *, unsigned)); \
VCL_UNINSTANTIATE_SPECIALIZATION(unsigned vnl_c_na_vector<T >::arg_max(T const *, unsigned)); \
VCL_UNINSTANTIATE_SPECIALIZATION(unsigned vnl_c_na_vector<T >::arg_min(T const *, unsigned))
#endif

#ifndef DOXYGEN_SHOULD_SKIP_THIS
#undef VNL_C_NA_VECTOR_INSTANTIATE
#define VNL_C_NA_VECTOR_INSTANTIATE(T) extern "no such macro; use e.g. VNL_C_NA_VECTOR_INSTANTIATE_ordered instead"
#endif // DOXYGEN_SHOULD_SKIP_THIS

#endif // vnl_c_na_vector_txx_
