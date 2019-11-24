// This is core/vnl/vnl_vector_ref.hxx
#ifndef vnl_vector_ref_hxx_
#define vnl_vector_ref_hxx_
#include <algorithm>
#include <iostream>
#include <cstdlib>
#include "vnl_vector_ref.h"
#include <cassert>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include "vnl_math.h"  // for vnl_math::isfinite


//------------------------------------------------------------
template <typename T>
vnl_vector_ref<T>::vnl_vector_ref(size_t n, T * space)
 : vnl_vector<T>(n, space, false)
{}

template <typename T>
vnl_vector_ref<T>::vnl_vector_ref(const vnl_vector_ref<T> & v)
  : vnl_vector<T>(v.size(), const_cast<T *>(v.data_block()),false)
{}

template <typename T>
vnl_vector_ref<T>& vnl_vector_ref<T>::non_const()
{
  return *this;
}

#define VNL_VECTOR_REF_INSTANTIATE(T) \
template class vnl_vector_ref<T >

#endif // vnl_vector_ref_hxx_
