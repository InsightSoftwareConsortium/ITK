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
#include <vnl/vnl_math.h>  // for vnl_math::isfinite


//------------------------------------------------------------
template <typename T>
vnl_vector_ref<T>::vnl_vector_ref(unsigned n, T *space) : vnl_vector<T>()
{
  Base::data = space;
  Base::num_elmts = n;
}

template <typename T>
vnl_vector_ref<T>::vnl_vector_ref(vnl_vector_ref<T> const& v) : vnl_vector<T>()
{
  Base::data = const_cast<T*>(v.data_block()); // const incorrect!
  Base::num_elmts = v.size();
}

template <typename T>
vnl_vector_ref<T>::~vnl_vector_ref()
{
  Base::data = nullptr;
}

template <typename T>
vnl_vector_ref<T>& vnl_vector_ref<T>::non_const()
{
  return *this;
}

#define VNL_VECTOR_REF_INSTANTIATE(T) \
template class vnl_vector_ref<T >

#endif // vnl_vector_ref_hxx_
