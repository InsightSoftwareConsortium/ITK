// -*- c++ -*-
#ifndef vcl_gcc295_v3_vector_txx_
#define vcl_gcc295_v3_vector_txx_

#include <vcl_vector.h>

// --- Vector ---
#define VCL_VECTOR_INSTANTIATE(T) \
template class vcl_vector<T >; \
namespace std { template void std::fill(vcl_vector<T >::iterator, vcl_vector<T >::iterator, T const &); } \
namespace { \
  bool tickler(vcl_vector<T >::const_iterator const &a, \
               vcl_vector<T >::iterator const &b) \
  { return (a != a) && (a != b) && (b != a) && (b != b) && (a <= a) && (a <= b) && (b <= a) && (b <= b); } \
}

#endif
