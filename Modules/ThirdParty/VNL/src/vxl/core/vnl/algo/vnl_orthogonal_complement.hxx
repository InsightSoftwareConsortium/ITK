// This is core/vnl/algo/vnl_orthogonal_complement.hxx
#ifndef vnl_orthogonal_complement_hxx_
#define vnl_orthogonal_complement_hxx_
/*
  fsm
*/
#include "vnl_orthogonal_complement.h"
#include <vnl/algo/vnl_svd.h>

template <class T>
vnl_matrix<T> vnl_orthogonal_complement(vnl_vector<T> const &v)
{
  unsigned n = v.size();
  vnl_matrix<T> tmp(1, n);
  tmp.set_row(0, v);
  return vnl_svd<T>(tmp).V().extract(n, n-1, 0, 1);
}

//--------------------------------------------------------------------------------

#undef VNL_ORTHOGONAL_COMPLEMENT_INSTANTIATE
#define VNL_ORTHOGONAL_COMPLEMENT_INSTANTIATE(T) \
/* template vnl_matrix<T > vnl_orthogonal_complement(vnl_matrix<T > const &); */ \
template VNL_ALGO_EXPORT vnl_matrix<T > vnl_orthogonal_complement(vnl_vector<T > const &)

#endif // vnl_orthogonal_complement_hxx_
