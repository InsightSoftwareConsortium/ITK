#ifndef vnl_orthogonal_complement_txx_
#define vnl_orthogonal_complement_txx_
/*
  fsm@robots.ox.ac.uk
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

//template <typename T>
//vnl_matrix<T> vnl_orthogonal_complement(vnl_matrix<T> const &M)

//--------------------------------------------------------------------------------

#undef VNL_ORTHOGONAL_COMPLEMENT_INSTANTIATE
#define VNL_ORTHOGONAL_COMPLEMENT_INSTANTIATE(T) \
template vnl_matrix<T > vnl_orthogonal_complement(vnl_vector<T > const &);
//template vnl_matrix<T > vnl_orthogonal_complement(vnl_matrix<T > const &);

#endif
