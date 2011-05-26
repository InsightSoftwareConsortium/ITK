// This is core/vnl/vnl_fortran_copy.txx
#ifndef vnl_fortran_copy_txx_
#define vnl_fortran_copy_txx_
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   29 Aug 96
//-----------------------------------------------------------------------------

#include "vnl_fortran_copy.h"

//: Generate a fortran column-storage matrix from the given matrix.
template <class T>
vnl_fortran_copy<T>::vnl_fortran_copy(vnl_matrix<T> const & M)
{
  unsigned n = M.rows();
  unsigned p = M.columns();

  data = vnl_c_vector<T>::allocate_T(sz = n*p);
  T *d = data;
  for (unsigned j = 0; j < p; ++j)
    for (unsigned i = 0; i < n; ++i)
      *d++ = M(i,j);
}

//: Destructor
template <class T>
vnl_fortran_copy<T>::~vnl_fortran_copy()
{
  vnl_c_vector<T>::deallocate(data, sz);
}

//--------------------------------------------------------------------------------

#undef VNL_FORTRAN_COPY_INSTANTIATE
#define VNL_FORTRAN_COPY_INSTANTIATE(T) template class vnl_fortran_copy<T >

#endif // vnl_fortran_copy_txx_
