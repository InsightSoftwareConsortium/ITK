// This is vxl/vnl/vnl_fortran_copy.txx

// vnl_fortran_copy
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 29 Aug 96
//
//-----------------------------------------------------------------------------

#include "vnl_fortran_copy.h"
#include <vnl/vnl_vector.h>

//: Generate a fortran column-storage matrix from the given matrix.
template <class T>
vnl_fortran_copy<T>::vnl_fortran_copy(vnl_matrix<T> const & M)
{
  unsigned n = M.rows();
  unsigned p = M.columns();

  data = vnl_c_vector<T>::allocate_T(sz = n*p);
  T *d = data;
  for(unsigned j = 0; j < p; ++j)
    for(unsigned i = 0; i < n; ++i)
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
