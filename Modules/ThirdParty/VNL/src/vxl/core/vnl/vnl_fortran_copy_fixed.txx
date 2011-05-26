// This is core/vnl/vnl_fortran_copy_fixed.txx
#ifndef vnl_fortran_copy_fixed_txx_
#define vnl_fortran_copy_fixed_txx_
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   29 Aug 96
//-----------------------------------------------------------------------------

#include "vnl_fortran_copy_fixed.h"

//: Generate a fortran column-storage matrix from the given matrix.
template <class T, unsigned int R, unsigned int C>
vnl_fortran_copy_fixed<T,R,C>::vnl_fortran_copy_fixed(vnl_matrix_fixed<T,R,C> const & M)
{
  T *d = data;
  for (unsigned j = 0; j < C; ++j)
    for (unsigned i = 0; i < R; ++i)
      *d++ = M(i,j);
}


//--------------------------------------------------------------------------------

#undef VNL_FORTRAN_COPY_FIXED_INSTANTIATE
#define VNL_FORTRAN_COPY_FIXED_INSTANTIATE(T , R , C ) template class vnl_fortran_copy_fixed<T , R , C >

#endif // vnl_fortran_copy_fixed_txx_
