#ifndef vnl_adjugate_txx_
#define vnl_adjugate_txx_
//:
// \file
// \author fsm

#include "vnl_adjugate.h"
#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_determinant.h>

// This is a rudimentary implementation. It could be improved by noting
// that adj(A B) = adj(B) adj(A) for all matrices A, B (invertible or
// not) and then using a matrix decomposition for larger matrices.
//
// E.g. using a singular value decomposition A = U D V^* gives
// adj(A) = V adj(D) U^*.
//
// On the other hand, SVD decomposition makes no sense for e.g. integer matrices
// and we want to keep T as general as possible.

template <class T>
void vnl_adjugate(vnl_matrix<T> const &A, vnl_matrix<T> *out)
{
  int n = A.rows();
  A.assert_size(n, n);
  out->assert_size(n, n);

  vnl_matrix<T> sub(n-1, n-1);
  for (int i=0; i<n; ++i)
    for (int j=0; j<n; ++j)
    {
      for (int u=0; u<n-1; ++u)
        for (int v=0; v<n-1; ++v)
          sub[u][v] = A[v+(v<i?0:1)][u+(u<j?0:1)];
      (*out)[i][j] = vnl_determinant(sub, false);
    }
}

template <class T>
vnl_matrix<T> vnl_adjugate(vnl_matrix<T> const &A)
{
  vnl_matrix<T> adj(A.rows(), A.cols());
  vnl_adjugate(A, &adj);
  return adj;
}

//--------------------------------------------------------------------------------

#undef VNL_ADJUGATE_INSTANTIATE
#define VNL_ADJUGATE_INSTANTIATE(T) \
template void vnl_adjugate(vnl_matrix<T > const &, vnl_matrix<T > *); \
template vnl_matrix<T > vnl_adjugate(vnl_matrix<T > const &)

#endif
