/*
  fsm@robots.ox.ac.uk
*/
#ifdef __GNUC__
#pragma implementation
#endif
#include "vnl_determinant.h"

#include <vcl_cassert.h>
#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_qr.h>

template <class T>
T vnl_determinant(T const * const * rows, unsigned n) {
  switch (n) {
  case 1: return rows[0][0];
  case 2: return vnl_determinant(rows[0], rows[1]);
  case 3: return vnl_determinant(rows[0], rows[1], rows[2]);
  case 4: return vnl_determinant(rows[0], rows[1], rows[2], rows[3]);
  default:
    { // for largish matrices it's better to use a matrix decomposition.
      vnl_matrix<T> tmp(n,n); // copy, not ref, as we can't assume the rows are contiguous
      for (unsigned i=0; i<n; ++i)
        tmp.set_row(i, rows[i]);
      return vnl_determinant(tmp);
    }
  }
}

template <class T>
T vnl_determinant(vnl_matrix<T> const &M) {
  unsigned n = M.rows();
  assert(M.cols() == n);
  if (n<=4)
    return vnl_determinant(M.data_array(), n);
  else
    return vnl_qr<T>(M).determinant();
}

//--------------------------------------------------------------------------------

// this macro is *not* intended for client use. so
// it can be called whatever it likes.
#define VNL_ALGO_DETERMINANT_INSTANTIATE(T) \
template T vnl_determinant(T const * const *, unsigned); \
template T vnl_determinant(vnl_matrix<T > const &);

#include <vcl_complex.h>

// QR only works for floating point data types.
VNL_ALGO_DETERMINANT_INSTANTIATE(float);
VNL_ALGO_DETERMINANT_INSTANTIATE(double);
VNL_ALGO_DETERMINANT_INSTANTIATE(vcl_complex<float>);
VNL_ALGO_DETERMINANT_INSTANTIATE(vcl_complex<double>);
