#ifndef vnl_algo_determinant_hxx_
#define vnl_algo_determinant_hxx_
/*
  fsm
*/
#include "vnl_determinant.h"

#include <cassert>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include <vnl/algo/vnl_qr.h>


template <class T>
T vnl_determinant(T const *row0, T const *row1)
{
  return row0[0]*row1[1] - row0[1]*row1[0];
}

template <class T>
T vnl_determinant(T const *row0, T const *row1, T const *row2)
{
  return // the extra '+' makes it work nicely with emacs indentation.
    + row0[0]*row1[1]*row2[2]
    - row0[0]*row2[1]*row1[2]
    - row1[0]*row0[1]*row2[2]
    + row1[0]*row2[1]*row0[2]
    + row2[0]*row0[1]*row1[2]
    - row2[0]*row1[1]*row0[2];
}

template <class T>
T vnl_determinant(T const *row0, T const *row1, T const *row2, T const *row3)
{
  return
    + row0[0]*row1[1]*row2[2]*row3[3]
    - row0[0]*row1[1]*row3[2]*row2[3]
    - row0[0]*row2[1]*row1[2]*row3[3]
    + row0[0]*row2[1]*row3[2]*row1[3]
    + row0[0]*row3[1]*row1[2]*row2[3]
    - row0[0]*row3[1]*row2[2]*row1[3]
    - row1[0]*row0[1]*row2[2]*row3[3]
    + row1[0]*row0[1]*row3[2]*row2[3]
    + row1[0]*row2[1]*row0[2]*row3[3]
    - row1[0]*row2[1]*row3[2]*row0[3]
    - row1[0]*row3[1]*row0[2]*row2[3]
    + row1[0]*row3[1]*row2[2]*row0[3]
    + row2[0]*row0[1]*row1[2]*row3[3]
    - row2[0]*row0[1]*row3[2]*row1[3]
    - row2[0]*row1[1]*row0[2]*row3[3]
    + row2[0]*row1[1]*row3[2]*row0[3]
    + row2[0]*row3[1]*row0[2]*row1[3]
    - row2[0]*row3[1]*row1[2]*row0[3]
    - row3[0]*row0[1]*row1[2]*row2[3]
    + row3[0]*row0[1]*row2[2]*row1[3]
    + row3[0]*row1[1]*row0[2]*row2[3]
    - row3[0]*row1[1]*row2[2]*row0[3]
    - row3[0]*row2[1]*row0[2]*row1[3]
    + row3[0]*row2[1]*row1[2]*row0[3];
}

//--------------------------------------------------------------------------------

template <class T>
T vnl_determinant(vnl_matrix<T> const &M, bool balance)
{
  unsigned n = M.rows();
  assert(M.cols() == n);

  switch (n)
  {
   case 1: return M[0][0];
   case 2: return vnl_determinant(M[0], M[1]);
   case 3: return vnl_determinant(M[0], M[1], M[2]);
   case 4: return vnl_determinant(M[0], M[1], M[2], M[3]);
   default:
    if (balance)
    {
      vnl_matrix<T> tmp(M);
      typedef typename vnl_numeric_traits<T>::abs_t abs_t;
      abs_t scalings(1);
      for (int t=0; t<5; ++t)
      {
        // normalize rows.
        for (unsigned int i=0; i<n; ++i) {
          abs_t rn = tmp.get_row(i).rms();
          if (rn > 0) {
            scalings *= rn;
            tmp.scale_row(i, abs_t(1)/rn);
          }
        }
        // normalize columns.
        for (unsigned int i=0; i<n; ++i) {
          abs_t rn = tmp.get_column(i).rms();
          if (rn > 0) {
            scalings *= rn;
            tmp.scale_column(i, abs_t(1)/rn);
          }
        }
      }
      T balanced_det = vnl_qr<T>(tmp).determinant();
      //std::clog << __FILE__ ": scalings, balanced_det = " << scalings << ", " << balanced_det << std::endl;
      return T(scalings) * balanced_det;
    }
    else
      return vnl_qr<T>(M).determinant();
  }
}


//--------------------------------------------------------------------------------

#define VNL_DETERMINANT_INSTANTIATE_1(T) \
template VNL_ALGO_EXPORT T vnl_determinant(T const *, T const *); \
template VNL_ALGO_EXPORT T vnl_determinant(T const *, T const *, T const *); \
template VNL_ALGO_EXPORT T vnl_determinant(T const *, T const *, T const *, T const *)

#define VNL_DETERMINANT_INSTANTIATE_2(T) \
template VNL_ALGO_EXPORT T vnl_determinant(vnl_matrix<T > const &, bool)

#undef VNL_DETERMINANT_INSTANTIATE
#define VNL_DETERMINANT_INSTANTIATE(T) \
VNL_DETERMINANT_INSTANTIATE_1(T); \
VNL_DETERMINANT_INSTANTIATE_2(T)

#endif // vnl_algo_determinant_hxx_
