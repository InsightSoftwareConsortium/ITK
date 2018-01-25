// This is core/vnl/vnl_power.h
#ifndef vnl_power_h_
#define vnl_power_h_
//:
// \file
// \brief Calculates nth power of a small vnl_matrix_fixed (not using svd)
// \author Peter Vanroose
// \date   21 July 2009
//
// \verbatim
//  Modifications
//   <none yet>
// \endverbatim

#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_inverse.h> // used for negative powers
#include <vcl_cassert.h>
#include "vnl/vnl_export.h"

//: Calculates nth power of a vnl_matrix_fixed (not using svd)
//  This allows you to write e.g.
//
//  x = vnl_power(A,7) * vnl_power(B,-4) * b;
//
// Note that this function is inlined (except for the call to vnl_inverse()),
// which makes it much faster than a full-fledged square matrix power
// implementation using svd, which belongs in vnl/algo.
//
//  \relatesalso vnl_matrix_fixed

template <class T, unsigned int d> VNL_TEMPLATE_EXPORT
vnl_matrix_fixed<T,d,d> vnl_power(vnl_matrix_fixed<T,d,d> const& m, int n)
{
  assert(n >= 0 || d <= 4); // to allow the use of vnl_inverse()
  if (n == 0)
    return vnl_matrix_fixed<T,d,d>().set_identity();
  else if (n == 1 || m.is_identity())
    return m;
  else if (n < 0)
    return vnl_inverse(vnl_power(m, -n));
  else {
    vnl_matrix_fixed<T,d,d> r = vnl_power(m, n/2);
    return n%2==0 ? r * r : r * r * m;
  }
}

//: Calculates nth power of a square vnl_matrix (not using svd)
//  This allows you to write e.g.
//
//  x = vnl_power(A,7) * vnl_power(B,-4) * b;
//
// Note that this function is inlined (except for the call to vnl_inverse()),
// which makes it much faster than a full-fledged square matrix power
// implementation using svd, which belongs in vnl/algo.
//
//  \relatesalso vnl_matrix

template <class T> VNL_TEMPLATE_EXPORT
vnl_matrix<T> vnl_power(vnl_matrix<T> const& m, int n)
{
  assert(m.rows() == m.columns());
  assert(n >= 0 || m.rows() <= 4);
  if (n == 0)
    return vnl_matrix<T>(m.rows(),m.columns()).set_identity();
  else if (n == 1 || m.is_identity())
    return m;
  else if (n < 0 && m.rows() == 1)
    return vnl_power(vnl_matrix_fixed<T,1,1>(m),n).as_ref();
  else if (n < 0 && m.rows() == 2)
    return vnl_power(vnl_matrix_fixed<T,2,2>(m),n).as_ref();
  else if (n < 0 && m.rows() == 3)
    return vnl_power(vnl_matrix_fixed<T,3,3>(m),n).as_ref();
  else if (n < 0 && m.rows() == 4)
    return vnl_power(vnl_matrix_fixed<T,4,4>(m),n).as_ref();
  else {
    vnl_matrix<T> r = vnl_power(m, n/2);
    return n%2==0 ? r * r : r * r * m;
  }
}

#endif // vnl_power_h_
