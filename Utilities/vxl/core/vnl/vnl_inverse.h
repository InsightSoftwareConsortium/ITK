// This is core/vnl/vnl_inverse.h
#ifndef vnl_inverse_h_
#define vnl_inverse_h_
//:
// \file
// \brief Calculates inverse of a small vnl_matrix_fixed (not using svd)
// \author Peter Vanroose
// \date   22 October 2002
//
// \verbatim
//  Modifications
//   19 April 2003 - PVr - added interface for vnl_matrix<T>
//   19 April 2004 - PVr - made 4x4 implementation a bit more robust (but still incomplete)
//   18 June  2004 - PVr - finally completed 4x4 implementation
//   19 June  2004 - PVr - added vnl_inverse_transpose() methods
// \endverbatim

#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_det.h>
#include <vcl_cassert.h>

//: Calculates inverse of a small vnl_matrix_fixed (not using svd)
//  This allows you to write e.g.
//
//  x = vnl_inverse(A) * b;
//
// Note that this function is inlined (except for the call to vnl_det()),
// which makes it much faster than the vnl_matrix_inverse class in vnl/algo
// since that one is using svd.

template <class T>
vnl_matrix_fixed<T,1,1> vnl_inverse(vnl_matrix_fixed<T,1,1> const& m)
{
  return vnl_matrix_fixed<T,1,1>(T(1)/m(0,0));
}

template <class T>
vnl_matrix_fixed<T,2,2> vnl_inverse(vnl_matrix_fixed<T,2,2> const& m)
{
  T det = vnl_det(m);
  if (det==0) {
    assert(!"Cannot invert 2x2 matrix with zero determinant");
    return vnl_matrix_fixed<T,2,2>();
  }
  det = T(1)/det;
  T d[4];
  d[0] = m(1,1)*det; d[1] = - m(0,1)*det;
  d[3] = m(0,0)*det; d[2] = - m(1,0)*det;
  return vnl_matrix_fixed<T,2,2>(d);
}

template <class T>
vnl_matrix_fixed<T,3,3> vnl_inverse(vnl_matrix_fixed<T,3,3> const& m)
{
  T det = vnl_det(m);
  if (det==0) {
    assert(!"Cannot invert 3x3 matrix with zero determinant");
    return vnl_matrix_fixed<T,3,3>();
  }
  det = T(1)/det;
  T d[9];
  d[0] = (m(1,1)*m(2,2)-m(1,2)*m(2,1))*det;
  d[1] = (m(2,1)*m(0,2)-m(2,2)*m(0,1))*det;
  d[2] = (m(0,1)*m(1,2)-m(0,2)*m(1,1))*det;
  d[3] = (m(1,2)*m(2,0)-m(1,0)*m(2,2))*det;
  d[4] = (m(0,0)*m(2,2)-m(0,2)*m(2,0))*det;
  d[5] = (m(1,0)*m(0,2)-m(1,2)*m(0,0))*det;
  d[6] = (m(1,0)*m(2,1)-m(1,1)*m(2,0))*det;
  d[7] = (m(0,1)*m(2,0)-m(0,0)*m(2,1))*det;
  d[8] = (m(0,0)*m(1,1)-m(0,1)*m(1,0))*det;
  return vnl_matrix_fixed<T,3,3>(d);
}

template <class T>
vnl_matrix_fixed<T,4,4> vnl_inverse(vnl_matrix_fixed<T,4,4> const& m)
{
  T det = vnl_det(m);
  if (det==0) {
    assert(!"Cannot invert 4x4 matrix with zero determinant");
    return vnl_matrix_fixed<T,4,4>();
  }
  det = T(1)/det;
  T d[16];
  d[0] =  m(1,1)*m(2,2)*m(3,3) - m(1,1)*m(2,3)*m(3,2) - m(2,1)*m(1,2)*m(3,3)
        + m(2,1)*m(1,3)*m(3,2) + m(3,1)*m(1,2)*m(2,3) - m(3,1)*m(1,3)*m(2,2);
  d[1] = -m(0,1)*m(2,2)*m(3,3) + m(0,1)*m(2,3)*m(3,2) + m(2,1)*m(0,2)*m(3,3)
        - m(2,1)*m(0,3)*m(3,2) - m(3,1)*m(0,2)*m(2,3) + m(3,1)*m(0,3)*m(2,2);
  d[2] =  m(0,1)*m(1,2)*m(3,3) - m(0,1)*m(1,3)*m(3,2) - m(1,1)*m(0,2)*m(3,3)
        + m(1,1)*m(0,3)*m(3,2) + m(3,1)*m(0,2)*m(1,3) - m(3,1)*m(0,3)*m(1,2);
  d[3] = -m(0,1)*m(1,2)*m(2,3) + m(0,1)*m(1,3)*m(2,2) + m(1,1)*m(0,2)*m(2,3)
        - m(1,1)*m(0,3)*m(2,2) - m(2,1)*m(0,2)*m(1,3) + m(2,1)*m(0,3)*m(1,2);
  d[4] = -m(1,0)*m(2,2)*m(3,3) + m(1,0)*m(2,3)*m(3,2) + m(2,0)*m(1,2)*m(3,3)
        - m(2,0)*m(1,3)*m(3,2) - m(3,0)*m(1,2)*m(2,3) + m(3,0)*m(1,3)*m(2,2);
  d[5] =  m(0,0)*m(2,2)*m(3,3) - m(0,0)*m(2,3)*m(3,2) - m(2,0)*m(0,2)*m(3,3)
        + m(2,0)*m(0,3)*m(3,2) + m(3,0)*m(0,2)*m(2,3) - m(3,0)*m(0,3)*m(2,2);
  d[6] = -m(0,0)*m(1,2)*m(3,3) + m(0,0)*m(1,3)*m(3,2) + m(1,0)*m(0,2)*m(3,3)
        - m(1,0)*m(0,3)*m(3,2) - m(3,0)*m(0,2)*m(1,3) + m(3,0)*m(0,3)*m(1,2);
  d[7] =  m(0,0)*m(1,2)*m(2,3) - m(0,0)*m(1,3)*m(2,2) - m(1,0)*m(0,2)*m(2,3)
        + m(1,0)*m(0,3)*m(2,2) + m(2,0)*m(0,2)*m(1,3) - m(2,0)*m(0,3)*m(1,2);
  d[8] =  m(1,0)*m(2,1)*m(3,3) - m(1,0)*m(2,3)*m(3,1) - m(2,0)*m(1,1)*m(3,3)
        + m(2,0)*m(1,3)*m(3,1) + m(3,0)*m(1,1)*m(2,3) - m(3,0)*m(1,3)*m(2,1);
  d[9] = -m(0,0)*m(2,1)*m(3,3) + m(0,0)*m(2,3)*m(3,1) + m(2,0)*m(0,1)*m(3,3)
        - m(2,0)*m(0,3)*m(3,1) - m(3,0)*m(0,1)*m(2,3) + m(3,0)*m(0,3)*m(2,1);
  d[10]=  m(0,0)*m(1,1)*m(3,3) - m(0,0)*m(1,3)*m(3,1) - m(1,0)*m(0,1)*m(3,3)
        + m(1,0)*m(0,3)*m(3,1) + m(3,0)*m(0,1)*m(1,3) - m(3,0)*m(0,3)*m(1,1);
  d[11]= -m(0,0)*m(1,1)*m(2,3) + m(0,0)*m(1,3)*m(2,1) + m(1,0)*m(0,1)*m(2,3)
        - m(1,0)*m(0,3)*m(2,1) - m(2,0)*m(0,1)*m(1,3) + m(2,0)*m(0,3)*m(1,1);
  d[12]= -m(1,0)*m(2,1)*m(3,2) + m(1,0)*m(2,2)*m(3,1) + m(2,0)*m(1,1)*m(3,2)
        - m(2,0)*m(1,2)*m(3,1) - m(3,0)*m(1,1)*m(2,2) + m(3,0)*m(1,2)*m(2,1);
  d[13]=  m(0,0)*m(2,1)*m(3,2) - m(0,0)*m(2,2)*m(3,1) - m(2,0)*m(0,1)*m(3,2)
        + m(2,0)*m(0,2)*m(3,1) + m(3,0)*m(0,1)*m(2,2) - m(3,0)*m(0,2)*m(2,1);
  d[14]= -m(0,0)*m(1,1)*m(3,2) + m(0,0)*m(1,2)*m(3,1) + m(1,0)*m(0,1)*m(3,2)
        - m(1,0)*m(0,2)*m(3,1) - m(3,0)*m(0,1)*m(1,2) + m(3,0)*m(0,2)*m(1,1);
  d[15]=  m(0,0)*m(1,1)*m(2,2) - m(0,0)*m(1,2)*m(2,1) - m(1,0)*m(0,1)*m(2,2)
        + m(1,0)*m(0,2)*m(2,1) + m(2,0)*m(0,1)*m(1,2) - m(2,0)*m(0,2)*m(1,1);
  return vnl_matrix_fixed<T,4,4>(d)*det;
}

template <class T>
vnl_matrix<T> vnl_inverse(vnl_matrix<T> const& m)
{
  assert(m.rows() == m.columns());
  assert(m.rows() <= 4);
  if (m.rows() == 1)
    return vnl_matrix<T>(1,1, T(1)/m(0,0));
  else if (m.rows() == 2)
    return vnl_matrix<T>(vnl_inverse(vnl_matrix_fixed<T,2,2>(m)));
  else if (m.rows() == 3)
    return vnl_matrix<T>(vnl_inverse(vnl_matrix_fixed<T,3,3>(m)));
  else
    return vnl_matrix<T>(vnl_inverse(vnl_matrix_fixed<T,4,4>(m)));
}

//: Calculates transpose of the inverse of a small vnl_matrix_fixed (not using svd)
//  This allows you to write e.g.
//
//  x = vnl_inverse_transpose(A) * b;
//
// Note that this function is inlined (except for the call to vnl_det()),
// which makes it much faster than the vnl_matrix_inverse class in vnl/algo
// since that one is using svd.  This is also faster than using
//
//  x = vnl_inverse(A).transpose() * b;

template <class T>
vnl_matrix_fixed<T,1,1> vnl_inverse_transpose(vnl_matrix_fixed<T,1,1> const& m)
{
  return vnl_matrix_fixed<T,1,1>(T(1)/m(0,0));
}

template <class T>
vnl_matrix_fixed<T,2,2> vnl_inverse_transpose(vnl_matrix_fixed<T,2,2> const& m)
{
  T det = vnl_det(m);
  if (det==0) {
    assert(!"Cannot invert 2x2 matrix with zero determinant");
    return vnl_matrix_fixed<T,2,2>();
  }
  det = T(1)/det;
  T d[4];
  d[0] = m(1,1)*det; d[2] = - m(0,1)*det;
  d[3] = m(0,0)*det; d[1] = - m(1,0)*det;
  return vnl_matrix_fixed<T,2,2>(d);
}

template <class T>
vnl_matrix_fixed<T,3,3> vnl_inverse_transpose(vnl_matrix_fixed<T,3,3> const& m)
{
  T det = vnl_det(m);
  if (det==0) {
    assert(!"Cannot invert 3x3 matrix with zero determinant");
    return vnl_matrix_fixed<T,3,3>();
  }
  det = T(1)/det;
  T d[9];
  d[0] = (m(1,1)*m(2,2)-m(1,2)*m(2,1))*det;
  d[3] = (m(2,1)*m(0,2)-m(2,2)*m(0,1))*det;
  d[6] = (m(0,1)*m(1,2)-m(0,2)*m(1,1))*det;
  d[1] = (m(1,2)*m(2,0)-m(1,0)*m(2,2))*det;
  d[4] = (m(0,0)*m(2,2)-m(0,2)*m(2,0))*det;
  d[7] = (m(1,0)*m(0,2)-m(1,2)*m(0,0))*det;
  d[2] = (m(1,0)*m(2,1)-m(1,1)*m(2,0))*det;
  d[5] = (m(0,1)*m(2,0)-m(0,0)*m(2,1))*det;
  d[8] = (m(0,0)*m(1,1)-m(0,1)*m(1,0))*det;
  return vnl_matrix_fixed<T,3,3>(d);
}

template <class T>
vnl_matrix_fixed<T,4,4> vnl_inverse_transpose(vnl_matrix_fixed<T,4,4> const& m)
{
  T det = vnl_det(m);
  if (det==0) {
    assert(!"Cannot invert 4x4 matrix with zero determinant");
    return vnl_matrix_fixed<T,4,4>();
  }
  det = T(1)/det;
  T d[16];
  d[0] =  m(1,1)*m(2,2)*m(3,3) - m(1,1)*m(2,3)*m(3,2) - m(2,1)*m(1,2)*m(3,3)
        + m(2,1)*m(1,3)*m(3,2) + m(3,1)*m(1,2)*m(2,3) - m(3,1)*m(1,3)*m(2,2);
  d[4] = -m(0,1)*m(2,2)*m(3,3) + m(0,1)*m(2,3)*m(3,2) + m(2,1)*m(0,2)*m(3,3)
        - m(2,1)*m(0,3)*m(3,2) - m(3,1)*m(0,2)*m(2,3) + m(3,1)*m(0,3)*m(2,2);
  d[8] =  m(0,1)*m(1,2)*m(3,3) - m(0,1)*m(1,3)*m(3,2) - m(1,1)*m(0,2)*m(3,3)
        + m(1,1)*m(0,3)*m(3,2) + m(3,1)*m(0,2)*m(1,3) - m(3,1)*m(0,3)*m(1,2);
  d[12]= -m(0,1)*m(1,2)*m(2,3) + m(0,1)*m(1,3)*m(2,2) + m(1,1)*m(0,2)*m(2,3)
        - m(1,1)*m(0,3)*m(2,2) - m(2,1)*m(0,2)*m(1,3) + m(2,1)*m(0,3)*m(1,2);
  d[1] = -m(1,0)*m(2,2)*m(3,3) + m(1,0)*m(2,3)*m(3,2) + m(2,0)*m(1,2)*m(3,3)
        - m(2,0)*m(1,3)*m(3,2) - m(3,0)*m(1,2)*m(2,3) + m(3,0)*m(1,3)*m(2,2);
  d[5] =  m(0,0)*m(2,2)*m(3,3) - m(0,0)*m(2,3)*m(3,2) - m(2,0)*m(0,2)*m(3,3)
        + m(2,0)*m(0,3)*m(3,2) + m(3,0)*m(0,2)*m(2,3) - m(3,0)*m(0,3)*m(2,2);
  d[9] = -m(0,0)*m(1,2)*m(3,3) + m(0,0)*m(1,3)*m(3,2) + m(1,0)*m(0,2)*m(3,3)
        - m(1,0)*m(0,3)*m(3,2) - m(3,0)*m(0,2)*m(1,3) + m(3,0)*m(0,3)*m(1,2);
  d[13]=  m(0,0)*m(1,2)*m(2,3) - m(0,0)*m(1,3)*m(2,2) - m(1,0)*m(0,2)*m(2,3)
        + m(1,0)*m(0,3)*m(2,2) + m(2,0)*m(0,2)*m(1,3) - m(2,0)*m(0,3)*m(1,2);
  d[2] =  m(1,0)*m(2,1)*m(3,3) - m(1,0)*m(2,3)*m(3,1) - m(2,0)*m(1,1)*m(3,3)
        + m(2,0)*m(1,3)*m(3,1) + m(3,0)*m(1,1)*m(2,3) - m(3,0)*m(1,3)*m(2,1);
  d[6] = -m(0,0)*m(2,1)*m(3,3) + m(0,0)*m(2,3)*m(3,1) + m(2,0)*m(0,1)*m(3,3)
        - m(2,0)*m(0,3)*m(3,1) - m(3,0)*m(0,1)*m(2,3) + m(3,0)*m(0,3)*m(2,1);
  d[10]=  m(0,0)*m(1,1)*m(3,3) - m(0,0)*m(1,3)*m(3,1) - m(1,0)*m(0,1)*m(3,3)
        + m(1,0)*m(0,3)*m(3,1) + m(3,0)*m(0,1)*m(1,3) - m(3,0)*m(0,3)*m(1,1);
  d[14]= -m(0,0)*m(1,1)*m(2,3) + m(0,0)*m(1,3)*m(2,1) + m(1,0)*m(0,1)*m(2,3)
        - m(1,0)*m(0,3)*m(2,1) - m(2,0)*m(0,1)*m(1,3) + m(2,0)*m(0,3)*m(1,1);
  d[3] = -m(1,0)*m(2,1)*m(3,2) + m(1,0)*m(2,2)*m(3,1) + m(2,0)*m(1,1)*m(3,2)
        - m(2,0)*m(1,2)*m(3,1) - m(3,0)*m(1,1)*m(2,2) + m(3,0)*m(1,2)*m(2,1);
  d[7] =  m(0,0)*m(2,1)*m(3,2) - m(0,0)*m(2,2)*m(3,1) - m(2,0)*m(0,1)*m(3,2)
        + m(2,0)*m(0,2)*m(3,1) + m(3,0)*m(0,1)*m(2,2) - m(3,0)*m(0,2)*m(2,1);
  d[11]= -m(0,0)*m(1,1)*m(3,2) + m(0,0)*m(1,2)*m(3,1) + m(1,0)*m(0,1)*m(3,2)
        - m(1,0)*m(0,2)*m(3,1) - m(3,0)*m(0,1)*m(1,2) + m(3,0)*m(0,2)*m(1,1);
  d[15]=  m(0,0)*m(1,1)*m(2,2) - m(0,0)*m(1,2)*m(2,1) - m(1,0)*m(0,1)*m(2,2)
        + m(1,0)*m(0,2)*m(2,1) + m(2,0)*m(0,1)*m(1,2) - m(2,0)*m(0,2)*m(1,1);
  return vnl_matrix_fixed<T,4,4>(d)*det;
}

template <class T>
vnl_matrix<T> vnl_inverse_transpose(vnl_matrix<T> const& m)
{
  assert(m.rows() == m.columns());
  assert(m.rows() <= 4);
  if (m.rows() == 1)
    return vnl_matrix<T>(1,1, T(1)/m(0,0));
  else if (m.rows() == 2)
    return vnl_matrix<T>(vnl_inverse_transpose(vnl_matrix_fixed<T,2,2>(m)));
  else if (m.rows() == 3)
    return vnl_matrix<T>(vnl_inverse_transpose(vnl_matrix_fixed<T,3,3>(m)));
  else
    return vnl_matrix<T>(vnl_inverse_transpose(vnl_matrix_fixed<T,4,4>(m)));
}

#endif // vnl_inverse_h_
