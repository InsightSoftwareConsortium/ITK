// This is core/vnl/vnl_det.h
#ifndef vnl_det_h_
#define vnl_det_h_
//:
//  \file
//  \brief Direct evaluation of 2x2, 3x3 and 4x4 determinants.
//  \author fsm
//
// \verbatim
//  Modifications
//  Peter Vanroose - 15 Oct. 2001 - Renamed from vnl_determinant to vnl_det
//  Peter Vanroose - 15 Oct. 2001 - Added vnl_matrix_fixed interface
// \endverbatim

#include "vnl_matrix_fixed.h"
#include "vnl/vnl_export.h"

//: 2x2 matrix
template <class T>
VNL_EXPORT T
vnl_det(const T * row0, const T * row1);

//: 3x3 matrix
template <class T>
VNL_EXPORT T
vnl_det(const T * row0, const T * row1, const T * row2);

//: 4x4 matrix
template <class T>
VNL_EXPORT T
vnl_det(const T * row0, const T * row1, const T * row2, const T * row3);

//: Determinant of small size matrices
// \relatesalso vnl_matrix_fixed
template <class T>
inline T
vnl_det(const vnl_matrix_fixed<T, 1, 1> & m)
{
  return m[0][0];
}

//: Determinant of small size matrices
// \relatesalso vnl_matrix_fixed
template <class T>
inline T
vnl_det(const vnl_matrix_fixed<T, 2, 2> & m)
{
  return vnl_det(m[0], m[1]);
}

//: Determinant of small size matrices
// \relatesalso vnl_matrix_fixed
template <class T>
inline T
vnl_det(const vnl_matrix_fixed<T, 3, 3> & m)
{
  return vnl_det(m[0], m[1], m[2]);
}

//: Determinant of small size matrices
// \relatesalso vnl_matrix_fixed
template <class T>
inline T
vnl_det(const vnl_matrix_fixed<T, 4, 4> & m)
{
  return vnl_det(m[0], m[1], m[2], m[3]);
}

#endif // vnl_det_h_
