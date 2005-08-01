// This is core/vnl/vnl_quaternion.txx
#ifndef vnl_quaternion_txx_
#define vnl_quaternion_txx_
//:
// \file
//
// Copyright (C) 1992 General Electric Company.
//
// Permission is granted to any individual or institution to use, copy, modify,
// and distribute this software, provided that this complete copyright and
// permission notice is maintained, intact, in all copies and supporting
// documentation.
//
// General Electric Company,
// provides this software "as is" without express or implied warranty.
//
// Created: VDN 06/23/92  design and implementation
//
// Quaternion IS-A vector, and is a special case of general n-dimensional space.
// The IS-A relationship is enforced with public inheritance.
// All member functions on vectors are applicable to quaternions.
//
// Rep Invariant:
//   -  norm = 1, for a rotation.
//   -  position vector represented by imaginary quaternion.
// References:
// -  Horn, B.K.P. (1987) Closed-form solution of absolute orientation using
//       unit quaternions. J. Opt. Soc. Am. Vol 4, No 4, April.
// -  Horn, B.K.P. (1987) Robot Vision. MIT Press. pp. 437-551.
//


#include "vnl_quaternion.h"

#include <vcl_cmath.h>
#include <vcl_iostream.h>

#include <vnl/vnl_cross.h>

//: Creates a quaternion from its ordered components.
// x, y, z denote the imaginary part, which are the  coordinates
// of the rotation axis multiplied by the sine of half the
// angle of rotation. r denotes  the  real  part,  or  the
// cosine  of  half  the  angle of rotation. Default is to
// create a null quaternion, corresponding to a null rotation
// or  an  identity  transform,  which has undefined
// rotation axis.

template <class T>
vnl_quaternion<T>::vnl_quaternion (T x, T y, T z, T r)
{
  this->operator[](0) = x;  // 3 first elements are
  this->operator[](1) = y;  // imaginary parts
  this->operator[](2) = z;
  this->operator[](3) = r;  // last element is real part
}

//: Creates a quaternion from the normalized axis direction and the angle of rotation in radians.

template <class T>
vnl_quaternion<T>::vnl_quaternion (const vnl_vector<T>& axis, T angle)
{
  double a = angle * 0.5;  // half angle
  T s = T(vcl_sin(a));
  for (int i = 0; i < 3; i++)           // imaginary vector is sine of
    this->operator[](i) = s * axis(i);  // half angle multiplied with axis
  this->operator[](3) = T(vcl_cos(a));  // real part is cosine of half angle
}

//: Creates a quaternion from a vector.
// 2D or 3D vector is converted into an imaginary quaternion with same
// (x, y, z) components. 4D vector is assumed to be a 4-element
// quaternion, to provide casting between vector and quaternion.

template <class T>
vnl_quaternion<T>::vnl_quaternion (const vnl_vector<T>& vec)
{
  unsigned i = 0;
  for (; i < vec.size(); i++)    // 1-1 layout between vector & quaternion
    this->operator[](i) = vec.get(i);
  for (; i < 4; i++)
    this->operator[](i) = 0.0;
}

//: Creates a quaternion from a vector.
// 4D vector is assumed to be a 4-element quaternion, to
// provide casting between vector and quaternion

template <class T>
vnl_quaternion<T>::vnl_quaternion (const vnl_vector_fixed<T,4>& vec)
{
  for (unsigned int i = 0; i < vec.size(); i++)    // 1-1 layout between vector & quaternion
    this->operator[](i) = vec[i];
}


//: Creates a quaternion from a rotation matrix.
// Its orthonormal basis vectors are row-wise in the top-left most block.
// The transform matrix may be any size,
// but the rotation matrix must be the upper left 3x3.
// WARNING: Takes the transpose of the rotation matrix...
template <class T>
vnl_quaternion<T>::vnl_quaternion (const vnl_matrix<T>& transform)
{
  vnl_matrix_fixed<T,3,3> rot = transform.extract(3, 3, 0, 0);
  double d0 = rot(0,0), d1 = rot(1,1), d2 = rot(2,2);
  double xx = 1.0 + d0 - d1 - d2;               // from the diagonal of rotation
  double yy = 1.0 - d0 + d1 - d2;               // matrix, find the terms in
  double zz = 1.0 - d0 - d1 + d2;               // each Quaternion compoment
  double rr = 1.0 + d0 + d1 + d2;

  double max = rr;                              // find the maximum of all
  if (xx > max) max = xx;                       // diagonal terms.
  if (yy > max) max = yy;
  if (zz > max) max = zz;

  if (rr == max) {
    T r4 = T(vcl_sqrt(rr * 4.0));
    this->x() = (rot(1,2) - rot(2,1)) / r4;     // find other components from
    this->y() = (rot(2,0) - rot(0,2)) / r4;     // off diagonal terms of
    this->z() = (rot(0,1) - rot(1,0)) / r4;     // rotation matrix.
    this->r() = r4 / 4;
  } else if (xx == max) {
    T x4 = T(vcl_sqrt(xx * 4.0));
    this->x() = x4 / 4;
    this->y() = (rot(0,1) + rot(1,0)) / x4;
    this->z() = (rot(0,2) + rot(2,0)) / x4;
    this->r() = (rot(1,2) - rot(2,1)) / x4;
  } else if (yy == max) {
    T y4 = T(vcl_sqrt(yy * 4.0));
    this->x() = (rot(0,1) + rot(1,0)) / y4;
    this->y() =  y4 / 4;
    this->z() = (rot(1,2) + rot(2,1)) / y4;
    this->r() = (rot(2,0) - rot(0,2)) / y4;
  } else {
    T z4 = T(vcl_sqrt(zz * 4.0));
    this->x() = (rot(0,2) + rot(2,0)) / z4;
    this->y() = (rot(1,2) + rot(2,1)) / z4;
    this->z() =  z4 / 4;
    this->r() = (rot(0,1) - rot(1,0)) / z4;
  }
}

//:

template <class T>
T vnl_quaternion<T>::angle () const
{
  return T(2 * vcl_atan2 (this->imaginary().magnitude(),
                          this->real()));            // angle is always positive
}

//: Queries the angle and the direction of the rotation axis of the quaternion.
//  A null quaternion will return zero for angle and k direction for axis.

template <class T>
vnl_vector<T> vnl_quaternion<T>::axis () const
{
  vnl_vector<T> direc = this->imaginary(); // direc parallel to imag. part
  T mag = direc.magnitude();
  if (mag == 0) {
    vcl_cout << "Axis not well defined for zero Quaternion. Use (0,0,1) instead.\n";
    direc[2] = 1.0;                     // or signal exception here.
  } else
    direc /= mag;                       // normalize direction vector
  return direc;
}


//: Converts a normalized quaternion into a square rotation matrix with dimension dim.
//  This is the reverse counterpart of constructing a quaternion from a transformation matrix.
// WARNING this is inconsistent with the quaternion docs and q.rotate()

template <class T>
vnl_matrix_fixed<T,3,3> vnl_quaternion<T>::rotation_matrix_transpose () const
{
  vnl_matrix_fixed<T,3,3> rot;
  vnl_quaternion<T> const& q = *this;

  T x2 = q.x() * q.x();
  T y2 = q.y() * q.y();
  T z2 = q.z() * q.z();
  T r2 = q.r() * q.r();
  rot(0,0) = r2 + x2 - y2 - z2;         // fill diagonal terms
  rot(1,1) = r2 - x2 + y2 - z2;
  rot(2,2) = r2 - x2 - y2 + z2;
  T xy = q.x() * q.y();
  T yz = q.y() * q.z();
  T zx = q.z() * q.x();
  T rx = q.r() * q.x();
  T ry = q.r() * q.y();
  T rz = q.r() * q.z();
  rot(0,1) = 2 * (xy + rz);             // fill off diagonal terms
  rot(0,2) = 2 * (zx - ry);
  rot(1,2) = 2 * (yz + rx);
  rot(1,0) = 2 * (xy - rz);
  rot(2,0) = 2 * (zx + ry);
  rot(2,1) = 2 * (yz - rx);

  return rot;
}


template <class T>
vnl_matrix_fixed<T,4,4> vnl_quaternion<T>::rotation_matrix_transpose_4() const
{
  vnl_matrix_fixed<T,4,4> rot;
  rot.set_identity();
  rot.update(this->rotation_matrix_transpose().as_ref());
  return rot;
}

//: Returns the conjugate of given quaternion, having same real and opposite imaginary parts.

template <class T>
vnl_quaternion<T> vnl_quaternion<T>::conjugate () const
{
  return vnl_quaternion<T> (-x(), -y(), -z(), r());
}

//: Returns the inverse of given quaternion.
//  For unit quaternion representing rotation, the inverse is the
// same as the conjugate.

template <class T>
vnl_quaternion<T> vnl_quaternion<T>::inverse () const
{
  vnl_quaternion<T> inv = this->conjugate();
  inv /= vnl_c_vector<T>::dot_product(this->data_, this->data_, 4);
  return inv;
}

//: Returns  the product of two quaternions.
// Multiplication of two quaternions is not symmetric and has
// fewer  operations  than  multiplication  of orthonormal
// matrices. If object is rotated by r1, then by r2,  then
// the  composed  rotation (r2 o r1) is represented by the
// quaternion (q2 * q1), or by the matrix (m1 * m2).  Note
// that  matrix  composition  is reversed because matrices
// and vectors are represented row-wise.

template <class T>
vnl_quaternion<T> vnl_quaternion<T>::operator* (const vnl_quaternion<T>& rhs) const
{
  T r1 = this->real();                  // real and img parts of args
  T r2 = rhs.real();
  vnl_vector<T> i1 = this->imaginary();
  vnl_vector<T> i2 = rhs.imaginary();
  T real_v = (r1 * r2) - ::dot_product(i1, i2); // real&img of product q1*q2
  vnl_vector<T> img = vnl_cross_3d(i1, i2);
  img += (i2 * r1) + (i1 * r2);
  vnl_quaternion<T> prod(img[0], img[1], img[2], real_v);
  return prod;
}

//: Rotates 3D vector v with source quaternion and stores the rotated vector back into v.
// For speed and greater accuracy, first convert quaternion into an orthonormal
// matrix,  then  use matrix multiplication to rotate many vectors.

template <class T>
vnl_vector<T> vnl_quaternion<T>::rotate (const vnl_vector<T>& v) const
{
  T r = this->real();
  vnl_vector<T> i = this->imaginary();
  vnl_vector<T> rotated = v+ vnl_cross_3d(i, v) * T(2*r)- vnl_cross_3d(vnl_cross_3d(i, v), i) * T(2);
  return rotated;
}

#undef VNL_QUATERNION_INSTANTIATE
#define VNL_QUATERNION_INSTANTIATE(T) \
template class vnl_quaternion<T >;\
VCL_INSTANTIATE_INLINE(vcl_ostream& operator<< (vcl_ostream&, const vnl_quaternion<T >&))

#endif // vnl_quaternion_txx_
