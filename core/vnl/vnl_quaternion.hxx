// This is core/vnl/vnl_quaternion.hxx
#ifndef vnl_quaternion_hxx_
#define vnl_quaternion_hxx_
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


#include <cmath>
#include <limits>
#include <iostream>
#include "vnl_quaternion.h"

#include <vcl_compiler.h>

#include <vnl/vnl_cross.h>
#include <vnl/vnl_math.h>

//: Creates a quaternion from its ordered components.
// x, y, z denote the imaginary part, which are the  coordinates
// of the rotation axis multiplied by the sine of half the
// angle of rotation. r denotes  the  real  part,  or  the
// cosine  of  half  the  angle of rotation. Default is to
// create a null quaternion, corresponding to a null rotation
// or  an  identity  transform,  which has undefined
// rotation axis.

template <class T>
vnl_quaternion<T>::vnl_quaternion (T tx, T ty, T tz, T rea)
{
  this->operator[](0) = tx;  // 3 first elements are
  this->operator[](1) = ty;  // imaginary parts
  this->operator[](2) = tz;
  this->operator[](3) = rea;  // last element is real part
}

//: Creates a quaternion from the normalized axis direction and the angle of rotation in radians.

template <class T>
vnl_quaternion<T>::vnl_quaternion(vnl_vector_fixed<T,3> const& Axis, double Angle)
{
  double a = Angle * 0.5;  // half angle
  T s = T(std::sin(a));
  for (int i = 0; i < 3; i++)            // imaginary vector is sine of
    this->operator[](i) = T(s * Axis(i));// half angle multiplied with axis
  this->operator[](3) = T(std::cos(a));   // real part is cosine of half angle
}

//: Creates a quaternion from a vector.
// 3D vector is converted into an imaginary quaternion with same
// (x, y, z) components.

template <class T>
vnl_quaternion<T>::vnl_quaternion(vnl_vector_fixed<T,3> const& vec)
{
  for (unsigned int i = 0; i < 3; ++i)
    this->operator[](i) = vec(i);
  this->operator[](3) = T(0);
}

//: Creates a quaternion from a vector.
// 4D vector is assumed to be a 4-element quaternion, to
// provide casting between vector and quaternion

template <class T>
vnl_quaternion<T>::vnl_quaternion(vnl_vector_fixed<T,4> const& vec)
{
  for (unsigned int i = 0; i < 4; ++i) // 1-1 layout between vector & quaternion
    this->operator[](i) = vec[i];
}


//: Creates a quaternion from a rotation matrix.
// Its orthonormal basis vectors are the matrix rows.
// \note this matrix \e must have determinant +1; this is not verified!
// \warning Takes the transpose of the rotation matrix, i.e.,
// the orthonormal vectors must be the rows of the matrix, not the columns.
template <class T>
vnl_quaternion<T>::vnl_quaternion(vnl_matrix_fixed<T,3,3> const& rot)
{
  double d0 = rot(0,0), d1 = rot(1,1), d2 = rot(2,2);
  double xx = 1.0 + d0 - d1 - d2;      // from the diagonal of the rotation
  double yy = 1.0 - d0 + d1 - d2;      // matrix, find the terms in
  double zz = 1.0 - d0 - d1 + d2;      // each Quaternion component
  double rr = 1.0 + d0 + d1 + d2;      // (using the fact that rr+xx+yy+zz=4)

  double max = rr;                     // find the maximum of all terms;
  if (xx > max) max = xx;              // dividing by the maximum makes
  if (yy > max) max = yy;              // the computations more stable
  if (zz > max) max = zz;              // and avoid division by zero

  if (rr == max) {
    T r4 = T(std::sqrt(rr)*2);
    this->r() = r4 / 4;
    r4 = T(1) / r4;
    this->x() = (rot(1,2) - rot(2,1)) * r4;     // find other components from
    this->y() = (rot(2,0) - rot(0,2)) * r4;     // off diagonal terms of
    this->z() = (rot(0,1) - rot(1,0)) * r4;     // rotation matrix.
  }
  else if (xx == max) {
    T x4 = T(std::sqrt(xx)*2);
    this->x() = x4 / 4;
    x4 = T(1) / x4;
    this->y() = (rot(0,1) + rot(1,0)) * x4;
    this->z() = (rot(0,2) + rot(2,0)) * x4;
    this->r() = (rot(1,2) - rot(2,1)) * x4;
  }
  else if (yy == max) {
    T y4 = T(std::sqrt(yy)*2);
    this->y() =  y4 / 4;
    y4 = T(1) / y4;
    this->x() = (rot(0,1) + rot(1,0)) * y4;
    this->z() = (rot(1,2) + rot(2,1)) * y4;
    this->r() = (rot(2,0) - rot(0,2)) * y4;
  }
  else {
    T z4 = T(std::sqrt(zz)*2);
    this->z() =  z4 / 4;
    z4 = T(1) / z4;
    this->x() = (rot(0,2) + rot(2,0)) * z4;
    this->y() = (rot(1,2) + rot(2,1)) * z4;
    this->r() = (rot(0,1) - rot(1,0)) * z4;
  }
}


//: Construct quaternion from Euler Angles
// That is a rotation about the X axis, followed by Y, followed by
// the Z axis, using a fixed reference frame.
template <class T>
vnl_quaternion<T>::vnl_quaternion(T theta_X, T theta_Y, T theta_Z)
{
  vnl_quaternion<T> Rx(T(std::sin(double(theta_X)*0.5)), 0, 0, T(std::cos(double(theta_X)*0.5)));
  vnl_quaternion<T> Ry(0, T(std::sin(double(theta_Y)*0.5)), 0, T(std::cos(double(theta_Y)*0.5)));
  vnl_quaternion<T> Rz(0, 0, T(std::sin(double(theta_Z)*0.5)), T(std::cos(double(theta_Z)*0.5)));
  *this = Rz * Ry * Rx;
}

//: Rotation representation in Euler angles.
// The angles returned will be [theta_X,theta_Y,theta_Z]
// where the final rotation is found be first applying theta_X radians
// about the X axis, then theta_Y about the Y-axis, etc.
// The axes stay in a fixed reference frame.
template <class T>
vnl_vector_fixed<T,3> vnl_quaternion<T>::rotation_euler_angles() const
{
  vnl_vector_fixed<T,3> angles;

  vnl_matrix_fixed<T,4,4> rotM = rotation_matrix_transpose_4();
  T xy = T(std::sqrt(double(vnl_math::sqr(rotM(0,0)) + vnl_math::sqr(rotM(0,1)))));
  if (xy > std::numeric_limits<T>::epsilon() * T(8))
  {
    angles(0) = T(std::atan2(double(rotM(1,2)), double(rotM(2,2))));
    angles(1) = T(std::atan2(double(-rotM(0,2)), double(xy)));
    angles(2) = T(std::atan2(double(rotM(0,1)), double(rotM(0,0))));
  }
  else
  {
    angles(0) = T(std::atan2(double(-rotM(2,1)), double(rotM(1,1))));
    angles(1) = T(std::atan2(double(-rotM(0,2)), double(xy)));
    angles(2) = T(0);
  }
  return angles;
}


//: Queries the rotation angle of the quaternion.
//  Returned angle lies in [0, 2*pi]
template <class T>
double vnl_quaternion<T>::angle() const
{
  return 2 * std::atan2(double(this->imaginary().magnitude()),
                       double(this->real()));    // angle is always positive
}

//: Queries the direction of the rotation axis of the quaternion.
//  A null quaternion will return zero for angle and k direction for axis.
template <class T>
vnl_vector_fixed<T,3> vnl_quaternion<T>::axis() const
{
  vnl_vector_fixed<T,3> direc = this->imaginary(); // direc parallel to imag. part
  T mag = direc.magnitude();
  if (mag == T(0)) {
    std::cout << "Axis not well defined for zero Quaternion. Using (0,0,1) instead.\n";
    direc[2] = T(1);                    // or signal exception here.
  }
  else
    direc /= mag;                       // normalize direction vector
  return direc;
}


//: Converts a normalized quaternion into a square rotation matrix with dimension dim.
//  This is the reverse counterpart of constructing a quaternion from a transformation matrix.
// WARNING this is inconsistent with the quaternion docs and q.rotate()

template <class T>
vnl_matrix_fixed<T,3,3> vnl_quaternion<T>::rotation_matrix_transpose() const
{
  T x2 = x() * x(),  xy = x() * y(),  rx = r() * x(),
    y2 = y() * y(),  yz = y() * z(),  ry = r() * y(),
    z2 = z() * z(),  zx = z() * x(),  rz = r() * z(),
    r2 = r() * r();
  vnl_matrix_fixed<T,3,3> rot;
  rot(0,0) = r2 + x2 - y2 - z2;         // fill diagonal terms
  rot(1,1) = r2 - x2 + y2 - z2;
  rot(2,2) = r2 - x2 - y2 + z2;
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
  return rot.set_identity().update(this->rotation_matrix_transpose().as_ref());
}

//: Returns the conjugate of given quaternion, having same real and opposite imaginary parts.

template <class T>
vnl_quaternion<T> vnl_quaternion<T>::conjugate() const
{
  return vnl_quaternion<T> (-x(), -y(), -z(), r());
}

//: Returns the inverse of given quaternion.
//  For unit quaternion representing rotation, the inverse is the
// same as the conjugate.

template <class T>
vnl_quaternion<T> vnl_quaternion<T>::inverse() const
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
vnl_quaternion<T> vnl_quaternion<T>::operator* (vnl_quaternion<T> const& rhs) const
{
  T r1 = this->real();                  // real and img parts of args
  T r2 = rhs.real();
  vnl_vector_fixed<T,3> i1 = this->imaginary();
  vnl_vector_fixed<T,3> i2 = rhs.imaginary();
  T real_v = (r1 * r2) - ::dot_product(i1, i2); // real&img of product q1*q2
  vnl_vector_fixed<T,3> img = vnl_cross_3d(i1, i2);
  img += (i2 * r1) + (i1 * r2);
  return vnl_quaternion<T>(img[0], img[1], img[2], real_v);
}

//: Rotates 3D vector v with source quaternion and stores the rotated vector back into v.
// For speed and greater accuracy, first convert quaternion into an orthonormal
// matrix,  then  use matrix multiplication to rotate many vectors.

template <class T>
vnl_vector_fixed<T,3> vnl_quaternion<T>::rotate(vnl_vector_fixed<T,3> const& v) const
{
  T rea = this->real();
  vnl_vector_fixed<T,3> i = this->imaginary();
  vnl_vector_fixed<T,3> i_x_v(vnl_cross_3d(i, v));
  return v + i_x_v * T(2*rea) - vnl_cross_3d(i_x_v, i) * T(2);
}

#undef VNL_QUATERNION_INSTANTIATE
#define VNL_QUATERNION_INSTANTIATE(T) \
template class VNL_EXPORT vnl_quaternion<T >;\
VCL_INSTANTIATE_INLINE(std::ostream& operator<< (std::ostream&, vnl_quaternion<T > const&))

#endif // vnl_quaternion_hxx_
