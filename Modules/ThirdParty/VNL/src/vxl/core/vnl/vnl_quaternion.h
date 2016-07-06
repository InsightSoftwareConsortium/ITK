// This is core/vnl/vnl_quaternion.h
#ifndef vnl_quaternion_h_
#define vnl_quaternion_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Unit quaternion represents rotation in 3D.
// \author awf@robots.ox.ac.uk
// \date   16 Mar 00
//
// \verbatim
//  Modifications
//   20-05-2000 fsm. changed FLOAT to T since gcc will barf at
//              the very reasonable forward declaration
//              template <class FLOAT> class vnl_quaternion;
//   23-3-2001 LSB (Manchester) Tidied documentation
//   13-1-2003 Peter Vanroose - removed unimplemented method rotation_matrix()
//   20-2-2006 Ian Scott - Added conversion to from Euler angles
//   06-5-2006 Peter Vanroose - replaced all vnl_vector by vnl_vector_fixed
// \endverbatim

#include <iostream>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vcl_compiler.h>
#include "vnl/vnl_export.h"

//: 4-element vector that represents rotation in 3D.
// vnl_quaternion is a 4-element vector with 1 real and 3 imaginary
// components:
// \code
//    q = r + (i*x + j*y + k*z)
//    r = cos(theta/2)
//    (x, y, z) = sin(theta/2) (kx, ky, kz)
// \endcode
// where theta and k are respectively the angle and axis of rotation.
//
// 3D vectors can be thought of as pure imaginary quaternions, and so a
// quaternion is represented as a vnl_vector_fixed<T,4> with the imaginary
// part before the real part for 1-1 alignment.
//
// Unit quaternions (i.e., for which $x^2 + y^2 + z^2 + r^2 = 1$)
// provide a more efficient representation for rotation
// than the usual orthonormal matrix that has nine
// parameters and six orthonormal constraints.  The unit
// quaternion has only one unit magnitude constraint.  Composing
// rotations with quaternions results in fewer multiplications
// and less error.  To insure valid rotation results, the
// nearest unit quaternion is computed, and this is much easier
// than finding the nearest orthonormal matrix.  Transforming
// vectors with a quaternion requires more operations compared
// to multiplication with the equivalent orthonormal matrix.
//
// \sa
// vnl_vector_fixed and vnl_matrix_fixed for basic operations on vectors and matrices.
// \sa
// Envelope for envelope-letter scheme that avoids deep copy on
// return by value in arithmetic expressions like: q1 * q2 * q3 *...
//

VCL_TEMPLATE_EXPORT template <class T>
class VNL_TEMPLATE_EXPORT vnl_quaternion : public vnl_vector_fixed<T, 4>
{
 private:
  typedef vnl_vector_fixed<T,4> Base;
 public:

  //: Constructor for null quaternion
  vnl_quaternion() {}

  //: Construct quaternion from components x,y,z,r
  vnl_quaternion(T x, T y, T z, T r);

  //: Construct quaternion from Euler Angles,
  // That is a rotation about the X axis, followed by Y, followed by
  // the Z axis, using a fixed reference frame.
  vnl_quaternion(T theta_X, T theta_Y, T theta_Z);

  //: Construct quaternion from axis and angle of rotation.
  // \note If you specify an angle in [0, 2pi], then methods angle() and axis() will return the same values.
  // However, if you specify an angle in [-2pi, 0], then methods angle() and axis() will return values with opposite signs.
  // \sa vnl_quaternion::angle()
  // \sa vnl_quaternion::axis()
  vnl_quaternion(vnl_vector_fixed<T,3> const& axis, double angle);

  //: Construct quaternion from 3x3 row-major matrix
  explicit vnl_quaternion(vnl_matrix_fixed<T,3,3> const& transform);

  //: Construct quaternion from a 3D vector
  vnl_quaternion(vnl_vector_fixed<T,3> const& vec);

  //: Construct quaternion from a 4D vector
  vnl_quaternion (vnl_vector_fixed<T,4> const& vec);

  //: Copy constructor -- Creates a copy of from quaternion.
  inline vnl_quaternion(vnl_quaternion<T> const& from) : Base(from) {}

  //: Free internal array
  inline ~vnl_quaternion() {} // vnl_vector_fixed will free data array

  //:  Overloads assignment operator to copy rhs quaternion into lhs quaternion.
  inline vnl_quaternion& operator= (vnl_quaternion<T> const& rhs) { Base::operator=(rhs); return *this; }

  //: Imaginary component, parallel to axis of rotation.
  // Use this accessor to both get and set the component.
  inline T& x() { return this->operator()(0); }
  //: Imaginary component, parallel to axis of rotation.
  // Use this accessor to both get and set the component.
  inline T& y() { return this->operator()(1); }
  //: Imaginary component, parallel to axis of rotation.
  // Use this accessor to both get and set the component.
  inline T& z() { return this->operator()(2); }
  //: Real component.
  // Use this accessor to both get and set the component.
  inline T& r() { return this->operator()(3); }

  //: Imaginary component, parallel to axis of rotation.
  // Use this accessor to get the component.
  inline T x() const { return this->operator()(0); }
  //: Imaginary component, parallel to axis of rotation.
  // Use this accessor to get the component.
  inline T y() const { return this->operator()(1); }
  //: Imaginary component, parallel to axis of rotation.
  // Use this accessor to get the component.
  inline T z() const { return this->operator()(2); }
  //: Real component.
  // Use this accessor to get the component.
  inline T r() const { return this->operator()(3); }

  //: Copies and returns the real part.
  inline T real() const { return (*this)[3]; }

  //: Copies and returns the imaginary part.
  inline vnl_vector_fixed<T,3> imaginary() const { return this->extract(3,0); }

  //: Axis of rotation.
  // \note Axis not well defined for theta==0. In such a case (or if provided axis==(0,0,0)), this function returns (0,0,1).
  vnl_vector_fixed<T,3> axis() const;

  //: Angle of rotation.
  // \note Returned angle lies in [0, 2*pi]
  double angle() const;

  //: 3x3 rotation matrix
  // The orthonormal vectors are the rows of the matrix, not its columns
  vnl_matrix_fixed<T,3,3> rotation_matrix_transpose() const;

  //: 4x4 rotation matrix
  vnl_matrix_fixed<T,4,4> rotation_matrix_transpose_4() const;

  //: Same real, opposite img part
  vnl_quaternion<T> conjugate() const;

  //: Inverse for nonzero quat
  vnl_quaternion<T> inverse() const;

  vnl_quaternion<T> operator* (vnl_quaternion<T> const&) const;

  //: Rotate 3D v
  // The quaternion must be normalised first.
  vnl_vector_fixed<T,3> rotate(vnl_vector_fixed<T,3> const&) const;

  //: Rotation representation in Euler angles.
  // The angles returned will be [theta_X,theta_Y,theta_Z]
  // where the final rotation is found be first applying theta_X radians
  // about the X axis, then theta_Y about the Y-axis, etc.
  // The axes stay in a fixed reference frame.
  // The quaternion mut be normalised first.
  vnl_vector_fixed<T,3> rotation_euler_angles() const;
};

//: operator<<
// \relatesalso vnl_quaternion
template <class T> VNL_TEMPLATE_EXPORT
std::istream& operator>> (std::istream& is, vnl_quaternion<T> &q)
{
  vnl_vector_fixed<T,4> v;
  is >> v;
  q = vnl_quaternion<T>(v);
  return is;
}


//: operator<<
// \relatesalso vnl_quaternion
template <class T>
inline std::ostream& operator<< (std::ostream& os, vnl_quaternion<T> const& q)
{
  return os << *((const vnl_vector_fixed<T,4>*) &q);
}

#define VNL_QUATERNION_INSTANTIATE(T) extern "Please #include <vnl/vnl_quaternion.hxx> first"

#endif // vnl_quaternion_h_
