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
// \endverbatim

#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vcl_iostream.h>

//: 4-element vector that represents rotation in 3D.
// vnl_quaternion is a 4-element vector with 1 real and 3 imaginary
// components:
// \code
//    q = r + (i*x + j*y + k*z)
//    r = cos(theta/2)
//    (x, y, z) = sin(theta/2) (kx, ky, kz)
// \endcode
// where theta and k are respectively the angle and axis of rotation.
// 3D vectors can be thought of as imaginary quaternions, and so a
// quaternion is represented as a vnl_vector<T> with the imaginary
// part before the real part for 1-1 alignment.
//
// Unit quaternions provide a more efficient representation for
// rotation, than the usual orthonormal matrix that has nine
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
// vnl_vector<T> and vnl_matrix<T> for basic operations on vectors and matrices.
// \sa
// CoolTransform for coordinate transformations.
// \sa
// Envelope for envelope-letter scheme that avoids deep copy on
// return by value in arithmetic expressions like: q1 * q2 * q3 *...
//

export template <class T>
class vnl_quaternion : public vnl_vector_fixed<T, 4>
{
  typedef vnl_vector_fixed<T, 4> Base;
 public:

  //: Constructor for null quaternion
  vnl_quaternion () {}

  //: Construct quaternion from components x,y,z,r
  vnl_quaternion (T x, T y, T z, T r);

  //: Construct quaternion from axis and angle of rotation
  vnl_quaternion (const vnl_vector<T>& axis, T angle);

  //: Construct quaternion from from 3-4 square row-major
  explicit vnl_quaternion (const vnl_matrix<T>& transform); // from 3-4 square row-major

  //: Construct quaternion from from from 3-4D vector
  vnl_quaternion (const vnl_vector<T>& vec);

  //: Construct quaternion from from from 4D vector
  vnl_quaternion (const vnl_vector_fixed<T,4>& vec);

  //: Copy constructor -- Creates a copy of from quaternion.
  inline vnl_quaternion (const vnl_quaternion<T>& from) : Base(from) {}

  //: Free internal array
  inline ~vnl_quaternion() {} // vnl_vector will free data array

  //:  Overloads assignment operator to copy rhs quaternion into lhs quaternion.
  inline vnl_quaternion& operator= (const vnl_quaternion<T>& rhs) { Base::operator=(rhs); return *this; }

  //: Imaginary component, parallel to axis of rotation.
  // Use this accessor to both get and set the component.
  inline T& x () { return this->operator()(0); }
  //: Imaginary component, parallel to axis of rotation.
  // Use this accessor to both get and set the component.
  inline T& y () { return this->operator()(1); }
  //: Imaginary component, parallel to axis of rotation.
  // Use this accessor to both get and set the component.
  inline T& z () { return this->operator()(2); }
  //: Real component.
  // Use this accessor to both get and set the component.
  inline T& r () { return this->operator()(3); }

  //: Imaginary component, parallel to axis of rotation.
  // Use this accessor to get the component.
  inline T x () const { return this->operator()(0); }
  //: Imaginary component, parallel to axis of rotation.
  // Use this accessor to get the component.
  inline T y () const { return this->operator()(1); }
  //: Imaginary component, parallel to axis of rotation.
  // Use this accessor to get the component.
  inline T z () const { return this->operator()(2); }
  //: Real component.
  // Use this accessor to get the component.
  inline T r () const { return this->operator()(3); }

  //: Copies and returns the real part.
  inline T real () const { return (*this)[3]; }

  //: Copies and returns the imaginary part.
  inline vnl_vector<T> imaginary () const { return this->extract(3,0); }

  //: Axis of rotation
  vnl_vector<T> axis () const;

  //: Angle of rotation
  T angle () const;

  //: 3x3 rotation matrix
  vnl_matrix_fixed<T,3,3> rotation_matrix_transpose () const;

  //: 4x4 rotation matrix
  vnl_matrix_fixed<T,4,4> rotation_matrix_transpose_4 () const;

  //: Same real, opposite img part
  vnl_quaternion<T> conjugate () const;

  //: Inverse for nonzero quat
  vnl_quaternion<T> inverse () const;

  vnl_quaternion<T> operator* (const vnl_quaternion<T>&) const;

  //: Rotate 3D v
  vnl_vector<T> rotate (const vnl_vector<T>& v) const;
};

//: operator<<
// \relates vnl_quaternion
template <class T>
inline vcl_ostream& operator<< (vcl_ostream& os, const vnl_quaternion<T>& q) {
  return os << *((const vnl_vector_fixed<T,4>*) &q);
}

#define VNL_QUATERNION_INSTANTIATE(T) extern "you must include vnl/vnl_quaternion.txx first"

#endif // vnl_quaternion_h_
