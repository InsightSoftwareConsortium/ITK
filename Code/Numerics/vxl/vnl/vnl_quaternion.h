#ifndef vnl_quaternion_h_
#define vnl_quaternion_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_quaternion.h

//: \file
//  \brief Unit quaternion represents rotation in 3D.
//  \author awf@robots.ox.ac.uk 16 Mar 00
//

 
//  Modifications
//  20-05-2000 fsm@robots. changed FLOAT to T since gcc will barf at
//            the very reasonable forward declaration
//            template <class T> class vnl_quaternion;
//  LSB (Manchester) 23/3/01 Tidied documentation

#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix_fixed.h>
//: 4-element vector that represents rotation in 3D.
// Quaternion is a 4-element vector with 1 real and 3 imaginary
// components:
// \verbatim
//    q = r + (i*x + j*y + k*z)
//    r = cos(theta/2)
//    (x, y, z) = sin(theta/2) (kx, ky, kz) 
// \endverbatim
// where theta and k are  respectively the angle and axis of rotation. 
// 3D vectors can be  thought  of  as  imaginary  quaternions, and  so  a
// quaternion is represented as a Vector<T> with the imaginary
// part before the real part for 1-1 alignment.
// 
// Unit quaternion provides a more efficient representation for
// rotation, than  the usual orthonormal matrix that has nine
// parameters  and  six  orthonormal  constraints.   The   unit
// quaternion  has only one unit magnitude constraint. Composing
// rotations with quaternions results in fewer multiplications
// and less error. To insure valid rotation result, the
// nearest unit quaternion is computed, and this is much easier
// than  finding  the  nearest orthonormal matrix. Transforming
// vectors with a quaternion requires more operations  compared
// to multiplication with the equivalent orthonormal matrix.
//
// See also
// Vector<Type> and Matrix<Type> for basic operations on vectors and matrices.
// 
// Transform for coordinate transformations.
// 
// Envelope for envelope-letter scheme that avoids deep copy on
// return by value in arithmetic expressions like: q1 * q2 * q3 *...
//

//what was this for? #include <vcl_functional.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix_fixed.h>

export template <class T>
class vnl_quaternion : public vnl_vector_fixed<T, 4> { // public for IS-A relation
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

 //: Copy constructor
  inline vnl_quaternion (const vnl_quaternion<T>& from);

 //: Free internal array
  inline ~vnl_quaternion();

 //: q1 = q2
  inline vnl_quaternion& operator= (const vnl_quaternion<T>& rhs);

  inline T& x ();
  inline T& y ();
//: Imaginary components, parallel to axis of rotation
  inline T& z ();

//: Real component
  inline T& r ();

  inline T x () const;
  inline T y () const;
  inline T z () const;
  inline T r () const;
  inline T real () const;
  
 //: Imaginary vector part
  inline vnl_vector<T> imaginary () const;

 //: Axis of rotation
  vnl_vector<T> axis () const;

 //: Angle of rotation
  T angle () const;
  
 //: 3x3 rotation matrix
  vnl_matrix_fixed<T,3,3> rotation_matrix () const;

 //: 4x4 rotation matrix
  vnl_matrix_fixed<T,4,4> rotation_matrix_4 () const;

 //: Same real, opposite img part 
  vnl_quaternion<T> conjugate () const;

 //: Inverse for nonzero quat
  vnl_quaternion<T> inverse () const;
  
  vnl_quaternion<T> operator* (const vnl_quaternion<T>&) const; 

  //: Rotate 3D v
  vnl_vector<T> rotate (const vnl_vector<T>& v) const;
};


//: Quaternion -- Creates a copy of from quaternion.
template <class T>
inline
vnl_quaternion<T>::vnl_quaternion (const vnl_quaternion<T>& from) :
  Base(from)
{    // 1-1 layout between vector&quat
}

//: ~Quaternion -- Frees space allocated for quaternion.

template <class T>
inline
vnl_quaternion<T>::~vnl_quaternion () {} // Vector will free data array


//: x

template <class T>
inline T& vnl_quaternion<T>::x () {
  return this->operator()(0);
}

//: y 

template <class T>
inline T& vnl_quaternion<T>::y () {
  return this->operator()(1);
}

//:  z 

template <class T>
inline T& vnl_quaternion<T>::z () {
  return this->operator()(2);
}

//: r
// Accessors for the imaginary and real components of  the
// quaternion. Use these accessors to both get
// and set the components.

template <class T>
inline T& vnl_quaternion<T>::r () {
  return this->operator()(3);
}


//: x
template <class T>
inline T vnl_quaternion<T>::x () const {
  return this->operator()(0);
}

//: y 
template <class T>
inline T vnl_quaternion<T>::y () const {
  return this->operator()(1);
}

//: z 
template <class T>
inline T vnl_quaternion<T>::z () const {
  return this->operator()(2);
}

//: r
// Accessors for the imaginary and real components of  the
// quaternion. Use these accessors to both get
// and set the components.
template <class T>
inline T vnl_quaternion<T>::r () const {
  return this->operator()(3);
}


//: Copies and returns the imaginary part.
template <class T>
inline vnl_vector<T> vnl_quaternion<T>::imaginary () const {
  return this->extract(3,0);
}

//: Copies and returns the real part.
template <class T>
inline T vnl_quaternion<T>::real () const {
  return this->get(3);
}

//: operator=
//  Overloads assignment operator to  copy rhs quaternion
//  into lhs quaternion.
template <class T>
inline vnl_quaternion<T>& vnl_quaternion<T>::operator= (const vnl_quaternion<T>& rhs) {
  Base::operator=(rhs);   // same as copy vector part
  return *this;
}


//: operator<<
template <class T>
inline vcl_ostream& operator<< (vcl_ostream& os, const vnl_quaternion<T>& q) {
  return os << *((vnl_vector<T>*) &q);
}

// operator<<  -- Print the components of Quaternion.
//  awf removed : pointers should never be printed dereffed.
// template <class T>
// inline ostream& operator<< (ostream& os, const vnl_quaternion<T>* q) {
//   return os << *((vnl_vector<T>*) q);
// }

#define VNL_QUATERNION_INSTANTIATE(T) extern "Error, include vnl/vnl_quaternion.txx";

#endif // vnl_quaternion_h_
