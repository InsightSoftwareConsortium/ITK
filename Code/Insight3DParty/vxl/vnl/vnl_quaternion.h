#ifndef vnl_quaternion_h_
#define vnl_quaternion_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME	vnl_quaternion
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_quaternion.h
// .FILE	vnl_quaternion.txx
// .SECTION Author
//    awf@robots.ox.ac.uk
// Created: 16 Mar 00

//: Unit quaternion represents rotation in 3D.
//
// Quaternion is a 4-element vector with 1 real and 3 imaginary
// components:
// <math>
//    q = r + (i*x + j*y + k*z)
//    r = cos(theta/2)
//    (x, y, z) = sin(theta/2) (kx, ky, kz) 
// </math>
// where theta and k are  respectively the angle and axis of rotation. 
// 3D vectors can be  thought  of  as  imaginary  quaternions, and  so  a
// quaternion is represented as a Vector<T> with the imaginary
// part before the real part for 1-1 alignment.
// <skip>
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
// .SECTION See also
// Vector<Type> and Matrix<Type> for basic operations on vectors and matrices.
// <skip>
// Transform for coordinate transformations.
// <skip>
// Envelope for envelope-letter scheme that avoids deep copy on
// return by value in arithmetic expressions like: q1 * q2 * q3 *...
//
// 20-05-2000 fsm@robots. changed FLOAT to T since gcc will barf at
//            the very reasonable forward declaration
//            template <class T> class vnl_quaternion;

//what was this for? #include <vcl_functional.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix_fixed.h>

template <class T>
class vnl_quaternion : public vnl_vector_fixed<T, 4> { // public for IS-A relation
  typedef vnl_vector_fixed<T, 4> Base;
public:
  vnl_quaternion () {} // null quat
  vnl_quaternion (T x, T y, T z, T r);
  vnl_quaternion (const vnl_vector<T>& axis, T angle); 
  explicit vnl_quaternion (const vnl_matrix<T>& transform); // from 3-4 square row-major
  vnl_quaternion (const vnl_vector<T>& vec);     // from 3-4D vector
  inline vnl_quaternion (const vnl_quaternion<T>& from);  // copy constructor
  inline ~vnl_quaternion();			       // free internal array
  inline vnl_quaternion& operator= (const vnl_quaternion<T>& rhs); // q1 = q2
  
  inline T& x ();			// imaginary component
  inline T& y ();			// parallel to axis of rotation
  inline T& z ();
  inline T& r ();			// real component
  inline T x () const;
  inline T y () const;
  inline T z () const;
  inline T r () const;
  inline T real () const;    
  inline vnl_vector<T> imaginary () const; // imaginary vector part

  vnl_vector<T> axis () const;		// Axis of rotation
  T angle () const;				// Angle of rotation
  
  vnl_matrix_fixed<T,3,3> rotation_matrix () const; // to 3 rot matrix
  vnl_matrix_fixed<T,4,4> rotation_matrix_4 () const; // to 4 rot matrix
  
  vnl_quaternion<T> conjugate () const;	// same real, opposite img part
  vnl_quaternion<T> inverse () const;	// inverse for nonzero quat
  
  vnl_quaternion<T> operator* (const vnl_quaternion<T>&) const; 
  vnl_vector<T> rotate (const vnl_vector<T>& v) const; // rotate 3D v
};


// Quaternion -- Creates a copy of from quaternion.
template <class T>
inline
vnl_quaternion<T>::vnl_quaternion (const vnl_quaternion<T>& from) :
  Base(from)
{			// 1-1 layout between vector&quat
}

// ~Quaternion -- Frees space allocated for quaternion.

template <class T>
inline
vnl_quaternion<T>::~vnl_quaternion () {} // Vector will free data array


// x -- 

template <class T>
inline T& vnl_quaternion<T>::x () {
  return this->operator()(0);
}

// y -- 

template <class T>
inline T& vnl_quaternion<T>::y () {
  return this->operator()(1);
}

// z -- 

template <class T>
inline T& vnl_quaternion<T>::z () {
  return this->operator()(2);
}

// r -- Accessors for the imaginary and real components of  the
//      quaternion. Use these accessors to both get
//      and set the components.

template <class T>
inline T& vnl_quaternion<T>::r () {
  return this->operator()(3);
}


// x -- 

template <class T>
inline T vnl_quaternion<T>::x () const {
  return this->operator()(0);
}

// y -- 

template <class T>
inline T vnl_quaternion<T>::y () const {
  return this->operator()(1);
}

// z -- 

template <class T>
inline T vnl_quaternion<T>::z () const {
  return this->operator()(2);
}

// r -- Accessors for the imaginary and real components of  the
//      quaternion. Use these accessors to both get
//      and set the components.

template <class T>
inline T vnl_quaternion<T>::r () const {
  return this->operator()(3);
}

// imaginary -- Copies and returns the imaginary part.

template <class T>
inline vnl_vector<T> vnl_quaternion<T>::imaginary () const {
  return this->extract(3,0);
}

// real -- Copies and returns the real part.

template <class T>
inline T vnl_quaternion<T>::real () const {
  return this->get(3);
}

// operator=  -- Overloads assignment operator to  copy rhs quaternion
//      into lhs quaternion.

template <class T>
inline vnl_quaternion<T>& vnl_quaternion<T>::operator= (const vnl_quaternion<T>& rhs) {
  Base::operator=(rhs);		// same as copy vector part
  return *this;
}



// operator<<  --

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
