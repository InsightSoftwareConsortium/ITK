/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVersor_h
#define __itkVersor_h

#include "itkVector.h"
#include "itkPoint.h"
#include "itkMatrix.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_quaternion.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk
{

/** \class Versor
 * \brief A templated class holding a unit quaternion.
 * 
 * Versor is a templated class that holds a unit quaternion.
 * The difference between versors and quaternions is that quaternions
 * can represent rotations and scale changes while versors are limited
 * to rotations.
 *
 * This class only implements the operations that maintain versors as
 * a group, that is, any operations between versors result in another
 * versor. For this reason, addition is not defined in this class, even
 * though it is a valid operation between quaternions.
 *
 * \ingroup Geometry
 * \ingroup DataRepresentation
 * 
 * \sa Vector
 * \sa Point
 * \sa CovariantVector
 * \sa Matrix
 */
template<class T> 
class Versor 
{
public:
  /** Standard class typedefs. */
  typedef Versor  Self;
   
  /** ValueType can be used to declare a variable that is the same type
   * as a data element held in a Versor.   */
  typedef T ValueType;

  /** Vector type used to represent the axis. */
  typedef  Vector<T,3>   VectorType;

  /** Point type.  */
  typedef  Point<T,3>   PointType;
 
  /** CovariantVector type.  */
  typedef  CovariantVector<T,3>   CovariantVectorType;
 
  /** Vnl Vector type.  */
  typedef  vnl_vector_fixed<T,3>   VnlVectorType;

  /** Vnl Quaternion type.  */
  typedef  vnl_quaternion<T>       VnlQuaternionType;

  /** Type of the rotation matrix equivalent to the Versor */
  typedef  Matrix<T,3,3>          MatrixType;

  /** Get a vnl_quaternion with a copy of the internal memory block. */
  vnl_quaternion<T> GetVnlQuaternion( void ) const;

  /** Set the Versor from a Quaternion 
   \warning After assignment, the corresponding quaternion will 
            be normalized in order to get a consistent Versor.  */
  void Set( const VnlQuaternionType & ); 

  /** Set the Versor from Quaternion components.
   \warning After assignment, the corresponding quaternion will 
            be normalized in order to get a consistent Versor.  */
  void Set( T x, T y, T z, T w );  


  /** Default constructor creates a null versor 
   * (representing 0 degrees  rotation). */
  Versor();

  /** Copy constructor.  */
  Versor(const Self & v);

  /** Assignment operator =.  Copy the versor argument. */
  const Self& operator=(const Self & v);
 
  /** Composition operator *=.  Compose the current versor
   * with the operand and store the result in the current
   * versor. */
  const Self& operator*=(const Self & v);

  /** Division operator /=.  Divide the current versor
   * with the operand and store the result in the current
   * versor. This is equivalent to compose the Versor with
   * the reciprocal of the operand \sa GetReciprocal */
  const Self& operator/=(const Self & v);


  /** Get Tensor part of the Versor. 
   * Given that Versors are normalized quaternions this value
   * is expected to be 1.0 always  */
  ValueType GetTensor(void) const;

  /** Normalize the Versor.
   * Given that Versors are normalized quaternions this method
   * is provided only for convinience when it is suspected that
   * a versor could be out of the unit sphere.   */
  void Normalize(void);

  /** Get Conjugate versor.  Returns the versor that produce
   * a rotation by the same angle but in opposite direction. */
  Self GetConjugate(void) const;

  /** Get Reciprocal versor.  Returns the versor that composed
   * with this one will result in a scalar operator equals to 1.
   * It is also equivalent to 1/this. */
  Self GetReciprocal(void) const;

  /** Versor operator*.  Performs the composition of two versors.
   * this operation is NOT commutative. */
  Self operator*(const Self &vec) const;

  /** Versor operator/.  Performs the division of two versors. */
  Self operator/(const Self &vec) const;

  /** Versor operator==  Performs the comparison between two versors.
   * this operation uses and arbitrary threshold for the comparison.  */
  bool operator==(const Self &vec) const;

  /** Versor operator!=  Performs the comparison between two versors.
   * this operation uses and arbitrary threshold for the comparison.  */
  bool operator!=(const Self &vec) const;

  /** Returns the Scalar part. */
  ValueType GetScalar( void ) const;

  /** Returns the X component. */
  ValueType GetX( void ) const
    { return m_X; }

  /** Returns the Y component. */
  ValueType GetY( void ) const
    { return m_Y; }

  /** Returns the Z component. */
  ValueType GetZ( void ) const
    { return m_Z; }

  /** Returns the W component. */
  ValueType GetW( void ) const
    { return m_W; }

  /** Returns the rotation angle in radians.  */
  ValueType GetAngle( void ) const;

  /** Returns the axis of the rotation.
   * It is a unit vector parallel to the axis. */
   VectorType GetAxis( void ) const;
   
  /** Returns the Right part
   * It is a vector part of the Versor. It is 
   * called Right because it is equivalent to
   * a right angle rotation. */
   VectorType GetRight( void ) const;
   
  /** Set the versor using a vector and angle
   * the unit vector parallel to the given vector 
   * will be used. The angle is expected in radians. */
  void Set( const VectorType & axis, ValueType angle );
  
  /** Set the versor using the right part.
   * the magnitude of the vector given is assumed to 
   * be equal to sin(angle/2).
   * This method will compute internally the scalar
   * part that preserve the Versor as a unit quaternion. */
  void Set( const VectorType & axis );

  /** Sets a rotation around the X axis using the parameter
   * as angle in radians. This is a method provided for 
   * convinience to initialize a rotation. The effect of
   * this methods is not cumulative with any value previously
   * stored in the Versor.
   * \sa Set \sa SetRotationAroundY \sa SetRotationAroundZ */
  void SetRotationAroundX( ValueType angle );
 
  /** Sets a rotation around the Y axis using the parameter
   * as angle in radians. This is a method provided for 
   * convinience to initialize a rotation. The effect of
   * this methods is not cumulative with any value previously
   * stored in the Versor.
   * \sa Set \sa SetRotationAroundX \sa SetRotationAroundZ */
  void SetRotationAroundY( ValueType angle );
 
  /** Sets a rotation around the Y axis using the parameter
   * as angle in radians. This is a method provided for 
   * convinience to initialize a rotation. The effect of
   * this methods is not cumulative with any value previously
   * stored in the Versor.
   * \sa Set \sa SetRotationAroundX \sa SetRotationAroundY */
  void SetRotationAroundZ( ValueType angle );

  /** Reset the values so the versor is equivalent to an identity 
   *  transformation. This is equivalent to set a zero angle */
  void SetIdentity();
  
  /** Transform a vector. */
  VectorType Transform( const VectorType & v ) const;

  /** Transform a covariant vector.  */
  CovariantVectorType Transform( const CovariantVectorType & v ) const;

  /** Transform a point. */
  PointType Transform( const PointType & v ) const;

  /** Transform a vnl_vector. */
  VnlVectorType Transform( const VnlVectorType & v ) const;

  /** Get the matrix representation. */
  MatrixType GetMatrix(void) const;
   
  /** Get the Square root of the unit quaternion. */
  Self SquareRoot(void) const;
   
  /** Compute the Exponential of the unit quaternion
   * Exponentiation by a factor is equivalent to 
   * multiplication of the rotaion angle of the quaternion. */
  Self Exponential( ValueType exponent ) const;

private:
   /** Component parallel to x axis.  */
   ValueType  m_X;
   
   /** Component parallel to y axis.  */
   ValueType  m_Y;
   
   /** Component parallel to z axis.  */
   ValueType  m_Z;

   /** Escalar component of the Versor.  */
   ValueType  m_W;
};

template< class T>  
ITK_EXPORT std::ostream& operator<<( std::ostream& os, 
                                     const Versor<T> & v)
{
  os << "[ ";
  os << v.GetX() << ", " << v.GetY() << ", ";
  os << v.GetZ() << ", " << v.GetW() << " ]";
  return os;
}

template< class T>
ITK_EXPORT std::istream& operator>>(std::istream& is, 
                                    Versor<T> & v); 


} // end namespace itk
  

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVersor.txx"
#endif


#endif 
