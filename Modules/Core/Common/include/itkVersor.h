/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkVersor_h
#define itkVersor_h

#include "itkMatrix.h"
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
 * \ingroup ITKCommon
 */
template< typename T >
class ITK_TEMPLATE_EXPORT Versor
{
public:
  /** Standard class typedefs. */
  typedef Versor Self;

  /** ValueType can be used to declare a variable that is the same type
   * as a data element held in a Versor.   */
  typedef T ValueType;

  /** Type used for computations on the versor components */
  typedef typename NumericTraits< ValueType >::RealType RealType;

  /** Vector type used to represent the axis. */
  typedef  Vector< T, 3 > VectorType;

  /** Point type.  */
  typedef  Point< T, 3 > PointType;

  /** CovariantVector type.  */
  typedef  CovariantVector< T, 3 > CovariantVectorType;

  /** Vnl Vector type.  */
  typedef  vnl_vector_fixed< T, 3 > VnlVectorType;

  /** Vnl Quaternion type.  */
  typedef  vnl_quaternion< T > VnlQuaternionType;

  /** Type of the rotation matrix equivalent to the Versor */
  typedef  Matrix< T, 3, 3 > MatrixType;

  /** Get a vnl_quaternion with a copy of the internal memory block. */
  vnl_quaternion< T > GetVnlQuaternion() const;

  /** Set the Versor from a Quaternion
   \warning After assignment, the corresponding quaternion will
            be normalized in order to get a consistent Versor.  */
  void Set(const VnlQuaternionType &);

  /** Set the Versor from Quaternion components.
   \warning After assignment, the corresponding quaternion will be normalized
   in order to get a consistent Versor.  Also, if the "w" component is
   negative, the four components will be negated in order to produce a
   quaternion where "w" is positive, since this is implicitly assumed in other
   sections of the code, in particular when "w" is computed from (x,y,z) via
   normalization. The reason why it is valid to negate all the components is
   that the rotation by angle \f$\theta\f$, is represented by \f$\sin(\frac{\theta}{2})\f$
   in the (x,y,z) components and by \f$\cos(\frac{\theta}{2})\f$ in the "w"
   component. The rotation by any \f$\theta\f$ should be equivalent to a rotation by
   \f$\theta + n \times \pi\f$, therefore we should be able to replace
   \f$\sin(\frac{\theta}{2})\f$ with \f$\sin(\frac{\theta}{2} + n \times \pi )\f$ and
   \f$\cos(\frac{\theta}{2})\f$ with \f$\cos(\frac{\theta}{2} + n \times \pi )\f$.
   Considering that \f$\cos( n \times \pi ) = (-1)^{n}\f$ we can conclude that if we
   simultaneously change the signs of all the Versor components, the rotation
   that it represents remains unchanged.
   */
  void Set(T x, T y, T z, T w);

  /** Default constructor creates a null versor
   * (representing 0 degrees  rotation). */
  Versor();

  /** Copy constructor.  */
  Versor(const Self & v);

  /** Assignment operator =.  Copy the versor argument. */
  const Self & operator=(const Self & v);

  /** Composition operator *=.  Compose the current versor
   * with the operand and store the result in the current
   * versor. */
  const Self & operator*=(const Self & v);

  /** Division operator /=.  Divide the current versor
   * with the operand and store the result in the current
   * versor. This is equivalent to compose the Versor with
   * the reciprocal of the operand \sa GetReciprocal */
  const Self & operator/=(const Self & v);

  /** Get Tensor part of the Versor.
   * Given that Versors are normalized quaternions this value
   * is expected to be 1.0 always  */
  ValueType GetTensor() const;

  /** Normalize the Versor.
   * Given that Versors are normalized quaternions this method
   * is provided only for convinience when it is suspected that
   * a versor could be out of the unit sphere.   */
  void Normalize();

  /** Get Conjugate versor.  Returns the versor that produce
   * a rotation by the same angle but in opposite direction. */
  Self GetConjugate() const;

  /** Get Reciprocal versor.  Returns the versor that composed
   * with this one will result in a scalar operator equals to 1.
   * It is also equivalent to 1/this. */
  Self GetReciprocal() const;

  /** Versor operator*.  Performs the composition of two versors.
   * this operation is NOT commutative. */
  Self operator *(const Self & vec) const;

  /** Versor operator/.  Performs the division of two versors. */
  Self operator/(const Self & vec) const;

  /** Versor operator==  Performs the comparison between two versors.
   * this operation uses an arbitrary threshold for the comparison.  */
  bool operator==(const Self & vec) const;

  /** Versor operator!=  Performs the comparison between two versors.
   * this operation uses an arbitrary threshold for the comparison.  */
  bool operator!=(const Self & vec) const;

  /** Returns the Scalar part. */
  ValueType GetScalar() const;

  /** Returns the X component. */
  ValueType GetX(void) const { return m_X; }

  /** Returns the Y component. */
  ValueType GetY(void) const { return m_Y; }

  /** Returns the Z component. */
  ValueType GetZ(void) const { return m_Z; }

  /** Returns the W component. */
  ValueType GetW(void) const { return m_W; }

  /** Returns the rotation angle in radians.  */
  ValueType GetAngle() const;

  /** Returns the axis of the rotation.
   * It is a unit vector parallel to the axis. */
  VectorType GetAxis() const;

  /** Returns the Right part
   * It is a vector part of the Versor. It is
   * called Right because it is equivalent to
   * a right angle rotation. */
  VectorType GetRight() const;

  /** Set the versor using a vector and angle
   * the unit vector parallel to the given vector
   * will be used. The angle is expected in radians. */
  void Set(const VectorType & axis, ValueType angle);

  /** Set the versor using an orthogonal matrix.
   *  Based on code from:
   *  http://www.euclideanspace.com/maths/geometry/rotations/
   *  conversions/matrixToQuaternion/index.htm
   */
  void Set(const MatrixType & m);

  /** Set the versor using the right part.
   * the magnitude of the vector given is assumed to
   * be equal to std::sin(angle/2).
   * This method will compute internally the scalar
   * part that preserve the Versor as a unit quaternion. */
  void Set(const VectorType & axis);

  /** Sets a rotation around the X axis using the parameter
   * as angle in radians. This is a method provided for
   * convinience to initialize a rotation. The effect of
   * this methods is not cumulative with any value previously
   * stored in the Versor.
   * \sa Set \sa SetRotationAroundY \sa SetRotationAroundZ */
  void SetRotationAroundX(ValueType angle);

  /** Sets a rotation around the Y axis using the parameter
   * as angle in radians. This is a method provided for
   * convinience to initialize a rotation. The effect of
   * this methods is not cumulative with any value previously
   * stored in the Versor.
   * \sa Set \sa SetRotationAroundX \sa SetRotationAroundZ */
  void SetRotationAroundY(ValueType angle);

  /** Sets a rotation around the Y axis using the parameter
   * as angle in radians. This is a method provided for
   * convinience to initialize a rotation. The effect of
   * this methods is not cumulative with any value previously
   * stored in the Versor.
   * \sa Set \sa SetRotationAroundX \sa SetRotationAroundY */
  void SetRotationAroundZ(ValueType angle);

  /** Reset the values so the versor is equivalent to an identity
   *  transformation. This is equivalent to set a zero angle */
  void SetIdentity();

  /** Transform a vector. */
  VectorType Transform(const VectorType & v) const;

  /** Transform a covariant vector.  */
  CovariantVectorType Transform(const CovariantVectorType & v) const;

  /** Transform a point. */
  PointType Transform(const PointType & v) const;

  /** Transform a vnl_vector. */
  VnlVectorType Transform(const VnlVectorType & v) const;

  /** Get the matrix representation. */
  MatrixType GetMatrix() const;

  /** Get the Square root of the unit quaternion. */
  Self SquareRoot() const;

  /** Compute the Exponential of the unit quaternion
   * Exponentiation by a factor is equivalent to
   * multiplication of the rotaion angle of the quaternion. */
  Self Exponential(ValueType exponent) const;

private:
  /** use different epsilon for float and double */
  static inline ValueType Epsilon(double *)
    {
      return 1e-10;
    }
  static inline ValueType Epsilon(float *)
    {
      return 1e-7;
    }
  static inline ValueType Epsilon()
    {
      return Epsilon((ValueType *)ITK_NULLPTR);
    }

  /** Component parallel to x axis.  */
  ValueType m_X;

  /** Component parallel to y axis.  */
  ValueType m_Y;

  /** Component parallel to z axis.  */
  ValueType m_Z;

  /** Escalar component of the Versor.  */
  ValueType m_W;
};

template< typename T >
std::ostream & operator<<(std::ostream & os,
                                     const Versor< T > & v)
{
  os << "[ ";
  os << v.GetX() << ", " << v.GetY() << ", ";
  os << v.GetZ() << ", " << v.GetW() << " ]";
  return os;
}

template< typename T >
std::istream & operator>>(std::istream & is,
                                     Versor< T > & v);
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVersor.hxx"
#endif

#endif
