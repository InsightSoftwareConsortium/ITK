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
#ifndef itkVersor_hxx
#define itkVersor_hxx

#include "itkVersor.h"
#include "itkNumericTraits.h"
#include "itkMath.h"
#include <vnl/vnl_det.h>

namespace itk
{
/** Constructor to initialize entire vector to one value. */
template< typename T >
Versor< T >
::Versor() :
  m_X(NumericTraits< T >::ZeroValue()),
  m_Y(NumericTraits< T >::ZeroValue()),
  m_Z(NumericTraits< T >::ZeroValue()),
  m_W(NumericTraits< T >::OneValue())
{}

/** Copy Constructor */
template< typename T >
Versor< T >
::Versor(const Self & v)
{
  m_X = v.m_X;
  m_Y = v.m_Y;
  m_Z = v.m_Z;
  m_W = v.m_W;
}

/** Assignment Operator */
template< typename T >
const Versor< T > &
Versor< T >
::operator=(const Self & v)
{
  m_X = v.m_X;
  m_Y = v.m_Y;
  m_Z = v.m_Z;
  m_W = v.m_W;
  return *this;
}

/** Set to an identity transform */
template< typename T >
void
Versor< T >
::SetIdentity()
{
  m_X = NumericTraits< T >::ZeroValue();
  m_Y = NumericTraits< T >::ZeroValue();
  m_Z = NumericTraits< T >::ZeroValue();
  m_W = NumericTraits< T >::OneValue();
}

/** Return a vnl_quaternion */
template< typename T >
vnl_quaternion< T >
Versor< T >
::GetVnlQuaternion(void) const
{
  return vnl_quaternion< T >(m_X, m_Y, m_Z, m_W);
}

/** Assignment and Composition Operator */
template< typename T >
const Versor< T > &
Versor< T >
::operator*=(const Self & v)
{
  const double mx =  m_W * v.m_X - m_Z * v.m_Y + m_Y * v.m_Z + m_X * v.m_W;
  const double my =  m_Z * v.m_X + m_W * v.m_Y - m_X * v.m_Z + m_Y * v.m_W;
  const double mz = -m_Y * v.m_X + m_X * v.m_Y + m_W * v.m_Z + m_Z * v.m_W;
  const double mw = -m_X * v.m_X - m_Y * v.m_Y - m_Z * v.m_Z + m_W * v.m_W;

  m_X = mx;
  m_Y = my;
  m_Z = mz;
  m_W = mw;

  return *this;
}

/** Composition Operator */
template< typename T >
Versor< T >
Versor< T >
::operator*(const Self & v) const
{
  Self result;

  result.m_X =  m_W * v.m_X - m_Z * v.m_Y + m_Y * v.m_Z + m_X * v.m_W;
  result.m_Y =  m_Z * v.m_X + m_W * v.m_Y - m_X * v.m_Z + m_Y * v.m_W;
  result.m_Z = -m_Y * v.m_X + m_X * v.m_Y + m_W * v.m_Z + m_Z * v.m_W;
  result.m_W = -m_X * v.m_X - m_Y * v.m_Y - m_Z * v.m_Z + m_W * v.m_W;

  return result;
}

/** Division and Assignment Operator */
template< typename T >
const Versor< T > &
Versor< T >
::operator/=(const Self & v)
{
  const double mx = -m_W * v.m_X + m_Z * v.m_Y - m_Y * v.m_Z + m_X * v.m_W;
  const double my = -m_Z * v.m_X - m_W * v.m_Y + m_X * v.m_Z + m_Y * v.m_W;
  const double mz =  m_Y * v.m_X - m_X * v.m_Y - m_W * v.m_Z + m_Z * v.m_W;
  const double mw =  m_X * v.m_X + m_Y * v.m_Y + m_Z * v.m_Z + m_W * v.m_W;

  m_X = mx;
  m_Y = my;
  m_Z = mz;
  m_W = mw;

  return *this;
}

/** Division Operator  */
template< typename T >
Versor< T >
Versor< T >
::operator/(const Self & v) const
{
  Self result;

  result.m_X = -m_W * v.m_X + m_Z * v.m_Y - m_Y * v.m_Z + m_X * v.m_W;
  result.m_Y = -m_Z * v.m_X - m_W * v.m_Y + m_X * v.m_Z + m_Y * v.m_W;
  result.m_Z =  m_Y * v.m_X - m_X * v.m_Y - m_W * v.m_Z + m_Z * v.m_W;
  result.m_W =  m_X * v.m_X + m_Y * v.m_Y + m_Z * v.m_Z + m_W * v.m_W;

  return result;
}

/** Comparison operator */
template< typename T >
bool
Versor< T >
::operator!=(const Self & v) const
{
  return !( *this == v );
}

/** Comparison operator */
template< typename T >
bool
Versor< T >
::operator==(const Self & v) const
{
  // Evaluate the quaternion ratio between them
  Self ratio = *this * v.GetReciprocal();

  const typename itk::NumericTraits< T >::AccumulateType
  square = ratio.m_W * ratio.m_W;

  const double epsilon = 1e-300;

  if ( std::fabs(1.0f - square) < epsilon )
    {
    return true;
    }

  return false;
}

/** Get Conjugate */
template< typename T >
Versor< T >
Versor< T >
::GetConjugate(void) const
{
  Self result;

  result.m_X = -m_X;
  result.m_Y = -m_Y;
  result.m_Z = -m_Z;
  result.m_W =  m_W;

  return result;
}

/** Get Reciprocal */
template< typename T >
Versor< T >
Versor< T >
::GetReciprocal(void) const
{
  Self result;

  result.m_X = -m_X;
  result.m_Y = -m_Y;
  result.m_Z = -m_Z;
  result.m_W =  m_W;

  return result;
}

/** Get Tensor part */
template< typename T >
typename Versor< T >::ValueType
Versor< T >
::GetTensor(void) const
{
  const ValueType tensor =
    static_cast< ValueType >(
      std::sqrt(m_X * m_X + m_Y * m_Y + m_Z * m_Z + m_W * m_W) );

  return tensor;
}

/** Normalize */
template< typename T >
void
Versor< T >
::Normalize(void)
{
  const ValueType tensor = this->GetTensor();

  if ( std::fabs(tensor) < 1e-20 )
    {
    ExceptionObject except;
    except.SetDescription("Attempt to normalize a \
                           itk::Versor with zero tensor");
    except.SetLocation(__FILE__);
    throw except;
    }
  m_X /= tensor;
  m_Y /= tensor;
  m_Z /= tensor;
  m_W /= tensor;
}

/** Get Axis */
template< typename T >
typename Versor< T >::VectorType
Versor< T >
::GetAxis(void) const
{
  VectorType axis;

  const RealType ax = static_cast< RealType >( m_X );
  const RealType ay = static_cast< RealType >( m_Y );
  const RealType az = static_cast< RealType >( m_Z );

  const RealType vectorNorm = std::sqrt(ax * ax  +  ay * ay  +  az * az);

  if ( vectorNorm == NumericTraits< RealType >::ZeroValue() )
    {
    axis[0] = NumericTraits< T >::ZeroValue();
    axis[1] = NumericTraits< T >::ZeroValue();
    axis[2] = NumericTraits< T >::ZeroValue();
    }
  else
    {
    axis[0] = m_X / vectorNorm;
    axis[1] = m_Y / vectorNorm;
    axis[2] = m_Z / vectorNorm;
    }

  return axis;
}

/** Get Right part */
template< typename T >
typename Versor< T >::VectorType
Versor< T >
::GetRight(void) const
{
  VectorType axis;

  axis[0] = m_X;
  axis[1] = m_Y;
  axis[2] = m_Z;

  return axis;
}

/** Get Scalar part */
template< typename T >
typename Versor< T >::ValueType
Versor< T >
::GetScalar(void) const
{
  return m_W;
}

/** Get Angle (in radians) */
template< typename T >
typename Versor< T >::ValueType
Versor< T >
::GetAngle(void) const
{
  const RealType ax = static_cast< RealType >( m_X );
  const RealType ay = static_cast< RealType >( m_Y );
  const RealType az = static_cast< RealType >( m_Z );

  const RealType vectorNorm = std::sqrt(ax * ax  +  ay * ay  +  az * az);

  const ValueType angle = 2.0 * std::atan2( vectorNorm, static_cast< RealType >( m_W ) );

  return angle;
}

/** Get the Square root of the unit quaternion */
template< typename T >
Versor< T >
Versor< T >
::SquareRoot(void) const
{
  const ValueType newScalar = std::sqrt( static_cast< double >( 1.0 + m_W ) );
  const double    sqrtOfTwo    = std::sqrt(2.0f);

  const double factor = 1.0f / ( newScalar * sqrtOfTwo );

  Self result;

  result.m_X = m_X * factor;
  result.m_Y = m_Y * factor;
  result.m_Z = m_Z * factor;
  result.m_W = newScalar / sqrtOfTwo;

  return result;
}

/** Compute the Exponential of the quaternion */
template< typename T >
Versor< T >
Versor< T >
::Exponential(ValueType exponent) const
{
  Self result;

  result.Set(this->GetAxis(),
             this->GetAngle() * exponent);

  return result;
}

/** Set Axis and Angle (in radians) */
template< typename T >
void
Versor< T >
::Set(const VectorType & axis, ValueType angle)
{
  const RealType vectorNorm = axis.GetNorm();
  if ( Math::FloatAlmostEqual<T>(vectorNorm, 0.0) )
    {
    ExceptionObject except;
    except.SetDescription("Attempt to set rotation axis with zero norm");
    except.SetLocation(__FILE__);
    throw except;
    }

  const RealType cosangle2 = std::cos(angle / 2.0);
  const RealType sinangle2 = std::sin(angle / 2.0);

  const RealType factor = sinangle2 / vectorNorm;

  m_X = axis[0] * factor;
  m_Y = axis[1] * factor;
  m_Z = axis[2] * factor;

  m_W = cosangle2;
}

/**  Set using an orthogonal matrix. */
template< typename T >
void
Versor< T >
::Set(const MatrixType & mat)
{
  //const double epsilon = 1e-30;
  //Keep the epsilon value large enough so that the alternate routes of
  //computing the quaternion are used to within floating point precision of the
  //math to be used.  Using 1e-30 results in degenerate matries for rotations
  //near itk::Math::pi due to imprecision of the math.  0.5/std::sqrt(trace) is
  //not accurate to 1e-30, so the resulting matrices would have very large
  //errors.  By decreasing this epsilon value to a higher tolerance, the
  //alternate stable methods for conversion are used.
  //
  //The use of std::numeric_limits< T >::epsilon() was not consistent with
  //the rest of the ITK toolkit with respect to epsilon values for
  //determining rotational orthogonality, and it occasionally
  //prevented the conversion between different rigid transform types.

  const T epsilon = Self::Epsilon(); // vnl_sqrt( std::numeric_limits< T >::epsilon() );
  // Use a slightly less epsilon for detecting difference
  const T epsilonDiff = Self::Epsilon(); //std::numeric_limits< T >::epsilon() * 10.0;

  const vnl_matrix< T > m( mat.GetVnlMatrix() );

  //check for orthonormality and that it isn't a reflection
  const vnl_matrix_fixed< T, 3, 3 > & I = m*m.transpose();
  if( std::abs( I[0][1] ) > epsilon || std::abs( I[0][2] ) > epsilon ||
    std::abs( I[1][0] ) > epsilon || std::abs( I[1][2] ) > epsilon ||
    std::abs( I[2][0] ) > epsilon || std::abs( I[2][1] ) > epsilon ||
    std::abs( I[0][0] - itk::NumericTraits<T>::OneValue() ) > epsilonDiff ||
    std::abs( I[1][1] - itk::NumericTraits<T>::OneValue() ) > epsilonDiff ||
    std::abs( I[2][2] - itk::NumericTraits<T>::OneValue() ) > epsilonDiff ||
    vnl_det( I ) < 0 )
    {
    itkGenericExceptionMacro(<< "The following matrix does not represent rotation to within an epsion of "
      << epsilon << "." << std::endl
      << m << std::endl
      << "det(m * m transpose) is: " << vnl_det(I) << std::endl
      << "m * m transpose is:" << std::endl
      << I << std::endl);
    }

  const double trace = m(0, 0) + m(1, 1) + m(2, 2) + 1.0;

  if ( trace > epsilon )
    {
    const double s = 0.5 / std::sqrt(trace);
    m_W = 0.25 / s;
    m_X = ( m(2, 1) - m(1, 2) ) * s;
    m_Y = ( m(0, 2) - m(2, 0) ) * s;
    m_Z = ( m(1, 0) - m(0, 1) ) * s;
    }
  else
    {
    if ( m(0, 0) > m(1, 1) && m(0, 0) > m(2, 2) )
      {
      const double s = 2.0 * std::sqrt( 1.0 + m(0, 0) - m(1, 1) - m(2, 2) );
      m_X = 0.25 * s;
      m_Y = ( m(0, 1) + m(1, 0) ) / s;
      m_Z = ( m(0, 2) + m(2, 0) ) / s;
      m_W = ( m(1, 2) - m(2, 1) ) / s;
      }
    else
      {
      if ( m(1, 1) > m(2, 2) )
        {
        const double s = 2.0 * std::sqrt( 1.0 + m(1, 1) - m(0, 0) - m(2, 2) );
        m_X = ( m(0, 1) + m(1, 0) ) / s;
        m_Y = 0.25 * s;
        m_Z = ( m(1, 2) + m(2, 1) ) / s;
        m_W = ( m(0, 2) - m(2, 0) ) / s;
        }
      else
        {
        const double s = 2.0 * std::sqrt( 1.0 + m(2, 2) - m(0, 0) - m(1, 1) );
        m_X = ( m(0, 2) + m(2, 0) ) / s;
        m_Y = ( m(1, 2) + m(2, 1) ) / s;
        m_Z = 0.25 * s;
        m_W = ( m(0, 1) - m(1, 0) ) / s;
        }
      }
    }
  this->Normalize();
}

/** Set right Part (in radians) */
template< typename T >
void
Versor< T >
::Set(const VectorType & axis)
{
  const ValueType sinangle2 =  axis.GetNorm();
  if ( sinangle2 > NumericTraits< ValueType >::OneValue() )
    {
    ExceptionObject exception;
    exception.SetDescription("Trying to initialize a Versor with " \
                             "a vector whose magnitude is greater than 1");
    exception.SetLocation("itk::Versor::Set( const VectorType )");
    throw exception;
    }

  const ValueType cosangle2 =  std::sqrt(NumericTraits< double >::OneValue() - sinangle2 * sinangle2);

  m_X = axis[0];
  m_Y = axis[1];
  m_Z = axis[2];

  m_W = cosangle2;
}

/** Set the Versor from four components.
 *  After assignment, the quaternion is normalized
 *  in order to get a consistent Versor (unit quaternion). */
template< typename T >
void
Versor< T >
::Set(T x, T y, T z, T w)
{
  //
  // We assume in this class that the W component is always non-negative.
  // The rotation represented by a Versor remains unchanged if all its
  // four components are negated simultaneously. Therefore, if we are
  // requested to initialize a Versor with a negative W, we negate the
  // signs of all the components.
  //
  if ( w < 0.0 )
    {
    m_X = -x;
    m_Y = -y;
    m_Z = -z;
    m_W = -w;
    }
  else
    {
    m_X = x;
    m_Y = y;
    m_Z = z;
    m_W = w;
    }
  this->Normalize();
}

/** Set from a vnl_quaternion
 *  After assignment, the quaternion is normalized
 *  in order to get a consistent Versor (unit quaternion). */
template< typename T >
void
Versor< T >
::Set(const VnlQuaternionType & quaternion)
{
  m_X = quaternion.x();
  m_Y = quaternion.y();
  m_Z = quaternion.z();
  m_W = quaternion.r();
  this->Normalize();
}

/** Set rotation around X axis */
template< typename T >
void
Versor< T >
::SetRotationAroundX(ValueType angle)
{
  const ValueType sinangle2 = std::sin(angle / 2.0);
  const ValueType cosangle2 = std::cos(angle / 2.0);

  m_X = sinangle2;
  m_Y = NumericTraits< T >::ZeroValue();
  m_Z = NumericTraits< T >::ZeroValue();
  m_W = cosangle2;
}

/** Set rotation around Y axis  */
template< typename T >
void
Versor< T >
::SetRotationAroundY(ValueType angle)
{
  const ValueType sinangle2 = std::sin(angle / 2.0);
  const ValueType cosangle2 = std::cos(angle / 2.0);

  m_X = NumericTraits< T >::ZeroValue();
  m_Y = sinangle2;
  m_Z = NumericTraits< T >::ZeroValue();
  m_W = cosangle2;
}

/**  Set rotation around Z axis  */
template< typename T >
void
Versor< T >
::SetRotationAroundZ(ValueType angle)
{
  const ValueType sinangle2 = std::sin(angle / 2.0);
  const ValueType cosangle2 = std::cos(angle / 2.0);

  m_X = NumericTraits< T >::ZeroValue();
  m_Y = NumericTraits< T >::ZeroValue();
  m_Z = sinangle2;
  m_W = cosangle2;
}

namespace {
  template< typename InputVectorType, typename ValueType, typename OutputVectorType >
    inline const OutputVectorType localTransformVectorMath(const InputVectorType & VectorObject,
      const ValueType & inputX,
      const ValueType & inputY,
      const ValueType & inputZ,
      const ValueType & inputW)
      {
      const ValueType xx = inputX * inputX;
      const ValueType yy = inputY * inputY;
      const ValueType zz = inputZ * inputZ;
      const ValueType xy = inputX * inputY;
      const ValueType xz = inputX * inputZ;
      const ValueType xw = inputX * inputW;
      const ValueType yz = inputY * inputZ;
      const ValueType yw = inputY * inputW;
      const ValueType zw = inputZ * inputW;

      const ValueType mxx = 1.0 - 2.0 * ( yy + zz );
      const ValueType myy = 1.0 - 2.0 * ( xx + zz );
      const ValueType mzz = 1.0 - 2.0 * ( xx + yy );
      const ValueType mxy = 2.0 * ( xy - zw );
      const ValueType mxz = 2.0 * ( xz + yw );
      const ValueType myx = 2.0 * ( xy + zw );
      const ValueType mzx = 2.0 * ( xz - yw );
      const ValueType mzy = 2.0 * ( yz + xw );
      const ValueType myz = 2.0 * ( yz - xw );

      OutputVectorType result;
      result[0] = mxx * VectorObject[0] + mxy * VectorObject[1] + mxz * VectorObject[2];
      result[1] = myx * VectorObject[0] + myy * VectorObject[1] + myz * VectorObject[2];
      result[2] = mzx * VectorObject[0] + mzy * VectorObject[1] + mzz * VectorObject[2];
      return result;
      }
}

/** Transform a Vector */
template< typename T >
typename Versor< T >::VectorType
Versor< T >
::Transform(const VectorType & v) const
{
  return localTransformVectorMath<VectorType,T,typename Versor< T >::VectorType>(v,this->m_X,this->m_Y,this->m_Z,this->m_W);
}

/** Transform a CovariantVector
 *  given that this is an orthogonal transformation
 *  CovariantVectors are transformed as vectors. */
template< typename T >
typename Versor< T >::CovariantVectorType
Versor< T >
::Transform(const CovariantVectorType & v) const
{
  return localTransformVectorMath<CovariantVectorType,T,typename Versor< T >::CovariantVectorType>(v,this->m_X,this->m_Y,this->m_Z,this->m_W);
}

/** Transform a Point */
template< typename T >
typename Versor< T >::PointType
Versor< T >
::Transform(const PointType & v) const
{
  return localTransformVectorMath<PointType,T,typename Versor< T >::PointType>(v,this->m_X,this->m_Y,this->m_Z,this->m_W);
}

/** Transform a VnlVector */
template< typename T >
typename Versor< T >::VnlVectorType
Versor< T >
::Transform(const VnlVectorType & v) const
{
  return localTransformVectorMath<VnlVectorType,T,typename Versor< T >::VnlVectorType>(v,this->m_X,this->m_Y,this->m_Z,this->m_W);
}

/** Get Matrix representation */
template< typename T >
Matrix< T, 3, 3 >
Versor< T >
::GetMatrix(void) const
{
  Matrix< T, 3, 3 > matrix;

  const RealType xx = m_X * m_X;
  const RealType yy = m_Y * m_Y;
  const RealType zz = m_Z * m_Z;
  const RealType xy = m_X * m_Y;
  const RealType xz = m_X * m_Z;
  const RealType xw = m_X * m_W;
  const RealType yz = m_Y * m_Z;
  const RealType yw = m_Y * m_W;
  const RealType zw = m_Z * m_W;

  matrix[0][0] = 1.0 - 2.0 * ( yy + zz );
  matrix[1][1] = 1.0 - 2.0 * ( xx + zz );
  matrix[2][2] = 1.0 - 2.0 * ( xx + yy );
  matrix[0][1] = 2.0 * ( xy - zw );
  matrix[0][2] = 2.0 * ( xz + yw );
  matrix[1][0] = 2.0 * ( xy + zw );
  matrix[2][0] = 2.0 * ( xz - yw );
  matrix[2][1] = 2.0 * ( yz + xw );
  matrix[1][2] = 2.0 * ( yz - xw );

  return matrix;
}
} // end namespace itk

#endif
