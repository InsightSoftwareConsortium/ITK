/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

#include "itkNumericTraits.h"
#include "itkMath.h"
#include "itkBridgeMathDeterminant.h"
#include <algorithm>
#include <limits>

namespace itk
{
template <typename T>
void
Versor<T>::SetIdentity()
{
  m_X = T{};
  m_Y = T{};
  m_Z = T{};
  m_W = NumericTraits<T>::OneValue();
}

template <typename T>
vnl_quaternion<T>
Versor<T>::GetVnlQuaternion() const
{
  return vnl_quaternion<T>(m_X, m_Y, m_Z, m_W);
}

template <typename T>
const Versor<T> &
Versor<T>::operator*=(const Self & v)
{
  const double mx = m_W * v.m_X - m_Z * v.m_Y + m_Y * v.m_Z + m_X * v.m_W;
  const double my = m_Z * v.m_X + m_W * v.m_Y - m_X * v.m_Z + m_Y * v.m_W;
  const double mz = -m_Y * v.m_X + m_X * v.m_Y + m_W * v.m_Z + m_Z * v.m_W;
  const double mw = -m_X * v.m_X - m_Y * v.m_Y - m_Z * v.m_Z + m_W * v.m_W;

  m_X = mx;
  m_Y = my;
  m_Z = mz;
  m_W = mw;

  return *this;
}

template <typename T>
Versor<T>
Versor<T>::operator*(const Self & v) const
{
  Self result;

  result.m_X = m_W * v.m_X - m_Z * v.m_Y + m_Y * v.m_Z + m_X * v.m_W;
  result.m_Y = m_Z * v.m_X + m_W * v.m_Y - m_X * v.m_Z + m_Y * v.m_W;
  result.m_Z = -m_Y * v.m_X + m_X * v.m_Y + m_W * v.m_Z + m_Z * v.m_W;
  result.m_W = -m_X * v.m_X - m_Y * v.m_Y - m_Z * v.m_Z + m_W * v.m_W;

  return result;
}

template <typename T>
const Versor<T> &
Versor<T>::operator/=(const Self & v)
{
  const double mx = -m_W * v.m_X + m_Z * v.m_Y - m_Y * v.m_Z + m_X * v.m_W;
  const double my = -m_Z * v.m_X - m_W * v.m_Y + m_X * v.m_Z + m_Y * v.m_W;
  const double mz = m_Y * v.m_X - m_X * v.m_Y - m_W * v.m_Z + m_Z * v.m_W;
  const double mw = m_X * v.m_X + m_Y * v.m_Y + m_Z * v.m_Z + m_W * v.m_W;

  m_X = mx;
  m_Y = my;
  m_Z = mz;
  m_W = mw;

  return *this;
}

template <typename T>
Versor<T>
Versor<T>::operator/(const Self & v) const
{
  Self result;

  result.m_X = -m_W * v.m_X + m_Z * v.m_Y - m_Y * v.m_Z + m_X * v.m_W;
  result.m_Y = -m_Z * v.m_X - m_W * v.m_Y + m_X * v.m_Z + m_Y * v.m_W;
  result.m_Z = m_Y * v.m_X - m_X * v.m_Y - m_W * v.m_Z + m_Z * v.m_W;
  result.m_W = m_X * v.m_X + m_Y * v.m_Y + m_Z * v.m_Z + m_W * v.m_W;

  return result;
}

template <typename T>
bool
Versor<T>::operator==(const Self & v) const
{
  // Evaluate the quaternion ratio between them
  const Self ratio = *this * v.GetReciprocal();

  const typename itk::NumericTraits<T>::AccumulateType square = ratio.m_W * ratio.m_W;

  constexpr double epsilon{ 1e-300 };

  if (itk::Math::Absolute(1.0f - square) < epsilon)
  {
    return true;
  }

  return false;
}

template <typename T>
Versor<T>
Versor<T>::GetConjugate() const
{
  Self result;

  result.m_X = -m_X;
  result.m_Y = -m_Y;
  result.m_Z = -m_Z;
  result.m_W = m_W;

  return result;
}

template <typename T>
Versor<T>
Versor<T>::GetReciprocal() const
{
  Self result;

  result.m_X = -m_X;
  result.m_Y = -m_Y;
  result.m_Z = -m_Z;
  result.m_W = m_W;

  return result;
}

template <typename T>
auto
Versor<T>::GetTensor() const -> ValueType
{
  const auto tensor = static_cast<ValueType>(std::sqrt(m_X * m_X + m_Y * m_Y + m_Z * m_Z + m_W * m_W));

  return tensor;
}

template <typename T>
void
Versor<T>::Normalize()
{
  const ValueType tensor = this->GetTensor();

  if (itk::Math::Absolute(tensor) < 1e-20)
  {
    ExceptionObject except;
    except.SetDescription("Attempt to normalize a itk::Versor with zero tensor");
    except.SetLocation(__FILE__);
    throw except;
  }
  m_X /= tensor;
  m_Y /= tensor;
  m_Z /= tensor;
  m_W /= tensor;
}

template <typename T>
auto
Versor<T>::GetAxis() const -> VectorType
{
  VectorType axis;

  const auto ax = static_cast<RealType>(m_X);
  const auto ay = static_cast<RealType>(m_Y);
  const auto az = static_cast<RealType>(m_Z);

  const RealType vectorNorm = std::sqrt(ax * ax + ay * ay + az * az);

  if (vectorNorm == RealType{})
  {
    axis[0] = T{};
    axis[1] = T{};
    axis[2] = T{};
  }
  else
  {
    axis[0] = m_X / vectorNorm;
    axis[1] = m_Y / vectorNorm;
    axis[2] = m_Z / vectorNorm;
  }

  return axis;
}

template <typename T>
auto
Versor<T>::GetRight() const -> VectorType
{
  VectorType axis;

  axis[0] = m_X;
  axis[1] = m_Y;
  axis[2] = m_Z;

  return axis;
}

template <typename T>
auto
Versor<T>::GetScalar() const -> ValueType
{
  return m_W;
}

template <typename T>
auto
Versor<T>::GetAngle() const -> ValueType
{
  const auto ax = static_cast<RealType>(m_X);
  const auto ay = static_cast<RealType>(m_Y);
  const auto az = static_cast<RealType>(m_Z);

  const RealType vectorNorm = std::sqrt(ax * ax + ay * ay + az * az);

  const ValueType angle = 2.0 * std::atan2(vectorNorm, static_cast<RealType>(m_W));

  return angle;
}

template <typename T>
Versor<T>
Versor<T>::SquareRoot() const
{
  const ValueType newScalar = std::sqrt(static_cast<double>(1.0 + m_W));
  const double    sqrtOfTwo = std::sqrt(2.0f);

  const double factor = 1.0f / (newScalar * sqrtOfTwo);

  Self result;

  result.m_X = m_X * factor;
  result.m_Y = m_Y * factor;
  result.m_Z = m_Z * factor;
  result.m_W = newScalar / sqrtOfTwo;

  return result;
}

template <typename T>
Versor<T>
Versor<T>::Exponential(ValueType exponent) const
{
  Self result;

  result.Set(this->GetAxis(), this->GetAngle() * exponent);

  return result;
}

template <typename T>
void
Versor<T>::Set(const VectorType & axis, ValueType angle)
{
  const RealType vectorNorm = axis.GetNorm();
  if (Math::FloatAlmostEqual<T>(vectorNorm, 0.0))
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

template <typename T>
void
Versor<T>::Set(const MatrixType & mat)
{
  // Rounding each entry to T perturbs m * m^T by about numeric_limits<T>::epsilon().
  const double tolerance = std::max<double>(Self::Epsilon(), 4.0 * std::numeric_limits<T>::epsilon());

  const vnl_matrix<T>            m(mat.GetVnlMatrix());
  vnl_matrix_fixed<double, 3, 3> md;
  for (unsigned int r = 0; r < 3; ++r)
  {
    for (unsigned int c = 0; c < 3; ++c)
    {
      md(r, c) = m(r, c);
    }
  }

  // check for orthonormality and that it isn't a reflection
  const vnl_matrix_fixed<double, 3, 3> I = md * md.transpose();
  if (itk::Math::Absolute(I[0][1]) > tolerance || itk::Math::Absolute(I[0][2]) > tolerance ||
      itk::Math::Absolute(I[1][0]) > tolerance || itk::Math::Absolute(I[1][2]) > tolerance ||
      itk::Math::Absolute(I[2][0]) > tolerance || itk::Math::Absolute(I[2][1]) > tolerance ||
      itk::Math::Absolute(I[0][0] - 1.0) > tolerance || itk::Math::Absolute(I[1][1] - 1.0) > tolerance ||
      itk::Math::Absolute(I[2][2] - 1.0) > tolerance || bridge::Math::Determinant(I) < 0)
  {
    itkGenericExceptionMacro("The following matrix does not represent rotation to within an epsilon of "
                             << tolerance << '.' << std::endl
                             << m << std::endl
                             << "det(m * m transpose) is: " << bridge::Math::Determinant(I) << std::endl
                             << "m * m transpose is:" << std::endl
                             << I);
  }

  // Shepperd's method: divide by the largest of 4w^2, 4x^2, 4y^2, 4z^2, so the divisor is never small.
  const double trace = md(0, 0) + md(1, 1) + md(2, 2);
  const double w4 = 1.0 + trace;
  const double x4 = 1.0 + 2.0 * md(0, 0) - trace;
  const double y4 = 1.0 + 2.0 * md(1, 1) - trace;
  const double z4 = 1.0 + 2.0 * md(2, 2) - trace;
  if (w4 >= x4 && w4 >= y4 && w4 >= z4)
  {
    const double s = 2.0 * std::sqrt(w4);
    m_W = 0.25 * s;
    m_X = (md(2, 1) - md(1, 2)) / s;
    m_Y = (md(0, 2) - md(2, 0)) / s;
    m_Z = (md(1, 0) - md(0, 1)) / s;
  }
  else if (x4 >= y4 && x4 >= z4)
  {
    const double s = 2.0 * std::sqrt(x4);
    m_X = 0.25 * s;
    m_Y = (md(0, 1) + md(1, 0)) / s;
    m_Z = (md(0, 2) + md(2, 0)) / s;
    m_W = (md(2, 1) - md(1, 2)) / s;
  }
  else if (y4 >= z4)
  {
    const double s = 2.0 * std::sqrt(y4);
    m_X = (md(0, 1) + md(1, 0)) / s;
    m_Y = 0.25 * s;
    m_Z = (md(1, 2) + md(2, 1)) / s;
    m_W = (md(0, 2) - md(2, 0)) / s;
  }
  else
  {
    const double s = 2.0 * std::sqrt(z4);
    m_X = (md(0, 2) + md(2, 0)) / s;
    m_Y = (md(1, 2) + md(2, 1)) / s;
    m_Z = 0.25 * s;
    m_W = (md(1, 0) - md(0, 1)) / s;
  }
  this->Normalize();
  // Keep w >= 0, as Set(x, y, z, w) does: Set(const VectorType &) rebuilds w as a positive root.
  if (m_W < 0)
  {
    m_W = -m_W;
    m_X = -m_X;
    m_Y = -m_Y;
    m_Z = -m_Z;
  }
}

template <typename T>
void
Versor<T>::Set(const VectorType & axis)
{
  const ValueType sinangle2 = axis.GetNorm();
  if (sinangle2 > NumericTraits<ValueType>::OneValue())
  {
    ExceptionObject exception;
    exception.SetDescription("Trying to initialize a Versor with a vector whose magnitude is greater than 1");
    exception.SetLocation("itk::Versor::Set( const VectorType )");
    throw exception;
  }

  const ValueType cosangle2 = std::sqrt(1.0 - sinangle2 * sinangle2);

  m_X = axis[0];
  m_Y = axis[1];
  m_Z = axis[2];

  m_W = cosangle2;
}

template <typename T>
void
Versor<T>::Set(T x, T y, T z, T w)
{
  //
  // We assume in this class that the W component is always non-negative.
  // The rotation represented by a Versor remains unchanged if all its
  // four components are negated simultaneously. Therefore, if we are
  // requested to initialize a Versor with a negative W, we negate the
  // signs of all the components.
  //
  if (w < 0.0)
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

template <typename T>
void
Versor<T>::Set(const VnlQuaternionType & quaternion)
{
  m_X = quaternion.x();
  m_Y = quaternion.y();
  m_Z = quaternion.z();
  m_W = quaternion.r();
  this->Normalize();
}

template <typename T>
void
Versor<T>::SetRotationAroundX(ValueType angle)
{
  const ValueType sinangle2 = std::sin(angle / 2.0);
  const ValueType cosangle2 = std::cos(angle / 2.0);

  m_X = sinangle2;
  m_Y = T{};
  m_Z = T{};
  m_W = cosangle2;
}

template <typename T>
void
Versor<T>::SetRotationAroundY(ValueType angle)
{
  const ValueType sinangle2 = std::sin(angle / 2.0);
  const ValueType cosangle2 = std::cos(angle / 2.0);

  m_X = T{};
  m_Y = sinangle2;
  m_Z = T{};
  m_W = cosangle2;
}

template <typename T>
void
Versor<T>::SetRotationAroundZ(ValueType angle)
{
  const ValueType sinangle2 = std::sin(angle / 2.0);
  const ValueType cosangle2 = std::cos(angle / 2.0);

  m_X = T{};
  m_Y = T{};
  m_Z = sinangle2;
  m_W = cosangle2;
}

namespace
{
template <typename InputVectorType, typename ValueType, typename OutputVectorType>
inline const OutputVectorType
localTransformVectorMath(const InputVectorType & VectorObject,
                         const ValueType &       inputX,
                         const ValueType &       inputY,
                         const ValueType &       inputZ,
                         const ValueType &       inputW)
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

  const ValueType mxx = 1.0 - 2.0 * (yy + zz);
  const ValueType myy = 1.0 - 2.0 * (xx + zz);
  const ValueType mzz = 1.0 - 2.0 * (xx + yy);
  const ValueType mxy = 2.0 * (xy - zw);
  const ValueType mxz = 2.0 * (xz + yw);
  const ValueType myx = 2.0 * (xy + zw);
  const ValueType mzx = 2.0 * (xz - yw);
  const ValueType mzy = 2.0 * (yz + xw);
  const ValueType myz = 2.0 * (yz - xw);

  OutputVectorType result;
  result[0] = mxx * VectorObject[0] + mxy * VectorObject[1] + mxz * VectorObject[2];
  result[1] = myx * VectorObject[0] + myy * VectorObject[1] + myz * VectorObject[2];
  result[2] = mzx * VectorObject[0] + mzy * VectorObject[1] + mzz * VectorObject[2];
  return result;
}
} // namespace

template <typename T>
auto
Versor<T>::Transform(const VectorType & v) const -> VectorType
{
  return localTransformVectorMath<VectorType, T, typename Versor<T>::VectorType>(
    v, this->m_X, this->m_Y, this->m_Z, this->m_W);
}

template <typename T>
auto
Versor<T>::Transform(const CovariantVectorType & v) const -> CovariantVectorType
{
  return localTransformVectorMath<CovariantVectorType, T, typename Versor<T>::CovariantVectorType>(
    v, this->m_X, this->m_Y, this->m_Z, this->m_W);
}

template <typename T>
auto
Versor<T>::Transform(const PointType & v) const -> PointType
{
  return localTransformVectorMath<PointType, T, typename Versor<T>::PointType>(
    v, this->m_X, this->m_Y, this->m_Z, this->m_W);
}

template <typename T>
auto
Versor<T>::Transform(const VnlVectorType & v) const -> VnlVectorType
{
  return localTransformVectorMath<VnlVectorType, T, typename Versor<T>::VnlVectorType>(
    v, this->m_X, this->m_Y, this->m_Z, this->m_W);
}

template <typename T>
Matrix<T, 3, 3>
Versor<T>::GetMatrix() const
{
  Matrix<T, 3, 3> matrix;

  const RealType xx = m_X * m_X;
  const RealType yy = m_Y * m_Y;
  const RealType zz = m_Z * m_Z;
  const RealType xy = m_X * m_Y;
  const RealType xz = m_X * m_Z;
  const RealType xw = m_X * m_W;
  const RealType yz = m_Y * m_Z;
  const RealType yw = m_Y * m_W;
  const RealType zw = m_Z * m_W;

  matrix[0][0] = 1.0 - 2.0 * (yy + zz);
  matrix[1][1] = 1.0 - 2.0 * (xx + zz);
  matrix[2][2] = 1.0 - 2.0 * (xx + yy);
  matrix[0][1] = 2.0 * (xy - zw);
  matrix[0][2] = 2.0 * (xz + yw);
  matrix[1][0] = 2.0 * (xy + zw);
  matrix[2][0] = 2.0 * (xz - yw);
  matrix[2][1] = 2.0 * (yz + xw);
  matrix[1][2] = 2.0 * (yz - xw);

  return matrix;
}
} // end namespace itk

#endif
