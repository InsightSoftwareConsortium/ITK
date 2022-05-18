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
#ifndef itkCovariantVector_hxx
#define itkCovariantVector_hxx

#include "itkMath.h"
#include "itkNumericTraits.h"

namespace itk
{
template <typename T, unsigned int TVectorDimension>
CovariantVector<T, TVectorDimension>::CovariantVector(const ValueType & r)
  : Superclass{ r }
{}

template <typename T, unsigned int VVectorDimension>
CovariantVector<T, VVectorDimension> &
CovariantVector<T, VVectorDimension>::operator=(const ValueType r[VVectorDimension])
{
  BaseArray::operator=(r);
  return *this;
}

template <typename T, unsigned int VVectorDimension>
auto
CovariantVector<T, VVectorDimension>::operator+=(const Self & vec) -> const Self &
{
  for (unsigned int i = 0; i < VVectorDimension; ++i)
  {
    (*this)[i] += vec[i];
  }
  return *this;
}

template <typename T, unsigned int VVectorDimension>
auto
CovariantVector<T, VVectorDimension>::operator-=(const Self & vec) -> const Self &
{
  for (unsigned int i = 0; i < VVectorDimension; ++i)
  {
    (*this)[i] -= vec[i];
  }
  return *this;
}

template <typename T, unsigned int VVectorDimension>
CovariantVector<T, VVectorDimension>
CovariantVector<T, VVectorDimension>::operator-() const
{
  Self result;

  for (unsigned int i = 0; i < VVectorDimension; ++i)
  {
    result[i] = -(*this)[i];
  }
  return result;
}

template <typename T, unsigned int VVectorDimension>
auto
CovariantVector<T, VVectorDimension>::operator+(const Self & vec) const -> Self
{
  Self result;

  for (unsigned int i = 0; i < VVectorDimension; ++i)
  {
    result[i] = (*this)[i] + vec[i];
  }
  return result;
}

template <typename T, unsigned int VVectorDimension>
auto
CovariantVector<T, VVectorDimension>::operator-(const Self & vec) const -> Self
{
  Self result;

  for (unsigned int i = 0; i < VVectorDimension; ++i)
  {
    result[i] = (*this)[i] - vec[i];
  }
  return result;
}

template <typename T, unsigned int VVectorDimension>
typename CovariantVector<T, VVectorDimension>::ValueType CovariantVector<T, VVectorDimension>::operator*(
  const Self & other) const
{
  typename NumericTraits<T>::AccumulateType value = NumericTraits<T>::ZeroValue();
  for (unsigned int i = 0; i < VVectorDimension; ++i)
  {
    value += (*this)[i] * other[i];
  }
  return static_cast<ValueType>(value);
}

template <typename T, unsigned int VVectorDimension>
typename CovariantVector<T, VVectorDimension>::ValueType CovariantVector<T, VVectorDimension>::operator*(
  const Vector<T, VVectorDimension> & other) const
{
  typename NumericTraits<T>::AccumulateType value = NumericTraits<T>::ZeroValue();
  for (unsigned int i = 0; i < VVectorDimension; ++i)
  {
    value += (*this)[i] * other[i];
  }
  return value;
}

template <typename T, unsigned int VVectorDimension>
auto
CovariantVector<T, VVectorDimension>::GetSquaredNorm() const -> RealValueType
{
  RealValueType sum = NumericTraits<RealValueType>::ZeroValue();

  for (unsigned int i = 0; i < VVectorDimension; ++i)
  {
    const RealValueType value = (*this)[i];
    sum += value * value;
  }
  return sum;
}

template <typename T, unsigned int VVectorDimension>
auto
CovariantVector<T, VVectorDimension>::GetNorm() const -> RealValueType
{
  return std::sqrt(this->GetSquaredNorm());
}

template <typename T, unsigned int VVectorDimension>
auto
CovariantVector<T, VVectorDimension>::Normalize() -> RealValueType
{
  const RealValueType norm = this->GetNorm();

  for (unsigned int i = 0; i < VVectorDimension; ++i)
  {
    (*this)[i] /= norm;
  }

  return norm;
}

template <typename T, unsigned int VVectorDimension>
void
CovariantVector<T, VVectorDimension>::SetVnlVector(const vnl_vector<T> & v)
{
  for (unsigned int i = 0; i < v.size(); ++i)
  {
    (*this)[i] = v(i);
  }
}

template <typename T, unsigned int VVectorDimension>
vnl_vector_ref<T>
CovariantVector<T, VVectorDimension>::GetVnlVector()
{
  return vnl_vector_ref<T>(VVectorDimension, this->GetDataPointer());
}

template <typename T, unsigned int VVectorDimension>
vnl_vector<T>
CovariantVector<T, VVectorDimension>::GetVnlVector() const
{
  // Return a vector_ref<>.  This will be automatically converted to a
  // vnl_vector<>.  We have to use a const_cast<> which would normally
  // be prohibited in a const method, but it is safe to do here
  // because the cast to vnl_vector<> will ultimately copy the data.
  return vnl_vector_ref<T>(VVectorDimension, const_cast<T *>(this->GetDataPointer()));
}

} // end namespace itk

#endif
