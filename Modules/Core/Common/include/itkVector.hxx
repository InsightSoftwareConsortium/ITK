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
#ifndef itkVector_hxx
#define itkVector_hxx

#include "itkMath.h"
#include "vnl/vnl_vector.h"
#include "itkObject.h"
#include "itkNumericTraitsVectorPixel.h"

namespace itk
{
template <typename T, unsigned int TVectorDimension>
Vector<T, TVectorDimension>::Vector(const ValueType & r)
  : Superclass{ r }
{}

template <typename T, unsigned int TVectorDimension>
Vector<T, TVectorDimension> &
Vector<T, TVectorDimension>::operator=(const ValueType r[TVectorDimension])
{
  BaseArray::operator=(r);
  return *this;
}

template <typename T, unsigned int TVectorDimension>
auto
Vector<T, TVectorDimension>::operator+=(const Self & vec) -> const Self &
{
  for (unsigned int i = 0; i < TVectorDimension; ++i)
  {
    (*this)[i] += vec[i];
  }
  return *this;
}

template <typename T, unsigned int TVectorDimension>
auto
Vector<T, TVectorDimension>::operator-=(const Self & vec) -> const Self &
{
  for (unsigned int i = 0; i < TVectorDimension; ++i)
  {
    (*this)[i] -= vec[i];
  }
  return *this;
}

template <typename T, unsigned int TVectorDimension>
Vector<T, TVectorDimension>
Vector<T, TVectorDimension>::operator-() const
{
  Self result;

  for (unsigned int i = 0; i < TVectorDimension; ++i)
  {
    result[i] = -(*this)[i];
  }
  return result;
}

template <typename T, unsigned int TVectorDimension>
Vector<T, TVectorDimension>
Vector<T, TVectorDimension>::operator+(const Self & vec) const
{
  Self result;

  for (unsigned int i = 0; i < TVectorDimension; ++i)
  {
    result[i] = (*this)[i] + vec[i];
  }
  return result;
}

template <typename T, unsigned int TVectorDimension>
Vector<T, TVectorDimension>
Vector<T, TVectorDimension>::operator-(const Self & vec) const
{
  Self result;

  for (unsigned int i = 0; i < TVectorDimension; ++i)
  {
    result[i] = (*this)[i] - vec[i];
  }
  return result;
}

template <typename T, unsigned int TVectorDimension>
auto
Vector<T, TVectorDimension>::GetSquaredNorm() const -> RealValueType
{
  T sum{};
  for (unsigned int i = 0; i < TVectorDimension; ++i)
  {
    const RealValueType value = (*this)[i];
    sum += value * value;
  }
  return sum;
}

template <typename T, unsigned int TVectorDimension>
auto
Vector<T, TVectorDimension>::GetNorm() const -> RealValueType
{
  return RealValueType(std::sqrt(static_cast<double>(this->GetSquaredNorm())));
}

template <typename T, unsigned int TVectorDimension>
auto
Vector<T, TVectorDimension>::Normalize() -> RealValueType
{
  const RealValueType norm = this->GetNorm();
  if (norm < NumericTraits<RealValueType>::epsilon())
  {
    return norm; // Prevent division by 0
  }

  const RealValueType inversedNorm = 1.0 / norm;
  for (unsigned int i = 0; i < TVectorDimension; ++i)
  {
    (*this)[i] = static_cast<T>(static_cast<RealValueType>((*this)[i] * inversedNorm));
  }
  return norm;
}

template <typename T, unsigned int TVectorDimension>
vnl_vector_ref<T>
Vector<T, TVectorDimension>::GetVnlVector()
{
  return vnl_vector_ref<T>(TVectorDimension, this->GetDataPointer());
}

template <typename T, unsigned int TVectorDimension>
vnl_vector<T>
Vector<T, TVectorDimension>::GetVnlVector() const
{
  return vnl_vector<T>(this->GetDataPointer(), TVectorDimension);
}

template <typename T, unsigned int TVectorDimension>
void
Vector<T, TVectorDimension>::SetVnlVector(const vnl_vector<T> & v)
{
  const unsigned int elements_to_copy = std::min<unsigned int>(TVectorDimension, v.size());
  for (unsigned int i = 0; i < elements_to_copy; ++i)
  {
    (*this)[i] = v(i);
  }
}

template <typename T, unsigned int TVectorDimension>
std::ostream &
operator<<(std::ostream & os, const Vector<T, TVectorDimension> & vct)
{
  os << '[';
  if constexpr (TVectorDimension == 1)
  {
    os << vct[0];
  }
  else
  {
    for (unsigned int i = 0; i + 1 < TVectorDimension; ++i)
    {
      os << vct[i] << ", ";
    }
    os << vct[TVectorDimension - 1];
  }
  os << ']';
  return os;
}

template <typename T, unsigned int TVectorDimension>
std::istream &
operator>>(std::istream & is, Vector<T, TVectorDimension> & vct)
{
  for (unsigned int i = 0; i < TVectorDimension; ++i)
  {
    is >> vct[i];
  }
  return is;
}

template <typename T, unsigned int TVectorDimension>
typename Vector<T, TVectorDimension>::ValueType
Vector<T, TVectorDimension>::operator*(const Self & other) const
{
  T value{};
  for (unsigned int i = 0; i < TVectorDimension; ++i)
  {
    value += (*this)[i] * other[i];
  }
  return value;
}

} // end namespace itk
#endif
