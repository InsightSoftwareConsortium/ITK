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
#ifndef itkCovariantVector_hxx
#define itkCovariantVector_hxx

#include "itkCovariantVector.h"
#include "vnl/vnl_math.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * Constructor to initialize entire vector to one value.
 */
template< typename T, unsigned int TVectorDimension >
CovariantVector< T, TVectorDimension >
::CovariantVector(const ValueType & r)
{
  for ( typename BaseArray::Iterator i = BaseArray::Begin(); i != BaseArray::End(); ++i )
    {
    *i = r;
    }
}

template< typename T, unsigned int NVectorDimension >
CovariantVector< T, NVectorDimension > &
CovariantVector< T, NVectorDimension >
::operator=(const Self & r)
{
  BaseArray::operator=(r);
  return *this;
}

template< typename T, unsigned int NVectorDimension >
CovariantVector< T, NVectorDimension > &
CovariantVector< T, NVectorDimension >
::operator=(const ValueType r[NVectorDimension])
{
  BaseArray::operator=(r);
  return *this;
}

/**
 *
 */
template< typename T, unsigned int NVectorDimension >
const typename CovariantVector< T, NVectorDimension >::Self &
CovariantVector< T, NVectorDimension >
::operator+=(const Self & vec)
{
  for ( unsigned int i = 0; i < NVectorDimension; i++ )
    {
    ( *this )[i] += vec[i];
    }
  return *this;
}

/**
 *
 */
template< typename T, unsigned int NVectorDimension >
const typename CovariantVector< T, NVectorDimension >::Self &
CovariantVector< T, NVectorDimension >
::operator-=(const Self & vec)
{
  for ( unsigned int i = 0; i < NVectorDimension; i++ )
    {
    ( *this )[i] -= vec[i];
    }
  return *this;
}

/**
 * Returns a temporary copy of a vector
 */
template< typename T, unsigned int NVectorDimension >
CovariantVector< T, NVectorDimension >
CovariantVector< T, NVectorDimension >
::operator-() const
{
  Self result;

  for ( unsigned int i = 0; i < NVectorDimension; i++ )
    {
    result[i] = -( *this )[i];
    }
  return result;
}

/**
 * Returns a temporary copy of a vector
 */
template< typename T, unsigned int NVectorDimension >
typename CovariantVector< T, NVectorDimension >::Self
CovariantVector< T, NVectorDimension >
::operator+(const Self & vec) const
{
  Self result;

  for ( unsigned int i = 0; i < NVectorDimension; i++ )
    {
    result[i] = ( *this )[i] + vec[i];
    }
  return result;
}

/**
 * Returns a temporary copy of a vector
 */
template< typename T, unsigned int NVectorDimension >
typename CovariantVector< T, NVectorDimension >::Self
CovariantVector< T, NVectorDimension >
::operator-(const Self & vec)  const
{
  Self result;

  for ( unsigned int i = 0; i < NVectorDimension; i++ )
    {
    result[i] = ( *this )[i] - vec[i];
    }
  return result;
}

/**
 *
 */
template< typename T, unsigned int NVectorDimension >
typename CovariantVector< T, NVectorDimension >::ValueType
CovariantVector< T, NVectorDimension >
::operator*(const Self & other) const
{
  typename NumericTraits< T >::AccumulateType value = NumericTraits< T >::ZeroValue();
  for ( unsigned int i = 0; i < NVectorDimension; i++ )
    {
    value += ( *this )[i] * other[i];
    }
  return static_cast< ValueType >( value );
}

/**
 *
 */
template< typename T, unsigned int NVectorDimension >
typename CovariantVector< T, NVectorDimension >::ValueType
CovariantVector< T, NVectorDimension >
::operator*(const Vector< T, NVectorDimension > & other) const
{
  typename NumericTraits< T >::AccumulateType value = NumericTraits< T >::ZeroValue();
  for ( unsigned int i = 0; i < NVectorDimension; i++ )
    {
    value += ( *this )[i] * other[i];
    }
  return value;
}

/**
 * Returns vector's Squared Euclidean Norm
 */
template< typename T, unsigned int NVectorDimension >
typename CovariantVector< T, NVectorDimension >::RealValueType
CovariantVector< T, NVectorDimension >
::GetSquaredNorm(void) const
{
  RealValueType sum = NumericTraits< RealValueType >::ZeroValue();

  for ( unsigned int i = 0; i < NVectorDimension; i++ )
    {
    const RealValueType value = ( *this )[i];
    sum += value * value;
    }
  return sum;
}

/**
 * Returns vector's Euclidean Norm
 */
template< typename T, unsigned int NVectorDimension >
typename CovariantVector< T, NVectorDimension >::RealValueType
CovariantVector< T, NVectorDimension >
::GetNorm(void) const
{
  return std::sqrt( this->GetSquaredNorm() );
}

/**
 * Divide vector's components by vector's norm
 */
template< typename T, unsigned int NVectorDimension >
void
CovariantVector< T, NVectorDimension >
::Normalize(void)
{
  const RealValueType norm = this->GetNorm();

  for ( unsigned int i = 0; i < NVectorDimension; i++ )
    {
    ( *this )[i] /= norm;
    }
}

/**
 * Set a vnl_vector
 */
template< typename T, unsigned int NVectorDimension >
void
CovariantVector< T, NVectorDimension >
::SetVnlVector(const vnl_vector< T > & v)
{
  for ( unsigned int i = 0; i < v.size(); i++ )
    {
    ( *this )[i] = v(i);
    }
}

/**
 * Return a vnl_vector_ref
 */
template< typename T, unsigned int NVectorDimension >
vnl_vector_ref< T >
CovariantVector< T, NVectorDimension >
::GetVnlVector(void)
{
  return vnl_vector_ref< T >( NVectorDimension, this->GetDataPointer() );
}

/**
 * Return a vnl_vector const
 */
template< typename T, unsigned int NVectorDimension >
vnl_vector< T >
CovariantVector< T, NVectorDimension >
::GetVnlVector(void) const
{
  // Return a vector_ref<>.  This will be automatically converted to a
  // vnl_vector<>.  We have to use a const_cast<> which would normally
  // be prohibited in a const method, but it is safe to do here
  // because the cast to vnl_vector<> will ultimately copy the data.
  return vnl_vector_ref< T >( NVectorDimension,
                              const_cast< T * >( this->GetDataPointer() ) );
}

#if !defined(ITK_LEGACY_REMOVE)
/**
 * Set a vnl_vector
 */
template< typename T, unsigned int NVectorDimension >
void
CovariantVector< T, NVectorDimension >
::Set_vnl_vector(const vnl_vector< T > & v)
{
  for ( unsigned int i = 0; i < v.size(); i++ )
    {
    ( *this )[i] = v(i);
    }
}

/**
 * Return a vnl_vector_ref
 */
template< typename T, unsigned int NVectorDimension >
vnl_vector_ref< T >
CovariantVector< T, NVectorDimension >
::Get_vnl_vector(void)
{
  return vnl_vector_ref< T >( NVectorDimension, this->GetDataPointer() );
}

/**
 * Return a vnl_vector const
 */
template< typename T, unsigned int NVectorDimension >
vnl_vector< T >
CovariantVector< T, NVectorDimension >
::Get_vnl_vector(void) const
{
  // Return a vector_ref<>.  This will be automatically converted to a
  // vnl_vector<>.  We have to use a const_cast<> which would normally
  // be prohibited in a const method, but it is safe to do here
  // because the cast to vnl_vector<> will ultimately copy the data.
  return vnl_vector_ref< T >( NVectorDimension,
                              const_cast< T * >( this->GetDataPointer() ) );
}
#endif
} // end namespace itk

#endif
