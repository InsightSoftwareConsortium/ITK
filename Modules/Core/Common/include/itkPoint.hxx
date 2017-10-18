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
#ifndef itkPoint_hxx
#define itkPoint_hxx
#include "itkPoint.h"
#include "itkNumericTraitsPointPixel.h"
#include "itkMath.h"
#include "itkObject.h"

namespace itk
{
/**
 * Assignment Operator
 */
template< typename T, unsigned int TPointDimension >
Point< T, TPointDimension > &
Point< T, TPointDimension >
::operator=(const Self & r)
{
  BaseArray::operator=(r);
  return *this;
}

/**
 * Assignment from a plain array
 */
template< typename T, unsigned int TPointDimension >
Point< T, TPointDimension > &
Point< T, TPointDimension >
::operator=(const ValueType r[TPointDimension])
{
  BaseArray::operator=(r);
  return *this;
}

/**
 * In place increment by a vector
 */
template< typename T, unsigned int TPointDimension >
const Point< T, TPointDimension > &
Point< T, TPointDimension >
::operator+=(const VectorType & vec)
{
  for ( unsigned int i = 0; i < TPointDimension; i++ )
    {
    ( *this )[i] += vec[i];
    }
  return *this;
}

/**
 * In place subtract a vector
 */
template< typename T, unsigned int TPointDimension >
const Point< T, TPointDimension > &
Point< T, TPointDimension >
::operator-=(const VectorType & vec)
{
  for ( unsigned int i = 0; i < TPointDimension; i++ )
    {
    ( *this )[i] -= vec[i];
    }
  return *this;
}

/*
 * Add operator with a vector
 */
template< typename T, unsigned int TPointDimension >
Point< T, TPointDimension >
Point< T, TPointDimension >
::operator+(const VectorType & vec) const
{
  Self result;

  for ( unsigned int i = 0; i < TPointDimension; i++ )
    {
    result[i] = ( *this )[i] + vec[i];
    }
  return result;
}

/*
 * Subtract a vector, return a point
 */
template< typename T, unsigned int TPointDimension >
Point< T, TPointDimension >
Point< T, TPointDimension >
::operator-(const VectorType & vec)  const
{
  Self result;

  for ( unsigned int i = 0; i < TPointDimension; i++ )
    {
    result[i] = ( *this )[i] - vec[i];
    }
  return result;
}

/*
 * Difference between two points
 */
template< typename T, unsigned int TPointDimension >
Vector< T, TPointDimension >
Point< T, TPointDimension >
::operator-(const Self & pnt)  const
{
  VectorType result;

  for ( unsigned int i = 0; i < TPointDimension; i++ )
    {
    result[i] = ( *this )[i] - pnt[i];
    }
  return result;
}

/*
 * Return a vnl_vector_ref
 */
template< typename T, unsigned int TPointDimension >
vnl_vector_ref< T >
Point< T, TPointDimension >
::GetVnlVector(void)
{
  return vnl_vector_ref< T >( TPointDimension, this->GetDataPointer() );
}

/**
 * Return a vnl_vector const
 */
template< typename T, unsigned int TPointDimension >
vnl_vector< T >
Point< T, TPointDimension >
::GetVnlVector(void) const
{
  // Return a vector_ref<>.  This will be automatically converted to a
  // vnl_vector<>.  We have to use a const_cast<> which would normally
  // be prohibited in a const method, but it is safe to do here
  // because the cast to vnl_vector<> will ultimately copy the data.
  return vnl_vector_ref< T >( TPointDimension,
                              const_cast< T * >( this->GetDataPointer() ) );
}

template< typename T, unsigned int TPointDimension >
typename Point< T, TPointDimension >::VectorType
Point< T, TPointDimension >
::GetVectorFromOrigin() const
{
  // VectorType knows how to construct from ValueType*.
  return &( *this )[0];
}

/**
 * Set the point to the median point of the two arguments
 */
template< typename T, unsigned int TPointDimension >
void
Point< T, TPointDimension >
::SetToMidPoint(const Self & A, const Self & B)
{
  for ( unsigned int i = 0; i < TPointDimension; i++ )
    {
    ( *this )[i] = ( A[i] + B[i] ) / 2.0;
    }
}

/**
 * Set the point to the barycentric combination of two points
 */
template< typename T, unsigned int TPointDimension >
void
Point< T, TPointDimension >
::SetToBarycentricCombination(const Self & A,
                              const Self & B,
                              double alpha)
{
  const double wa = alpha;
  const double wb = 1.0 - alpha;

  for ( unsigned int i = 0; i < TPointDimension; i++ )
    {
    ( *this )[i] =  wa * A[i] + wb * B[i];
    }
}

/**
 * Set the point to the barycentric combination of three points
 */
template< typename T, unsigned int TPointDimension >
void
Point< T, TPointDimension >
::SetToBarycentricCombination(const Self & A,
                              const Self & B,
                              const Self & C,
                              double weightForA,
                              double weightForB)
{
  const double weightForC = 1.0 - weightForA - weightForB;

  for ( unsigned int i = 0; i < TPointDimension; i++ )
    {
    ( *this )[i] =  weightForA * A[i] + weightForB * B[i] + weightForC * C[i];
    }
}

/**
 * Set the point to the barycentric combination of N points
 */
template< typename T, unsigned int TPointDimension >
void
Point< T, TPointDimension >
::SetToBarycentricCombination(const Self *P,
                              const double *weights, unsigned int N)
{
  this->Fill(NumericTraits< T >::ZeroValue()); // put this point to null
  double weightSum = 0.0;
  for ( unsigned int j = 0; j < N - 1; j++ )
    {
    const double weight = weights[j];
    weightSum += weight;
    for ( unsigned int i = 0; i < TPointDimension; i++ )
      {
      ( *this )[i] += weight * P[j][i];
      }
    }
  const double weight = ( 1.0 - weightSum );
  for ( unsigned int i = 0; i < TPointDimension; i++ )
    {
    ( *this )[i] += weight * P[N - 1][i];
    }
}

/**
 * Set the point to the barycentric combination of N points in a Container
 */
template< typename TPointContainer, typename TWeightContainer >
typename BarycentricCombination< TPointContainer, TWeightContainer >
::PointType
BarycentricCombination< TPointContainer, TWeightContainer >
::Evaluate(
  const PointContainerPointer & points,
  const WeightContainerType & weights)
{
  typedef typename PointType::ValueType     ValueType;
  PointType barycentre;
  barycentre.Fill(NumericTraits< ValueType >::ZeroValue());   // set to null

  typename TPointContainer::Iterator point = points->Begin();
  typename TPointContainer::Iterator final = points->End();
  typename TPointContainer::Iterator last  = final;
  --last; // move to the (N)th point

  double weightSum = 0.0;

  const unsigned int PointDimension = PointType::PointDimension;
  unsigned int       j = 0;

  while ( point != last )
    {
    const double weight = weights[j++];
    weightSum += weight;
    for ( unsigned int i = 0; i < PointDimension; i++ )
      {
      barycentre[i] += weight * ( point->Value() )[i];
      }
    ++point;
    }

  // Compute directly the last one
  // to make sure that the weights sum 1
  const double weight = ( 1.0 - weightSum );
  for ( unsigned int i = 0; i < PointDimension; i++ )
    {
    barycentre[i] += weight * ( last->Value() )[i];
    }

  return barycentre;
}

/**
 * Print content to an ostream
 */
template< typename T, unsigned int TPointDimension >
std::ostream &
operator<<(std::ostream & os, const Point< T, TPointDimension > & vct)
{
  os << "[";
  if ( TPointDimension == 1 )
    {
    os << vct[0];
    }
  else
    {
    for ( unsigned int i = 0; i + 1 < TPointDimension; i++ )
      {
      os <<  vct[i] << ", ";
      }
    os <<  vct[TPointDimension - 1];
    }
  os << "]";
  return os;
}

/**
 * Read content from an istream
 */
template< typename T, unsigned int TPointDimension >
std::istream &
operator>>(std::istream & is, Point< T, TPointDimension > & vct)
{
  for ( unsigned int i = 0; i < TPointDimension; i++ )
    {
    is >>  vct[i];
    }
  return is;
}

#if !defined(ITK_LEGACY_REMOVE)
/**
 * Return a vnl_vector_ref
 */
template< typename T, unsigned int TPointDimension >
vnl_vector_ref< T >
Point< T, TPointDimension >
::Get_vnl_vector(void)
{
  return vnl_vector_ref< T >( TPointDimension, this->GetDataPointer() );
}

/**
 * Return a vnl_vector const
 */
template< typename T, unsigned int TPointDimension >
vnl_vector< T >
Point< T, TPointDimension >
::Get_vnl_vector(void) const
{
  // Return a vector_ref<>.  This will be automatically converted to a
  // vnl_vector<>.  We have to use a const_cast<> which would normally
  // be prohibited in a const method, but it is safe to do here
  // because the cast to vnl_vector<> will ultimately copy the data.
  return vnl_vector_ref< T >( TPointDimension,
                              const_cast< T * >( this->GetDataPointer() ) );
}
#endif

} // end namespace itk

#endif
