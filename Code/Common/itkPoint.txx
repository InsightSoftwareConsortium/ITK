/*=========================================================================


Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPoint.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPoint_txx
#define _itkPoint_txx
#include "itkPoint.h" 
#include <vnl/vnl_math.h>



namespace itk
{


/*
 * Assignment Operator
 */
template<class T, unsigned int TPointDimension>
Point<T, TPointDimension>&
Point<T, TPointDimension>
::operator= (const Self& r)
{
  BaseArray::operator=(r);
  return *this;
}


/*
 * Assignemt from a plain array
 */
template<class T, unsigned int TPointDimension>
Point<T, TPointDimension>&
Point<T, TPointDimension>
::operator= (const ValueType r[PointDimension])
{
  BaseArray::operator=(r);
  return *this;
}


/*
 * In place increment by a vector
 */
template<class T, unsigned int TPointDimension>
const Point<T, TPointDimension> &
Point<T, TPointDimension>
::operator+=( const VectorType & vec )
{
  for( unsigned int i=0; i<TPointDimension; i++) 
    {
    (*this)[i] += vec[i];
    }
  return *this;
}

 
/*
 * In place subtract a vector
 */
template<class T, unsigned int TPointDimension>
const Point<T, TPointDimension> &
Point<T, TPointDimension>
::operator-=( const VectorType & vec )
{
  for( unsigned int i=0; i<TPointDimension; i++) 
    {
    (*this)[i] -= vec[i];
    }
  return *this;
}

 

/*
 * Add operator with a vector
 */
template<class T, unsigned int TPointDimension>
Point<T, TPointDimension> 
Point<T, TPointDimension>
::operator+( const VectorType & vec ) const
{
  Self result;
  for( unsigned int i=0; i<TPointDimension; i++) 
    {
    result[i] = (*this)[i] + vec[i];
    }
  return result;
}



/*
 * Subtract a vector, return a point
 */
template<class T, unsigned int TPointDimension>
Point<T, TPointDimension> 
Point<T, TPointDimension>
::operator-( const VectorType & vec )  const
{
  Self result;
  for( unsigned int i=0; i<TPointDimension; i++) 
    {
    result[i] = (*this)[i] - vec[i];
    }
  return result;
}



/*
 * Difference between two points
 */
template<class T, unsigned int TPointDimension>
Vector<T, TPointDimension> 
Point<T, TPointDimension>
::operator-( const Self & pnt )  const
{
  VectorType result;
  for( unsigned int i=0; i<TPointDimension; i++) 
    {
    result[i] = (*this)[i] - pnt[i];
    }
  return result;
}


/*
 * Return a vnl_vector_ref
 */
template<class T, unsigned int TPointDimension >
vnl_vector_ref< T >
Point<T, TPointDimension>
::Get_vnl_vector( void ) 
{
  vnl_vector_ref< T > vector_ref( TPointDimension, this->GetDataPointer());
  return vector_ref;
}

template<class T, unsigned int TPointDimension >
typename Point<T, TPointDimension>::VectorType
Point<T, TPointDimension>
::GetVectorFromOrigin() const
{
  // VectorType knows how to construct from ValueType*.
  return &(*this)[0];
}


/*
 * Returns Squared Euclidean distance between two points
 */
template<class T, unsigned int TPointDimension>
typename Point<T, TPointDimension>::ValueType
Point<T, TPointDimension>
::SquaredEuclideanDistanceTo( const Self & pnt )  const
{
  ValueType sum = NumericTraits<ValueType>::Zero;
  for( unsigned int i=0; i<TPointDimension; i++) 
    {
    const ValueType difference = (*this)[i] - pnt[i];
    sum += difference * difference;
    }
  return sum;
}



/*
 * Returns Euclidean distance between two points
 */
template<class T, unsigned int TPointDimension>
typename Point<T, TPointDimension>::ValueType
Point<T, TPointDimension>
::EuclideanDistanceTo( const Self & pnt )  const
{
  const double distance = sqrt( 
    static_cast<double>( SquaredEuclideanDistanceTo( pnt ) ) ) ;
  return static_cast<ValueType>( distance );
}
 


/*
 * Set the point to the median point of the two arguments
 */
template<class T, unsigned int TPointDimension>
void
Point<T, TPointDimension>
::SetToMidPoint( const Self & A, const Self & B )  
{
  for( unsigned int i=0; i<TPointDimension; i++) 
    {
    (*this)[i] = ( A[i] + B[i] ) /2.0;
    }
}





/*
 * Set the point to the barycentric combination of two points
 */
template<class T, unsigned int TPointDimension>
void
Point<T, TPointDimension>
::SetToBarycentricCombination( const Self & A,
                               const Self & B,
                               double alpha )  
{
  const double wa = alpha;
  const double wb = 1.0 - alpha;
  for( unsigned int i=0; i<TPointDimension; i++) 
    {
    (*this)[i] =  wa * A[i] + wb * B[i];
    }
}



/*
 * Set the point to the barycentric combination of three points
 */
template<class T, unsigned int TPointDimension>
void
Point<T, TPointDimension>
::SetToBarycentricCombination( const Self & A,
                               const Self & B,
                               const Self & C,
                               double weightForA, 
                               double weightForB  )
{
  const double weightForC = 1.0 - weightForA - weightForB;
  for( unsigned int i=0; i<TPointDimension; i++) 
    {
    (*this)[i] =  weightForA * A[i] + weightForB * B[i] + weightForC * C[i];
    }
}



/*
 * Set the point to the barycentric combination of N points
 */
template<class T, unsigned int TPointDimension>
void
Point<T, TPointDimension>
::SetToBarycentricCombination( const Self * P,
                               const double * weights, unsigned int N )
{
  Fill( NumericTraits<T>::Zero ); // put this point to null
  double weightSum = 0.0;
  for( unsigned int j=0; j<N-1; j++) 
    {
    const double weight = weights[j];
    weightSum += weight;
    for( unsigned int i=0; i<TPointDimension; i++) 
      {
      (*this)[i] +=  weight * P[j][i];
      }
    }
  const double weight = ( 1.0 - weightSum );
  for( unsigned int i=0; i<TPointDimension; i++) 
    {
    (*this)[i] +=  weight * P[N-1][i];
    }

}




/*
 * Set the point to the barycentric combination of N points in a Container
 */
template< class TPointContainer, class TWeightContainer >
typename BarycentricCombination<TPointContainer,TWeightContainer>
::PointType 
BarycentricCombination<TPointContainer,TWeightContainer>
::Evaluate( 
  const PointContainerPointer & points, 
  const WeightContainerType & weights ) 
{
  
  typedef typename TPointContainer::Element   PointType;
  typedef typename PointType::ValueType       ValueType;
  PointType barycentre;
  barycentre.Fill( NumericTraits< ValueType >::Zero ); // set to null

  typename TPointContainer::Iterator point = points->Begin();
  typename TPointContainer::Iterator final = points->End();
  typename TPointContainer::Iterator last  = final;
  last--; // move to the (N)th point
  
  double weightSum = 0.0;

  const unsigned int PointDimension = PointType::PointDimension;
  unsigned int j = 0;

  while( point != last )
    {
    const double weight = weights[j++];
    weightSum += weight;
    for( unsigned int i=0; i<PointDimension; i++) 
      {
      barycentre[i] +=  weight * (point->Value())[i];
      }
    point++;
    }

  // Compute directly the last one
  // to make sure that the weights sum 1
  const double weight = ( 1.0 - weightSum );
  for( unsigned int i=0; i<PointDimension; i++) 
    {
    barycentre[i] +=  weight * (last->Value())[i];
    }

  return barycentre;

}





/*
 * Print content to an ostream
 */
template<class T, unsigned int TPointDimension>
std::ostream &
operator<<(std::ostream& os,const Point<T,TPointDimension> & vct ) 
{
  for( unsigned int i=0; i+1<TPointDimension; i++)
    {
    os <<  vct[i] << "  ";
    }
  os <<  vct[TPointDimension-1];
  return os;
}



/*
 * Read content from an istream
 */
template<class T, unsigned int TPointDimension>
std::istream &
operator>>(std::istream& is, Point<T,TPointDimension> & vct ) 
{
  for( unsigned int i=0; i<TPointDimension; i++)
    {
    is >>  vct[i];
    }
  return is;
}



} // end namespace itk


#endif
