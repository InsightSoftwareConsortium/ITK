/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCovariantVector.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/

#include "itkCovariantVector.h" 
#include <math.h>

namespace itk
{


template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension>&
CovariantVector<T, TCovariantVectorDimension>
::operator= (const Self& r)
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension>&
CovariantVector<T, TCovariantVectorDimension>
::operator= (const typename BaseArray::Reference& r)
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension>&
CovariantVector<T, TCovariantVectorDimension>
::operator= (const typename BaseArray::ConstReference& r)
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension>&
CovariantVector<T, TCovariantVectorDimension>
::operator= (const ValueType r[CovariantVectorDimension])
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TCovariantVectorDimension>
typename CovariantVector<T, TCovariantVectorDimension>::ArrayCommaListCopier
CovariantVector<T, TCovariantVectorDimension>
::operator= (const ValueType& r)
{
  return BaseArray::operator=(r);
}


/**
 *
 */
template<class T, unsigned int TCovariantVectorDimension>
const CovariantVector<T, TCovariantVectorDimension> &
CovariantVector<T, TCovariantVectorDimension>
::operator*=( const ValueType & value )
{
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    (*this)[i] *= value;
  }
  return *this;
}

  
/**
 *
 */
template<class T, unsigned int TCovariantVectorDimension>
const CovariantVector<T, TCovariantVectorDimension> &
CovariantVector<T, TCovariantVectorDimension>
::operator/=( const ValueType & value )
{
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    (*this)[i] /= value;
  }
  return *this;
}


/**
 *
 */
template<class T, unsigned int TCovariantVectorDimension>
const CovariantVector<T, TCovariantVectorDimension> &
CovariantVector<T, TCovariantVectorDimension>
::operator+=( const CovariantVector<T, TCovariantVectorDimension> & vec )
{
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    (*this)[i] += vec[i];
  }
  return *this;
}

 
/**
 *
 */
template<class T, unsigned int TCovariantVectorDimension>
const CovariantVector<T, TCovariantVectorDimension> &
CovariantVector<T, TCovariantVectorDimension>
::operator-=( const CovariantVector<T, TCovariantVectorDimension> & vec )
{
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    (*this)[i] -= vec[i];
  }
  return *this;
}

 
/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension> 
CovariantVector<T, TCovariantVectorDimension>
::operator-() const
{
  Self result;
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    result[i] = -(*this)[i];
  }
  return result;
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension> 
CovariantVector<T, TCovariantVectorDimension>
::operator+( const CovariantVector<T, TCovariantVectorDimension> & vec ) const
{
  Self result;
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    result[i] = (*this)[i] + vec[i];
  }
  return result;
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension> 
CovariantVector<T, TCovariantVectorDimension>
::operator-( const CovariantVector<T, TCovariantVectorDimension> & vec )  const
{
  Self result;
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    result[i] = (*this)[i] - vec[i];
  }
  return result;
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension> 
CovariantVector<T, TCovariantVectorDimension>
::operator*( const ValueType & value ) const
{
  Self result;
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    result[i] = (*this)[i] * value;
  }
  return result;
}


/**
 * Returns vector's Squared Euclidean Norm
 */
template<class T, unsigned int TCovariantVectorDimension>
T
CovariantVector<T, TCovariantVectorDimension>
::GetSquaredNorm( void ) const
{
  T sum = 0;  // consider a trait for null here ?
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    const T value = (*this)[i];
    sum += value * value;
  }
  return sum;
}



/**
 * Returns vector's Euclidean Norm
 */
template<class T, unsigned int TCovariantVectorDimension>
T
CovariantVector<T, TCovariantVectorDimension>
::GetNorm( void ) const
{
  return sqrt( GetSquaredNorm() ); 
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension> 
CovariantVector<T, TCovariantVectorDimension>
::operator/( const ValueType & value ) const
{
  Self result;
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    result[i] = (*this)[i] / value;
  }
  return result;
}



/**
 * Return a vnl_vector_ref
 */
template<class T, unsigned int TCovariantVectorDimension>
vnl_vector_ref< T >
CovariantVector<T, TCovariantVectorDimension>
::Get_vnl_vector( void ) 
{
  vnl_vector_ref< T > vector_ref( TCovariantVectorDimension, ( T * )(*this) );
  return vector_ref;
}
 

} // end namespace itk

