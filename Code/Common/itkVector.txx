/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVector.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#include "itkVector.h" 



namespace itk
{




/**
 *
 */
template<class T, unsigned int TVectorDimension>
const Vector<T, TVectorDimension> &
Vector<T, TVectorDimension>
::operator*=( const ValueType & value )
{
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    (*this)[i] *= value;
  }
  return *this;
}

  
/**
 *
 */
template<class T, unsigned int TVectorDimension>
const Vector<T, TVectorDimension> &
Vector<T, TVectorDimension>
::operator/=( const ValueType & value )
{
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    (*this)[i] /= value;
  }
  return *this;
}


/**
 *
 */
template<class T, unsigned int TVectorDimension>
const Vector<T, TVectorDimension> &
Vector<T, TVectorDimension>
::operator+=( const Vector<T, TVectorDimension> & vector )
{
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    (*this)[i] += vector[i];
  }
  return *this;
}

 
/**
 *
 */
template<class T, unsigned int TVectorDimension>
const Vector<T, TVectorDimension> &
Vector<T, TVectorDimension>
::operator-=( const Vector<T, TVectorDimension> & vector )
{
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    (*this)[i] -= vector[i];
  }
  return *this;
}

 
/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension> 
Vector<T, TVectorDimension>
::operator-() const
{
  Self result;
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    result[i] = -(*this)[i];
  }
  return result;
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension> 
Vector<T, TVectorDimension>
::operator+( const Vector<T, TVectorDimension> & vector ) const
{
  Self result;
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    result[i] = (*this)[i] + vector[i];
  }
  return result;
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension> 
Vector<T, TVectorDimension>
::operator-( const Vector<T, TVectorDimension> & vector )  const
{
  Self result;
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    result[i] = (*this)[i] - vector[i];
  }
  return result;
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension> 
Vector<T, TVectorDimension>
::operator*( const ValueType & value ) const
{
  Self result;
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    result[i] = (*this)[i] * value;
  }
  return result;
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension> 
Vector<T, TVectorDimension>
::operator/( const ValueType & value ) const
{
  Self result;
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    result[i] = (*this)[i] / value;
  }
  return result;
}


} // end namespace itk

