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


template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension>&
Vector<T, TVectorDimension>
::operator= (const Self& r)
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension>&
Vector<T, TVectorDimension>
::operator= (const typename BaseArray::Reference& r)
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension>&
Vector<T, TVectorDimension>
::operator= (const typename BaseArray::ConstReference& r)
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension>&
Vector<T, TVectorDimension>
::operator= (const ValueType r[VectorDimension])
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TVectorDimension>
typename Vector<T, TVectorDimension>::ArrayCommaListCopier
Vector<T, TVectorDimension>
::operator= (const ValueType& r)
{
  return BaseArray::operator=(r);
}


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
::operator+=( const Vector<T, TVectorDimension> & vec )
{
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    (*this)[i] += vec[i];
  }
  return *this;
}

 
/**
 *
 */
template<class T, unsigned int TVectorDimension>
const Vector<T, TVectorDimension> &
Vector<T, TVectorDimension>
::operator-=( const Vector<T, TVectorDimension> & vec )
{
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    (*this)[i] -= vec[i];
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
::operator+( const Vector<T, TVectorDimension> & vec ) const
{
  Self result;
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    result[i] = (*this)[i] + vec[i];
  }
  return result;
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension> 
Vector<T, TVectorDimension>
::operator-( const Vector<T, TVectorDimension> & vec )  const
{
  Self result;
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    result[i] = (*this)[i] - vec[i];
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

