/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPoint.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#include "itkPoint.h" 



namespace itk
{


template<class T, unsigned int TPointDimension>
Point<T, TPointDimension>&
Point<T, TPointDimension>
::operator= (const Self& r)
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TPointDimension>
Point<T, TPointDimension>&
Point<T, TPointDimension>
::operator= (const typename BaseArray::Reference& r)
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TPointDimension>
Point<T, TPointDimension>&
Point<T, TPointDimension>
::operator= (const typename BaseArray::ConstReference& r)
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TPointDimension>
Point<T, TPointDimension>&
Point<T, TPointDimension>
::operator= (const ValueType r[Length])
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TPointDimension>
typename Point<T, TPointDimension>::ArrayCommaListCopier
Point<T, TPointDimension>
::operator= (const ValueType& r)
{
  return BaseArray::operator=(r);
}


/**
 *
 */
template<class T, unsigned int TPointDimension>
const Point<T, TPointDimension> &
Point<T, TPointDimension>
::operator+=( const Vector<T, TPointDimension> & vec )
{
  for( unsigned int i=0; i<TPointDimension; i++) 
  {
    (*this)[i] += vec[i];
  }
  return *this;
}

 
/**
 *
 */
template<class T, unsigned int TPointDimension>
const Point<T, TPointDimension> &
Point<T, TPointDimension>
::operator-=( const Vector<T, TPointDimension> & vec )
{
  for( unsigned int i=0; i<TPointDimension; i++) 
  {
    (*this)[i] -= vec[i];
  }
  return *this;
}

 

/**
 * Returns a temporary copy of a point
 */
template<class T, unsigned int TPointDimension>
Point<T, TPointDimension> 
Point<T, TPointDimension>
::operator+( const Vector<T, TPointDimension> & vec ) const
{
  Self result;
  for( unsigned int i=0; i<TPointDimension; i++) 
  {
    result[i] = (*this)[i] + vec[i];
  }
  return result;
}



/**
 * Returns a temporary copy of a point
 */
template<class T, unsigned int TPointDimension>
Point<T, TPointDimension> 
Point<T, TPointDimension>
::operator-( const Vector<T, TPointDimension> & vec )  const
{
  Self result;
  for( unsigned int i=0; i<TPointDimension; i++) 
  {
    result[i] = (*this)[i] - vec[i];
  }
  return result;
}



/**
 * Difference between two points
 */
template<class T, unsigned int TPointDimension>
Vector<T, TPointDimension> 
Point<T, TPointDimension>
::operator-( const Point<T, TPointDimension> & pnt )  const
{
  VectorType result;
  for( unsigned int i=0; i<TPointDimension; i++) 
  {
    result[i] = (*this)[i] - pnt[i];
  }
  return result;
}



 
} // end namespace itk

