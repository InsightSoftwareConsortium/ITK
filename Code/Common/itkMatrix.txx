/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMatrix.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/

#include "itkMatrix.h" 
#include "itkNumericTraits.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include "vnl/vnl_transpose.h"

namespace itk
{



 
/**
 *  Product by a Vector
 */
template<class T, unsigned int NRows, unsigned int NColumns >
Vector<T, NRows> 
Matrix<T, NRows, NColumns>
::operator*( const Vector<T, NColumns> & vect ) const
{
  Vector<T,NRows> result;
  for( unsigned int r=0; r<NRows; r++) 
  {
    T sum = NumericTraits<T>::Zero;   
    for( unsigned int c=0; c<NColumns; c++ ) 
    {
       sum += (*this)(r,c) * vect[c];
    }
    result[r] = sum;
  }
  return result;
}



 
/**
 *  Product by a Point
 */
template<class T, unsigned int NRows, unsigned int NColumns >
Point<T, NRows> 
Matrix<T, NRows, NColumns>
::operator*( const Point<T, NColumns> & pnt ) const
{
  Point<T,NRows> result;
  for( unsigned int r=0; r<NRows; r++) 
  {
    T sum = NumericTraits<T>::Zero;   
    for( unsigned int c=0; c<NColumns; c++ ) 
    {
       sum += (*this)(r,c) * pnt[c];
    }
    result[r] = sum;
  }
  return result;
}



 
/**
 *  Product by a CovariantVector
 */
template<class T, unsigned int NRows, unsigned int NColumns >
CovariantVector<T, NRows> 
Matrix<T, NRows, NColumns>
::operator*( const CovariantVector<T, NColumns> & covect ) const
{
  CovariantVector<T,NRows> result;
  for( unsigned int r=0; r<NRows; r++) 
  {
    T sum = NumericTraits<T>::Zero;   
    for( unsigned int c=0; c<NColumns; c++ ) 
    {
       sum += (*this)(r,c) * covect[c];
    }
    result[r] = sum;
  }
  return result;
}


 
/**
 *  Product by a Matrix
 */
template<class T, unsigned int NRows, unsigned int NColumns >
vnl_matrix<T> 
Matrix<T, NRows, NColumns>
::operator*( const vnl_matrix<T> & matrix ) const
{
  return vnl_matrix<T>::operator*( matrix );
}

 
/**
 *  Assignment
 */
template<class T, unsigned int NRows, unsigned int NColumns >
const Matrix<T, NRows, NColumns> &
Matrix<T, NRows, NColumns>
::operator=( const vnl_matrix<T> & matrix )
{
  vnl_matrix_fixed<T,NRows,NColumns>::operator=( matrix );
  return *this;
}


/**
 *  Returns the inverse matrix
 */
template<class T, unsigned int NRows, unsigned int NColumns >
vnl_matrix<T> 
Matrix<T, NRows, NColumns>
::GetInverse( void ) const
{
  return vnl_matrix_inverse<T>( *this );
}

/**
 *  Returns the transposed matrix
 */
template<class T, unsigned int NRows, unsigned int NColumns >
vnl_matrix<T> 
Matrix<T, NRows, NColumns>
::GetTranspose( void ) const
{
  return vnl_transpose( *this );
}

 

} // end namespace itk

