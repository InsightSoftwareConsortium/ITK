/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMatrix.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMatrix_txx
#define _itkMatrix_txx

#include "itkMatrix.h" 
#include "itkNumericTraits.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include "vnl/vnl_transpose.h"
#include "vnl/vnl_matrix.h"

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
       sum += m_Matrix(r,c) * vect[c];
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
       sum += m_Matrix(r,c) * pnt[c];
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
       sum += m_Matrix(r,c) * covect[c];
    }
    result[r] = sum;
  }
  return result;
}

 
 
/**
 *  Product by a matrix
 */
template<class T, unsigned int NRows, unsigned int NColumns >
Matrix<T, NRows, NColumns>
Matrix<T, NRows, NColumns>
::operator*( const Self & matrix ) const
{
  Self result;
  result = m_Matrix * matrix.m_Matrix;
  return result;
}

 
/**
 *  Product by a vnl_matrix
 */
template<class T, unsigned int NRows, unsigned int NColumns >
vnl_matrix<T>
Matrix<T, NRows, NColumns>
::operator*( const vnl_matrix<T> & matrix ) const
{
  return m_Matrix * matrix;
}


 
/**
 *  Product by a matrix
 */
template<class T, unsigned int NRows, unsigned int NColumns >
void
Matrix<T, NRows, NColumns>
::operator*=( const Self & matrix ) 
{
  m_Matrix *= matrix.m_Matrix;
}

 
/**
 *  Product by a vnl_matrix
 */
template<class T, unsigned int NRows, unsigned int NColumns >
void
Matrix<T, NRows, NColumns>
::operator*=( const vnl_matrix<T> & matrix ) 
{
  m_Matrix *= matrix;
}



/**
 *  Product by a vnl_vector
 */
template<class T, unsigned int NRows, unsigned int NColumns >
vnl_vector<T>
Matrix<T, NRows, NColumns>
::operator*( const vnl_vector<T> & vc ) const
{
  return m_Matrix * vc;
}



/**
 *  Assignment
 */
template<class T, unsigned int NRows, unsigned int NColumns >
const Matrix<T, NRows, NColumns> &
Matrix<T, NRows, NColumns>
::operator=( const Self  & matrix )
{
  m_Matrix = matrix.m_Matrix;
  return *this;
}



 
/**
 *  Assignment
 */
template<class T, unsigned int NRows, unsigned int NColumns >
const Matrix<T, NRows, NColumns> &
Matrix<T, NRows, NColumns>
::operator=( const vnl_matrix<T> & matrix )
{
  m_Matrix = matrix;
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
  return vnl_matrix_inverse<T>( m_Matrix );
}


/**
 *  Returns the transposed matrix
 */
template<class T, unsigned int NRows, unsigned int NColumns >
vnl_matrix<T> 
Matrix<T, NRows, NColumns>
::GetTranspose( void ) const
{
  return m_Matrix.transpose();
}

 

} // end namespace itk


#endif
