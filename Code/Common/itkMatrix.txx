/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMatrix.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkMatrix_txx
#define _itkMatrix_txx

#include "itkMatrix.h" 
#include "itkNumericTraits.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include "vnl/vnl_transpose.h"
#include "vnl/vnl_matrix.txx"

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
::operator*( const Matrix<T, NRows, NColumns> & matrix ) const
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
::operator*=( const Matrix<T, NRows, NColumns> & matrix ) 
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
::operator=( const Matrix<T, NRows, NColumns> & matrix )
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
