/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMatrix.h
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
#ifndef __itkMatrix_h
#define __itkMatrix_h


#include "itkPoint.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_matrix_fixed.h"


namespace itk
{

/** \class Matrix
 * \brief A templated class holding a M x N size Matrix
 * This class contains a vnl_matrix_fixed in order 
 * to make all the vnl mathematical methods available.
 * 
 */

template<class T, unsigned int NRows=3, unsigned int NColumns=3>
class Matrix {
 public:
  /**
   * Standard "Self" typedef.
   */
  typedef Matrix  Self;

  /**
   * Internal matrix type
   */
  typedef vnl_matrix_fixed<T,NRows,NColumns>  InternalMatrixType;

  /**
   * Matrix by Vector multiplication. 
   */
  Vector<T,NRows> operator*(const Vector<T,NColumns> & vector) const;
 
  /**
   * Matrix by Point multiplication. 
   */
  Point<T,NRows> operator*(const Point<T,NColumns> & vector) const;
 
  /**
   * Matrix by CovariantVector multiplication. 
   */
  CovariantVector<T,NRows> operator*(
                 const CovariantVector<T,NColumns> & vector) const;
  
  /**
   * Matrix by Matrix multiplication. 
   */
  Self operator*(const Self & matrix) const;
 
  /**
   * Matrix by vnl_matrix multiplication. 
   */
  vnl_matrix<T> operator*(const vnl_matrix<T> & matrix) const;

  /**
   * Matrix by Matrix multiplication. 
   */
  void operator*=(const Self & matrix);
 
  /**
   * Matrix by vnl_matrix multiplication. 
   */
  void operator*=(const vnl_matrix<T> & matrix);


  /**
   * Matrix by vnl_vector multiplication. 
   */
  vnl_vector<T> operator*(const vnl_vector<T> & matrix) const;


  /**
   * Matrix by scalar multiplication. 
   */
  void operator*=(const T & value)
                    { m_Matrix * value; }
 

  /**
   * Return a row
   */
  inline T * operator[]( unsigned int i )
    { return m_Matrix[i]; }


  /**
   * Return a row
   */
  inline const T * operator[]( unsigned int i ) const
    { return m_Matrix[i]; }


  /**
   * Return the matrix
   */
  inline InternalMatrixType & GetVnlMatrix( void )
    { return m_Matrix; }


  /**
   * Return the matrix
   */
  inline const InternalMatrixType & GetVnlMatrix( void ) const
    { return m_Matrix; }


  /**
   * Set the matrix to Identity
   */
  inline void SetIdentity( void ) 
    { m_Matrix.set_identity(); }

  /**
   * Fill the matrix with a value
   */
  inline void Fill( const T & value ) 
    { m_Matrix.fill( value ); }


  /**
   * Assignment Operator
   */
  inline const Self & operator=( const vnl_matrix<T> & matrix);


  /**
   * Assignment Operator
   */
  inline const Self & operator=( const Self & matrix);


  /**
   * Return the inverse matrix
   */
  inline vnl_matrix<T> GetInverse( void ) const;
 
  /**
   * Return the transposed matrix
   */
  inline vnl_matrix<T> GetTranspose( void ) const;
 
private:

  InternalMatrixType     m_Matrix;

};

template< class T, unsigned int NRows, unsigned int NColumns >  
ITK_EXPORT std::ostream& operator<<(std::ostream& os, 
                                    const Matrix<T,NRows,NColumns> & v) 
                            { os << v.GetVnlMatrix(); return os; }


  
} // end namespace itk
  

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMatrix.txx"
#endif


#endif 
