/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMatrix.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
 * \ingroup DataRepresentation
 */

template<class T, unsigned int NRows=3, unsigned int NColumns=3>
class Matrix {
public:
  /** Standard class typedefs. */
  typedef Matrix  Self;

  /** Internal matrix type */
  typedef vnl_matrix_fixed<T,NRows,NColumns>  InternalMatrixType;

  /** Matrix by Vector multiplication.  */
  Vector<T,NRows> operator*(const Vector<T,NColumns> & vector) const;
 
  /** Matrix by Point multiplication.  */
  Point<T,NRows> operator*(const Point<T,NColumns> & vector) const;
 
  /** Matrix by CovariantVector multiplication.  */
  CovariantVector<T,NRows> 
  operator*(const CovariantVector<T,NColumns> & vector) const;
  
  /** Matrix by Matrix multiplication.  */
  Self operator*(const Self & matrix) const;
 
  /** Matrix by vnl_matrix multiplication.  */
  vnl_matrix<T> operator*(const vnl_matrix<T> & matrix) const;

  /** Matrix by Matrix multiplication.  */
  void operator*=(const Self & matrix);
 
  /** Matrix by vnl_matrix multiplication.  */
  void operator*=(const vnl_matrix<T> & matrix);

  /** Matrix by vnl_vector multiplication.  */
  vnl_vector<T> operator*(const vnl_vector<T> & matrix) const;

  /** Matrix by scalar multiplication.  */
  void operator*=(const T & value)
    { m_Matrix *= value; }

  /** Matrix by scalar multiplication.  */
  Self operator*(const T & value)
    { Self result( *this );
      result *= value;
      return result; }

  /** Return a row of the matrix. */
  inline T * operator[]( unsigned int i )
    { return m_Matrix[i]; }

  /** Return a row of the matrix.*/
  inline const T * operator[]( unsigned int i ) const
    { return m_Matrix[i]; }

  /** Return the matrix. */
  inline InternalMatrixType & GetVnlMatrix( void )
    { return m_Matrix; }

  /** Return the matrix. */
  inline const InternalMatrixType & GetVnlMatrix( void ) const
    { return m_Matrix; }

  /** Set the matrix to identity. */
  inline void SetIdentity( void ) 
    { m_Matrix.set_identity(); }

  /** Fill the matrix with a value. */
  inline void Fill( const T & value ) 
    { m_Matrix.fill( value ); }

  /** Assignment operator. */
  inline const Self & operator=( const vnl_matrix<T> & matrix);

  /** Assignment operator. */
  inline const Self & operator=( const Self & matrix);

  /** Return the inverse matrix. */
  inline vnl_matrix<T> GetInverse( void ) const;
 
  /** Return the transposed matrix. */
  inline vnl_matrix<T> GetTranspose( void ) const;

  /** Default constructor. */
  Matrix() : m_Matrix(NumericTraits<T>::Zero) {};

  /** Copy constructor. */
  Matrix(const Self & matrix) : m_Matrix( matrix.m_Matrix ) {};
 
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
