/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSymmetricEigenSystem.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSymmetricEigenSystem_txx
#define _itkSymmetricEigenSystem_txx

#include "itkSymmetricEigenSystem.h"

namespace itk
{

/**
 * Constructor
 */
template< class TMatrixElement, int VNumberOfRows >
SymmetricEigenSystem< TMatrixElement, VNumberOfRows >
::SymmetricEigenSystem()
{
  m_Matrix = 0 ;
  m_UseAbsoluteOrder = true ;
}

/**
 * Destructor
 */
template< class TMatrixElement, int VNumberOfRows >
SymmetricEigenSystem< TMatrixElement, VNumberOfRows >
::~SymmetricEigenSystem()
{
}

template< class TMatrixElement, int VNumberOfRows >
void
SymmetricEigenSystem< TMatrixElement, VNumberOfRows >
::PrintSelf(std::ostream& os, Indent indent) const 
{
  Superclass::PrintSelf(os, indent) ;

  os << indent << "Matrix         " << m_Matrix << std::endl ;
  os << indent << "Eigen Vectors  " << m_EigenVectors << std::endl ;
  os << indent << "Eigen Values   " << m_EigenValues << std::endl ;
  os << indent << "Absolute order " << m_UseAbsoluteOrder << std::endl ;
}


/**
 * Compute the eigen values and vectors
 */
template< class TMatrixElement, int VNumberOfRows >
void
SymmetricEigenSystem< TMatrixElement, VNumberOfRows >
::GenerateData( void )
{
  int i, j, k ;

  InternalEigenSystemType internalEigenSystem(m_Matrix->GetVnlMatrix()) ;

  vnl_vector< double > tempVector ;

  for (i = 0 ; i < VNumberOfRows ; i++)
    {
    tempVector = internalEigenSystem.get_eigenvector(i) ;
    m_EigenValues[i] = internalEigenSystem.get_eigenvalue(i) ;
    for (j = 0 ; j < VNumberOfRows ; j++)
      {
      m_EigenVectors[i][j] = tempVector[j] ;
      }
    }

  double temp ;
  for(i = 0 ; i < (VNumberOfRows - 1) ; i++)
    {
    for(j = i + 1 ; j < VNumberOfRows ; j++)
      {
      if( (m_EigenValues[j] > m_EigenValues[i] && !m_UseAbsoluteOrder) ||
          ( (vnl_math_abs( m_EigenValues[j] ) > vnl_math_abs( m_EigenValues[i] ) ) 
            && m_UseAbsoluteOrder) )
        {
        temp= m_EigenValues[i] ;
        m_EigenValues[i] = m_EigenValues[j] ;
        m_EigenValues[j] = temp;
        for ( k = 0 ; k < VNumberOfRows ; k++ )
          {
          temp = m_EigenVectors[i][k];
          m_EigenVectors[i][k] = m_EigenVectors[j][k];
          m_EigenVectors[j][k] = temp;
          }
        }
      }
    }
}

} // end namespace itk

#endif
