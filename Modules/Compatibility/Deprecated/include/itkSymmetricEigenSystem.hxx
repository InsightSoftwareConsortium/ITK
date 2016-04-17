/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkSymmetricEigenSystem_hxx
#define itkSymmetricEigenSystem_hxx

#include "itkSymmetricEigenSystem.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TMatrixElement, int VNumberOfRows >
SymmetricEigenSystem< TMatrixElement, VNumberOfRows >
::SymmetricEigenSystem()
{
  m_Matrix = ITK_NULLPTR;
  m_UseAbsoluteOrder = true;
  m_EigenValues.Fill(NumericTraits< TMatrixElement >::ZeroValue());
  ArrayType temp;
  temp.Fill(NumericTraits< TMatrixElement >::ZeroValue());
  m_EigenVectors.Fill(temp);
}

/**
 * Destructor
 */
template< typename TMatrixElement, int VNumberOfRows >
SymmetricEigenSystem< TMatrixElement, VNumberOfRows >
::~SymmetricEigenSystem()
{}

template< typename TMatrixElement, int VNumberOfRows >
void
SymmetricEigenSystem< TMatrixElement, VNumberOfRows >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Matrix:";

  if ( m_Matrix != ITK_NULLPTR )
    {
    os << m_Matrix << std::endl;
    }
  else
    {
    os << "not set." << std::endl;
    }

  os << indent << "Eigen Vectors  " << m_EigenVectors << std::endl;
  os << indent << "Eigen Values   " << m_EigenValues << std::endl;
  os << indent << "Absolute order " << m_UseAbsoluteOrder << std::endl;
}

/**
 * Compute the eigen values and vectors
 */
template< typename TMatrixElement, int VNumberOfRows >
void
SymmetricEigenSystem< TMatrixElement, VNumberOfRows >
::GenerateData(void)
{
  int i, j, k;

  InternalEigenSystemType internalEigenSystem( m_Matrix->GetVnlMatrix() );

  typedef vnl_vector< TMatrixElement > EigenVectorType;
  EigenVectorType tempVector;

  for ( i = 0; i < VNumberOfRows; i++ )
    {
    tempVector = internalEigenSystem.get_eigenvector(i);
    m_EigenValues[i] = internalEigenSystem.get_eigenvalue(i);
    for ( j = 0; j < VNumberOfRows; j++ )
      {
      m_EigenVectors[i][j] = tempVector[j];
      }
    }

  double temp;
  for ( i = 0; i < ( VNumberOfRows - 1 ); i++ )
    {
    for ( j = i + 1; j < VNumberOfRows; j++ )
      {
      if ( ( m_EigenValues[j] > m_EigenValues[i] && !m_UseAbsoluteOrder )
           || ( ( itk::Math::abs(m_EigenValues[j]) > itk::Math::abs(m_EigenValues[i]) )
                && m_UseAbsoluteOrder ) )
        {
        temp = m_EigenValues[i];
        m_EigenValues[i] = m_EigenValues[j];
        m_EigenValues[j] = temp;
        for ( k = 0; k < VNumberOfRows; k++ )
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
