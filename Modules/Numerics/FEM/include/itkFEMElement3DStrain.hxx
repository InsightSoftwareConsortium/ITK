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

#ifndef itkFEMElement3DStrain_hxx
#define itkFEMElement3DStrain_hxx

#include "itkFEMElement3DStrain.h"

namespace itk
{
namespace fem
{
template <typename TBaseClass>
Element3DStrain<TBaseClass>
::Element3DStrain() : Superclass(), m_mat(ITK_NULLPTR)
{
}

// ////////////////////////////////////////////////////////////////////////
/**
 * Methods related to the physics of the problem.
 */

template <typename TBaseClass>
void Element3DStrain<TBaseClass>
::GetStrainDisplacementMatrix(MatrixType & B, const MatrixType & shapeDgl) const
{
  unsigned int p;
  unsigned int Nn = 3 * this->GetNumberOfNodes();

  B.set_size(6, Nn);

  // Initialize the B matrix to zero - subsequently, only the nonzero
  // terms will be filled in
  B.fill(0.0);

  // Copy the shape function derivatives wrt global coordinates
  // in right position in B matrix.
  for( unsigned int i = 0; i < Nn; i++ )
    {
    p = i / 3;

    switch( i % 3 )
      {
      // case 0:  /** Columns 1, 4, 7, ..., 22 */
      //   B[0][i] = shapeDgl[0][p];
      //   B[3][i] = shapeDgl[1][p];
      //   B[5][i] = shapeDgl[2][p];
      //   break;

      // case 1:  /** Columns 2, 5, 8, ..., 23 */
      //   B[1][i] = shapeDgl[1][p];
      //   B[3][i] = shapeDgl[0][p];
      //   B[4][i] = shapeDgl[2][p];
      //   break;

      // case 2:  /** Columns 3, 6, 9, ..., 24 */
      //   B[2][i] = shapeDgl[2][p];
      //   B[4][i] = shapeDgl[1][p];
      //   B[5][i] = shapeDgl[0][p];
      //   break;

      case 0:  /** Columns 1, 4, 7, ..., 22 */
        B[0][i] = shapeDgl[0][p];
        B[4][i] = shapeDgl[2][p];
        B[5][i] = shapeDgl[1][p];
        break;

      case 1:  /** Columns 2, 5, 8, ..., 23 */
        B[1][i] = shapeDgl[1][p];
        B[3][i] = shapeDgl[2][p];
        B[5][i] = shapeDgl[0][p];
        break;

      case 2:  /** Columns 3, 6, 9, ..., 24 */
        B[2][i] = shapeDgl[2][p];
        B[3][i] = shapeDgl[1][p];
        B[4][i] = shapeDgl[0][p];
        break;
      }
    }
}

template <typename TBaseClass>
void
Element3DStrain<TBaseClass>
::GetMaterialMatrix(MatrixType & D) const
{
  D.set_size(6, 6);
  D.fill(0.0);

  /* Material properties matrix */
  Float fac = ( m_mat->GetThickness() * m_mat->GetYoungsModulus() )
    / ( ( 1 + m_mat->GetPoissonsRatio() ) * ( 1 - 2 * m_mat->GetPoissonsRatio() ) );
  /** Set the elements in the top left quadrant */
  for( int j = 0; j < 3; j++ )
    {
    for( int k = 0; k < 3; k++ )
      {
      D[j][k] = m_mat->GetPoissonsRatio();
      }
    }
  /** Set the diagonal elements */
  for( int k = 0; k < 3; k++ )
    {
    D[k][k] = 1 - m_mat->GetPoissonsRatio();
    }
  for( int k = 3; k < 6; k++ )
    {
    D[k][k] = ( 1 - ( 2 * m_mat->GetPoissonsRatio() ) ) * 0.5;
    }

  /** Multiply by the factor */
  D = D * fac;
}

template <typename TBaseClass>
void
Element3DStrain<TBaseClass>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Materials: " << this->m_mat << std::endl;
}

}
}  // end namespace itk::fem

#endif // #ifndef itkFEMElement3DStrain_hxx
