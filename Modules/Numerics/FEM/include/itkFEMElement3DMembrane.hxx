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

#ifndef itkFEMElement3DMembrane_hxx
#define itkFEMElement3DMembrane_hxx

#include "itkFEMElement3DMembrane.h"

namespace itk
{
namespace fem
{
template <typename TBaseClass>
Element3DMembrane<TBaseClass>
::Element3DMembrane() : Superclass(), m_mat(ITK_NULLPTR)
{
}

// ////////////////////////////////////////////////////////////////////////
/**
 * Methods related to the physics of the problem.
 */

template <typename TBaseClass>
void
Element3DMembrane<TBaseClass>
::GetStrainDisplacementMatrix(MatrixType & B, const MatrixType & shapeDgl) const
{
  unsigned int p;
  unsigned int Nn = this->GetNumberOfNodes();

  B.set_size(9, 3 * Nn); //  note minor difference from 2D membrane
  // Copy the shape function derivatives to the B matrix.
  for( unsigned int i = 0; i < Nn; i++ )
    {
    // Compute B index
    p = i * 3;

    // Compute B elements
    // below are the dN/dx entries
    B[0][p]   = shapeDgl[0][i];
    B[0][p + 1] = 0.0;
    B[0][p + 2] = 0.0;

    B[1][p]   = 0.0;
    B[1][p + 1] = shapeDgl[0][i];
    B[1][p + 2] = 0.0;

    B[2][p]   = 0.0;
    B[2][p + 1] = 0.0;
    B[2][p + 2] = shapeDgl[0][i];

    // below are the dN/dy entries
    B[3][p]   = shapeDgl[1][i];
    B[3][p + 1] = 0.0;
    B[3][p + 2] = 0.0;

    B[4][p]   = 0.0;
    B[4][p + 1] = shapeDgl[1][i];
    B[4][p + 2] = 0.0;

    B[5][p]   = 0.0;
    B[5][p + 1] = 0.0;
    B[5][p + 2] = shapeDgl[1][i];

    // below are the dN/dz entries
    B[6][p]   = shapeDgl[2][i];
    B[6][p + 1] = 0.0;
    B[6][p + 2] = 0.0;

    B[7][p]   = 0.0;
    B[7][p + 1] = shapeDgl[2][i];
    B[7][p + 2] = 0.0;

    B[8][p]   = 0.0;
    B[8][p + 1] = 0.0;
    B[8][p + 2] = shapeDgl[2][i];
    }
}

template <typename TBaseClass>
void
Element3DMembrane<TBaseClass>
::GetMassMatrix(MatrixType & Me) const
{
  // Call the parent's get matrix function
  Superclass::GetMassMatrix(Me);

  // Since parent class doesn't have the material properties,
  // we need to adjust Me matrix here for the density of the element.
  Me = Me * m_mat->GetDensityHeatProduct();
}

template <typename TBaseClass>
void
Element3DMembrane<TBaseClass>
::GetMaterialMatrix(MatrixType & D) const
{
  unsigned int d = 9;

  D.set_size(d, d);

  D.fill(0.0);

  // This is the main difference from the linear elasticity problem.
  /* Material properties matrix.  Simpler than linear elasticity. */
  Float disot = m_mat->GetYoungsModulus();
  for( unsigned int i = 0; i < d; i++ )
    {
    D[i][i] = disot;
    }
}

template <typename TBaseClass>
void
Element3DMembrane<TBaseClass>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Materials: " << this->m_mat << std::endl;
}

}
}  // end namespace itk::fem

#endif // #ifndef itkFEMElement3DMembrane_hxx
