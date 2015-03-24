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

#ifndef itkFEMElement2DMembrane_hxx
#define itkFEMElement2DMembrane_hxx

#include "itkFEMElement2DMembrane.h"

namespace itk
{
namespace fem
{
template <typename TBaseClass>
Element2DMembrane<TBaseClass>
::Element2DMembrane() : Superclass(), m_mat(ITK_NULLPTR)
{
}

// ////////////////////////////////////////////////////////////////////////
/**
 * Methods related to the physics of the problem.
 */

template <typename TBaseClass>
void
Element2DMembrane<TBaseClass>
::GetStrainDisplacementMatrix(MatrixType & B, const MatrixType & shapeDgl) const
{
  unsigned int p;
  unsigned int Nn = this->GetNumberOfNodes();

  B.set_size(4, 2 * Nn); // note minor difference from linear elasticity
  // Copy the shape function derivatives to the B matrix.
  for( unsigned int i = 0; i < Nn; i++ )
    {
    // Compute B index
    p = i << 1;

    // Compute B elements
    B[0][p]   = shapeDgl[0][i];
    B[0][p + 1] = 0.0;

    B[1][p]   = 0.0;
    B[1][p + 1] = shapeDgl[0][i];

    B[2][p]   = shapeDgl[1][i];
    B[2][p + 1] = 0.0;

    B[3][p]   = 0.0;
    B[3][p + 1] = shapeDgl[1][i];
    }
}

template <typename TBaseClass>
void
Element2DMembrane<TBaseClass>
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
Element2DMembrane<TBaseClass>
::GetMaterialMatrix(MatrixType & D) const
{
  unsigned int d = 4;

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
Element2DMembrane<TBaseClass>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Materials: " << this->m_mat << std::endl;
}

}
}  // end namespace itk::fem

#endif // #ifndef itkFEMElement2DMembrane_hxx
