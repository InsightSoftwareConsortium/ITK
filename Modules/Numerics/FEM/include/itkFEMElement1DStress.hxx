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

#ifndef itkFEMElement1DStress_hxx
#define itkFEMElement1DStress_hxx

#include "itkFEMElement1DStress.h"
#include "itkFEMMaterialLinearElasticity.h"

namespace itk
{
namespace fem
{
template <typename TBaseClass>
Element1DStress<TBaseClass>
::Element1DStress() : Superclass(), m_mat(ITK_NULLPTR)
{
}

// ////////////////////////////////////////////////////////////////////////
/**
 * Methods related to the physics of the problem.
 */

template <typename TBaseClass>
void
Element1DStress<TBaseClass>
::GetStrainDisplacementMatrix(MatrixType & B, const MatrixType & shapeDgl) const
{
  B.set_size(1, 2);

  // Copy the shape function derivatives to the B matrix.
  B[0][0] = shapeDgl[0][0];
  B[0][1] = shapeDgl[0][1];
}

template <typename TBaseClass>
void
Element1DStress<TBaseClass>
::GetMaterialMatrix(MatrixType & D) const
{
  D.set_size(1, 1);
  D.fill(0.0);

  // Material properties matrix is a scalar
  D[0][0] = m_mat->GetYoungsModulus() * m_mat->GetCrossSectionalArea();
}

template <typename TBaseClass>
void
Element1DStress<TBaseClass>
::GetStiffnessMatrix(MatrixType & Ke) const
{
  const unsigned int Ndims = this->GetNumberOfSpatialDimensions();
  const unsigned int Nn = this->GetNumberOfNodes();

  // First we obtain the Ke by calling the parent's
  // GetStiffnessMatrix function. This is the stiffness matrix
  // with only one DOF per node.
  Superclass::GetStiffnessMatrix(Ke);

  // Calculate the nodal displacement transformation matrix according
  // to the number of dimensions in global coordinate system
  MatrixType T(2, 2 * Ndims, 0.0);

  VectorType d = this->GetNodeCoordinates(1) - this->GetNodeCoordinates(0);
  d = d / d.magnitude();
  for( unsigned int i = 0; i < Ndims; i++ )
    {
    for( unsigned int n = 0; n < Nn; n++ )
      {
      T[n][n * Ndims + i] = d[i];
      }
    }

  // Apply the nodal displacement transformation matrix to original
  // element stiffness matrix to obtain the element stiffness
  // matrix in global coordinates.
  Ke = T.transpose() * Ke * T;
}

template <typename TBaseClass>
void
Element1DStress<TBaseClass>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Young Modulus: " << this->m_mat << std::endl;
}

}
}  // end namespace itk::fem

#endif // #ifndef itkFEMElement1DStress_hxx
