/*=========================================================================
*
* Copyright Insight Software Consortium
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0.txt
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*
*=========================================================================*/

#ifndef itkFEMElement3DMembrane1DOF_hxx
#define itkFEMElement3DMembrane1DOF_hxx

#include "itkFEMElement3DMembrane1DOF.h"

namespace itk
{
namespace fem
{
template <typename TBaseClass>
Element3DMembrane1DOF<TBaseClass>
::Element3DMembrane1DOF() : Superclass(), m_Mat(ITK_NULLPTR)
{
}

// ////////////////////////////////////////////////////////////////////////
/*
 * Methods related to the physics of the problem.
 */

template <typename TBaseClass>
void
Element3DMembrane1DOF<TBaseClass>
::GetStrainDisplacementMatrix(MatrixType & /*HACK B*/, const MatrixType & /*HACK shapeDgl*/) const
{
  //HACK:  Comment.
}

template <typename TBaseClass>
void
Element3DMembrane1DOF<TBaseClass>
::GetMassMatrix(MatrixType & Me) const
{
  // Call the parent's get matrix function
  Superclass::GetMassMatrix(Me);

  // Since parent class doesn't have the material properties,
  // we need to adjust Me matrix here for the density of the element.
  Me = Me * m_Mat->GetDensityHeatProduct();
}

template <typename TBaseClass>
void
Element3DMembrane1DOF<TBaseClass>
::GetMaterialMatrix(MatrixType & D) const
{
  unsigned int d = 3;

  D.set_size(d, d);

  D.fill(0.0);

  // This is the main difference from the linear elasticity problem.
  /* Material properties matrix.  Simpler than linear elasticity. */
  Float disot = m_Mat->GetYoungsModulus();
  for( unsigned int i = 0; i < d; i++ )
    {
    D[i][i] = disot;
    }
}

template <typename TBaseClass>
void Element3DMembrane1DOF<TBaseClass>
::GetStiffnessMatrix(MatrixType & Ke) const
{
  Superclass::GetStiffnessMatrix(Ke);
}

template <typename TBaseClass>
void
Element3DMembrane1DOF<TBaseClass>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Materials: " << this->m_Mat << std::endl;
}

}
}  // end namespace itk::fem

#endif // #ifndef itkFEMElement3DMembrane1DOF_hxx
