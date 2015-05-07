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

#include "itkFEMLoadGrav.h"

namespace itk
{
namespace fem
{

void LoadGrav::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

// Overload the CreateAnother() method.
::itk::LightObject::Pointer LoadGravConst::CreateAnother(void) const
{
  ::itk::LightObject::Pointer smartPtr;
  Pointer copyPtr = Self::New();

  // Copy Load Contents
  copyPtr->m_GravityForce = this->m_GravityForce;
  for( unsigned int i = 0; i < this->m_Element.size(); i++ )
    {
    copyPtr->AddNextElement( this->m_Element[i] );
    }
  copyPtr->SetGlobalNumber( this->GetGlobalNumber() );

  smartPtr = static_cast<Pointer>(copyPtr);

  return smartPtr;
}

vnl_vector<Element::Float> LoadGravConst::GetGravitationalForceAtPoint(vnl_vector<Element::Float> )
{
  return m_GravityForce;
}

void LoadGravConst::SetForce(const vnl_vector<itk::fem::Element::Float> force)
{
  this->m_GravityForce = force;
}

vnl_vector<itk::fem::Element::Float> & LoadGravConst::GetForce()
{
  return this->m_GravityForce;
}

const vnl_vector<itk::fem::Element::Float> & LoadGravConst::GetForce() const
{
  return this->m_GravityForce;
}

void LoadGravConst::ApplyLoad(Element::ConstPointer element, Element::VectorType & Fe)
{
  // Order of integration
  // FIXME: Allow changing the order of integration by setting a
  //        static member within an element base class.
  unsigned int order = 0;

  const unsigned int Nip = element->GetNumberOfIntegrationPoints(order);
  const unsigned int Ndofs = element->GetNumberOfDegreesOfFreedomPerNode();
  const unsigned int Nnodes = element->GetNumberOfNodes();

  Element::VectorType force(Ndofs, 0.0),
  ip, gip, force_tmp, shapeF;

  Fe.set_size( element->GetNumberOfDegreesOfFreedom() );
  Fe.fill(0.0);

  Element::Float w, detJ;
  for( unsigned int i = 0; i < Nip; i++ )
    {
    element->GetIntegrationPointAndWeight(i, ip, w, order);
    gip = element->GetGlobalFromLocalCoordinates(ip);

    shapeF = element->ShapeFunctions(ip);
    detJ = element->JacobianDeterminant(ip);

    // Adjust the size of a force vector returned from the load object so
    // that it is equal to the number of DOFs per node. If
    // GetGravitationalForceAtPoint returns a vector with less dimensions,
    // we add zero elements. If the GetGravitationalForceAtPoint
    // returned a vector with more dimensions, we remove the extra dimensions.
    force.fill(0.0);
    force_tmp = this->GetGravitationalForceAtPoint(gip);
    unsigned int Nd = Ndofs;
    if( force_tmp.size() < Nd )
      {
      Nd = force_tmp.size();
      }
    for( unsigned int d = 0; d < Nd; d++ )
      {
      force[d] = force_tmp[d];
      }
    // Claculate the equivalent nodal loads
    for( unsigned int n = 0; n < Nnodes; n++ )
      {
      for( unsigned int d = 0; d < Ndofs; d++ )
        {
        Fe[n * Ndofs + d] += shapeF[n] * force[d] * w * detJ;
        }
      }
    }
}

void LoadGravConst::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Gravity Force: " << this->m_GravityForce << std::endl;
}

}
}  // end namespace itk::fem
