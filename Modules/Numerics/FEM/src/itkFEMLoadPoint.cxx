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

#include "itkFEMLoadPoint.h"

namespace itk
{
namespace fem
{

::itk::LightObject::Pointer LoadPoint::CreateAnother(void) const
{
  ::itk::LightObject::Pointer smartPtr;
  Pointer copyPtr = Self::New();

  copyPtr->m_Point = this->m_Point;
  copyPtr->m_ForcePoint = this->m_ForcePoint;
  copyPtr->SetGlobalNumber( this->GetGlobalNumber() );

  smartPtr = static_cast<Pointer>(copyPtr);

  return smartPtr;
}

void LoadPoint::SetPoint(const vnl_vector<Float> p)
{
  this->m_Point = p;
}

vnl_vector<itk::fem::Element::Float> LoadPoint::GetPoint()
{
  return this->m_Point;
}

void LoadPoint::SetForce(const vnl_vector<Float> f)
{
  this->m_ForcePoint = f;
}

vnl_vector<itk::fem::Element::Float> LoadPoint::GetForce()
{
  return this->m_ForcePoint;
}

void LoadPoint::ApplyLoad(Element::ConstPointer element, Element::VectorType & Fe)
{
  const unsigned int NnDOF = element->GetNumberOfDegreesOfFreedomPerNode();
  const unsigned int Nnodes = element->GetNumberOfNodes();

  Element::VectorType force(NnDOF, 0.0);
  Element::VectorType shapeF;

  Fe.set_size( element->GetNumberOfDegreesOfFreedom() );
  Fe.fill(0.0);

  force = this->GetForce();

  // Retrieve the local coordinate at which the force acts
  Element::VectorType pt = this->GetPoint();

  // "Integrate" at the location of the point load
  shapeF = element->ShapeFunctions(pt);
  // Calculate the equivalent nodal loads
  for( unsigned int n = 0; n < Nnodes; n++ )
    {
    for( unsigned int d = 0; d < NnDOF; d++ )
      {
      Fe[n * NnDOF + d] += shapeF[n] * force[d];
      }
    }
}

void LoadPoint::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Point: " << this->m_Point << std::endl;
  os << indent << "Force Point: " << this->m_ForcePoint << std::endl;
}

}
}  // end namespace itk::fem
