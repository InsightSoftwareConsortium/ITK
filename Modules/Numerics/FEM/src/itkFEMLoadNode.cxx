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

#include "itkFEMLoadNode.h"

namespace itk
{
namespace fem
{

// Overload the CreateAnother() method.
::itk::LightObject::Pointer LoadNode::CreateAnother(void) const
{
  ::itk::LightObject::Pointer smartPtr;
  Pointer copyPtr = Self::New();

  copyPtr->m_Element = this->m_Element;
  copyPtr->m_Point = this->m_Point;
  copyPtr->m_Force = this->m_Force;
  copyPtr->SetGlobalNumber( this->GetGlobalNumber() );

  smartPtr = static_cast<Pointer>(copyPtr);

  return smartPtr;
}

void LoadNode::SetNode(int num)
{
  this->m_Point = num;
}

int LoadNode::GetNode() const
{
  return this->m_Point;
}

void LoadNode::SetForce(const vnl_vector<Float> force)
{
  this->m_Force = force;
}

vnl_vector<itk::fem::Element::Float> LoadNode::GetForce() const
{
  return this->m_Force;
}

void LoadNode::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Element: " << this->m_Element << std::endl;
  os << indent << "Point: " << this->m_Point << std::endl;
  os << indent << "Force: " << this->m_Force << std::endl;
}

}
}  // end namespace itk::fem
