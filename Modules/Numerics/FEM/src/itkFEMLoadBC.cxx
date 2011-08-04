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

#include "itkFEMLoadBC.h"

namespace itk
{
namespace fem
{

// Overload the CreateAnother() method.
::itk::LightObject::Pointer LoadBC::CreateAnother(void) const
{
  ::itk::LightObject::Pointer smartPtr;
  Pointer copyPtr = Self::New();

  // Copy Load Contents
  copyPtr->m_DegreeOfFreedom = this->m_DegreeOfFreedom;
  copyPtr->m_Value = this->m_Value;
  copyPtr->m_Element = this->m_Element;
  copyPtr->SetGlobalNumber( this->GetGlobalNumber() );

  smartPtr = static_cast<Pointer>(copyPtr);

  return smartPtr;
}

void LoadBC::SetDegreeOfFreedom(int dof)
{
  this->m_DegreeOfFreedom = dof;
}

int LoadBC::GetDegreeOfFreedom() const
{
  return this->m_DegreeOfFreedom;
}

void LoadBC::SetValue(const vnl_vector<Element::Float> val)
{
  this->m_Value = val;
}

vnl_vector<Element::Float> LoadBC::GetValue() const
{
  return this->m_Value;
}

void LoadBC::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Element: " << this->m_Element << std::endl;
  os << indent << "Value: " << this->m_Value << std::endl;
  os << indent << "Degree Of Freedom: " << this->m_DegreeOfFreedom << std::endl;
}

}
}  // end namespace itk::fem
