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

#include "itkFEMLoadElementBase.h"

namespace itk
{
namespace fem
{

// Overload the CreateAnother() method
::itk::LightObject::Pointer LoadElement::CreateAnother(void) const
{
  ::itk::LightObject::Pointer smartPtr;
  Pointer copyPtr = Self::New();
  for( unsigned int i = 0; i < this->m_Element.size(); i++ )
    {
    copyPtr->AddNextElement( this->m_Element[i] );
    }
  copyPtr->SetGlobalNumber( this->GetGlobalNumber() );

  smartPtr = static_cast<Pointer>(copyPtr);

  return smartPtr;
}

void LoadElement::AddNextElementInternal(const Element *e)
{
  Element::ConstPointer p(e);
  this->m_Element.push_back(p);
}

unsigned int LoadElement::GetNumberOfElements(void)
{
  return static_cast<unsigned int>( this->m_Element.size() );
}

Element::ConstPointer LoadElement::GetElement(int i)
{
  return this->m_Element[i];
}

void LoadElement::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "#Elements: " << this->m_Element.size() << std::endl;
  for( unsigned int i = 0; i < this->m_Element.size(); i++ )
    {
    os << indent << "Element (" << i << "): " << this->m_Element[i] << std::endl;
    }
}

}
}  // end namespace itk::fem
