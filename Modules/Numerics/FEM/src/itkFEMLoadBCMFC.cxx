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

#include "itkFEMLoadBCMFC.h"

namespace itk
{
namespace fem
{
// Overload the CreateAnother() method.
::itk::LightObject::Pointer LoadBCMFC::CreateAnother(void) const
{
  ::itk::LightObject::Pointer smartPtr;
  Pointer copyPtr = Self::New();

  // Copy Load Contents
  copyPtr->m_Index = this->m_Index;
  copyPtr->m_LeftHandSide = this->m_LeftHandSide;
  copyPtr->m_RightHandSide = this->m_RightHandSide;
  copyPtr->SetGlobalNumber( this->GetGlobalNumber() );

  smartPtr = static_cast<Pointer>(copyPtr);

  return smartPtr;
}

/**
 * Fix a DOF to a prescribed value
 */
LoadBCMFC::LoadBCMFC(Element::ConstPointer element, int dof, vnl_vector<Element::Float> val):
  m_Index(0)
{
  m_LeftHandSide.clear();

  /** Set the correct weight */
  m_LeftHandSide.push_back( MFCTerm(element, dof, 1.0) );
  m_RightHandSide = val;
}

void LoadBCMFC::SetIndex(int ind)
{
  this->m_Index = ind;
}

int LoadBCMFC::GetIndex()
{
  return this->m_Index;
}

void LoadBCMFC::AddLeftHandSideTerm(LoadBCMFC::MFCTerm term)
{
  this->m_LeftHandSide.push_back(term);
}

void LoadBCMFC::AddRightHandSideTerm(Element::Float term)
{
  vnl_vector<Element::Float> tmpRightHandSide;
  tmpRightHandSide.set_size( this->m_RightHandSide.size() );
  for( unsigned int i = 0; i < this->m_RightHandSide.size(); i++ )
    {
    tmpRightHandSide[i] = this->m_RightHandSide[i];
    }

  this->m_RightHandSide.set_size(this->m_RightHandSide.size() + 1);
  for( unsigned int i = 0; i < tmpRightHandSide.size(); i++ )
    {
    this->m_RightHandSide[i] = tmpRightHandSide[i];
    }

  this->m_RightHandSide.put(this->m_RightHandSide.size() - 1, term);
}

int LoadBCMFC::GetNumberOfLeftHandSideTerms() const
{
  return static_cast<int>( this->m_LeftHandSide.size() );
}

int LoadBCMFC::GetNumberOfRightHandSideTerms() const
{
  return this->m_RightHandSide.size();
}

const LoadBCMFC::MFCTerm
LoadBCMFC::
GetLeftHandSideTerm(int lhs) const
{
  return this->m_LeftHandSide.at(lhs);
}

Element::Float LoadBCMFC::GetRightHandSideTerm(int rhs) const
{
  return this->m_RightHandSide.get(rhs);
}

const std::vector<LoadBCMFC::MFCTerm> & LoadBCMFC::GetLeftHandSideArray() const
{
  return this->m_LeftHandSide;
}
std::vector<LoadBCMFC::MFCTerm> & LoadBCMFC::GetLeftHandSideArray()
{
  return this->m_LeftHandSide;
}

vnl_vector<Element::Float> & LoadBCMFC::GetRightHandSideArray()
{
  return this->m_RightHandSide;
}

void LoadBCMFC::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Index: " << this->m_Index << std::endl;
  os << indent << "Left Hand Side Size: " << this->m_LeftHandSide.size() << std::endl;
  for( unsigned int i = 0; i < this->m_LeftHandSide.size(); i++ )
    {
    os << indent << "Left Hand Side Element (" << i << "): " << this->m_LeftHandSide[i].m_element << std::endl;
    os << indent << "Left Hand Side DOF (" << i << "): " << this->m_LeftHandSide[i].dof << std::endl;
    os << indent << "Left Hand Side Value (" << i << "): " << this->m_LeftHandSide[i].value << std::endl;
    }

  os << indent << "Left Hand Side Size: " << this->m_LeftHandSide.size() << std::endl;
  os << indent << "Right HandSide: " << this->m_RightHandSide << std::endl;
}

}
}  // end namespace itk::fem
