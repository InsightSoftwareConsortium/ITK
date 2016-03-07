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

#include "itkFEMLoadEdge.h"

namespace itk
{
namespace fem
{

// Overload the CreateAnother() method.
::itk::LightObject::Pointer LoadEdge::CreateAnother(void) const
{
  ::itk::LightObject::Pointer smartPtr;
  Pointer copyPtr = Self::New();

  copyPtr->m_Edge = this->m_Edge;

  // vnl_matrix = operator copies all elements
  copyPtr->m_Force = this->m_Force;
  for( unsigned int i = 0; i < this->m_Element.size(); i++ )
    {
    copyPtr->AddNextElement( this->m_Element[i] );
    }

  copyPtr->SetGlobalNumber( this->GetGlobalNumber() );

  smartPtr = static_cast<Pointer>(copyPtr);

  return smartPtr;
}

void LoadEdge::SetEdge(int edge)
{
  this->m_Edge = edge;
}

int LoadEdge::GetEdge() const
{
  return this->m_Edge;
}

void LoadEdge::SetForce(const vnl_matrix<itk::fem::Element::Float> force)
{
  this->m_Force = force;
}

const vnl_matrix<itk::fem::Element::Float> & LoadEdge::GetForce() const
{
  return this->m_Force;
}

vnl_matrix<itk::fem::Element::Float> & LoadEdge::GetForce()
{
  return this->m_Force;
}

void LoadEdge::ApplyLoad(Element::ConstPointer element, Element::VectorType & Fe)
{
  //
  // Only some classes modify their state in PopulateEdges, but that
  // means for it to be a virtual method in every class, it has to be non-const.
  // But in general, the ApplyLoad method is correct in taking a const
  // pointer to element.
  const_cast<Element *>(element.GetPointer())->PopulateEdgeIds();

  const unsigned int NnDOF = element->GetNumberOfDegreesOfFreedomPerNode();
  const unsigned int EdgeNum = this->GetEdge();

  vnl_matrix<itk::fem::Element::Float> Force = this->GetForce();

  const std::vector<std::vector<int> > EdgeIds = element->GetEdgeIds();

  Fe.set_size( element->GetNumberOfDegreesOfFreedom() );
  Fe.fill(0.0);

  unsigned int NEdgePts = static_cast<unsigned int>( (EdgeIds[0]).size() );
  int EdgePt;
  // access the edge points.
  for( unsigned int i = 0; i < NEdgePts; i++ )
    {
    EdgePt = (EdgeIds[EdgeNum])[i];
    for( unsigned int j = 0; j < NnDOF; j++ )
      {
      Fe[NnDOF * EdgePt + j] = Fe[NnDOF * EdgePt + j] + Force[i][j];
      }
    }

}

void LoadEdge::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Edge: " << this->m_Edge << std::endl;
  os << indent << "Force: " << this->m_Force << std::endl;
}

}
}  // end namespace itk::fem
