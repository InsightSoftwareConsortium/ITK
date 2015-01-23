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
#ifndef itkSpatialObjectTreeContainer_hxx
#define itkSpatialObjectTreeContainer_hxx

#include "itkSpatialObjectTreeContainer.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
SpatialObjectTreeContainer< TDimension >::SpatialObjectTreeContainer()
{}

/** Destructor */
template< unsigned int TDimension >
SpatialObjectTreeContainer< TDimension >::~SpatialObjectTreeContainer()
{}

/** Set the root */
template< unsigned int TDimension >
bool
SpatialObjectTreeContainer< TDimension >::SetRoot(const SpatialObjectPointer element)
{
  if ( this->m_Root )
    {
    std::cout << "This tree has already a root" << std::endl;
    return false;
    }

  if ( element->GetTreeNode() )
    {
    this->m_Root = element->GetModifiableTreeNode();
    }
  else
    {
    this->m_Root = SpatialObjectTreeNode< TDimension >::New();
    this->m_Root->Set(element);
    }
  return true;
}
} // namespace itk

#endif
