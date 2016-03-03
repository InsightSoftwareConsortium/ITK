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

#ifndef itkFEMElementStd_hxx
#define itkFEMElementStd_hxx

#include "itkFEMElementStd.h"

namespace itk
{
namespace fem
{
template< unsigned int VNumberOfNodes, unsigned int VNumberOfSpatialDimensions, typename TBaseClass >
ElementStd< VNumberOfNodes, VNumberOfSpatialDimensions, TBaseClass >
::ElementStd()
{
  // Set all node ids to 0 (undefined)
  for( int i = 0; i < NumberOfNodes; i++ )
    {
    this->m_node[i] = ITK_NULLPTR;
    }
}

template<unsigned int VNumberOfNodes, unsigned int VNumberOfSpatialDimensions, typename TBaseClass >
void
ElementStd< VNumberOfNodes, VNumberOfSpatialDimensions, TBaseClass >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "#Nodes: " << NumberOfNodes << std::endl;
  for( unsigned int i = 0; i < NumberOfNodes; i++ )
    {
    os << indent << "Node (" << i << "): " << this->m_node[i] << std::endl;
    }
}

}  // end namespace fem
}  // end namespace itk

#endif // #ifndef itkFEMElementStd_hxx
