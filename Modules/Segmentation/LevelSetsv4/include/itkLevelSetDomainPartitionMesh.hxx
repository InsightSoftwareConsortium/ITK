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
#ifndef itkLevelSetDomainPartitionMesh_hxx
#define itkLevelSetDomainPartitionMesh_hxx

#include "itkLevelSetDomainPartitionMesh.h"

namespace itk
{
template< typename TMesh >
LevelSetDomainPartitionMesh <TMesh>
::LevelSetDomainPartitionMesh()
{
}

template< typename TMesh >
LevelSetDomainPartitionMesh <TMesh>
::~LevelSetDomainPartitionMesh()
{
}

template< typename TMesh >
void
LevelSetDomainPartitionMesh <TMesh>
::PopulateListDomain()
{
  PointsContainerConstPointer points = this->m_Mesh->GetPoints();
  PointsContainerConstIterator p_it = points->Begin();
  PointsContainerConstIterator p_end = points->End();

  while( p_it != p_end )
    {
    PointIdentifierType & idx = p_it->Index();
    IdentifierListType identifierList;

    for( IdentifierType i = NumericTraits< IdentifierType >::ZeroValue(); i < this->m_NumberOfLevelSetFunctions; ++i )
      {
      if ( this->m_LevelSetDataPointerVector[i]->VerifyInsideRegion( idx ) )
        {
        identifierList.push_back(i);
        }

      this->m_ListDomain[ idx ] = identifierList;
      ++p_it;
      }
    }
}

template< typename TMesh >
void
LevelSetDomainPartitionMesh <TMesh>
::AllocateListDomain()
{
}

} //end namespace itk

#endif
