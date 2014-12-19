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
#ifndef itkLevelSetDomainPartitionImageWithKdTree_hxx
#define itkLevelSetDomainPartitionImageWithKdTree_hxx

#include "itkLevelSetDomainPartitionImageWithKdTree.h"

namespace itk
{
template< typename TImage >
LevelSetDomainPartitionImageWithKdTree< TImage >
::LevelSetDomainPartitionImageWithKdTree() :
  m_KdTree(ITK_NULLPTR), m_NumberOfNeighbors( 10 )
{
}

template< typename TImage >
LevelSetDomainPartitionImageWithKdTree< TImage >
::~LevelSetDomainPartitionImageWithKdTree()
{
}

template< typename TImage >
void LevelSetDomainPartitionImageWithKdTree< TImage >
::PopulateListDomain()
{
  if( this->m_KdTree.IsNotNull() )
    {
    this->PopulateDomainWithKdTree();
    }
  else
    {
    Superclass::PopulateListDomain();
    }
}

template< typename TImage >
void LevelSetDomainPartitionImageWithKdTree< TImage >
::PopulateDomainWithKdTree()
{
  Superclass::AllocateListDomain();

  const ListRegionType region = this->m_ListDomain->GetLargestPossibleRegion();

  ListIteratorType lIt(this->m_ListDomain, region);

  for ( lIt.GoToBegin(); !lIt.IsAtEnd(); ++lIt )
    {
    const ListIndexType & index = lIt.GetIndex();
    ListPointType pt;

    this->m_ListDomain->TransformIndexToPhysicalPoint( index, pt );

    CentroidVectorType queryPoint = pt.GetVectorFromOrigin();

    typename TreeType::InstanceIdentifierVectorType neighbors;
    this->m_KdTree->Search(queryPoint, this->m_NumberOfNeighbors, neighbors);

    IdentifierListType identifierList;
    for ( NeighborsIdType i = 0; i < this->m_NumberOfNeighbors; ++i )
      {
      IdentifierType levelSetID = neighbors[i];
      if ( this->m_LevelSetDomainRegionVector[levelSetID].IsInside( index ) )
        {
        identifierList.push_back(neighbors[i]);
        }
      }
    lIt.Set(identifierList);
    }
}

} //end namespace itk
#endif
