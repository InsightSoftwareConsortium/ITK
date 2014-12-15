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
#ifndef itkLevelSetDomainPartition_hxx
#define itkLevelSetDomainPartition_hxx

#include "itkLevelSetDomainPartition.h"

namespace itk
{
template< typename TImage >
LevelSetDomainPartition< TImage >::
LevelSetDomainPartition()
{
}

template< typename TImage >
LevelSetDomainPartition< TImage >::
~LevelSetDomainPartition()
{
}

template< typename TImage >
void
LevelSetDomainPartition< TImage >::
PopulateListImage()
{
  ListPixelType pixelList;
  IdentifierType i = NumericTraits< IdentifierType >::ZeroValue();
  while( i < this->m_NumberOfLevelSetFunctions )
    {
    pixelList.push_back(i++);
    }
  this->m_NearestNeighborListImage->FillBuffer( pixelList );
}

}
#endif
