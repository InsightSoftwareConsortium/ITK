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
#ifndef __itkLevelSetEvolutionComputeIterationThreader_hxx
#define __itkLevelSetEvolutionComputeIterationThreader_hxx

#include "itkLevelSetEvolutionComputeIterationThreader.h"

#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

template< class TImage, class TLevelSetEvolution >
LevelSetEvolutionComputeIterationThreader< LevelSetDenseImageBase< TImage >, ThreadedImageRegionPartitioner< TImage::ImageDimension >, TLevelSetEvolution >
::LevelSetEvolutionComputeIterationThreader()
{
}

template< class TImage, class TLevelSetEvolution >
void
LevelSetEvolutionComputeIterationThreader< LevelSetDenseImageBase< TImage >, ThreadedImageRegionPartitioner< TImage::ImageDimension >, TLevelSetEvolution >
::ThreadedExecution( const DomainType & imageSubRegion,
                     const ThreadIdType itkNotUsed(threadId) )
{
  typename LevelSetContainerType::Iterator levelSetContainerIt = this->m_Associate->m_LevelSetContainer->Begin();
  typename LevelSetType::Pointer levelSet = levelSetContainerIt->GetLevelSet();
  typename LevelSetImageType::ConstPointer levelSetImage = levelSet->GetImage();
  ImageRegionConstIteratorWithIndex< LevelSetImageType > imageIt( levelSetImage, imageSubRegion );
  imageIt.GoToBegin();

  if( this->m_Associate->m_LevelSetContainer->HasDomainMap() )
    {
    const IdListType * idList = this->m_Associate->m_IdListToProcessWhenThreading;

    // Avoid repeated map lookups.
    const size_t numberOfLevelSets = idList->size();
    std::vector< LevelSetImageType * > levelSetUpdateImages( numberOfLevelSets );
    std::vector< TermContainerType * > termContainers( numberOfLevelSets );
    IdListConstIterator idListIt = idList->begin();
    unsigned int idListIdx = 0;
    while( idListIt != idList->end() )
      {
      //! \todo Fix me for string identifiers
      LevelSetType * levelSetUpdate = this->m_Associate->m_UpdateBuffer->GetLevelSet( *idListIt - 1 );
      levelSetUpdateImages[idListIdx] = levelSetUpdate->GetImage();
      termContainers[idListIdx] = this->m_Associate->m_EquationContainer->GetEquation( *idListIt - 1 );
      ++idListIt;
      ++idListIdx;
      }

    while( !imageIt.IsAtEnd() )
      {
      const typename InputImageType::IndexType index = imageIt.GetIndex();
      for( idListIdx = 0; idListIdx < numberOfLevelSets; ++idListIdx )
        {
        LevelSetDataType characteristics;
        termContainers[idListIdx]->ComputeRequiredData( index, characteristics );
        LevelSetOutputRealType temp_update = termContainers[idListIdx]->Evaluate( index, characteristics );
        levelSetUpdateImages[idListIdx]->SetPixel( index, temp_update );
        }
      ++imageIt;
      }
    }
  else
    {
    // This is for single level set analysis, so we only process the first level
    // set.
    typename LevelSetContainerType::ConstIterator levelSetUpdateContainerIt = this->m_Associate->m_UpdateBuffer->Begin();
    typename LevelSetType::Pointer levelSetUpdate = levelSetUpdateContainerIt->GetLevelSet();
    typename LevelSetImageType::Pointer levelSetUpdateImage = levelSetUpdate->GetImage();

    typename EquationContainerType::Iterator equationContainerIt = this->m_Associate->m_EquationContainer->Begin();
    typename TermContainerType::Pointer termContainer = equationContainerIt->GetEquation();

    imageIt.GoToBegin();
    while( !imageIt.IsAtEnd() )
      {
      const typename InputImageType::IndexType index = imageIt.GetIndex();
      LevelSetDataType characteristics;
      termContainer->ComputeRequiredData( index, characteristics );
      LevelSetOutputRealType temp_update = termContainer->Evaluate( index, characteristics );
      levelSetUpdateImage->SetPixel( index, temp_update );
      ++imageIt;
      }
    }
}

} // end namespace itk

#endif
