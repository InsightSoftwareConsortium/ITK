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
#ifndef itkLevelSetEvolutionComputeIterationThreader_hxx
#define itkLevelSetEvolutionComputeIterationThreader_hxx

#include "itkLevelSetEvolutionComputeIterationThreader.h"

#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

template< typename TImage, typename TLevelSetEvolution >
LevelSetEvolutionComputeIterationThreader< LevelSetDenseImage< TImage >, ThreadedImageRegionPartitioner< TImage::ImageDimension >, TLevelSetEvolution >
::LevelSetEvolutionComputeIterationThreader()
{
}

template< typename TImage, typename TLevelSetEvolution >
void
LevelSetEvolutionComputeIterationThreader< LevelSetDenseImage< TImage >, ThreadedImageRegionPartitioner< TImage::ImageDimension >, TLevelSetEvolution >
::ThreadedExecution( const DomainType & imageSubRegion,
                     const ThreadIdType itkNotUsed(threadId) )
{
  typename LevelSetContainerType::Iterator levelSetContainerIt = this->m_Associate->m_LevelSetContainer->Begin();
  typename LevelSetType::Pointer levelSet = levelSetContainerIt->GetLevelSet();
  typename LevelSetImageType::ConstPointer levelSetImage = levelSet->GetImage();

  // Identify the level-set region
  OffsetType offset = levelSet->GetDomainOffset();
  IndexType index = imageSubRegion.GetIndex() - offset;
  RegionType subRegion;
  subRegion.SetSize( imageSubRegion.GetSize() );
  subRegion.SetIndex( index );

  ImageRegionConstIteratorWithIndex< LevelSetImageType > imageIt( levelSetImage, subRegion );
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
      levelSetUpdateImages[idListIdx] = levelSetUpdate->GetModifiableImage();
      termContainers[idListIdx] = this->m_Associate->m_EquationContainer->GetEquation( *idListIt - 1 );
      ++idListIt;
      ++idListIdx;
      }

    while( !imageIt.IsAtEnd() )
      {
      const IndexType levelSetIndex = imageIt.GetIndex();
      const IndexType inputIndex = imageIt.GetIndex() + offset;
      for( idListIdx = 0; idListIdx < numberOfLevelSets; ++idListIdx )
        {
        LevelSetDataType characteristics;
        termContainers[idListIdx]->ComputeRequiredData( inputIndex, characteristics );
        LevelSetOutputRealType temp_update = termContainers[idListIdx]->Evaluate( inputIndex, characteristics );
        levelSetUpdateImages[idListIdx]->SetPixel( levelSetIndex, temp_update );
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
    typename LevelSetImageType::Pointer levelSetUpdateImage = levelSetUpdate->GetModifiableImage();

    typename EquationContainerType::Iterator equationContainerIt = this->m_Associate->m_EquationContainer->Begin();
    typename TermContainerType::Pointer termContainer = equationContainerIt->GetEquation();

    imageIt.GoToBegin();
    while( !imageIt.IsAtEnd() )
      {
      const IndexType levelSetIndex = imageIt.GetIndex();
      const IndexType inputIndex = imageIt.GetIndex() + offset;
      LevelSetDataType characteristics;
      termContainer->ComputeRequiredData( inputIndex, characteristics );
      LevelSetOutputRealType temp_update = termContainer->Evaluate( inputIndex, characteristics );
      levelSetUpdateImage->SetPixel( levelSetIndex, temp_update );
      ++imageIt;
      }
    }
}

template< typename TImage, typename TLevelSetEvolution >
LevelSetEvolutionComputeIterationThreader<
  LevelSetDenseImage< TImage >,
  ThreadedIteratorRangePartitioner< typename TLevelSetEvolution::DomainMapImageFilterType::DomainMapType::const_iterator >,
    TLevelSetEvolution >
::LevelSetEvolutionComputeIterationThreader()
{
}

template< typename TImage, typename TLevelSetEvolution >
void
LevelSetEvolutionComputeIterationThreader<
  LevelSetDenseImage< TImage >,
  ThreadedIteratorRangePartitioner< typename TLevelSetEvolution::DomainMapImageFilterType::DomainMapType::const_iterator >,
    TLevelSetEvolution >
::ThreadedExecution( const DomainType & imageSubDomain,
                     const ThreadIdType itkNotUsed(threadId) )
{
  typename InputImageType::ConstPointer inputImage = this->m_Associate->m_EquationContainer->GetInput();

  typename DomainType::IteratorType mapIt = imageSubDomain.Begin();
  while( mapIt != imageSubDomain.End() )
    {
    ImageRegionConstIteratorWithIndex< InputImageType > it( inputImage, *(mapIt->second.GetRegion()) );
    it.GoToBegin();

    while( !it.IsAtEnd() )
      {
      const IdListType idList = *(mapIt->second.GetIdList());

      //itkAssertInDebugOrThrowInReleaseMacro( !idList.empty() );

      for( IdListConstIterator idListIt = idList.begin(); idListIt != idList.end(); ++idListIt )
        {
        typename LevelSetType::Pointer levelSetUpdate = this->m_Associate->m_UpdateBuffer->GetLevelSet( *idListIt - 1 );

        OffsetType offset = levelSetUpdate->GetDomainOffset();
        IndexType levelSetIndex = it.GetIndex() - offset;

        LevelSetDataType characteristics;
        typename TermContainerType::Pointer termContainer = this->m_Associate->m_EquationContainer->GetEquation( *idListIt - 1 );
        termContainer->ComputeRequiredData( it.GetIndex(), characteristics );
        LevelSetOutputRealType tempUpdate = termContainer->Evaluate( it.GetIndex(), characteristics );

        LevelSetImageType * levelSetImage = levelSetUpdate->GetModifiableImage();
        levelSetImage->SetPixel( levelSetIndex, tempUpdate );
        }
      ++it;
      }
    ++mapIt;
    }
}

template< typename TOutput, unsigned int VDimension, typename TLevelSetEvolution >
LevelSetEvolutionComputeIterationThreader<
      WhitakerSparseLevelSetImage< TOutput, VDimension >,
      ThreadedIteratorRangePartitioner< typename WhitakerSparseLevelSetImage< TOutput, VDimension >::LayerConstIterator >,
      TLevelSetEvolution >
::LevelSetEvolutionComputeIterationThreader()
{
}

template< typename TOutput, unsigned int VDimension, typename TLevelSetEvolution >
void
LevelSetEvolutionComputeIterationThreader<
      WhitakerSparseLevelSetImage< TOutput, VDimension >,
      ThreadedIteratorRangePartitioner< typename WhitakerSparseLevelSetImage< TOutput, VDimension >::LayerConstIterator >,
      TLevelSetEvolution >
::BeforeThreadedExecution()
{
  const ThreadIdType numberOfThreads = this->GetNumberOfThreadsUsed();
  this->m_NodePairsPerThread.resize( numberOfThreads );

  for( ThreadIdType ii = 0; ii < numberOfThreads; ++ii )
    {
    this->m_NodePairsPerThread[ii].clear();
    }
}

template< typename TOutput, unsigned int VDimension, typename TLevelSetEvolution >
void
LevelSetEvolutionComputeIterationThreader<
      WhitakerSparseLevelSetImage< TOutput, VDimension >,
      ThreadedIteratorRangePartitioner< typename WhitakerSparseLevelSetImage< TOutput, VDimension >::LayerConstIterator >,
      TLevelSetEvolution >
::ThreadedExecution( const DomainType & iteratorSubRange,
                     const ThreadIdType threadId )
{
  typename LevelSetContainerType::Iterator it = this->m_Associate->m_LevelSetContainerIteratorToProcessWhenThreading;
  typename LevelSetType::ConstPointer levelSet = it->GetLevelSet();

  LevelSetIdentifierType levelSetId = it->GetIdentifier();
  OffsetType offset = levelSet->GetDomainOffset();

  typename TermContainerType::Pointer termContainer = this->m_Associate->m_EquationContainer->GetEquation( levelSetId );

  typename LevelSetType::LayerConstIterator listIt = iteratorSubRange.Begin();

  while( listIt != iteratorSubRange.End() )
    {
    const LevelSetInputType levelsetIndex = listIt->first;
    LevelSetInputType inputIndex = listIt->first + offset;

    LevelSetDataType characteristics;

    termContainer->ComputeRequiredData( inputIndex, characteristics );

    const LevelSetOutputType temp_update =
        static_cast< LevelSetOutputType >( termContainer->Evaluate( inputIndex, characteristics ) );

    this->m_NodePairsPerThread[threadId].push_back( NodePairType( levelsetIndex, temp_update ) );

    ++listIt;
    }
}

template< typename TOutput, unsigned int VDimension, typename TLevelSetEvolution >
void
LevelSetEvolutionComputeIterationThreader<
      WhitakerSparseLevelSetImage< TOutput, VDimension >,
      ThreadedIteratorRangePartitioner< typename WhitakerSparseLevelSetImage< TOutput, VDimension >::LayerConstIterator >,
      TLevelSetEvolution >
::AfterThreadedExecution()
{
  typename LevelSetContainerType::Iterator it = this->m_Associate->m_LevelSetContainerIteratorToProcessWhenThreading;
  LevelSetIdentifierType levelSetId = it->GetIdentifier();
  typename LevelSetEvolutionType::LevelSetLayerType * levelSetLayerUpdateBuffer = this->m_Associate->m_UpdateBuffer[ levelSetId ];

  const ThreadIdType numberOfThreads = this->GetNumberOfThreadsUsed();
  for( ThreadIdType ii = 0; ii < numberOfThreads; ++ii )
    {
    typename std::vector< NodePairType >::const_iterator pairIt = this->m_NodePairsPerThread[ii].begin();
    while( pairIt != this->m_NodePairsPerThread[ii].end() )
      {
      levelSetLayerUpdateBuffer->insert( *pairIt );
      ++pairIt;
      }
    }
}

} // end namespace itk

#endif
