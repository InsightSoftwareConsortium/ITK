/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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


#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

template <typename TImage, typename TLevelSetEvolution>
void
LevelSetEvolutionComputeIterationThreader<LevelSetDenseImage<TImage>,
                                          ThreadedImageRegionPartitioner<TImage::ImageDimension>,
                                          TLevelSetEvolution>::ThreadedExecution(const DomainType & imageSubRegion,
                                                                                 const ThreadIdType itkNotUsed(
                                                                                   threadId))
{
  typename LevelSetContainerType::Iterator       levelSetContainerIt = this->m_Associate->m_LevelSetContainer->Begin();
  const typename LevelSetType::Pointer           levelSet = levelSetContainerIt->GetLevelSet();
  const typename LevelSetImageType::ConstPointer levelSetImage = levelSet->GetImage();

  // Identify the level-set region
  const OffsetType offset = levelSet->GetDomainOffset();
  const IndexType  index = imageSubRegion.GetIndex() - offset;
  const RegionType subRegion(index, imageSubRegion.GetSize());

  ImageRegionConstIteratorWithIndex<LevelSetImageType> imageIt(levelSetImage, subRegion);
  imageIt.GoToBegin();

  if (this->m_Associate->m_LevelSetContainer->HasDomainMap())
  {
    const IdListType * idList = this->m_Associate->m_IdListToProcessWhenThreading;

    // Avoid repeated map lookups.
    const size_t                     numberOfLevelSets = idList->size();
    std::vector<LevelSetImageType *> levelSetUpdateImages(numberOfLevelSets);
    std::vector<TermContainerType *> termContainers(numberOfLevelSets);
    auto                             idListIt = idList->begin();
    unsigned int                     idListIdx = 0;
    while (idListIt != idList->end())
    {
      //! \todo Fix me for string identifiers
      LevelSetType * levelSetUpdate = this->m_Associate->m_UpdateBuffer->GetLevelSet(*idListIt - 1);
      levelSetUpdateImages[idListIdx] = levelSetUpdate->GetModifiableImage();
      termContainers[idListIdx] = this->m_Associate->m_EquationContainer->GetEquation(*idListIt - 1);
      ++idListIt;
      ++idListIdx;
    }

    while (!imageIt.IsAtEnd())
    {
      const IndexType levelSetIndex = imageIt.GetIndex();
      const IndexType inputIndex = imageIt.GetIndex() + offset;
      for (idListIdx = 0; idListIdx < numberOfLevelSets; ++idListIdx)
      {
        LevelSetDataType characteristics;
        termContainers[idListIdx]->ComputeRequiredData(inputIndex, characteristics);
        const LevelSetOutputRealType temp_update = termContainers[idListIdx]->Evaluate(inputIndex, characteristics);
        levelSetUpdateImages[idListIdx]->SetPixel(levelSetIndex, temp_update);
      }
      ++imageIt;
    }
  }
  else
  {
    // This is for single level set analysis, so we only process the first level
    // set.
    typename LevelSetContainerType::ConstIterator levelSetUpdateContainerIt =
      this->m_Associate->m_UpdateBuffer->Begin();
    const typename LevelSetType::Pointer      levelSetUpdate = levelSetUpdateContainerIt->GetLevelSet();
    const typename LevelSetImageType::Pointer levelSetUpdateImage = levelSetUpdate->GetModifiableImage();

    typename EquationContainerType::Iterator  equationContainerIt = this->m_Associate->m_EquationContainer->Begin();
    const typename TermContainerType::Pointer termContainer = equationContainerIt->GetEquation();

    imageIt.GoToBegin();
    while (!imageIt.IsAtEnd())
    {
      const IndexType  levelSetIndex = imageIt.GetIndex();
      const IndexType  inputIndex = imageIt.GetIndex() + offset;
      LevelSetDataType characteristics;
      termContainer->ComputeRequiredData(inputIndex, characteristics);
      const LevelSetOutputRealType temp_update = termContainer->Evaluate(inputIndex, characteristics);
      levelSetUpdateImage->SetPixel(levelSetIndex, temp_update);
      ++imageIt;
    }
  }
}

template <typename TImage, typename TLevelSetEvolution>
void
LevelSetEvolutionComputeIterationThreader<
  LevelSetDenseImage<TImage>,
  ThreadedIteratorRangePartitioner<
    typename TLevelSetEvolution::DomainMapImageFilterType::DomainMapType::const_iterator>,
  TLevelSetEvolution>::ThreadedExecution(const DomainType & imageSubDomain, const ThreadIdType itkNotUsed(threadId))
{
  const typename InputImageType::ConstPointer inputImage = this->m_Associate->m_EquationContainer->GetInput();

  typename DomainType::IteratorType mapIt = imageSubDomain.Begin();
  while (mapIt != imageSubDomain.End())
  {
    ImageRegionConstIteratorWithIndex<InputImageType> it(inputImage, *(mapIt->second.GetRegion()));
    it.GoToBegin();

    while (!it.IsAtEnd())
    {
      const IdListType idList = *(mapIt->second.GetIdList());

      // itkAssertInDebugOrThrowInReleaseMacro( !idList.empty() );

      for (auto idListIt = idList.begin(); idListIt != idList.end(); ++idListIt)
      {
        const typename LevelSetType::Pointer levelSetUpdate =
          this->m_Associate->m_UpdateBuffer->GetLevelSet(*idListIt - 1);

        const OffsetType offset = levelSetUpdate->GetDomainOffset();
        const IndexType  levelSetIndex = it.GetIndex() - offset;

        LevelSetDataType                          characteristics;
        const typename TermContainerType::Pointer termContainer =
          this->m_Associate->m_EquationContainer->GetEquation(*idListIt - 1);
        termContainer->ComputeRequiredData(it.GetIndex(), characteristics);
        const LevelSetOutputRealType tempUpdate = termContainer->Evaluate(it.GetIndex(), characteristics);

        LevelSetImageType * levelSetImage = levelSetUpdate->GetModifiableImage();
        levelSetImage->SetPixel(levelSetIndex, tempUpdate);
      }
      ++it;
    }
    ++mapIt;
  }
}

template <typename TOutput, unsigned int VDimension, typename TLevelSetEvolution>
void
LevelSetEvolutionComputeIterationThreader<
  WhitakerSparseLevelSetImage<TOutput, VDimension>,
  ThreadedIteratorRangePartitioner<typename WhitakerSparseLevelSetImage<TOutput, VDimension>::LayerConstIterator>,
  TLevelSetEvolution>::BeforeThreadedExecution()
{
  const ThreadIdType numberOfWorkUnits = this->GetNumberOfWorkUnitsUsed();
  this->m_NodePairsPerThread.resize(numberOfWorkUnits);

  for (ThreadIdType ii = 0; ii < numberOfWorkUnits; ++ii)
  {
    this->m_NodePairsPerThread[ii].clear();
  }
}

template <typename TOutput, unsigned int VDimension, typename TLevelSetEvolution>
void
LevelSetEvolutionComputeIterationThreader<
  WhitakerSparseLevelSetImage<TOutput, VDimension>,
  ThreadedIteratorRangePartitioner<typename WhitakerSparseLevelSetImage<TOutput, VDimension>::LayerConstIterator>,
  TLevelSetEvolution>::ThreadedExecution(const DomainType & iteratorSubRange, const ThreadIdType threadId)
{
  typename LevelSetContainerType::Iterator  it = this->m_Associate->m_LevelSetContainerIteratorToProcessWhenThreading;
  const typename LevelSetType::ConstPointer levelSet = it->GetLevelSet();

  const LevelSetIdentifierType levelSetId = it->GetIdentifier();
  const OffsetType             offset = levelSet->GetDomainOffset();

  const typename TermContainerType::Pointer termContainer =
    this->m_Associate->m_EquationContainer->GetEquation(levelSetId);

  typename LevelSetType::LayerConstIterator listIt = iteratorSubRange.Begin();

  while (listIt != iteratorSubRange.End())
  {
    const LevelSetInputType levelsetIndex = listIt->first;
    const LevelSetInputType inputIndex = listIt->first + offset;

    LevelSetDataType characteristics;

    termContainer->ComputeRequiredData(inputIndex, characteristics);

    const auto temp_update = static_cast<LevelSetOutputType>(termContainer->Evaluate(inputIndex, characteristics));

    this->m_NodePairsPerThread[threadId].push_back(NodePairType(levelsetIndex, temp_update));

    ++listIt;
  }
}

template <typename TOutput, unsigned int VDimension, typename TLevelSetEvolution>
void
LevelSetEvolutionComputeIterationThreader<
  WhitakerSparseLevelSetImage<TOutput, VDimension>,
  ThreadedIteratorRangePartitioner<typename WhitakerSparseLevelSetImage<TOutput, VDimension>::LayerConstIterator>,
  TLevelSetEvolution>::AfterThreadedExecution()
{
  typename LevelSetContainerType::Iterator it = this->m_Associate->m_LevelSetContainerIteratorToProcessWhenThreading;
  const LevelSetIdentifierType             levelSetId = it->GetIdentifier();
  typename LevelSetEvolutionType::LevelSetLayerType * levelSetLayerUpdateBuffer =
    this->m_Associate->m_UpdateBuffer[levelSetId];

  const ThreadIdType numberOfWorkUnits = this->GetNumberOfWorkUnitsUsed();
  for (ThreadIdType ii = 0; ii < numberOfWorkUnits; ++ii)
  {
    auto pairIt = this->m_NodePairsPerThread[ii].begin();
    while (pairIt != this->m_NodePairsPerThread[ii].end())
    {
      levelSetLayerUpdateBuffer->insert(*pairIt);
      ++pairIt;
    }
  }
}

} // end namespace itk

#endif
