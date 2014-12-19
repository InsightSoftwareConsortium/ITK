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
#ifndef itkLevelSetEvolutionUpdateLevelSetsThreader_hxx
#define itkLevelSetEvolutionUpdateLevelSetsThreader_hxx

#include "itkLevelSetEvolutionUpdateLevelSetsThreader.h"

#include "itkImageRegionIterator.h"

namespace itk
{

template< typename TImage, typename TLevelSetEvolution >
LevelSetEvolutionUpdateLevelSetsThreader< LevelSetDenseImage< TImage >, ThreadedImageRegionPartitioner< TImage::ImageDimension >, TLevelSetEvolution >
::LevelSetEvolutionUpdateLevelSetsThreader()
{
}

template< typename TImage, typename TLevelSetEvolution >
void
LevelSetEvolutionUpdateLevelSetsThreader< LevelSetDenseImage< TImage >, ThreadedImageRegionPartitioner< TImage::ImageDimension >, TLevelSetEvolution >
::BeforeThreadedExecution()
{
  this->m_RMSChangeAccumulatorPerThread.resize( this->GetNumberOfThreadsUsed() );

  for( ThreadIdType ii = 0, maxThreads = this->GetNumberOfThreadsUsed(); ii < maxThreads; ++ii )
    {
    this->m_RMSChangeAccumulatorPerThread[ii].ResetToZero();
    }
}

template< typename TImage, typename TLevelSetEvolution >
void
LevelSetEvolutionUpdateLevelSetsThreader< LevelSetDenseImage< TImage >, ThreadedImageRegionPartitioner< TImage::ImageDimension >, TLevelSetEvolution >
::ThreadedExecution( const DomainType & imageSubRegion,
                     const ThreadIdType threadId )
{
  typename LevelSetContainerType::Iterator levelSetContainerIt = this->m_Associate->m_LevelSetContainerIteratorToProcessWhenThreading;
  typename LevelSetContainerType::ConstIterator levelSetUpdateContainerIt = this->m_Associate->m_LevelSetUpdateContainerIteratorToProcessWhenThreading;

  // This is for single level set analysis, so we only process the first level
  // set.
  typename LevelSetType::Pointer levelSet = levelSetContainerIt->GetLevelSet();
  typename LevelSetType::Pointer levelSetUpdate = levelSetUpdateContainerIt->GetLevelSet();

  typename LevelSetImageType::Pointer levelSetImage = levelSet->GetModifiableImage();
  typename LevelSetImageType::ConstPointer levelSetUpdateImage = levelSetUpdate->GetImage();

  ImageRegionIterator< LevelSetImageType > levelSetImageIt( levelSetImage, imageSubRegion );
  ImageRegionConstIterator< LevelSetImageType > levelSetUpdateImageIt( levelSetUpdateImage, imageSubRegion );
  levelSetImageIt.GoToBegin();
  levelSetUpdateImageIt.GoToBegin();

  while( !levelSetImageIt.IsAtEnd() )
    {
    const LevelSetOutputRealType & p = this->m_Associate->m_Dt * levelSetUpdateImageIt.Get();
    levelSetImageIt.Set( levelSetImageIt.Get() + p );

    this->m_RMSChangeAccumulatorPerThread[threadId] += p*p;

    ++levelSetImageIt;
    ++levelSetUpdateImageIt;
    }
}

template< typename TImage, typename TLevelSetEvolution >
void
LevelSetEvolutionUpdateLevelSetsThreader< LevelSetDenseImage< TImage >, ThreadedImageRegionPartitioner< TImage::ImageDimension >, TLevelSetEvolution >
::AfterThreadedExecution()
{
  for( ThreadIdType ii = 0, maxThreads = this->GetNumberOfThreadsUsed(); ii < maxThreads; ++ii )
    {
    this->m_Associate->m_RMSChangeAccumulator += this->m_RMSChangeAccumulatorPerThread[ii].GetSum();
    }
}

} // end namespace itk

#endif
