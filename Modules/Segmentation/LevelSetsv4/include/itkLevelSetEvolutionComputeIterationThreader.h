/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkLevelSetEvolutionComputeIterationThreader_h
#define itkLevelSetEvolutionComputeIterationThreader_h

#include "itkDomainThreader.h"
#include "itkThreadedImageRegionPartitioner.h"
#include "itkThreadedIteratorRangePartitioner.h"

#include "itkLevelSetDenseImage.h"
#include "itkWhitakerSparseLevelSetImage.h"

namespace itk
{

/**
 *\class LevelSetEvolutionComputeIterationThreader
 * \brief Thread the ComputeIteration method.
 *
 * Thread the \c ComputeIteration method of the LevelSetEvolution class
 *
 * \ingroup ITKLevelSetsv4
 */
template <typename TLevelSet, typename TDomainPartitioner, typename TLevelSetEvolution>
class ITK_TEMPLATE_EXPORT LevelSetEvolutionComputeIterationThreader
{};

// For dense image level set split by putting part of the level set region in
// each thread.
template <typename TImage, typename TLevelSetEvolution>
class ITK_TEMPLATE_EXPORT
  LevelSetEvolutionComputeIterationThreader<LevelSetDenseImage<TImage>,
                                            ThreadedImageRegionPartitioner<TImage::ImageDimension>,
                                            TLevelSetEvolution>
  : public DomainThreader<ThreadedImageRegionPartitioner<TImage::ImageDimension>, TLevelSetEvolution>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEvolutionComputeIterationThreader);

  /** Standard class type aliases. */
  using Self = LevelSetEvolutionComputeIterationThreader;
  using Superclass = DomainThreader<ThreadedImageRegionPartitioner<TImage::ImageDimension>, TLevelSetEvolution>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run time type information. */
  itkTypeMacro(LevelSetEvolutionComputeIterationThreader, DomainThreader);

  /** Standard New macro. */
  itkNewMacro(Self);

  /** Superclass types. */
  using DomainType = typename Superclass::DomainType;
  using AssociateType = typename Superclass::AssociateType;

  /** Types of the associate class. */
  using LevelSetEvolutionType = TLevelSetEvolution;
  using IdListType = typename LevelSetEvolutionType::IdListType;
  using IdListConstIterator = typename LevelSetEvolutionType::IdListConstIterator;
  using InputImageType = typename LevelSetEvolutionType::InputImageType;
  using LevelSetType = typename LevelSetEvolutionType::LevelSetType;
  using IndexType = typename LevelSetType::IndexType;
  using RegionType = typename LevelSetType::RegionType;
  using OffsetType = typename LevelSetType::OffsetType;
  using LevelSetImageType = typename LevelSetEvolutionType::LevelSetImageType;
  using LevelSetDataType = typename LevelSetEvolutionType::LevelSetDataType;
  using LevelSetOutputRealType = typename LevelSetEvolutionType::LevelSetOutputRealType;
  using LevelSetContainerType = typename LevelSetEvolutionType::LevelSetContainerType;
  using EquationContainerType = typename LevelSetEvolutionType::EquationContainerType;
  using TermContainerType = typename LevelSetEvolutionType::TermContainerType;

protected:
  LevelSetEvolutionComputeIterationThreader() = default;

  void
  ThreadedExecution(const DomainType & imageSubRegion, const ThreadIdType threadId) override;
};

// For dense image level set split by putting a level set domain in each thread.
template <typename TImage, typename TLevelSetEvolution>
class ITK_TEMPLATE_EXPORT LevelSetEvolutionComputeIterationThreader<
  LevelSetDenseImage<TImage>,
  ThreadedIteratorRangePartitioner<
    typename TLevelSetEvolution::DomainMapImageFilterType::DomainMapType::const_iterator>,
  TLevelSetEvolution>
  : public DomainThreader<ThreadedIteratorRangePartitioner<
                            typename TLevelSetEvolution::DomainMapImageFilterType::DomainMapType::const_iterator>,
                          TLevelSetEvolution>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEvolutionComputeIterationThreader);

  using DomainMapConstIteratorType =
    typename TLevelSetEvolution::DomainMapImageFilterType::DomainMapType::const_iterator;
  using ThreadedDomainMapPartitionerType = ThreadedIteratorRangePartitioner<DomainMapConstIteratorType>;

  /** Standard class type aliases. */
  using Self = LevelSetEvolutionComputeIterationThreader;
  using Superclass = DomainThreader<ThreadedDomainMapPartitionerType, TLevelSetEvolution>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run time type information. */
  itkTypeMacro(LevelSetEvolutionComputeIterationThreader, DomainThreader);

  /** Standard New macro. */
  itkNewMacro(Self);

  /** Superclass types. */
  using DomainType = typename Superclass::DomainType;
  using AssociateType = typename Superclass::AssociateType;

  /** Types of the associate class. */
  using LevelSetEvolutionType = TLevelSetEvolution;
  using IdListType = typename LevelSetEvolutionType::IdListType;
  using IdListConstIterator = typename LevelSetEvolutionType::IdListConstIterator;
  using InputImageType = typename LevelSetEvolutionType::InputImageType;
  using LevelSetType = typename LevelSetEvolutionType::LevelSetType;
  using IndexType = typename LevelSetType::IndexType;
  using RegionType = typename LevelSetType::RegionType;
  using OffsetType = typename LevelSetType::OffsetType;
  using LevelSetImageType = typename LevelSetEvolutionType::LevelSetImageType;
  using LevelSetDataType = typename LevelSetEvolutionType::LevelSetDataType;
  using LevelSetOutputRealType = typename LevelSetEvolutionType::LevelSetOutputRealType;
  using LevelSetContainerType = typename LevelSetEvolutionType::LevelSetContainerType;
  using EquationContainerType = typename LevelSetEvolutionType::EquationContainerType;
  using TermContainerType = typename LevelSetEvolutionType::TermContainerType;

protected:
  LevelSetEvolutionComputeIterationThreader() = default;

  void
  ThreadedExecution(const DomainType & imageSubRegion, const ThreadIdType threadId) override;
};

// For Whitaker sparse level set split by putting part of the level set in each
// thread.
template <typename TOutput, unsigned int VDimension, typename TLevelSetEvolution>
class ITK_TEMPLATE_EXPORT LevelSetEvolutionComputeIterationThreader<
  WhitakerSparseLevelSetImage<TOutput, VDimension>,
  ThreadedIteratorRangePartitioner<typename WhitakerSparseLevelSetImage<TOutput, VDimension>::LayerConstIterator>,
  TLevelSetEvolution>
  : public DomainThreader<
      ThreadedIteratorRangePartitioner<typename WhitakerSparseLevelSetImage<TOutput, VDimension>::LayerConstIterator>,
      TLevelSetEvolution>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEvolutionComputeIterationThreader);

  /** Standard class type aliases. */
  using Self = LevelSetEvolutionComputeIterationThreader;
  using Superclass = DomainThreader<
    ThreadedIteratorRangePartitioner<typename WhitakerSparseLevelSetImage<TOutput, VDimension>::LayerConstIterator>,
    TLevelSetEvolution>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run time type information. */
  itkTypeMacro(LevelSetEvolutionComputeIterationThreader, DomainThreader);

  /** Standard New macro. */
  itkNewMacro(Self);

  /** Superclass types. */
  using DomainType = typename Superclass::DomainType;
  using AssociateType = typename Superclass::AssociateType;

  /** Types of the associate class. */
  using LevelSetEvolutionType = TLevelSetEvolution;
  using LevelSetType = typename LevelSetEvolutionType::LevelSetType;
  using IndexType = typename LevelSetType::IndexType;
  using RegionType = typename LevelSetType::RegionType;
  using OffsetType = typename LevelSetType::OffsetType;
  using LevelSetContainerType = typename LevelSetEvolutionType::LevelSetContainerType;
  using LevelSetIdentifierType = typename LevelSetEvolutionType::LevelSetIdentifierType;
  using LevelSetInputType = typename LevelSetEvolutionType::LevelSetInputType;
  using LevelSetOutputType = typename LevelSetEvolutionType::LevelSetOutputType;
  using LevelSetDataType = typename LevelSetEvolutionType::LevelSetDataType;
  using TermContainerType = typename LevelSetEvolutionType::TermContainerType;
  using NodePairType = typename LevelSetEvolutionType::NodePairType;

protected:
  LevelSetEvolutionComputeIterationThreader() = default;

  void
  BeforeThreadedExecution() override;

  void
  ThreadedExecution(const DomainType & iteratorSubRange, const ThreadIdType threadId) override;

  void
  AfterThreadedExecution() override;

  using NodePairsPerThreadType = std::vector<std::vector<NodePairType>>;
  NodePairsPerThreadType m_NodePairsPerThread;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetEvolutionComputeIterationThreader.hxx"
#endif

#endif
