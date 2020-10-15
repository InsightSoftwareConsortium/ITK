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
#ifndef itkLevelSetEvolutionUpdateLevelSetsThreader_h
#define itkLevelSetEvolutionUpdateLevelSetsThreader_h

#include "itkCompensatedSummation.h"
#include "itkDomainThreader.h"
#include "itkLevelSetDenseImage.h"
#include "itkThreadedImageRegionPartitioner.h"

namespace itk
{

/**
 *\class LevelSetEvolutionUpdateLevelSetsThreader
 * \brief Threade the UpdateLevelSets method.
 *
 * Thread the \c UpdateLevelSets method of the LevelSetEvolution class.
 *
 * \ingroup ITKLevelSetsv4
 */
template <typename TLevelSet, typename TDomainPartitioner, typename TLevelSetEvolution>
class ITK_TEMPLATE_EXPORT LevelSetEvolutionUpdateLevelSetsThreader
{};

// For dense image level set.
template <typename TImage, typename TLevelSetEvolution>
class ITK_TEMPLATE_EXPORT
  LevelSetEvolutionUpdateLevelSetsThreader<LevelSetDenseImage<TImage>,
                                           ThreadedImageRegionPartitioner<TImage::ImageDimension>,
                                           TLevelSetEvolution>
  : public DomainThreader<ThreadedImageRegionPartitioner<TImage::ImageDimension>, TLevelSetEvolution>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetEvolutionUpdateLevelSetsThreader);

  /** Standard class type aliases. */
  using Self = LevelSetEvolutionUpdateLevelSetsThreader;
  using Superclass = DomainThreader<ThreadedImageRegionPartitioner<TImage::ImageDimension>, TLevelSetEvolution>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run time type information. */
  itkTypeMacro(LevelSetEvolutionUpdateLevelSetsThreader, DomainThreader);

  /** Standard New macro. */
  itkNewMacro(Self);

  /** Superclass types. */
  using DomainType = typename Superclass::DomainType;
  using AssociateType = typename Superclass::AssociateType;

  /** Types of the associate class. */
  using LevelSetEvolutionType = TLevelSetEvolution;
  using LevelSetContainerType = typename LevelSetEvolutionType::LevelSetContainerType;
  using LevelSetType = typename LevelSetEvolutionType::LevelSetType;
  using LevelSetImageType = typename LevelSetEvolutionType::LevelSetImageType;
  using LevelSetOutputRealType = typename LevelSetEvolutionType::LevelSetOutputRealType;

protected:
  LevelSetEvolutionUpdateLevelSetsThreader() = default;

  void
  BeforeThreadedExecution() override;

  void
  ThreadedExecution(const DomainType & imageSubRegion, const ThreadIdType threadId) override;

  void
  AfterThreadedExecution() override;

  using RMSChangeAccumulatorType = CompensatedSummation<LevelSetOutputRealType>;
  using RMSChangeAccumulatorPerThreadType = std::vector<RMSChangeAccumulatorType>;

  RMSChangeAccumulatorPerThreadType m_RMSChangeAccumulatorPerThread;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetEvolutionUpdateLevelSetsThreader.hxx"
#endif

#endif
