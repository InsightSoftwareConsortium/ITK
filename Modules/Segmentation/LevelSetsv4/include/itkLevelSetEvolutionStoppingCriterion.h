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

#ifndef itkLevelSetEvolutionStoppingCriterion_h
#define itkLevelSetEvolutionStoppingCriterion_h

#include "itkStoppingCriterionBase.h"
#include "itkNumericTraits.h"
#include "itkIntTypes.h"

namespace itk
{
/**
 *\class LevelSetEvolutionStoppingCriterion
 \ingroup ITKLevelSetsv4
 */
template <typename TLevelSetContainer>
class ITK_TEMPLATE_EXPORT LevelSetEvolutionStoppingCriterion : public StoppingCriterionBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEvolutionStoppingCriterion);

  using Self = LevelSetEvolutionStoppingCriterion;
  using Superclass = StoppingCriterionBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LevelSetEvolutionStoppingCriterion, StoppingCriterionBase);

  using LevelSetContainerType = TLevelSetContainer;
  using LevelSetContainerPointer = typename LevelSetContainerType::Pointer;

  using LevelSetIdentifierType = typename LevelSetContainerType::LevelSetIdentifierType;

  using LevelSetType = typename LevelSetContainerType::LevelSetType;
  using LevelSetPointer = typename LevelSetContainerType::LevelSetPointer;

  using InputIndexType = typename LevelSetContainerType::InputIndexType;
  using OutputType = typename LevelSetContainerType::OutputType;
  using OutputRealType = typename LevelSetContainerType::OutputRealType;
  using GradientType = typename LevelSetContainerType::GradientType;
  using HessianType = typename LevelSetContainerType::HessianType;

  using HeavisideType = typename LevelSetContainerType::HeavisideType;
  using HeavisidePointer = typename LevelSetContainerType::HeavisideType;

  using IterationIdType = IdentifierType;

  itkSetObjectMacro(LevelSetContainer, LevelSetContainerType);
  itkGetModifiableObjectMacro(LevelSetContainer, LevelSetContainerType);

  itkSetMacro(NumberOfIterations, IterationIdType);
  itkGetMacro(NumberOfIterations, IterationIdType);

  itkSetMacro(CurrentIteration, IterationIdType);
  itkGetMacro(CurrentIteration, IterationIdType);

  itkSetMacro(RMSChangeAccumulator, OutputRealType);
  itkGetMacro(RMSChangeAccumulator, OutputRealType);

protected:
  /** Constructor */
  LevelSetEvolutionStoppingCriterion();

  /** Destructor */
  ~LevelSetEvolutionStoppingCriterion() override = default;

  LevelSetContainerPointer m_LevelSetContainer;
  OutputRealType           m_RMSChangeAccumulator;
  IterationIdType          m_NumberOfIterations;
  IterationIdType          m_CurrentIteration;
};
} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetEvolutionStoppingCriterion.hxx"
#endif
#endif
