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


#ifndef itkLevelSetEvolutionBase_h
#define itkLevelSetEvolutionBase_h

#include <list>

#include "itkImage.h"
#include "itkDiscreteLevelSetImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkSignedMaurerDistanceMapImageFilter.h"
#include "itkNumericTraits.h"
#include "itkLevelSetEvolutionStoppingCriterion.h"

namespace itk
{
/**
 *  \class LevelSetEvolutionBase
 *  \brief Class for iterating and evolving the dense level-set function
 *
 *  \tparam TEquationContainer Container holding the system of level set of equations
 *   \ingroup ITKLevelSetsv4
 */
template <typename TEquationContainer, typename TLevelSet>
class ITK_TEMPLATE_EXPORT LevelSetEvolutionBase : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetEvolutionBase);

  using Self = LevelSetEvolutionBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = Object;

  /** Run-time type information */
  itkTypeMacro(LevelSetEvolutionBase, Object);

  using EquationContainerType = TEquationContainer;
  using EquationContainerPointer = typename EquationContainerType::Pointer;
  using TermContainerType = typename EquationContainerType::TermContainerType;
  using TermContainerPointer = typename TermContainerType::Pointer;

  using TermType = typename TermContainerType::TermType;
  using TermPointer = typename TermType::Pointer;

  using InputImageType = typename TermContainerType::InputImageType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputPixelRealType = typename NumericTraits<InputImagePixelType>::RealType;

  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

  using LevelSetContainerType = typename TermContainerType::LevelSetContainerType;

  using LevelSetIdentifierType = typename LevelSetContainerType::LevelSetIdentifierType;

  using LevelSetType = TLevelSet;
  using LevelSetInputType = typename LevelSetType::InputType;
  using LevelSetOutputType = typename LevelSetType::OutputType;
  using LevelSetOutputRealType = typename LevelSetType::OutputRealType;
  using LevelSetDataType = typename LevelSetType::LevelSetDataType;

  using IdListType = typename LevelSetContainerType::IdListType;
  using IdListIterator = typename LevelSetContainerType::IdListIterator;
  using IdListConstIterator = typename LevelSetContainerType::IdListConstIterator;
  using IdListImageType = typename LevelSetContainerType::IdListImageType;
  using CacheImageType = typename LevelSetContainerType::CacheImageType;

  using DomainMapImageFilterType = typename LevelSetContainerType::DomainMapImageFilterType;

  using StoppingCriterionType = LevelSetEvolutionStoppingCriterion<LevelSetContainerType>;
  using StoppingCriterionPointer = typename StoppingCriterionType::Pointer;

  itkSetObjectMacro(LevelSetContainer, LevelSetContainerType);
  itkGetModifiableObjectMacro(LevelSetContainer, LevelSetContainerType);

  /** Set/Get the value of alpha for computing the time-step using CFL conditions */
  itkSetMacro(Alpha, LevelSetOutputRealType);
  itkGetMacro(Alpha, LevelSetOutputRealType);

  /** Set a user-specified value of the time-step */
  void
  SetTimeStep(const LevelSetOutputRealType & iDt);

  /** Set/Get the equation container for updating all the level sets */
  itkSetObjectMacro(EquationContainer, EquationContainerType);
  itkGetModifiableObjectMacro(EquationContainer, EquationContainerType);

  /** Set/Get the Stopping Criterion */
  itkSetObjectMacro(StoppingCriterion, StoppingCriterionType);
  itkGetModifiableObjectMacro(StoppingCriterion, StoppingCriterionType);

  /** Get the number of iterations that have occurred. */
  itkGetConstMacro(NumberOfIterations, IdentifierType);

  /** Update the filter by computing the output level function
   * by calling Evolve() once the instantiation of necessary variables
   * is verified */
  void
  Update();

protected:
  LevelSetEvolutionBase();

  ~LevelSetEvolutionBase() override = default;

  void
  CheckSetUp();

  /** Initialize the iteration by computing parameters in the terms of the level set equation */
  void
  InitializeIteration();

  /** Run the iterative loops of calculating levelset function updates until
   *  the stopping criterion is satisfied.  Calls AllocateUpdateBuffer,
   *  ComputeIteration, ComputeTimeStepForNextIteration, UpdateLevelSets,
   *  UpdateEquations.  */
  void
  Evolve();

  /** Initialize the update buffers for all level sets to hold the updates of
   *  equations in each iteration. No-op by default. */
  virtual void
  AllocateUpdateBuffer();

  /** Computer the update at each pixel and store in the update buffer. No-op by
   * default. */
  virtual void
  ComputeIteration();

  /** Compute the time-step for the next iteration. No-op by default. */
  virtual void
  ComputeTimeStepForNextIteration();

  virtual void
  UpdateLevelSets() = 0;

  virtual void
  UpdateEquations() = 0;

  StoppingCriterionPointer m_StoppingCriterion;

  EquationContainerPointer                m_EquationContainer;
  typename LevelSetContainerType::Pointer m_LevelSetContainer;

  LevelSetOutputRealType m_Alpha;
  LevelSetOutputRealType m_Dt;
  LevelSetOutputRealType m_RMSChangeAccumulator;
  bool                   m_UserGloballyDefinedTimeStep;
  IdentifierType         m_NumberOfIterations;

  /** Helper members for threading. */
  typename LevelSetContainerType::Iterator m_LevelSetContainerIteratorToProcessWhenThreading;
  typename LevelSetContainerType::Iterator m_LevelSetUpdateContainerIteratorToProcessWhenThreading;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetEvolutionBase.hxx"
#endif

#endif // itkLevelSetEvolutionBase_h
