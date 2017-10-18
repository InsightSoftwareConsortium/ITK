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
template< typename TEquationContainer, typename TLevelSet >
class ITK_TEMPLATE_EXPORT LevelSetEvolutionBase : public Object
{
public:
  typedef LevelSetEvolutionBase      Self;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef Object                     Superclass;

  /** Run-time type information */
  itkTypeMacro( LevelSetEvolutionBase, Object );

  typedef TEquationContainer                      EquationContainerType;
  typedef typename EquationContainerType::Pointer EquationContainerPointer;
  typedef typename EquationContainerType::TermContainerType
                                                  TermContainerType;
  typedef typename TermContainerType::Pointer     TermContainerPointer;

  typedef typename TermContainerType::TermType TermType;
  typedef typename TermType::Pointer           TermPointer;

  typedef typename TermContainerType::InputImageType InputImageType;
  typedef typename InputImageType::PixelType         InputImagePixelType;
  typedef typename InputImageType::ConstPointer      InputImageConstPointer;
  typedef typename InputImageType::RegionType        InputImageRegionType;
  typedef typename NumericTraits< InputImagePixelType >::RealType
                                                     InputPixelRealType;

  itkStaticConstMacro ( ImageDimension, unsigned int, InputImageType::ImageDimension );

  typedef typename TermContainerType::LevelSetContainerType       LevelSetContainerType;

  typedef typename LevelSetContainerType::LevelSetIdentifierType  LevelSetIdentifierType;

  typedef TLevelSet                               LevelSetType;
  typedef typename LevelSetType::InputType        LevelSetInputType;
  typedef typename LevelSetType::OutputType       LevelSetOutputType;
  typedef typename LevelSetType::OutputRealType   LevelSetOutputRealType;
  typedef typename LevelSetType::LevelSetDataType LevelSetDataType;

  typedef typename LevelSetContainerType::IdListType          IdListType;
  typedef typename LevelSetContainerType::IdListIterator      IdListIterator;
  typedef typename LevelSetContainerType::IdListConstIterator IdListConstIterator;
  typedef typename LevelSetContainerType::IdListImageType     IdListImageType;
  typedef typename LevelSetContainerType::CacheImageType      CacheImageType;

  typedef typename LevelSetContainerType::DomainMapImageFilterType DomainMapImageFilterType;

  typedef LevelSetEvolutionStoppingCriterion< LevelSetContainerType >
                                                  StoppingCriterionType;
  typedef typename StoppingCriterionType::Pointer StoppingCriterionPointer;

  itkSetObjectMacro( LevelSetContainer, LevelSetContainerType );
  itkGetModifiableObjectMacro(LevelSetContainer, LevelSetContainerType );

  /** Set/Get the value of alpha for computing the time-step using CFL conditions */
  itkSetMacro( Alpha, LevelSetOutputRealType );
  itkGetMacro( Alpha, LevelSetOutputRealType );

  /** Set a user-specified value of the time-step */
  void SetTimeStep( const LevelSetOutputRealType& iDt );

  /** Set/Get the equation container for updating all the level sets */
  itkSetObjectMacro( EquationContainer, EquationContainerType );
  itkGetModifiableObjectMacro( EquationContainer, EquationContainerType );

  /** Set/Get the Stopping Criterion */
  itkSetObjectMacro( StoppingCriterion, StoppingCriterionType );
  itkGetModifiableObjectMacro( StoppingCriterion, StoppingCriterionType );

  /** Get the number of iterations that have occurred. */
  itkGetConstMacro( NumberOfIterations, IdentifierType );

  /** Update the filter by computing the output level function
   * by calling Evolve() once the instantiation of necessary variables
   * is verified */
  void Update();

protected:
  LevelSetEvolutionBase();

  virtual ~LevelSetEvolutionBase() ITK_OVERRIDE;

  void CheckSetUp();

  /** Initialize the iteration by computing parameters in the terms of the level set equation */
  void InitializeIteration();

  /** Run the iterative loops of calculating levelset function updates until
   *  the stopping criterion is satisfied.  Calls AllocateUpdateBuffer,
   *  ComputeIteration, ComputeTimeStepForNextIteration, UpdateLevelSets,
   *  UpdateEquations.  */
  void Evolve();

  /** Initialize the update buffers for all level sets to hold the updates of
   *  equations in each iteration. No-op by default. */
  virtual void AllocateUpdateBuffer();

  /** Computer the update at each pixel and store in the update buffer. No-op by
   * default. */
  virtual void ComputeIteration();

  /** Compute the time-step for the next iteration. No-op by default. */
  virtual void ComputeTimeStepForNextIteration();

  virtual void UpdateLevelSets() = 0;

  virtual void UpdateEquations() = 0;

  StoppingCriterionPointer    m_StoppingCriterion;

  EquationContainerPointer                 m_EquationContainer;
  typename LevelSetContainerType::Pointer  m_LevelSetContainer;

  LevelSetOutputRealType      m_Alpha;
  LevelSetOutputRealType      m_Dt;
  LevelSetOutputRealType      m_RMSChangeAccumulator;
  bool                        m_UserGloballyDefinedTimeStep;
  IdentifierType              m_NumberOfIterations;

  /** Helper members for threading. */
  typename LevelSetContainerType::Iterator m_LevelSetContainerIteratorToProcessWhenThreading;
  typename LevelSetContainerType::Iterator m_LevelSetUpdateContainerIteratorToProcessWhenThreading;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEvolutionBase);
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetEvolutionBase.hxx"
#endif

#endif // itkLevelSetEvolutionBase_h
