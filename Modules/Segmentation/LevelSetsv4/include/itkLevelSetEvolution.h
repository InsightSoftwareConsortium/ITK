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


#ifndef __itkLevelSetEvolution_h
#define __itkLevelSetEvolution_h

#include "itkLevelSetEvolutionBase.h"
#include "itkLevelSetDenseImageBase.h"

#include "itkWhitakerSparseLevelSetImage.h"
#include "itkUpdateWhitakerSparseLevelSet.h"

#include "itkShiSparseLevelSetImage.h"
#include "itkUpdateShiSparseLevelSet.h"

#include "itkMalcolmSparseLevelSetImage.h"
#include "itkUpdateMalcolmSparseLevelSet.h"

namespace itk
{
/**
 *  \class LevelSetEvolution
 *  \brief Class for iterating and evolving the level-set function
 *
 *  \tparam TEquationContainer Container holding the system of level set of equations
 *  \tparam TLevelSet Level-set function representation (e.g. dense, sparse)
 *
 *   \ingroup ITKLevelSetsv4
 */
template< class TEquationContainer, class TLevelSet >
class LevelSetEvolution
{};

template< class TEquationContainer, class TImage >
class LevelSetEvolution<  TEquationContainer,
                          LevelSetDenseImageBase< TImage > > :
  public LevelSetEvolutionBase< TEquationContainer, LevelSetDenseImageBase< TImage > >
{
public:
  typedef LevelSetDenseImageBase< TImage > LevelSetType;

  typedef LevelSetEvolution                                         Self;
  typedef SmartPointer< Self >                                      Pointer;
  typedef SmartPointer< const Self >                                ConstPointer;
  typedef LevelSetEvolutionBase< TEquationContainer, LevelSetType > Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( LevelSetEvolution, LevelSetEvolutionBase );

  typedef typename Superclass::EquationContainerType    EquationContainerType;
  typedef typename Superclass::EquationContainerPointer EquationContainerPointer;
  typedef typename Superclass::TermContainerType        TermContainerType;
  typedef typename Superclass::TermContainerPointer     TermContainerPointer;

  typedef typename Superclass::TermType     TermType;
  typedef typename Superclass::TermPointer  TermPointer;

  typedef typename Superclass::InputImageType         InputImageType;
  typedef typename Superclass::InputImagePixelType    InputImagePixelType;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;
  typedef typename Superclass::InputImageRegionType   InputImageRegionType;
  typedef typename Superclass::InputPixelRealType     InputPixelRealType;

  itkStaticConstMacro ( ImageDimension, unsigned int, Superclass::ImageDimension );

  typedef typename Superclass::LevelSetContainerType    LevelSetContainerType;
  typedef typename Superclass::LevelSetContainerPointer LevelSetContainerPointer;

  typedef typename Superclass::LevelSetIdentifierType             LevelSetIdentifierType;
  typedef typename Superclass::LevelSetContainerConstIteratorType LevelSetContainerConstIteratorType;
  typedef typename Superclass::LevelSetContainerIteratorType      LevelSetContainerIteratorType;

  typedef typename LevelSetType::ImageType        LevelSetImageType;
  typedef typename LevelSetImageType::Pointer     LevelSetImagePointer;

  typedef typename Superclass::LevelSetPointer        LevelSetPointer;
  typedef typename Superclass::LevelSetOutputType     LevelSetOutputType;
  typedef typename Superclass::LevelSetOutputRealType LevelSetOutputRealType;
  typedef typename Superclass::LevelSetDataType       LevelSetDataType;


  typedef typename Superclass::IdListType                   IdListType;
  typedef typename Superclass::IdListIterator               IdListIterator;
  typedef typename Superclass::IdListImageType              IdListImageType;
  typedef typename Superclass::CacheImageType               CacheImageType;
  typedef typename Superclass::DomainMapImageFilterType     DomainMapImageFilterType;
  typedef typename Superclass::DomainMapImageFilterPointer  DomainMapImageFilterPointer;

  typedef typename Superclass::StoppingCriterionType    StoppingCriterionType;
  typedef typename Superclass::StoppingCriterionPointer StoppingCriterionPointer;

  typedef BinaryThresholdImageFilter< LevelSetImageType, LevelSetImageType >  ThresholdFilterType;
  typedef typename ThresholdFilterType::Pointer                               ThresholdFilterPointer;

  typedef SignedMaurerDistanceMapImageFilter< LevelSetImageType, LevelSetImageType >  MaurerType;
  typedef typename MaurerType::Pointer                                                MaurerPointer;

  typedef ImageRegionIteratorWithIndex< LevelSetImageType > LevelSetImageIteratorType;

  typedef ImageRegionConstIteratorWithIndex< LevelSetImageType > LevelSetImageConstIteratorType;

  typedef ImageRegionConstIteratorWithIndex< InputImageType > InputImageConstIteratorType;

protected:
  LevelSetEvolution();
  ~LevelSetEvolution();

  LevelSetContainerPointer    m_UpdateBuffer;

  /** Initialize the update buffers for all level sets to hold the updates of
   *  equations in each iteration */
  void AllocateUpdateBuffer();

  /** Run the iterative loops of calculating levelset function updates until
   *  the stopping criterion is satisfied */
  virtual void Evolve();

  /** Computer the update at each pixel and store in the update buffer */
  void ComputeIteration();

  /** Compute the time-step for the next iteration */
  void ComputeTimeStepForNextIteration();

  /** Update the levelset by 1 iteration from the computed updates */
  virtual void UpdateLevelSets();

  /** Update the equations at the end of 1 iteration */
  void UpdateEquations();

  /** Reinitialize the level set functions to a signed distance function */
  void Reinitialize();
};


template< class TEquationContainer, typename TOutput, unsigned int VDimension >
class LevelSetEvolution<
    TEquationContainer,
    WhitakerSparseLevelSetImage< TOutput, VDimension > > :
  public LevelSetEvolutionBase<
    TEquationContainer,
    WhitakerSparseLevelSetImage< TOutput, VDimension > >
{
public:
  typedef WhitakerSparseLevelSetImage< TOutput, VDimension > LevelSetType;

  typedef LevelSetEvolution                                         Self;
  typedef SmartPointer< Self >                                      Pointer;
  typedef SmartPointer< const Self >                                ConstPointer;
  typedef LevelSetEvolutionBase< TEquationContainer, LevelSetType > Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( LevelSetEvolution, LevelSetEvolutionBase );

  typedef typename Superclass::EquationContainerType    EquationContainerType;
  typedef typename Superclass::EquationContainerPointer EquationContainerPointer;
  typedef typename Superclass::TermContainerType        TermContainerType;
  typedef typename Superclass::TermContainerPointer     TermContainerPointer;

  typedef typename Superclass::TermType     TermType;
  typedef typename Superclass::TermPointer  TermPointer;

  typedef typename Superclass::InputImageType         InputImageType;
  typedef typename Superclass::InputImagePixelType    InputImagePixelType;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;
  typedef typename Superclass::InputImageRegionType   InputImageRegionType;
  typedef typename Superclass::InputPixelRealType     InputPixelRealType;

  itkStaticConstMacro ( ImageDimension, unsigned int, Superclass::ImageDimension );

  typedef typename Superclass::LevelSetContainerType    LevelSetContainerType;
  typedef typename Superclass::LevelSetContainerPointer LevelSetContainerPointer;

  typedef typename Superclass::LevelSetIdentifierType             LevelSetIdentifierType;
  typedef typename Superclass::LevelSetContainerConstIteratorType LevelSetContainerConstIteratorType;
  typedef typename Superclass::LevelSetContainerIteratorType      LevelSetContainerIteratorType;

  typedef typename Superclass::LevelSetPointer        LevelSetPointer;
  typedef typename Superclass::LevelSetInputType      LevelSetInputType;
  typedef typename Superclass::LevelSetOutputType     LevelSetOutputType;
  typedef typename Superclass::LevelSetOutputRealType LevelSetOutputRealType;
  typedef typename Superclass::LevelSetDataType       LevelSetDataType;

  typedef typename LevelSetType::LayerType             LevelSetLayerType;
  typedef typename LevelSetType::LayerIterator         LevelSetLayerIterator;

  typedef typename LevelSetType::LabelMapType          LevelSetLabelMapType;
  typedef typename LevelSetType::LabelMapPointer       LevelSetLabelMapPointer;


  typedef typename Superclass::IdListType                   IdListType;
  typedef typename Superclass::IdListIterator               IdListIterator;
  typedef typename Superclass::IdListImageType              IdListImageType;
  typedef typename Superclass::CacheImageType               CacheImageType;
  typedef typename Superclass::DomainMapImageFilterType     DomainMapImageFilterType;
  typedef typename Superclass::DomainMapImageFilterPointer  DomainMapImageFilterPointer;

  typedef typename Superclass::StoppingCriterionType    StoppingCriterionType;
  typedef typename Superclass::StoppingCriterionPointer StoppingCriterionPointer;

  typedef ImageRegionConstIteratorWithIndex< InputImageType > InputImageConstIteratorType;

  typedef UpdateWhitakerSparseLevelSet< ImageDimension, LevelSetOutputType, EquationContainerType > UpdateLevelSetFilterType;
  typedef typename UpdateLevelSetFilterType::Pointer                                                UpdateLevelSetFilterPointer;

protected:
  LevelSetEvolution();
  ~LevelSetEvolution();

  typedef std::pair< LevelSetInputType, LevelSetOutputType > NodePairType;

  // For sparse case, the update buffer needs to be the size of the active layer
  std::map< IdentifierType, LevelSetLayerType* >  m_UpdateBuffer;

  /** Initialize the update buffers for all level sets to hold the updates of
   *  equations in each iteration */
  void AllocateUpdateBuffer();

  /** Run the iterative loops of calculating levelset function updates until
   *  the stopping criterion is satisfied */
  virtual void Evolve();

  /** Compute the update at each pixel and store in the update buffer */
  void ComputeIteration();

  /** Compute the time-step for the next iteration */
  void ComputeTimeStepForNextIteration();

  /** Update the levelset by 1 iteration from the computed updates */
  virtual void UpdateLevelSets();

  /** Update the equations at the end of 1 iteration */
  void UpdateEquations();

private:
  LevelSetEvolution( const Self& );
  void operator = ( const Self& );
};


// Shi
template< class TEquationContainer, unsigned int VDimension >
class LevelSetEvolution<
    TEquationContainer,
    ShiSparseLevelSetImage< VDimension > > :
public LevelSetEvolutionBase< TEquationContainer, ShiSparseLevelSetImage< VDimension > >
{
public:
  typedef ShiSparseLevelSetImage< VDimension > LevelSetType;

  typedef LevelSetEvolution                                         Self;
  typedef SmartPointer< Self >                                      Pointer;
  typedef SmartPointer< const Self >                                ConstPointer;
  typedef LevelSetEvolutionBase< TEquationContainer, LevelSetType > Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( LevelSetEvolution, LevelSetEvolutionBase );

  typedef typename Superclass::EquationContainerType    EquationContainerType;
  typedef typename Superclass::EquationContainerPointer EquationContainerPointer;
  typedef typename Superclass::TermContainerType        TermContainerType;
  typedef typename Superclass::TermContainerPointer     TermContainerPointer;

  typedef typename Superclass::TermType     TermType;
  typedef typename Superclass::TermPointer  TermPointer;

  typedef typename Superclass::InputImageType         InputImageType;
  typedef typename Superclass::InputImagePixelType    InputImagePixelType;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;
  typedef typename Superclass::InputImageRegionType   InputImageRegionType;
  typedef typename Superclass::InputPixelRealType     InputPixelRealType;

  itkStaticConstMacro ( ImageDimension, unsigned int, Superclass::ImageDimension );

  typedef typename Superclass::LevelSetContainerType    LevelSetContainerType;
  typedef typename Superclass::LevelSetContainerPointer LevelSetContainerPointer;

  typedef typename Superclass::LevelSetIdentifierType             LevelSetIdentifierType;
  typedef typename Superclass::LevelSetContainerConstIteratorType LevelSetContainerConstIteratorType;
  typedef typename Superclass::LevelSetContainerIteratorType      LevelSetContainerIteratorType;

  typedef typename Superclass::LevelSetPointer        LevelSetPointer;
  typedef typename Superclass::LevelSetInputType      LevelSetInputType;
  typedef typename Superclass::LevelSetOutputType     LevelSetOutputType;
  typedef typename Superclass::LevelSetOutputRealType LevelSetOutputRealType;
  typedef typename Superclass::LevelSetDataType       LevelSetDataType;

  typedef typename LevelSetType::LayerType             LevelSetLayerType;
  typedef typename LevelSetType::LayerIterator         LevelSetLayerIterator;

  typedef typename LevelSetType::LabelMapType          LevelSetLabelMapType;
  typedef typename LevelSetType::LabelMapPointer       LevelSetLabelMapPointer;


  typedef typename Superclass::IdListType                   IdListType;
  typedef typename Superclass::IdListIterator               IdListIterator;
  typedef typename Superclass::IdListImageType              IdListImageType;
  typedef typename Superclass::CacheImageType               CacheImageType;
  typedef typename Superclass::DomainMapImageFilterType     DomainMapImageFilterType;
  typedef typename Superclass::DomainMapImageFilterPointer  DomainMapImageFilterPointer;

  typedef typename Superclass::StoppingCriterionType    StoppingCriterionType;
  typedef typename Superclass::StoppingCriterionPointer StoppingCriterionPointer;

  typedef ImageRegionConstIteratorWithIndex< InputImageType > InputImageConstIteratorType;

  typedef UpdateShiSparseLevelSet< ImageDimension, EquationContainerType >  UpdateLevelSetFilterType;
  typedef typename UpdateLevelSetFilterType::Pointer                        UpdateLevelSetFilterPointer;

protected:
  LevelSetEvolution();
  ~LevelSetEvolution();

  /** Initialize the update buffers for all level sets to hold the updates of
   *  equations in each iteration */
  void AllocateUpdateBuffer();

  /** Run the iterative loops of calculating levelset function updates until
   *  the stopping criterion is satisfied */
  virtual void Evolve();

  /** Computer the update at each pixel and store in the update buffer */
  void ComputeIteration();

  /** Compute the time-step for the next iteration */
  void ComputeTimeStepForNextIteration();

  /** Update the levelset by 1 iteration from the computed updates */
  virtual void UpdateLevelSets();

  /** Update the equations at the end of 1 iteration */
  void UpdateEquations();

private:
  LevelSetEvolution( const Self& );
  void operator = ( const Self& );
};

// Malcolm
template< class TEquationContainer, unsigned int VDimension >
class LevelSetEvolution< TEquationContainer,
    MalcolmSparseLevelSetImage< VDimension > > :
public LevelSetEvolutionBase< TEquationContainer, MalcolmSparseLevelSetImage< VDimension > >
{
public:
  typedef MalcolmSparseLevelSetImage< VDimension > LevelSetType;

  typedef LevelSetEvolution                                         Self;
  typedef SmartPointer< Self >                                      Pointer;
  typedef SmartPointer< const Self >                                ConstPointer;
  typedef LevelSetEvolutionBase< TEquationContainer, LevelSetType > Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( LevelSetEvolution, LevelSetEvolutionBase );

  typedef typename Superclass::EquationContainerType    EquationContainerType;
  typedef typename Superclass::EquationContainerPointer EquationContainerPointer;
  typedef typename Superclass::TermContainerType        TermContainerType;
  typedef typename Superclass::TermContainerPointer     TermContainerPointer;

  typedef typename Superclass::TermType     TermType;
  typedef typename Superclass::TermPointer  TermPointer;

  typedef typename Superclass::InputImageType         InputImageType;
  typedef typename Superclass::InputImagePixelType    InputImagePixelType;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;
  typedef typename Superclass::InputImageRegionType   InputImageRegionType;
  typedef typename Superclass::InputPixelRealType     InputPixelRealType;

  itkStaticConstMacro ( ImageDimension, unsigned int, Superclass::ImageDimension );

  typedef typename Superclass::LevelSetContainerType    LevelSetContainerType;
  typedef typename Superclass::LevelSetContainerPointer LevelSetContainerPointer;

  typedef typename Superclass::LevelSetIdentifierType             LevelSetIdentifierType;
  typedef typename Superclass::LevelSetContainerConstIteratorType LevelSetContainerConstIteratorType;
  typedef typename Superclass::LevelSetContainerIteratorType      LevelSetContainerIteratorType;

  typedef typename Superclass::LevelSetPointer        LevelSetPointer;
  typedef typename Superclass::LevelSetInputType      LevelSetInputType;
  typedef typename Superclass::LevelSetOutputType     LevelSetOutputType;
  typedef typename Superclass::LevelSetOutputRealType LevelSetOutputRealType;
  typedef typename Superclass::LevelSetDataType       LevelSetDataType;

  typedef typename LevelSetType::LayerType             LevelSetLayerType;
  typedef typename LevelSetType::LayerIterator         LevelSetLayerIterator;

  typedef typename LevelSetType::LabelMapType          LevelSetLabelMapType;
  typedef typename LevelSetType::LabelMapPointer       LevelSetLabelMapPointer;


  typedef typename Superclass::IdListType                   IdListType;
  typedef typename Superclass::IdListIterator               IdListIterator;
  typedef typename Superclass::IdListImageType              IdListImageType;
  typedef typename Superclass::CacheImageType               CacheImageType;
  typedef typename Superclass::DomainMapImageFilterType     DomainMapImageFilterType;
  typedef typename Superclass::DomainMapImageFilterPointer  DomainMapImageFilterPointer;

  typedef typename Superclass::StoppingCriterionType    StoppingCriterionType;
  typedef typename Superclass::StoppingCriterionPointer StoppingCriterionPointer;

  typedef ImageRegionConstIteratorWithIndex< InputImageType > InputImageConstIteratorType;

  typedef UpdateMalcolmSparseLevelSet< ImageDimension, EquationContainerType > UpdateLevelSetFilterType;
  typedef typename UpdateLevelSetFilterType::Pointer UpdateLevelSetFilterPointer;

protected:
  LevelSetEvolution();
  ~LevelSetEvolution();

  /** Initialize the update buffers for all level sets to hold the updates of
   *  equations in each iteration */
  void AllocateUpdateBuffer();

  /** Computer the update at each pixel and store in the update buffer */
  void ComputeIteration();

  /** Compute the time-step for the next iteration */
  void ComputeTimeStepForNextIteration();

  /** Run the iterative loops of calculating levelset function updates until
   *  the stopping criterion is satisfied */
  virtual void Evolve();

  void UpdateLevelSets();

  void UpdateEquations();

private:
  LevelSetEvolution( const Self& ); // purposely not implemented
  void operator = ( const Self& );  // purposely not implemented
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetEvolution.hxx"
#endif

#endif // __itkLevelSetEvolution_h
