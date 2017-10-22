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
#ifndef itkLevelSetEvolution_h
#define itkLevelSetEvolution_h

#include "itkLevelSetEvolutionBase.h"
#include "itkLevelSetDenseImage.h"

#include "itkWhitakerSparseLevelSetImage.h"
#include "itkUpdateWhitakerSparseLevelSet.h"

#include "itkShiSparseLevelSetImage.h"
#include "itkUpdateShiSparseLevelSet.h"

#include "itkMalcolmSparseLevelSetImage.h"
#include "itkUpdateMalcolmSparseLevelSet.h"

#include "itkLevelSetEvolutionComputeIterationThreader.h"
#include "itkLevelSetEvolutionUpdateLevelSetsThreader.h"

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
template< typename TEquationContainer, typename TLevelSet >
class ITK_TEMPLATE_EXPORT LevelSetEvolution
{};

template< typename TEquationContainer, typename TImage >
class ITK_TEMPLATE_EXPORT LevelSetEvolution<  TEquationContainer,
                          LevelSetDenseImage< TImage > > :
  public LevelSetEvolutionBase< TEquationContainer, LevelSetDenseImage< TImage > >
{
public:
  typedef LevelSetDenseImage< TImage > LevelSetType;

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

  typedef typename Superclass::LevelSetIdentifierType   LevelSetIdentifierType;

  typedef typename LevelSetType::ImageType        LevelSetImageType;

  typedef typename Superclass::LevelSetOutputType     LevelSetOutputType;
  typedef typename Superclass::LevelSetOutputRealType LevelSetOutputRealType;
  typedef typename Superclass::LevelSetDataType       LevelSetDataType;

  typedef typename Superclass::IdListType                   IdListType;
  typedef typename Superclass::IdListIterator               IdListIterator;
  typedef typename Superclass::IdListConstIterator          IdListConstIterator;
  typedef typename Superclass::IdListImageType              IdListImageType;
  typedef typename Superclass::CacheImageType               CacheImageType;
  typedef typename Superclass::DomainMapImageFilterType     DomainMapImageFilterType;

  typedef typename Superclass::StoppingCriterionType    StoppingCriterionType;
  typedef typename Superclass::StoppingCriterionPointer StoppingCriterionPointer;

  typedef BinaryThresholdImageFilter< LevelSetImageType, LevelSetImageType >  ThresholdFilterType;
  typedef typename ThresholdFilterType::Pointer                               ThresholdFilterPointer;

  typedef SignedMaurerDistanceMapImageFilter< LevelSetImageType, LevelSetImageType >  MaurerType;
  typedef typename MaurerType::Pointer                                                MaurerPointer;

  typedef ImageRegionIteratorWithIndex< LevelSetImageType > LevelSetImageIteratorType;

  typedef ImageRegionConstIteratorWithIndex< LevelSetImageType > LevelSetImageConstIteratorType;

  typedef ImageRegionConstIteratorWithIndex< InputImageType > InputImageConstIteratorType;

  /** Set the maximum number of threads to be used. */
  void SetNumberOfThreads( const ThreadIdType threads );
  /** Set the maximum number of threads to be used. */
  ThreadIdType GetNumberOfThreads() const;

protected:
  LevelSetEvolution();
  ~LevelSetEvolution() ITK_OVERRIDE;

  /** Initialize the update buffers for all level sets to hold the updates of
   *  equations in each iteration */
  virtual void AllocateUpdateBuffer() ITK_OVERRIDE;

  /** Computer the update at each pixel and store in the update buffer */
  virtual void ComputeIteration() ITK_OVERRIDE;

  /** Compute the time-step for the next iteration */
  virtual void ComputeTimeStepForNextIteration() ITK_OVERRIDE;

  /** Update the levelset by 1 iteration from the computed updates */
  virtual void UpdateLevelSets() ITK_OVERRIDE;

  /** Update the equations at the end of 1 iteration */
  virtual void UpdateEquations() ITK_OVERRIDE;

  /** Reinitialize the level set functions to a signed distance function */
  void ReinitializeToSignedDistance();

  typename LevelSetContainerType::Pointer    m_UpdateBuffer;

  friend class LevelSetEvolutionComputeIterationThreader< LevelSetType, ThreadedImageRegionPartitioner< TImage::ImageDimension >, Self >;
  typedef LevelSetEvolutionComputeIterationThreader< LevelSetType, ThreadedImageRegionPartitioner< TImage::ImageDimension >, Self > SplitLevelSetComputeIterationThreaderType;
  typename SplitLevelSetComputeIterationThreaderType::Pointer m_SplitLevelSetComputeIterationThreader;

  typedef typename DomainMapImageFilterType::DomainMapType::const_iterator DomainMapConstIteratorType;
  typedef ThreadedIteratorRangePartitioner< DomainMapConstIteratorType > ThreadedDomainMapPartitionerType;
  friend class LevelSetEvolutionComputeIterationThreader< LevelSetType, ThreadedDomainMapPartitionerType, Self >;
  typedef LevelSetEvolutionComputeIterationThreader< LevelSetType, ThreadedDomainMapPartitionerType, Self > SplitDomainMapComputeIterationThreaderType;
  typename SplitDomainMapComputeIterationThreaderType::Pointer m_SplitDomainMapComputeIterationThreader;

  friend class LevelSetEvolutionUpdateLevelSetsThreader< LevelSetType, ThreadedImageRegionPartitioner< TImage::ImageDimension >, Self >;
  typedef LevelSetEvolutionUpdateLevelSetsThreader< LevelSetType, ThreadedImageRegionPartitioner< TImage::ImageDimension >, Self > SplitLevelSetUpdateLevelSetsThreaderType;
  typename SplitLevelSetUpdateLevelSetsThreaderType::Pointer m_SplitLevelSetUpdateLevelSetsThreader;

  /** Helper variable for threading. */
  const IdListType * m_IdListToProcessWhenThreading;
};


template< typename TEquationContainer, typename TOutput, unsigned int VDimension >
class ITK_TEMPLATE_EXPORT LevelSetEvolution< TEquationContainer, WhitakerSparseLevelSetImage< TOutput, VDimension > > :
  public LevelSetEvolutionBase< TEquationContainer, WhitakerSparseLevelSetImage< TOutput, VDimension > >
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

  typedef typename Superclass::LevelSetContainerType              LevelSetContainerType;
  typedef typename Superclass::LevelSetIdentifierType             LevelSetIdentifierType;

  typedef typename Superclass::LevelSetInputType      LevelSetInputType;
  typedef typename Superclass::LevelSetOutputType     LevelSetOutputType;
  typedef typename Superclass::LevelSetOutputRealType LevelSetOutputRealType;
  typedef typename Superclass::LevelSetDataType       LevelSetDataType;

  typedef typename LevelSetType::LayerType             LevelSetLayerType;

  typedef typename LevelSetType::LabelMapType          LevelSetLabelMapType;
  typedef typename LevelSetType::LabelMapPointer       LevelSetLabelMapPointer;


  typedef typename Superclass::IdListType                   IdListType;
  typedef typename Superclass::IdListIterator               IdListIterator;
  typedef typename Superclass::IdListImageType              IdListImageType;
  typedef typename Superclass::CacheImageType               CacheImageType;
  typedef typename Superclass::DomainMapImageFilterType     DomainMapImageFilterType;

  typedef typename Superclass::StoppingCriterionType    StoppingCriterionType;
  typedef typename Superclass::StoppingCriterionPointer StoppingCriterionPointer;

  typedef ImageRegionConstIteratorWithIndex< InputImageType > InputImageConstIteratorType;

  typedef UpdateWhitakerSparseLevelSet< ImageDimension, LevelSetOutputType, EquationContainerType > UpdateLevelSetFilterType;
  typedef typename UpdateLevelSetFilterType::Pointer                                                UpdateLevelSetFilterPointer;

  /** Set the maximum number of threads to be used. */
  void SetNumberOfThreads( const ThreadIdType threads );
  /** Set the maximum number of threads to be used. */
  ThreadIdType GetNumberOfThreads() const;

protected:
  LevelSetEvolution();
  ~LevelSetEvolution() ITK_OVERRIDE;

  typedef std::pair< LevelSetInputType, LevelSetOutputType > NodePairType;

  // For sparse case, the update buffer needs to be the size of the active layer
  std::map< IdentifierType, LevelSetLayerType* >  m_UpdateBuffer;

  /** Initialize the update buffers for all level sets to hold the updates of
   *  equations in each iteration */
  virtual void AllocateUpdateBuffer() ITK_OVERRIDE;

  /** Compute the update at each pixel and store in the update buffer */
  virtual void ComputeIteration() ITK_OVERRIDE;

  /** Compute the time-step for the next iteration */
  virtual void ComputeTimeStepForNextIteration() ITK_OVERRIDE;

  /** Update the levelset by 1 iteration from the computed updates */
  virtual void UpdateLevelSets() ITK_OVERRIDE;

  /** Update the equations at the end of 1 iteration */
  virtual void UpdateEquations() ITK_OVERRIDE;

  typedef ThreadedIteratorRangePartitioner< typename LevelSetType::LayerConstIterator > SplitLevelSetPartitionerType;
  friend class LevelSetEvolutionComputeIterationThreader< LevelSetType, SplitLevelSetPartitionerType, Self >;
  typedef LevelSetEvolutionComputeIterationThreader< LevelSetType, SplitLevelSetPartitionerType, Self > SplitLevelSetComputeIterationThreaderType;
  typename SplitLevelSetComputeIterationThreaderType::Pointer m_SplitLevelSetComputeIterationThreader;

private:
  LevelSetEvolution( const Self& );
  void operator = ( const Self& );
};


// Shi
template< typename TEquationContainer, unsigned int VDimension >
class ITK_TEMPLATE_EXPORT LevelSetEvolution<
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
  typedef typename Superclass::LevelSetIdentifierType   LevelSetIdentifierType;

  typedef typename Superclass::LevelSetInputType      LevelSetInputType;
  typedef typename Superclass::LevelSetOutputType     LevelSetOutputType;
  typedef typename Superclass::LevelSetOutputRealType LevelSetOutputRealType;
  typedef typename Superclass::LevelSetDataType       LevelSetDataType;

  typedef typename LevelSetType::LayerType             LevelSetLayerType;

  typedef typename LevelSetType::LabelMapType          LevelSetLabelMapType;
  typedef typename LevelSetType::LabelMapPointer       LevelSetLabelMapPointer;


  typedef typename Superclass::IdListType                   IdListType;
  typedef typename Superclass::IdListIterator               IdListIterator;
  typedef typename Superclass::IdListImageType              IdListImageType;
  typedef typename Superclass::CacheImageType               CacheImageType;
  typedef typename Superclass::DomainMapImageFilterType     DomainMapImageFilterType;

  typedef typename Superclass::StoppingCriterionType    StoppingCriterionType;
  typedef typename Superclass::StoppingCriterionPointer StoppingCriterionPointer;

  typedef ImageRegionConstIteratorWithIndex< InputImageType > InputImageConstIteratorType;

  typedef UpdateShiSparseLevelSet< ImageDimension, EquationContainerType >  UpdateLevelSetFilterType;
  typedef typename UpdateLevelSetFilterType::Pointer                        UpdateLevelSetFilterPointer;

protected:
  LevelSetEvolution();
  ~LevelSetEvolution() ITK_OVERRIDE;

  /** Update the levelset by 1 iteration from the computed updates */
  virtual void UpdateLevelSets() ITK_OVERRIDE;

  /** Update the equations at the end of 1 iteration */
  virtual void UpdateEquations() ITK_OVERRIDE;

private:
  LevelSetEvolution( const Self& );
  void operator = ( const Self& );
};

// Malcolm
template< typename TEquationContainer, unsigned int VDimension >
class ITK_TEMPLATE_EXPORT LevelSetEvolution< TEquationContainer,
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
  typedef typename Superclass::LevelSetIdentifierType   LevelSetIdentifierType;

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

  typedef typename Superclass::StoppingCriterionType    StoppingCriterionType;
  typedef typename Superclass::StoppingCriterionPointer StoppingCriterionPointer;

  typedef ImageRegionConstIteratorWithIndex< InputImageType > InputImageConstIteratorType;

  typedef UpdateMalcolmSparseLevelSet< ImageDimension, EquationContainerType > UpdateLevelSetFilterType;
  typedef typename UpdateLevelSetFilterType::Pointer UpdateLevelSetFilterPointer;

protected:
  LevelSetEvolution();
  virtual ~LevelSetEvolution() ITK_OVERRIDE;

  virtual void UpdateLevelSets() ITK_OVERRIDE;

  virtual void UpdateEquations() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEvolution);
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetEvolution.hxx"
#endif

#endif // itkLevelSetEvolution_h
