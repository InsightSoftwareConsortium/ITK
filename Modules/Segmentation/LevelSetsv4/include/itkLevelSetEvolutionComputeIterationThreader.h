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
#ifndef __itkLevelSetEvolutionComputeIterationThreader_h
#define __itkLevelSetEvolutionComputeIterationThreader_h

#include "itkDomainThreader.h"
#include "itkThreadedImageRegionPartitioner.h"
#include "itkThreadedIteratorRangePartitioner.h"

#include "itkLevelSetDenseImage.h"
#include "itkWhitakerSparseLevelSetImage.h"

namespace itk
{

/** \class LevelSetEvolutionComputeIterationThreader
 * \brief Thread the ComputeIteration method.
 *
 * Thread the \c ComputeIteration method of the LevelSetEvolution class
 *
 * \ingroup ITKLevelSetsv4
 */
template< class TLevelSet, class TDomainPartitioner, class TLevelSetEvolution >
class LevelSetEvolutionComputeIterationThreader
{};

// For dense image level set split by putting part of the level set region in
// each thread.
template< class TImage, class TLevelSetEvolution >
class LevelSetEvolutionComputeIterationThreader< LevelSetDenseImage< TImage >, ThreadedImageRegionPartitioner< TImage::ImageDimension >, TLevelSetEvolution >
  : public DomainThreader< ThreadedImageRegionPartitioner< TImage::ImageDimension >, TLevelSetEvolution >
{
public:
  /** Standard class typedefs. */
  typedef LevelSetEvolutionComputeIterationThreader                                                      Self;
  typedef DomainThreader< ThreadedImageRegionPartitioner< TImage::ImageDimension >, TLevelSetEvolution > Superclass;
  typedef SmartPointer< Self >                                                                           Pointer;
  typedef SmartPointer< const Self >                                                                     ConstPointer;

  /** Run time type information. */
  itkTypeMacro( LevelSetEvolutionComputeIterationThreader, DomainThreader );

  /** Standard New macro. */
  itkNewMacro( Self );

  /** Superclass types. */
  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  /** Types of the associate class. */
  typedef TLevelSetEvolution                                     LevelSetEvolutionType;
  typedef typename LevelSetEvolutionType::IdListType             IdListType;
  typedef typename LevelSetEvolutionType::IdListConstIterator    IdListConstIterator;
  typedef typename LevelSetEvolutionType::InputImageType         InputImageType;
  typedef typename LevelSetEvolutionType::LevelSetType           LevelSetType;
  typedef typename LevelSetType::IndexType                       IndexType;
  typedef typename LevelSetType::RegionType                      RegionType;
  typedef typename LevelSetType::OffsetType                      OffsetType;
  typedef typename LevelSetEvolutionType::LevelSetImageType      LevelSetImageType;
  typedef typename LevelSetEvolutionType::LevelSetDataType       LevelSetDataType;
  typedef typename LevelSetEvolutionType::LevelSetOutputRealType LevelSetOutputRealType;
  typedef typename LevelSetEvolutionType::LevelSetContainerType  LevelSetContainerType;
  typedef typename LevelSetEvolutionType::EquationContainerType  EquationContainerType;
  typedef typename LevelSetEvolutionType::TermContainerType      TermContainerType;

protected:
  LevelSetEvolutionComputeIterationThreader();

  virtual void ThreadedExecution( const DomainType & imageSubRegion, const ThreadIdType threadId );

private:
  LevelSetEvolutionComputeIterationThreader( const Self & ); // purposely not implemented
  void operator=( const Self & ); // purposely not implemented
};

// For dense image level set split by putting a level set domain in each thread.
template< class TImage, class TLevelSetEvolution >
class LevelSetEvolutionComputeIterationThreader< LevelSetDenseImage< TImage >,
      ThreadedIteratorRangePartitioner< typename TLevelSetEvolution::DomainMapImageFilterType::DomainMapType::const_iterator >, TLevelSetEvolution >
  : public DomainThreader< ThreadedIteratorRangePartitioner< typename TLevelSetEvolution::DomainMapImageFilterType::DomainMapType::const_iterator >, TLevelSetEvolution >
{
public:
  typedef typename TLevelSetEvolution::DomainMapImageFilterType::DomainMapType::const_iterator DomainMapConstIteratorType;
  typedef ThreadedIteratorRangePartitioner< DomainMapConstIteratorType >                       ThreadedDomainMapPartitionerType;

  /** Standard class typedefs. */
  typedef LevelSetEvolutionComputeIterationThreader                              Self;
  typedef DomainThreader< ThreadedDomainMapPartitionerType, TLevelSetEvolution > Superclass;
  typedef SmartPointer< Self >                                                   Pointer;
  typedef SmartPointer< const Self >                                             ConstPointer;

  /** Run time type information. */
  itkTypeMacro( LevelSetEvolutionComputeIterationThreader, DomainThreader );

  /** Standard New macro. */
  itkNewMacro( Self );

  /** Superclass types. */
  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  /** Types of the associate class. */
  typedef TLevelSetEvolution                                     LevelSetEvolutionType;
  typedef typename LevelSetEvolutionType::IdListType             IdListType;
  typedef typename LevelSetEvolutionType::IdListConstIterator    IdListConstIterator;
  typedef typename LevelSetEvolutionType::InputImageType         InputImageType;
  typedef typename LevelSetEvolutionType::LevelSetType           LevelSetType;
  typedef typename LevelSetType::IndexType                       IndexType;
  typedef typename LevelSetType::RegionType                      RegionType;
  typedef typename LevelSetType::OffsetType                      OffsetType;
  typedef typename LevelSetEvolutionType::LevelSetImageType      LevelSetImageType;
  typedef typename LevelSetEvolutionType::LevelSetDataType       LevelSetDataType;
  typedef typename LevelSetEvolutionType::LevelSetOutputRealType LevelSetOutputRealType;
  typedef typename LevelSetEvolutionType::LevelSetContainerType  LevelSetContainerType;
  typedef typename LevelSetEvolutionType::EquationContainerType  EquationContainerType;
  typedef typename LevelSetEvolutionType::TermContainerType      TermContainerType;

protected:
  LevelSetEvolutionComputeIterationThreader();

  virtual void ThreadedExecution( const DomainType & imageSubRegion, const ThreadIdType threadId );

private:
  LevelSetEvolutionComputeIterationThreader( const Self & ); // purposely not implemented
  void operator=( const Self & ); // purposely not implemented
};

// For Whitaker sparse level set split by putting part of the level set in each
// thread.
template< class TOutput, unsigned int VDimension, class TLevelSetEvolution >
class LevelSetEvolutionComputeIterationThreader<
      WhitakerSparseLevelSetImage< TOutput, VDimension >,
      ThreadedIteratorRangePartitioner< typename WhitakerSparseLevelSetImage< TOutput, VDimension >::LayerConstIterator >,
      TLevelSetEvolution
      >
  : public DomainThreader< ThreadedIteratorRangePartitioner< typename WhitakerSparseLevelSetImage< TOutput, VDimension >::LayerConstIterator >, TLevelSetEvolution >
{
public:
  /** Standard class typedefs. */
  typedef LevelSetEvolutionComputeIterationThreader                                                                                                                 Self;
  typedef DomainThreader< ThreadedIteratorRangePartitioner< typename WhitakerSparseLevelSetImage< TOutput, VDimension >::LayerConstIterator >, TLevelSetEvolution > Superclass;
  typedef SmartPointer< Self >                                                                                                                                      Pointer;
  typedef SmartPointer< const Self >                                                                                                                                ConstPointer;

  /** Run time type information. */
  itkTypeMacro( LevelSetEvolutionComputeIterationThreader, DomainThreader );

  /** Standard New macro. */
  itkNewMacro( Self );

  /** Superclass types. */
  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  /** Types of the associate class. */
  typedef TLevelSetEvolution                                     LevelSetEvolutionType;
  typedef typename LevelSetEvolutionType::LevelSetType           LevelSetType;
  typedef typename LevelSetType::IndexType                       IndexType;
  typedef typename LevelSetType::RegionType                      RegionType;
  typedef typename LevelSetType::OffsetType                      OffsetType;
  typedef typename LevelSetEvolutionType::LevelSetContainerType  LevelSetContainerType;
  typedef typename LevelSetEvolutionType::LevelSetIdentifierType LevelSetIdentifierType;
  typedef typename LevelSetEvolutionType::LevelSetInputType      LevelSetInputType;
  typedef typename LevelSetEvolutionType::LevelSetOutputType     LevelSetOutputType;
  typedef typename LevelSetEvolutionType::LevelSetDataType       LevelSetDataType;
  typedef typename LevelSetEvolutionType::TermContainerType      TermContainerType;
  typedef typename LevelSetEvolutionType::NodePairType           NodePairType;

protected:
  LevelSetEvolutionComputeIterationThreader();

  virtual void BeforeThreadedExecution();

  virtual void ThreadedExecution( const DomainType & iteratorSubRange, const ThreadIdType threadId );

  virtual void AfterThreadedExecution();

  typedef std::vector< std::vector< NodePairType > > NodePairsPerThreadType;
  NodePairsPerThreadType m_NodePairsPerThread;

private:
  LevelSetEvolutionComputeIterationThreader( const Self & ); // purposely not implemented
  void operator=( const Self & ); // purposely not implemented
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetEvolutionComputeIterationThreader.hxx"
#endif

#endif
