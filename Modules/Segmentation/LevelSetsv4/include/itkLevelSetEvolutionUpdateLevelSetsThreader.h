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
#ifndef itkLevelSetEvolutionUpdateLevelSetsThreader_h
#define itkLevelSetEvolutionUpdateLevelSetsThreader_h

#include "itkCompensatedSummation.h"
#include "itkDomainThreader.h"
#include "itkLevelSetDenseImage.h"
#include "itkThreadedImageRegionPartitioner.h"

namespace itk
{

/** \class LevelSetEvolutionUpdateLevelSetsThreader
 * \brief Threade the UpdateLevelSets method.
 *
 * Thread the \c UpdateLevelSets method of the LevelSetEvolution class.
 *
 * \ingroup ITKLevelSetsv4
 */
template< typename TLevelSet, typename TDomainPartitioner, typename TLevelSetEvolution >
class ITK_TEMPLATE_EXPORT LevelSetEvolutionUpdateLevelSetsThreader
{};

// For dense image level set.
template< typename TImage, typename TLevelSetEvolution >
class ITK_TEMPLATE_EXPORT LevelSetEvolutionUpdateLevelSetsThreader< LevelSetDenseImage< TImage >, ThreadedImageRegionPartitioner< TImage::ImageDimension >, TLevelSetEvolution >
  : public DomainThreader< ThreadedImageRegionPartitioner< TImage::ImageDimension >, TLevelSetEvolution >
{
public:
  /** Standard class typedefs. */
  typedef LevelSetEvolutionUpdateLevelSetsThreader                                                       Self;
  typedef DomainThreader< ThreadedImageRegionPartitioner< TImage::ImageDimension >, TLevelSetEvolution > Superclass;
  typedef SmartPointer< Self >                                                                           Pointer;
  typedef SmartPointer< const Self >                                                                     ConstPointer;

  /** Run time type information. */
  itkTypeMacro( LevelSetEvolutionUpdateLevelSetsThreader, DomainThreader );

  /** Standard New macro. */
  itkNewMacro( Self );

  /** Superclass types. */
  typedef typename Superclass::DomainType    DomainType;
  typedef typename Superclass::AssociateType AssociateType;

  /** Types of the associate class. */
  typedef TLevelSetEvolution                                     LevelSetEvolutionType;
  typedef typename LevelSetEvolutionType::LevelSetContainerType  LevelSetContainerType;
  typedef typename LevelSetEvolutionType::LevelSetType           LevelSetType;
  typedef typename LevelSetEvolutionType::LevelSetImageType      LevelSetImageType;
  typedef typename LevelSetEvolutionType::LevelSetOutputRealType LevelSetOutputRealType;

protected:
  LevelSetEvolutionUpdateLevelSetsThreader();

  virtual void BeforeThreadedExecution() ITK_OVERRIDE;

  virtual void ThreadedExecution( const DomainType & imageSubRegion, const ThreadIdType threadId ) ITK_OVERRIDE;

  virtual void AfterThreadedExecution() ITK_OVERRIDE;

  typedef CompensatedSummation< LevelSetOutputRealType > RMSChangeAccumulatorType;
  typedef std::vector< RMSChangeAccumulatorType > RMSChangeAccumulatorPerThreadType;

  RMSChangeAccumulatorPerThreadType m_RMSChangeAccumulatorPerThread;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEvolutionUpdateLevelSetsThreader);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetEvolutionUpdateLevelSetsThreader.hxx"
#endif

#endif
