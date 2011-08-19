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

#ifndef __itkDenseLevelSetContainer_h
#define __itkDenseLevelSetContainer_h

#include "itkLevelSetContainerBase.h"

namespace itk
{
/**
 *  \class DenseLevelSetContainer
 *  \brief Container class for dense level sets
 *
 *  \tparam TIdentifier Input level set id type
 *  \tparam TLevelSet Input level set image type
 *  \ingroup ITKLevelSetsv4
 */
template< class TIdentifier,
          class TLevelSet >
class DenseLevelSetContainer :
    public LevelSetContainerBase< TIdentifier, TLevelSet >
{
public:
  typedef DenseLevelSetContainer                          Self;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;
  typedef LevelSetContainerBase< TIdentifier, TLevelSet > Superclass;

  /** Method for creation through object factory */
  itkNewMacro ( Self );

  /** Run-time type information */
  itkTypeMacro ( DenseLevelSetContainer, LevelSetContainerBase );

  typedef typename Superclass::LevelSetIdentifierType LevelSetIdentifierType;

  typedef typename Superclass::LevelSetType       LevelSetType;
  typedef typename Superclass::LevelSetPointer    LevelSetPointer;
  typedef typename Superclass::InputIndexType     InputIndexType;
  typedef typename Superclass::OutputType         OutputPixelType;
  typedef typename Superclass::OutputRealType     OutputRealType;
  typedef typename Superclass::GradientType       GradientType;
  typedef typename Superclass::HessianType        HessianType;

  typedef typename Superclass::LevelSetContainerType              LevelSetContainerType;
  typedef typename Superclass::LevelSetContainerConstIteratorType LevelSetContainerConstIteratorType;
  typedef typename Superclass::LevelSetContainerIteratorType      LevelSetContainerIteratorType;

  typedef typename Superclass::HeavisideType    HeavisideType;
  typedef typename Superclass::HeavisidePointer HeavisidePointer;

  itkStaticConstMacro ( Dimension, unsigned int, LevelSetType::Dimension );

  typedef typename Superclass::IdListType               IdListType;
  typedef typename Superclass::IdListIterator           IdListIterator;
  typedef typename Superclass::IdListImageType          IdListImageType;
  typedef typename Superclass::CacheImageType           CacheImageType;
  typedef typename Superclass::DomainMapImageFilterType DomainMapImageFilterType;

  typedef typename Superclass::DomainMapImageFilterPointer  DomainMapImageFilterPointer;
  typedef typename Superclass::LevelSetDomainType           LevelSetDomainType;
  typedef typename Superclass::DomainIteratorType           DomainIteratorType;

  typedef typename LevelSetType::ImageType    LevelSetImageType;
  typedef typename LevelSetImageType::Pointer LevelSetImagePointer;

  /** Compute information from data object and/or allocate new level set image */
  void CopyInformationAndAllocate( const Self * iOther, const bool & iAllocate );

protected:
  DenseLevelSetContainer();
  ~DenseLevelSetContainer();

private:
  DenseLevelSetContainer( const Self & ); // purposely not implemented
  void operator = ( const Self & ); // purposely not implemented

};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDenseLevelSetContainer.hxx"
#endif

#endif // __itkDenseLevelSetContainer_h
