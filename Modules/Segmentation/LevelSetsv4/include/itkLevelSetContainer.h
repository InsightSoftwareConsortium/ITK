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

#ifndef itkLevelSetContainer_h
#define itkLevelSetContainer_h

#include "itkLevelSetContainerBase.h"

#include "itkLevelSetDenseImage.h"


namespace itk
{
/**
 *  \class LevelSetContainer
 *  \brief Container of Level-Sets
 *
 *  \tparam TIdentifier Input level set id type
 *  \tparam TLevelSet Level Set Type
 *
 *  \ingroup ITKLevelSetsv4
 */
template< typename TIdentifier, typename TLevelSet >
class LevelSetContainer :
public LevelSetContainerBase< TIdentifier, TLevelSet >
{
public:

  typedef LevelSetContainer                               Self;
  typedef LevelSetContainerBase< TIdentifier, TLevelSet > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through object factory */
  itkNewMacro ( Self );

  itkTypeMacro ( LevelSetContainer, LevelSetContainerBase );

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

  typedef typename Superclass::HeavisideType          HeavisideType;
  typedef typename Superclass::HeavisideConstPointer  HeavisideConstPointer;

  itkStaticConstMacro ( Dimension, unsigned int, LevelSetType::Dimension );

  typedef typename Superclass::IdListType               IdListType;
  typedef typename Superclass::IdListIterator           IdListIterator;
  typedef typename Superclass::IdListImageType          IdListImageType;
  typedef typename Superclass::CacheImageType           CacheImageType;
  typedef typename Superclass::DomainMapImageFilterType DomainMapImageFilterType;

  typedef typename Superclass::DomainMapImageFilterPointer  DomainMapImageFilterPointer;
  typedef typename Superclass::LevelSetDomainType           LevelSetDomainType;
  typedef typename Superclass::DomainIteratorType           DomainIteratorType;

protected:
  LevelSetContainer() {}
  ~LevelSetContainer() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetContainer);
};

/**
 *  \brief Container class for dense level sets
 *  \ingroup ITKLevelSetsv4
 */
template< typename TIdentifier, typename TImage >
class LevelSetContainer< TIdentifier, LevelSetDenseImage< TImage > > :
public LevelSetContainerBase< TIdentifier, LevelSetDenseImage< TImage > >
{
public:
  typedef LevelSetDenseImage< TImage >  LevelSetType;

  typedef LevelSetContainer                                   Self;
  typedef LevelSetContainerBase< TIdentifier, LevelSetType >  Superclass;
  typedef SmartPointer< Self >                                Pointer;
  typedef SmartPointer< const Self >                          ConstPointer;

  /** Method for creation through object factory */
  itkNewMacro ( Self );

  itkTypeMacro ( LevelSetContainer, LevelSetContainerBase );

  typedef typename Superclass::LevelSetIdentifierType LevelSetIdentifierType;

  typedef typename Superclass::LevelSetPointer    LevelSetPointer;
  typedef typename Superclass::InputIndexType     InputIndexType;
  typedef typename Superclass::OutputType         OutputPixelType;
  typedef typename Superclass::OutputRealType     OutputRealType;
  typedef typename Superclass::GradientType       GradientType;
  typedef typename Superclass::HessianType        HessianType;

  typedef typename Superclass::LevelSetContainerType              LevelSetContainerType;
  typedef typename Superclass::LevelSetContainerConstIteratorType LevelSetContainerConstIteratorType;
  typedef typename Superclass::LevelSetContainerIteratorType      LevelSetContainerIteratorType;

  typedef typename Superclass::HeavisideType          HeavisideType;
  typedef typename Superclass::HeavisideConstPointer  HeavisideConstPointer;

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
  void CopyInformationAndAllocate( const Self * iOther, const bool & iAllocate )
    {
    LevelSetContainerType internalContainer = iOther->GetContainer();
    LevelSetContainerConstIteratorType it = internalContainer.begin();

    LevelSetContainerType newContainer;

    while( it != internalContainer.end() )
      {
      if( iAllocate )
        {
        LevelSetPointer temp_ls = LevelSetType::New();

        LevelSetImagePointer image = LevelSetImageType::New();
        const LevelSetImageType * otherImage = (it->second)->GetImage();

        image->CopyInformation( otherImage );
        image->SetBufferedRegion( otherImage->GetBufferedRegion() );
        image->SetRequestedRegion( otherImage->GetRequestedRegion() );
        image->SetLargestPossibleRegion( otherImage->GetLargestPossibleRegion() );
        image->Allocate();
        image->FillBuffer( NumericTraits< OutputPixelType >::ZeroValue() );

        temp_ls->SetImage( image );
        newContainer[ it->first ] = temp_ls;
        newContainer[ it->first ]->SetDomainOffset( (it->second)->GetDomainOffset() );
        }
      else
        {
        LevelSetPointer temp_ls;
        newContainer[ it->first ] = temp_ls;
        }
      ++it;
      }

    this->SetContainer( newContainer );
    }

protected:
  LevelSetContainer() {}
  ~LevelSetContainer() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetContainer);
};

}

#endif // itkLevelSetContainer_h
