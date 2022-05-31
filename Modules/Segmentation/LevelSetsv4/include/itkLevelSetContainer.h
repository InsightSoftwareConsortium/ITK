/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
template <typename TIdentifier, typename TLevelSet>
class ITK_TEMPLATE_EXPORT LevelSetContainer : public LevelSetContainerBase<TIdentifier, TLevelSet>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetContainer);

  using Self = LevelSetContainer;
  using Superclass = LevelSetContainerBase<TIdentifier, TLevelSet>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  itkTypeMacro(LevelSetContainer, LevelSetContainerBase);

  using typename Superclass::LevelSetIdentifierType;

  using typename Superclass::LevelSetType;
  using typename Superclass::LevelSetPointer;
  using typename Superclass::InputIndexType;
  using OutputPixelType = typename Superclass::OutputType;
  using typename Superclass::OutputRealType;
  using typename Superclass::GradientType;
  using typename Superclass::HessianType;

  using typename Superclass::LevelSetContainerType;
  using typename Superclass::LevelSetContainerConstIteratorType;
  using typename Superclass::LevelSetContainerIteratorType;

  using typename Superclass::HeavisideType;
  using typename Superclass::HeavisideConstPointer;

  static constexpr unsigned int Dimension = LevelSetType::Dimension;

  using typename Superclass::IdListType;
  using typename Superclass::IdListIterator;
  using typename Superclass::IdListImageType;
  using typename Superclass::CacheImageType;
  using typename Superclass::DomainMapImageFilterType;

  using typename Superclass::DomainMapImageFilterPointer;
  using typename Superclass::LevelSetDomainType;
  using typename Superclass::DomainIteratorType;

protected:
  LevelSetContainer() = default;
  ~LevelSetContainer() override = default;
};

/**
 *  \brief Container class for dense level sets
 *  \ingroup ITKLevelSetsv4
 */
template <typename TIdentifier, typename TImage>
class LevelSetContainer<TIdentifier, LevelSetDenseImage<TImage>>
  : public LevelSetContainerBase<TIdentifier, LevelSetDenseImage<TImage>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetContainer);

  using LevelSetType = LevelSetDenseImage<TImage>;

  using Self = LevelSetContainer;
  using Superclass = LevelSetContainerBase<TIdentifier, LevelSetType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  itkTypeMacro(LevelSetContainer, LevelSetContainerBase);

  using typename Superclass::LevelSetIdentifierType;

  using typename Superclass::LevelSetPointer;
  using typename Superclass::InputIndexType;
  using OutputPixelType = typename Superclass::OutputType;
  using typename Superclass::OutputRealType;
  using typename Superclass::GradientType;
  using typename Superclass::HessianType;

  using typename Superclass::LevelSetContainerType;
  using typename Superclass::LevelSetContainerConstIteratorType;
  using typename Superclass::LevelSetContainerIteratorType;

  using typename Superclass::HeavisideType;
  using typename Superclass::HeavisideConstPointer;

  static constexpr unsigned int Dimension = LevelSetType::Dimension;

  using typename Superclass::IdListType;
  using typename Superclass::IdListIterator;
  using typename Superclass::IdListImageType;
  using typename Superclass::CacheImageType;
  using typename Superclass::DomainMapImageFilterType;

  using typename Superclass::DomainMapImageFilterPointer;
  using typename Superclass::LevelSetDomainType;
  using typename Superclass::DomainIteratorType;

  using LevelSetImageType = typename LevelSetType::ImageType;
  using LevelSetImagePointer = typename LevelSetImageType::Pointer;

  /** Compute information from data object and/or allocate new level set image */
  void
  CopyInformationAndAllocate(const Self * iOther, const bool iAllocate)
  {
    LevelSetContainerType              internalContainer = iOther->GetContainer();
    LevelSetContainerConstIteratorType it = internalContainer.begin();

    LevelSetContainerType newContainer;

    while (it != internalContainer.end())
    {
      if (iAllocate)
      {
        LevelSetPointer temp_ls = LevelSetType::New();

        LevelSetImagePointer      image = LevelSetImageType::New();
        const LevelSetImageType * otherImage = (it->second)->GetImage();

        image->CopyInformation(otherImage);
        image->SetBufferedRegion(otherImage->GetBufferedRegion());
        image->SetRequestedRegion(otherImage->GetRequestedRegion());
        image->SetLargestPossibleRegion(otherImage->GetLargestPossibleRegion());
        image->Allocate();
        image->FillBuffer(NumericTraits<OutputPixelType>::ZeroValue());

        temp_ls->SetImage(image);
        newContainer[it->first] = temp_ls;
        newContainer[it->first]->SetDomainOffset((it->second)->GetDomainOffset());
      }
      else
      {
        LevelSetPointer temp_ls;
        newContainer[it->first] = temp_ls;
      }
      ++it;
    }

    this->SetContainer(newContainer);
  }

protected:
  LevelSetContainer() = default;
  ~LevelSetContainer() override = default;
};

} // namespace itk

#endif // itkLevelSetContainer_h
