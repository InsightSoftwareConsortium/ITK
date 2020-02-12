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
#ifndef itkLevelSetDomainPartitionImage_h
#define itkLevelSetDomainPartitionImage_h

#include "itkLevelSetDomainPartitionBase.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
/**
 *\class LevelSetDomainPartitionImage
 *
 * \brief Helper class used to partition domain and efficiently compute overlap.
 * \ingroup ITKLevelSetsv4
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT LevelSetDomainPartitionImage : public LevelSetDomainPartitionBase<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetDomainPartitionImage);

  using Self = LevelSetDomainPartitionImage;
  using Superclass = LevelSetDomainPartitionBase<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  itkTypeMacro(LevelSetDomainPartitionImage, LevelSetDomainPartitionBase);

  using ImageType = TImage;
  using ImagePointer = typename ImageType::Pointer;
  using ImageConstPointer = typename ImageType::ConstPointer;
  using PixelType = typename ImageType::PixelType;
  using RegionType = typename ImageType::RegionType;
  using SizeType = typename ImageType::SizeType;
  using SizeValueType = typename SizeType::SizeValueType;
  using SpacingType = typename ImageType::SpacingType;
  using IndexType = typename ImageType::IndexType;
  using IndexValueType = typename IndexType::IndexValueType;
  using PointType = typename ImageType::PointType;

  using IdentifierListType = typename Superclass::IdentifierListType;

  using ListImageType = Image<IdentifierListType, ImageDimension>;
  using ListImagePointer = typename ListImageType::Pointer;
  using ListImageConstPointer = typename ListImageType::ConstPointer;
  using ListRegionType = typename ListImageType::RegionType;
  using ListSizeType = typename ListImageType::SizeType;
  using ListSizeValueType = typename ListSizeType::SizeValueType;
  using ListSpacingType = typename ListImageType::SpacingType;
  using ListIndexType = typename ListImageType::IndexType;
  using ListIndexValueType = typename ListIndexType::IndexValueType;
  using ListPointType = typename ListImageType::PointType;
  using ListIteratorType = ImageRegionIteratorWithIndex<ListImageType>;

  using LevelSetDomainRegionVectorType = std::vector<RegionType>;

  /** Set the input image that will be used to compute an image with the list
   * of level sets domain overlaps. */
  itkSetConstObjectMacro(Image, ImageType);
  itkGetConstObjectMacro(Image, ImageType);

  /** Get the image with the list of level set domains. */
  itkGetModifiableObjectMacro(ListDomain, ListImageType);

  void
  SetLevelSetDomainRegionVector(const LevelSetDomainRegionVectorType & domain);
  const LevelSetDomainRegionVectorType &
  GetLevelSetDomainRegionVector() const;

  /** Populate a list image with each pixel being a list of overlapping
   *  level set support at that pixel */
  void
  PopulateListDomain() override;

protected:
  LevelSetDomainPartitionImage() = default;
  ~LevelSetDomainPartitionImage() override = default;

  /** Allocate a list image with each pixel being a list of overlapping
   *  level set support at that pixel */
  void
  AllocateListDomain() override;

  ImageConstPointer              m_Image;
  ListImagePointer               m_ListDomain;
  LevelSetDomainRegionVectorType m_LevelSetDomainRegionVector;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetDomainPartitionImage.hxx"
#endif

#endif
