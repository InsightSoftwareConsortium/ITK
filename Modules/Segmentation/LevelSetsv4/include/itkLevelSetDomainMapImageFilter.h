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

#ifndef itkLevelSetDomainMapImageFilter_h
#define itkLevelSetDomainMapImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include <list>
#include <utility>
#include <vector>

namespace itk
{
/**
  \class LevelSetDomainMapImageFilter
  \tparam TInputImage  Image where the pixel type is a container (e.g. std::list) of level set ids
  \tparam TOutputImage Image where the pixel type is an identifier integer to associate with each subdomain

  Every subdomain (image region) has a consistent set of level sets ids associated with every pixel.
  \ingroup ITKLevelSetsv4
*/
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT LevelSetDomainMapImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetDomainMapImageFilter);

  using Self = LevelSetDomainMapImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(LevelSetDomainMapImageFilter, ImageToImageFilter);

  using InputImageType = TInputImage;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImagePixelType = typename InputImageType::PixelType;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImageSizeType = typename InputImageType::SizeType;
  using InputImageSizeValueType = typename InputImageSizeType::SizeValueType;
  using InputImageIndexType = typename InputImageType::IndexType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageIndexType = typename OutputImageType::IndexType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  using InputConstIteratorType = ImageRegionConstIteratorWithIndex<InputImageType>;
  using InputIndexIteratorType = ImageRegionIteratorWithIndex<InputImageType>;
  using InputIteratorType = ImageRegionIterator<InputImageType>;

  using OutputConstIteratorType = ImageRegionConstIteratorWithIndex<OutputImageType>;
  using OutputIndexIteratorType = ImageRegionIteratorWithIndex<OutputImageType>;
  using OutputIteratorType = ImageRegionIterator<OutputImageType>;

  /**
   *\class LevelSetDomain
   * \brief Specifies an image region where an unique std::list of level sets Id's are defined.
   * \ingroup ITKLevelSetsv4 */
  class LevelSetDomain
  {
  public:
    LevelSetDomain() = default;

    LevelSetDomain(const InputImageRegionType & reg, InputImagePixelType iList)
      : m_Region(reg)
      , m_IdList(std::move(iList))
    {}

    const InputImageRegionType *
    GetRegion() const
    {
      return &(this->m_Region);
    }

    const InputImagePixelType *
    GetIdList() const
    {
      return &(this->m_IdList);
    }

  private:
    InputImageRegionType m_Region;
    InputImagePixelType  m_IdList;
  };

  /** Map from a integer identifier to the level set list image domain. */
  using DomainMapType = std::map<IdentifierType, LevelSetDomain>;

  /** Get a map from the identifier for the domains with consistent level set ids
   * * struct containing an (int, string, etc) identifier and the ImageRegion that
   * specifies the domain. */
  const DomainMapType &
  GetDomainMap() const;

protected:
  LevelSetDomainMapImageFilter();
  ~LevelSetDomainMapImageFilter() override = default;

  /** Computes a consistent region for the same set of overlapping
   * level set support. */
  InputImageRegionType
  ComputeConsistentRegion(const InputImageRegionType & subRegion) const;

  /** Identify image partitions where each partition has the same overlapping
   *  level set support */
  void
  GenerateData() override;

  /** Display */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  DomainMapType m_DomainMap;

  const InputImageType * m_InputImage;
  OutputImageType *      m_OutputImage;
};

} /* namespace itk */

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetDomainMapImageFilter.hxx"
#endif

#endif
