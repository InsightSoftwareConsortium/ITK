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
#ifndef itkMiniPipelineSeparableImageFilter_h
#define itkMiniPipelineSeparableImageFilter_h

#include "itkBoxImageFilter.h"

namespace itk
{
/**
 * \class MiniPipelineSeparableImageFilter
 * \brief A separable filter for filter which are using radius
 *
 * This filter takes a non separable implementation of a neighborhood
 * filter, and run it several times (one per dimension) to implement
 * the same separable transform.
 * This filter can be used with the filter for which the neighborhood is
 * defined by the SetRadius() method, like the BoxImageFilter and its
 * subclasses.
 *
 *
 * This code was contributed in the Insight Journal paper:
 * "Efficient implementation of kernel filtering"
 * by Beare R., Lehmann G
 * https://hdl.handle.net/1926/555
 * http://www.insight-journal.org/browse/publication/160
 *
 *
 * \author Gaetan Lehmann
 * \author Richard Beare
 * \ingroup ITKReview
 */

template <typename TInputImage, typename TOutputImage, typename TFilter>
class ITK_TEMPLATE_EXPORT MiniPipelineSeparableImageFilter : public BoxImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MiniPipelineSeparableImageFilter);

  /** Standard class type aliases. */
  using Self = MiniPipelineSeparableImageFilter;
  using Superclass = BoxImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MiniPipelineSeparableImageFilter, BoxImageFilter);

  /** Image related type alias. */
  using InputImageType = TInputImage;
  using RegionType = typename TInputImage::RegionType;
  using SizeType = typename TInputImage::SizeType;
  using IndexType = typename TInputImage::IndexType;
  using PixelType = typename TInputImage::PixelType;
  using OffsetType = typename TInputImage::OffsetType;

  using OutputImageType = TOutputImage;
  using OutputPixelType = typename TOutputImage::PixelType;

  using FilterType = TFilter;
  using CastType = CastImageFilter<InputImageType, OutputImageType>;

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** n-dimensional Kernel radius. */
  using RadiusType = typename TInputImage::SizeType;

  void
  SetRadius(const RadiusType &) override;

  void
  SetRadius(const SizeValueType & radius) override
  {
    // needed because of the overloading of the method
    Superclass::SetRadius(radius);
  }

  void
  Modified() const override;

  void
  SetNumberOfWorkUnits(ThreadIdType nb) override;

protected:
  MiniPipelineSeparableImageFilter();
  ~MiniPipelineSeparableImageFilter() override = default;

  void
  GenerateData() override;

  typename FilterType::Pointer m_Filters[ImageDimension];
  typename CastType::Pointer   m_Cast;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMiniPipelineSeparableImageFilter.hxx"
#endif

#endif
