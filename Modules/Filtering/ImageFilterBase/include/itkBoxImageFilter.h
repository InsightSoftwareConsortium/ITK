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
#ifndef itkBoxImageFilter_h
#define itkBoxImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkCastImageFilter.h"

namespace itk
{
/**
 * \class BoxImageFilter
 * \brief A base class for all the filters working on a box neighborhood
 *
 * This filter provides the code to store the radius information about the
 * neighborhood used in the subclasses.
 * It also conveniently reimplement the GenerateInputRequestedRegion() so
 * that region is well defined for the provided radius.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 * \ingroup ITKImageFilterBase
 */

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT BoxImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BoxImageFilter);

  /** Standard class type aliases. */
  using Self = BoxImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BoxImageFilter, ImageToImageFilter);

  /** Image related type alias. */
  using InputImageType = TInputImage;
  using RegionType = typename TInputImage::RegionType;
  using SizeType = typename TInputImage::SizeType;
  using IndexType = typename TInputImage::IndexType;
  using OffsetType = typename TInputImage::OffsetType;

  using InputPixelType = typename TInputImage::PixelType;

  using OutputImageType = TOutputImage;
  using OutputPixelType = typename TOutputImage::PixelType;

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  /** n-dimensional Kernel radius. */
  using RadiusType = typename TInputImage::SizeType;
  using RadiusValueType = typename TInputImage::SizeValueType;

  virtual void
  SetRadius(const RadiusType & radius);

  virtual void
  SetRadius(const RadiusValueType & radius);

  itkGetConstReferenceMacro(Radius, RadiusType);

protected:
  BoxImageFilter();
  ~BoxImageFilter() override = default;

  void
  GenerateInputRequestedRegion() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  RadiusType m_Radius;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBoxImageFilter.hxx"
#endif

#endif
