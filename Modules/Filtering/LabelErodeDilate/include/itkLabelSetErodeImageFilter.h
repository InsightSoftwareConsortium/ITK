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
#ifndef itkLabelSetErodeImageFilter_h
#define itkLabelSetErodeImageFilter_h

#include "itkLabelSetMorphBaseImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * \class LabelSetErodeImageFilter
 * \brief Class for binary morphological erosion of label images.
 *
 * This filter will separate touching labels. If you don't want this
 * then use a conventional binary erosion to mask the label image.
 * This filter is threaded.
 *
 * \sa itkLabelSetDilateImageFilter
 *
 * \ingroup LabelErodeDilate
 *
 * \author Richard Beare, Department of Medicine, Monash University,
 * Australia.  <Richard.Beare@monash.edu>
 **/
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_EXPORT LabelSetErodeImageFilter : public LabelSetMorphBaseImageFilter<TInputImage, false, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LabelSetErodeImageFilter);

  /** Standard class type alias. */
  using Self = LabelSetErodeImageFilter;
  using Superclass = LabelSetMorphBaseImageFilter<TInputImage, false, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelSetErodeImageFilter, LabelSetMorphBaseImageFilter);

  /** Pixel Type of the input image */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using PixelType = typename TInputImage::PixelType;
  using RealType = typename NumericTraits<PixelType>::FloatType;
  using OutputPixelType = typename TOutputImage::PixelType;
  using ScalarRealType = typename NumericTraits<PixelType>::ScalarRealType;

  /** Smart pointer type alias support.  */
  using InputImagePointer = typename TInputImage::Pointer;
  using InputImageConstPointer = typename TInputImage::ConstPointer;
  using InputSizeType = typename TInputImage::SizeType;
  using OutputSizeType = typename TOutputImage::SizeType;

  /** a type to represent the "kernel radius" */
  using RadiusType = typename itk::FixedArray<ScalarRealType, TInputImage::ImageDimension>;
  /** Image dimension. */

  using OutputImageRegionType = typename OutputImageType::RegionType;
  /** Image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;

protected:
  LabelSetErodeImageFilter() { this->DynamicMultiThreadingOff(); }
  ~LabelSetErodeImageFilter() override = default;

  void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) override;

  // Override since the filter produces the entire dataset.
private:
  using DistanceImageType = typename Superclass::DistanceImageType;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelSetErodeImageFilter.hxx"
#endif

#endif
