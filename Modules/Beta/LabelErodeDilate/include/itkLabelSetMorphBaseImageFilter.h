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
#ifndef itkLabelSetMorphBaseImageFilter_h
#define itkLabelSetMorphBaseImageFilter_h

#include "itkNumericTraits.h"
#include "itkImageToImageFilter.h"

namespace itk
{
#if ITK_VERSION_MAJOR < 4
using ThreadIdType = int;
using RegionIndexType = int;
#else
using RegionIndexType = unsigned int;
#endif
/**
 * \class LabelSetMorphBaseImageFilter
 * \brief Base class for binary morphological erosion of label images.
 *
 * This filter is threaded. This class handles the threading for subclasses.
 *
 * \sa itkLabelSetDilateImageFilter itkLabelSetErodeImageFilter
 *
 * \ingroup LabelErodeDilate
 *
 * \author Richard Beare, Department of Medicine, Monash University,
 * Australia.  <Richard.Beare@monash.edu>
 **/
template <typename TInputImage, bool doDilate, typename TOutputImage = TInputImage>
class ITK_EXPORT LabelSetMorphBaseImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LabelSetMorphBaseImageFilter);

  /** Standard class type alias. */
  using Self = LabelSetMorphBaseImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelSetMorphBaseImageFilter, ImageToImageFilter);

  /** Pixel Type of the input image */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using PixelType = typename TInputImage::PixelType;
  using RealType = typename NumericTraits<PixelType>::FloatType;
  using OutputPixelType = typename TOutputImage::PixelType;
  using ScalarRealType = typename NumericTraits<PixelType>::ScalarRealType;

  using OutputIndexType = typename OutputImageType::IndexType;
  using OutputIndexValueType = typename OutputImageType::IndexValueType;

  /** Smart pointer type alias support.  */
  using InputImagePointer = typename TInputImage::Pointer;
  using InputImageConstPointer = typename TInputImage::ConstPointer;
  using InputSizeType = typename TInputImage::SizeType;
  using OutputSizeType = typename TOutputImage::SizeType;

  /** a type to represent the "kernel radius" */
  using RadiusType = typename itk::FixedArray<ScalarRealType, TInputImage::ImageDimension>;
  /** Image dimension. */

  using OutputImageRegionType = typename OutputImageType::RegionType;

  // set all of the scales the same
  void
  SetRadius(ScalarRealType scale);

  itkSetMacro(Radius, RadiusType);
  itkGetConstReferenceMacro(Radius, RadiusType);

  /**
   * Set/Get whether the scale refers to pixels or world units -
   * default is false
   */
  itkSetMacro(UseImageSpacing, bool);
  itkGetConstReferenceMacro(UseImageSpacing, bool);
  itkBooleanMacro(UseImageSpacing);

  /** Image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;

  /** Define the image type for internal computations
      RealType is usually 'double' in NumericTraits.
      Here we prefer float in order to save memory.  */

  void
  writeDist(std::string fname);

protected:
  LabelSetMorphBaseImageFilter();
  ~LabelSetMorphBaseImageFilter() override = default;

  RegionIndexType
  SplitRequestedRegion(RegionIndexType i, RegionIndexType num, OutputImageRegionType & splitRegion) override;

  void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) override;

  void
  GenerateData() override;

  // Override since the filter produces the entire dataset.
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  bool m_UseImageSpacing;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  RadiusType m_Radius;
  RadiusType m_Scale;
  using DistanceImageType = typename itk::Image<RealType, TInputImage::ImageDimension>;
  RealType m_Extreme;

  typename DistanceImageType::Pointer m_DistanceImage;

  int  m_MagnitudeSign;
  int  m_CurrentDimension;
  bool m_FirstPassDone;

  // this is the first non-zero entry in the radius. Needed to
  // support elliptical operations
  RealType m_BaseSigma;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelSetMorphBaseImageFilter.hxx"
#endif

#endif
