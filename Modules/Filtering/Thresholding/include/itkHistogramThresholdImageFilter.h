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

#ifndef itkHistogramThresholdImageFilter_h
#define itkHistogramThresholdImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkHistogramThresholdImageFilterBase.h"
#include "itkHistogram.h"

namespace itk
{

/**
 *\class HistogramThresholdImageFilter
 * \brief Threshold an image using a HistogramThresholdCalculator
 *
 * This filter creates a binary thresholded image that separates an
 * image into foreground and background components. The filter
 * computes the threshold using a user provided HistogramThresholdCalculator and
 * applies that threshold to the input image using the
 * BinaryThresholdImageFilter.
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/10380/3279  or
 * http://www.insight-journal.org/browse/publication/811
 *
 * \ingroup Multithreaded
 * \ingroup ITKThresholding
 */

template <typename TInputImage, typename TOutputImage, typename TMaskImage = TOutputImage>
class ITK_TEMPLATE_EXPORT HistogramThresholdImageFilter
  : public HistogramThresholdImageFilterBase<TInputImage, TOutputImage, TMaskImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(HistogramThresholdImageFilter);

  /** Standard Self type alias */
  using Self = HistogramThresholdImageFilter;
  using Superclass = HistogramThresholdImageFilterBase<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(HistogramThresholdImageFilter, HistogramThresholdImageFilterBase);

  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using MaskImageType = TMaskImage;

  /** Image pixel value type alias. */
  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;
  using MaskPixelType = typename MaskImageType::PixelType;

  /** Image related type alias. */
  using InputImagePointer = typename InputImageType::Pointer;
  using OutputImagePointer = typename OutputImageType::Pointer;

  using InputSizeType = typename InputImageType::SizeType;
  using InputIndexType = typename InputImageType::IndexType;
  using InputImageRegionType = typename InputImageType::RegionType;
  using OutputSizeType = typename OutputImageType::SizeType;
  using OutputIndexType = typename OutputImageType::IndexType;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using MaskSizeType = typename MaskImageType::SizeType;
  using MaskIndexType = typename MaskImageType::IndexType;
  using MaskImageRegionType = typename MaskImageType::RegionType;

  using ValueType = typename NumericTraits<InputPixelType>::ValueType;
  using ValueRealType = typename NumericTraits<ValueType>::RealType;
  using HistogramType = Statistics::Histogram<ValueRealType>;
  using HistogramPointer = typename HistogramType::Pointer;
  using HistogramConstPointer = typename HistogramType::ConstPointer;
  using HistogramSizeType = typename HistogramType::SizeType;
  using HistogramMeasurementType = typename HistogramType::MeasurementType;
  using HistogramMeasurementVectorType = typename HistogramType::MeasurementVectorType;

  using CalculatorType = HistogramThresholdCalculator<HistogramType, InputPixelType>;
  using CalculatorPointer = typename CalculatorType::Pointer;

  /** Set/Get the calculator to use to compute the threshold */
  void
  SetCalculator(CalculatorType * calculator) override;
  CalculatorType *
  GetCalculator() override;

protected:
  HistogramThresholdImageFilter();
  ~HistogramThresholdImageFilter() override = default;

  void
  VerifyPreconditions() ITKv5_CONST override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  CalculatorPointer m_Calculator;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkHistogramThresholdImageFilter.hxx"
#endif

#endif
