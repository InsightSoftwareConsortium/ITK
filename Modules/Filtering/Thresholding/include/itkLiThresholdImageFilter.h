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

#ifndef itkLiThresholdImageFilter_h
#define itkLiThresholdImageFilter_h

#include "itkHistogramThresholdImageFilter.h"
#include "itkLiThresholdCalculator.h"

namespace itk
{

/**
 *\class LiThresholdImageFilter
 * \brief Threshold an image using the Li Threshold
 *
 * This filter creates a binary thresholded image that separates an
 * image into foreground and background components. The filter
 * computes the threshold using the LiThresholdCalculator and
 * applies that threshold to the input image using the
 * BinaryThresholdImageFilter.
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/811
 *
 * \sa HistogramThresholdImageFilter
 *
 * \ingroup Multithreaded
 * \ingroup ITKThresholding
 *
 * \sphinx
 * \sphinxexample{Filtering/Thresholding/DemonstrateThresholdAlgorithms,Demonstrate Available Threshold Algorithms}
 * \endsphinx
 */

template <typename TInputImage, typename TOutputImage, typename TMaskImage = TOutputImage>
class LiThresholdImageFilter : public HistogramThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LiThresholdImageFilter);

  /** Standard Self type alias */
  using Self = LiThresholdImageFilter;
  using Superclass = HistogramThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LiThresholdImageFilter, HistogramThresholdImageFilter);

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

  using HistogramType = typename Superclass::HistogramType;
  using CalculatorType = LiThresholdCalculator<HistogramType, InputPixelType>;

  /** Image related type alias. */
  static constexpr unsigned int InputImageDimension = InputImageType::ImageDimension;
  static constexpr unsigned int OutputImageDimension = OutputImageType::ImageDimension;

protected:
  LiThresholdImageFilter() { this->SetCalculator(CalculatorType::New()); }
  ~LiThresholdImageFilter() override = default;
};

} // end namespace itk

#endif
