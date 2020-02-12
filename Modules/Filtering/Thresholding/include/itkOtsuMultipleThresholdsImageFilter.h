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
#ifndef itkOtsuMultipleThresholdsImageFilter_h
#define itkOtsuMultipleThresholdsImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkFixedArray.h"
#include "itkOtsuMultipleThresholdsCalculator.h"
#include "itkScalarImageToHistogramGenerator.h"

namespace itk
{
/**
 *\class OtsuMultipleThresholdsImageFilter
 * \brief Threshold an image using multiple Otsu Thresholds.
 *
 * This filter creates a labeled image that separates the input
 * image into various classes. The filter
 * computes the thresholds using the OtsuMultipleThresholdsCalculator and
 * applies those thresholds to the input image using the
 * ThresholdLabelerImageFilter. The NumberOfHistogramBins and
 * NumberOfThresholds can be set
 * for the Calculator. The LabelOffset can be set
 * for the ThresholdLabelerImageFilter.
 *
 * This filter also includes an option to use the valley emphasis algorithm from
 * H.F. Ng, "Automatic thresholding for defect detection", Pattern Recognition Letters, (27): 1644-1649, 2006.
 * The valley emphasis algorithm is particularly effective when the object to be thresholded is small.
 * See the following tests for examples:
 * itkOtsuMultipleThresholdsImageFilterTest3 and itkOtsuMultipleThresholdsImageFilterTest4
 * To use this algorithm, simple call the setter: SetValleyEmphasis(true)
 * It is turned off by default.
 *
 * \sa ScalarImageToHistogramGenerator
 * \sa OtsuMultipleThresholdsCalculator
 * \sa ThresholdLabelerImageFilter
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKThresholding
 *
 * \sphinx
 * \sphinxexample{Filtering/Thresholding/ThresholdAnImageUsingOtsu,Threshold An Image Using Otsu}
 * \endsphinx
 */

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT OtsuMultipleThresholdsImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(OtsuMultipleThresholdsImageFilter);

  /** Standard Self type alias */
  using Self = OtsuMultipleThresholdsImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(OtsuMultipleThresholdsImageFilter, ImageToImageFilter);

  /** Image pixel value type alias. */
  using InputPixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;

  /** Image related type alias. */
  using InputImagePointer = typename TInputImage::Pointer;
  using OutputImagePointer = typename TOutputImage::Pointer;

  using InputSizeType = typename TInputImage::SizeType;
  using InputIndexType = typename TInputImage::IndexType;
  using InputImageRegionType = typename TInputImage::RegionType;
  using OutputSizeType = typename TOutputImage::SizeType;
  using OutputIndexType = typename TOutputImage::IndexType;
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** Threshold vector types. */
  using HistogramGeneratorType = itk::Statistics::ScalarImageToHistogramGenerator<TInputImage>;
  using HistogramType = typename HistogramGeneratorType::HistogramType;
  using OtsuCalculatorType = OtsuMultipleThresholdsCalculator<HistogramType>;
  using ThresholdVectorType = typename OtsuCalculatorType::OutputType;

  /** Image related type alias. */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Set/Get the number of histogram bins. Default is 128. */
  itkSetClampMacro(NumberOfHistogramBins, SizeValueType, 1, NumericTraits<SizeValueType>::max());
  itkGetConstMacro(NumberOfHistogramBins, SizeValueType);

  /** Set/Get the number of thresholds. Default is 1. */
  itkSetClampMacro(NumberOfThresholds, SizeValueType, 1, NumericTraits<SizeValueType>::max());
  itkGetConstMacro(NumberOfThresholds, SizeValueType);

  /** Set/Get the offset which labels have to start from. Default is 0. */
  itkSetClampMacro(LabelOffset,
                   OutputPixelType,
                   NumericTraits<OutputPixelType>::ZeroValue(),
                   NumericTraits<OutputPixelType>::max());
  itkGetConstMacro(LabelOffset, OutputPixelType);

  /** Set/Get the use of valley emphasis. Default is false. */
  itkSetMacro(ValleyEmphasis, bool);
  itkGetConstReferenceMacro(ValleyEmphasis, bool);
  itkBooleanMacro(ValleyEmphasis);

  /** Should the threshold value be mid-point of the bin or the maximum?
   * Default is to return bin maximum. */
  itkSetMacro(ReturnBinMidpoint, bool);
  itkGetConstReferenceMacro(ReturnBinMidpoint, bool);
  itkBooleanMacro(ReturnBinMidpoint);

  /** Get the computed threshold. */
  const ThresholdVectorType &
  GetThresholds() const
  {
    return m_Thresholds;
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputComparableCheck, (Concept::Comparable<OutputPixelType>));
  itkConceptMacro(OutputOStreamWritableCheck, (Concept::OStreamWritable<OutputPixelType>));
  // End concept checking
#endif

protected:
  OtsuMultipleThresholdsImageFilter();
  ~OtsuMultipleThresholdsImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateInputRequestedRegion() override;

  void
  GenerateData() override;

private:
  SizeValueType       m_NumberOfHistogramBins{ 128 };
  SizeValueType       m_NumberOfThresholds{ 1 };
  OutputPixelType     m_LabelOffset;
  ThresholdVectorType m_Thresholds;
  bool                m_ValleyEmphasis{ false };
#if defined(ITKV4_COMPATIBILITY)
  bool m_ReturnBinMidpoint{ true };
#else
  bool m_ReturnBinMidpoint{ false };
#endif
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkOtsuMultipleThresholdsImageFilter.hxx"
#endif

#endif
