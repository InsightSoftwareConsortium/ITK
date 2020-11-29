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
#include "itkImageToHistogramFilter.h"
#include "itkHistogram.h"
#include "itkHistogramThresholdCalculator.h"

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
 * The filter also has the option of providing a mask, in which case
 * the histogram and therefore the threshold is computed from the
 * parts of the mask with values indicated by MaskValue. The output
 * image is, by default, masked by the same image. This output
 * masking can be disabled using SetMaskOutput(false). Note that there
 * is an inconsistency here. The MaskImageFilter (used internally)
 * masks by non zero values, where as the MaskedImageToHistogramFilter
 * uses explicit values. If this does not match your usage then the
 * output masking will need to be managed by the user.
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/811
 *
 * \ingroup Multithreaded
 * \ingroup ITKThresholding
 */

template <typename TInputImage, typename TOutputImage, typename TMaskImage = TOutputImage>
class ITK_TEMPLATE_EXPORT HistogramThresholdImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(HistogramThresholdImageFilter);

  /** Standard Self type alias */
  using Self = HistogramThresholdImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(HistogramThresholdImageFilter, ImageToImageFilter);

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
  using MaskImagePointer = typename MaskImageType::Pointer;

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

  using HistogramGeneratorType = Statistics::ImageToHistogramFilter<InputImageType>;
  using HistogramGeneratorPointer = typename HistogramGeneratorType::Pointer;

  /** Image related type alias. */
  static constexpr unsigned int InputImageDimension = InputImageType::ImageDimension;
  static constexpr unsigned int OutputImageDimension = OutputImageType::ImageDimension;
  static constexpr unsigned int MaskImageDimension = MaskImageType::ImageDimension;

  /** Set and Get the mask image */
  itkSetInputMacro(MaskImage, TMaskImage);
  itkGetInputMacro(MaskImage, TMaskImage);

  /** Set the input image */
  void
  SetInput1(const TInputImage * input)
  {
    this->SetInput(input);
  }

  /** Set the marker image */
  void
  SetInput2(const TMaskImage * input)
  {
    this->SetMaskImage(input);
  }

  /** Set the "outside" pixel value. The default value
   * NumericTraits<OutputPixelType>::ZeroValue(). */
  itkSetMacro(OutsideValue, OutputPixelType);

  /** Get the "outside" pixel value. */
  itkGetConstMacro(OutsideValue, OutputPixelType);

  /** Set the "inside" pixel value. The default value
   * NumericTraits<OutputPixelType>::max() */
  itkSetMacro(InsideValue, OutputPixelType);

  /** Get the "inside" pixel value. */
  itkGetConstMacro(InsideValue, OutputPixelType);

  /** Set the number of histogram bins */
  itkSetMacro(NumberOfHistogramBins, unsigned int);
  itkGetConstMacro(NumberOfHistogramBins, unsigned int);

  /** Does histogram generator compute min and max from data?
   * Default is true for all but char types */
  itkSetMacro(AutoMinimumMaximum, bool);
  itkGetConstMacro(AutoMinimumMaximum, bool);
  itkBooleanMacro(AutoMinimumMaximum);

  /** Do you want the output to be masked by the mask used in
   * histogram construction. Only relevant if masking is in
   * use. Default is true. */
  itkSetMacro(MaskOutput, bool);
  itkGetConstMacro(MaskOutput, bool);
  itkBooleanMacro(MaskOutput);

  /** The value in the mask image, if used, indicating voxels that
  should be included. Default is the max of pixel type, as in the
  MaskedImageToHistogramFilter */
  itkSetMacro(MaskValue, MaskPixelType);
  itkGetConstMacro(MaskValue, MaskPixelType);

  /** Get the computed threshold. */
  itkGetConstMacro(Threshold, InputPixelType);

  /** Set/Get the calculator to use to compute the threshold */
  itkSetObjectMacro(Calculator, CalculatorType);
  itkGetModifiableObjectMacro(Calculator, CalculatorType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputEqualityComparableCheck, (Concept::EqualityComparable<OutputPixelType>));
  itkConceptMacro(InputOStreamWritableCheck, (Concept::OStreamWritable<InputPixelType>));
  itkConceptMacro(OutputOStreamWritableCheck, (Concept::OStreamWritable<OutputPixelType>));
  // End concept checking
#endif

protected:
  HistogramThresholdImageFilter();
  ~HistogramThresholdImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateInputRequestedRegion() override;
  void
  GenerateData() override;

  void
  VerifyPreconditions() ITKv5_CONST override
  {
    Superclass::VerifyPreconditions();
    if (m_Calculator.IsNull())
    {
      itkExceptionMacro(<< "No threshold calculator set.");
    }
  }

private:
  /** Set up the histogram generator. */
  void
  SetUpHistogramGenerator(HistogramGeneratorPointer histogramGenerator);

  OutputPixelType   m_InsideValue;
  OutputPixelType   m_OutsideValue;
  InputPixelType    m_Threshold;
  MaskPixelType     m_MaskValue;
  CalculatorPointer m_Calculator;
  unsigned          m_NumberOfHistogramBins{ 256 };
  bool              m_AutoMinimumMaximum;
  bool              m_MaskOutput{ true };
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkHistogramThresholdImageFilter.hxx"
#endif

#endif
