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

#ifndef itkIntermodesThresholdImageFilter_h
#define itkIntermodesThresholdImageFilter_h

#include "itkHistogramThresholdImageFilter.h"
#include "itkIntermodesThresholdCalculator.h"

namespace itk
{

/**
 *\class IntermodesThresholdImageFilter
 * \brief Threshold an image using the Intermodes Threshold
 *
 * This filter creates a binary thresholded image that separates an
 * image into foreground and background components. The filter
 * computes the threshold using the IntermodesThresholdCalculator and
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
 */

template <typename TInputImage, typename TOutputImage, typename TMaskImage = TOutputImage>
class IntermodesThresholdImageFilter : public HistogramThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(IntermodesThresholdImageFilter);

  /** Standard Self type alias */
  using Self = IntermodesThresholdImageFilter;
  using Superclass = HistogramThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(IntermodesThresholdImageFilter, HistogramThresholdImageFilter);

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
  using CalculatorType = IntermodesThresholdCalculator<HistogramType, InputPixelType>;


  itkSetMacro(MaximumSmoothingIterations, SizeValueType);
  itkGetMacro(MaximumSmoothingIterations, SizeValueType);

  /** Select whether midpoint (intermode=true) or minimum between
     peaks is used. */
  itkSetMacro(UseInterMode, bool);
  itkGetConstReferenceMacro(UseInterMode, bool);
  itkBooleanMacro(UseInterMode);

  /** Image related type alias. */
  static constexpr unsigned int InputImageDimension = InputImageType::ImageDimension;
  static constexpr unsigned int OutputImageDimension = OutputImageType::ImageDimension;

protected:
  IntermodesThresholdImageFilter()
  {
    auto calculator = CalculatorType::New();
    calculator->SetMaximumSmoothingIterations(m_MaximumSmoothingIterations);
    calculator->SetUseInterMode(m_UseInterMode);
    Superclass::SetCalculator(calculator);
  }
  ~IntermodesThresholdImageFilter() override = default;

  void
  VerifyPreconditions() ITKv5_CONST override
  {
    Superclass::VerifyPreconditions();
    if (dynamic_cast<const CalculatorType *>(Superclass::GetCalculator()) == nullptr)
    {
      itkExceptionMacro(<< "Invalid IntermodesCalculator.");
    }
  }

  void
  GenerateData() override
  {
    auto calculator = static_cast<CalculatorType *>(this->Superclass::GetModifiableCalculator());
    calculator->SetMaximumSmoothingIterations(m_MaximumSmoothingIterations);
    calculator->SetUseInterMode(m_UseInterMode);
    this->Superclass::GenerateData();
  }


  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "MaximumSmoothingIterations: " << m_MaximumSmoothingIterations << std::endl;
    os << indent << "UseInterMode: " << m_UseInterMode << std::endl;
  }

private:
  SizeValueType m_MaximumSmoothingIterations{ 1000 };
  bool          m_UseInterMode{ true };
};

} // end namespace itk

#endif
