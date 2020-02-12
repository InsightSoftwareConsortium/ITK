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
#ifndef itkBinaryThresholdProjectionImageFilter_h
#define itkBinaryThresholdProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
/**
 *\class BinaryThresholdProjectionImageFilter
 * \brief BinaryThreshold projection
 *
 *
 * This class was contributed to the Insight Journal by Gaetan Lehmann.
 * the original paper can be found at
 *   https://hdl.handle.net/1926/164
 *
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *
 * \sa ProjectionImageFilter
 * \sa MedianProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \sa MaximumProjectionImageFilter
 * \sa MinimumProjectionImageFilter
 * \sa StandardDeviationProjectionImageFilter
 * \sa SumProjectionImageFilter
 * \ingroup ITKThresholding
 */

namespace Function
{
template <typename TInputPixel, typename TOutputPixel>
class BinaryThresholdAccumulator
{
public:
  BinaryThresholdAccumulator(SizeValueType) {}
  ~BinaryThresholdAccumulator() = default;

  inline void
  Initialize()
  {
    m_IsForeground = false;
  }

  inline void
  operator()(const TInputPixel & input)
  {
    if (input >= m_ThresholdValue)
    {
      m_IsForeground = true;
    }
  }

  inline TOutputPixel
  GetValue()
  {
    if (m_IsForeground)
    {
      return m_ForegroundValue;
    }
    else
    {
      return m_BackgroundValue;
    }
  }

  bool m_IsForeground;

  TInputPixel  m_ThresholdValue;
  TOutputPixel m_ForegroundValue;
  TOutputPixel m_BackgroundValue;
};
} // end namespace Function

template <typename TInputImage, typename TOutputImage>
class BinaryThresholdProjectionImageFilter
  : public ProjectionImageFilter<
      TInputImage,
      TOutputImage,
      Function::BinaryThresholdAccumulator<typename TInputImage::PixelType, typename TOutputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryThresholdProjectionImageFilter);

  using Self = BinaryThresholdProjectionImageFilter;
  using Superclass = ProjectionImageFilter<
    TInputImage,
    TOutputImage,
    Function::BinaryThresholdAccumulator<typename TInputImage::PixelType, typename TOutputImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Runtime information support. */
  itkTypeMacro(BinaryThresholdProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Convenient type alias for simplifying declarations. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  /** Image type alias support */
  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;

  using AccumulatorType = typename Superclass::AccumulatorType;

  /** Set/Get the output value used as "foreground". Defaults to
   * maximum value of PixelType. */
  itkSetMacro(ForegroundValue, OutputPixelType);
  itkGetConstMacro(ForegroundValue, OutputPixelType);

  /** Set/Get the output value used as "background". Defaults to
   * NumericTraits<PixelType>::NonpositiveMin(). */
  itkSetMacro(BackgroundValue, OutputPixelType);
  itkGetConstMacro(BackgroundValue, OutputPixelType);

  /** Set/Get the input value consider as "threshold". Defaults to
   *  NumericTraits<InputPixelType>::max() */
  itkSetMacro(ThresholdValue, InputPixelType);
  itkGetConstMacro(ThresholdValue, InputPixelType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputPixelTypeGreaterThanComparable, (Concept::GreaterThanComparable<InputPixelType>));
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));
  // End concept checking
#endif

protected:
  BinaryThresholdProjectionImageFilter()
  {
    m_ForegroundValue = NumericTraits<OutputPixelType>::max();
    m_BackgroundValue = NumericTraits<OutputPixelType>::NonpositiveMin();
    m_ThresholdValue = NumericTraits<InputPixelType>::ZeroValue();
  }

  ~BinaryThresholdProjectionImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);

    using InputPixelPrintType = typename NumericTraits<InputPixelType>::PrintType;

    os << indent << "ForegroundValue: " << static_cast<InputPixelPrintType>(m_ForegroundValue) << std::endl;

    using OutputPixelPrintType = typename NumericTraits<OutputPixelType>::PrintType;

    os << indent << "BackgroundValue: " << static_cast<OutputPixelPrintType>(m_BackgroundValue) << std::endl;

    os << indent << "ThresholdValue: " << static_cast<InputPixelPrintType>(m_ThresholdValue) << std::endl;
  }

  AccumulatorType
  NewAccumulator(SizeValueType size) const override
  {
    AccumulatorType accumulator(size);

    accumulator.m_ForegroundValue = m_ForegroundValue;
    accumulator.m_BackgroundValue = m_BackgroundValue;
    accumulator.m_ThresholdValue = m_ThresholdValue;
    return accumulator;
  }

  /** Pixel value for output foreground */
  OutputPixelType m_ForegroundValue;

  /** Pixel value for output background */
  OutputPixelType m_BackgroundValue;

  /** Pixel value for input Threshold */
  InputPixelType m_ThresholdValue;
}; // end BinaryThresholdProjectionImageFilter
} // end namespace itk

#endif
