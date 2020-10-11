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
#ifndef itkBinaryThresholdImageFilter_h
#define itkBinaryThresholdImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkConceptChecking.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
/**
 *\class BinaryThresholdImageFilter
 *
 * \brief Binarize an input image by thresholding.
 *
 * This filter produces an output image whose pixels
 * are either one of two values ( OutsideValue or InsideValue ),
 * depending on whether the corresponding input image pixels
 * lie between the two thresholds ( LowerThreshold and UpperThreshold ).
 * Values equal to either threshold is considered to be between the thresholds.
 *
 * More precisely
 * \f[ Output(x_i) =
 *        \begin{cases}
 *          InsideValue  & \text{if $LowerThreshold \leq x_i \leq UpperThreshold$} \\
 *          OutsideValue & \text{otherwise}
 *         \end{cases}
 *    \f]
 *
 * This filter is templated over the input image type
 * and the output image type.
 *
 * The filter expect both images to have the same number of dimensions.
 *
 * The default values for LowerThreshold and UpperThreshold are:
 * LowerThreshold = NumericTraits<TInput>::NonpositiveMin();
 * UpperThreshold = NumericTraits<TInput>::max();
 * Therefore, generally only one of these needs to be set, depending
 * on whether the user wants to threshold above or below the desired threshold.
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKThresholding
 *
 * \sphinx
 * \sphinxexample{Filtering/Thresholding/ThresholdAnImageUsingBinary,Threshold An Image Using Binary Thresholding}
 * \endsphinx
 */
namespace Functor
{
template <typename TInput, typename TOutput>
class ITK_TEMPLATE_EXPORT BinaryThreshold
{
public:
  BinaryThreshold()
  {
    m_LowerThreshold = NumericTraits<TInput>::NonpositiveMin();
    m_UpperThreshold = NumericTraits<TInput>::max();
    m_OutsideValue = NumericTraits<TOutput>::ZeroValue();
    m_InsideValue = NumericTraits<TOutput>::max();
  }

  ~BinaryThreshold() = default;

  void
  SetLowerThreshold(const TInput & thresh)
  {
    m_LowerThreshold = thresh;
  }
  void
  SetUpperThreshold(const TInput & thresh)
  {
    m_UpperThreshold = thresh;
  }
  void
  SetInsideValue(const TOutput & value)
  {
    m_InsideValue = value;
  }
  void
  SetOutsideValue(const TOutput & value)
  {
    m_OutsideValue = value;
  }

  bool
  operator!=(const BinaryThreshold & other) const
  {
    if (m_LowerThreshold != other.m_LowerThreshold || m_UpperThreshold != other.m_UpperThreshold ||
        Math::NotExactlyEquals(m_InsideValue, other.m_InsideValue) ||
        Math::NotExactlyEquals(m_OutsideValue, other.m_OutsideValue))
    {
      return true;
    }
    return false;
  }

  bool
  operator==(const BinaryThreshold & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput & A) const
  {
    if (m_LowerThreshold <= A && A <= m_UpperThreshold)
    {
      return m_InsideValue;
    }
    return m_OutsideValue;
  }

private:
  TInput  m_LowerThreshold;
  TInput  m_UpperThreshold;
  TOutput m_InsideValue;
  TOutput m_OutsideValue;
};
} // namespace Functor

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT BinaryThresholdImageFilter
  : public UnaryFunctorImageFilter<
      TInputImage,
      TOutputImage,
      Functor::BinaryThreshold<typename TInputImage::PixelType, typename TOutputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryThresholdImageFilter);

  /** Standard class type aliases. */
  using Self = BinaryThresholdImageFilter;
  using Superclass = UnaryFunctorImageFilter<
    TInputImage,
    TOutputImage,
    Functor::BinaryThreshold<typename TInputImage::PixelType, typename TOutputImage::PixelType>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryThresholdImageFilter, UnaryFunctorImageFilter);

  /** Pixel types. */
  using InputPixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;

  /** Type of DataObjects to use for scalar inputs */
  using InputPixelObjectType = SimpleDataObjectDecorator<InputPixelType>;

  /** Set the "outside" pixel value. The default value
   * NumericTraits<OutputPixelType>::ZeroValue(). */
  itkSetMacro(OutsideValue, OutputPixelType);

  /** Get the "outside" pixel value. */
  itkGetConstReferenceMacro(OutsideValue, OutputPixelType);

  /** Set the "inside" pixel value. The default value
   * NumericTraits<OutputPixelType>::max() */
  itkSetMacro(InsideValue, OutputPixelType);

  /** Get the "inside" pixel value. */
  itkGetConstReferenceMacro(InsideValue, OutputPixelType);

  /** Set the thresholds. The default lower threshold
   * is NumericTraits<InputPixelType>::NonpositiveMin(). The default upper
   * threshold is NumericTraits<InputPixelType>::max. An exception is thrown
   * if the lower threshold is greater than the upper threshold. */
  virtual void
  SetUpperThreshold(const InputPixelType threshold);

  virtual void
  SetUpperThresholdInput(const InputPixelObjectType *);

  virtual void
  SetLowerThreshold(const InputPixelType threshold);

  virtual void
  SetLowerThresholdInput(const InputPixelObjectType *);

  /** Get the threshold values. */
  virtual InputPixelType
  GetUpperThreshold() const;

  virtual InputPixelObjectType *
  GetUpperThresholdInput();

  virtual const InputPixelObjectType *
  GetUpperThresholdInput() const;

  virtual InputPixelType
  GetLowerThreshold() const;

  virtual InputPixelObjectType *
  GetLowerThresholdInput();

  virtual const InputPixelObjectType *
  GetLowerThresholdInput() const;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputEqualityComparableCheck, (Concept::EqualityComparable<OutputPixelType>));
  itkConceptMacro(InputPixelTypeComparable, (Concept::Comparable<InputPixelType>));
  itkConceptMacro(InputOStreamWritableCheck, (Concept::OStreamWritable<InputPixelType>));
  itkConceptMacro(OutputOStreamWritableCheck, (Concept::OStreamWritable<OutputPixelType>));
  // End concept checking
#endif

protected:
  BinaryThresholdImageFilter();
  ~BinaryThresholdImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** This method is used to set the state of the filter before
   * multi-threading. */
  void
  BeforeThreadedGenerateData() override;

private:
  OutputPixelType m_InsideValue;
  OutputPixelType m_OutsideValue;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryThresholdImageFilter.hxx"
#endif

#endif
