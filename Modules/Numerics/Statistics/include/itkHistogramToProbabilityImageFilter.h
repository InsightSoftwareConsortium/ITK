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
#ifndef itkHistogramToProbabilityImageFilter_h
#define itkHistogramToProbabilityImageFilter_h

#include "itkHistogramToImageFilter.h"

namespace itk
{
/**
 *\class HistogramToProbabilityImageFilter
 * \brief The class takes a histogram as an input and gives the probability
 * image as the output. A pixel, at position I,  in the output image is given by
 *
 * \f[
 * f(I) = \frac{q_I}{\sum_{i \in I} q_I}
 * \f]
 *  where \f$q_I\f$ is the frequency of measurement vector, I.
 *
 * This is the frequency of a measurement vector by the sum of all frequencies =
 * Probability of the the measurement vector
 *
 * The output image is of type float.
 *
 * This is useful in plotting the joint histograms during registration.
 *
 *  \sa HistogramToImageFilter, HistogramToLogProbabilityImageFilter,
 *  HistogramToIntensityImageFilter, HistogramToEntropyImageFilter
 *
 * \ingroup ITKStatistics
 */

namespace Function
{
template <typename TInput, typename TOutput = float>
class HistogramProbabilityFunction
{
public:
  // Probability function = Number of occurrences in each bin /
  //   Total Number of occurrences.
  //
  // Returns pixels of float..
  using OutputPixelType = TOutput;

  HistogramProbabilityFunction() = default;

  ~HistogramProbabilityFunction() = default;

  inline OutputPixelType
  operator()(const TInput & A) const
  {
    return static_cast<OutputPixelType>(static_cast<OutputPixelType>(A) /
                                        static_cast<OutputPixelType>(m_TotalFrequency));
  }

  void
  SetTotalFrequency(SizeValueType n)
  {
    m_TotalFrequency = n;
  }

  SizeValueType
  GetTotalFrequency() const
  {
    return m_TotalFrequency;
  }

private:
  SizeValueType m_TotalFrequency{ 1 };
};
} // namespace Function

template <typename THistogram, typename TImage = Image<float, 3>>
class HistogramToProbabilityImageFilter
  : public HistogramToImageFilter<THistogram,
                                  TImage,
                                  Function::HistogramProbabilityFunction<SizeValueType, typename TImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(HistogramToProbabilityImageFilter);

  /** Standard class type aliases. */
  using Self = HistogramToProbabilityImageFilter;

  /** Standard "Superclass" type alias. */
  using Superclass =
    HistogramToImageFilter<THistogram,
                           TImage,
                           Function::HistogramProbabilityFunction<SizeValueType, typename TImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(HistogramToProbabilityImageFilter, HistogramToImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

protected:
  HistogramToProbabilityImageFilter() = default;
  ~HistogramToProbabilityImageFilter() override = default;
};
} // end namespace itk

#endif
