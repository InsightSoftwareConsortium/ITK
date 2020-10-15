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
#ifndef itkHistogramToLogProbabilityImageFilter_h
#define itkHistogramToLogProbabilityImageFilter_h

#include "itkHistogramToImageFilter.h"

namespace itk
{
/** \class HistogramToLogProbabilityImageFilter
 * \brief The class takes a histogram as an input and gives the log probability
 * image as the output. A pixel, at position I,  in the output image is given by
 *
 * \f[
 * f(I) = \log_2( \frac{q_I}{\sum_{i \in I} q_I} )
 * \f]
 *  where \f$q_I\f$ is the frequency of measurement vector, I.
 *
 * This is the log of the  frequency of a measurement vector by the sum of all
 * frequencies.
 *
 * The output image is of type double.
 *
 * This is useful in plotting the joint histograms during registration.
 *
 *  \sa HistogramToImageFilter, HistogramToProbabilityImageFilter,
 *  HistogramToIntensityImageFilter, HistogramToEntropyImageFilter
 *
 * \ingroup ITKStatistics
 */

namespace Function
{
template <typename TInput, typename TOutput = double>
class HistogramLogProbabilityFunction
{
public:
  // Probability function = Number of occurrences in each bin /
  //   Total Number of occurrences.
  //
  // Returns pixels of float..
  using OutputPixelType = TOutput;

  HistogramLogProbabilityFunction() = default;

  ~HistogramLogProbabilityFunction() = default;

  inline OutputPixelType
  operator()(const TInput & A) const
  {
    if (A)
    {
      return static_cast<OutputPixelType>(
        std::log(static_cast<OutputPixelType>(A) / static_cast<OutputPixelType>(m_TotalFrequency)) / std::log(2.0));
    }
    else
    { // Check for Log 0. Always assume that the frequency is atleast 1.
      return static_cast<OutputPixelType>(
        std::log(static_cast<OutputPixelType>(A + 1) / static_cast<OutputPixelType>(m_TotalFrequency)) / std::log(2.0));
    }
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

template <typename THistogram, typename TImage = Image<double, 3>>
class HistogramToLogProbabilityImageFilter
  : public HistogramToImageFilter<THistogram,
                                  TImage,
                                  Function::HistogramLogProbabilityFunction<SizeValueType, typename TImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(HistogramToLogProbabilityImageFilter);

  /** Standard class type aliases. */
  using Self = HistogramToLogProbabilityImageFilter;

  /** Standard "Superclass" type alias. */
  using Superclass =
    HistogramToImageFilter<THistogram,
                           TImage,
                           Function::HistogramLogProbabilityFunction<SizeValueType, typename TImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(HistogramToLogProbabilityImageFilter, HistogramToImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

protected:
  HistogramToLogProbabilityImageFilter() = default;
  ~HistogramToLogProbabilityImageFilter() override = default;
};
} // end namespace itk

#endif
