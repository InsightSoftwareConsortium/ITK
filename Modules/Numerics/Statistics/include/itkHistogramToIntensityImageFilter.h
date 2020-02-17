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
#ifndef itkHistogramToIntensityImageFilter_h
#define itkHistogramToIntensityImageFilter_h

#include "itkHistogramToImageFilter.h"

namespace itk
{
/** \class HistogramToIntensityImageFilter
 * \brief The class takes a histogram as an input and produces an image
 * as the output. A pixel, at position I,  in the output image is given by
 *
 * \f[
 * f(I) = q_I
 * \f]
 *  where  \f$q_I\f$ is the frequency of measurement vector, I.
 *
 *  \sa HistogramToProbabilityImageFilter, HistogramToLogProbabilityImageFilter,
 *  HistogramToImageFilter, HistogramToEntropyImageFilter
 * \ingroup ITKStatistics
 */

namespace Function
{
template <typename TInput, typename TOutput = SizeValueType>
class HistogramIntensityFunction
{
public:
  // Intensity function returns pixels of SizeValueType.
  using OutputPixelType = TOutput;

  HistogramIntensityFunction() = default;

  ~HistogramIntensityFunction() = default;

  inline OutputPixelType
  operator()(const TInput & A) const
  {
    return static_cast<OutputPixelType>(A);
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

template <typename THistogram, typename TImage = Image<SizeValueType, 3>>
class HistogramToIntensityImageFilter
  : public HistogramToImageFilter<THistogram,
                                  TImage,
                                  Function::HistogramIntensityFunction<SizeValueType, typename TImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(HistogramToIntensityImageFilter);

  /** Standard class type aliases. */
  using Self = HistogramToIntensityImageFilter;

  /** Standard "Superclass" type alias. */
  using Superclass =
    HistogramToImageFilter<THistogram,
                           TImage,
                           Function::HistogramIntensityFunction<SizeValueType, typename TImage::PixelType>>;

  // using FunctorType = typename Function::HistogramIntensityFunction;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(HistogramToIntensityImageFilter, HistogramToImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

protected:
  HistogramToIntensityImageFilter() = default;
  ~HistogramToIntensityImageFilter() override = default;
};
} // end namespace itk

#endif
