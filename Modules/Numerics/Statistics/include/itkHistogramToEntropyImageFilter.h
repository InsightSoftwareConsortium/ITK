/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkHistogramToEntropyImageFilter_h
#define itkHistogramToEntropyImageFilter_h

#include "itkHistogramToImageFilter.h"

namespace itk
{
/** \class HistogramToEntropyImageFilter
 * \brief The class takes a histogram as an input and gives the entropy
 * image as the output. A pixel, at position I,  in the output image is given by
 *
 * \f[
 * f(I) = -p \log_2 p
 * \f]
 *
 * where
 * \f[
 * p = \frac{q_I}{\sum_{i \in I} q_I}
 * \f]
 *  where  \f$q_I\f$ is the frequency of measurement vector, I.
 *
 * \f$p\f$ is the frequency of a measurement vector by the sum of all frequencies =
 * Probability of the the measurement vector
 *
 * The output image is of type double.
 *
 * This is useful in plotting the joint histograms during registration.
 *
 *  \sa HistogramToImageFilter, HistogramToLogProbabilityImageFilter,
 *  HistogramToIntensityImageFilter, HistogramToProbabilityImageFilter
 *
 * \ingroup ITKStatistics
 */

namespace Function
{
template< typename TInput, typename TOutput = double >
class HistogramEntropyFunction
{
public:

  //Probability function = Number of occurrences in each bin /
  //   Total Number of occurrences.
  //
  // Returns pixels of float..
  typedef  TOutput OutputPixelType;

  HistogramEntropyFunction():
    m_TotalFrequency(1) {}

  ~HistogramEntropyFunction() {}

  inline OutputPixelType operator()(const TInput & A) const
  {
    if ( A )
      {
      const double p = static_cast< OutputPixelType >( A )
                       / static_cast< OutputPixelType >( m_TotalFrequency );
      return static_cast< OutputPixelType >( ( -1 ) * p * std::log(p) / std::log(2.0) );
      }
    else
      {
      const double p = static_cast< OutputPixelType >( A + 1 )
                       / static_cast< OutputPixelType >( m_TotalFrequency );
      return static_cast< OutputPixelType >( ( -1 ) * p * std::log(p) / std::log(2.0) );
      }
  }

  void SetTotalFrequency(const SizeValueType n)
  {
    m_TotalFrequency = n;
  }

  SizeValueType GetTotalFrequency() const
  {
    return m_TotalFrequency;
  }

private:
  SizeValueType m_TotalFrequency;
};
}

template< typename THistogram, typename TImage=Image< double, 3> >
class HistogramToEntropyImageFilter:
  public HistogramToImageFilter< THistogram, TImage,
                                 Function::HistogramEntropyFunction< SizeValueType, typename TImage::PixelType > >
{
public:

  /** Standard class typedefs. */
  typedef HistogramToEntropyImageFilter Self;

  /** Standard "Superclass" typedef. */
  typedef HistogramToImageFilter< THistogram, TImage,
                                 Function::HistogramEntropyFunction< SizeValueType, typename TImage::PixelType > >
  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(HistogramToEntropyImageFilter, HistogramToImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

protected:
  HistogramToEntropyImageFilter() {}
  virtual ~HistogramToEntropyImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(HistogramToEntropyImageFilter);
};
} // end namespace itk

#endif
